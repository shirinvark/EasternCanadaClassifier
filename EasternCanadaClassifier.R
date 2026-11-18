## Everything in this file and any files in the R directory are sourced during `simInit()`
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules)
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`...
defineModule(sim, list(
  name = "EasternCanadaClassifier",
  description = "Classifies harvestable landbase into analysis units and computes area summaries.",
  keywords = c("analysis units", "classification", "harvestable landbase", "AAC"),
  authors = structure(list(list(given = c("Shirin"), family = "Varkouhi", role = c("aut", "cre"), email = "shirin.varkuhi@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(EasternCanadaClassifier = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "EasternCanadaClassifier.Rmd"),
  reqdPkgs = c(
    "SpaDES.core (>= 3.0.4)",
    "terra",
    "data.table"
  ),
  parameters = bindrows(
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric",NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter(
      "jurisdiction",
      "character",
      "AB",  # default
      NA, NA,
      "Jurisdiction code (e.g., AB, NL, ON, QC)"
    ),
    defineParameter(
      "maxYieldAge",
      "numeric",
      255,
      NA, NA,
      "Maximum age used for standardized yield tables"
    )
  ),
  inputObjects = bindrows(
    expectsInput(
      "cohortData",
      objectClass = "data.frame",
      desc = "LandR cohort data containing species biomass"
    ),
    expectsInput(
      "pixelGroupMap",
      objectClass = "SpatRaster",
      desc = "Raster identifying pixelGroup IDs"
    ),
    expectsInput(
      "yieldVolFile",
      objectClass = "character",
      desc = "Path to .vol yield file"
    ),
    expectsInput(
      "harvestableFraction",
      objectClass = "SpatRaster",
      desc = "Fraction of each pixel that is harvestable (from Landbase module)"
    )
  ),
  
  outputObjects = bindrows(
    
    createsOutput(
      "yieldTables",
      "matrix",
      "Yield tables used for classifier."
    ),
    
    
    createsOutput(
      "yieldAges",
      "numeric",
      "Age classes corresponding to yield tables."
    ),
    
    createsOutput(
      "yieldConifer",
      "matrix",
      "Conifer component of the yield tables."
    ),
    
    createsOutput(
      "yieldDeciduous",
      "matrix",
      "Deciduous component of the yield tables."
    ),
    
    createsOutput(
      "pixelGroupToAU",
      "data.table",
      "Lookup table linking pixelGroup IDs to analysis unit IDs."
    ),
    
    createsOutput(
      "areaByAU",
      "data.table",
      "Area summary in hectares for each analysis unit."
    ),
    createsOutput(
      "analysisUnitSummary",
      "data.table",
      "Summary table of Analysis Units including assigned yield curve, effective area, and number of pixel groups."
    ),
    createsOutput(
      "analysisUnitMap",
      "SpatRaster",
      "Raster map showing the spatial distribution of Analysis Units."
    ),
    createsOutput(
      "ageStructureByAU",
      "data.table",
      "Age-class distribution for each analysis unit."
    ),
    
    createsOutput(
      "ageSummaryByAU",
      "data.table",
      "Mean age and number of stands for each analysis unit."
    ),
    createsOutput(
      "classification",
      "data.table",
      "Pixel group classification results."
    ),
    
    createsOutput(
      "AUtoCurve",
      "data.table",
      "Mapping between analysis units and yield curves."
    ),
    
    createsOutput(
      "standDT",
      "data.table",
      "Stand-level table used by AAC."
    ),
    
    createsOutput(
      "speciesSummaryByAU",
      "data.table",
      "Species biomass composition summary for each analysis unit."
    )
    
  ))
   )

Init <- function(sim) {
  
  jur <- toupper(P(sim)$jurisdiction)
  
  fun_name <- paste0("classifyProvince_", jur)
  
  if (!exists(fun_name, mode = "function")) {
    stop("No classifier function found for jurisdiction: ", jur)
  }
  
  sim <- get(fun_name, mode = "function")(sim)
  
  return(sim)
}


doEvent.EasternCanadaClassifier <- function(sim, eventTime, eventType) {
  
  switch(
    
    eventType,
    
    init = {
      
      sim <- Init(sim)
      
      # ===================================================
      # build standardized annual yield tables
      # ===================================================
      
      sim$yieldTables <- standardizeYieldTables(
        sim,
        maxAge = P(sim)$maxYieldAge
      )
     
      # =====================================================
      # AAC input table
      # =====================================================
      aacInput <- sim$standDT[
        ,
        .(
          area = sum(effectiveArea, na.rm = TRUE)
        ),
        by = .(
          AU,
          age,
          curveID
        )
      ]
      
      maxAge <- max(
        vapply(
          sim$yieldTables,
          nrow,
          integer(1)
        )
      )
      
      expanded <- data.table::rbindlist(
        lapply(seq_len(nrow(aacInput)), function(i) {
          
          x <- aacInput[i]
          
         
          if (x$age <= 70) {
            
            ages <- x$age
            
          } else if (x$age %% 10 == 0) {
            
            # Ontario-style ages: 80 -> 71:80
            ages <- seq(
              x$age - 9L,
              min(x$age, maxAge),
              by = 1L
            )
            
          } else if ((x$age - 1L) %% 10 == 0) {
            
            # Newfoundland/LandR-style ages: 71 -> 71:80
            ages <- seq(
              x$age,
              min(x$age + 9L, maxAge),
              by = 1L
            )
            
          } else {
            
            ages <- x$age
            
          }
          data.table(
            AU = x$AU,
            curveID = x$curveID,
            age = ages,
            area = x$area / length(ages)
          )
        })
      )
      
      expanded <- expanded[
        ,
        .(
          area = sum(area)
        ),
        by = .(
          AU,
          curveID,
          age
        )
      ]
      
      aacInput <- expanded
      
      allAU <- unique(
        aacInput[
          ,
          .(AU, curveID)
        ]
      )
      
      fullGrid <- allAU[
        ,
        .(age = 1:maxAge),
        by = .(AU, curveID)
      ]
      ##################################
      sim$aacInput <- merge(
        fullGrid,
        aacInput,
        by = c(
          "AU",
          "curveID",
          "age"
        ),
        all.x = TRUE
      )
      
      sim$aacInput[
        is.na(area),
        area := 0
      ]
      sim$aacInput <- sim$aacInput[
        ,
        .(area = sum(area)),
        by = .(AU, curveID, age)
      ]
      setorder(
        sim$aacInput,
        AU,
        age
      )
    },
    
    warning(noEventWarning(sim))
  )
  
  invisible(sim)
}



.inputObjects <- function(sim) {
  
  requireNamespace("terra")
  requireNamespace("data.table")
  requireNamespace("reproducible")
  
  jur <- toupper(P(sim)$jurisdiction)
  message("Selected jurisdiction: ", jur)
  
  # ========================================================
  # pixelGroupMap (fake if missing)
  # ========================================================
  
  if (!("pixelGroupMap" %in% names(sim))) {
    
    message("Creating fake pixelGroupMap")
    
    r <- terra::rast(
      nrows = 10,
      ncols = 10,
      xmin = -140,
      xmax = -50,
      ymin = 40,
      ymax = 70,
      crs = "EPSG:4326"
    )
    
    terra::values(r) <- seq_len(terra::ncell(r))
    
    sim$pixelGroupMap <- r
  }
  
  # ========================================================
  # cohortData (fake if missing)
  # ========================================================
  
  if (!("cohortData" %in% names(sim))) {
    
    message("Creating fake cohortData")
  
    pg <- c(70,79,80)
    sim$cohortData <- data.table::data.table(
      
      pixelGroup = rep(pg, each = 3),
      
      speciesCode = c(
        
        "Pice_mar","Abie_bal","Pinu_ban",
        
        "Pinu_res","Pinu_str","Acer_sah",
        
        "Thuj_occ","Tsug_can","Acer_sah",
        
        "Betu_pap","Popu_tre","Pice_mar",
        
        "Pice_gla","Abie_bal","Betu_all",
        
        "Acer_sah","Quer_rub","Fagu_gra"
      ),
      
      age = c(
        80,80,80,
        60,60,60,
        120,120,120,
        50,50,50,
        70,70,70,
        100,100,100
      ),
      
      B = c(
        70,20,10,
        50,40,10,
        60,30,10,
        40,40,20,
        40,30,30,
        50,30,20
      )
    )
  }
  # ========================================================
  # harvestableFraction (fake if missing)
  # ========================================================
  
  if (!("harvestableFraction" %in% names(sim))) {
    
    message("Creating fake harvestableFraction")
    
    r <- sim$pixelGroupMap
    
    terra::values(r) <- sample(
      c(0, 1),
      terra::ncell(r),
      replace = TRUE,
      prob = c(0.3, 0.7)
    )
    
    sim$harvestableFraction <- r
  }
  
  # ========================================================
  # CANADA JURISDICTION SHAPEFILE
  # ========================================================
  
  
  # ========================================================
  # CANADA JURISDICTION SHAPEFILE
  # ========================================================
  
  if (!inherits(sim$canadaJurisdiction, "SpatVector")) {
    
    message("Preparing Canada jurisdiction shapefile")
    
    input_dir <- getPaths()$inputPath
    zip_path <- file.path(input_dir, "lpr_000b21a_e.zip")
    extract_dir <- file.path(input_dir, "CanadaJurisdiction")
    
    dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
    
    shp_path <- file.path(extract_dir, "lpr_000b21a_e.shp")
    
    if (!file.exists(shp_path)) {
      
      if (!file.exists(zip_path)) {
        
        file_id <- "1XzwDRGzSwiP3yVB04tHFEISLQZcTb-fC"
        
        zip_url <- paste0(
          "https://drive.google.com/uc?export=download&id=",
          file_id
        )
        
        download.file(
          zip_url,
          destfile = zip_path,
          mode = "wb"
        )
      }
      
      # Check that the downloaded file is a valid ZIP
      zip_contents <- tryCatch(
        utils::unzip(zip_path, list = TRUE),
        error = function(e) {
          stop("Invalid ZIP download: ", conditionMessage(e))
        }
      )
      
      if (!any(grepl("lpr_000b21a_e.shp$", zip_contents$Name))) {
        stop("Expected shapefile not found inside ZIP.")
      }
      
      utils::unzip(zip_path, exdir = extract_dir)
    }
    
    sim$canadaJurisdiction <- terra::vect(shp_path)
  }
  
  # ======================================================
  # YIELD FILES (NL + ON)
  # ========================================================
  
  
  # ======================================================
  # YIELD FILES
  # ======================================================
  
  if (jur == "NL") {
    
    # Newfoundland yield files are downloaded and loaded
    # inside classifyProvince_NL().
    
    message("NL yield files will be prepared by classifyProvince_NL")
    
  } else if (jur == "ON") {
    
    message("Loading ON yield (YTF) files")
    
    ytf_dir <- file.path(
      getPaths()$inputPath,
      "Provincial",
      "ON",
      "YTF"
    )
    
      # Prepare Ontario YTF files from repository data
      if (!dir.exists(ytf_dir) ||
          length(list.files(ytf_dir, pattern = "\\.(csv|txt)$")) == 0) {
        
        source_dir <- "E:/EasternCanadaClassifier/data/Provincial/ON/YTF"
        
        if (!dir.exists(source_dir)) {
          stop("Ontario YTF source not found: ", source_dir)
        }
        
        dir.create(ytf_dir, recursive = TRUE, showWarnings = FALSE)
        
        files <- list.files(
          source_dir,
          pattern = "\\.(csv|txt)$",
          full.names = TRUE
        )
        
        copied <- file.copy(files, ytf_dir, overwrite = TRUE)
        
        if (!length(files) || !all(copied)) {
          stop("Failed to prepare Ontario YTF files.")
        }
      }
     
    
    ytf_files <- list.files(
      ytf_dir,
      pattern = "\\.(csv|txt)$",
      full.names = TRUE
    )
    
    if (length(ytf_files) == 0) {
      stop("No YTF files found in: ", ytf_dir)
    }
    
    sim$yieldFiles <- ytf_files
    
    message("Found ", length(ytf_files), " ON YTF files")
    
  } else {
    
    message("No yield file logic defined for jurisdiction: ", jur)
  }
  
  return(sim)
}