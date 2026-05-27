## Everything in this file and any files in the R directory are sourced during `simInit()`
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules)
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
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
      "standAgeMap",
      objectClass = "SpatRaster",
      desc = "Stand age map from LandR"
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
      "analysisUnitMap",
      "SpatRaster",
      "Raster assigning harvestable pixels to analysis units."
    ),
    
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
      
    #  sim$yieldTables <- standardizeYieldTables(sim)
      sim$yieldTables <- standardizeYieldTables(
        sim,
        maxAge = P(sim)$maxYieldAge
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
      nrows = 10, ncols = 10,
      xmin = 0, xmax = 1000,
      ymin = 0, ymax = 1000,
      crs = "EPSG:4326"   # 🔥 این خط مهمه
    )
    
    vals <- rep(
      sim$analysisUnitDT$pixelGroup,
      length.out = terra::ncell(r)
    )
    
    terra::values(r) <- vals    
    sim$pixelGroupMap <- r
  }
  
  # ========================================================
  # cohortData (fake if missing)
  # ========================================================
  
  if (!("cohortData" %in% names(sim))) {
    
    message("Creating fake cohortData")
    
    sim$cohortData <- data.table::data.table(
      
      pixelGroup = c(
        100,100,100,
        
        50000,50000,50000,
        
        150000,150000,150000,
        
        300000,300000,300000,
        
        600000,600000,600000,
        
        900000,900000,900000
      ),
      
      speciesCode = c(
        
        # black spruce dominated
        "Pice_mar","Abie_bal","Pinu_ban",
        
        # pine dominated
        "Pinu_res","Pinu_str","Acer_sah",
        
        # cedar / hemlock...
        "Thuj_occ","Tsug_can","Acer_sah",
        
        # boreal broadleaf
        "Betu_pap","Popu_tre","Pice_mar",
        
        # mixedwood
        "Pice_gla","Abie_bal","Betu_all",
        
        # tolerant hardwood
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
  
  if (!("canadaJurisdiction" %in% names(sim))) {
    
    message("Downloading Canada jurisdiction shapefile")
    
    zip_url <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000b21a_e.zip"
    
    shp_path <- reproducible::prepInputs(
      url = zip_url,
      destinationPath = "data",
      targetFile = "lpr_000b21a_e.shp"
    )
    
    sim$canadaJurisdiction <- shp_path
  }
  
  # ========================================================
  # YIELD FILES (CORE FIX 🔥)
  # ========================================================
  
  # ======================================================
  # YIELD FILES (UPDATED FOR NL + ON)
  # ========================================================
  
  if (jur == "NL") {
    
    message("Loading NL yield (.yld) files")
    
    yld_dir <- "modules/EasternCanadaClassifier/data/NL"
    
    if (!dir.exists(yld_dir)) {
      stop("Directory not found: ", yld_dir)
    }
    
    yld_files <- list.files(
      yld_dir,
      pattern = "\\.yld$",
      full.names = TRUE
    )
    
    if (length(yld_files) == 0) {
      stop("No .yld files found in: ", yld_dir)
    }
    
    sim$yieldFiles <- yld_files
    
    message("Found ", length(yld_files), " NL yield files")
    
  } else if (jur == "ON") {
    
    message("Loading ON yield (YTF) files")
    
    ytf_dir <- "modules/EasternCanadaClassifier/data/ON/YTF"
    
    if (!dir.exists(ytf_dir)) {
      stop("Directory not found: ", ytf_dir)
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