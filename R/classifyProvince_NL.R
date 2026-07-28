classifyProvince_NL <- function(sim) {
  
  message("Running Newfoundland classifier")
  
  # ===================================================
  # === DOWNLOAD DATA FROM GITHUB ===
  # ===================================================
  
  nl_dir <- file.path(
    getPaths()$inputPath,
    "NL"
  )
  
  dir.create(
    nl_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  ytf_dir <- file.path(
    nl_dir,
    "YTF"
  )
  
  dir.create(
    ytf_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  base_url <- "https://raw.githubusercontent.com/shirinvark/EasternCanadaClassifier/main/data/NL/"
  
  # ===================================================
  # === DOWNLOAD MAPPING FILES ===
  # ===================================================
  
  mapping_files <- c(
    "speciesGroups.txt",
    "mapSpeciesGroups.txt"
  )
  
  for (f in mapping_files) {
    
    dest <- file.path(nl_dir, f)
    
    if (!file.exists(dest)) {
      
      download.file(
        paste0(base_url, f),
        destfile = dest,
        mode = "wb"
      )
      
    }
  }
  
  # ===================================================
  # === DOWNLOAD YIELD TABLES ===
  # ===================================================
  
  ytf_files <- c(
    "BarNS_sub_all.yld",
    "Central_Sub_all.yld",
    "District 1.yld",
    "NpMainLong_sub_all.yld",
    "West_sub_all.yld"
  )
  
  for (f in ytf_files) {
    
    dest <- file.path(ytf_dir, f)
    
    if (!file.exists(dest)) {
      
      url <- paste0(
        base_url,
        "YTF/",
        URLencode(f)
      )
      
      download.file(
        url,
        destfile = dest,
        mode = "wb"
      )
      
    }
  }
  
  
  
  source(
    file.path(
      getPaths()$modulePath,
      "EasternCanadaClassifier",
      "R",
      "helpers.R"
    )
  )
  
  # ===================================================
  # === READ MAPPINGS ===
  # ===================================================
  
  speciesGroups <- read_curve_mapping(
    file.path(nl_dir, "speciesGroups.txt")
  )
  
  mapSpeciesGroups <- read_curve_mapping(
    file.path(nl_dir, "mapSpeciesGroups.txt")
  )
  
  sim$mapSpeciesGroups <- mapSpeciesGroups
  
  groups <- unique(
    unlist(mapSpeciesGroups)
  )
  
  groups <- groups[!is.na(groups)]
  groups <- groups[groups != ""]
  
  inventory <- data.table()
  for (f in ytf_files) {
    
    path <- file.path(
      ytf_dir,
      f
    )
    
    lines <- readLines(path)
    
    y_lines <- grep(
      "^\\*Y",
      lines,
      value = TRUE
    )
    
    y_lines <- y_lines[
      grepl("\\smedium\\s", y_lines)
    ]
    
    tmp <- data.table(
      file = f,
      community = sub(
        "^\\*Y\\s+(\\S+).*",
        "\\1",
        y_lines
      ),
      region = sub(
        "^\\*Y\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\?\\s+(\\S+)\\s+\\?.*$",
        "\\1",
        y_lines
      )
    )
    
    inventory <- rbind(
      inventory,
      tmp
    )
  }
  
  
  #######AU
  AU_table <- unique(
    inventory[
      ,
      .(
        AU = paste(
          region,
          community,
          sep = "_"
        ),
        region,
        community
      )
    ]
  )
  
  
  ####AUTOcurve####
  
  AUtoCurve <- copy(AU_table)[
    ,
    .(
      AU,
      curveID = AU
    )
  ]
  
  dupAU <- AUtoCurve[, .N, by = AU][N > 1]
  
  if (nrow(dupAU) > 0) {
    stop("More than one curve assigned to the same AU.")
  }
  
  
  
  region_file <- unique(
    inventory[
      ,
      .(
        region,
        file
      )
    ]
  )
  
  
  AU_table <- merge(
    AU_table,
    region_file,
    by = "region",
    all.x = TRUE
  )
  
  
  
  #################3
  file_cache <- list()
  
  yieldTables_NL <- list()
  cleanOriginalYieldTables_NL <- list()
  
  for (i in seq_len(nrow(AU_table))) {
    
    au <- AU_table$AU[i]
    region <- AU_table$region[i]
    community <- AU_table$community[i]
    file <- AU_table$file[i]
    
    if (!file %in% names(file_cache)) {
      
      path <- file.path(
        ytf_dir,
        file
      )
      
      lines <- readLines(path)
      lines <- trimws(lines)
      lines <- lines[lines != ""]
      
      file_cache[[file]] <- lines
    }
    
    lines <- file_cache[[file]]
    
    
    
    
    
    
    
    ####################new
    
    headers <- parseYLDHeaders(lines)
    comm <- community
    reg  <- region
    header <- headers[
      community == comm &
        quality == "medium" &
        density == "d2" &
        region == reg
    ]
    
    
    stopifnot(nrow(header) == 1)
    speciesInfo <- parseSpecies(
      lines,
      startLine = header$lineNumber
    )
    
    raw_curve <- parseVolumes(
      lines,
      speciesInfo
    )
    cleanOriginalYieldTables_NL[[au]] <- raw_curve    
    ######################new
    
    curve_data <- rewrite_yld_curve(
      raw_curve,
      mapSpeciesGroups
    )
    
    
    
    
    
    checkVolumeConservation <- FALSE
    
    if (checkVolumeConservation) {
      
      raw_total <- rowSums(as.data.frame(raw_curve))
      
      group_total <- rowSums(as.data.frame(curve_data))
      
      tmp_check <- data.frame(
        age = seq(0, by = 10, length.out = length(raw_total)),
        raw_total = raw_total,
        group_total = group_total,
        difference = raw_total - group_total
      )
      
      
      cat(
        "\nMAX DIFFERENCE:",
        max(abs(tmp_check$difference)),
        "\n"
      )
    }
    ######
    ages <- seq(
      0,
      by = 5,
      length.out = length(curve_data[[1]])
    )
    
    maxAge <- max(ages)
    
    dt_curve <- data.table(
      AC10 = 1:maxAge
    )
    
    for (sp in names(curve_data)) {
      
      tmp <- standardizeYieldCurve(
        ages    = ages,
        volumes = curve_data[[sp]],
        maxAge  = maxAge
      )
      
      dt_curve[[sp]] <- tmp$volume
    }
    
    # -------------------------------------------------------
    # Convert yield volume (m3/ha) to approximate biomass (kg/ha)
    # Steve's suggested conversion
    # -------------------------------------------------------
    #summary(dt_curve)
    conversionFactor <- 1000 * 0.5 / 0.8
    
    species_cols <- setdiff(names(dt_curve), "AC10")
    
    dt_curve[
      ,
      (species_cols) := lapply(
        .SD,
        function(x) x * conversionFactor
      ),
      .SDcols = species_cols
    ]
    
    
    yieldTables_NL[[au]] <- dt_curve
  }
  # =========================================================
  # BUILD pixel_region FROM NL_YCF
  # =========================================================
  
  shp_path <- file.path(
    getPaths()$modulePath,
    "EasternCanadaClassifier",
    "data",
    "NL",
    "NL_YCF.shp"
  )
  
  shp <- terra::vect(shp_path)
  
  
  
  # ---- project ----
  shp <- terra::project(
    shp,
    terra::crs(sim$pixelGroupMap)
  )
  NL_YCF <- terra::vect(shp_path)
  
  NL_YCF <- terra::project(
    NL_YCF,
    terra::crs(sim$pixelGroupMap)
  )
  # ---- rasterize ----
  stopifnot(
    terra::same.crs(
      NL_YCF,
      sim$pixelGroupMap
    )
  )
  region_raster <- terra::rasterize(
    shp,
    sim$pixelGroupMap,
    field = "YCF",
    touches = TRUE
  )
  
  
  
  
  if (!terra::same.crs(
    NL_YCF,
    sim$pixelGroupMap
  )) {
    
    NL_YCF <- terra::project(
      NL_YCF,
      terra::crs(sim$pixelGroupMap)
    )
    
  }
  # ---- extract values ----
  pg  <- data.table::as.data.table(
    terra::values(sim$pixelGroupMap)
  )
  
  reg <- data.table::as.data.table(
    terra::values(region_raster)
  )
  
  data.table::setnames(
    pg,
    names(pg),
    "pixelGroup"
  )
  
  data.table::setnames(
    reg,
    names(reg),
    "region"
  )
  
  pixel_region <- cbind(
    pg,
    reg
  )
  
  # ---- clean ----
  pixel_region <- pixel_region[
    !is.na(pixelGroup) &
      !is.na(region)
  ]
  lut <- terra::cats(region_raster)[[1]]
  
  pixel_region[
    ,
    region := lut$YCF[
      match(region, lut$ID)
    ]
  ]
  
  pixel_region[
    ,
    region := gsub(
      "^NL_",
      "",
      region
    )
  ]
  
  
  lut <- levels(region_raster)[[1]]
  
  
  
  pixel_region[
    ,
    region := gsub(
      "^NL_",
      "",
      as.character(region)
    )
  ]
  
  # ---- save ----
  sim$pixel_region <- pixel_region
  
  
  # =========================================================
  # CLASSIFIER - LOAD COHORT DATA
  # =========================================================
  #
  
  
  cohortDT <- data.table::as.data.table(
    sim$cohortData
  )
  
  cohortDT[
    ,
    speciesCode := as.character(speciesCode)
  ]
  
  cohortDT[
    ,
    final_group := speciesGroups[[speciesCode]],
    by = speciesCode
  ]
  
  cohortDT <- cohortDT[
    !is.na(final_group)
  ]
  
  cohort_group <- cohortDT[
    ,
    .(
      B = sum(
        B,
        na.rm = TRUE
      )
    ),
    by = .(
      pixelGroup,
      age,
      final_group
    )
  ]
  
  
  ########
  
  cohort_wide <- dcast(
    cohort_group,
    pixelGroup + age ~ final_group,
    value.var = "B",
    fill = 0
  )
  
  
  group_cols <- setdiff(
    names(cohort_wide),
    c("pixelGroup", "age", "total")
  )
  
  cohort_wide[
    ,
    total := rowSums(.SD),
    .SDcols = group_cols
  ]
  
  
  cohort_wide <- cohort_wide[
    total > 0
  ]
  
  ####normalization
  group_cols <- setdiff(
    names(cohort_wide),
    c("pixelGroup", "age")
  )
  
  prop_cols <- intersect(
    groups,
    group_cols
  )
  
  prop_cols <- prop_cols[!is.na(prop_cols)]
  
  if (length(prop_cols) == 0) {
    stop("❌ No matching species groups between cohort and yield")
  }
  
  
  
  yield_by_region <- list()
  
  for (reg in unique(AU_table$region)) {
    
    region_aus <- AU_table[
      region == reg,
      AU
    ]
    
    region_list <- lapply(
      region_aus,
      function(au) {
        
        dt <- copy(
          yieldTables_NL[[au]]
        )
        
        dt[, AU := au]
        
        dt[, zone := reg]
        
        dt
      }
    )
    
    yield_by_region[[reg]] <- rbindlist(
      region_list,
      fill = TRUE
    )
    
    
  }   ## ← این هم پایان حلقه for
  
  # =========================================================
  # NORMALIZE CURVES ONCE
  # =========================================================
  
  curve_cols <- prop_cols
  sim$yield_by_region <- yield_by_region
  # yield_by_region_norm <- lapply(
  #   yield_by_region,
  #   function(dt) {
  #     
  #     dt <- copy(dt)
  #     
  #     dt[
  #       ,
  #       total := rowSums(.SD),
  #       .SDcols = curve_cols
  #     ]
  #     
  #     dt[
  #       ,
  #       (curve_cols) := lapply(
  #         .SD,
  #         function(x) x / total
  #       ),
  #       .SDcols = curve_cols
  #     ]
  #     
  #     dt
  #   }
  # )
  
  
  
  # =========================================================
  # KEEP ONLY PIXELS WITH REGION
  # =========================================================
  
  cohort_classifiable <- cohort_wide[
    pixelGroup %in% pixel_region$pixelGroup
  ]
  
  # ==========================================
  # FIX DUPLICATED PIXELGROUPS IN pixel_region
  # ==========================================
  
  
  
  pixel_region <- pixel_region[
    ,
    .(
      region = names(sort(table(region), decreasing = TRUE))[1]
    ),
    by = pixelGroup
  ]
  
  
  
  dup <- pixel_region[
    ,
    uniqueN(region),
    by = pixelGroup
  ][V1 > 1]
  
  # if (nrow(dup) > 0) {
  #   stop("Some pixelGroups belong to multiple regions.")
  # }
  setkey(
    pixel_region,
    pixelGroup
  )
  
  
  # ==========================================
  # MERGE REGION INTO COHORT TABLE
  # ==========================================
  
  cohort_classifiable <- merge(
    cohort_classifiable,
    pixel_region,
    by = "pixelGroup",
    all.x = FALSE
  )
  
  # ======================================================
  # CLASSIFY ONE PIXEL
  # ====================================================
  results <- cohort_classifiable[, {
    
    
    # cohort_vec <- unlist(
    #   .SD[1, curve_cols, with = FALSE],
    #   use.names = FALSE
    # )
    cohort_vec <- colSums(
      .SD[, ..curve_cols],
      na.rm = TRUE
    )
    
    if (sum(cohort_vec) == 0) {
      
      list(
        bestAU = NA_character_,
        distance = NA_real_
      )
      
    } else {
      
      
      # curves <- copy(
      #   yield_by_region_norm[[region[1]]]
      # )
      
      
      curves <- copy(
        yield_by_region[[region[1]]]
      )
      
      #age_val <- mean(age)
      cohortTotals <- rowSums(
        .SD[, ..curve_cols],
        na.rm = TRUE
      )
      
      dominantRow <- which.max(cohortTotals)
      
      age_val <- .SD$age[dominantRow]
      
      
      
      
      curves[
        ,
        age_diff := abs(
          AC10 - age_val
        )
      ]
      
      curves <- curves[
        age_diff == min(age_diff)
      ]
      
      curves[
        ,
        age_diff := NULL
      ]
      if (pixelGroup[1] %in% c(17612, 10498, 14407)) {
        
        #cat("Candidate AUs AFTER age filter:\n")
        #print(unique(curves$AU))
      }
      pixel_total <- sum(cohort_vec)
      
      curve_total <- rowSums(
        curves[, curve_cols, with = FALSE],
        na.rm = TRUE
      )
      
      ratio <- pixel_total / curve_total
      ################
      
      ##############
      curves_filtered <- curves[
        ratio >= 0.6 &
          ratio <= (1 / 0.6)
      ]
      
      if (nrow(curves_filtered) > 0) {
        curves <- curves_filtered
      }
      curves_mat <- as.matrix(
        curves[, curve_cols, with = FALSE]
      )
      
      cohort_mat <- matrix(
        cohort_vec,
        nrow = nrow(curves_mat),
        ncol = length(cohort_vec),
        byrow = TRUE
      )
      
      dists <- sqrt(
        rowSums(
          (curves_mat - cohort_mat)^2
        )
      )
      
      
      
      best_idx <- which.min(dists)
      # if (best_dist > SOME_THRESHOLD) {
      #   
      #   all_curves <- rbindlist(
      #     yield_by_region,
      #     fill = TRUE
      #   )
      
      if (pixelGroup[1] %in% c(17612, 10498, 14407)) {
        
        tmp_debug <- data.table(
          AU = curves$AU,
          distance = dists
        )
        
        print(tmp_debug)
        
        cat("Selected AU:", curves$AU[best_idx], "\n")
      }
      list(
        bestAU = curves$AU[best_idx],
        distance = dists[best_idx]
      )
      
    }
    
  }, by = pixelGroup]
  
  
  # sim$yield_by_region_raw  <- yield_by_region
  # sim$yield_by_region      <- yield_by_region_norm
  # 
  sim$yield_by_region <- yield_by_region
  sim$classification <- results[
    ,
    .(
      pixelGroup,
      AU = bestAU,
      distance
    )
  ]
  
  sim$classification <- merge(
    sim$classification,
    AUtoCurve,
    by = "AU",
    all.x = TRUE
  )
  ###standDT
  # standDT <- sim$cohortData[
  #   ,
  #   .(
  #     age = as.numeric(names(which.max(table(age))))
  #   ),
  #   by = pixelGroup
  # ]
  standDT <- sim$cohortData[
    ,
    .(
      age = max(
        1,
        as.numeric(names(which.max(table(age))))
      )
    ),
    by = pixelGroup
  ]
  standDT <- merge(
    standDT,
    sim$classification[
      ,
      .(
        pixelGroup,
        AU,
        curveID
      )
    ],
    by = "pixelGroup",
    all.x = TRUE
  )
  
  standDT <- standDT[
    !is.na(AU) &
      !is.na(curveID)
  ]
  
  sim$standDT <- standDT
  
  ######################################
  sim$pixelGroupToAU <- results[
    ,
    .(
      pixelGroup,
      analysisUnit = bestAU
    )
  ]
  sim$AUtoCurve <- AUtoCurve
  # =====================================================
  # pixelAreaDT
  # =====================================================
  # Make sure both rasters are perfectly aligned
  if (!terra::compareGeom(
    sim$pixelGroupMap,
    sim$harvestableFraction,
    stopOnError = FALSE
  )) {
    
    
    
    stop(
      "pixelGroupMap and harvestableFraction are not aligned. ",
      "Effective area cannot be calculated."
    )
  }
  cellArea_ha <- prod(
    terra::res(sim$pixelGroupMap)
  ) / 10000
  
  hf <- as.vector(
    terra::values(sim$harvestableFraction)
  )
  
  pg <- as.vector(
    terra::values(sim$pixelGroupMap)
  )
  
  pixel_area_dt <- data.table(
    pixelGroup = pg,
    harvestableFraction = hf
  )
  
  pixel_area_dt <- pixel_area_dt[
    pixelGroup > 0
  ]
  
  pixel_area_dt <- pixel_area_dt[
    ,
    .(
      #harvestableFraction = sum(harvestableFraction, na.rm = TRUE),
      effectiveArea = sum(harvestableFraction * cellArea_ha, na.rm = TRUE)
    ),
    by = pixelGroup
  ]
  
  pixel_area_dt <- merge(
    pixel_area_dt,
    sim$pixelGroupToAU,
    by = "pixelGroup",
    all.x = TRUE
  )
  
  sim$pixelAreaDT <- pixel_area_dt
  # =====================================================
  # Area by Analysis Unit
  # =====================================================
  
  sim$areaByAU <- sim$pixelAreaDT[
    ,
    .(
      nPixelGroups = .N,
      effectiveArea = sum(effectiveArea, na.rm = TRUE)
    ),
    by = .(AU = analysisUnit)
  ]
  # =====================================================
  # Analysis Unit Summary
  # =====================================================
  
  sim$analysisUnitSummary <- merge(
    sim$areaByAU,
    sim$AUtoCurve,
    by = "AU",
    all.x = TRUE
  )
  
  sim$analysisUnitSummary[
    ,
    percentArea :=
      100 * effectiveArea /
      sum(effectiveArea, na.rm = TRUE)
  ]
  
  setorder(
    sim$analysisUnitSummary,
    -effectiveArea
  )
  # =====================================================
  # Add effectiveArea to standDT
  # =================================================
  
  standDT <- merge(
    sim$standDT,
    sim$pixelAreaDT[
      ,
      .(
        pixelGroup,
        effectiveArea
      )
    ],
    by = "pixelGroup",
    all.x = TRUE
  )
  
  sim$standDT <- standDT
  
  # =====================================================
  # analysisUnitMap
  # =====================================================
  
  analysisUnitMap <- sim$pixelGroupMap
  
  lookup <- sim$pixelGroupToAU
  
  pg_vals <- terra::values(
    sim$pixelGroupMap
  )
  
  idx <- match(
    pg_vals,
    lookup$pixelGroup
  )
  
  terra::values(analysisUnitMap) <-
    lookup$analysisUnit[idx]
  
  sim$analysisUnitMap <- analysisUnitMap
  if (is.null(sim$rawYieldTables)) {
    sim$rawYieldTables <- list()
  }
  
  sim$rawYieldTables$NL <- yield_by_region
  # Yield tables after species grouping and biomass conversion
  sim$yieldTables_NL <- yieldTables_NL
  
  # Clean parsed yield tables before species grouping
  sim$cleanOriginalYieldTables_NL <- cleanOriginalYieldTables_NL
  return(sim)
}