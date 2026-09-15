classifyProvince_NL <- function(sim) {
  
  message("Running Newfoundland classifier")
  
  # ===================================================
  # SET UP NEWFOUNDLAND DATA PATHS
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
  # =========================================================
  # LOAD CLASSIFIER HELPER FUNCTIONS
  # =========================================================
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
  # ===================================================
  # BUILD YIELD CURVE INVENTORY
  # ===================================================
 
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
  # ===================================================
  # BUILD ANALYSIS UNIT (AU) TABLE
  # ===================================================
  # Define each unique community-region combination as an AU
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
  
  # ===================================================
  # BUILD AU-TO-CURVE LOOKUP
  # ===================================================
  # Assign each AU to its corresponding yield curve
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
  # Link each AU region to its source YLD file
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
  # ===================================================
  # BUILD NEWFOUNDLAND YIELD TABLES
  # ===================================================
  # Initialize storage for source files and processed yield tables
  file_cache <- list()
  yieldTablesVolume_NL  <- list()
  yieldTablesBiomass_NL <- list()
  cleanOriginalYieldTables_NL <- list()
  
  for (i in seq_len(nrow(AU_table))) {
    # Get the AU, region, community, and source YLD file
    
    au <- AU_table$AU[i]
    region <- AU_table$region[i]
    community <- AU_table$community[i]
    file <- AU_table$file[i]
    # Read each YLD file once and store it in the file cache
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
    # Select the matching medium-density (d2) yield curve for this AU
    headers <- parseYLDHeaders(lines)
    comm <- community
    reg  <- region
    header <- headers[
      community == comm &
        quality == "medium" &
        density == "d2" &
        region == reg
    ]
    # Confirm that exactly one yield curve matches the current AU
    stopifnot(nrow(header) == 1)
    # Parse species information and volume data from the selected curve
    speciesInfo <- parseSpecies(
      lines,
      startLine = header$lineNumber
    )
    raw_curve <- parseVolumes(
      lines,
      speciesInfo
    )
    cleanOriginalYieldTables_NL[[au]] <- raw_curve    
    # Aggregate original species into classifier species groups
    curve_data <- rewrite_yld_curve(
      raw_curve,
      mapSpeciesGroups
    )
    # Standardize yield curves to annual age classes up to age 255
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
  
    # Keep volume version for AAC / Hanzlik
    dt_curve_volume <- copy(dt_curve)
    # Convert yield volume (m3/ha) to approximate biomass (kg/ha)
    # for comparison with cohort biomass
    dt_curve_biomass <- copy(dt_curve_volume)
    
    conversionFactor <- 1000 * 0.5 / 0.8
    
    species_cols <- setdiff(names(dt_curve_biomass), "AC10")
    
    dt_curve_biomass[
      ,
      (species_cols) := lapply(
        .SD,
        function(x) x * conversionFactor
      ),
      .SDcols = species_cols
    ]
    yieldTablesVolume_NL[[au]]  <- dt_curve_volume
    yieldTablesBiomass_NL[[au]] <- dt_curve_biomass
    
  }   
  
 
  # =========================================================
  # BUILD PIXEL-TO-REGION LOOKUP
  # =========================================================
  
  if (!("yieldCurveFamily" %in% names(sim))) {
    stop("sim$yieldCurveFamily is missing.")
  }
  
  if (!("yieldCurveLookup" %in% names(sim))) {
    stop("sim$yieldCurveLookup is missing.")
  }
  
  if (!terra::compareGeom(
    sim$yieldCurveFamily,
    sim$pixelGroupMap,
    stopOnError = FALSE
  )) {
    stop("yieldCurveFamily and pixelGroupMap are not aligned.")
  }
  # Extract pixel group and Yield Curve Family ID for each raster cell
  pixel_region <- data.table::data.table(
    pixelGroup = as.vector(
      terra::values(sim$pixelGroupMap)
    ),
    YCF_ID = as.vector(
      terra::values(sim$yieldCurveFamily)
    )
  )
  # Keep only cells with both pixel group and YCF information
  pixel_region <- pixel_region[
    !is.na(pixelGroup) &
      !is.na(YCF_ID)
  ]
  # Match YCF IDs to Newfoundland Yield Curve Family names
  lookup <- data.table::as.data.table(
    sim$yieldCurveLookup
  )
  
  pixel_region <- merge(
    pixel_region,
    lookup[
      Province == "NL",
      .(
        YCF_ID = ID,
        YCF
      )
    ],
    by = "YCF_ID",
    all.x = TRUE
  )
  # Keep only cells successfully matched to a Newfoundland YCF
  pixel_region <- pixel_region[
    !is.na(YCF)
  ]
  # Convert YCF names to Newfoundland region names
  # Example: "NL_Aphid" -> "Aphid"
  pixel_region[
    ,
    region := sub(
      "^NL_",
      "",
      as.character(YCF)
    )
  ]
  # Remove intermediate YCF fields
  pixel_region[
    ,
    c("YCF_ID", "YCF") := NULL
  ]
  
  sim$pixel_region <- pixel_region
  # ===================================================
  # LOAD AND GROUP COHORT DATA
  # ===================================================
  
  cohortDT <- data.table::as.data.table(
    sim$cohortData
  )
  # Map cohort species codes to Newfoundland classifier species groups
  cohortDT[
    ,
    speciesCode := as.character(speciesCode)
  ]
  
  cohortDT[
    ,
    final_group := speciesGroups[[speciesCode]],
    by = speciesCode
  ]
  # Remove species that are not included in the species-group mapping
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
  # Reshape species-group biomass into separate columns
  cohort_wide <- dcast(
    cohort_group,
    pixelGroup + age ~ final_group,
    value.var = "B",
    fill = 0
  )
  # Calculate total biomass across species groups
  group_cols <- setdiff(
    names(cohort_wide),
    c("pixelGroup", "age", "total")
  )
  
  cohort_wide[
    ,
    total := rowSums(.SD),
    .SDcols = group_cols
  ]
  # Remove cohort rows with no biomass
  cohort_wide <- cohort_wide[
    total > 0
  ]
  # Identify species groups shared by cohort data and yield curves
  group_cols <- setdiff(
    names(cohort_wide),
    c("pixelGroup", "age")
  )
  prop_cols <- intersect(
    groups,
    group_cols
  )
  prop_cols <- prop_cols[!is.na(prop_cols)]
  # Stop if cohort data and yield curves have no species groups in common
  if (length(prop_cols) == 0) {
    stop("No matching species groups between cohort and yield curves.")
  }
  # ===================================================
  # ORGANIZE YIELD TABLES BY REGION
  # ===================================================
  
  # Keep separate regional tables for volume and biomass
  
  yield_by_region_volume  <- list()
  yield_by_region_biomass <- list()
  
  for (reg in unique(AU_table$region)) {
    # Get all analysis units belonging to the current region
    region_aus <- AU_table[
      region == reg,
      AU
    ]
    
    # Combine volume yield tables for all AUs in the region
    region_list_volume <- lapply(
      region_aus,
      function(au) {
        
        dt <- copy(
          yieldTablesVolume_NL[[au]]
        )
        
        dt[, AU := au]
        dt[, zone := reg]
        
        dt
      }
    )
    
    yield_by_region_volume[[reg]] <- rbindlist(
      region_list_volume,
      fill = TRUE
    )
    # Combine biomass yield tables for all AUs in the region
    region_list_biomass <- lapply(
      region_aus,
      function(au) {
        
        dt <- copy(
          yieldTablesBiomass_NL[[au]]
        )
        
        dt[, AU := au]
        dt[, zone := reg]
        
        dt
      }
    )
    
    yield_by_region_biomass[[reg]] <- rbindlist(
      region_list_biomass,
      fill = TRUE
    )
    
  } 
  
  # =========================================================
  # NORMALIZE CURVES ONCE
  # =========================================================
  
  curve_cols <- prop_cols
  sim$yield_by_region <- yield_by_region_biomass
  
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
      
      
      
      
      curves <- copy(
        yield_by_region_biomass[[region[1]]]
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
  sim$yield_by_region <- yield_by_region_biomass
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
  if (is.null(sim$processedYieldTables)) {
    sim$processedYieldTables <- list()
  }
  
  sim$processedYieldTables$NL <- yield_by_region_volume
  # Yield tables after species grouping and biomass conversion
  sim$yieldTablesVolume_NL  <- yieldTablesVolume_NL
  sim$yieldTablesBiomass_NL <- yieldTablesBiomass_NL
  
  # Clean parsed yield tables before species grouping
  sim$cleanOriginalYieldTables_NL <- cleanOriginalYieldTables_NL
  return(sim)
}