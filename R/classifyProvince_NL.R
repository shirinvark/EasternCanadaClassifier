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
  
  # ===================================================
  # STAGE 1: READ RAW YIELD CURVES
  # ===================================================
  #
  # Goal:
  #   Build the initial yield curve library for Newfoundland.
  #
  # At the end of this stage we should have all raw yield
  # table files and species mapping files available locally.
  #
  # Inputs:
  #   - Newfoundland yield table files (*.yld)
  #   - speciesGroups.txt
  #   - mapSpeciesGroups.txt
  #
  # Yield tables at this stage are still in their original
  # species-code format (e.g. WSv, BSv, BFv, TLv, etc.).
  #
  # No species grouping, standardization, classification,
  # distance calculations, or curve matching have occurred yet.
  #
  # Conceptually the output of this stage is:
  #
  #   Raw Yield Table Library
  #
  #        Region / Yield Table
  #                 ↓
  #            Raw Curves
  #                 ↓
  #            Age Classes
  #                 ↓
  #          Species Volumes
  #
  # Similar to Ontario, this stage is responsible only for:
  #
  #   ✓ downloading files
  #   ✓ reading raw yield tables
  #   ✓ preparing a local yield table library
  #
  # This stage does NOT:
  #
  #   ✗ rewrite species groups
  #   ✗ aggregate species
  #   ✗ convert to proportions
  #   ✗ assign regions to pixels
  #   ✗ compare cohorts to curves
  #   ✗ select best curves
  #
  # Those steps occur later in the classifier pipeline.
  #
  # Expected next stage:
  #
  #   Stage 2:
  #   Rewrite raw species codes into standardized
  #   Newfoundland species groups.
  #
  # ===================================================
  #تا اینجا درست
  
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
  # conversionFactor <- 1000 * 0.5 / 0.8
  
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
    print(headers)
    comm <- community
    reg  <- region
    header <- headers[
      community == comm &
        quality == "medium" &
        density == "d2" &
        region == reg
    ]
    cat("\n========================\n")
    cat("AU:", au, "\n")
    cat("Community:", comm, "\n")
    cat("Region:", reg, "\n")
    cat("Matched headers:", nrow(header), "\n")
    
    if (nrow(header) != 1) {
      print(
        headers[
          community == comm &
            quality == "medium" &
            density == "D2"
        ]
      )
    }
    
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
      
      print(tmp_check)
      
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
    
    dt_curve <- data.table(
      AC10 = ages
    )
    
    for (sp in names(curve_data)) {
      dt_curve[[sp]] <- curve_data[[sp]]
    }
    # # Steve:
    # # Convert yield volume (m3/ha) to approximate biomass (kg/ha)
    # 
    # dt_curve[
    #   ,
    #   (names(curve_data)) := lapply(
    #     .SD,
    #     function(x) x * conversionFactor
    #   ),
    #   .SDcols = names(curve_data)
    # ]
    
    if (au == "Central_BF") {
      
      cat("\n==============================\n")
      cat("RAW dt_curve BEFORE STANDARDIZATION\n")
      cat("==============================\n")
      
      print(dt_curve)
      
    }
    if (au == "Aphid_bF") {
      
      cat("\n==============================\n")
      cat("Aphid_bF BEFORE CLASSIFIER\n")
      cat("==============================\n")
      
      print(
        dt_curve[
          AC10 %in% c(0,10,20,30,40,50,60,70,80,90),
          .(
            AC10,
            blackSpruce_NL,
            balsamFir_NL,
            tamarack_NL,
            otherConifer_NL,
            broadleaf_NL
          )
        ]
      )
    }
    
    
    yieldTables_NL[[au]] <- dt_curve
    
  }
  # =========================================================
  # BUILD pixel_region FROM NL_YCF
  # =========================================================
  ####فعلا بمونه توی مدل چون هنوز پیکسل گروپ نداریم
  
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
  
  print(region_raster)
  
  print(terra::cats(region_raster))
  
  print(levels(region_raster))
  
  
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
  
  print(lut)
  
  
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
  cat("\n===== AGE DISTRIBUTION =====\n")
  
  print(
    cohort_wide[
      ,
      .N,
      by = age
    ][order(age)]
  )
  print(sort(unique(cohort_wide$age))[1:30])
  cohort_wide[
    ,
    .N,
    by = age
  ][order(age)]
  group_cols <- setdiff(
    names(cohort_wide),
    c("pixelGroup", "age", "total")
  )
  
  cohort_wide[
    ,
    total := rowSums(.SD),
    .SDcols = group_cols
  ]
  cat("\n===== AGE DISTRIBUTION =====\n")
  
  print(
    cohort_wide[
      ,
      .N,
      by = age
    ][order(age)]
  )
  print("===== COHORT WIDE CHECK =====")
  print(head(cohort_wide))
  
  print(summary(cohort_wide$total))
  
  print(any(is.na(cohort_wide$total)))
  
  cohort_wide <- cohort_wide[
    total > 0
  ]
  
  print(names(cohort_wide))
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
  
  #cohort_wide[
  #,
  # (prop_cols) := lapply(
  # .SD,
  #function(x) x / total
  #  ),
  # .SDcols = prop_cols
  #]
  
  ####pixel by region
  # shp2 <- terra::project(
  # shp,
  # terra::crs(sim$pixelGroupMap)
  #)
  #print(region_raster)
  
  # print(
  #terra::freq(
  # region_raster,
  # digits = 0
  # )
  # )
  # summary(
  #  terra::values(region_raster)
  #)
  #region_raster <- terra::rasterize(
  #shp2,
  #sim$pixelGroupMap,
  # field = "YCF"
  #)
  
  #pg <- data.table::as.data.table(
  #terra::values(sim$pixelGroupMap)
  #)
  
  #  reg <- data.table::as.data.table(
  #   terra::values(region_raster)
  #)
  
  #setnames(pg, names(pg), "pixelGroup")
  #setnames(reg, names(reg), "region")
  
  #pixel_region <- cbind(pg, reg)
  #cat("\n===== BEFORE FILTER =====\n")
  
  #print(head(pixel_region, 20))
  
  #print(dim(pixel_region))
  #pixel_region <- pixel_region[
  #!is.na(pixelGroup) &
  #   !is.na(region)
  #]
  
  #pixel_region[
  #,
  #region := gsub(
  #"^NL_",
  # "",
  #  as.character(region)
  # )
  #]
  
  
  
  #lut <- levels(region_raster)[[1]]
  
  # pixel_region[
  #  ,
  # region := lut$YCF[
  #  match(region, lut$ID)
  #]
  #]
  
  #pixel_region[
  # ,
  #region := gsub(
  # "^NL_",
  #"",
  #region
  #)
  #]
  
  ##yield by region
  
  yield_by_region <- list()
  
  for (reg in unique(AU_table$region)) {
    
    region_aus <- AU_table[
      region == reg,
      AU
    ]
    cat("\n===== yieldTables_NL =====\n")
    print(length(yieldTables_NL))
    print(head(names(yieldTables_NL), 10))
    
    cat("\n===== region_aus =====\n")
    print(region_aus)
    region_list <- lapply(
      region_aus,
      function(au) {
        
        dt <- copy(
          yieldTables_NL[[au]]
        )
        print(class(yieldTables_NL[[au]]))
        print(is.data.table(yieldTables_NL[[au]]))
        dt[, AU := au]
        
        dt[, zone := reg]
        
        dt
      }
    )
    
    yield_by_region[[reg]] <- rbindlist(
      region_list,
      fill = TRUE
    )
    
    if (reg == "Aphid") {
      
      cat("\n===== APHID TOTAL BY AGE =====\n")
      
      print(
        yield_by_region[["Aphid"]][
          AC10 %in% c(10,20,30,40,50,60,70),
          .(
            total =
              blackSpruce_NL +
              balsamFir_NL +
              tamarack_NL +
              otherConifer_NL +
              broadleaf_NL
          ),
          by = .(AU, AC10)
        ]
      )
      
    }   ## ← این آکولاد خیلی مهم است
    
  }   ## ← این هم پایان حلقه for
  cat("\n===== BAREAST TOTAL BY AGE =====\n")
  
  print(
    yield_by_region[["BarEast"]][
      AC10 %in% c(10,20,30,40,50,60,70,80),
      .(
        total =
          blackSpruce_NL +
          balsamFir_NL +
          tamarack_NL +
          otherConifer_NL +
          broadleaf_NL
      ),
      by = .(AU, AC10)
    ]
  )
  cat("\n===== yield_by_region names =====\n")
  print(names(yield_by_region))
  
  cat("\n===== First region =====\n")
  print(names(yield_by_region)[1])
  
  cat("\n===== Columns =====\n")
  print(names(yield_by_region[[1]]))
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
    .(region = region[1]),
    by = pixelGroup
  ]
  
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
  cat("\n===== curve_cols =====\n")
  print(curve_cols)
  
  cat("\n===== cohort_wide =====\n")
  print(names(cohort_wide))
  
  cat("\n===== yield_by_region =====\n")
  print(names(yield_by_region[[1]]))
  cat("\n===== AGE 10 CURVES =====\n")
  
  print(
    yield_by_region[[1]][
      AC10 == 10,
      .(
        AU,
        blackSpruce_NL,
        balsamFir_NL,
        tamarack_NL,
        otherConifer_NL,
        broadleaf_NL
      )
    ]
  )
  cat("\n===== curve_cols =====\n")
  print(curve_cols)
  
  cat("\n===== cohort columns =====\n")
  print(names(cohort_wide))
  
  cat("\n===== yield columns =====\n")
  print(names(yield_by_region[[1]]))
  results <- cohort_classifiable[, {
    if (.GRP %% 10000 == 0)
      cat("Processed:", .GRP, "\n")
    
    cohort_vec <- unlist(
      .SD[1, curve_cols, with = FALSE],
      use.names = FALSE
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
      age_val <- age[1]
      
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
      pixel_total <- sum(cohort_vec)
      
      curve_total <- rowSums(
        curves[, curve_cols, with = FALSE],
        na.rm = TRUE
      )
      
      ratio <- pixel_total / curve_total
      ################
      if (!exists(".debugDone", inherits = FALSE)) {
        
        assign(".debugDone", TRUE, envir = .GlobalEnv)
        
        cat("\n=============================\n")
        cat("DEBUG FIRST PIXEL\n")
        cat("=============================\n")
        
        cat("PixelGroup:", .BY$pixelGroup, "\n")
        cat("Region:", region, "\n")
        cat("Age:", age_val, "\n")
        
        cat("\nPixel biomass by group:\n")
        print(cohort_vec)
        
        cat("\nPixel total biomass:\n")
        print(pixel_total)
        
        cat("\nCurve totals:\n")
        print(summary(curve_total))
        
        cat("\nRatio:\n")
        print(summary(ratio))
        
        cat("\nCandidate curves:\n")
        print(nrow(curves))
        
        cat("\nSelected AC10:\n")
        print(unique(curves$AC10))
        
        cat("\nCandidate AUs:\n")
        print(unique(curves$AU))
      }
      ##############
      curves_filtered <- curves[
        ratio >= 0.6 &
          ratio <= (1 / 0.6)
      ]
      if (age_val > 0 && !exists(".printedDebug", inherits = FALSE)) {
        
        .printedDebug <- TRUE
        
        cat("\n==============================\n")
        cat("FIRST NONZERO AGE PIXEL\n")
        cat("==============================\n")
        
        cat("\nPixel age:\n")
        print(age_val)
        
        cat("\nSelected curve ages:\n")
        print(unique(curves$AC10))
        
        cat("\nPixel total:\n")
        print(pixel_total)
        
        cat("\nCurve totals:\n")
        print(summary(curve_total))
        
        cat("\nRatio:\n")
        print(summary(ratio))
        
        cat("\nBefore filter:\n")
        print(nrow(curves))
        
        cat("\nAfter filter:\n")
        print(nrow(curves_filtered))
      }
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
      
      list(
        bestAU = curves$AU[best_idx],
        distance = dists[best_idx]
      )
      
    }
    
  }, by = .(
    pixelGroup,
    region,
    age
  )]
  
  
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
  standDT <- sim$cohortData[
    ,
    .(
      age = as.numeric(names(which.max(table(age))))
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