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
    cat("\nFILE:", f, "\n")
    
    print(
      table(
        sub(
          "^\\*Y\\s+\\S+\\s+(\\S+).*",
          "\\1",
          y_lines
        )
      )
    )
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
    
    block <- extract_curve_block(
      lines,
      community = community,
      region = region
    )
    ####For test
    raw_curve <- parse_curve(block)
    if (au == "Aphid_bF") {
      cat("\n====================\n")
      cat("AU =", au, "\n")
      cat("community =", community, "\n")
      cat("region =", region, "\n")
      cat("====================\n")
      
      cat("\n===== RAW CURVE =====\n")
      
      print(head(raw_curve$BFv, 30))
      print(head(block, 30))
    }
    curve_data <- rewrite_yld_curve(
      raw_curve,
      mapSpeciesGroups
    )
    
    if (au == "Aphid_bF") {
      
      cat("\n===== RAW BFv =====\n")
      print(raw_curve$BFv)
      
    }
    
    if (au == "Aphid_bF") {
      
      cat("\n===== REWRITTEN balsamFir_NL =====\n")
      print(curve_data$balsamFir_NL)
      
    }
    
    
    
    
    if (au == "Aphid_bF") {
      
      cat("\n===== REWRITTEN CURVE =====\n")
      
      print(head(curve_data$balsamFir_NL, 30))
      
    }
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
      by = 10,
      length.out = length(curve_data[[1]])
    )
    
    dt_curve <- data.table(
      AC10 = ages
    )
    if (au == "Aphid_bF") {
      
      cat("\n===== DT CURVE =====\n")
      
      print(dt_curve)
      
    }
    for (sp in names(curve_data)) {
      dt_curve[[sp]] <- curve_data[[sp]]
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
  cat("\n===== AFTER FILTER =====\n")
  
  print(head(pixel_region, 20))
  
  print(dim(pixel_region))
  
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
  
  
  group_cols <- setdiff(
    names(cohort_wide),
    c("pixelGroup", "age", "total")
  )
  
  cohort_wide[
    ,
    total := rowSums(.SD),
    .SDcols = group_cols
  ]
  
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
  
  cohort_wide[
    ,
    (prop_cols) := lapply(
      .SD,
      function(x) x / total
    ),
    .SDcols = prop_cols
  ]
  
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
  }
  
  
  # =========================================================
  # NORMALIZE CURVES ONCE
  # =========================================================
  
  curve_cols <- prop_cols
  
  yield_by_region_norm <- lapply(
    yield_by_region,
    function(dt) {
      
      dt <- copy(dt)
      
      dt[
        ,
        total := rowSums(.SD),
        .SDcols = curve_cols
      ]
      
      dt[
        ,
        (curve_cols) := lapply(
          .SD,
          function(x) x / total
        ),
        .SDcols = curve_cols
      ]
      
      dt
    }
  )
  cat("\n===== PIXEL REGION =====\n")
  print(head(pixel_region))
  print(nrow(pixel_region))
  
  cat("\n===== COHORT PIXELS =====\n")
  
  cat("\n===== INTERSECTION =====\n")
  
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
  print("===== COHORT CLASSIFIABLE =====")
  print(head(cohort_classifiable))
  
  print(names(cohort_classifiable))
  
  print(
    cohort_classifiable[
      1:6,
      .(
        pixelGroup,
        age,
        region,
        balsamFir_NL,
        blackSpruce_NL,
        broadleaf_NL,
        otherConifer_NL,
        total
      )
    ]
  )
  # ======================================================
  # CLASSIFY ONE PIXEL
  # ====================================================
  cat("\n===== YIELD REGIONS =====\n")
  print(names(yield_by_region_norm))
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
      
      
      curves <- copy(
        yield_by_region_norm[[region[1]]]
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
  
  
  sim$yield_by_region_raw  <- yield_by_region
  sim$yield_by_region      <- yield_by_region_norm
  
  classification <- results[
    ,
    .(
      pixelGroup,
      analysisUnit = bestAU,
      distance
    )
  ]
  
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
  sim$pixelGroupToAU <- classification[
    ,
    .(
      pixelGroup,
      analysisUnit
    )
  ]
  sim$AUtoCurve <- AUtoCurve
  # =====================================================
  # pixelAreaDT
  # =====================================================
  
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
    !is.na(pixelGroup)
  ]
  
  pixel_area_dt <- pixel_area_dt[
    ,
    .(
      harvestableFraction = sum(harvestableFraction, na.rm = TRUE),
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
  # AAC input table
  # =====================================================
  cat("\n===== STANDDT AFTER AREA MERGE =====\n")
  
  print(names(standDT))
  
  print(head(standDT))
  sim$aacInput <- standDT[
    ,
    .(
      area = sum(
        effectiveArea,
        na.rm = TRUE
      )
    ),
    by = .(
      AU,
      age,
      curveID
    )
  ]
  
  setorder(
    sim$aacInput,
    AU,
    age
  )
  
  cat("\n===== AAC INPUT =====\n")
  
  print(
    head(sim$aacInput)
  )
  
  print(
    summary(sim$aacInput$area)
  )
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
  
  #sim$rawYieldTables$NL <- yield_by_regionپ
  sim$rawYieldTables$NL <- sim$yield_by_region_raw
  sim$yieldTables_NL <- yieldTables_NL
  return(sim)
}