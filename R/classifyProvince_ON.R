classifyProvince_ON <- function(sim) {
  
  library(data.table)
  
  # =========================================================
  # Download Ontario mapping files and yield tables.
  # Files are downloaded only if they are not already present
  # in the local input directory.
  # =========================================================
  
  on_dir <- file.path(getPaths()$inputPath, "ON")
  dir.create(on_dir, recursive = TRUE, showWarnings = FALSE)
  
  ytf_dir <- file.path(on_dir, "YTF")
  dir.create(ytf_dir, recursive = TRUE, showWarnings = FALSE)
  
  base_url <- "https://raw.githubusercontent.com/shirinvark/EasternCanadaClassifier/main/data/ON/"
  
  # ---- mapping files ----
  mapping_files <- c(
    "speciesGroups_ON.txt",
    "mapSpeciesGroups.txt"
  )
  
  for (f in mapping_files) {
    dest <- file.path(on_dir, f)
    if (!file.exists(dest)) {
      download.file(paste0(base_url, f), destfile = dest, mode = "wb")
    }
  }
  
  # ---- yield tables ----
  ytf_files <- c(
    "3e_tbl_yield_final.csv",
    "3w_tbl_yield_final.txt",
    "4e_5e_tbl_yield_final.txt",
    "4s_3s_tbl_yield_final.csv",
    "4w_tbl_yield_final.txt",
    "5e_tbl_yield_final.txt"
  )
  
  for (f in ytf_files) {
    dest <- file.path(ytf_dir, f)
    if (!file.exists(dest)) {
      download.file(paste0(base_url, "YTF/", f), destfile = dest, mode = "wb")
    }
  }
  speciesGroups <- read_curve_mapping(file.path(on_dir, "speciesGroups_ON.txt"))
  mapSpeciesGroups <- read_curve_mapping(file.path(on_dir, "mapSpeciesGroups.txt"))
  sim$mapSpeciesGroups <- mapSpeciesGroups
  groups    <- unique(unlist(mapSpeciesGroups))
  groups <- groups[!is.na(groups)]
  groups <- groups[groups != ""]
  # =========================================================
  # Process a single Ontario yield-table region.
  #
  # Steps:
  #   1. Read raw yield table.
  #   2. Filter to the appropriate ecological region.
  #   3. Aggregate species into LandR species groups.
  #   4. Convert volume to approximate biomass.
  #   5. Build standardized analysis-unit yield table.
  # =========================================================
  
  stopifnot(inherits(sim$pixelGroupMap, "SpatRaster"))
  stopifnot(inherits(sim$harvestableFraction, "SpatRaster"))
  
  process_zone <- function(path, submu) {
    dt <- fread(path)
    # ======================================
    # explicit Ontario filtering
    # ======================================
    
    if (submu == "3e") {
      
      dt <- dt[
        tolower(SUBMU) == "3e" &
          tolower(SI) == "prsnt"
      ]
      
    } else if (submu == "3w") {
      
      dt <- dt[
        tolower(SUBMU) == "3w" &
          tolower(SI) == "prsnt"
      ]
      
    } else if (submu == "4e") {
      
      dt <- dt[
        tolower(SUBMU) == "4e_5e" &
          tolower(SI) == "prsnt"
      ]
      
    } else if (submu == "4s") {
      
      dt <- dt[
        tolower(SUBMU) == "4s_3s" &
          tolower(SI) == "prsnt"
      ]
      
    } else if (submu == "4w") {
      
      dt <- dt[
        tolower(SUBMU) == "4w" &
          tolower(SI) == "prsnt"
      ]
      
    } else if (submu == "5e") {
      
      dt <- dt[
        tolower(SUBMU) == "5e" &
          tolower(SI) == "exten"
      ]
      
    }
    
    # safety
    if (nrow(dt) == 0) {
      warning(paste("No data after filtering for zone:", submu))
      return(NULL)
    }
    # ---- Species columns ----
    species_cols <- c("PW","PR","PJ","SB","SW","BF","CE","OC","HE","PO","PB","BW","MH","QR","YB","OH")
    
    # ---- Convert to numeric ----
    dt[, (species_cols) := lapply(.SD, function(x) {
      x[x == "." | x == "" | is.na(x)] <- NA
      as.numeric(x)
    }), .SDcols = species_cols]
    
    
    # -------------------------------------------------------
    # Calculate total biomass across species for each yield
    # table record. Species values remain as absolute biomass
    # and are not converted to proportions.
    # -------------------------------------------------------
    dt[, total := rowSums(.SD, na.rm = TRUE), .SDcols = species_cols]
    if (sum(dt$total) == 0) {
      next
    }
    
    # ---- Initialize group columns ----
    for (g in groups) {
      dt[[g]] <- 0
    }
    
    # ---- Mapping species → groups ----
    
    for (sp in species_cols) {
      
      key <- trimws(sp)
      final_group <- mapSpeciesGroups[[key]]
      
      if (is.null(final_group)) {
        
        next
      }
      
      dt[[final_group]] <- dt[[final_group]] + dt[[sp]]
    }
    
    # ---- Aggregate to curve level ----
    
    dt_summary <- dt[, lapply(.SD, sum, na.rm = TRUE),
                     by = .(CURVENO, AC10, FU),
                     .SDcols = groups]
    
    # Steve:
    # Convert yield volume (m3/ha) to approximate biomass (kg/ha)
    conversionFactor <- 1000 * 0.5 / 0.8
    
    dt_summary[
      ,
      (groups) := lapply(
        .SD,
        function(x) x * conversionFactor
      ),
      .SDcols = groups
    ]
    
    # ---- Remove NA ----
    dt_summary <- dt_summary[
      complete.cases(dt_summary[, ..groups])
    ]
    
    # ---- Add zone ----
    dt_summary[, zone := submu]
    
    # ---- Assign AU ----
    dt_summary[
      ,
      AU := paste0(
        FU,
        "_",
        zone
      )
    ]
    
    return(dt_summary)
  }
  
  # =========================================================
  # 2. BUILD YIELD TABLE
  # =========================================================
  
  zones <- list(
    list(path = file.path(ytf_dir, "3e_tbl_yield_final.csv"), submu = "3e"),
    list(path = file.path(ytf_dir, "3w_tbl_yield_final.txt"), submu = "3w"),
    list(path = file.path(ytf_dir, "4e_5e_tbl_yield_final.txt"), submu = "4e"),
    list(path = file.path(ytf_dir, "4s_3s_tbl_yield_final.csv"), submu = "4s"),
    list(path = file.path(ytf_dir, "4w_tbl_yield_final.txt"), submu = "4w"),
    list(path = file.path(ytf_dir, "5e_tbl_yield_final.txt"), submu = "5e")
  )
  results_list <- lapply(zones, function(z) {
    process_zone(z$path, z$submu)
  })
  
  # فقط خروجی‌های غیر NULL نگه دار
  results_list <- results_list[!sapply(results_list, is.null)]
  
  # بعد combine کن
  yield_all <- rbindlist(results_list, fill = TRUE)
  
  yield_by_region <- split(yield_all, yield_all$zone)
  
  sim$yield_by_region <- yield_by_region
  
  if (is.null(sim$rawYieldTables)) {
    sim$rawYieldTables <- list()
  }
  
  sim$rawYieldTables$ON <- yield_by_region
  # =========================================================
  # 2.5 BUILD pixel_region FROM SHAPEFILE
  # =========================================================
  
  on_dir <- file.path(getPaths()$inputPath, "ON")
  dir.create(on_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(on_dir, "combined_regions.zip")
  
  shp_path <- file.path(
    on_dir,
    "combined_regions.shp"
  )
  
  if (!file.exists(shp_path)) {
    
    zip_url <- paste0(
      "https://raw.githubusercontent.com/",
      "shirinvark/EasternCanadaClassifier/main/",
      "data/ON/combined_regions.zip"
    )
    
    download.file(
      zip_url,
      destfile = zip_path,
      mode = "wb"
    )
    
    unzip(
      zip_path,
      exdir = on_dir
    )
  }
  
  shp <- terra::vect(shp_path)
  
  # =========================================================
  # BUILD pixel_region (MISSING PART - FIX)
  # =========================================================
  
  # ---- project ----
  shp <- terra::project(shp, terra::crs(sim$pixelGroupMap))  
  
  # ---- rasterize ----
  region_raster <- terra::rasterize(
    shp,
    sim$pixelGroupMap,
    field = "SITEREGION",
    touches = TRUE
  )
  print(unique(terra::values(region_raster)))
  print(compareGeom(
    sim$pixelGroupMap,
    region_raster,
    stopOnError = FALSE
  ))
  # ---- extract values ----
  pg  <- as.data.table(terra::values(sim$pixelGroupMap))
  reg <- as.data.table(terra::values(region_raster))
  
  setnames(pg, names(pg), "pixelGroup")
  setnames(reg, names(reg), "region")
  
  pixel_region <- cbind(pg, reg)
  cats_dt <- as.data.table(
    terra::cats(region_raster)[[1]]
  )
  
  cats_dt[
    ,
    SITEREGION := tolower(SITEREGION)
  ]
  
  setnames(cats_dt, "ID", "region")
  
  pixel_region <- merge(
    pixel_region,
    cats_dt,
    by = "region",
    all.x = TRUE
  )
  
  pixel_region[
    ,
    region := SITEREGION
  ]
  
  pixel_region[
    ,
    SITEREGION := NULL
  ]
  ########################################################
  # Ensure region identifiers are stored as character values.
  pixel_region[, region := as.character(region)]
  
  cat("\n===== DUPLICATE CHECK =====\n")
  print(
    pixel_region[
      ,
      .N,
      by = pixelGroup
    ][
      order(-N)
    ][1:20]
  )  # ---- clean ----
  #pixel_region[, region := tolower(as.character(region))]
  ##############################################################3 
  pixel_region <- pixel_region[
    !is.na(pixelGroup) & !is.na(region)
  ]
  
  # ---- save in sim ----
  sim$pixel_region <- pixel_region
  # =========================================================
  # 3. CLASSIFIER
  # =========================================================
  cohortDT <- as.data.table(sim$cohortData)
  cohortDT[, speciesCode := as.character(speciesCode)]
  # browser() 
  pg_col <- grep("pixelgroup", names(cohortDT), ignore.case = TRUE, value = TRUE)  
  if (length(pg_col) != 1) {
    stop("❌ pixelGroup column not found or duplicated in cohortDT")
  }
  
  setnames(cohortDT, pg_col, "pixelGroup")
  
  # group species to analysis units
  #cohortDT[, final_group := speciesGroups[[speciesCode]]]
  cohortDT[, final_group := speciesGroups[[speciesCode]], by = speciesCode]  # remove unmapped
  cohortDT <- cohortDT[!is.na(final_group)]
  cat("\n===== STEP 1 =====\n")
  
  cat(
    "Unique pixelGroups in cohortDT:",
    uniqueN(cohortDT$pixelGroup),
    "\n"
  )
  cat("\n========== AFTER final_group FILTER ==========\n")
  
  cat(
    "PixelGroup 85 exists:",
    85 %in% cohortDT$pixelGroup,
    "\n"
  )
  
  print(
    cohortDT[
      pixelGroup == 85
    ]
  )
  # aggregate biomass
  cohort_group <- cohortDT[, .(
    B = sum(B, na.rm = TRUE)
  ), by = .(pixelGroup, age, final_group)]
  #####  # convert to wide
  cohort_wide <- dcast(
    cohort_group,
    pixelGroup + age ~ final_group,
    value.var = "B",
    fill = 0
  )
  cat("\n===== STEP 2 =====\n")
  
  cat(
    "Unique pixelGroups in cohort_wide:",
    uniqueN(cohort_wide$pixelGroup),
    "\n"
  )
  cat(
    "\n===== COHORT_WIDE =====\n"
  )
  
  cat(
    "Unique pixelGroups:",
    uniqueN(cohort_wide$pixelGroup),
    "\n"
  )
  
  cat(
    "Missing pixelGroups still present:",
    sum(unique(cohort_wide$pixelGroup) %in% missingPG),
    "\n"
  )
  # force pixelGroup name
  pg_col <- names(cohort_wide)[grepl("pixelgroup", names(cohort_wide), ignore.case = TRUE)]
  
  if (length(pg_col) != 1) {
    stop("❌ Could not uniquely identify pixelGroup column")
  }
  
  setnames(cohort_wide, pg_col, "pixelGroup")
  if (!"pixelGroup" %in% names(cohort_wide)) {
    stop("❌ pixelGroup column still missing in cohort_wide")
  }
  
  # Identify the species-group columns used for
  # biomass comparison.
  group_cols <- setdiff(names(cohort_wide), c("pixelGroup","age"))
  prop_cols <- intersect(groups, group_cols)
  prop_cols <- prop_cols[!is.na(prop_cols)]
  if (length(prop_cols) == 0) {
    stop("❌ No matching species groups between cohort and yield")
  }
  #######Steve is talking about this part
  cohort_wide[, total := rowSums(.SD), .SDcols = group_cols]
  #this line commented for steve suggestion
  #cohort_wide[, (group_cols) := lapply(.SD, function(x) x / total),
  #            .SDcols = group_cols]
  
  cohort_wide <- cohort_wide[total > 0]
  
  cat("\n===== STEP 3 =====\n")
  
  cat(
    "Unique pixelGroups after total > 0:",
    uniqueN(cohort_wide$pixelGroup),
    "\n"
  )
  
  
  
  
  
  pixel_region <- sim$pixel_region
  
  
  pg <- terra::values(sim$pixelGroupMap)[,1]
  
  missingPG <- setdiff(
    unique(pg[!is.na(pg)]),
    cohort_wide$pixelGroup
  )
  
  
  results <- cohort_wide[, {
    
    cohort_vec <- as.numeric(.SD[1, prop_cols, with = FALSE])
    
    if (sum(cohort_vec) == 0) {
      list(
        bestAU = NA_character_,
        CURVENO = NA_integer_,
        distance = NA_real_
      )
    } else {
      # Steve: compare using total biomass (kg/ha), not proportions
      # cohort_vec <- cohort_vec / sum(cohort_vec)      
      region_vals <- pixel_region[pixelGroup == .BY$pixelGroup, region]
      
      region <- if (length(region_vals) == 0) {
        "3e"
      } else {
        names(which.max(table(region_vals)))
      }
      # browser()
      if (!(region %in% names(yield_by_region))) {
        list(
          bestAU = NA_character_,
          CURVENO = NA_integer_,
          distance = NA_real_
        )
      } else {
        
        curves <- copy(
          sim$yield_by_region[[region]]
        )
        
        
        
        age <- mean(.SD$age)
        
        curves[, age_diff := abs(AC10 - age)]
        curves <- curves[age_diff == min(age_diff)]
        curves[, age_diff := NULL]
        if (nrow(curves) == 0) {
          
          list(
            bestAU = NA_character_,
            CURVENO = NA_integer_,
            distance = NA_real_
          )
        } else {
          
          
          
          ####PROPORTIONS
          ##and also this part: WE HAD TO DO SOMETHING FOR AGE O ROES.FOR NOW THEY DOES NOT ENTER
          #curves[
          #  ,
          # total_prop := rowSums(
          #  .SD,
          # na.rm = TRUE
          #  ),
          # .SDcols = prop_cols
          # ]
          
          # curves[
          # ,
          # (prop_cols) := lapply(
          #  .SD,
          # function(x) fifelse(
          # total_prop > 0,
          #  x / total_prop,
          # 0
          # ))
          # ,
          # .SDcols = prop_cols
          # ]
          # Steve: compare using total biomass (kg/ha), not proportions.
          # Yield tables will be converted from volume (m3/ha) to biomass (kg/ha).
          # Do not normalize to proportions.
          
          # Restrict candidate yield curves to those with a
          # comparable total biomass before calculating the
          # Euclidean distance in species-group biomass.
          
          pixel_total <- sum(cohort_vec)
          if (pixel_total == 0) {
            
            cat(
              "\nSkipping zero-biomass PixelGroup:",
              .BY$pixelGroup,
              "\n"
            )
            
            list(
              bestAU = NA_character_,
              CURVENO = NA_integer_,
              distance = NA_real_
            )
            
          } else {
            
            # ادامه کد فعلی
          }
          curve_total <- rowSums(
            curves[, prop_cols, with = FALSE],
            na.rm = TRUE
          )
          
          ratio <- pixel_total / curve_total
          
          curves_filtered <- curves[
            ratio >= 0.6 &
              ratio <= (1 / 0.6)
          ]
          
          cat(
            "Before filter:", nrow(curves),
            " After filter:", nrow(curves_filtered),
            "\n"
          )
          
          if (nrow(curves_filtered) > 0) {
            curves <- curves_filtered
          } else {
            
            cat(
              "\nNO CURVES PASSED BIOMASS FILTER - USING ALL CURVES:",
              .BY$pixelGroup,
              "\n"
            )
            
            ## عمداً curves را تغییر نمی‌دهیم
            ## یعنی از همه AUها برای Euclidean distance استفاده می‌کنیم.
          }
          cat(
            "Before filter:", length(ratio),
            " After filter:", nrow(curves),
            "\n"
          )
          curves_mat <- as.matrix(
            curves[, prop_cols, with = FALSE]
          )
          
          cohort_mat <- matrix(
            cohort_vec,
            nrow = nrow(curves_mat),
            ncol = length(cohort_vec),
            byrow = TRUE
          )  
          
          dists <- sqrt(rowSums((curves_mat - cohort_mat)^2))
          best_idx <- which.min(dists)
          best_curve <- curves$CURVENO[best_idx]
          list(
            bestAU   = curves$AU[best_idx],
            CURVENO  = best_curve,
            distance = dists[best_idx]
          )
        }
      }
    }
    
  }, by = pixelGroup]
  cat("\n===== STEP 4 =====\n")
  
  cat(
    "Unique pixelGroups in results:",
    uniqueN(results$pixelGroup),
    "\n"
  )
  cat("\n=========================\n")
  
  cat(
    "PixelGroups with NA AU:",
    sum(is.na(results$bestAU)),
    "\n"
  )
  
  cat(
    "Unique missing PixelGroups:",
    uniqueN(results[is.na(bestAU), pixelGroup]),
    "\n"
  )
  
  cat("=========================\n")
  # =========================================================
  # 4. SAVE OUTPUT
  # =========================================================
  
  # =========================================================
  # FINAL OUTPUTS
  # =========================================================
  
  # =====================================================
  # Standardized classification output
  # =====================================================
  sim$AUtoCurve <- unique(
    rbindlist(
      sim$yield_by_region
    )[
      ,
      .(
        AU,
        curveID = paste0(
          zone,
          "_",
          CURVENO
        )
      )
    ]
  )
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
    sim$AUtoCurve,
    by = "AU",
    all.x = TRUE
  )
  names(sim$classification)
  
  head(sim$classification)
  # 🔥 pixelGroup → AU mapping
  
  
  
  pg <- terra::values(sim$pixelGroupMap)[,1]
  
  missingPG <- setdiff(
    unique(pg[!is.na(pg)]),
    results$pixelGroup
  )
  
  
  
  
  
  
  sim$pixelGroupToAU <- results[, .(
    pixelGroup,
    analysisUnit = bestAU
  )]
  
  
  ######################################3
  #######################################3
  #################################3
  
  #  sim$AUtoCurve <- unique(
  # results[, .(
  # AU = bestAU,
  # curveID = as.character(CURVENO)
  # )]
  #)
  # sim$AUtoCurve <- results[
  #  !is.na(bestAU),
  # .N,
  #by = .(
  #   AU = bestAU,
  #    curveID = as.character(CURVENO)
  #   )
  #  ][
  #,
  #.SD[which.max(N)],
  # by = AU
  #][
  # ,
  #  .(AU, curveID)
  # ]
  
  
  ###
  
  
  
  
  
  # =====================================================
  # Build standDT for AAC
  # =====================================================
  
  # -------------------------------------------------------
  # Temporary stand age definition
  # -------------------------------------------------------
  # A pixelGroup may contain multiple cohorts with different ages.
  # For now, we assign the stand age as the modal (most frequent)
  # cohort age within each pixelGroup.
  #
  # TODO:
  # Verify the appropriate stand-age definition for LandR.
  # Alternatives include:
  #   - oldest cohort
  #   - biomass-weighted age
  #   - LandR stand age (if available)
  # -------------------------------------------------------
  
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
  
  
  # -------------------------------------------------------
  # 🔥 COMPUTE pixel-level effective area (hectares)
  # -------------------------------------------------------
  # cell area (ha)
  # -------------------------------------------------------
  # 🔥 COMPUTE pixel-level effective area (hectares)
  # -------------------------------------------------------
  
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
  cellArea_ha <- prod(terra::res(sim$pixelGroupMap)) / 10000
  
  # raster -> vectors
  hf <- as.vector(terra::values(sim$harvestableFraction))
  pg <- as.vector(terra::values(sim$pixelGroupMap))
  
  # build table
  pixel_area_dt <- data.table(
    pixelGroup = pg,
    harvestableFraction = hf
  )
  
  # remove NA cells
  pixel_area_dt <- pixel_area_dt[
    pixelGroup > 0
  ]  
  # aggregate by pixelGroup
  pixel_area_dt <- pixel_area_dt[
    ,
    .(
      effectiveArea = sum(
        harvestableFraction * cellArea_ha,
        na.rm = TRUE
      )
    ),
    by = pixelGroup
  ]
  
  # join AU
  pixel_area_dt <- merge(
    pixel_area_dt,
    sim$pixelGroupToAU,
    by = "pixelGroup",
    all.x = TRUE
  )
  
  # save
  sim$pixelAreaDT <- pixel_area_dt
  # =====================================================
  # Add effectiveArea to standDT
  # =====================================================
  
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
  
  
  # 🔥 area per AU
  
  print(
    pixel_area_dt[
      ,
      .(
        nPixelGroups = .N,
        effectiveArea = sum(effectiveArea)
      ),
      by = analysisUnit
    ][
      order(-effectiveArea)
    ][1:20]
  )
  sim$areaByAU <- sim$pixelAreaDT[
    !is.na(analysisUnit),
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
  
  
  ######################OBject new based on AU(standdt is based on pixel)
  # =====================================================
  # AAC input table
  # =====================================================
  
  ########AUMAP
  ##########################
  analysisUnitMap <- sim$pixelGroupMap
  
  lookup <- sim$pixelGroupToAU
  
  pg_vals <- terra::values(sim$pixelGroupMap)
  
  idx <- match(
    pg_vals,
    lookup$pixelGroup
  )
  
  terra::values(analysisUnitMap) <-
    lookup$analysisUnit[idx]
  
  sim$analysisUnitMap <- analysisUnitMap
  
  
  
  
  return(sim)
}

# TODO:
# Border pixelGroups intersecting multiple SITEREGIONs
# currently use which.max(table(region_vals))