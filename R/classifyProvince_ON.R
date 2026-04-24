classifyProvince_ON <- function(sim) {
  
  library(data.table)
  message("Running Ontario classifier")
  
  # =========================================================
  # 0. LOAD MAPPINGS (ONLY ONCE)
  # =========================================================
  
  speciesGroups <- read_curve_mapping("data/ON/speciesGroups_ON.txt")
  mapSpeciesGroups <- read_curve_mapping("data/ON/mapSpeciesGroups.txt")
  
  groups    <- unique(unlist(mapSpeciesGroups))
  prop_cols <- groups
  
  # =========================================================
  # 1. PROCESS ZONE FUNCTION
  # =========================================================
  
  process_zone <- function(path, submu) {
    
    dt <- fread(path)
    
    cat("\n===== DEBUG BEFORE FILTER =====\n")
    print(unique(dt$SUBMU))
    print(unique(dt$SI))
    print(unique(dt$FU))
    
    # step 1: subset by region
    dt_sub <- dt[grepl(tolower(submu), tolower(SUBMU))]
    
    # step 2: prepare SI values
    si_vals <- tolower(dt_sub$SI)
    si_vals <- si_vals[!is.na(si_vals)]
    
    # step 3: choose SI
    target_si <- if (any(grepl("prsnt", si_vals))) {
      "prsnt"
    } else {
      names(sort(table(si_vals), decreasing = TRUE))[1]
    }
    
    # step 4: final filter
    dt <- dt_sub[
      !is.na(SI) &
        grepl(target_si, tolower(SI)) &
        tolower(FU) != "bog"
    ]
    
    # debug
    cat("\n===== DEBUG AFTER FILTER =====\n")
    cat("Zone:", submu, "\n")
    cat("Selected SI:", target_si, "\n")
    print(dim(dt))
    print(unique(tolower(dt$SI)))
    
    # safety
    if (nrow(dt) == 0) {
      warning(paste("No data after filtering for zone:", submu))
      return(NULL)
    }
    
    # ---- ادامه کدت (species processing) ----
    # ---- Species columns ----
    species_cols <- c("PW","PR","PJ","SB","SW","BF","CE","OC","HE","PO","PB","BW","MH","QR","YB","OH")
    
    # ---- Convert to numeric ----
    dt[, (species_cols) := lapply(.SD, function(x) {
      x[x == "." | x == ""] <- NA
      as.numeric(x)
    }), .SDcols = species_cols]
    
    # ---- Convert to proportions ----
    dt[, total := rowSums(.SD, na.rm = TRUE), .SDcols = species_cols]
    dt <- dt[total > 0]
    
    dt[, (species_cols) := lapply(.SD, function(x) x / total), .SDcols = species_cols]
    
    # ---- Initialize group columns ----
    for (g in groups) {
      dt[[g]] <- 0
    }
    
    # ---- Mapping species → groups ----
    print(names(speciesGroups))
    print(names(mapSpeciesGroups))
    for (sp in species_cols) {
      
      key <- trimws(sp)
      final_group <- mapSpeciesGroups[[key]]
      
      if (is.null(final_group)) {
        cat("❌ NO mapping for:", sp, "\n")
        next
      }
      
      cat("✔️", sp, "→", final_group,
          "| sum =", sum(dt[[sp]], na.rm = TRUE), "\n")
      
      dt[[final_group]] <- dt[[final_group]] + dt[[sp]]
    }
    cat("\n===== GROUP SUM CHECK =====\n")
    print(colSums(dt[, ..groups], na.rm = TRUE))
    print(colSums(dt[, ..groups], na.rm = TRUE))
    # ---- Aggregate to curve level ----
    dt_summary <- dt[, lapply(.SD, sum, na.rm = TRUE),
                     by = .(CURVENO, AC10, FU),
                     .SDcols = groups]
    
    # ---- Normalize ----
    dt_summary[, total := rowSums(.SD), .SDcols = groups]
    dt_summary <- dt_summary[total > 0]
    
    dt_summary[, (groups) := lapply(.SD, function(x) x / total),
               .SDcols = groups]
    
    # ---- Remove NA ----
    dt_summary <- dt_summary[complete.cases(dt_summary[, ..groups])]
    
    # ---- Assign AU ----
    dt_summary[, AU := FU]
    
    # ---- Add zone ----
    dt_summary[, zone := submu]
    
    return(dt_summary)
  }
  
  # =========================================================
  # 2. BUILD YIELD TABLE
  # =========================================================
  
  zones <- list(
    list(path = "data/ON/YTF/3e_tbl_yield_final.csv", submu = "3e"),
    list(path = "data/ON/YTF/3w_tbl_yield_final.txt", submu = "3w"),
    list(path = "data/ON/YTF/4e_5e_tbl_yield_final.txt", submu = "4e"),
    list(path = "data/ON/YTF/4s_3s_tbl_yield_final.csv", submu = "4s"),
    list(path = "data/ON/YTF/4w_tbl_yield_final.txt", submu = "4w"),
    list(path = "data/ON/YTF/5e_tbl_yield_final.txt", submu = "5e")
  )
  results_list <- lapply(zones, function(z) {
    process_zone(z$path, z$submu)
  })
  
  # فقط خروجی‌های غیر NULL نگه دار
  results_list <- results_list[!sapply(results_list, is.null)]
  
  # بعد combine کن
  yield_all <- rbindlist(results_list, fill = TRUE)
  yield_by_region <- split(yield_all, yield_all$zone)
  
  
  
  
  # =========================================================
  # 2.5 BUILD pixel_region FROM SHAPEFILE
  # =========================================================
  
  library(terra)
  library(data.table)
  
  # ---- load shapefile ----
  shp <- vect("E:/EasternCanadaClassifier/ON_selected_regions.shp")
  
  # ---- align projection ----
  shp <- project(shp, sim$pixelGroupMap)
  
  # ---- rasterize ----
  region_raster <- rasterize(shp, sim$pixelGroupMap, field = "SITEREGION")
  
  # ---- extract values ----
  pg  <- values(sim$pixelGroupMap)
  reg <- values(region_raster)
  
  pixel_region <- data.table(
    pixelGroup = pg,
    region     = tolower(reg)
  )
  
  pixel_region <- pixel_region[
    !is.na(pixelGroup) & !is.na(region)
  ]
  
  # ---- clean ----
  pixel_region <- pixel_region[
    !is.na(pixelGroup) & !is.na(region)
  ]
  
  # ---- debug ----
  cat("\n===== REGION TABLE =====\n")
  print(table(pixel_region$region))
  
  # ---- assign to sim ----
  sim$pixel_region <- pixel_region
  
  
  
  
  
  # =========================================================
  # 3. CLASSIFIER
  # =========================================================
  
  cohort_wide  <- sim$cohort_wide
  pixel_region <- sim$pixel_region
  setnames(
    cohort_wide,
    names(cohort_wide)[1],
    "pixelGroup"
  )  results <- cohort_wide[, {
    
    # ---- cohort vector ----
    cohort_vec <- as.numeric(.SD[1, prop_cols, with = FALSE])    
    if (sum(cohort_vec) == 0) {
      return(list(bestAU = NA, distance = NA))
    }
    
    cohort_vec <- cohort_vec / sum(cohort_vec)
    names(cohort_vec) <- prop_cols   
    # ---- region ----
    region <- pixel_region[
      pixelGroup == .BY$pixelGroup,
      {
        tbl <- table(region)
        names(tbl)[which.max(tbl)]
      }
    ]   
    # ---- age ----
    age <- mean(.SD$age)    
    # ---- matching curves ----
    # ---- region safe ----
    if (is.na(region) || !(region %in% names(yield_by_region))) {
      return(list(bestAU = NA, distance = NA))
    }
    
    curves <- yield_by_region[[region]]
    
    # ---- age matching (FIXED) ----
    curves_local <- copy(curves)
    
    curves_local[, age_diff := abs(AC10 - age)]
    curves_local <- curves_local[age_diff == min(age_diff)]

    # اگر هنوز چیزی نبود
    if (nrow(curves_local) == 0) {
      return(list(bestAU = NA, distance = NA))
    }
    
    # ---- distance ----
    curves_mat <- as.matrix(curves_local[, ..prop_cols])
    cohort_mat <- matrix(cohort_vec, nrow = nrow(curves_mat), ncol = length(prop_cols), byrow = TRUE)
    
    dists <- sqrt(rowSums((curves_mat - cohort_mat)^2))
    best_idx <- which.min(dists)
    
    list(
      bestAU   = curves_local$AU[best_idx],
      distance = dists[best_idx]
    )
    
  }, by = pixelGroup]
  
  # =========================================================
  # 4. SAVE OUTPUT
  # =========================================================
  
  sim$classification <- results
  
  return(sim)
}