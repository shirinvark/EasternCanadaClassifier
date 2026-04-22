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
    # ---- Filtering ----
    dt <- dt[
      SUBMU == submu &
        !is.na(SI) &
        grepl("prsnt", tolower(SI)) &
        FU != "BOG"
    ]
    cat("\n===== DEBUG AFTER FILTER =====\n")
    print(dim(dt))
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
    for (sp in species_cols) {
      
      sp_group <- speciesGroups[[sp]]
      if (is.null(sp_group)) next
      
      final_group <- mapSpeciesGroups[[sp_group]]
      if (is.null(final_group)) next
      
      dt[[final_group]] <- dt[[final_group]] + dt[[sp]]
    }
    
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
  
  yield_all <- rbindlist(lapply(zones, function(z) {
    process_zone(z$path, z$submu)
  }))
  
  # =========================================================
  # 3. CLASSIFIER
  # =========================================================
  
  cohort_wide  <- sim$cohort_wide
  pixel_region <- sim$pixel_region
  
  results <- cohort_wide[, {
    
    # ---- cohort vector ----
    cohort_vec <- as.numeric(.SD[, ..prop_cols][1])
    
    # ---- region ----
    region <- pixel_region[pixelGroup == .BY$pixelGroup, region][1]
    
    # ---- age ----
    age <- unique(.SD$age)[1]
    
    # ---- matching curves ----
    curves <- yield_all[
      zone == region & AC10 == age
    ]
    
    # ---- handle missing ----
    if (nrow(curves) == 0) {
      return(list(bestAU = NA, distance = NA))
    }
    
    # ---- distance ----
    dists <- curves[
      , sqrt(rowSums((.SD - cohort_vec)^2)),
      .SDcols = prop_cols
    ]
    
    best_idx <- which.min(dists)
    
    list(
      bestAU   = curves$AU[best_idx],
      distance = dists[best_idx]
    )
    
  }, by = pixelGroup]
  
  # =========================================================
  # 4. SAVE OUTPUT
  # =========================================================
  
  sim$classification <- results
  
  return(sim)
}