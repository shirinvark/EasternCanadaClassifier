classifyProvince_ON <- function(sim) {
  
  library(data.table)
  message("Running Ontario classifier")
  
  # ===================================================
  # === DOWNLOAD DATA FROM GITHUB ===
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
  print(groups)
  # =========================================================
  # 1. PROCESS ZONE FUNCTION
  # =========================================================
  
  process_zone <- function(path, submu) {
    #browser()
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
      x[x == "." | x == "" | is.na(x)] <- NA
      as.numeric(x)
    }), .SDcols = species_cols]
    
    # ---- Convert to proportions ----
    dt[, total := rowSums(.SD, na.rm = TRUE), .SDcols = species_cols]
    dt <- dt[total > 0]
    
   # dt[, (species_cols) := lapply(.SD, function(x) x / total), .SDcols = species_cols]
    
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
    
  #  dt_summary[, (groups) := lapply(.SD, function(x) x / total),
      #         .SDcols = groups]
    
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
  # ---- load shapefile (CORRECT WAY) ----
  # =========================================================
  # 2.5 BUILD pixel_region FROM SHAPEFILE
  # =========================================================
  
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
  
  cat("\n===== SHP PATH =====\n")
  print(shp_path)
  
  cat("\n===== SHP FIELDS =====\n")
  print(names(shp))
  # =========================================================
  # BUILD pixel_region (MISSING PART - FIX)
  # =========================================================
  
  # ---- project ----
  shp <- terra::project(shp, terra::crs(sim$pixelGroupMap))  
  # ---- align extent (خیلی مهم) ----
  #shp <- terra::crop(shp, terra::ext(sim$pixelGroupMap))
  cat("\n===== EXTENT CHECK =====\n")
  print(ext(sim$pixelGroupMap))
  print(ext(shp))
  # ---- rasterize ----
  region_raster <- terra::rasterize(
    shp,
    sim$pixelGroupMap,
    field = "SITEREGION",
    touches = TRUE
  )
  print(unique(terra::values(region_raster)))
  # ---- extract values ----
  pg  <- as.data.table(terra::values(sim$pixelGroupMap))
  reg <- as.data.table(terra::values(region_raster))
  
  setnames(pg, names(pg), "pixelGroup")
  setnames(reg, names(reg), "region")
  
  pixel_region <- cbind(pg, reg)
  ########################################################
  ####it is temporary and just for fake data
  pixel_region[, region := as.character(region)]
  pixel_region[, region := "3e"]
  # ---- clean ----
  #pixel_region[, region := tolower(as.character(region))]
 ##############################################################3 
  pixel_region <- pixel_region[
    !is.na(pixelGroup) & !is.na(region)
  ]
  
  # ---- save in sim ----
  sim$pixel_region <- pixel_region
  
  # ---- debug ----
  cat("\n===== REGION TABLE =====\n")
  print(table(pixel_region$region))
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
  
  # aggregate biomass
  cohort_group <- cohortDT[, .(
    B = sum(B, na.rm = TRUE)
  ), by = .(pixelGroup, age, final_group)]
  #####
  # اسم ستون pixelGroup رو تمیز کن
  
  
  
  
  
  
  
  print(names(cohortDT))
  # convert to wide
  cohort_wide <- dcast(
    cohort_group,
    pixelGroup + age ~ final_group,
    value.var = "B",
    fill = 0
  )
  print(names(cohort_wide))
  # force pixelGroup name
  pg_col <- names(cohort_wide)[grepl("pixelgroup", names(cohort_wide), ignore.case = TRUE)]
  
  if (length(pg_col) != 1) {
    stop("❌ Could not uniquely identify pixelGroup column")
  }
  
  setnames(cohort_wide, pg_col, "pixelGroup")
  if (!"pixelGroup" %in% names(cohort_wide)) {
    stop("❌ pixelGroup column still missing in cohort_wide")
  }
  # normalize to proportions
  group_cols <- setdiff(names(cohort_wide), c("pixelGroup","age"))
  prop_cols <- intersect(groups, group_cols)
  prop_cols <- prop_cols[!is.na(prop_cols)]
  if (length(prop_cols) == 0) {
    stop("❌ No matching species groups between cohort and yield")
  }
  cohort_wide[, total := rowSums(.SD), .SDcols = group_cols]
  
  cohort_wide[, (group_cols) := lapply(.SD, function(x) x / total),
              .SDcols = group_cols]
  
  cohort_wide <- cohort_wide[total > 0]
  #cohort_wide  <- sim$cohort_wide
  pixel_region <- sim$pixel_region
  
  results <- cohort_wide[, {
    
    cohort_vec <- as.numeric(.SD[1, prop_cols, with = FALSE])
    
    if (sum(cohort_vec) == 0) {
      list(bestAU = NA, distance = NA)
    } else {
      
      cohort_vec <- cohort_vec / sum(cohort_vec)
      
      region_vals <- pixel_region[pixelGroup == .BY$pixelGroup, region]
      
      region <- if (length(region_vals) == 0) {
        "3e"
      } else {
        names(which.max(table(region_vals)))
      }
      
      if (!(region %in% names(yield_by_region))) {
        list(bestAU = NA, distance = NA)
      } else {
        
        curves <- sim$yield_by_region[[region]]        
        age <- mean(.SD$age)
        
        curves[, age_diff := abs(AC10 - age)]
        curves <- curves[age_diff == min(age_diff)]
        curves[, age_diff := NULL]
        if (nrow(curves) == 0) {
          list(bestAU = NA, distance = NA)
        } else {
          
          curves_mat <- as.matrix(curves[, ..prop_cols])
          cohort_mat <- matrix(
            cohort_vec,
            nrow = nrow(curves_mat),
            ncol = length(cohort_vec),
            byrow = TRUE
          )  
          print(prop_cols)
          
          print(head(curves_mat))
          
          print(cohort_mat)
          dists <- sqrt(rowSums((curves_mat - cohort_mat)^2))
          best_idx <- which.min(dists)
          
          list(
            bestAU   = curves$AU[best_idx],
            distance = dists[best_idx]
          )
        }
      }
    }
    
  }, by = pixelGroup]
  
  # =========================================================
  # 4. SAVE OUTPUT
  # =========================================================
  
  # =========================================================
  # FINAL OUTPUTS
  # =========================================================
  
  # classification already exists
  sim$classification <- results
  
  # 🔥 pixelGroup → AU mapping
  sim$pixelGroupToAU <- results[, .(
    pixelGroup,
    analysisUnit = bestAU
  )]
  sim$AUtoCurve <- unique(
    results[, .(
      AU = bestAU,
      curveID = bestAU
    )]
  )
  # 🔥 area per AU
  sim$areaByAU <- sim$pixelGroupToAU[
    ,
    .N,
    by = .(AU = analysisUnit)
  ]
  setnames(sim$areaByAU, "N", "nPixels")
  
  # -------------------------------------------------------
  # 🔥 COMPUTE pixel-level effective area (hectares)
  # -------------------------------------------------------
  
  # cell area (ha)
  cellArea_ha <- prod(terra::res(sim$pixelGroupMap)) / 10000
  
  # harvestable fraction raster → vector
  hf <- as.vector(terra::values(sim$harvestableFraction))
  
  # pixelGroup raster → vector
  pg <- as.vector(terra::values(sim$pixelGroupMap))
  
  # build table
  pixel_area_dt <- data.table(
    pixelGroup = pg,
    harvestableFraction = hf
  )
  pixel_area_dt <- unique(
    pixel_area_dt,
    by = "pixelGroup"
  )
  # remove NA
  pixel_area_dt <- pixel_area_dt[!is.na(pixelGroup)]
  
  # join with AU mapping
  pixel_area_dt <- merge(
    pixel_area_dt,
    sim$pixelGroupToAU,
    by = "pixelGroup",
    all.x = TRUE
  )
  
  # compute effective area
  pixel_area_dt[, effectiveArea := harvestableFraction * cellArea_ha]
  
  # save in sim
  sim$pixelAreaDT <- pixel_area_dt
  return(sim)
}