Init <- function(sim) {
  
  # message("Building analysisUnitMap from LandR state")
  # Inform user that the classifier module is starting
  
  ## ------------------------------------------------
  ## 1. Read yield tables (.vol)
  ## ------------------------------------------------
  requireNamespace("data.table")
  requireNamespace("terra")

  cohortData <- sim$cohortData
  pixelGroupMap <- sim$pixelGroupMap
  # =========================================================
  # BUILD jurisdiction raster
  # =========================================================
  
  library(terra)
  
  jur_vect <- terra::vect(sim$canadaJurisdiction)
  
  # project to match raster
  jur_vect <- terra::project(jur_vect, pixelGroupMap)  
  # crop to study area
  jur_vect <- terra::crop(jur_vect, pixelGroupMap)
  
  # rasterize
  jur_raster <- terra::rasterize(
    jur_vect,
    pixelGroupMap,
    field = "PRUID"
  )
  
  # store in sim
  sim$jurRaster <- jur_raster
 
  # =========================================================
  # EXTRACT jurisdictions present
  # =========================================================
  browser()
  names(pixelGroupMap)
  jur_levels <- unique(as.data.frame(jur_vect)[, c("PRUID", "PRNAME")])  
  vals <- terra::values(jur_raster)
  vals <- as.vector(vals)
  vals <- vals[!is.na(vals)]
  
  ids_present <- unique(vals)
  
  jur_names <- jur_levels$PRNAME[match(ids_present, jur_levels$PRUID)]
  
  cat("Jurisdictions in this run:\n")
  print(jur_names)
  cat("\n")
  
#####this is temporary hardcoded Sorry
  
  
  jur <- "AB"
  vol_file <- sim$yieldVolFile
  vol_files <- c(vol_file)  
  # objects to fill
  curves <- list()
  curves_prop <- list()
  
  for (f in vol_files) {
    
    
    lines <- readLines(f)
    lines <- trimws(lines)
    
    # 👇 این خط خیلی مهمه
    cat("Reading file:", f, "\n")
    cat("Number of lines:", length(lines), "\n\n")
    
    # Parse header
    lines <- lines[lines != ""]
    header_line <- lines[1]
    
    header <- strsplit(header_line, "\\s+")[[1]]
    
    nCurves <- as.numeric(gsub("#", "", header[1]))
    nAges   <- as.numeric(header[2])   
  
    cat("nCurves:", nCurves, " | nAges:", nAges, "\n\n")
    
    # Extract numeric data
    dataLines <- lines[2:(nCurves * 2 + 1)]
    
    dataMatrix <- do.call(rbind, lapply(dataLines, function(x) {
      x <- trimws(x)
      as.numeric(strsplit(x, "\\s+")[[1]])
    }))
    
    cat("dataMatrix dim:", dim(dataMatrix), "\n\n")
    
    # Build curves
    for (i in 1:nCurves) {
      
      conifer_row   <- dataMatrix[(2*i - 1), ]
      deciduous_row <- dataMatrix[(2*i), ]
      
      curves[[i]] <- list(
        conifer   = conifer_row,
        deciduous = deciduous_row
      )
    }
  }
  
  cat("Number of curves built:", length(curves), "\n\n")
  
  # =========================================================
  # BUILD yield matrices (for outputs)
  # =========================================================
  
  nCurves <- length(curves)
  nAges <- length(curves[[1]]$conifer)
  
  yieldConifer <- matrix(NA, nrow = nCurves, ncol = nAges)
  yieldDeciduous <- matrix(NA, nrow = nCurves, ncol = nAges)
  
  for (i in seq_len(nCurves)) {
    yieldConifer[i, ]   <- curves[[i]]$conifer
    yieldDeciduous[i, ] <- curves[[i]]$deciduous
  }
  
  yieldAges <- seq(0, by = 10, length.out = nAges)
  
  annualAges <- 1:max(yieldAges)
  
  yieldConifer_annual <- t(apply(yieldConifer, 1, function(curve) {
    approx(
      x = yieldAges,
      y = curve,
      xout = annualAges,
      rule = 2
    )$y
  }))
  
  yieldDeciduous_annual <- t(apply(yieldDeciduous, 1, function(curve) {
    approx(
      x = yieldAges,
      y = curve,
      xout = annualAges,
      rule = 2
    )$y
  }))
  
  sim$yieldConifer   <- yieldConifer_annual
  sim$yieldDeciduous <- yieldDeciduous_annual
  sim$yieldTables    <- yieldConifer_annual + yieldDeciduous_annual
  sim$yieldAges      <- annualAges
  
  
  # Convert to proportional form
  for (i in seq_along(curves)) {
    
    con <- sim$yieldConifer[i, ]
    dec <- sim$yieldDeciduous[i, ]
    
    total <- con + dec
    total[total == 0] <- 1
    
    curves_prop[[i]] <- list(
      conifer   = con / total,
      deciduous = dec / total
    )
  }
  
  # Assign names
  curve_names <- c(
    "Aw",
    "AwS",
    "AwSw",
    "SwAw",
    "Sw",
    "Sb",
    "Pj",
    "MxPj"
  )
  
  names(curves_prop) <- curve_names
  
  # Remove unused curve
  curves_prop$AwS <- NULL
  
  cat("Curves (proportion) names:\n")
  print(names(curves_prop))
  cat("\n")
  # =========================================================
  # READ speciesGroups
  # =========================================================
  
  species_file <- file.path("data", jur, "speciesGroups.txt")
  sg_lines <- readLines(species_file)
  
  speciesGroups <- lapply(sg_lines, function(x) {
    parts <- strsplit(x, ":")[[1]]
    group_name <- trimws(parts[1])
    spp <- trimws(strsplit(parts[2], ",")[[1]])
    return(spp)
  })
  
  names(speciesGroups) <- sapply(strsplit(sg_lines, ":"), function(x) trimws(x[1]))
  
  # =========================================================
  # READ mapSpeciesGroups  👈 اینجا اضافه کن
  # =========================================================
  
  #jur <- "AB"
  map_file <- file.path("data", jur, "mapSpeciesGroups.txt")
  map_lines <- readLines(map_file)
  
  mapSpeciesGroups <- lapply(map_lines, function(x) {
    parts <- strsplit(x, ":")[[1]]
    curve_name <- trimws(parts[1])
    groups <- trimws(strsplit(parts[2], ",")[[1]])
    return(groups)
  })
  
  names(mapSpeciesGroups) <- sapply(strsplit(map_lines, ":"), function(x) trimws(x[1]))
  # =========================================================
  # COHORT DATA PROCESSING
  # =========================================================
  
  cohortDT <- data.table::copy(cohortData)
  
  print(names(cohortDT))  # 👈 اینو اضافه کن
  # 2️⃣ Create group column
  cohortDT[, group := NA_character_]
  for (g in names(speciesGroups)) {
    cohortDT[speciesCode %in% speciesGroups[[g]], group := g]
  }
  # 4️⃣ Remove species not assigned to any group (e.g. Acer)
  cohortDT <- cohortDT[!is.na(group)]
  cat("Group counts:\n")
  print(table(cohortDT$group))
  cat("\n")
  # 5️⃣ Aggregate biomass by pixel and group
  pixelGroups <- cohortDT[
    , .(biomass = sum(B)), 
    by = .(pixelGroup, group, age)
  ]
  
  # 6️⃣ Convert to wide format (one column per group)
  # 6️⃣ Convert to wide format (one row per pixelGroup)
  pixelWide <- data.table::dcast(
    pixelGroups,
    pixelGroup ~ group,
    value.var = "biomass",
    fun.aggregate = sum,
    fill = 0
  )
  
  # 7️⃣ Compute total biomass
  group_cols <- setdiff(names(pixelWide), "pixelGroup")
  
  pixelWide[, total := rowSums(.SD), .SDcols = group_cols]
  # 8️⃣ Avoid division by zero
  pixelWide[total == 0, total := 1]
  print(names(pixelWide))
  # 9️⃣ Convert to proportions
  # make sure columns exist (safer)
  if (!"borealDeciduous_AB" %in% names(pixelWide)) pixelWide[, borealDeciduous_AB := 0]
  if (!"whiteSpruce_AB" %in% names(pixelWide)) pixelWide[, whiteSpruce_AB := 0]
  if (!"blackSpruce_AB" %in% names(pixelWide)) pixelWide[, blackSpruce_AB := 0]
  if (!"borealPine_AB" %in% names(pixelWide)) pixelWide[, borealPine_AB := 0]
  
  # then compute proportions
  pixelWide[, prop_deciduous := borealDeciduous_AB / total]
  pixelWide[, prop_sw        := whiteSpruce_AB / total]
  pixelWide[, prop_sb        := blackSpruce_AB / total]
  pixelWide[, prop_pine      := borealPine_AB / total]
  cat("pixelWide columns:\n")
  print(names(pixelWide))
  cat("\n")
  
  cat("Check proportions (first rows):\n")
  print(head(pixelWide))
  cat("\n")
  # =========================================================
  # PREP AGE INDEX (ONCE)
  # =========================================================
  
  # extract raster values
  pg_vals  <- terra::values(pixelGroupMap)
  age_vals <- terra::values(sim$standAgeMap)
  
  # بساز جدول pixelGroup → age
  ageDT <- data.table::data.table(
    pixelGroup = pg_vals,
    age = age_vals
  )
  
  # حذف NA
  ageDT <- ageDT[!is.na(pixelGroup) & !is.na(age)]
  
  # اگر چند پیکسل برای یک pixelGroup بود → میانگین بگیر
  ageDT <- cohortDT[
    , .(age = weighted.mean(age, B)),
    by = pixelGroup
  ]  
  # merge با pixelWide
  pixelWide <- merge(pixelWide, ageDT, by = "pixelGroup", all.x = TRUE)
  
  # Convert age to index (annual)
  age_index <- round(pixelWide$age)
  age_index <- pmax(1, pmin(age_index, length(sim$yieldAges)))  
  # =========================================================
  # MATCH PIXELS TO CURVES
  # =========================================================
  
  results <- list()
  for (curve_name in names(curves_prop)) {
    
    curve <- curves_prop[[curve_name]]
    
    curve_con_vals <- curve$conifer[age_index]
    curve_dec_vals <- curve$deciduous[age_index]
    # پیدا کن این curve مربوط به کدوم group هست
    target_groups <- mapSpeciesGroups[[curve_name]]    
    # بساز نسخه 4 بعدی table
    curve_dec <- rep(0, length(curve_con_vals))
    curve_sw  <- rep(0, length(curve_con_vals))
    curve_sb  <- rep(0, length(curve_con_vals))
    curve_pine <- rep(0, length(curve_con_vals))
    
    for (g in target_groups) {
      if (g == "deciduous") {
        curve_dec <- curve_dec_vals
      } else if (g == "whiteSpruce_AB") {
        curve_sw <- curve_con_vals
      } else if (g == "blackSpruce_AB") {
        curve_sb <- curve_con_vals
      } else if (g == "borealPine_AB") {
        curve_pine <- curve_con_vals
      }
    }
    
    cat("Curve:", curve_name, "\n")
    cat("sw:", head(curve_sw), "\n")
    cat("sb:", head(curve_sb), "\n")
    cat("pine:", head(curve_pine), "\n\n")
    cat("Curve:", curve_name, "→ groups:", paste(target_groups, collapse = ","), "\n")
    
    # Distribute conifer proportion across species groups
    
    
    # Compute distance (max absolute difference)
    dist <- pmax(
      abs(pixelWide$prop_deciduous - curve_dec),
      abs(pixelWide$prop_sw        - curve_sw),
      abs(pixelWide$prop_sb        - curve_sb),
      abs(pixelWide$prop_pine      - curve_pine)
    )
    
    results[[curve_name]] <- dist
  }
  cat("\n")
  # Convert to matrix and select best matching curve
  distMatrix <- as.data.frame(results)
  
  pixelWide$bestCurve <- names(distMatrix)[
    max.col(-as.matrix(distMatrix))
  ]
  
  # =========================================================
  # CREATE RASTER OUTPUT
  # =========================================================
  
  # Convert bestCurve to numeric class ID
  curve_levels <- c("Aw","AwSw","SwAw","Sw","Sb","Pj","MxPj")
  
  pixelWide[, classID := as.numeric(factor(bestCurve, levels = curve_levels))]  
  
  # Create lookup table
  lookup <- pixelWide[, .(pixelGroup, classID)]
  #browser()
  
  # Extract raster values
  vals <- terra::values(pixelGroupMap)
  
  # Match pixelGroup to classID
  idx <- match(vals, lookup$pixelGroup)
  
  cat("Unmatched pixelGroups:\n")
  print(sum(is.na(idx)))
  cat("\n")
  # Warn if some pixelGroups are missing
  if (any(is.na(idx))) {
    warning("Some pixelGroups not matched to lookup")
  }  
  
  # Assign class IDs to raster
  new_vals <- lookup$classID[idx]
  new_vals[is.na(new_vals)] <- 0  
  
  # Create output raster
  analysisUnitRaster <- pixelGroupMap
  terra::values(analysisUnitRaster) <- new_vals  
  
  # =========================================================
  # APPLY HARVESTABLE MASK
  # =========================================================
  #browser()
  
  hf <- terra::values(sim$harvestableFraction)
  analysisUnitRaster[hf == 0] <- NA
  hf <- terra::values(sim$harvestableFraction)
  analysisUnitRaster[hf == 0] <- NA  
  # Define raster categories
  
  class_table <- data.frame(
    classID = 0:7,
    curve = c("background","Aw","AwSw","SwAw","Sw","Sb","Pj","MxPj")
  )
  
  levels(analysisUnitRaster) <- class_table  
  # =========================================================
  # AREA CALCULATION
  # =========================================================
  
  cell_area <- prod(terra::res(analysisUnitRaster))
  cell_area_ha <- cell_area / 10000  
  
  freq_table <- as.data.frame(terra::freq(analysisUnitRaster))
  
  # Remove background (class 0)
  freq_table <- freq_table[freq_table$value != 0, ]
  
  # Compute area in hectares
  freq_table$area_ha <- freq_table$count * cell_area_ha  
  
  areaByAU <- freq_table[, c("value", "area_ha")]
  names(areaByAU) <- c("curve", "area_ha")
  # =========================================================
  # FINAL CHECKS & VISUALIZATION
  # =========================================================
  
  cat("\n================ FINAL CHECKS ================\n")
  
  # 1️⃣ Distribution of classes
  cat("\nClass distribution (bestCurve):\n")
  print(table(pixelWide$bestCurve))
  
  # 2️⃣ Check proportions summary
  cat("\nSummary of proportions:\n")
  print(summary(pixelWide[, .(
    prop_deciduous,
    prop_sw,
    prop_sb,
    prop_pine
  )]))
  
  # 3️⃣ Check if proportions sum ~ 1
  cat("\nCheck sum of proportions (should be ~1):\n")
  prop_sum <- pixelWide$prop_deciduous +
    pixelWide$prop_sw +
    pixelWide$prop_sb +
    pixelWide$prop_pine
  print(summary(prop_sum))
  
  # 4️⃣ Check for NA values
  cat("\nNA check in key columns:\n")
  print(colSums(is.na(pixelWide)))
  
  # 5️⃣ Cross-check: class vs deciduous level
  cat("\nCross-tab: class vs deciduous proportion:\n")
  print(table(
    pixelWide$bestCurve,
    cut(pixelWide$prop_deciduous, breaks = 3)
  ))
  
  # =======================================================
  
  # =========================================================
  # SAVE OUTPUTS
  # =========================================================
  
  sim$analysisUnitDT <- pixelWide
  sim$analysisUnitMap <- analysisUnitRaster
  sim$areaByAU <- areaByAU
  sim$yieldCurves <- curves
  invisible(sim)
}
  