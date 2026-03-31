classifyProvince_AB <- function(sim) {
  
  requireNamespace("data.table")
  requireNamespace("terra")
  
  cohortData <- sim$cohortData
  pixelGroupMap <- sim$pixelGroupMap
  jur <- toupper(P(sim)$jurisdiction)
  
  if (jur != "AB") {
    stop("classifyProvince_AB was called with jurisdiction = ", jur)
  }
  
  # =========================================================
  # BUILD jurisdiction raster
  # =========================================================
  
  jur_vect <- terra::vect(sim$canadaJurisdiction)
  jur_vect <- terra::project(jur_vect, pixelGroupMap)
  jur_vect <- terra::crop(jur_vect, pixelGroupMap)
  
  jur_raster <- terra::rasterize(
    jur_vect,
    pixelGroupMap,
    field = "PRUID"
  )
  
  sim$jurRaster <- jur_raster
  
  # =========================================================
  # EXTRACT jurisdictions present
  # =========================================================
  
  jur_levels <- unique(as.data.frame(jur_vect)[, c("PRUID", "PRNAME")])
  vals <- terra::values(jur_raster)
  vals <- as.vector(vals)
  vals <- vals[!is.na(vals)]
  
  ids_present <- unique(vals)
  jur_names <- jur_levels$PRNAME[match(ids_present, jur_levels$PRUID)]
  
  cat("Jurisdictions in this run:\n")
  print(jur_names)
  cat("\n")
  
  # =========================================================
  # READ yield file(s)
  # =========================================================
  
  vol_file <- sim$yieldVolFile
  vol_files <- c(vol_file)
  
  curves <- list()
  curves_prop <- list()
  
  for (f in vol_files) {
    
    lines <- readLines(f)
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    
    cat("Reading file:", f, "\n")
    cat("Number of lines:", length(lines), "\n\n")
    
    header_line <- lines[1]
    header <- strsplit(header_line, "\\s+")[[1]]
    
    nCurves <- as.numeric(gsub("#", "", header[1]))
    nAges   <- as.numeric(header[2])
    
    cat("nCurves:", nCurves, " | nAges:", nAges, "\n\n")
    
    dataLines <- lines[2:(nCurves * 2 + 1)]
    
    dataMatrix <- do.call(rbind, lapply(dataLines, function(x) {
      x <- trimws(x)
      as.numeric(strsplit(x, "\\s+")[[1]])
    }))
    
    cat("dataMatrix dim:", dim(dataMatrix), "\n\n")
    
    for (i in 1:nCurves) {
      conifer_row   <- dataMatrix[(2 * i - 1), ]
      deciduous_row <- dataMatrix[(2 * i), ]
      
      curves[[i]] <- list(
        conifer = conifer_row,
        deciduous = deciduous_row
      )
    }
  }
  
  cat("Number of curves built:", length(curves), "\n\n")
  
  # =========================================================
  # BUILD yield matrices
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
    approx(x = yieldAges, y = curve, xout = annualAges, rule = 2)$y
  }))
  
  yieldDeciduous_annual <- t(apply(yieldDeciduous, 1, function(curve) {
    approx(x = yieldAges, y = curve, xout = annualAges, rule = 2)$y
  }))
  
  sim$yieldConifer   <- yieldConifer_annual
  sim$yieldDeciduous <- yieldDeciduous_annual
  sim$yieldTables    <- yieldConifer_annual + yieldDeciduous_annual
  sim$yieldAges      <- annualAges
  
  for (i in seq_along(curves)) {
    con <- sim$yieldConifer[i, ]
    dec <- sim$yieldDeciduous[i, ]
    
    total <- con + dec
    total[total == 0] <- 1
    
    curves_prop[[i]] <- list(
      conifer = con / total,
      deciduous = dec / total
    )
  }
  
  curve_names <- c("Aw", "AwS", "AwSw", "SwAw", "Sw", "Sb", "Pj", "MxPj")
  names(curves_prop) <- curve_names
  curves_prop$AwS <- NULL
  
  # =========================================================
  # READ species groups
  # =========================================================
  
  species_file <- file.path("data", jur, "speciesGroups.txt")
  sg_lines <- readLines(species_file)
  
  speciesGroups <- lapply(sg_lines, function(x) {
    parts <- strsplit(x, ":")[[1]]
    spp <- trimws(strsplit(parts[2], ",")[[1]])
    spp
  })
  
  names(speciesGroups) <- sapply(strsplit(sg_lines, ":"), function(x) trimws(x[1]))
  
  # =========================================================
  # READ mapSpeciesGroups
  # =========================================================
  
  map_file <- file.path("data", jur, "mapSpeciesGroups.txt")
  map_lines <- readLines(map_file)
  
  mapSpeciesGroups <- lapply(map_lines, function(x) {
    parts <- strsplit(x, ":")[[1]]
    trimws(strsplit(parts[2], ",")[[1]])
  })
  
  names(mapSpeciesGroups) <- sapply(strsplit(map_lines, ":"), function(x) trimws(x[1]))
  
  # =========================================================
  # COHORT DATA PROCESSING
  # =========================================================
  
  cohortDT <- data.table::copy(cohortData)
  
  cohortDT[, group := NA_character_]
  for (g in names(speciesGroups)) {
    cohortDT[speciesCode %in% speciesGroups[[g]], group := g]
  }
  
  cohortDT <- cohortDT[!is.na(group)]
  
  pixelGroups <- cohortDT[
    , .(biomass = sum(B)),
    by = .(pixelGroup, group, age)
  ]
  
  pixelWide <- data.table::dcast(
    pixelGroups,
    pixelGroup ~ group,
    value.var = "biomass",
    fun.aggregate = sum,
    fill = 0
  )
  
  group_cols <- setdiff(names(pixelWide), "pixelGroup")
  pixelWide[, total := rowSums(.SD), .SDcols = group_cols]
  pixelWide[total == 0, total := 1]
  
  if (!"borealDeciduous_AB" %in% names(pixelWide)) pixelWide[, borealDeciduous_AB := 0]
  if (!"whiteSpruce_AB" %in% names(pixelWide)) pixelWide[, whiteSpruce_AB := 0]
  if (!"blackSpruce_AB" %in% names(pixelWide)) pixelWide[, blackSpruce_AB := 0]
  if (!"borealPine_AB" %in% names(pixelWide)) pixelWide[, borealPine_AB := 0]
  
  pixelWide[, prop_deciduous := borealDeciduous_AB / total]
  pixelWide[, prop_sw        := whiteSpruce_AB / total]
  pixelWide[, prop_sb        := blackSpruce_AB / total]
  pixelWide[, prop_pine      := borealPine_AB / total]
  
  # =========================================================
  # PREP AGE INDEX
  # =========================================================
  
  ageDT <- cohortDT[
    , .(age = weighted.mean(age, B)),
    by = pixelGroup
  ]
  
  pixelWide <- merge(pixelWide, ageDT, by = "pixelGroup", all.x = TRUE)
  
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
    
    target_groups <- mapSpeciesGroups[[curve_name]]
    
    curve_dec  <- rep(0, length(curve_con_vals))
    curve_sw   <- rep(0, length(curve_con_vals))
    curve_sb   <- rep(0, length(curve_con_vals))
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
    
    dist <- pmax(
      abs(pixelWide$prop_deciduous - curve_dec),
      abs(pixelWide$prop_sw        - curve_sw),
      abs(pixelWide$prop_sb        - curve_sb),
      abs(pixelWide$prop_pine      - curve_pine)
    )
    
    results[[curve_name]] <- dist
  }
  
  distMatrix <- as.data.frame(results)
  
  pixelWide$bestCurve <- names(distMatrix)[
    max.col(-as.matrix(distMatrix))
  ]
  
  # =========================================================
  # CREATE RASTER OUTPUT
  # =========================================================
  
  curve_levels <- c("Aw", "AwSw", "SwAw", "Sw", "Sb", "Pj", "MxPj")
  pixelWide[, classID := as.numeric(factor(bestCurve, levels = curve_levels))]
  
  lookup <- pixelWide[, .(pixelGroup, classID)]
  
  vals <- terra::values(pixelGroupMap)[, 1]
  
  valid_vals <- !is.na(vals) & !is.nan(vals)
  idx <- match(vals, lookup$pixelGroup)
  
  if (any(is.na(idx[valid_vals]))) {
    warning(sum(is.na(idx[valid_vals])), " non-NA pixelGroups not matched to lookup")
  }
  
  new_vals <- lookup$classID[idx]
  new_vals[is.na(new_vals)] <- 0
  
  analysisUnitRaster <- pixelGroupMap
  terra::values(analysisUnitRaster) <- new_vals
  
  # =========================================================
  # APPLY HARVESTABLE MASK
  # =========================================================
  
  hf <- terra::values(sim$harvestableFraction)
  analysisUnitRaster[hf == 0] <- NA
  
  class_table <- data.frame(
    classID = 0:7,
    curve = c("background", "Aw", "AwSw", "SwAw", "Sw", "Sb", "Pj", "MxPj")
  )
  
  levels(analysisUnitRaster) <- class_table
  
  # =========================================================
  # AREA CALCULATION
  # =========================================================
  
  cell_area <- prod(terra::res(analysisUnitRaster))
  cell_area_ha <- cell_area / 10000
  
  freq_table <- as.data.frame(terra::freq(analysisUnitRaster))
  freq_table <- freq_table[freq_table$value != 0, ]
  freq_table$area_ha <- freq_table$count * cell_area_ha
  
  areaByAU <- freq_table[, c("value", "area_ha")]
  names(areaByAU) <- c("curve", "area_ha")
  
  # =========================================================
  # SAVE OUTPUTS
  # =========================================================
  
  sim$analysisUnitDT <- pixelWide
  sim$analysisUnitMap <- analysisUnitRaster
  sim$areaByAU <- areaByAU
  sim$yieldCurves <- curves
  
  return(sim)
}