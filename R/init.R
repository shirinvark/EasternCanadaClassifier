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
  
  url <- "https://raw.githubusercontent.com/shirinvark/EasternCanadaClassifier/main/data/AlPac%20AME%20Mixedwood%20VolTabs.vol"
  dest <- "AlPac_AME_Mixedwood_VolTabs.vol"
  
  # Download file if it does not exist locally
  if (!file.exists(dest)) {
    download.file(url, dest, mode = "wb")
  }
  
  # Read file
  lines <- readLines(dest)
  
  # Parse header information
  header <- strsplit(lines[1], "\\s+")[[1]]
  nCurves <- as.numeric(gsub("#", "", header[1]))
  nAges   <- as.numeric(header[2])
  
  # Extract numeric data
  dataLines <- lines[2:(nCurves * 2 + 1)]
  
  dataMatrix <- do.call(rbind, lapply(dataLines, function(x) {
    x <- trimws(x)   # Important: remove extra whitespace
    as.numeric(strsplit(x, "\\s+")[[1]])
  }))
  
  # Build list of curves (conifer / deciduous)
  curves <- list()
  
  for (i in 1:nCurves) {
    
    conifer_row   <- dataMatrix[(2*i - 1), ]
    deciduous_row <- dataMatrix[(2*i), ]
    
    curves[[i]] <- list(
      conifer   = conifer_row,
      deciduous = deciduous_row
    )
  }
  
  # Convert curves to proportional form
  curves_prop <- list()
  
  for (i in 1:nCurves) {
    
    con <- curves[[i]]$conifer
    dec <- curves[[i]]$deciduous
    
    total <- con + dec
    total[total == 0] <- 1
    
    prop_con <- con / total
    prop_dec <- dec / total
    
    curves_prop[[i]] <- list(
      conifer   = prop_con,
      deciduous = prop_dec
    )
  }
  
  # Assign names to curves
  curve_names <- c(
    "Aw",
    "AwS",   # ignored
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
  
  # =========================================================
  # COHORT DATA PROCESSING
  # =========================================================
  
  cohortDT <- data.table::copy(cohortData)
  
  # 1️⃣ Convert speciesCode to character
  cohortDT[, speciesCode := as.character(speciesCode)]
  
  # 2️⃣ Create group column
  cohortDT[, group := NA_character_]
  
  # 3️⃣ Map species to groups
  
  # deciduous species
  cohortDT[speciesCode %in% c("Popu_tre", "Betu_pap"),
           group := "borealDeciduous_AB"]
  
  # white spruce / fir
  cohortDT[speciesCode %in% c("Abie_bal"),
           group := "whiteSpruce_AB"]
  
  # black spruce
  cohortDT[speciesCode %in% c("Pice_mar"),
           group := "blackSpruce_AB"]
  
  # pine species
  cohortDT[speciesCode %in% c("Pinu_ban", "Pinu_res", "Pinu_str"),
           group := "borealPine_AB"]
  
  # 4️⃣ Remove species not assigned to any group (e.g. Acer)
  cohortDT <- cohortDT[!is.na(group)]
  
  # 5️⃣ Aggregate biomass by pixel and group
  pixelGroups <- cohortDT[
    , .(biomass = sum(B)), 
    by = .(pixelGroup, group)
  ]
  
  # 6️⃣ Convert to wide format (one column per group)
  pixelWide <- data.table::dcast(
    pixelGroups,
    pixelGroup ~ group,
    value.var = "biomass",
    fill = 0
  )
  
  # 7️⃣ Compute total biomass
  pixelWide[, total := borealDeciduous_AB + whiteSpruce_AB + 
              blackSpruce_AB + borealPine_AB]
  
  # 8️⃣ Avoid division by zero
  pixelWide[total == 0, total := 1]
  
  # 9️⃣ Convert to proportions
  pixelWide[, `:=`(
    prop_deciduous = borealDeciduous_AB / total,
    prop_sw        = whiteSpruce_AB / total,
    prop_sb        = blackSpruce_AB / total,
    prop_pine      = borealPine_AB / total
  )]
  
  # =========================================================
  # PREP AGE INDEX (ONCE)
  # =========================================================
  
  # Compute biomass-weighted mean age per pixel
  ages <- cohortDT[, .(age = weighted.mean(age, B)), by = pixelGroup]  
  
  # Merge age into pixel table
  pixelWide <- merge(pixelWide, ages, by = "pixelGroup", all.x = TRUE)
  
  # Replace missing ages with 0
  pixelWide[is.na(age), age := 0]
  
  # Convert age to index (10-year classes)
  age_index <- floor(pixelWide$age / 10) + 1
  age_index <- pmax(1, pmin(age_index, 21))
  
  # =========================================================
  # MATCH PIXELS TO CURVES
  # =========================================================
  
  # Compute total conifer proportion
  total_con <- pixelWide$prop_sw + pixelWide$prop_sb + pixelWide$prop_pine
  total_con[total_con == 0] <- 1
  
  # Compute shares within conifer
  sw_share   <- pixelWide$prop_sw   / total_con
  sb_share   <- pixelWide$prop_sb   / total_con
  pine_share <- pixelWide$prop_pine / total_con
  
  results <- list()
  
  for (curve_name in names(curves_prop)) {
    
    curve <- curves_prop[[curve_name]]
    
    curve_con_vals <- curve$conifer[age_index]
    curve_dec_vals <- curve$deciduous[age_index]
    
    # Distribute conifer proportion across species groups
    curve_sw   <- curve_con_vals * sw_share
    curve_sb   <- curve_con_vals * sb_share
    curve_pine <- curve_con_vals * pine_share
    
    # Compute distance (max absolute difference)
    dist <- pmax(
      abs(pixelWide$prop_deciduous - curve_dec_vals),
      abs(pixelWide$prop_sw        - curve_sw),
      abs(pixelWide$prop_sb        - curve_sb),
      abs(pixelWide$prop_pine      - curve_pine)
    )
    
    results[[curve_name]] <- dist
  }
  
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
  
  # Extract raster values
  vals <- terra::values(pixelGroupMap)
  
  # Match pixelGroup to classID
  idx <- match(vals, lookup$pixelGroup)
  
  # Warn if some pixelGroups are missing
  if (any(is.na(idx))) {
    warning("Some pixelGroups not matched to lookup")
  }  
  
  # Assign class IDs to raster
  new_vals <- lookup$classID[idx]
  new_vals[is.na(new_vals)] <- 0  
  
  # Create output raster
  analysisUnitRaster <- pixelGroupMap
  values(analysisUnitRaster) <- new_vals
  
  # Define raster categories
  class_table <- data.frame(
    classID = 1:7,
    curve = c("Aw","AwSw","SwAw","Sw","Sb","Pj","MxPj")
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
  # SAVE OUTPUTS
  # =========================================================
  
  sim$analysisUnitDT <- pixelWide
  sim$analysisUnitRaster <- analysisUnitRaster
  sim$areaByAU <- areaByAU
}