planningRaster <- sim$Landbase$baseData$planningRaster
landcover      <- sim$Landbase$baseData$landcover
standAge       <- sim$Landbase$baseData$standAge
harvestable    <- sim$Landbase$fractional$harvestableFraction

message("Building simple analysisUnitMap")

# =========================================================
# BUILD SIMPLE ANALYSIS UNIT (DEV MODE)
# =========================================================

analysisUnitMap <- terra::ifel(
  harvestable > 0,
  terra::classify(
    landcover,
    rcl = matrix(
      c(210, 1,
        220, 2,
        230, 3),
      ncol = 2,
      byrow = TRUE
    )
  ),
  NA
)

sim$analysisUnitMap <- analysisUnitMap


# =========================================================
# AREA PER ANALYSIS UNIT (AAC-ready)
# =========================================================

resXY <- terra::res(planningRaster)
cellArea_ha <- (resXY[1] * resXY[2]) / 10000

isHarvestEligible <- harvestable > 0

# ---------------------------
# ZONAL CALCULATIONS
# ---------------------------

eligibleArea_by_AU <- terra::zonal(
  isHarvestEligible,
  analysisUnitMap,
  fun = "sum",
  na.rm = TRUE
)

harvestableArea_by_AU <- terra::zonal(
  harvestable,
  analysisUnitMap,
  fun = "sum",
  na.rm = TRUE
)

# ---------------------------
# Convert to hectares
# ---------------------------

if (!is.null(eligibleArea_by_AU) &&
    nrow(eligibleArea_by_AU) > 0 &&
    "sum" %in% names(eligibleArea_by_AU)) {
  
  eligibleArea_by_AU$sum <- eligibleArea_by_AU$sum * cellArea_ha
  colnames(eligibleArea_by_AU) <- c("analysisUnit", "eligibleArea_ha")
  
} else {
  
  eligibleArea_by_AU <- data.frame(
    analysisUnit = numeric(0),
    eligibleArea_ha = numeric(0)
  )
}

if (!is.null(harvestableArea_by_AU) &&
    nrow(harvestableArea_by_AU) > 0 &&
    "sum" %in% names(harvestableArea_by_AU)) {
  
  harvestableArea_by_AU$sum <- harvestableArea_by_AU$sum * cellArea_ha
  colnames(harvestableArea_by_AU) <- c("analysisUnit", "harvestableArea_ha")
  
} else {
  
  harvestableArea_by_AU <- data.frame(
    analysisUnit = numeric(0),
    harvestableArea_ha = numeric(0)
  )
}

areaByAU <- merge(
  eligibleArea_by_AU,
  harvestableArea_by_AU,
  by = "analysisUnit",
  all = TRUE
)

sim$AU_summaries <- areaByAU


# =========================================================
# AGE CLASS PER AU
# =========================================================

ageBreaks <- c(0, 20, 40, 60, 80, 100, 150, Inf)

ageClassRaster <- terra::classify(
  standAge,
  rcl = cbind(
    ageBreaks[-length(ageBreaks)],
    ageBreaks[-1],
    seq_len(length(ageBreaks) - 1)
  )
)

au_age_combo <- terra::ifel(
  analysisUnitMap > 0,
  analysisUnitMap * 1000 + ageClassRaster,
  NA
)

ageAreaTable <- terra::zonal(
  harvestable,
  au_age_combo,
  fun = "sum",
  na.rm = TRUE
)

if (!is.null(ageAreaTable) &&
    nrow(ageAreaTable) > 0 &&
    "sum" %in% names(ageAreaTable)) {
  
  colnames(ageAreaTable) <- c("AU_AgeCode", "sum")
  ageAreaTable <- ageAreaTable[ageAreaTable$AU_AgeCode > 0, ]
  
  if (nrow(ageAreaTable) > 0) {
    
    ageAreaTable$harvestableArea_ha <- ageAreaTable$sum * cellArea_ha
    ageAreaTable$analysisUnit <- ageAreaTable$AU_AgeCode %/% 1000
    ageAreaTable$ageClass     <- ageAreaTable$AU_AgeCode %% 1000
    
    ageAreaTable <- ageAreaTable[, c("analysisUnit", "ageClass", "harvestableArea_ha")]
    
  } else {
    ageAreaTable <- data.frame(
      analysisUnit = numeric(0),
      ageClass = numeric(0),
      harvestableArea_ha = numeric(0)
    )
  }
  
} else {
  
  ageAreaTable <- data.frame(
    analysisUnit = numeric(0),
    ageClass = numeric(0),
    harvestableArea_ha = numeric(0)
  )
}

sim$ageAreaTable <- ageAreaTable


# =========================================================
# TOTAL AREA METRICS
# =========================================================

sim$totalEligibleArea_ha <- terra::global(
  isHarvestEligible,
  "sum",
  na.rm = TRUE
)[1,1] * cellArea_ha

sim$totalHarvestableArea_ha <- terra::global(
  harvestable,
  "sum",
  na.rm = TRUE
)[1,1] * cellArea_ha