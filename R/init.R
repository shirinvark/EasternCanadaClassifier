Init <- function(sim) {
  
  planningRaster <- sim$Landbase$baseData$planningRaster
  landcover      <- sim$Landbase$baseData$landcover
  standAge       <- sim$Landbase$baseData$standAge
  harvestable    <- sim$Landbase$fractional$harvestableFraction
  
  message("Building simple analysisUnitMap")
  
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
  
  # --- cell area ---
  resXY <- terra::res(planningRaster)
  cellArea_ha <- (resXY[1] * resXY[2]) / 10000
  
  # --- Eligible mask ---
  isHarvestEligible <- harvestable > 0
  
  # --- Zonal summaries ---
  eligibleArea_by_AU <- terra::zonal(
    isHarvestEligible,
    analysisUnitMap,
    fun = "sum",
    na.rm = TRUE
  )
  
  colnames(eligibleArea_by_AU)[1] <- "zone"
  colnames(eligibleArea_by_AU)[2] <- "eligibleCells"
  
  harvestableArea_by_AU <- terra::zonal(
    harvestable,
    analysisUnitMap,
    fun = "sum",
    na.rm = TRUE
  )
  
  colnames(harvestableArea_by_AU)[1] <- "zone"
  colnames(harvestableArea_by_AU)[2] <- "harvestableFraction"
  
  AU_summaries <- merge(
    eligibleArea_by_AU,
    harvestableArea_by_AU,
    by = "zone"
  )
  
  # SAFETY CHECK
  if (!is.null(eligibleArea_by_AU) && nrow(eligibleArea_by_AU) > 0) {
    
    AU_summaries <- merge(
      eligibleArea_by_AU,
      harvestableArea_by_AU,
      by = "zone",
      suffixes = c("_eligibleCells", "_harvestableFraction")
    )
    
    AU_summaries$eligibleArea_ha <-
      AU_summaries$sum_eligibleCells * cellArea_ha
    
    AU_summaries$harvestableArea_ha <-
      AU_summaries$sum_harvestableFraction * cellArea_ha
    
    sim$AU_summaries <- AU_summaries
    
  } else {
    
    sim$AU_summaries <- data.frame()
  }
  
  # --- Totals ---
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
  
  # placeholder for ageAreaTable (so module matches metadata)
  sim$ageAreaTable <- data.frame()
  
  return(sim)
}