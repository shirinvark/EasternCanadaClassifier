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
  
  harvestableArea_by_AU <- terra::zonal(
    harvestable,
    analysisUnitMap,
    fun = "sum",
    na.rm = TRUE
  )
  
  # اگر zonal خالی بود
  if (is.null(eligibleArea_by_AU) || nrow(eligibleArea_by_AU) == 0) {
    sim$AU_summaries <- data.frame()
    
  } else {
    
    colnames(eligibleArea_by_AU) <- c("zone", "eligibleCells")
    colnames(harvestableArea_by_AU) <- c("zone", "harvestableFraction")
    
    AU_summaries <- merge(
      eligibleArea_by_AU,
      harvestableArea_by_AU,
      by = "zone",
      all = TRUE
    )
    
    AU_summaries$eligibleArea_ha <-
      AU_summaries$eligibleCells * cellArea_ha
    
    AU_summaries$harvestableArea_ha <-
      AU_summaries$harvestableFraction * cellArea_ha
    
    sim$AU_summaries <- AU_summaries
  }
  
  # --- Totals ---
  sim$totalEligibleArea_ha <-
    terra::global(isHarvestEligible, "sum", na.rm = TRUE)[1,1] * cellArea_ha
  
  sim$totalHarvestableArea_ha <-
    terra::global(harvestable, "sum", na.rm = TRUE)[1,1] * cellArea_ha
  
  sim$ageAreaTable <- data.frame()
  
  return(sim)
}