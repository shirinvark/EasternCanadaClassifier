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
  
  resXY <- terra::res(planningRaster)
  cellArea_ha <- (resXY[1] * resXY[2]) / 10000
  
  isHarvestEligible <- harvestable > 0
  
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
  
  # --- بقیه کدت همینجا ادامه پیدا کند ---
  
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
  
  return(sim)
}