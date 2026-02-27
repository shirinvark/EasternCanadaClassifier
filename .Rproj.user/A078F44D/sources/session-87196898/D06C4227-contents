Init <- function(sim) {
  
  checkObject(sim, "PlanningGrid_250m", "SpatRaster")
  checkObject(sim, "LandCover_250m", "SpatRaster")
  checkObject(sim, "standAge_250m", "SpatRaster")
  checkObject(sim, "Riparian", "list")
  checkObject(sim, "LegalConstraints", "list")
  
  landCoverAligned  <- sim$LandCover_250m
  standAgeAligned   <- sim$standAge_250m
  riparianAligned   <- sim$Riparian$riparianFraction  
  # 2) protectedAreaMask
  # =========================================================
  
  message("Preparing protectedAreaMask")
  
  if (!is.null(sim$LegalConstraints) &&
      !is.null(sim$LegalConstraints$CPCAD_Raster_250m) &&
      inherits(sim$LegalConstraints$CPCAD_Raster_250m, "SpatRaster")) {
    
    sim$protectedAreaMask <- sim$LegalConstraints$CPCAD_Raster_250m
    
  } else {
    
    message("No valid CPCAD raster found → protectedAreaMask = 0")
    
    sim$protectedAreaMask <- terra::rast(sim$PlanningGrid_250m)
    sim$protectedAreaMask[] <- 0
  }
  # ========================================================
  # 3) forestCoverMask
  # ========================================================
  
  sim$forestCoverMask <- terra::ifel(
    !is.na(landCoverAligned) &
      (
        landCoverAligned == 210 |
          landCoverAligned == 220 |
          landCoverAligned == 230
      ),
    1,
    0
  )
  
  # =========================================================
  # 5) isHarvestEligible (forest + protected + age)
  # =========================================================
  
  ageValid <- terra::ifel(
    !is.na(standAgeAligned) & standAgeAligned > 0,
    1,
    0
  )
  
  isHarvestEligible <- terra::ifel(
    sim$forestCoverMask == 1 &
      sim$protectedAreaMask == 0 &
      ageValid == 1,
    1,
    0
  )
  
  
  
  # =========================================================
  # 6) APPLY RIPARIAN REDUCTION
  # =========================================================
  
  if (!inherits(riparianAligned, "SpatRaster")) {
    stop("Riparian$riparianFraction must be a SpatRaster")
  }
  
  message("Applying riparian reduction")
  
  sim$harvestableFraction <- isHarvestEligible * (1 - riparianAligned)
  
 
  # =========================================================
  # 8) FINAL LANDBASE
  # =========================================================
  sim$isHarvestEligible <- isHarvestEligible
  
  sim$Landbase <- list(
    
    baseData = list(
      planningRaster = sim$PlanningGrid_250m,
      landcover      = landCoverAligned,
      standAge       = standAgeAligned
    ),
    
    masks = list(
      forestCoverMask     = sim$forestCoverMask,
      protectedAreaMask   = sim$protectedAreaMask,
      isHarvestEligible   = sim$isHarvestEligible
    ),
    
    fractional = list(
      riparianFraction    = riparianAligned,
      harvestableFraction = sim$harvestableFraction
    )
  )
    
  invisible(sim)
}
