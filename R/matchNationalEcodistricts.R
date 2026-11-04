
# =============================================================================
# NATIONAL RSYC - MATCH PIXEL GROUPS TO ECODISTRICTS
#
# EasternCanadaClassifier
#
# Purpose:
#   Assign each pixelGroup to the ecodistrict covering the
#   largest number of its raster cells.
#
# Inputs:
#   pixelGroupMap : SpatRaster containing pixelGroup IDs
#   ecodistrictMap: SpatRaster containing ecodistrict IDs
#
# Output:
#   data.table with:
#     pixelGroup
#     ecodistrict_id
#     nEcodistricts
#     nCells
#     nMatchedCells
#
# Notes:
#   - Uses the dominant ecodistrict by cell count.
#   - Reports pixelGroups spanning multiple ecodistricts.
#   - Does not modify the input rasters.
# =============================================================================

matchNationalEcodistricts <- function(
    pixelGroupMap,
    ecodistrictMap
) {
  
  # ---------------------------------------------------------------------------
  # 1. Validate raster inputs
  # ---------------------------------------------------------------------------
  
  stopifnot(
    inherits(pixelGroupMap, "SpatRaster"),
    inherits(ecodistrictMap, "SpatRaster")
  )
  
  if (
    terra::nlyr(pixelGroupMap) != 1L ||
    terra::nlyr(ecodistrictMap) != 1L
  ) {
    stop("Both input rasters must contain exactly one layer.")
  }
  
  if (!terra::compareGeom(
    pixelGroupMap,
    ecodistrictMap,
    stopOnError = FALSE
  )) {
    stop("pixelGroupMap and ecodistrictMap are not aligned.")
  }
  
  # ---------------------------------------------------------------------------
  # 2. Extract raster values
  # ---------------------------------------------------------------------------
  
  pixelDT <- data.table::data.table(
    pixelGroup = terra::values(pixelGroupMap)[, 1],
    ecodistrict_id = terra::values(ecodistrictMap)[, 1]
  )
  
  pixelDT <- pixelDT[
    !is.na(pixelGroup)
  ]
  
  if (nrow(pixelDT) == 0L) {
    stop("No valid pixelGroups found.")
  }
  
  # ---------------------------------------------------------------------------
  # 3. Count all cells per pixelGroup
  # ---------------------------------------------------------------------------
  
  pixelCounts <- pixelDT[
    ,
    .(nCells = .N),
    by = pixelGroup
  ]
  
  # ---------------------------------------------------------------------------
  # 4. Count cells within each ecodistrict
  # ---------------------------------------------------------------------------
  
  matchedDT <- pixelDT[
    !is.na(ecodistrict_id)
  ]
  
  ecoCounts <- matchedDT[
    ,
    .(n = .N),
    by = .(pixelGroup, ecodistrict_id)
  ]
  
  # ---------------------------------------------------------------------------
  # 5. Select dominant ecodistrict
  #
  # If two ecodistricts have equal cell counts,
  # choose the smaller ecodistrict_id for reproducibility.
  # ---------------------------------------------------------------------------
  
  data.table::setorder(
    ecoCounts,
    pixelGroup,
    -n,
    ecodistrict_id
  )
  
  dominantEco <- ecoCounts[
    ,
    .SD[1],
    by = pixelGroup
  ]
  
  data.table::setnames(
    dominantEco,
    "n",
    "nMatchedCells"
  )
  
  # ---------------------------------------------------------------------------
  # 6. Count ecodistricts per pixelGroup
  # ---------------------------------------------------------------------------
  
  ecoSummary <- ecoCounts[
    ,
    .(nEcodistricts = .N),
    by = pixelGroup
  ]
  
  # ---------------------------------------------------------------------------
  # 7. Build final lookup
  # ---------------------------------------------------------------------------
  
  result <- merge(
    pixelCounts,
    dominantEco,
    by = "pixelGroup",
    all.x = TRUE
  )
  
  result <- merge(
    result,
    ecoSummary,
    by = "pixelGroup",
    all.x = TRUE
  )
  
  result[
    is.na(nEcodistricts),
    nEcodistricts := 0L
  ]
  
  data.table::setcolorder(
    result,
    c(
      "pixelGroup",
      "ecodistrict_id",
      "nEcodistricts",
      "nCells",
      "nMatchedCells"
    )
  )
  
  data.table::setorder(result, pixelGroup)
  
  return(result)
}
