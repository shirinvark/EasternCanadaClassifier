
# =============================================================================
# NATIONAL RSYC - BUILD ECODISTRICT MAP
#
# Purpose:
#   Rasterize National RSYC ecodistrict polygons using
#   the classifier pixelGroupMap as the spatial template.
#
# Inputs:
#   pixelGroupMap : SpatRaster
#   nationalEco   : SpatVector
#
# Output:
#   ecodistrictMap : SpatRaster aligned with pixelGroupMap
# =============================================================================

buildNationalEcodistrictMap <- function(
    pixelGroupMap,
    nationalEco
) {
  
  # Validate inputs
  stopifnot(
    inherits(pixelGroupMap, "SpatRaster"),
    inherits(nationalEco, "SpatVector")
  )
  
  if (!("ecodistrict_id" %in% names(nationalEco))) {
    stop("Missing ecodistrict_id in National RSYC polygons.")
  }
  
  # Use pixelGroupMap as the spatial template
  template <- pixelGroupMap[[1]]
  
  # Reproject ecodistrict polygons to match raster CRS
  ecoProjected <- terra::project(
    nationalEco,
    terra::crs(template)
  )
  
  # Rasterize ecodistrict IDs
  ecodistrictMap <- terra::rasterize(
    ecoProjected,
    template,
    field = "ecodistrict_id",
    touches = FALSE,
    background = NA
  )
  
  names(ecodistrictMap) <- "ecodistrict_id"
  
  return(ecodistrictMap)
}
