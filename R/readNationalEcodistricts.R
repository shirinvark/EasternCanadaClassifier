# =============================================================================
# NATIONAL RSYC - READ ECODISTRICT POLYGONS
#
# Purpose:
#   Read and validate National RSYC ecodistrict polygons.
#
# Output:
#   SpatVector containing ecodistrict polygons and IDs.
# =============================================================================

readNationalEcodistricts <- function(
    nationalPath = "E:/EasternCanadaClassifier/data/National"
) {
  
  ecoFile <- file.path(
    nationalPath,
    "RSYC_ecostrat.gpkg"
  )
  
  if (!file.exists(ecoFile)) {
    stop("National RSYC ecodistrict file not found: ", ecoFile)
  }
  
  eco <- terra::vect(
    ecoFile,
    layer = "ecodistricts"
  )
  
  if (!("ecodistrict_id" %in% names(eco))) {
    stop("Missing ecodistrict_id in National RSYC spatial layer.")
  }
  
  ids <- eco$ecodistrict_id
  
  if (anyNA(ids) || anyDuplicated(ids)) {
    stop("Missing or duplicate National RSYC ecodistrict IDs.")
  }
  
  return(eco)
}