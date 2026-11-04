
# =============================================================================
# NATIONAL RSYC - READ SPECIES MAPPING
#
# EasternCanadaClassifier
#
# Purpose:
#   Read and validate species mapping between LandR and National RSYC.
#
# Inputs:
#   mapSpeciesGroups_National.txt
#   speciesGroups_National.txt
#
# Output:
#   data.table with columns:
#     LandR
#     species
# =============================================================================

readNationalSpeciesMapping <- function(
    nationalPath = "E:/EasternCanadaClassifier/data/National"
) {
  
  # Read LandR-to-RSYC species mapping
  mapFile <- file.path(
    nationalPath,
    "mapSpeciesGroups_National.txt"
  )
  
  # Read the list of National RSYC species
  speciesFile <- file.path(
    nationalPath,
    "speciesGroups_National.txt"
  )
  
  mapLines <- trimws(readLines(mapFile, warn = FALSE))
  speciesLines <- trimws(readLines(speciesFile, warn = FALSE))
  
  # Remove empty lines
  mapLines <- mapLines[nzchar(mapLines)]
  speciesLines <- speciesLines[nzchar(speciesLines)]
  
  # Validate mapping format
  if (!all(grepl("^[^:]+:[^:]+$", mapLines))) {
    stop("Invalid National species mapping format.")
  }
  
  # Parse LandR: RSYC entries
  mapping <- data.table::data.table(
    LandR = trimws(sub(":.*$", "", mapLines)),
    species = trimws(sub("^[^:]+:", "", mapLines))
  )
  
  # Validate uniqueness and completeness
  if (
    anyDuplicated(mapping$LandR) ||
    anyDuplicated(mapping$species) ||
    anyDuplicated(speciesLines) ||
    !setequal(mapping$species, speciesLines)
  ) {
    stop("National species mapping validation failed.")
  }
  
  return(mapping)
}
