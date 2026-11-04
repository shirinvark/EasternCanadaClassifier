# =============================================================================
# NATIONAL RSYC - PIXEL GROUP CLASSIFICATION
#
# EasternCanadaClassifier
#
# Purpose:
#   Classify LandR pixel groups using National RSYC yield curves.
#
# Inputs:
#   - sim$cohortData
#   - sim$pixelGroupMap
#   - National RSYC yield curves
#   - National species mapping
#   - National ecodistricts
#
# =============================================================================

classifyNationalRSYC <- function(sim) {
  
  # ---------------------------------------------------------------------------
  # 1. Validate upstream inputs
  # ---------------------------------------------------------------------------
  
  message("Starting National RSYC classification...")
  
  if (is.null(sim$cohortData)) {
    stop("Missing sim$cohortData.")
  }
  
  if (is.null(sim$pixelGroupMap)) {
    stop("Missing sim$pixelGroupMap.")
  }
  
  # ---------------------------------------------------------------------------
  # 2. Load National RSYC reference data
  # ---------------------------------------------------------------------------
  
  nationalResult <- translateNationalRSYC()
  
  nationalMapping <- readNationalSpeciesMapping()
  
  nationalEco <- readNationalEcodistricts()
  
  message("National RSYC reference data loaded.")
  
  # ---------------------------------------------------------------------------
  # 3. Match pixel groups to National RSYC ecodistricts
  # ---------------------------------------------------------------------------
  
  ecodistrictMap <- buildNationalEcodistrictMap(
    pixelGroupMap = sim$pixelGroupMap,
    nationalEco = nationalEco
  )
  
  pixelGroupEco <- matchNationalEcodistricts(
    pixelGroupMap = sim$pixelGroupMap,
    ecodistrictMap = ecodistrictMap
  )
  
  message(
    "Pixel groups matched to ecodistricts: ",
    sum(!is.na(pixelGroupEco$ecodistrict_id)),
    " / ",
    nrow(pixelGroupEco)
  )
  # ---------------------------------------------------------------------------
  # 4. Map LandR cohort species to National RSYC species
  # ---------------------------------------------------------------------------
  
  cohortDT <- data.table::as.data.table(
    data.table::copy(sim$cohortData)
  )
  
  # Standardize pixelGroup column name
  pg_col <- grep(
    "^pixelgroup$",
    names(cohortDT),
    ignore.case = TRUE,
    value = TRUE
  )
  
  if (length(pg_col) != 1L) {
    stop("Could not uniquely identify pixelGroup column.")
  }
  
  data.table::setnames(cohortDT, pg_col, "pixelGroup")
  
  # Validate required cohort columns
  requiredCols <- c("pixelGroup", "speciesCode", "age", "B")
  
  if (!all(requiredCols %in% names(cohortDT))) {
    stop("Missing required columns in cohortData.")
  }
  
  # Match LandR species codes to National RSYC species
  cohortDT[, speciesCode := as.character(speciesCode)]
  
  cohortDT <- merge(
    cohortDT,
    nationalMapping,
    by.x = "speciesCode",
    by.y = "LandR",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Check species mapping
  message(
    "Mapped cohort records: ",
    sum(!is.na(cohortDT$species)),
    " / ",
    nrow(cohortDT)
  )
  
  unmappedSpecies <- unique(
    cohortDT[is.na(species), speciesCode]
  )
  
  if (length(unmappedSpecies) > 0L) {
    warning(
      "Unmapped LandR species: ",
      paste(unmappedSpecies, collapse = ", ")
    )
  }
    # ---------------------------------------------------------------------------
    # 5. Attach National RSYC ecodistricts to cohort records
    # ---------------------------------------------------------------------------
    
    cohortEco <- merge(
      cohortDT,
      pixelGroupEco[, .(pixelGroup, ecodistrict_id)],
      by = "pixelGroup",
      all.x = TRUE,
      sort = FALSE
    )
    
    message(
      "Cohort records matched to ecodistricts: ",
      sum(!is.na(cohortEco$ecodistrict_id)),
      " / ",
      nrow(cohortEco)
    )
    # ---------------------------------------------------------------------------
    # 6. Match cohorts to species-specific National RSYC curves
    # ---------------------------------------------------------------------------
    
    speciesCurves <- nationalResult$curveLookup[
      model_group == "species",
      .(ecodistrict_id, species, curveID)
    ]
    
    cohortCurves <- merge(
      cohortEco,
      speciesCurves,
      by = c("ecodistrict_id", "species"),
      all.x = TRUE,
      sort = FALSE
    )
    
    message(
      "Cohort records with species-specific curves: ",
      sum(!is.na(cohortCurves$curveID)),
      " / ",
      nrow(cohortCurves)
    )
    # Validate cohort record counts
    message(
      "Record count validation: ",
      "cohortDT = ", nrow(cohortDT),
      ", cohortEco = ", nrow(cohortEco),
      ", cohortCurves = ", nrow(cohortCurves)
    )
 
}