
# =============================================================================
# NATIONAL RSYC - YIELD CURVE TRANSLATION
#
# EasternCanadaClassifier
#
# Purpose:
#   Read national RSYC volume yield curves and translate them
#   into the annual yield-table structure used by the classifier.
#
# Input:
#   RSYC national volume yield curves (ages 0-255)
#
# Output:
#   $yieldTables : named list of annual yield tables
#   $curveLookup : curve metadata and ecodistrict IDs
#
# Notes:
#   - National curves are NOT separated by province.
#   - Original volume values are preserved.
#   - No species mapping is performed.
#   - No spatial matching or pixel classification is performed.
#   - The volume column is temporary until species mapping
#     and classifier integration are finalized.
# =============================================================================


translateNationalRSYC <- function(
    nationalFile = paste0(
      "E:/EasternCanadaClassifier/data/National/",
      "RSYC_national_volume_yield_curves_v20260923_age255.csv"
    ),
    maxAge = 255
) {
  
  # ---------------------------------------------------------------------------
  # 1. Read national RSYC curves
  # ---------------------------------------------------------------------------
  
  message("Reading National RSYC yield curves...")
  
  if (!file.exists(nationalFile)) {
    stop("National RSYC file not found: ", nationalFile)
  }
  
  nationalCurves <- data.table::fread(nationalFile)
  
  requiredCols <- c(
    "curveID",
    "model_id",
    "ecodistrict_id",
    "species",
    "model_group",
    "age",
    "volume_m3_ha",
    "model_version"
  )
  
  missingCols <- setdiff(requiredCols, names(nationalCurves))
  
  if (length(missingCols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missingCols, collapse = ", ")
    )
  }
  
  if (!identical(as.integer(maxAge), 255L) ||
      length(maxAge) != 1L ||
      is.na(maxAge)) {
    stop("This RSYC dataset is prepared for maxAge = 255.")
  }
  
  message("Input rows: ", nrow(nationalCurves))
  
  
  # ---------------------------------------------------------------------------
  # 2. Validate original curve structure
  # ---------------------------------------------------------------------------
  
  message("Validating National RSYC curves...")
  
  curveCheck <- nationalCurves[
    ,
    .(
      nAges = .N,
      nUniqueAges = data.table::uniqueN(age),
      minAge = min(age),
      maxAge = max(age),
      nEco = data.table::uniqueN(ecodistrict_id),
      nSpecies = data.table::uniqueN(species),
      nGroups = data.table::uniqueN(model_group),
      nModels = data.table::uniqueN(model_id),
      nVersions = data.table::uniqueN(model_version)
    ),
    by = curveID
  ]
  
  invalidCurves <- curveCheck[
    nAges != 256 |
      nUniqueAges != 256 |
      minAge != 0 |
      maxAge != 255 |
      nEco != 1 |
      nSpecies != 1 |
      nGroups != 1 |
      nModels != 1 |
      nVersions != 1
  ]
  
  if (nrow(invalidCurves) > 0) {
    stop(
      "Invalid National RSYC curves found: ",
      nrow(invalidCurves)
    )
  }
  
  if (
    anyNA(nationalCurves[, ..requiredCols]) ||
    any(!is.finite(nationalCurves$volume_m3_ha)) ||
    any(nationalCurves$volume_m3_ha < 0)
  ) {
    stop("Missing, non-finite, or negative values in RSYC data.")
  }
  
  message("Validated curves: ", nrow(curveCheck))
  
  
  # ---------------------------------------------------------------------------
  # 3. Prepare annual curves (ages 1-255)
  #
  # RSYC curves are already annual.
  # No spline interpolation or extrapolation is needed.
  # ---------------------------------------------------------------------------
  
  nationalAnnual <- nationalCurves[
    age >= 1 & age <= maxAge,
    .(
      curveID = paste0("NAT_", curveID),
      age,
      volume = volume_m3_ha
    )
  ]
  
  data.table::setorder(nationalAnnual, curveID, age)
  
  
  # ---------------------------------------------------------------------------
  # 4. Translate to classifier yieldTables structure
  #
  # Each curve is stored as:
  #
  #   yieldTables[["NAT_1"]]
  #
  #   age   volume
  #     1   ...
  #     2   ...
  #   ...
  #   255   ...
  # ---------------------------------------------------------------------------
  
  message("Building National yield tables...")
  
  nationalYieldTables <- split(
    nationalAnnual[, .(age, volume)],
    nationalAnnual$curveID
  )
  
  
  # ---------------------------------------------------------------------------
  # 5. Build curve lookup
  #
  # Preserve the original species and model group information.
  # Species mapping will be implemented separately.
  # ---------------------------------------------------------------------------
  
  nationalLookup <- unique(
    nationalCurves[
      ,
      .(
        curveID = paste0("NAT_", curveID),
        model_id,
        ecodistrict_id,
        species,
        model_group,
        model_version
      )
    ]
  )
  
  data.table::setorder(nationalLookup, curveID)
  
  
  # ---------------------------------------------------------------------------
  # 6. Validate translated yield tables
  # ---------------------------------------------------------------------------
  
  message("Validating translated yield tables...")
  
  curveLengths <- vapply(
    nationalYieldTables,
    nrow,
    integer(1)
  )
  
  if (!all(curveLengths == maxAge)) {
    stop("Some National yield tables do not contain 255 ages.")
  }
  
  if (anyDuplicated(nationalLookup$curveID)) {
    stop("Duplicate curve IDs found in National lookup.")
  }
  
  if (!setequal(
    names(nationalYieldTables),
    nationalLookup$curveID
  )) {
    stop("Yield table IDs do not match National lookup IDs.")
  }
  
  if (nrow(nationalAnnual) !=
      nrow(curveCheck) * maxAge) {
    stop("Unexpected number of translated yield rows.")
  }
  
  # Verify original volume values were preserved
  
  originalAnnual <- nationalCurves[
    age >= 1 & age <= maxAge,
    .(
      curveID = paste0("NAT_", curveID),
      age,
      originalVolume = volume_m3_ha
    )
  ]
  
  volumeCheck <- merge(
    nationalAnnual,
    originalAnnual,
    by = c("curveID", "age")
  )
  
  if (
    nrow(volumeCheck) != nrow(nationalAnnual) ||
    any(volumeCheck$volume != volumeCheck$originalVolume)
  ) {
    stop("Volume values changed during translation.")
  }
  
  
  # ---------------------------------------------------------------------------
  # 7. Summary
  # ---------------------------------------------------------------------------
  
  message("----------------------------------------")
  message("NATIONAL RSYC TRANSLATION COMPLETE")
  message("----------------------------------------")
  message("Yield tables: ", length(nationalYieldTables))
  message("Lookup records: ", nrow(nationalLookup))
  message("Ecodistricts: ",
          data.table::uniqueN(nationalLookup$ecodistrict_id))
  message("Age range: 1-", maxAge)
  message("Species mapping: NOT APPLIED")
  message("----------------------------------------")
  
  
  # ---------------------------------------------------------------------------
  # 8. Return
  # ---------------------------------------------------------------------------
  
  return(
    list(
      yieldTables = nationalYieldTables,
      curveLookup = nationalLookup
    )
  )
  
}
