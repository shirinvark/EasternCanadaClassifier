
library(data.table)

# ============================================================
# QC RSYC - Curve Availability by Ecodistrict
# ============================================================

qcDir <- file.path(
  "E:/EasternCanadaClassifier",
  "data", "Provincial", "QC", "processed"
)

inputFile <- file.path(
  qcDir,
  "QC_RSYC_Volume_Yield_Curves_v20260923_age255.csv"
)

outputFile <- file.path(
  qcDir,
  "QC_RSYC_Curve_Availability.csv"
)

stopifnot(file.exists(inputFile))

# 1. Read RSYC curves
rsyc <- fread(inputFile)

requiredCols <- c(
  "curveID",
  "ecodistrict_id",
  "ecoregion_id",
  "species",
  "model_group",
  "age"
)

stopifnot(all(requiredCols %in% names(rsyc)))

# 2. One record per curve
availability <- unique(
  rsyc[, .(
    ecodistrict_id,
    ecoregion_id,
    species,
    model_group,
    curveID
  )]
)

# 3. Validate uniqueness
duplicates <- availability[
  ,
  .N,
  by = .(ecodistrict_id, species)
][N > 1]

if (nrow(duplicates) > 0) {
  stop(
    "Multiple curves found for the same ",
    "ecodistrict and species."
  )
}

# 4. Curve type
availability[
  ,
  curve_type := fifelse(
    species == "generic",
    "generic",
    fifelse(
      species %in% c("coniferous", "broadleaf"),
      "functional_group",
      "species_specific"
    )
  )
]

# 5. Order and save
setcolorder(
  availability,
  c(
    "ecodistrict_id",
    "ecoregion_id",
    "species",
    "curve_type",
    "model_group",
    "curveID"
  )
)

setorder(
  availability,
  ecodistrict_id,
  species
)

fwrite(availability, outputFile)

# ============================================================
# VALIDATION
# ============================================================

cat("\n===== QC RSYC VALIDATION =====\n")

cat(
  "Ecodistricts:",
  uniqueN(availability$ecodistrict_id),
  "\n"
)

cat(
  "Curves:",
  uniqueN(availability$curveID),
  "\n"
)

cat("\n===== CURVES BY SPECIES =====\n")

print(
  availability[
    ,
    .N,
    by = species
  ][order(species)]
)

# Check generic and functional group availability
requiredGeneral <- c(
  "generic",
  "coniferous",
  "broadleaf"
)

ecodistricts <- sort(
  unique(availability$ecodistrict_id)
)

generalCheck <- CJ(
  ecodistrict_id = ecodistricts,
  species = requiredGeneral
)

generalCheck[
  availability,
  curveID := i.curveID,
  on = .(ecodistrict_id, species)
]

missingGeneral <- generalCheck[
  is.na(curveID)
]

cat("\n===== MISSING GENERAL CURVES =====\n")
print(missingGeneral)

cat("\nSaved to:\n", outputFile, "\n")
