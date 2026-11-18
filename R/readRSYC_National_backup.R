
# =============================================================================
# National RSYC Yield Curves
# EasternCanadaClassifier
#
# Purpose:
#   - Read pre-extracted national RSYC volume yield curves
#   - Validate curve and ecodistrict identifiers
#   - Inspect model groups and species
#   - Read national ecodistrict boundaries
#
# Note:
#   National RSYC data are organized by ecodistrict,
#   NOT by province.
# =============================================================================


# ---------------------------------------------------------
# 1. Load libraries
# ---------------------------------------------------------

library(data.table)
library(sf)


# ---------------------------------------------------------
# 2. Define National data directory
# ---------------------------------------------------------

nationalDir <- "E:/EasternCanadaClassifier/data/National"

curveFile <- file.path(
  nationalDir,
  "RSYC_national_volume_yield_curves_v20260923_age200.csv"
)

lookupFile <- file.path(
  nationalDir,
  "RSYC_national_volume_curve_lookup_v20260923_age200.csv"
)

ecoFile <- "E:/QC/RSYC_ecostrat.gpkg"


# ---------------------------------------------------------
# 3. Read National yield curves
# ---------------------------------------------------------

stopifnot(
  file.exists(curveFile),
  file.exists(lookupFile),
  file.exists(ecoFile)
)

yieldCurves <- fread(curveFile)

curveLookup <- fread(lookupFile)

cat("\nNATIONAL RSYC DATA\n")

cat("Yield curve rows:", nrow(yieldCurves), "\n")
cat("Curve lookup rows:", nrow(curveLookup), "\n")

cat("\nYield curve columns:\n")
print(names(yieldCurves))

cat("\nCurve lookup columns:\n")
print(names(curveLookup))

print(head(yieldCurves, 3))
print(head(curveLookup, 3))


# ---------------------------------------------------------
# 4. Validate National curve structure
# ---------------------------------------------------------

requiredYieldColumns <- c(
  "curveID",
  "ecodistrict_id",
  "species",
  "model_group",
  "age",
  "volume_m3_ha"
)

requiredLookupColumns <- c(
  "curveID",
  "ecodistrict_id",
  "species",
  "model_group"
)

stopifnot(
  all(requiredYieldColumns %in% names(yieldCurves)),
  all(requiredLookupColumns %in% names(curveLookup))
)

cat("\nUNIQUE CURVES\n")

cat(
  "Yield curves:",
  uniqueN(yieldCurves$curveID),
  "\n"
)

cat(
  "Lookup curves:",
  uniqueN(curveLookup$curveID),
  "\n"
)

# Check duplicate curve IDs in lookup
cat(
  "Duplicate lookup curve IDs:",
  sum(duplicated(curveLookup$curveID)),
  "\n"
)

# Check that all yield curves have lookup records
cat(
  "Curves missing from lookup:",
  length(setdiff(
    unique(yieldCurves$curveID),
    unique(curveLookup$curveID)
  )),
  "\n"
)

cat(
  "Lookup curves missing yield data:",
  length(setdiff(
    unique(curveLookup$curveID),
    unique(yieldCurves$curveID)
  )),
  "\n"
)


# ---------------------------------------------------------
# 5. Inspect age and volume ranges
# ---------------------------------------------------------

cat("\nAGE RANGE\n")
print(range(yieldCurves$age, na.rm = TRUE))

cat("\nVOLUME SUMMARY (m3/ha)\n")
print(summary(yieldCurves$volume_m3_ha))

cat(
  "\nMissing volumes:",
  sum(is.na(yieldCurves$volume_m3_ha)),
  "\n"
)

cat(
  "Negative volumes:",
  sum(yieldCurves$volume_m3_ha < 0, na.rm = TRUE),
  "\n"
)

# Check duplicate age records within each curve
duplicateAges <- yieldCurves[
  ,
  .N,
  by = .(curveID, age)
][N > 1]

cat(
  "Duplicate curve-age combinations:",
  nrow(duplicateAges),
  "\n"
)


# ---------------------------------------------------------
# 6. Inspect curves per ecodistrict
# ---------------------------------------------------------

curvesPerDistrict <- curveLookup[
  ,
  .(
    nCurves = uniqueN(curveID),
    nSpecies = uniqueN(species)
  ),
  by = ecodistrict_id
]

cat("\nECODISTRICTS\n")

cat(
  "Number of ecodistricts:",
  nrow(curvesPerDistrict),
  "\n"
)

cat("\nCurves per ecodistrict:\n")
print(summary(curvesPerDistrict$nCurves))

cat("\nModel groups:\n")
print(table(curveLookup$model_group, useNA = "ifany"))


# ---------------------------------------------------------
# 7. Inspect National RSYC species
# ---------------------------------------------------------

speciesSummary <- curveLookup[
  ,
  .(
    nCurves = uniqueN(curveID),
    nEcodistricts = uniqueN(ecodistrict_id)
  ),
  by = .(model_group, species)
]

setorder(
  speciesSummary,
  model_group,
  species
)

cat("\nNATIONAL SPECIES SUMMARY\n")

print(speciesSummary, nrows = Inf)


# ---------------------------------------------------------
# 8. Inspect existing provincial species group files
#
# Reference only:
# Do not modify provincial classifier logic.
# ---------------------------------------------------------

onFile <- paste0(
  "E:/EasternCanadaClassifier/data/",
  "Provincial/ON/speciesGroups_ON.txt"
)

nlFile <- paste0(
  "E:/EasternCanadaClassifier/data/",
  "Provincial/NL/speciesGroups.txt"
)

if (file.exists(onFile)) {
  
  onGroups <- readLines(onFile, warn = FALSE)
  
  cat("\nONTARIO SPECIES GROUPS:\n")
  cat(onGroups, sep = "\n")
  
}

if (file.exists(nlFile)) {
  
  nlGroups <- readLines(nlFile, warn = FALSE)
  
  cat("\n\nNEWFOUNDLAND SPECIES GROUPS:\n")
  cat(nlGroups, sep = "\n")
  
}


# ---------------------------------------------------------
# 9. Read National RSYC ecodistrict boundaries
# ---------------------------------------------------------

eco <- st_read(
  ecoFile,
  layer = "ecodistricts",
  quiet = TRUE
)

cat("\nRSYC SPATIAL DATA\n")

cat(
  "Number of ecodistrict polygons:",
  nrow(eco),
  "\n"
)

print(names(eco))

stopifnot(
  "ecodistrict_id" %in% names(eco)
)


# ---------------------------------------------------------
# 10. Validate ecodistrict identifiers
# ---------------------------------------------------------

modelIDs <- unique(curveLookup$ecodistrict_id)

spatialIDs <- unique(eco$ecodistrict_id)

cat("\nECODISTRICT ID VALIDATION\n")

cat(
  "Model ecodistricts:",
  length(modelIDs),
  "\n"
)

cat(
  "Spatial ecodistricts:",
  length(spatialIDs),
  "\n"
)

missingSpatial <- setdiff(
  modelIDs,
  spatialIDs
)

missingModels <- setdiff(
  spatialIDs,
  modelIDs
)

cat(
  "Model IDs missing from spatial data:",
  length(missingSpatial),
  "\n"
)

cat(
  "Spatial IDs without models:",
  length(missingModels),
  "\n"
)

cat(
  "Duplicate spatial ecodistrict IDs:",
  sum(duplicated(eco$ecodistrict_id)),
  "\n"
)


# ---------------------------------------------------------
# 11. Validate ecodistrict geometries
# ---------------------------------------------------------

cat("\nSPATIAL VALIDATION\n")

cat(
  "CRS EPSG:",
  st_crs(eco)$epsg,
  "\n"
)

print(st_bbox(eco))

cat("\nGeometry validity:\n")
print(table(st_is_valid(eco), useNA = "ifany"))


# ---------------------------------------------------------
# 12. Final validation summary
# ---------------------------------------------------------

cat("\n========================================\n")
cat("NATIONAL RSYC VALIDATION SUMMARY\n")
cat("========================================\n")

cat("Yield rows:", nrow(yieldCurves), "\n")

cat(
  "Unique curves:",
  uniqueN(curveLookup$curveID),
  "\n"
)

cat(
  "Ecodistricts:",
  uniqueN(curveLookup$ecodistrict_id),
  "\n"
)

cat(
  "Spatial polygons:",
  nrow(eco),
  "\n"
)

cat(
  "Missing spatial IDs:",
  length(missingSpatial),
  "\n"
)

cat(
  "Missing model IDs:",
  length(missingModels),
  "\n"
)

cat(
  "Invalid geometries:",
  sum(!st_is_valid(eco), na.rm = TRUE),
  "\n"
)

cat("========================================\n")
# ---------------------------------------------------------
# 13. Check existing PlanningGrid
# ---------------------------------------------------------

exists("sim")

if (exists("sim")) {
  "PlanningGrid" %in% names(sim)
}

# Find PlanningGrid-related files
list.files(
  "E:/EasternCanadaDataPrep",
  recursive = TRUE,
  pattern = "PlanningGrid",
  ignore.case = TRUE
)

# ---------------------------------------------------------
# 14. Inspect existing PlanningGrid function
# ---------------------------------------------------------

planningGridCode <- readLines(
  "E:/EasternCanadaDataPrep/R/buildPlanningGrid.R",
  warn = FALSE
)

cat(planningGridCode, sep = "\n")

sim$PlanningGrid <- sim$rasterToMatch

terra::values(sim$PlanningGrid) <- 1
# ---------------------------------------------------------
# 15. Inspect Classifier module files
# ---------------------------------------------------------

list.files(
  "E:/EasternCanadaClassifier/modules",
  recursive = TRUE,
  pattern = "\\.R$"
)
# ---------------------------------------------------------
# 16. Inspect main EasternCanadaClassifier module
# ---------------------------------------------------------

classifierCode <- readLines(
  paste0(
    "E:/EasternCanadaClassifier/modules/",
    "EasternCanadaClassifier/EasternCanadaClassifier.R"
  ),
  warn = FALSE
)

cat(classifierCode, sep = "\n")
# ---------------------------------------------------------
# 17. Find yield curve assignment in provincial classifiers
# ---------------------------------------------------------

classifierDir <- paste0(
  "E:/EasternCanadaClassifier/modules/",
  "EasternCanadaClassifier/R"
)

for (province in c("ON", "NL")) {
  
  file <- file.path(
    classifierDir,
    paste0("classifyProvince_", province, ".R")
  )
  
  code <- readLines(file, warn = FALSE)
  
  hits <- grep(
    "curveID|AUtoCurve|pixelGroupToAU|classification",
    code,
    ignore.case = TRUE
  )
  
  cat("\n==========", province, "==========\n")
  
  print(
    data.frame(
      line = hits,
      code = trimws(code[hits])
    ),
    row.names = FALSE
  )
}
# ---------------------------------------------------------
# 18. Inspect AU and curve assignment
# ---------------------------------------------------------

onCode <- readLines(
  file.path(classifierDir, "classifyProvince_ON.R"),
  warn = FALSE
)

nlCode <- readLines(
  file.path(classifierDir, "classifyProvince_NL.R"),
  warn = FALSE
)

cat("\n========== ONTARIO ==========\n")
cat(
  sprintf(
    "%4d  %s",
    520:585,
    onCode[520:585]
  ),
  sep = "\n"
)

cat("\n\n========== NEWFOUNDLAND ==========\n")
cat(
  sprintf(
    "%4d  %s",
    700:785,
    nlCode[700:785]
  ),
  sep = "\n"
)
###############3
library(data.table)
library(ggplot2)

# Select ecodistrict with the most curves
selectedEco <- curvesPerDistrict[
  which.max(nCurves),
  ecodistrict_id
]

# Extract its curves
testCurves <- yieldCurves[
  ecodistrict_id == selectedEco
]

cat("Selected ecodistrict:", selectedEco, "\n")
cat("Number of curves:", uniqueN(testCurves$curveID), "\n")

# Plot all curves
ggplot(
  testCurves,
  aes(
    x = age,
    y = volume_m3_ha,
    color = species,
    group = curveID
  )
) +
  geom_line(linewidth = 0.9) +
  labs(
    title = paste("National RSYC – Ecodistrict", selectedEco),
    x = "Stand age (years)",
    y = "Volume (m³/ha)",
    color = "Species"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# 19. Check growth behaviour of all National curves
# ---------------------------------------------------------

curveBehaviour <- yieldCurves[
  order(age),
  .(
    volume100 = volume_m3_ha[age == 100],
    volume150 = volume_m3_ha[age == 150],
    volume200 = volume_m3_ha[age == 200],
    maxVolume = max(volume_m3_ha),
    peakAge = age[which.max(volume_m3_ha)]
  ),
  by = .(curveID, ecodistrict_id, species, model_group)
]

curveBehaviour[
  ,
  behaviour := fifelse(
    volume200 > volume150,
    "Increasing",
    fifelse(
      volume200 < volume150,
      "Decreasing",
      "Stable"
    )
  )
]

cat("\nGrowth behaviour between ages 150 and 200:\n")
print(table(curveBehaviour$behaviour))

cat("\nPeak age distribution:\n")
print(summary(curveBehaviour$peakAge))

cat("\nCurves peaking at age 200:\n")
print(sum(curveBehaviour$peakAge == 200))
# ---------------------------------------------------------
# 20. Check volume decline after peak
# ---------------------------------------------------------

curveBehaviour[
  ,
  declinePercent := fifelse(
    maxVolume > 0,
    100 * (maxVolume - volume200) / maxVolume,
    NA_real_
  )
]

cat("\nDECLINE FROM PEAK TO AGE 200 (%)\n")

print(
  summary(curveBehaviour$declinePercent)
)

cat("\nCurves with decline > 20%:\n")

print(
  curveBehaviour[
    declinePercent > 20,
    .N
  ]
)

cat("\nCurves with decline > 40%:\n")

print(
  curveBehaviour[
    declinePercent > 40,
    .N
  ]
)

cat("\nTop 10 largest declines:\n")

print(
  curveBehaviour[
    order(-declinePercent),
    .(
      curveID,
      ecodistrict_id,
      species,
      model_group,
      peakAge,
      maxVolume,
      volume200,
      declinePercent
    )
  ][1:10]
)
# ---------------------------------------------------------
# 21. Inspect three extreme PICE.ABI curves
# ---------------------------------------------------------

library(ggplot2)

suspiciousCurves <- yieldCurves[
  curveID %in% c(2495, 2496, 2497)
]

ggplot(
  suspiciousCurves,
  aes(
    x = age,
    y = volume_m3_ha,
    color = factor(ecodistrict_id)
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "National RSYC - PICE.ABI",
    subtitle = "Three curves with 93% decline",
    x = "Stand age (years)",
    y = "Volume (m³/ha)",
    color = "Ecodistrict"
  ) +
  theme_minimal()
# ---------------------------------------------------------
# 22. Inspect ecodistricts of extreme PICE.ABI curves
# ---------------------------------------------------------

library(sf)

suspiciousEco <- eco[
  eco$ecodistrict_id %in% c(517, 531, 519),
]

print(
  st_drop_geometry(suspiciousEco)[
    ,
    c(
      "ecodistrict_id",
      "ecozone",
      "ecoprovince",
      "ecoregion",
      "ecodistrict"
    )
  ]
)
# ---------------------------------------------------------
# 23. Check PICE.ABI in provincial species mappings
# ---------------------------------------------------------

cat("\nONTARIO:\n")
grep(
  "Pice_abi",
  onGroups,
  value = TRUE,
  ignore.case = TRUE
)

cat("\nNEWFOUNDLAND:\n")
grep(
  "Pice_abi",
  nlGroups,
  value = TRUE,
  ignore.case = TRUE
)
# ---------------------------------------------------------
# 24. Identify curves with very high volumes
# ---------------------------------------------------------

highVolumeCurves <- curveBehaviour[
  maxVolume > 500
]

cat("\nNumber of curves with max volume > 500:\n")
print(nrow(highVolumeCurves))

cat("\nSpecies with high volumes:\n")
print(
  highVolumeCurves[
    , .N,
    by = .(species, model_group)
  ][order(-N)]
)

cat("\nTop 15 curves by maximum volume:\n")
print(
  curveBehaviour[
    order(-maxVolume),
    .(
      curveID,
      ecodistrict_id,
      species,
      model_group,
      peakAge,
      maxVolume
    )
  ][1:15]
)
# ---------------------------------------------------------
# 25. Locate ecodistricts with highest yield volumes
# ---------------------------------------------------------

topEcoIDs <- unique(
  curveBehaviour[
    order(-maxVolume),
    ecodistrict_id
  ][1:15]
)

topEco <- eco[
  eco$ecodistrict_id %in% topEcoIDs,
]

print(
  st_drop_geometry(topEco)[
    ,
    c(
      "ecodistrict_id",
      "ecozone",
      "ecoprovince",
      "ecoregion",
      "ecodistrict"
    )
  ]
)
# ---------------------------------------------------------
# 26. Inspect available National ecodistrict ecozones
# ---------------------------------------------------------

print(
  sort(
    unique(eco$ecozone)
  )
)
# ---------------------------------------------------------
# 27. Summarize National curves by ecozone
# ---------------------------------------------------------

ecoLookup <- data.table::as.data.table(
  sf::st_drop_geometry(eco)
)[
  , .(ecodistrict_id, ecozone)
]

curveByEcozone <- merge(
  curveBehaviour,
  ecoLookup,
  by = "ecodistrict_id",
  all.x = TRUE
)

ecozoneSummary <- curveByEcozone[
  ,
  .(
    nEcodistricts = uniqueN(ecodistrict_id),
    nCurves = uniqueN(curveID),
    meanMaxVolume = round(mean(maxVolume), 2),
    medianMaxVolume = round(median(maxVolume), 2),
    highestVolume = round(max(maxVolume), 2),
    nAbove500 = sum(maxVolume > 500),
    nDeclineAbove20 = sum(declinePercent > 20)
  ),
  by = ecozone
][order(-highestVolume)]

print(ecozoneSummary, nrows = Inf)
# ---------------------------------------------------------
# 28. Identify species with strong volume decline
# ---------------------------------------------------------

decliningCurves <- curveByEcozone[
  declinePercent > 20
]

declineBySpecies <- decliningCurves[
  ,
  .(
    nCurves = .N,
    nEcodistricts = uniqueN(ecodistrict_id),
    maxDecline = round(max(declinePercent), 2),
    meanDecline = round(mean(declinePercent), 2)
  ),
  by = .(ecozone, species, model_group)
][order(-nCurves)]

print(declineBySpecies, nrows = Inf)
# ---------------------------------------------------------
# 29. Check whether species curves share the same shape
# ---------------------------------------------------------

shapeCheck <- curveBehaviour[
  species %in% c("PICE.RUB", "PICE.ABI"),
  .(
    curveID,
    ecodistrict_id,
    species,
    peakAge,
    ratio100 = round(volume100 / maxVolume, 6),
    ratio150 = round(volume150 / maxVolume, 6),
    ratio200 = round(volume200 / maxVolume, 6)
  )
]

print(
  shapeCheck[
    ,
    .(
      nCurves = .N,
      nPeakAges = uniqueN(peakAge),
      nRatio100 = uniqueN(ratio100),
      nRatio150 = uniqueN(ratio150),
      nRatio200 = uniqueN(ratio200)
    ),
    by = species
  ]
)
# ---------------------------------------------------------
# 30. Compare normalized curves across all ages
# ---------------------------------------------------------

shapeData <- merge(
  yieldCurves[
    species %in% c("PICE.RUB", "PICE.ABI")
  ],
  curveBehaviour[
    , .(curveID, maxVolume)
  ],
  by = "curveID"
)

shapeData[
  , normalizedVolume := volume_m3_ha / maxVolume
]

shapeVariation <- shapeData[
  ,
  .(
    minRatio = min(normalizedVolume),
    maxRatio = max(normalizedVolume)
  ),
  by = .(species, age)
]

shapeVariation[
  , difference := maxRatio - minRatio
]

print(
  shapeVariation[
    ,
    .(
      maxDifference = max(difference),
      meanDifference = mean(difference)
    ),
    by = species
  ]
)
# ---------------------------------------------------------
# 31. Inspect original RSYC model structure
# ---------------------------------------------------------

models <- data.table::fread(
  "E:/QC/RSYC_models.csv"
)

cat("\nModel dimensions:\n")
print(dim(models))

cat("\nColumn names:\n")
print(names(models))
# ---------------------------------------------------------
# 32. Inspect original model parameters
# ---------------------------------------------------------

selectedIDs <- curveLookup[
  species %in% c("PICE.ABI", "PICE.RUB"),
  model_id
]

originalModels <- models[
  model_id %in% selectedIDs &
    model_version == "v20260923" &
    scale == "national" &
    strata_type == "ecodistrict" &
    strata_level == "ecodistrict"
]

print(
  originalModels[
    species == "PICE.ABI",
    .(
      model_id,
      ecodistrict_id,
      species,
      equation,
      response,
      response_units,
      age_min,
      age_max,
      b1, b2, b3, b4
    )
  ]
)

cat("\nPICE.RUB examples:\n")

print(
  originalModels[
    species == "PICE.RUB",
    .(
      model_id,
      ecodistrict_id,
      species,
      equation,
      age_min,
      age_max,
      b1, b2, b3, b4
    )
  ][1:5]
)
# ---------------------------------------------------------
# 33. Validate extracted curve against original equation
# ---------------------------------------------------------

m <- originalModels[
  species == "PICE.RUB"
][1]

testAges <- c(1, 50, 100, 150, 200)

predicted <- with(
  m,
  b1 * exp(-b4 * testAges) *
    (1 - exp(-b2 * testAges))^b3
)

extracted <- yieldCurves[
  model_id == m$model_id &
    age %in% testAges,
  .(age, volume_m3_ha)
][order(age)]

comparison <- data.table::data.table(
  age = testAges,
  predicted = predicted,
  extracted = extracted$volume_m3_ha
)

comparison[
  , difference := predicted - extracted
]

print(comparison)
# ---------------------------------------------------------
# 34. Validate ALL National curves against original models
# ---------------------------------------------------------

modelParams <- originalModels[
  ,
  .(model_id, equation, b1, b2, b3, b4)
]

# For full validation, use all national models
modelParams <- models[
  model_version == "v20260923" &
    scale == "national" &
    strata_type == "ecodistrict" &
    strata_level == "ecodistrict" &
    publish_ok == TRUE,
  .(model_id, equation, b1, b2, b3, b4)
]

validation <- merge(
  yieldCurves,
  modelParams,
  by = "model_id",
  all.x = TRUE
)

# Check whether all curves use the expected equation
print(
  validation[
    , .N,
    by = equation
  ]
)

# Check missing model parameters
print(
  validation[
    ,
    .(
      missingModels = sum(is.na(equation)),
      missingParameters = sum(
        !complete.cases(.SD)
      )
    ),
    .SDcols = c("b1", "b2", "b3", "b4")
  ]
)
# ---------------------------------------------------------
# 35. Validate all extracted National yield curves
# ---------------------------------------------------------

expectedEquation <- paste0(
  "b1 * exp(-b4 * age) * ",
  "(1 - exp(-b2 * age))^b3"
)

# Check equation types
print(
  validation[, .N, by = equation]
)

# Stop if an unexpected equation exists
stopifnot(
  all(validation$equation == expectedEquation)
)

# Calculate predicted volumes
validation[
  , predicted := b1 * exp(-b4 * age) *
    (1 - exp(-b2 * age))^b3
]

# Calculate differences
validation[
  , absDifference := abs(predicted - volume_m3_ha)
]

# Summarize validation
validationSummary <- validation[
  ,
  .(
    nRows = .N,
    nCurves = uniqueN(curveID),
    maxDifference = max(absDifference),
    meanDifference = mean(absDifference),
    nAboveTolerance = sum(absDifference > 1e-6)
  )
]

print(validationSummary)
# ---------------------------------------------------------
# 35. Validate all extracted National yield curves
# ---------------------------------------------------------

expectedEquation <- paste0(
  "b1 * exp(-b4 * age) * ",
  "(1 - exp(-b2 * age))^b3"
)

# Check equation types
print(
  validation[, .N, by = equation]
)

# Stop if an unexpected equation exists
stopifnot(
  all(validation$equation == expectedEquation)
)

# Calculate predicted volumes
validation[
  , predicted := b1 * exp(-b4 * age) *
    (1 - exp(-b2 * age))^b3
]

# Calculate differences
validation[
  , absDifference := abs(predicted - volume_m3_ha)
]

# Summarize validation
validationSummary <- validation[
  ,
  .(
    nRows = .N,
    nCurves = uniqueN(curveID),
    maxDifference = max(absDifference),
    meanDifference = mean(absDifference),
    nAboveTolerance = sum(absDifference > 1e-6)
  )
]

print(validationSummary)
# ---------------------------------------------------------
# 36. Check original model age ranges
# ---------------------------------------------------------

ageRanges <- models[
  model_version == "v20260923" &
    scale == "national" &
    strata_type == "ecodistrict" &
    strata_level == "ecodistrict" &
    publish_ok == TRUE,
  .N,
  by = .(age_min, age_max)
][order(age_min, age_max)]

print(ageRanges, nrows = Inf)
# ---------------------------------------------------------
# 37. Test National RSYC curves at age 255
# ---------------------------------------------------------

nationalParams <- unique(
  validation[
    ,
    .(curveID, model_id, species, b1, b2, b3, b4)
  ],
  by = "curveID"
)

nationalParams[
  , volume255 := b1 * exp(-b4 * 255) *
    (1 - exp(-b2 * 255))^b3
]

print(
  nationalParams[
    ,
    .(
      nCurves = .N,
      minVolume255 = min(volume255),
      maxVolume255 = max(volume255),
      nNegative = sum(volume255 < 0),
      nNonFinite = sum(!is.finite(volume255))
    )
  ]
)
# ---------------------------------------------------------
# 38. Extend National RSYC yield curves to age 255
# ---------------------------------------------------------

library(data.table)

# Use the 4637 validated national models
nationalParams <- unique(
  validation[
    ,
    .(
      curveID,
      model_id,
      ecodistrict_id,
      species,
      model_group,
      model_version,
      b1, b2, b3, b4
    )
  ],
  by = "curveID"
)

# Generate ages 0–255 for every curve
national255 <- nationalParams[
  ,
  .(age = 0:255),
  by = .(
    curveID,
    model_id,
    ecodistrict_id,
    species,
    model_group,
    model_version,
    b1, b2, b3, b4
  )
]

# Calculate volume using original RSYC equation
national255[
  , volume_m3_ha :=
    b1 * exp(-b4 * age) *
    (1 - exp(-b2 * age))^b3
]

# Remove model parameters from final output
national255[
  , c("b1", "b2", "b3", "b4") := NULL
]

setcolorder(
  national255,
  names(yieldCurves)
)

# Check dimensions and validity
print(dim(national255))
print(uniqueN(national255$curveID))
print(range(national255$age))
print(national255[
  ,
  .(
    nNegative = sum(volume_m3_ha < 0),
    nNonFinite = sum(!is.finite(volume_m3_ha))
  )
])
# ---------------------------------------------------------
# 39. Compare National age 0-200 vs age 0-255
# ---------------------------------------------------------

comparison255 <- merge(
  yieldCurves[
    , .(curveID, age, oldVolume = volume_m3_ha)
  ],
  national255[
    age <= 200,
    .(curveID, age, newVolume = volume_m3_ha)
  ],
  by = c("curveID", "age"),
  all = TRUE
)

comparison255[
  , difference := abs(oldVolume - newVolume)
]

print(
  comparison255[
    ,
    .(
      nRows = .N,
      missingOld = sum(is.na(oldVolume)),
      missingNew = sum(is.na(newVolume)),
      maxDifference = max(difference, na.rm = TRUE),
      nAboveTolerance = sum(difference > 1e-6, na.rm = TRUE)
    )
  ]
)
# ---------------------------------------------------------
# 40. Save validated National RSYC curves (age 0-255)
# ---------------------------------------------------------

nationalPath <- "E:/EasternCanadaClassifier/data/National"

outputFile <- file.path(
  nationalPath,
  "RSYC_national_volume_yield_curves_v20260923_age255.csv"
)

data.table::fwrite(
  national255,
  outputFile
)

cat("\nSaved National RSYC curves to:\n")
cat(outputFile, "\n")

# Verify saved file
stopifnot(file.exists(outputFile))

cat("\nFile size (MB):\n")
print(
  round(file.info(outputFile)$size / 1024^2, 2)
)
# ---------------------------------------------------------
# 41. Inspect Newfoundland pixel filtering
# ---------------------------------------------------------

nlFile <- paste0(
  "E:/EasternCanadaClassifier/modules/",
  "EasternCanadaClassifier/R/classifyProvince_NL.R"
)

nlCode <- readLines(nlFile)

cat(
  paste(
    sprintf(
      "%4d: %s",
      720:790,
      nlCode[720:790]
    ),
    collapse = "\n"
  )
)
# ---------------------------------------------------------
# 42. Inspect NL classification before bestAU selection
# ---------------------------------------------------------

cat(
  paste(
    sprintf(
      "%4d: %s",
      590:719,
      nlCode[590:719]
    ),
    collapse = "\n"
  )
)

# ---------------------------------------------------------
# 43. Inspect input preparation for NL classification
# ---------------------------------------------------------

cat(
  paste(
    sprintf(
      "%4d: %s",
      510:590,
      nlCode[510:590]
    ),
    collapse = "\n"
  )
)
# ---------------------------------------------------------
# 44. Find how pixel_region is constructed
# ---------------------------------------------------------

grep(
  "pixel_region",
  nlCode,
  value = TRUE
)

grep(
  "pixel_region",
  nlCode
)
# ---------------------------------------------------------
# 45. Inspect pixel_region construction in NL
# ---------------------------------------------------------

cat(
  paste(
    sprintf(
      "%4d: %s",
      330:405,
      nlCode[330:405]
    ),
    collapse = "\n"
  )
)
# ---------------------------------------------------------
# 46. Search for existing ecodistrict handling
# ---------------------------------------------------------

classifierFiles <- list.files(
  "E:/EasternCanadaClassifier",
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = TRUE
)

ecoReferences <- lapply(classifierFiles, function(f) {
  lines <- readLines(f, warn = FALSE)
  
  hits <- grep(
    "ecodistrict|ecoDistrict|RSYC",
    lines,
    ignore.case = TRUE
  )
  
  if (length(hits) == 0) return(NULL)
  
  data.table::data.table(
    file = f,
    line = hits,
    code = lines[hits]
  )
})

ecoReferences <- data.table::rbindlist(
  ecoReferences,
  fill = TRUE
)

print(ecoReferences, nrows = Inf)
# ---------------------------------------------------------
# 47. Inspect existing National RSYC reader
# ---------------------------------------------------------

nationalReaderFile <- paste0(
  "E:/EasternCanadaClassifier/R/",
  "readRSYC_National.R"
)

nationalReaderCode <- readLines(
  nationalReaderFile,
  warn = FALSE
)

cat("\nTotal lines:\n")
print(length(nationalReaderCode))

cat("\nFunction definitions:\n")
print(
  grep(
    "<- function",
    nationalReaderCode,
    value = TRUE
  )
)

cat("\nFirst 100 lines:\n")
cat(
  paste(
    sprintf(
      "%4d: %s",
      seq_len(min(100, length(nationalReaderCode))),
      head(nationalReaderCode, 100)
    ),
    collapse = "\n"
  )
)

# ---------------------------------------------------------
# 48. Search for spatial integration logic
# ---------------------------------------------------------

hits <- grep(
  paste(
    "rasterize",
    "pixelGroup",
    "PlanningGrid",
    "st_join",
    "st_intersects",
    "terra::extract",
    "ecodistrictMap",
    sep = "|"
  ),
  nationalReaderCode,
  ignore.case = TRUE
)

cat(
  paste(
    sprintf(
      "%4d: %s",
      hits,
      nationalReaderCode[hits]
    ),
    collapse = "\n"
  )
)
# ---------------------------------------------------------
# 49. Inspect National ecodistrict spatial identifiers
# ---------------------------------------------------------

library(sf)

ecoInfo <- sf::st_read(
  "E:/QC/RSYC_ecostrat.gpkg",
  layer = "ecodistricts",
  quiet = TRUE
)

cat("Number of polygons:", nrow(ecoInfo), "\n")
cat("CRS:", sf::st_crs(ecoInfo)$epsg, "\n")

print(names(ecoInfo))

print(
  head(
    sf::st_drop_geometry(ecoInfo),
    5
  )
)