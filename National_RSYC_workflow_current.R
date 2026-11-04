library(data.table)

# ---------------------------------------------------------
# STEP 1 - Read National RSYC yield curves
# ---------------------------------------------------------

nationalFile <- file.path(
  "E:/EasternCanadaClassifier/data/National",
  "RSYC_national_volume_yield_curves_v20260923_age255.csv"
)

nationalDT <- fread(nationalFile)

# Check structure
dim(nationalDT)

names(nationalDT)

head(nationalDT, 3)

uniqueN(nationalDT$curveID)

range(nationalDT$age)
# ---------------------------------------------------------
# STEP 2 - Prepare National yield data
# Similar to Ontario yield_all
# ---------------------------------------------------------

national_yield_all <- copy(
  nationalDT[
    age >= 1,
    .(
      curveID = paste0("NAT_", curveID),
      ecodistrict_id,
      species,
      model_group,
      age,
      volume = volume_m3_ha
    )
  ]
)

setorder(
  national_yield_all,
  ecodistrict_id,
  curveID,
  age
)

# Check
dim(national_yield_all)

head(national_yield_all, 5)

uniqueN(national_yield_all$curveID)

uniqueN(national_yield_all$ecodistrict_id)
# ---------------------------------------------------------
# STEP 3 - Organize National curves by ecodistrict
# ---------------------------------------------------------

yield_by_ecodistrict <- split(
  national_yield_all,
  by = "ecodistrict_id",
  keep.by = TRUE
)

# Check structure
length(yield_by_ecodistrict)

head(names(yield_by_ecodistrict))

dim(yield_by_ecodistrict[[1]])

uniqueN(yield_by_ecodistrict[[1]]$curveID)

unique(yield_by_ecodistrict[[1]]$ecodistrict_id)
# ---------------------------------------------------------
# STEP 4 - Inspect curves within one ecodistrict
# ---------------------------------------------------------

testEco <- yield_by_ecodistrict[["191"]]

# One record per curve
ecoCurves <- unique(
  testEco[
    ,
    .(
      curveID,
      species,
      model_group
    )
  ]
)

print(ecoCurves)

# Count curves by model group
table(ecoCurves$model_group)
# ---------------------------------------------------------
# STEP 6 - Validate annual National yield curves
# ---------------------------------------------------------

curveCheck <- national_yield_all[
  ,
  .(
    nAges = uniqueN(age),
    minAge = min(age),
    maxAge = max(age)
  ),
  by = curveID
]

table(
  curveCheck$nAges,
  curveCheck$minAge,
  curveCheck$maxAge
)
# ---------------------------------------------------------
# STEP 7 - Identify National species for mapping
# ---------------------------------------------------------

nationalSpecies <- sort(unique(
  national_yield_all[
    model_group == "species",
    species
  ]
))

print(nationalSpecies)

length(nationalSpecies)
# ---------------------------------------------------------
# STEP 10 - Read National species-group mappings
# ---------------------------------------------------------

national_dir <- "E:/EasternCanadaClassifier/data/National"

speciesGroups_NAT <- read_curve_mapping(
  file.path(national_dir, "speciesGroups_NATional.txt")
)

mapSpeciesGroups_NAT <- read_curve_mapping(
  file.path(national_dir, "mapSpeciesGroups_NATional.txt")
)

groups_NAT <- unique(unlist(mapSpeciesGroups_NAT))
groups_NAT <- groups_NAT[!is.na(groups_NAT) & groups_NAT != ""]

print(groups_NAT)
# ---------------------------------------------------------
# STEP 11 - Map species and reshape National yield tables
# ---------------------------------------------------------

national_wide_input <- copy(national_yield_all)

# Map species-specific curves
national_wide_input[
  model_group == "species",
  final_group := unname(mapSpeciesGroups_NAT[species])
]

# Keep general models in separate columns
national_wide_input[
  model_group %in% c("generic", "coniferous", "broadleaf"),
  final_group := paste0(model_group, "_NAT")
]

# Convert long to wide
national_yield_wide <- dcast(
  national_wide_input,
  ecodistrict_id + curveID + age ~ final_group,
  value.var = "volume",
  fill = 0
)

head(national_yield_wide)
# Convert mapping list to a named character vector
map_NAT <- unlist(mapSpeciesGroups_NAT)

# Fix final_group
national_wide_input[
  model_group == "species",
  final_group := unname(map_NAT[species])
]
sapply(
  national_wide_input[, .(ecodistrict_id, curveID, age, final_group)],
  class
)

str(national_wide_input$final_group)
national_wide_input[
  , final_group := as.character(unlist(final_group))
]

class(national_wide_input$final_group)
national_yield_wide <- dcast(
  national_wide_input,
  ecodistrict_id + curveID + age ~ final_group,
  value.var = "volume",
  fill = 0
)

dim(national_yield_wide)
head(national_yield_wide)
national_wide_input[
  model_group %in% c("generic", "coniferous", "broadleaf"),
  final_group := paste0(model_group, "_NAT")
]

# Convert to wide format
national_yield_wide <- dcast(
  national_wide_input,
  ecodistrict_id + curveID + age ~ final_group,
  value.var = "volume",
  fill = NA_real_
)

head(national_yield_wide)
#------------------------------------------------------------------------------
# STEP 12 - Organize National Yield Tables by Ecodistrict
#
# Split the National RSYC yield tables into separate datasets for each
# ecodistrict, while preserving curve IDs, age, species-group volumes,
# and missing values (NA).
#------------------------------------------------------------------------------

yield_by_ecodistrict_wide <- split(
  national_yield_wide,
  by = "ecodistrict_id",
  keep.by = TRUE
)

length(yield_by_ecodistrict_wide)