
#Init <- function(sim) {

#message("Building analysisUnitMap from LandR state")
# Print a message so the user knows the classifier module is starting

## ------------------------------------------------
## -------------------------------------------
## 1. Read yield tables (.vol)
## ------------------------------------------------

url <- "https://raw.githubusercontent.com/shirinvark/EasternCanadaClassifier/main/data/AlPac%20AME%20Mixedwood%20VolTabs.vol"

dest <- "AlPac_AME_Mixedwood_VolTabs.vol"

if (!file.exists(dest)) {
  download.file(url, dest, mode = "wb")
}
lines <- readLines(dest)

header <- strsplit(lines[1], "\\s+")[[1]]

nCurves <- as.numeric(gsub("#", "", header[1]))
nAges   <- as.numeric(header[2])
dataLines <- lines[2:(nCurves * 2 + 1)]

dataMatrix <- do.call(rbind, lapply(dataLines, function(x) {
  x <- trimws(x)   # ✨ این خط مهمه
  as.numeric(strsplit(x, "\\s+")[[1]])
}))



curves <- list()

for (i in 1:nCurves) {
  
  conifer_row   <- dataMatrix[(2*i - 1), ]
  deciduous_row <- dataMatrix[(2*i), ]
  
  curves[[i]] <- list(
    conifer   = conifer_row,
    deciduous = deciduous_row
  )
}


curves_prop <- list()

for (i in 1:nCurves) {
  
  con <- curves[[i]]$conifer
  dec <- curves[[i]]$deciduous
  
  total <- con + dec
  
  total[total == 0] <- 1
  
  prop_con <- con / total
  prop_dec <- dec / total
  
  curves_prop[[i]] <- list(
    conifer   = prop_con,
    deciduous = prop_dec
  )
}

curve_names <- c(
  "Aw",
  "AwS",   # ignore
  "AwSw",
  "SwAw",
  "Sw",
  "Sb",
  "Pj",
  "MxPj"
)

names(curves_prop) <- curve_names


curves_prop$AwS <- NULL
# =========================================================
# COHORT DATA PROCESSING
# =========================================================

library(data.table)

# 1️⃣ تبدیل speciesCode به character
cohortData[, speciesCode := as.character(speciesCode)]

# 2️⃣ ساخت ستون group
cohortData[, group := NA_character_]

# 3️⃣ mapping species → group

# deciduous
cohortData[speciesCode %in% c("Popu_tre", "Betu_pap"),
           group := "borealDeciduous_AB"]

# white spruce (fir)
cohortData[speciesCode %in% c("Abie_bal"),
           group := "whiteSpruce_AB"]

# black spruce
cohortData[speciesCode %in% c("Pice_mar"),
           group := "blackSpruce_AB"]

# pine
cohortData[speciesCode %in% c("Pinu_ban", "Pinu_res", "Pinu_str"),
           group := "borealPine_AB"]

# 4️⃣ حذف species بدون group (مثل Acer)
cohortData <- cohortData[!is.na(group)]

# 5️⃣ aggregation: جمع biomass در هر pixel و group
pixelGroups <- cohortData[
  , .(biomass = sum(B)), 
  by = .(pixelGroup, group)
]

# 6️⃣ تبدیل به wide (هر group یک ستون)
pixelWide <- dcast(
  pixelGroups,
  pixelGroup ~ group,
  value.var = "biomass",
  fill = 0
)

# 7️⃣ ساخت total biomass
pixelWide[, total := borealDeciduous_AB + whiteSpruce_AB + 
            blackSpruce_AB + borealPine_AB]

# 8️⃣ جلوگیری از تقسیم بر صفر
pixelWide[total == 0, total := 1]

# 9️⃣ تبدیل به proportion
pixelWide[, `:=`(
  prop_deciduous = borealDeciduous_AB / total,
  prop_sw        = whiteSpruce_AB / total,
  prop_sb        = blackSpruce_AB / total,
  prop_pine      = borealPine_AB / total
)]

# =========================================================
# PREP AGE INDEX (ONCE)
# =========================================================

ages <- cohortData[, .(age = age[1]), by = pixelGroup]

setkey(ages, pixelGroup)
setkey(pixelWide, pixelGroup)

pixelWide <- ages[pixelWide]
pixelWide[is.na(age), age := 0]
age_index <- floor(pixelWide$age / 10) + 1
age_index <- pmax(1, pmin(age_index, 21))

# =========================================================
# MATCH PIXELS TO CURVES
# =========================================================

results <- list()

for (curve_name in names(curves_prop)) {
  
  curve <- curves_prop[[curve_name]]
  
  if (curve_name %in% c("Aw", "AwSw", "SwAw", "Sw")) {
    pixel_con <- pixelWide$prop_sw
  } else if (curve_name == "Sb") {
    pixel_con <- pixelWide$prop_sb
  } else {
    pixel_con <- pixelWide$prop_pine
  }
  
  pixel_dec <- pixelWide$prop_deciduous
  
  curve_con_vals <- curve$conifer[age_index]
  curve_dec_vals <- curve$deciduous[age_index]
  
  dist <- abs(pixel_con - curve_con_vals) +
    abs(pixel_dec - curve_dec_vals)
  
  results[[curve_name]] <- dist
}

distMatrix <- as.data.frame(results)

pixelWide$bestCurve <- names(distMatrix)[
  max.col(-as.matrix(distMatrix))
]
sim$analysisUnitMap <- pixelWide
#RASTER
library(terra)

# تبدیل bestCurve به عدد (برای raster)
pixelWide[, classID := as.numeric(factor(bestCurve))]

# lookup table
lookup <- pixelWide[, .(pixelGroup, classID)]

# رستر جدید
# 1️⃣ تبدیل raster به vector
vals <- values(pixelGroupMap)

# 2️⃣ match با lookup
idx <- match(vals, lookup$pixelGroup)

# 3️⃣ گرفتن classID
new_vals <- lookup$classID[idx]

# 4️⃣ ساخت raster جدید
analysisUnitRaster <- pixelGroupMap
values(analysisUnitRaster) <- new_vals
plot(analysisUnitRaster)  
