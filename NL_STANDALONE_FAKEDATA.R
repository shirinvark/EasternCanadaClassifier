rm(list = ls())
gc()

# =========================================================
# LOAD LIBRARIES
# =========================================================

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)
library(data.table)

# =========================================================
# SET PATHS
# =========================================================

setPaths(
  cachePath   = "E:/EasternCanadaClassifier/cache",
  inputPath   = "E:/EasternCanadaClassifier/inputs",
  outputPath  = "E:/EasternCanadaClassifier/outputs",
  modulePath  = "E:/EasternCanadaClassifier/modules",
  scratchPath = "E:/EasternCanadaClassifier/scratch"
)

print(getPaths())

# =========================================================
# DOWNLOAD / UPDATE MODULE
# =========================================================

SpaDES.project::getModule(
  modules    = "shirinvark/EasternCanadaClassifier",
  modulePath = "E:/EasternCanadaClassifier/modules",
  overwrite  = TRUE
)

# =========================================================
# LOAD NL YCF AND CREATE TEST STUDY AREA
# =========================================================

ycf <- sf::st_read(
  "E:/OntarioAAC_Test/data/NL_YCF.shp",
  quiet = TRUE
) |>
  sf::st_make_valid() |>
  sf::st_transform(3978)

BarEast <- ycf |>
  dplyr::filter(YCF == "NL_BarEast")

sf::st_write(
  BarEast,
  "E:/OntarioAAC_Test/data/NL_BarEast.shp",
  delete_layer = TRUE,
  quiet = TRUE
)

studyAreaFull <- BarEast |>
  sf::st_union() |>
  sf::st_sf(
    id = 1,
    geometry = _
  )

plot(studyAreaFull)

# =========================================================
# PLOT TEST AREA
# =========================================================

plot(
  st_geometry(ycf),
  col = "grey90",
  border = "grey60",
  main = "NL_BarEast (Inside) Newfoundland"
)

plot(
  st_geometry(studyAreaFull),
  col = adjustcolor("red", alpha.f = 0.5),
  border = "red",
  lwd = 2,
  add = TRUE
)

# =========================================================
# LOAD CLASSIFIER INPUTS
# =========================================================

pixelGroupMap <- terra::rast(
  "E:/EasternCanadaClassifier/NLmap/pixelGroupMap.tif"
)

cohortData <- data.table::fread(
  "E:/EasternCanadaClassifier/NLmap/cohortData.csv"
)

# =========================================================
# BUILD SIMPLE HARVESTABLE FRACTION
# =========================================================

harvestableFraction <- terra::rast(pixelGroupMap)

terra::values(harvestableFraction) <- 1

names(harvestableFraction) <- "harvestableFraction"

# =========================================================
# TEMPORARY YIELD CURVE FAMILY OBJECTS
#
# These objects simulate the outputs normally supplied by
# the upstream module.
#
# They are used only for standalone classifier testing.
# =========================================================

yieldCurveFamily <- terra::rast(pixelGroupMap)

terra::values(yieldCurveFamily) <- 1

names(yieldCurveFamily) <- "yieldCurveFamily"

yieldCurveLookup <- data.frame(
  ID       = 1,
  Province = "NL",
  YCF      = "NL_BarEast"
)

# =========================================================
# CHECK TEMPORARY YCF OBJECTS
# =========================================================

cat("\n===== YCF GEOMETRY CHECK =====\n")

print(
  terra::compareGeom(
    yieldCurveFamily,
    pixelGroupMap,
    stopOnError = FALSE
  )
)

cat("\n===== YIELD CURVE LOOKUP =====\n")

print(yieldCurveLookup)

cat("\n===== YIELD CURVE FAMILY FREQUENCY =====\n")

print(
  terra::freq(yieldCurveFamily)
)

# =========================================================
# INITIALIZE SIMULATION
# =========================================================

sim <- simInit(
  times = list(
    start = 0,
    end = 1
  ),
  
  modules = "EasternCanadaClassifier",
  
  objects = list(
    cohortData = cohortData,
    pixelGroupMap = pixelGroupMap,
    harvestableFraction = harvestableFraction,
    yieldCurveFamily = yieldCurveFamily,
    yieldCurveLookup = yieldCurveLookup
  ),
  
  params = list(
    EasternCanadaClassifier = list(
      jurisdiction = "NL"
    )
  ),
  
  options = list(
    spades.checkpoint = FALSE,
    spades.save       = FALSE,
    spades.progress   = FALSE
  )
)

# =========================================================
# CHECK OBJECTS AFTER simInit
# =========================================================

cat("\n===== OBJECT CHECK AFTER simInit =====\n")

cat(
  "yieldCurveFamily exists:",
  "yieldCurveFamily" %in% names(sim),
  "\n"
)

cat(
  "yieldCurveLookup exists:",
  "yieldCurveLookup" %in% names(sim),
  "\n"
)

cat("\n===== sim$yieldCurveLookup =====\n")

print(sim$yieldCurveLookup)

cat("\n===== sim$yieldCurveFamily FREQUENCY =====\n")

print(
  terra::freq(sim$yieldCurveFamily)
)

# =========================================================
# RUN MODEL
# =========================================================

system.time({
  sim <- spades(sim)
})

# =========================================================
# CHECK MAIN OUTPUTS
# =========================================================

cat("\n===== AREA BY AU =====\n")

print(sim$areaByAU)

cat("\n===== HEAD OF analysisUnitDT =====\n")

print(
  head(sim$analysisUnitDT)
)

# =========================================================
# CLASS DISTRIBUTION
# =========================================================

cat("\n===== CLASS DISTRIBUTION =====\n")

print(
  table(sim$analysisUnitDT$bestCurve)
)

# =========================================================
# REGION / CLASS CHECK
# =========================================================

cat("\n===== REGION / BEST CURVE =====\n")

print(
  sim$analysisUnitDT[
    ,
    .N,
    by = .(region, bestCurve)
  ]
)

# =========================================================
# PLOT ANALYSIS UNIT MAP
# =========================================================

terra::plot(
  sim$analysisUnitMap,
  col = terrain.colors(8),
  main = "Analysis Unit Map"
)

# =========================================================
# SPECIES COMPOSITION CHECK
# =========================================================

sim$analysisUnitDT[
  ,
  prop_deciduous := broadleaf_NL
]

boxplot(
  prop_deciduous ~ bestCurve,
  data = sim$analysisUnitDT,
  main = "Deciduous proportion by class",
  col = "lightgreen"
)

# =========================================================
# MEAN SPECIES COMPOSITION BY CLASS
# =========================================================

cat("\n===== MEAN SPECIES COMPOSITION =====\n")

print(
  sim$analysisUnitDT[
    ,
    .(
      mean_bs = mean(blackSpruce_NL),
      mean_bf = mean(balsamFir_NL),
      mean_bl = mean(broadleaf_NL)
    ),
    by = bestCurve
  ]
)

# =========================================================
# CLASS PROPORTIONS
# =========================================================

cat("\n===== CLASS PROPORTIONS =====\n")

print(
  prop.table(
    table(sim$analysisUnitDT$bestCurve)
  )
)

# =========================================================
# SAVE OUTPUTS
# =========================================================

data.table::fwrite(
  sim$areaByAU,
  "E:/EasternCanadaClassifier/outputs/AU_area.csv"
)

data.table::fwrite(
  sim$analysisUnitDT,
  "E:/EasternCanadaClassifier/outputs/analysisUnitDT.csv"
)

# =========================================================
# CHECK ONE YIELD CURVE
# =========================================================

selectedCurve <- names(sim$yieldTables)[1]

cat("\n===== SELECTED YIELD CURVE =====\n")
print(selectedCurve)

yt <- sim$yieldTables[[selectedCurve]]

totalVolume <- rowSums(
  yt[, -1, with = FALSE],
  na.rm = TRUE
)

plot(
  yt$age,
  totalVolume,
  type = "l",
  lwd = 2,
  main = selectedCurve,
  xlab = "Age",
  ylab = "Total volume"
)

cat("\n===== YIELD TABLE HEAD =====\n")

print(
  head(yt, 20)
)

# =========================================================
# FINISHED
# =========================================================

cat("\n===== TEST COMPLETED SUCCESSFULLY =====\n")