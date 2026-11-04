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
# LOAD CLASSIFIER INPUTS
# =========================================================

pixelGroupMap <- terra::rast(
  "E:/EasternCanadaClassifier/ONmap/pixelGroupMap.tif"
)

plot(
  pixelGroupMap,
  main = "Ontario Pixel Group Map"
)

cohortData <- data.table::fread(
  "E:/EasternCanadaClassifier/ONmap/cohortData.csv"
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
  Province = "ON",
  YCF      = "5E"
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
      jurisdiction = "ON"
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

print(
  sim$yieldCurveLookup
)

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

print(
  sim$areaByAU
)

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

cat("\n===== CLASS PROPORTION =====\n")

print(
  prop.table(
    table(sim$analysisUnitDT$bestCurve)
  )
)

# =========================================================
# DISTANCE CHECK
# =========================================================

cat("\n===== DISTANCE SUMMARY =====\n")

print(
  summary(sim$classification$distance)
)

# =========================================================
# NA CHECK
# =========================================================

cat("\n===== NA RATE =====\n")

print(
  mean(
    is.na(sim$classification$bestAU)
  )
)

# =========================================================
# REGION CHECK
# =========================================================

cat("\n===== REGION x CLASS =====\n")

print(
  sim$analysisUnitDT[
    ,
    .N,
    by = .(region, bestCurve)
  ]
)

# =========================================================
# COMPOSITION CHECK
# Ontario species groups
# =========================================================

cat("\n===== COMPOSITION BY CLASS =====\n")

print(
  sim$analysisUnitDT[
    ,
    .(
      spruce = mean(
        spruce_ON,
        na.rm = TRUE
      ),
      fir = mean(
        balsamFir_ON,
        na.rm = TRUE
      ),
      pine = mean(
        pine_ON,
        na.rm = TRUE
      ),
      broad = mean(
        broadleaf_ON,
        na.rm = TRUE
      )
    ),
    by = bestCurve
  ]
)

# =========================================================
# BOXPLOT
# =========================================================

sim$analysisUnitDT[
  ,
  prop_broad := broadleaf_ON
]

boxplot(
  prop_broad ~ bestCurve,
  data = sim$analysisUnitDT,
  main = "Broadleaf proportion by class (ON)",
  col = "lightgreen"
)

# =========================================================
# PLOT ANALYSIS UNIT MAP
# =========================================================

terra::plot(
  sim$analysisUnitMap,
  col = terrain.colors(8),
  main = "Analysis Unit Map (Ontario)"
)

# =========================================================
# SAVE OUTPUTS
# =========================================================

data.table::fwrite(
  sim$areaByAU,
  "E:/EasternCanadaClassifier/outputs/ON_AU_area.csv"
)

data.table::fwrite(
  sim$analysisUnitDT,
  "E:/EasternCanadaClassifier/outputs/ON_analysisUnitDT.csv"
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