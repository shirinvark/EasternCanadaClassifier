rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)
library(data.table)

## =========================================================
## 1) SET PATHS
## =========================================================

setPaths(
  cachePath   = "E:/EasternCanadaClassifier/cache",
  inputPath   = "E:/EasternCanadaClassifier/inputs",
  outputPath  = "E:/EasternCanadaClassifier/outputs",
  modulePath  = "E:/EasternCanadaClassifier/modules",
  scratchPath = "E:/EasternCanadaClassifier/scratch"
)

print(getPaths())

## =========================================================
## 2) DOWNLOAD MODULE
## =========================================================

SpaDES.project::getModule(
  modules    = "shirinvark/EasternCanadaClassifier",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## =========================================================
## 3) CREATE TEST DATA FOR NEW CLASSIFIER
## =========================================================

# pixelGroup raster

pixelGroupMap <- rast(nrows=10, ncols=10, xmin=0, xmax=1000, ymin=0, ymax=1000)
values(pixelGroupMap) <- sample(1:20, 100, replace=TRUE)

# cohortData (LandR style)

cohortData <- data.table(
  pixelGroup = sample(1:20, 200, replace = TRUE),
  speciesCode = sample(
    c("Abie_bal","Pice_mar","Pinu_ban","Pinu_res","Pinu_str","Acer_sah"),
    200,
    replace = TRUE
  ),
  age = sample(1:120, 200, replace = TRUE),
  B = runif(200, 1, 50)
)

## =========================================================
## 4) INIT SIMULATION
## =========================================================

sim <- simInit(
  times = list(start = 0, end = 1),
  modules = "EasternCanadaClassifier",
  objects = list(
    cohortData = cohortData,
    pixelGroupMap = pixelGroupMap
  ),
  options = list(
    spades.checkpoint = FALSE,
    spades.save       = FALSE,
    spades.progress   = FALSE
  )
)

## =========================================================
## RUN MODEL
## =========================================================

system.time({
  sim <- spades(sim)
})


## =========================================================
## CHECK ANALYSIS UNIT MAP
## =========================================================

plot(sim$analysisUnitMap,
     main = "Analysis Unit Map")


## =========================================================
## AREA PER ANALYSIS UNIT
## =========================================================

library(terra)

cellArea <- prod(res(sim$analysisUnitMap)) / 10000

areaTable <- as.data.frame(freq(sim$analysisUnitMap))

areaTable$area_ha <- areaTable$count * cellArea

print(areaTable)


## =========================================================
## AGE STRUCTURE PER ANALYSIS UNIT
## =========================================================

library(data.table)

dt <- as.data.table(sim$cohortData)


## create age classes

ageBreaks <- c(0,20,40,60,80,100,150,Inf)

dt[, ageClass := cut(
  age,
  breaks = ageBreaks,
  right = FALSE,
  labels = FALSE
)]


## join analysis unit from raster

auValues <- terra::values(sim$analysisUnitMap)

pixelGroups <- unique(dt$pixelGroup)

lookupAU <- data.table(
  pixelGroup = pixelGroups,
  analysisUnit = auValues[pixelGroups]
)

dt <- merge(
  dt,
  lookupAU,
  by = "pixelGroup",
  all.x = TRUE
)


## age distribution table

ageStructure <- dt[, .N, by = .(analysisUnit, ageClass)]

print(ageStructure)


## =========================================================
## CLEAN ANALYSIS UNIT MAP
## =========================================================

library(terra)

auMap <- sim$analysisUnitMap

auValues <- sort(unique(values(auMap), na.rm = TRUE))


plot(
  sim$analysisUnitMap,
  col = terrain.colors(8),
  main = "Analysis Unit Map"
)

## =========================================================
## MEAN AGE PER ANALYSIS UNIT
## =========================================================

library(data.table)
library(terra)

dt <- as.data.table(sim$cohortData)

## گرفتن AU هر pixelGroup از raster

auValues <- terra::values(sim$analysisUnitMap)

lookupAU <- data.table(
  pixelGroup = unique(dt$pixelGroup),
  analysisUnit = auValues[unique(dt$pixelGroup)]
)

dt <- merge(dt, lookupAU, by = "pixelGroup", all.x = TRUE)

## محاسبه میانگین سن

ageSummary <- dt[, .(
  meanAge = mean(age, na.rm = TRUE),
  nStands = .N
), by = analysisUnit]

print(ageSummary)

## =========================================================
## PLOT YIELD CURVES
## =========================================================

yieldTables <- sim$yieldTables
yieldAges   <- sim$yieldAges

matplot(
  yieldAges,
  t(yieldTables),
  type = "l",
  lwd = 2,
  lty = 1,
  col = rainbow(nrow(yieldTables)),
  xlab = "Age",
  ylab = "Volume",
  main = "Yield Curves"
)

legend(
  "topleft",
  legend = paste("Curve", 1:nrow(yieldTables)),
  col = rainbow(nrow(yieldTables)),
  lty = 1,
  lwd = 2
)
