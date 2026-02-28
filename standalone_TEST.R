rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

## =========================================================
## 1) SET PATHS (Classifier Project)
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
## 2) FORCE DOWNLOAD MODULE FROM GITHUB
## =========================================================

SpaDES.project::getModule(
  modules    = "shirinvark/EasternCanadaClassifier",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## =========================================================
## 3) CREATE DUMMY LANDBASE (FOR TESTING CLASSIFIER ONLY)
## =========================================================

planningRaster <- rast(nrows=10, ncols=10, xmin=0, xmax=1000, ymin=0, ymax=1000)
values(planningRaster) <- 1

landcover <- planningRaster
values(landcover) <- sample(c(210,220,230), 100, replace=TRUE)

standAge <- planningRaster
values(standAge) <- sample(1:120, 100, replace=TRUE)

harvestable <- planningRaster
values(harvestable) <- runif(100)

Landbase <- list(
  baseData = list(
    planningRaster = planningRaster,
    landcover = landcover,
    standAge = standAge
  ),
  fractional = list(
    harvestableFraction = harvestable
  )
)

## =========================================================
## 4) INIT SIMULATION
## =========================================================

sim <- simInit(
  times = list(start = 0, end = 1),
  modules = "EasternCanadaClassifier",
  objects = list(
    Landbase = Landbase
  ),
  options = list(
    spades.checkpoint = FALSE,
    spades.save       = FALSE,
    spades.progress   = FALSE
  )
)

## =========================================================
## 5) RUN SIMULATION
## =========================================================

system.time(
  sim <- spades(sim)
)

## =========================================================
## 6) CHECK OUTPUTS
## =========================================================

cat("\nObjects in sim:\n")
print(names(sim))

if ("analysisUnitMap" %in% names(sim)) {
  plot(sim$analysisUnitMap, main = "Analysis Unit Map")
} else {
  stop("❌ analysisUnitMap was not created")
}

if ("AU_summaries" %in% names(sim)) {
  print(sim$AU_summaries)
} else {
  stop("❌ AU_summaries not created")
}

cat("\n✅ EasternCanadaClassifier test completed successfully.\n")