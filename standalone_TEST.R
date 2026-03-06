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
## 5) RUN
## ========================================================

system.time({
  sim <- spades(sim)
})

## =========================================================
## 6) CHECK OUTPUT
## =========================================================

cat("\nObjects in sim:\n")
print(names(sim))

if ("analysisUnitMap" %in% names(sim)) {
  
  plot(sim$analysisUnitMap, main = "Analysis Unit Map")
  
} else {
  
  stop("analysisUnitMap was not created")
}

cat("\nClassifier test completed successfully.\n")

