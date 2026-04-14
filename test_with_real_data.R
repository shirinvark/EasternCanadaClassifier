rm(list = ls())
gc()

# ========================================================
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
# DOWNLOAD MODULE
# =========================================================

SpaDES.project::getModule(
  modules    = "shirinvark/EasternCanadaClassifier",
  modulePath = "E:/EasternCanadaClassifier/modules",
  overwrite  = TRUE
)

# =========================================================
# LOAD INPUT DATA
# =========================================================

pixelGroupMap <- terra::rast(
  "E:/EasternCanadaClassifier/NLmap/pixelGroupMap.tif"
)

standAgeMap <- terra::rast(
  "E:/EasternCanadaClassifier/NLmap/standAgeMap.tif"
)

cohortData <- data.table::fread(
  "E:/EasternCanadaClassifier/NLmap/cohortData.csv"
)

# =========================================================
# BUILD SIMPLE HARVESTABLE FRACTION
# =========================================================

harvestableFraction <- pixelGroupMap
terra::values(harvestableFraction) <- 1

# =========================================================
# INITIALIZE SIMULATION
# =========================================================

sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "EasternCanadaClassifier",
  objects = list(
    cohortData = cohortData,
    pixelGroupMap = pixelGroupMap,
    standAgeMap = standAgeMap,
    harvestableFraction = harvestableFraction
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
# RUN MODEL
# =========================================================

system.time({
  sim <- spades(sim)
})

# =========================================================
# =========================================================

# TEST SCRIPT FOR EasternCanadaClassifier

# =========================================================

# =========================================================

# CHECK MAIN OUTPUTS

# =========================================================

cat("\n===== AREA BY AU =====\n")
print(sim$areaByAU)

cat("\n===== HEAD OF analysisUnitDT =====\n")
print(head(sim$analysisUnitDT))

# =========================================================

# CLASS DISTRIBUTION

# =========================================================

cat("\n===== CLASS DISTRIBUTION =====\n")
print(table(sim$analysisUnitDT$bestCurve))

# =========================================================

# PLOT ANALYSIS UNIT MAP

# =========================================================

terra::plot(
  sim$analysisUnitMap,
  col  = terrain.colors(8),
  main = "Analysis Unit Map"
)

# =========================================================

# SPECIES COMPOSITION CHECK

# =========================================================

boxplot(
  prop_deciduous ~ bestCurve,
  data = sim$analysisUnitDT,
  main = "Deciduous proportion by class",
  xlab = "Analysis Unit",
  ylab = "Deciduous proportion",
  col  = "lightgreen"
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

cat("\n===== TEST COMPLETED SUCCESSFULLY =====\n")

#These numbers are class IDs from the raster output (analysisUnitMap).
#Each number corresponds to a specific yield curve (bestCurve).
#The raster stores numeric IDs, while the actual curve names are in the attribute table (analysisUnitDT).
#I can also plot it with labeled curve names instead of numeric IDs if needed.




#
#The points represent the spatial units (pixel groups), and the colours show the assigned analysis units based on the best-matching growth curves.

#Each colour corresponds to a different growth pattern, grouping areas with similar forest structure and dynamics.



