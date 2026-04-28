rm(list = ls())
gc()

# =========================================================
# LIBRARIES
# =========================================================

library(SpaDES.core)
library(SpaDES.project)

# =========================================================
# PATHS
# =========================================================

setPaths(
  cachePath   = "E:/EasternCanadaClassifier/cache",
  inputPath   = "E:/EasternCanadaClassifier/inputs",
  outputPath  = "E:/EasternCanadaClassifier/outputs",
  modulePath  = "E:/EasternCanadaClassifier/modules",
  scratchPath = "E:/EasternCanadaClassifier/scratch"
)

# =========================================================
# DOWNLOAD MODULE
# =========================================================

SpaDES.project::getModule(
  modules    = "shirinvark/EasternCanadaClassifier",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

# =========================================================
# SIMULATION (🔥 بدون هیچ داده)
# =========================================================

sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "EasternCanadaClassifier",
  
  params = list(
    EasternCanadaClassifier = list(
      jurisdiction = "ON"
    )
  ),
  
  options = list(
    spades.checkpoint = FALSE,
    spades.save       = FALSE,
    spades.progress   = TRUE
  )
)

# =========================================================
# RUN
# =========================================================

sim <- spades(sim)

# =========================================================
# CHECK OUTPUT
# =========================================================

print(head(sim$cohortData))      # fake cohort
print(head(sim$pixelGroupToAU))  # mapping
print(head(sim$areaByAU))        # area summary

