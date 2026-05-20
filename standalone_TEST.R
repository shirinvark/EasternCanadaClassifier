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
# =======================================================

sim <- spades(sim)

# =========================================================
# CHECK OUTPUT
# =========================================================

print(head(sim$cohortData))      # fake cohort
print(head(sim$pixelGroupToAU))  # mapping
print(head(sim$areaByAU))        # area summary

























# =========================================================
# LOAD STANDARDIZE FUNCTIONS
# =========================================================

source(
  "E:/EasternCanadaClassifier/R/standardizeYieldCurve.R"
)

source(
  "E:/EasternCanadaClassifier/R/standardizeYieldTables.R"
)

# ========================================================
# CHECK
# =========================================================

exists("standardizeYieldCurve")

exists("standardizeYieldTables")

# =========================================================
# RUN STANDARDIZATION
# =========================================================

yt <- standardizeYieldTables(sim)

# =========================================================
# CHECK OUTPUT
# =========================================================

names(yt)

names(yt$ON)

names(yt$ON$`3e`)

head(yt$ON$`3e`[[1]])

