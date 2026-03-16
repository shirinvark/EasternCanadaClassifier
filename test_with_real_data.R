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
  "E:/EasternCanadaClassifier/maps/pixel_groups.tif"
)

standAgeMap <- terra::rast(
  "E:/EasternCanadaClassifier/maps/stand_age_map.tif"
)

cohortData <- readRDS(
  "E:/EasternCanadaClassifier/maps/cohortData.rds"
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
# CHECK MAIN OUTPUTS FROM MODULE
# =========================================================

print(sim$areaByAU)
print(sim$pixelGroupToAU)
print(sim$ageStructureByAU)
print(sim$ageSummaryByAU)
print(sim$speciesSummaryByAU)

# =========================================================
# PLOT ANALYSIS UNIT MAP
# =========================================================

plot(
  sim$analysisUnitMap,
  col  = terrain.colors(8),
  main = "Analysis Unit Map"
)

# =========================================================
# PLOT YIELD CURVES (ALL CURVES)
# =========================================================

yieldTables <- sim$yieldTables
yieldAges   <- sim$yieldAges

x11()

matplot(
  yieldAges,
  t(yieldTables),
  type = "l",
  lwd  = 2,
  lty  = 1,
  col  = rainbow(nrow(yieldTables)),
  xlab = "Age",
  ylab = "Volume",
  main = "Yield Curves"
)

legend(
  "topleft",
  legend = paste("Curve", 1:nrow(yieldTables)),
  col    = rainbow(nrow(yieldTables)),
  lty    = 1,
  lwd    = 2
)

# =========================================================
# SPECIES COMPOSITION PLOT
# =========================================================

x11()

barplot(
  t(as.matrix(
    sim$speciesSummaryByAU[, .(
      deciduous_p,
      white_spruce_p,
      black_spruce_p,
      pine_p
    )]
  )),
  col = c("darkgreen", "lightblue", "blue", "orange"),
  legend = c("Deciduous", "WhiteSpruce", "BlackSpruce", "Pine"),
  xlab = "Analysis Unit",
  ylab = "Proportion"
)

# =========================================================
# CONIFER YIELD CURVES
# =========================================================

x11()

matplot(
  yieldAges,
  t(sim$yieldConifer),
  type = "l",
  lwd  = 2,
  col  = rainbow(nrow(sim$yieldConifer)),
  xlab = "Age",
  ylab = "Volume",
  main = "Conifer Yield Curves"
)

legend(
  "topleft",
  legend = paste("AU", 1:nrow(sim$yieldConifer)),
  col    = rainbow(nrow(sim$yieldConifer)),
  lty    = 1,
  lwd    = 2
)

# =========================================================
# SAVE OUTPUT TABLES
# =========================================================

data.table::fwrite(
  sim$areaByAU,
  "E:/EasternCanadaClassifier/outputs/AU_area.csv"
)

data.table::fwrite(
  sim$ageStructureByAU,
  "E:/EasternCanadaClassifier/outputs/AU_age_structure.csv"
)

data.table::fwrite(
  sim$ageSummaryByAU,
  "E:/EasternCanadaClassifier/outputs/AU_age_summary.csv"
)

data.table::fwrite(
  sim$speciesSummaryByAU,
  "E:/EasternCanadaClassifier/outputs/AU_species_summary.csv"
)

