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
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

# =========================================================
# CREATE TEST DATA (FAKE LANDR STATE)
# =========================================================

# pixelGroup raster

pixelGroupMap <- rast(
  nrows = 10,
  ncols = 10,
  xmin  = 0,
  xmax  = 1000,
  ymin  = 0,
  ymax  = 1000
)

values(pixelGroupMap) <- sample(1:20, 100, replace = TRUE)

# cohortData (LandR-like structure)

cohortData <- data.table(
  pixelGroup = sample(1:20, 200, replace = TRUE),
  speciesCode = sample(
    c("Abie_bal","Pice_mar","Pinu_ban","Pinu_res","Pinu_str","Acer_sah"),
    200,
    replace = TRUE
  ),
  age = sample(1:120, 200, replace = TRUE),
  B   = runif(200, 1, 50)
)

# =========================================================
# INITIALIZE SIMULATION
# =========================================================

sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "EasternCanadaClassifier",
  
  # uncomment if you want to pass objects directly
  # objects = list(
  #   cohortData = cohortData,
  #   pixelGroupMap = pixelGroupMap
  # ),
  
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
# PLOT ANALYSIS UNIT MAP
# =========================================================

plot(
  sim$analysisUnitMap,
  col  = terrain.colors(8),
  main = "Analysis Unit Map"
)

# =========================================================
# AREA PER ANALYSIS UNIT
# =========================================================

cellArea <- prod(res(sim$analysisUnitMap)) / 10000

areaTable <- as.data.frame(freq(sim$analysisUnitMap))

areaTable$area_ha <- areaTable$count * cellArea

print(areaTable)

# =========================================================
# AGE STRUCTURE PER ANALYSIS UNIT
# =========================================================

dt <- as.data.table(sim$cohortData)

ageBreaks <- c(0,20,40,60,80,100,150,Inf)

dt[, ageClass := cut(
  age,
  breaks = ageBreaks,
  right = FALSE,
  labels = FALSE
)]

auValues <- terra::values(sim$analysisUnitMap)

lookupAU <- data.table(
  pixelGroup   = unique(dt$pixelGroup),
  analysisUnit = auValues[unique(dt$pixelGroup)]
)

dt <- merge(dt, lookupAU, by = "pixelGroup", all.x = TRUE)

ageStructure <- dt[, .N, by = .(analysisUnit, ageClass)]

print(ageStructure)

# =========================================================
# MEAN AGE PER ANALYSIS UNIT
# =========================================================

ageSummary <- dt[, .(
  meanAge = mean(age, na.rm = TRUE),
  nStands = .N
), by = analysisUnit]

print(ageSummary)

# =========================================================
# PLOT YIELD CURVES
# =========================================================

yieldTables <- sim$yieldTables
yieldAges   <- sim$yieldAges

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
  col = rainbow(nrow(yieldTables)),
  lty = 1,
  lwd = 2
)