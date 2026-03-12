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

# LOAD MAPS FROM PROJECT

# =========================================================

pixelGroupMap <- rast(
  "E:/EasternCanadaClassifier/maps/pixel_groups.tif"
)
#======================================================
standAgeMap <- terra::rast("E:/EasternCanadaClassifier/maps/stand_age_map.tif")
# =========================================================

# LOAD COHORT DATA

# =========================================================

cohortData <- readRDS(
  "E:/EasternCanadaClassifier/maps/cohortData.rds"
)

# =========================================================

# INITIALIZE SIMULATION

# =========================================================
harvestableFraction <- pixelGroupMap
values(harvestableFraction) <- 1
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "EasternCanadaClassifier",
  
  objects = list(
    cohortData    = cohortData,
    pixelGroupMap = pixelGroupMap,
    standAgeMap   = standAgeMap,
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

pg <- terra::values(sim$pixelGroupMap)[,1]
au <- terra::values(sim$analysisUnitMap)[,1]

lookupAU <- data.table(
  pixelGroup = pg,
  analysisUnit = au
)

lookupAU <- lookupAU[!is.na(pixelGroup) & !is.na(analysisUnit)]
lookupAU <- unique(lookupAU)

dt <- merge(dt, lookupAU, by = "pixelGroup", all.x = TRUE)

ageStructure <- dt[, .N, by = .(analysisUnit, ageClass)]

print(ageStructure[order(analysisUnit, ageClass)])
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
  col = rainbow(nrow(yieldTables)),
  lty = 1,
  lwd = 2
)

#================================================
x11()

plot(
  sim$analysisUnitMap,
  col = c("grey80","orange","darkgreen"),
  main = "Analysis Units"
)

legend(
  "bottomleft",
  legend = c("Non-harvestable","Young","Mature"),
  fill = c("grey80","orange","darkgreen"),
  bg = "white"
)
#==================================================
fwrite(ageStructure,
       "E:/EasternCanadaClassifier/outputs/AU_age_structure.csv")

fwrite(ageSummary,
       "E:/EasternCanadaClassifier/outputs/AU_age_summary.csv")

fwrite(areaTable,
       "E:/EasternCanadaClassifier/outputs/AU_area.csv")
