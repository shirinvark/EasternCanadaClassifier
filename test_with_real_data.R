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
# PREPARE DATA FOR ANALYSIS
# =========================================================

dt <- as.data.table(sim$cohortData)

ageBreaks <- c(0,20,40,60,80,100,150,Inf)

dt[, ageClass := cut(
  age,
  breaks = ageBreaks,
  right = FALSE,
  labels = FALSE
)]

# =========================================================
# BUILD PIXELGROUP → ANALYSISUNIT LOOKUP
# =========================================================

pg <- terra::values(sim$pixelGroupMap)[,1]
au <- terra::values(sim$analysisUnitMap)[,1]

lookupAU <- data.table(
  pixelGroup = pg,
  analysisUnit = au
)

lookupAU <- lookupAU[!is.na(pixelGroup) & !is.na(analysisUnit)]
lookupAU <- unique(lookupAU)

# attach AU to cohort data
dt <- merge(dt, lookupAU, by = "pixelGroup", all.x = TRUE)

# =========================================================
# AGE STRUCTURE PER ANALYSIS UNIT
# =========================================================

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
  col = rainbow(nrow(yieldTables)),
  lty = 1,
  lwd = 2
)

# =========================================================
# SIMPLE MAP PLOT
# =========================================================

x11()

plot(
  sim$analysisUnitMap,
  col = terrain.colors(8),
  main = "Analysis Units"
)

# =========================================================
# SAVE OUTPUT TABLES
# =========================================================

fwrite(
  ageStructure,
  "E:/EasternCanadaClassifier/outputs/AU_age_structure.csv"
)

fwrite(
  ageSummary,
  "E:/EasternCanadaClassifier/outputs/AU_age_summary.csv"
)

fwrite(
  areaTable,
  "E:/EasternCanadaClassifier/outputs/AU_area.csv"
)

# =========================================================
# SPECIES COMPOSITION PER ANALYSIS UNIT
# =========================================================

speciesSummary <- dt[, .(
  
  deciduous = sum(B[speciesCode %in% c(
    "Popu_tre","Popu_bal","Betu_pap"
  )]),
  
  white_spruce = sum(B[speciesCode %in% c(
    "Pice_gla","Abie_bal"
  )]),
  
  black_spruce = sum(B[speciesCode %in% c(
    "Pice_mar","Lari_lar"
  )]),
  
  pine = sum(B[speciesCode %in% c(
    "Pinu_ban","Pinu_res","Pinu_str"
  )])
  
), by = analysisUnit]

# remove NA AU
speciesSummary <- speciesSummary[!is.na(analysisUnit)]

# total biomass
speciesSummary[, total :=
                 deciduous +
                 white_spruce +
                 black_spruce +
                 pine
]

# proportions
speciesSummary[, `:=`(
  
  deciduous_p    = deciduous / total,
  white_spruce_p = white_spruce / total,
  black_spruce_p = black_spruce / total,
  pine_p         = pine / total
  
)]

print(speciesSummary)

# =========================================================
# SPECIES COMPOSITION PLOT
# =========================================================

x11()

barplot(
  t(as.matrix(
    speciesSummary[, .(
      deciduous_p,
      white_spruce_p,
      black_spruce_p,
      pine_p
    )]
  )),
  col = c("darkgreen","lightblue","blue","orange"),
  legend = c("Deciduous","WhiteSpruce","BlackSpruce","Pine"),
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
  legend = paste("AU",1:nrow(sim$yieldConifer)),
  col = rainbow(nrow(sim$yieldConifer)),
  lty = 1,
  lwd = 2
)