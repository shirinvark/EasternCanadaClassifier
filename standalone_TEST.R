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


#ON
names(yt)

names(yt$ON)

names(yt$ON$`3e`)

max(
  sim$rawYieldTables$ON$`3e`[
    CURVENO == 1
  ]$AC10
)


raw <- sim$rawYieldTables$ON$`3e`[
  CURVENO == 1
]

plot(
  yt$ON$`3e`[[1]]$age,
  yt$ON$`3e`[[1]]$pine_boreal_ON,
  type = "l",
  lwd = 2
)

points(
  raw$AC10,
  raw$pine_boreal_ON,
  pch = 16
)



all(
  diff(yt$ON$`3e`[[1]]$age) == 1
)


tmp <- copy(yt$ON$`3e`[[1]])

tmp[
  ,
  total := rowSums(.SD),
  .SDcols = setdiff(names(tmp), "age")
]

plot(
  tmp$age,
  tmp$total,
  type = "l",
  lwd = 2
)


all(
  diff(yt$ON$`3e`[[1]]$age) == 1
)
tail(
  diff(tmp$total),
  20
)







####NL
names(yt)

names(yt$NL)

names(yt$NL$NPen)
head(
  yt$NL$NPen$BarNS_sub_all
)

tail(
  yt$NL$NPen$BarNS_sub_all
)
plot(
  yt$NL$NPen$BarNS_sub_all$age,
  yt$NL$NPen$BarNS_sub_all$blackSpruce_NL,
  type = "l",
  lwd = 2
)
tmp <- copy(
  yt$NL$NPen$BarNS_sub_all
)

tmp[
  ,
  total := rowSums(
    .SD
  ),
  .SDcols = setdiff(names(tmp), "age")
]
plot(
  tmp$age,
  tmp$total,
  type = "l",
  lwd = 2
)
tmp[
  which.max(total)
]
