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
      jurisdiction = "NL"
    )
  ),
  
  options = list(
    spades.checkpoint = FALSE,
    spades.save       = FALSE,
    spades.progress   = TRUE
  )
)

# ======================================================
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


# =========================================================
# CHECK OUTPUT
# =========================================================
sim$rawYieldTables$ON$`3e`[
  ,
  list(
    max_otherConifer =
      max(otherConifer_ON, na.rm = TRUE)
  ),
  by = FU
][
  order(-max_otherConifer)
]



lapply(
  sim$rawYieldTables$ON,
  function(x) {
    
    x[
      ,
      .(
        max_otherConifer =
          max(otherConifer_ON, na.rm = TRUE)
      ),
      by = FU
    ][
      order(-max_otherConifer)
    ]
    
  }
)




#####
curve_dt <- sim$rawYieldTables$ON$`3e`[
  CURVENO == 1
]
curve_dt[
  ,
  grouped_total :=
    blackSpruce_ON +
    spruce_ON +
    balsamFir_ON +
    pine_boreal_ON +
    pine_temperate_ON +
    otherConifer_ON +
    broadleaf_boreal_ON +
    broadleaf_temperate_ON
]
plot(
  curve_dt$AC10,
  curve_dt$grouped_total,
  type = "b",
  pch = 16
)
curve_dt[
  ,
  .(
    AC10,
    grouped_total
  )
]







curve_dt <- sim$rawYieldTables$ON$`3e`[
  CURVENO == 1
]
curve_dt[
  ,
  raw_total :=
    blackSpruce_ON +
    spruce_ON +
    balsamFir_ON +
    pine_boreal_ON +
    pine_temperate_ON +
    otherConifer_ON +
    broadleaf_boreal_ON +
    broadleaf_temperate_ON
]
yt <- sim$yieldTables
tmp <- yt$ON$`3e`[["1"]]
species_cols <- setdiff(
  names(tmp),
  "age"
)

tmp[
  ,
  interp_total :=
    rowSums(.SD),
  .SDcols = species_cols
]
tmp_check <- tmp[
  age %in% curve_dt$AC10
]
data.table(
  age = curve_dt$AC10,
  raw_total = curve_dt$raw_total,
  interp_total = tmp_check$interp_total,
  difference =
    curve_dt$raw_total -
    tmp_check$interp_total
)

names(sim$rawYieldTables$ON)

lapply(
  sim$rawYieldTables$ON,
  function(x) unique(x$SUBMU)
)
length(unique(sim$rawYieldTables$ON$`3e`$CURVENO))
table(sim$rawYieldTables$ON$`3e`$CURVENO)[1:10]
unique(sim$cohortData$speciesCode)
tmp <- sim$yieldTables$ON$`3e`[[1]]

head(tmp)

curve_check <- sim$rawYieldTables$ON$`3e`

species_cols <- c(
  "blackSpruce_ON",
  "spruce_ON",
  "balsamFir_ON",
  "pine_ON",
  "broadleaf_ON"
)

curve_check[
  ,
  total := rowSums(.SD, na.rm = TRUE),
  .SDcols = species_cols
]

summary(curve_check$total)


















































































##NL
yt <- sim$yieldTables
sim$rawYieldTables$NL$NPen[
  ,
  list(
    max_otherConifer =
      max(otherConifer_NL, na.rm = TRUE)
  ),
  by = CURVENO
][
  order(-max_otherConifer)
]

curve_dt <- sim$rawYieldTables$NL$NPen[
  CURVENO == "BarNS_sub_all"
]

curve_dt[
  ,
  total :=
    blackSpruce_NL +
    balsamFir_NL +
    tamarack_NL +
    otherConifer_NL +
    broadleaf_NL
]

plot(
  curve_dt$AC10,
  curve_dt$total,
  type = "l"
)
std <- yt$NL$NPen$BarNS_sub_all

std[
  ,
  total_std :=
    blackSpruce_NL +
    balsamFir_NL +
    tamarack_NL +
    otherConifer_NL +
    broadleaf_NL
]

plot(
  curve_dt$AC10,
  curve_dt$total,
  type = "l"
)

lines(
  std$age,
  std$total_std,
  lty = 2
)


head(
  yt$NL$NPen$BarNS_sub_all,
  20
)

tail(
  yt$NL$NPen$BarNS_sub_all,
  20
)


plot(
  yt$NL$NPen$BarNS_sub_all$age,
  rowSums(
    yt$NL$NPen$BarNS_sub_all[
      ,
      .(
        blackSpruce_NL,
        balsamFir_NL,
        tamarack_NL,
        otherConifer_NL,
        broadleaf_NL
      )
    ]
  ),
  type = "l"
)


curve_dt <- sim$rawYieldTables$NL$NPen[
  CURVENO == "BarNS_sub_all"
]

curve_dt[
  ,
  total_grouped :=
    blackSpruce_NL +
    balsamFir_NL +
    tamarack_NL +
    otherConifer_NL +
    broadleaf_NL
]
std <- yt$NL$NPen$BarNS_sub_all

std[
  ,
  total_std :=
    blackSpruce_NL +
    balsamFir_NL +
    tamarack_NL +
    otherConifer_NL +
    broadleaf_NL
]
merge(
  curve_dt[
    ,
    .(
      age = AC10,
      total_grouped
    )
  ],
  
  std[
    age %in% curve_dt$AC10,
    .(
      age,
      total_std
    )
  ],
  
  by = "age"
)

