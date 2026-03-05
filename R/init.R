Init <- function(sim) {
  
  message("Building analysisUnitMap from LandR state")
  
  dt <- data.table::as.data.table(sim$cohortData)
  
  # ----------------------------
  # species grouping
  # ----------------------------
  
  conifer <- c(
    "Abie_bal",
    "Pice_mar",
    "Pinu_ban",
    "Pinu_res",
    "Pinu_str"
  )
  
  dt[, type := ifelse(
    speciesCode %in% conifer,
    "conifer",
    "broadleaf"
  )]
  
  # ----------------------------
  # biomass per pixelGroup
  # ----------------------------
  
  summaryTable <- dt[, .(
    volume = sum(B)
  ), by = .(pixelGroup, age, type)]
  
  summaryWide <- data.table::dcast(
    summaryTable,
    pixelGroup + age ~ type,
    value.var = "volume",
    fill = 0
  )
  
  # ----------------------------
  # proportions
  # ----------------------------
  
  summaryWide[, total := conifer + broadleaf]
  
  summaryWide[, prop_conifer := conifer / total]
  
  summaryWide[, prop_broadleaf := broadleaf / total]
  
  # ----------------------------
  # simple classifier
  # ----------------------------
  
  summaryWide[, AU := "Mixed"]
  
  summaryWide[prop_conifer >= 0.7, AU := "Conifer"]
  
  summaryWide[prop_broadleaf >= 0.7, AU := "Broadleaf"]
  
  summaryWide[, AU_id := as.numeric(as.factor(AU))]
  
  # ----------------------------
  # lookup table
  # ----------------------------
  
  lookup <- summaryWide[, .(pixelGroup, AU_id)]
  
  # ----------------------------
  # build raster
  # ----------------------------
  
  analysisUnitMap <- sim$pixelGroupMap
  
  terra::values(analysisUnitMap) <- lookup$AU_id[
    match(
      terra::values(sim$pixelGroupMap),
      lookup$pixelGroup
    )
  ]
  
  sim$analysisUnitMap <- analysisUnitMap
  
  message("analysisUnitMap created")
  
  return(sim)
}