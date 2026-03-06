Init <- function(sim) {
  
  message("Building analysisUnitMap from LandR state")
  
  ## ------------------------------------------------
  ## 1. Read yield tables
  ## ------------------------------------------------
  
  file <- "D:/GrowthSurves_STEVE/yieldTables/data/AB/AlPac AME Mixedwood VolTabs.vol"
  
  lines <- readLines(file)
  
  header <- strsplit(lines[1], " ")[[1]]
  
  nCurves <- as.numeric(gsub("#", "", header[1]))
  nAges   <- as.numeric(header[2])
  
  yieldTables <- do.call(
    rbind,
    lapply(lines[2:(nCurves + 1)], function(x)
      as.numeric(strsplit(trimws(x), "\\s+")[[1]])
    )
  )
  
  sim$yieldTables <- yieldTables
  sim$yieldAges   <- seq(0, by = 10, length.out = nAges)
  
  
  ## ------------------------------------------------
  ## 2. Convert cohortData
  ## ------------------------------------------------
  
  dt <- data.table::as.data.table(sim$cohortData)
  
  
  ## ------------------------------------------------
  ## 3. Species grouping
  ## ------------------------------------------------
  
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
  
  
  ## ------------------------------------------------
  ## 4. Biomass aggregation per pixelGroup
  ## ------------------------------------------------
  
  summaryTable <- dt[, .(
    volume = sum(B)
  ), by = .(pixelGroup, age, type)]
  
  
  summaryWide <- data.table::dcast(
    summaryTable,
    pixelGroup + age ~ type,
    value.var = "volume",
    fill = 0
  )
  
  
  ## ------------------------------------------------
  ## 5. Compute proportions
  ## ------------------------------------------------
  
  summaryWide[, total := conifer + broadleaf]
  
  summaryWide[, prop_conifer :=
                ifelse(total > 0, conifer / total, 0)]
  
  summaryWide[, prop_broadleaf :=
                ifelse(total > 0, broadleaf / total, 0)]
  
  
  ## ------------------------------------------------
  ## 6. Simple classifier (prototype)
  ## ------------------------------------------------
  
  summaryWide[, AU := "Mixed"]
  
  summaryWide[prop_conifer >= 0.7, AU := "Conifer"]
  
  summaryWide[prop_broadleaf >= 0.7, AU := "Broadleaf"]
  
  summaryWide[, AU_id := as.numeric(as.factor(AU))]
  
  
  ## ------------------------------------------------
  ## 7. Lookup table
  ## ------------------------------------------------
  
  lookup <- summaryWide[, .(pixelGroup, AU_id)]
  
  
  ## ------------------------------------------------
  ## 8. Build analysisUnitMap
  ## ------------------------------------------------
  
  analysisUnitMap <- sim$pixelGroupMap
  
  pixelValues <- terra::values(sim$pixelGroupMap)
  
  mappedValues <- lookup$AU_id[
    match(pixelValues, lookup$pixelGroup)
  ]
  
  terra::values(analysisUnitMap) <- mappedValues
  
  
  ## ------------------------------------------------
  ## 9. Save outputs
  ## ------------------------------------------------
  
  sim$analysisUnitMap <- analysisUnitMap
  
  
  message("analysisUnitMap created")
  
  return(sim)
}