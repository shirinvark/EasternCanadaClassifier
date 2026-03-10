Init <- function(sim) {
  
  message("Building analysisUnitMap from LandR state")
  
  ## ------------------------------------------------
  ## 1. Read yield tables (CSV)
  ## ------------------------------------------------
  
  file <- file.path(
    "modules",
    "EasternCanadaClassifier",
    "data",
    "yieldTables.csv"
  )
  
  yield_long <- data.table::fread(file)
  
  yieldTables <- data.table::dcast(
    yield_long,
    AU ~ age,
    value.var = "volume"
  )
  
  yieldTables <- as.matrix(yieldTables[, -1])
  sim$yieldAges   <- as.numeric(colnames(yieldTables))
  
  
  ## ------------------------------------------------
  ## 2. Convert cohortData to data.table
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
  
  
  ## ------------------------------------------------
  ## 5. Convert to wide table
  ## ------------------------------------------------
  
  summaryWide <- data.table::dcast(
    summaryTable,
    pixelGroup + age ~ type,
    value.var = "volume",
    fill = 0
  )
  
  if (!"conifer" %in% names(summaryWide))
    summaryWide[, conifer := 0]
  
  if (!"broadleaf" %in% names(summaryWide))
    summaryWide[, broadleaf := 0]
  
  
  ## ------------------------------------------------
  ## 6. Compute stand properties
  ## ------------------------------------------------
  
  summaryWide[, standVolume := conifer + broadleaf]
  
  summaryWide[, prop_conifer :=
                ifelse(standVolume > 0, conifer / standVolume, 0)]
  
  summaryWide[, prop_broadleaf :=
                ifelse(standVolume > 0, broadleaf / standVolume, 0)]
  
  
  ## ------------------------------------------------
  ## 7. Yield-table classifier
  ## ------------------------------------------------
  
  yieldTables <- sim$yieldTables
  
  nCurves <- nrow(yieldTables)
  nAges   <- ncol(yieldTables)
  
  summaryWide[, ageClass :=
                pmin(
                  floor(age / 10) + 1,
                  nAges
                )]
  
  summaryWide[, AU_id :=
                sapply(seq_len(.N), function(i) {
                  
                  a <- summaryWide$ageClass[i]
                  
                  vols <- yieldTables[, a]
                  
                  which.min(abs(vols - summaryWide$standVolume[i]))
                  
                })]
  
  ## ------------------------------------------------
  ## 8. Build lookup table
  ## ------------------------------------------------
  
  lookup <- summaryWide[, .(
    pixelGroup,
    AU_id
  )]
  
  
  ## ------------------------------------------------
  ## 9. Build analysisUnitMap
  ## ------------------------------------------------
  
  analysisUnitMap <- sim$pixelGroupMap
  
  pixelValues <- terra::values(sim$pixelGroupMap)
  
  mappedValues <- lookup$AU_id[
    match(pixelValues, lookup$pixelGroup)
  ]
  
  terra::values(analysisUnitMap) <- mappedValues
  
  analysisUnitMap <- terra::ifel(
    sim$harvestableFraction > 0,
    analysisUnitMap,
    NA
  )
  
  
  ## ------------------------------------------------
  ## 10. Save outputs
  ## ------------------------------------------------
  
  sim$analysisUnitMap <- analysisUnitMap
  
  message("analysisUnitMap created")
  
  return(sim)
}