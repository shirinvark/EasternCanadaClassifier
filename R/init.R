Init <- function(sim) {
  
  message("Building analysisUnitMap from LandR state")
  
  ## ------------------------------------------------
  ## 1. Read yield tables (CSV)
  ## ------------------------------------------------
  
  file <- file.path(
    sim@paths$inputPath,
    "yieldTables",
    "yieldTables.csv"
  )
  
  yield_long <- data.table::fread(file)
  
  yieldTables <- data.table::dcast(
    yield_long,
    AU ~ age,
    value.var = "volume"
  )
  
  yieldTables <- as.matrix(yieldTables[, -1])
  
  sim$yieldTables <- yieldTables
  sim$yieldAges   <- as.numeric(colnames(yieldTables))
  
  
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
  
  if (!"conifer" %in% names(summaryWide))
    summaryWide[, conifer := 0]
  
  if (!"broadleaf" %in% names(summaryWide))
    summaryWide[, broadleaf := 0]
  
  
  ## ------------------------------------------------
  ## 5. Compute stand proportions
  ## ------------------------------------------------
  
  summaryWide[, total := conifer + broadleaf]
  
  summaryWide[, prop_conifer :=
                ifelse(total > 0, conifer / total, 0)]
  
  summaryWide[, prop_broadleaf :=
                ifelse(total > 0, broadleaf / total, 0)]
  
  
  ## ------------------------------------------------
  ## 6. Yield-table classifier
  ## ------------------------------------------------
  
  yieldTables <- sim$yieldTables
  
  nCurves <- nrow(yieldTables)
  nAges   <- ncol(yieldTables)
  
  summaryWide[, ageClass :=
                pmin(
                  floor(age / 10) + 1,
                  nAges
                )]
  
  summaryWide[, standVolume :=
                conifer + broadleaf]
  
  summaryWide[, AU_id :=
                sapply(seq_len(.N), function(i) {
                  
                  a <- summaryWide$ageClass[i]
                  
                  vols <- yieldTables[, a]
                  
                  if (all(is.na(vols)))
                    return(NA)
                  
                  which.min(
                    abs(vols - summaryWide$standVolume[i])
                  )
                  
                })]
  
  
  ## ------------------------------------------------
  ## 7. Build lookup table
  ## ------------------------------------------------
  
  lookup <- summaryWide[, .(
    pixelGroup,
    AU_id
  )]
  
  
  ## ------------------------------------------------
  ## 8. Build analysisUnitMap
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
  ## 9. Save outputs
  ## ------------------------------------------------
  
  sim$analysisUnitMap <- analysisUnitMap
  
  
  message("analysisUnitMap created")
  
  return(sim)
}