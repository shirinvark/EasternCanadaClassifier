
Init <- function(sim) {
  
  message("Building analysisUnitMap from LandR state")
  
  ## ------------------------------------------------
  ## 1. Read yield tables
  ## ------------------------------------------------
  
  file <- file.path(
    "modules",
    "EasternCanadaClassifier",
    "data",
    "yieldTables.csv"
  )
  
  yield_long <- data.table::fread(file)
  
  yieldTablesDT <- data.table::dcast(
    yield_long,
    AU ~ age,
    value.var = "volume"
  )
  
  yieldTables <- as.matrix(yieldTablesDT[, -1])
  
  storage.mode(yieldTables) <- "numeric"
  
  sim$yieldTables <- yieldTables
  sim$yieldAges <- as.numeric(colnames(yieldTables))
  
  
  ## ------------------------------------------------
  ## Yield curve composition (temporary approximation)
  ## ------------------------------------------------
  
  yieldComposition <- data.table::data.table(
    AU = yieldTablesDT$AU,
    
    deciduous = c(0.9,0.6,0.4,0.1,0.1,0.1,0.2,0.3),
    whiteSpruce = c(0.05,0.3,0.5,0.8,0.1,0.1,0.1,0.1),
    pine = c(0.02,0.05,0.05,0.05,0.05,0.05,0.7,0.6),
    blackSpruce = c(0.03,0.05,0.05,0.05,0.75,0.75,0,0)
  )
  
  sim$yieldComposition <- yieldComposition
  ## ------------------------------------------------
  ## 2. Convert cohortData
  ## ------------------------------------------------
  
  dt <- data.table::as.data.table(sim$cohortData)
  
  
  ## ------------------------------------------------
  ## 3. Species grouping
  ## ------------------------------------------------
  
  deciduous <- c("Popu_tre","Popu_bal","Betu_pap")
  whiteSpruce <- c("Pice_gla","Abie_bal")
  pine <- c("Pinu_ban","Pinu_res","Pinu_str")
  blackSpruce <- c("Pice_mar")
  
  dt[, group :=
       data.table::fifelse(
         speciesCode %in% deciduous,"deciduous",
         data.table::fifelse(
           speciesCode %in% whiteSpruce,"whiteSpruce",
           data.table::fifelse(
             speciesCode %in% pine,"pine",
             data.table::fifelse(
               speciesCode %in% blackSpruce,"blackSpruce",
               NA_character_
             )
           )
         )
       )]
  
  dt <- dt[!is.na(group)]
  
  
  ## ------------------------------------------------
  ## 4. Biomass aggregation
  ## ------------------------------------------------
  
  summaryTable <- dt[, .(
    volume = sum(B)
  ), by = .(pixelGroup, age, group)]
  
  
  ## ------------------------------------------------
  ## 5. Convert to wide
  ## ------------------------------------------------
  
  summaryWide <- data.table::dcast(
    summaryTable,
    pixelGroup + age ~ group,
    value.var = "volume",
    fill = 0
  )
  
  for (g in c("deciduous","whiteSpruce","pine","blackSpruce")) {
    if (!g %in% names(summaryWide)) {
      summaryWide[, (g) := 0]
    }
  }
  
  
  ## ------------------------------------------------
  ## 6. Compute composition
  ## ------------------------------------------------
  
  summaryWide[, total :=
                deciduous +
                whiteSpruce +
                pine +
                blackSpruce]
  summaryWide <- summaryWide[total > 0]
  summaryWide[, deciduous_p :=
                data.table::fifelse(total > 0, deciduous / total, 0)]
  
  summaryWide[, whiteSpruce_p :=
                data.table::fifelse(total > 0, whiteSpruce / total, 0)]
  
  summaryWide[, pine_p :=
                data.table::fifelse(total > 0, pine / total, 0)]
  
  summaryWide[, blackSpruce_p :=
                data.table::fifelse(total > 0, blackSpruce / total, 0)]
  

  ## ------------------------------------------------
  ## 7. Vector classifier
  ## ------------------------------------------------
  
  yieldMat <- as.matrix(
    sim$yieldComposition[,.(deciduous,whiteSpruce,pine,blackSpruce)]
  )
  storage.mode(yieldMat) <- "numeric"
  summaryWide[, AU_id :=
                
                sapply(seq_len(.N), function(i) {
                  
                  pixelVec <- as.numeric(
                    summaryWide[i,.(deciduous_p,whiteSpruce_p,pine_p,blackSpruce_p)]
                  )
                  
                  diffs <- apply(
                    yieldMat,
                    1,
                    function(y) max(abs(pixelVec - y))
                  )
                  
                  which.min(diffs)
                  
                })
  ]
  
  ## ------------------------------------------------
  ## 8. Build lookup table
  ## ------------------------------------------------
  
  lookup <- summaryWide[,.(pixelGroup,AU_id)]
  lookup <- unique(lookup, by = "pixelGroup")
  data.table::setorder(lookup, pixelGroup)
  
  
  ## ------------------------------------------------
  ## 9. Build analysisUnitMap
  ## ------------------------------------------------
  
  analysisUnitMap <- sim$pixelGroupMap
  
  pixelValues <- terra::values(sim$pixelGroupMap)
  
  mappedValues <- lookup$AU_id[
    match(pixelValues, lookup$pixelGroup)
  ]
  
  mappedValues[is.na(mappedValues)] <- NA_integer_
  
  terra::values(analysisUnitMap) <- as.integer(mappedValues)
  
  
  ## ------------------------------------------------
  ## 10. Apply harvestable mask
  ## ------------------------------------------------
  
  analysisUnitMap <- terra::ifel(
    sim$harvestableFraction > 0,
    analysisUnitMap,
    NA
  )
  
  
  ## ------------------------------------------------
  ## 11. Save outputs
  ## ------------------------------------------------
  
  sim$analysisUnitMap <- analysisUnitMap
  
  message("analysisUnitMap created successfully")
  
  auFreq <- terra::freq(analysisUnitMap)
  
  message("Analysis Unit distribution:")
  print(auFreq)
  
  return(sim)
}

