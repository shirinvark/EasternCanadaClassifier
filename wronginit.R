
Init <- function(sim) {
  
  message("Building analysisUnitMap from LandR state")
  # Print a message so the user knows the classifier module is starting
  
  ## ------------------------------------------------
  ## -------------------------------------------
  ## 1. Read yield tables (.vol)
  ## ------------------------------------------------
  
  url <- "https://raw.githubusercontent.com/shirinvark/EasternCanadaClassifier/main/data/AlPac%20AME%20Mixedwood%20VolTabs.vol"
  
  dest <- "AlPac_AME_Mixedwood_VolTabs.vol"
  
  # download automatically if file does not exist
  if (!file.exists(dest)) {
    download.file(url, dest, mode = "wb")
  }
  
  # read file
  lines <- readLines(dest)
  
  # first line contains metadata (#8 21)
  header <- strsplit(lines[1], "\\s+")[[1]]
  
  nCurves <- as.numeric(gsub("#", "", header[1]))
  nAges   <- as.numeric(header[2])
  
  # read the numeric table(each table contains 2 lines conifer and deciduous)
  dataLines <- lines[2:(nCurves * 2 + 1)]
  
  
  #This section splits the numbers in the file and converts them into a numeric matrix.
  yieldTables <- t(sapply(dataLines, function(x) {
    as.numeric(unlist(strsplit(trimws(x), "\\s+")))
  }))
  
  
  #Ensures the matrix is stored as numeric values
  storage.mode(yieldTables) <- "numeric"
  #These lines identify which rows correspond to conifer and deciduous yields.
  coniferRows <- seq(1, nCurves * 2, by = 2)
  decidRows   <- seq(2, nCurves * 2, by = 2)
  
  
  #These lines create separate matrices for conifer and deciduous yields
  yieldConifer <- yieldTables[coniferRows, ]
  yieldDeciduous   <- yieldTables[decidRows, ]
  
  
  #These objects are stored in the simulation object so other modules can use them
  sim$yieldConifer <- yieldConifer
  sim$yieldDeciduous   <- yieldDeciduous
  
  #This creates the vector of stand ages.
  sim$yieldTables <- yieldTables
  sim$yieldAges <- seq(0, by = 10, length.out = nAges)
  
  
  ## ------------------------------------------------
  ## 2. Convert cohortData
  ## ------------------------------------------------
  # Convert cohortData to data.table for efficient aggregation
  
  dt <- data.table::as.data.table(sim$cohortData)
  
  
  ## ------------------------------------------------
  ## 3. Species grouping
  ## ------------------------------------------------
  
  # Define species groups used for classification
  deciduous <- c("Popu_tre","Popu_bal","Betu_pap")
  
  whiteSpruce <- c("Pice_gla","Abie_bal")
  
  pine <- c("Pinu_ban","Pinu_res","Pinu_str")
  
  blackSpruce <- c("Pice_mar","Lari_lar")
  
  
  
  #not sure if these are correct!
  knownConifer <- c(
    "Pice_gla","Abie_bal",
    "Pinu_ban","Pinu_res","Pinu_str",
    "Pice_mar","Lari_lar"
  )
  
  knownBroadleaf <- c(
    "Popu_tre","Popu_bal","Betu_pap"
  )
  
  
  # Assign each species to one of the four groups
  # fifelse is a faster version of ifelse from data.table
  dt[, group :=
       fifelse(speciesCode %in% deciduous, "deciduous",
               fifelse(speciesCode %in% whiteSpruce, "whiteSpruce",
                       fifelse(speciesCode %in% pine, "pine",
                               fifelse(speciesCode %in% blackSpruce, "blackSpruce",
                                       fifelse(speciesCode %in% knownConifer, "unknown_conifer",
                                               fifelse(speciesCode %in% knownBroadleaf, "unknown_broadleaf",
                                                       "unknown"))))))]
  ## ------------------------------------------------
  ## 4. Biomass aggregation
  ## ------------------------------------------------
  # Aggregate biomass (B) by pixelGroup, age, and species group
  # This summarizes stand biomass composition
  
  summaryTable <- dt[, .(
    volume = sum(B)
  ), by = .(pixelGroup, age, group)]
  
  
  ## ------------------------------------------------
  ## 5. Convert to wide
  ## ------------------------------------------------
  # Convert the table so each species group becomes a column
  summaryWide <- data.table::dcast(
    summaryTable,
    pixelGroup + age ~ group,
    value.var = "volume",
    fill = 0
  )
  
  # Ensure all expected species groups exist as columns
  # If a group is absent, create it with zeros
  for (g in c(
    "deciduous",
    "whiteSpruce",
    "pine",
    "blackSpruce",
    "unknown_conifer",
    "unknown_broadleaf"
  )) {
    #If a column does not exist, it is created and filled with zeros
    if (!g %in% names(summaryWide)) {
      summaryWide[, (g) := 0]
    }
  }
  #If a species is labeled unknown, it is added to the deciduous group
  if ("unknown" %in% names(summaryWide)) {
    summaryWide[, deciduous := deciduous + unknown]
  }
  summaryWide[, deciduous :=
                deciduous + unknown_broadleaf]
  #Unknown broadleaf species are added to the deciduous group
  summaryWide[, whiteSpruce :=
                whiteSpruce + unknown_conifer]
  #Unknown conifer species are added to the white spruce group
  ## ------------------------------------------------
  ## 6. Compute composition
  ## ------------------------------------------------
  # Compute total biomass per stand
  summaryWide[, total :=
                deciduous +
                whiteSpruce +
                pine +
                blackSpruce]
  
  
  
  
  #Stand age is rounded to the nearest 10-year age class.
  summaryWide[, ageClass := round(age / 10) * 10]
  
  # Remove stands with zero biomass
  summaryWide <- summaryWide[total > 0]
  
  #If multiple cohorts exist within a pixelGroup, the cohort with the maximum age(oldest) is selected
  summaryWide <- summaryWide[
    , .SD[base::which.max(age)],
    by = pixelGroup
  ]  
  
  
  # Here the proportion of each species group relative to the total biomass is calculated
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
  # Convert stand age to an index compatible with the yield tables
  summaryWide[, ageIndex := pmin(
    nAges,
    round(age / 10) + 1
  )]
  
  # Assign each pixelGroup to the closest yield curve(This loop runs for each pixelGroup and finds the closest yield curve)
  
  summaryWide[, AU_id :=
                sapply(seq_len(.N), function(i) {
                  #Conifer and deciduous yield values are extracted for the corresponding age
                  ageIndex <- summaryWide$ageIndex[i]
                  
                  conif <- yieldConifer[, ageIndex]
                  decid <- yieldDeciduous[, ageIndex]
                  
                  
                  #Yield values are converted to proportions
                  total <- conif + decid
                  conifFrac <- conif / total
                  decidFrac <- decid / total
                  
                  
                  #If division by zero occurs, the fraction is set to zero
                  conifFrac[is.nan(conifFrac)] <- 0
                  decidFrac[is.nan(decidFrac)] <- 0
                  
                  
                  # Construct comparison matrix(Curve number 2 is removed because it should be ignored)
                  validCurves <- setdiff(1:nCurves, 2)
                  
                  
                  
                  #This matrix represents the species composition of each yield curve
                  yieldMat <- cbind(
                    deciduous = decidFrac,
                    whiteSpruce = c(
                      conifFrac[1],
                      0,
                      conifFrac[3],
                      conifFrac[4],
                      conifFrac[5],
                      0,
                      0,
                      0
                    ),
                    pine = c(
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      conifFrac[7],
                      conifFrac[8]
                    ),
                    blackSpruce = c(
                      0,
                      0,
                      0,
                      0,
                      0,
                      conifFrac[6],
                      0,
                      0
                    )
                  )
                  yieldMat <- yieldMat[validCurves, ]
                  
                  
                  
                  # Pixel composition vector(This vector represents the species composition of the actual stand)
                  
                  pixelVec <- as.numeric(
                    summaryWide[i,.(deciduous_p,whiteSpruce_p,pine_p,blackSpruce_p)]
                  )
                  
                  
                  # Compute distance to each yield curve(The difference between stand composition and each yield curve composition is computed)
                  
                  diffs <- apply(
                    yieldMat,
                    1,
                    function(y) max(abs(pixelVec - y))
                  )
                  # Assign the closest yield curve
                  
                  validCurves[which.min(diffs)]               })
  ]
  ## ------------------------------------------------
  ## 8. Build lookup table
  ## ------------------------------------------------
  # Create a lookup table mapping pixelGroup → AU
  
  lookup <- summaryWide[,.(pixelGroup,AU_id)]
  
  # Remove duplicates
  lookup <- unique(lookup, by = "pixelGroup")
  
  # Sort by pixelGroup
  data.table::setorder(lookup, pixelGroup)
  
  
  ## ------------------------------------------------
  ## 9. Build analysisUnitMap
  ## -----------------------------------------------
  
  # Copy the pixelGroup raster
  analysisUnitMap <- sim$pixelGroupMap
  
  # Extract pixelGroup values
  pixelValues <- as.vector(terra::values(sim$pixelGroupMap))  
  # Match pixelGroup values to AU IDs
  mappedValues <- as.integer(
    lookup$AU_id[match(pixelValues, lookup$pixelGroup)]
  )
  
  # Ensure NA values are stored as integer NA
  mappedValues[is.na(mappedValues)] <- NA_integer_
  
  
  # Assign AU IDs back to the raster
  terra::values(analysisUnitMap) <- as.integer(mappedValues)
  
  
  ## ------------------------------------------------
  ## 10. Apply harvestable mask
  ## ------------------------------------------------
  # Remove non-harvestable pixels
  
  analysisUnitMap <- terra::ifel(
    sim$harvestableFraction > 0,
    analysisUnitMap,
    NA
  )
  
  
  ## ------------------------------------------------
  ## 11. Save outputs
  ## ------------------------------------------------
  # Store the resulting raster in the simulation object
  
  sim$analysisUnitMap <- analysisUnitMap
  ## ------------------------------------------------
  
  
  ## ------------------------------------------------
  ## 13. Area per analysis unit
  ## ------------------------------------------------
  # Compute cell area in hectares
  cellArea <- prod(terra::res(analysisUnitMap)) / 10000
  #The number of cells per analysis unit is counted
  areaTable <- data.table::as.data.table(terra::freq(analysisUnitMap))
  
  
  #The area of each analysis unit is calculated
  if (nrow(areaTable) > 0) {
    data.table::setnames(
      areaTable,
      old = c("value", "count"),
      new = c("analysisUnit", "nCells")
    )
    
    areaTable <- areaTable[!is.na(analysisUnit)]
    areaTable[, nCells := as.integer(nCells)]
    areaTable[, area_ha := nCells * cellArea]
    
  } else {
    areaTable <- data.table::data.table(
      analysisUnit = integer(0),
      nCells = integer(0),
      area_ha = numeric(0)
    )
  }
  
  sim$areaByAU <- areaTable
  
  
  ## ------------------------------------------------
  ## 14. Attach analysis unit to cohort data
  ## ------------------------------------------------
  # Build a lookup directly from rasters so cohort data can be linked to AUs
  pg <- terra::values(sim$pixelGroupMap)[, 1]
  au <- terra::values(analysisUnitMap)[, 1]
  
  
  
  #The analysis unit is attached to cohort data
  lookupAU <- data.table::data.table(
    pixelGroup = pg,
    analysisUnit = au
  )
  
  lookupAU <- lookupAU[!is.na(pixelGroup) & !is.na(analysisUnit)]
  lookupAU <- unique(lookupAU, by = "pixelGroup")
  data.table::setorder(lookupAU, pixelGroup)
  sim$pixelGroupToAU <- data.table::copy(lookupAU)
  # Merge AU into cohort data
  dtAU <- data.table::merge.data.table(
    data.table::copy(dt),
    lookupAU,
    by = "pixelGroup",
    all.x = TRUE
  )
  
  
  ## ------------------------------------------------
  ## 15. Age structure per analysis unit
  ## ------------------------------------------------
  ageBreaks <- c(0, 20, 40, 60, 80, 100, 150, Inf)
  
  dtAU[, ageClass := cut(
    age,
    breaks = ageBreaks,
    right = FALSE,
    labels = FALSE
  )]
  
  ageStructure <- dtAU[
    !is.na(analysisUnit),
    .N,
    by = .(analysisUnit, ageClass)
  ]
  
  data.table::setorder(ageStructure, analysisUnit, ageClass)
  
  sim$ageStructureByAU <- ageStructure
  
  
  ## ------------------------------------------------
  ## 16. Mean age summary per analysis unit
  ## ------------------------------------------------
  ageSummary <- dtAU[
    !is.na(analysisUnit),
    .(
      meanAge = mean(age, na.rm = TRUE),
      nStands = .N
    ),
    by = analysisUnit
  ]
  
  data.table::setorder(ageSummary, analysisUnit)
  
  sim$ageSummaryByAU <- ageSummary
  
  
  ## ------------------------------------------------
  ## 17. Species composition per analysis unit
  ## ------------------------------------------------
  speciesSummary <- dtAU[
    !is.na(analysisUnit),
    .(
      deciduous = sum(B[speciesCode %in% c(
        "Popu_tre","Popu_bal","Betu_pap"
      )], na.rm = TRUE),
      
      white_spruce = sum(B[speciesCode %in% c(
        "Pice_gla","Abie_bal"
      )], na.rm = TRUE),
      
      black_spruce = sum(B[speciesCode %in% c(
        "Pice_mar","Lari_lar"
      )], na.rm = TRUE),
      
      pine = sum(B[speciesCode %in% c(
        "Pinu_ban","Pinu_res","Pinu_str"
      )], na.rm = TRUE)
    ),
    by = analysisUnit
  ]
  
  speciesSummary[, total :=
                   deciduous +
                   white_spruce +
                   black_spruce +
                   pine]
  
  speciesSummary[, `:=`(
    deciduous_p = data.table::fifelse(total > 0, deciduous / total, 0),
    white_spruce_p = data.table::fifelse(total > 0, white_spruce / total, 0),
    black_spruce_p = data.table::fifelse(total > 0, black_spruce / total, 0),
    pine_p = data.table::fifelse(total > 0, pine / total, 0)
  )]
  
  data.table::setorder(speciesSummary, analysisUnit)
  
  sim$speciesSummaryByAU <- speciesSummary
  message("analysisUnitMap created successfully")
  # Print the distribution of analysis units
  auFreq <- terra::freq(analysisUnitMap)
  
  message("Analysis Unit distribution:")
  print(auFreq)
  
  return(sim)
}

