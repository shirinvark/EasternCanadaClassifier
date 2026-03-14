
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
  
  # read the numeric table
  dataLines <- lines[2:(nCurves * 2 + 1)]
  
  yieldTables <- t(sapply(dataLines, function(x) {
    as.numeric(unlist(strsplit(trimws(x), "\\s+")))
  }))
  
  storage.mode(yieldTables) <- "numeric"
  coniferRows <- seq(1, nCurves * 2, by = 2)
  decidRows   <- seq(2, nCurves * 2, by = 2)
  
  yieldConifer <- yieldTables[coniferRows, ]
  yieldDeciduous   <- yieldTables[decidRows, ]
  
  sim$yieldConifer <- yieldConifer
  sim$yieldDeciduous   <- yieldDeciduous
  
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
    if (!g %in% names(summaryWide)) {
      summaryWide[, (g) := 0]
    }
  }
  if ("unknown" %in% names(summaryWide)) {
    summaryWide[, deciduous := deciduous + unknown]
  }
  summaryWide[, deciduous :=
                deciduous + unknown_broadleaf]
  
  summaryWide[, whiteSpruce :=
                whiteSpruce + unknown_conifer]
  
  ## ------------------------------------------------
  ## 6. Compute composition
  ## ------------------------------------------------
  # Compute total biomass per stand
  summaryWide[, total :=
                deciduous +
                whiteSpruce +
                pine +
                blackSpruce]
  
  summaryWide[, ageClass := round(age / 10) * 10]
  # Remove stands with zero biomass
  summaryWide <- summaryWide[total > 0]
  summaryWide <- summaryWide[
    , .SD[which.max(age)],
    by = pixelGroup
  ]  # Compute proportional composition of each species group
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
  
  # Assign each pixelGroup to the closest yield curve
  
    summaryWide[, AU_id :=
                sapply(seq_len(.N), function(i) {
                  
                  ageIndex <- summaryWide$ageIndex[i]
                  
            
                  decidFrac <- yieldDeciduous[, ageIndex] /
                    (yieldDeciduous[, ageIndex] + yieldConifer[, ageIndex])
                  
                  conifFrac <- yieldConifer[, ageIndex] /
                    (yieldDeciduous[, ageIndex] + yieldConifer[, ageIndex])
                  # Compute deciduous and conifer fractions from yield tables
                  
                  decidFrac[is.nan(decidFrac)] <- 0
                  conifFrac[is.nan(conifFrac)] <- 0
                  # Construct comparison matrix
                  
                  yieldMat <- cbind(
                    deciduous = decidFrac,
                    whiteSpruce = conifFrac,
                    pine = conifFrac,
                    blackSpruce = conifFrac
                  )   
                  # Pixel composition vector
                  
                  pixelVec <- as.numeric(
                    summaryWide[i,.(deciduous_p,whiteSpruce_p,pine_p,blackSpruce_p)]
                  )
                  # Compute distance to each yield curve
                  
                  diffs <- apply(
                    yieldMat,
                    1,
                    function(y) max(abs(pixelVec - y))
                  )
                  # Assign the closest yield curve
                  
                  which.min(diffs)                })
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
  
  message("analysisUnitMap created successfully")
  # Print the distribution of analysis units
  auFreq <- terra::freq(analysisUnitMap)
  
  message("Analysis Unit distribution:")
  print(auFreq)
  
  return(sim)
}

