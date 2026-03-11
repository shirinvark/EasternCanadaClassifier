
Init <- function(sim) {
  
  message("Building analysisUnitMap from LandR state")
  # Print a message so the user knows the classifier module is starting
  
  ## ------------------------------------------------
  ## 1. Read yield tables
  ## ------------------------------------------------
  # Build the path to the yield table file stored inside the module
  
  file <- file.path(
    "modules",
    "EasternCanadaClassifier",
    "data",
    "yieldTables.csv"
  )
  # Read the yield table file using data.table for speed
  # The file is expected to contain columns: AU, age, volume
  yield_long <- data.table::fread(file)
  # Convert the yield table from long format to wide format
  # Each AU becomes one row and each age becomes a column
  yieldTablesDT <- data.table::dcast(
    yield_long,
    AU ~ age,
    value.var = "volume"
  )
  # Remove the AU column and convert the table to a matrix
  # Matrices are faster for numerical operations
  yieldTables <- as.matrix(yieldTablesDT[, -1])
  
  # Ensure the matrix contains numeric values
  storage.mode(yieldTables) <- "numeric"
  
  # Store yield tables in the simulation object
  sim$yieldTables <- yieldTables
  
  # Extract the age classes from the column names
  sim$yieldAges <- as.numeric(colnames(yieldTables))
  
  
  ## ------------------------------------------------
  ## Yield curve composition (temporary approximation)
  ## ------------------------------------------------
  
  # Define species composition for each yield curve
  # These values are temporary approximations used for classification
  # Yield curve composition derived from YcNames described by Steve
  yieldComposition <- data.table::data.table(
    AU = yieldTablesDT$AU,
    
    deciduous = c(
      0.9,  # Aw
      NA,   # Aw/S (ignored)
      0.6,  # AwSw
      0.4,  # SwAw
      0.1,  # Sw
      0.1,  # Sb
      0.0,  # Pj
      0.3   # MxPj
    ),
    
    whiteSpruce = c(
      0.1,  # Aw
      NA,
      0.4,  # AwSw
      0.6,  # SwAw
      0.9,  # Sw
      0.0,  # Sb
      0.0,  # Pj
      0.0   # MxPj
    ),
    
    pine = c(
      0.0,
      NA,
      0.0,
      0.0,
      0.0,
      0.0,
      1.0,  # Pj
      0.7   # MxPj
    ),
    
    blackSpruce = c(
      0.0,
      NA,
      0.0,
      0.0,
      0.0,
      0.9,  # Sb
      0.0,
      0.0
    )
  )
  yieldComposition <- yieldComposition[!is.na(deciduous)]
  
  # Store species composition associated with yield curves
  sim$yieldComposition <- yieldComposition
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
  blackSpruce <- c("Pice_mar")
  
  # Assign each species to one of the four groups
  # fifelse is a faster version of ifelse from data.table
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
  
  ##########??????????? Remove species that do not belong to any defined group?maybe we can defin others instead of removing?
  
  dt <- dt[!is.na(group)]
  
  
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
  for (g in c("deciduous","whiteSpruce","pine","blackSpruce")) {
    if (!g %in% names(summaryWide)) {
      summaryWide[, (g) := 0]
    }
  }
  
  
  ## ------------------------------------------------
  ## 6. Compute composition
  ## ------------------------------------------------
  # Compute total biomass per stand
  summaryWide[, total :=
                deciduous +
                whiteSpruce +
                pine +
                blackSpruce]
  
  
  # Remove stands with zero biomass
  summaryWide <- summaryWide[total > 0]
  
  # Compute proportional composition of each species group
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
  
  # Convert yield composition to a numeric matrix
  yieldMat <- as.matrix(
    sim$yieldComposition[,.(deciduous,whiteSpruce,pine,blackSpruce)]
  )
  
  # For each pixelGroup, find the yield curve whose species composition
  # is closest to the observed stand composition
  storage.mode(yieldMat) <- "numeric"
  summaryWide[, AU_id :=
                
                sapply(seq_len(.N), function(i) {
                  
                  # Species composition vector of the current stand
                  pixelVec <- as.numeric(
                    summaryWide[i,.(deciduous_p,whiteSpruce_p,pine_p,blackSpruce_p)]
                  )
                  
                  # Compute the maximum absolute difference
                  # between the stand composition and each yield curve
                  diffs <- apply(
                    yieldMat,
                    1,
                    function(y) sum((pixelVec - y)^2)
                  )
                  
                  # Select the yield curve with the smallest difference
                  sim$yieldComposition$AU[which.min(diffs)]                  
                })
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
  pixelValues <- terra::values(sim$pixelGroupMap)
  
  # Match pixelGroup values to AU IDs
  mappedValues <- lookup$AU_id[
    match(pixelValues, lookup$pixelGroup)
  ]
  
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

