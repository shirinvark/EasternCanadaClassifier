Init <- function(sim) {
  
  # ---------------------------------------------------------
  # Inform the user that the classifier is starting
  # ---------------------------------------------------------
  
  message("Building analysisUnitMap from LandR state")
  
  
  ## ------------------------------------------------
  ## 1. Read yield tables
  ## ------------------------------------------------
  
  # Build the path to the yield table CSV file located
  # inside the module data directory
  
  file <- file.path(
    "modules",
    "EasternCanadaClassifier",
    "data",
    "yieldTables.csv"
  )
  
  # Read the CSV file containing yield curves
  # The table is expected in long format:
  # AU | age | volume
  
  yield_long <- data.table::fread(file)
  
  # Convert the table from long format to wide format
  # Rows = analysis units (AU)
  # Columns = ages
  # Values = volume
  
  yieldTablesDT <- data.table::dcast(
    yield_long,
    AU ~ age,
    value.var = "volume"
  )
  
  # Convert the data.table to a numeric matrix
  # This improves computational speed later
  
  yieldTables <- as.matrix(yieldTablesDT[, -1])
  
  # Ensure the matrix storage mode is numeric
  
  storage.mode(yieldTables) <- "numeric"
  
  # Store yield tables in the simulation object
  
  sim$yieldTables <- yieldTables
  
  # Extract the age values from the column names
  # and store them separately
  
  sim$yieldAges <- as.numeric(colnames(yieldTables))
  
  
  ## ------------------------------------------------
  ## 2. Convert cohortData to data.table
  ## ------------------------------------------------
  
  # Convert cohortData to a data.table for fast grouping
  # and aggregation operations
  
  dt <- data.table::as.data.table(sim$cohortData)
  
  
  ## ------------------------------------------------
  ## 3. Species grouping
  ## ------------------------------------------------
  
  # Define a vector of conifer species codes
  
  conifer <- c(
    "Abie_bal",
    "Pice_mar",
    "Pinu_ban",
    "Pinu_res",
    "Pinu_str"
  )
  
  # Create a new column indicating whether each cohort
  # belongs to conifer or broadleaf species
  
  dt[, type := ifelse(
    speciesCode %in% conifer,
    "conifer",
    "broadleaf"
  )]
  
  
  ## ------------------------------------------------
  ## 4. Biomass aggregation per pixelGroup
  ## ------------------------------------------------
  
  # Aggregate biomass (B) by pixelGroup, age, and species type
  # This produces the total biomass for each cohort group
  
  summaryTable <- dt[, .(
    volume = sum(B)
  ), by = .(pixelGroup, age, type)]
  
  
  ## ------------------------------------------------
  ## 5. Convert to wide table
  ## ------------------------------------------------
  
  # Convert the table to wide format so that
  # conifer and broadleaf volumes become separate columns
  
  summaryWide <- data.table::dcast(
    summaryTable,
    pixelGroup + age ~ type,
    value.var = "volume",
    fill = 0
  )
  
  # Ensure that the conifer column exists
  
  if (!"conifer" %in% names(summaryWide)) {
    summaryWide[, conifer := 0]
  }
  
  # Ensure that the broadleaf column exists
  
  if (!"broadleaf" %in% names(summaryWide)) {
    summaryWide[, broadleaf := 0]
  }
  
  
  ## ------------------------------------------------
  ## 6. Compute stand properties
  ## ------------------------------------------------
  
  # Compute total stand volume
  
  summaryWide[, standVolume := conifer + broadleaf]
  
  # Compute proportion of conifer biomass
  
  summaryWide[, prop_conifer :=
                ifelse(standVolume > 0, conifer / standVolume, 0)]
  
  # Compute proportion of broadleaf biomass
  
  summaryWide[, prop_broadleaf :=
                ifelse(standVolume > 0, broadleaf / standVolume, 0)]
  
  ## ------------------------------------------------
  ## 6b. Stand type classification
  ## ------------------------------------------------
  
  summaryWide[, standType :=
                data.table::fifelse(prop_conifer > 0.7, "conifer",
                                    data.table::fifelse(prop_broadleaf > 0.7, "broadleaf",
                                                        "mixed"))]
  # Optional: store stand types for diagnostics
  sim$standTypeTable <- summaryWide[, .(pixelGroup, age, standType)]
  ## ------------------------------------------------
  ## 7. Yield-table classifier
  ## ------------------------------------------------
  
  # Retrieve yield tables from the simulation object
  
  yieldTables <- sim$yieldTables
  
  # Number of age classes available in yield tables
  
  nAges <- ncol(yieldTables)
  
  # Convert stand age into a yield-table age class index
  
  summaryWide[, ageClass :=
                pmin(floor(age / 10) + 1, nAges)]
  
  # Assign each stand to the yield curve whose volume
  # best matches the observed stand volume
  
  summaryWide[, AU_id :=
                sapply(seq_len(.N), function(i) {
                  
                  a <- summaryWide$ageClass[i]
                  
                  # Extract volumes of all curves at this age
                  
                  vols <- yieldTables[, a]
                  
                  # Find the curve with minimum difference
                  # from the stand volume
                  
                  which.min(abs(vols - summaryWide$standVolume[i]))
                  
                })]
  
  # ------------------------------------------------
  # QA: report stand type distribution
  # ------------------------------------------------
  
  typeCount <- summaryWide[, .N, by = standType]
  
  message("Stand type distribution:")
  print(typeCount)
  ## ------------------------------------------------
  ## 8. Build lookup table
  ## ------------------------------------------------
  
  # Create a lookup table mapping pixelGroup to AU_id
  
  lookup <- summaryWide[, .(
    pixelGroup,
    AU_id
  )]
  
  # Remove duplicated pixelGroup entries
  
  lookup <- lookup[!duplicated(pixelGroup)]
  
  
  ## ------------------------------------------------
  ## 9. Build analysisUnitMap
  ## ------------------------------------------------
  
  # Use pixelGroupMap as the template raster
  
  analysisUnitMap <- sim$pixelGroupMap
  
  # Extract pixelGroup values from the raster
  
  pixelValues <- terra::values(sim$pixelGroupMap)
  
  # Map pixelGroup values to analysis units using the lookup table
  
  mappedValues <- lookup$AU_id[
    match(pixelValues, lookup$pixelGroup)
  ]
  
  # Ensure integer storage
  
  mappedValues <- as.integer(mappedValues)
  
  # Assign analysis unit values to the raster
  
  terra::values(analysisUnitMap) <- mappedValues
  
  
  ## ------------------------------------------------
  ## 10. Apply harvestable mask
  ## ------------------------------------------------
  
  # Mask out areas that are not harvestable
  
  analysisUnitMap <- terra::ifel(
    sim$harvestableFraction > 0,
    analysisUnitMap,
    NA
  )
  
  
  ## ------------------------------------------------
  ## 11. Save outputs
  ## ------------------------------------------------
  
  # Store the final Analysis Unit raster in the simulation object
  
  sim$analysisUnitMap <- analysisUnitMap
  
  # Inform the user that the map was successfully created
  
  message("analysisUnitMap created successfully")
  # ------------------------------------------------
  # QA: AU distribution
  # ------------------------------------------------
  
  auFreq <- terra::freq(analysisUnitMap)
  
  message("Analysis Unit distribution:")
  print(auFreq)
  return(sim)
}