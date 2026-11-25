# =============================================================================
# NATIONAL RSYC - PIXEL GROUP CLASSIFICATION
#
# EasternCanadaClassifier
#
# Purpose:
#   Classify LandR pixel groups using National RSYC yield curves.
#
# Inputs:
#   - sim$cohortData
#   - sim$pixelGroupMap
#   - National RSYC yield curves
#   - National species mapping
#   - National ecodistricts
#
# =============================================================================

classifyNationalRSYC <- function(
    sim,
    experimentalLarix = FALSE,
    experimentalFallback = FALSE
) {
  
  # ---------------------------------------------------------------------------
  # 1. Validate upstream inputs
  # ---------------------------------------------------------------------------
  
  message("Starting National RSYC classification...")
  
  if (is.null(sim$cohortData)) {
    stop("Missing sim$cohortData.")
  }
  
  if (is.null(sim$pixelGroupMap)) {
    stop("Missing sim$pixelGroupMap.")
  }
  
  if (!inherits(sim$pixelGroupMap, "SpatRaster")) {
    stop("sim$pixelGroupMap must be a terra SpatRaster.")
  }
  
  if (terra::nlyr(sim$pixelGroupMap) != 1L) {
    stop("sim$pixelGroupMap must contain exactly one layer.")
  }
  
  if (!inherits(sim$cohortData, c("data.frame", "data.table"))) {
    stop("sim$cohortData must be a data.frame or data.table.")
  }
  
  # ---------------------------------------------------------------------------
  # 2. Load National RSYC reference data
  # ---------------------------------------------------------------------------
  
  nationalResult <- translateNationalRSYC()
  
  nationalMapping <- readNationalSpeciesMapping()
  
  nationalEco <- readNationalEcodistricts()
  
  # Validate National RSYC reference data
  
  if (!is.list(nationalResult) ||
      is.null(nationalResult$curveLookup)) {
    stop("translateNationalRSYC() must return a list containing curveLookup.")
  }
  
  if (!all(c("ecodistrict_id", "species", "model_group", "curveID") %in%
           names(nationalResult$curveLookup))) {
    stop("Missing required columns in National RSYC curveLookup.")
  }
  
  if (!all(c("LandR", "species") %in% names(nationalMapping))) {
    stop("Missing required columns in National species mapping.")
  }
  
  if (!inherits(nationalEco, "SpatVector") ||
      !("ecodistrict_id" %in% names(nationalEco))) {
    stop("Invalid National RSYC ecodistrict polygons.")
  }
  
  message("National RSYC reference data loaded.")
  
  # ---------------------------------------------------------------------------
  # 3. Match pixel groups to National RSYC ecodistricts
  # ---------------------------------------------------------------------------
  
  ecodistrictMap <- buildNationalEcodistrictMap(
    pixelGroupMap = sim$pixelGroupMap,
    nationalEco = nationalEco
  )
  
  pixelGroupEco <- matchNationalEcodistricts(
    pixelGroupMap = sim$pixelGroupMap,
    ecodistrictMap = ecodistrictMap
  )
  
  message(
    "Pixel groups matched to ecodistricts: ",
    sum(!is.na(pixelGroupEco$ecodistrict_id)),
    " / ",
    nrow(pixelGroupEco)
  )
  
  # Ecodistrict assignment diagnostics
  
  nUnmatchedPG <- sum(is.na(pixelGroupEco$ecodistrict_id))
  
  nMultiEcoPG <- sum(
    pixelGroupEco$nEcodistricts > 1,
    na.rm = TRUE
  )
  
  message(
    "Pixel groups without ecodistrict: ",
    nUnmatchedPG
  )
  
  message(
    "Pixel groups spanning multiple ecodistricts: ",
    nMultiEcoPG
  )
  
  # Check whether assigned ecodistrict IDs exist in RSYC lookup
  
  assignedEcoIDs <- unique(
    pixelGroupEco[!is.na(ecodistrict_id), ecodistrict_id]
  )
  
  availableEcoIDs <- unique(
    nationalResult$curveLookup$ecodistrict_id
  )
  
  missingEcoIDs <- setdiff(
    as.character(assignedEcoIDs),
    as.character(availableEcoIDs)
  )
  
  message(
    "Assigned ecodistrict IDs absent from RSYC lookup: ",
    length(missingEcoIDs)
  )
  
  if (length(missingEcoIDs) > 0L) {
    warning(
      "Ecodistrict IDs without matching RSYC records: ",
      paste(missingEcoIDs, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # 4. Map LandR cohort species to National RSYC species
  # ---------------------------------------------------------------------------
  
  cohortDT <- data.table::as.data.table(
    data.table::copy(sim$cohortData)
  )
  
  # Standardize pixelGroup column name
  pg_col <- grep(
    "^pixelgroup$",
    names(cohortDT),
    ignore.case = TRUE,
    value = TRUE
  )
  
  if (length(pg_col) != 1L) {
    stop("Could not uniquely identify pixelGroup column.")
  }
  
  data.table::setnames(cohortDT, pg_col, "pixelGroup")
  
  # Validate required cohort columns
  requiredCols <- c("pixelGroup", "speciesCode", "age", "B")
  
  if (!all(requiredCols %in% names(cohortDT))) {
    stop("Missing required columns in cohortData.")
  }
  
  # Match LandR species codes to National RSYC species
  
  # Match LandR species codes to National RSYC species
  
  cohortDT[, speciesCode := as.character(speciesCode)]
  
  nationalMapping <- data.table::as.data.table(
    data.table::copy(nationalMapping)
  )
  
  nationalMapping[, LandR := as.character(LandR)]
  nationalMapping[, species := as.character(species)]
  
  # Validate unique LandR species mapping
  if (anyDuplicated(nationalMapping$LandR)) {
    stop("Duplicate LandR species codes in National mapping.")
  }
  
  # Preserve original cohort record count
  nCohortsBefore <- nrow(cohortDT)
  if ("species" %in% names(cohortDT)) {
    stop("cohortData already contains a species column; inspect before mapping.")
  }
  cohortDT <- merge(
    cohortDT,
    nationalMapping,
    by.x = "speciesCode",
    by.y = "LandR",
    all.x = TRUE,
    sort = FALSE
  )
  
  if (nrow(cohortDT) != nCohortsBefore) {
    stop("Cohort record count changed during species mapping.")
  }
  
  
  # Check species mapping
  message(
    "Mapped cohort records: ",
    sum(!is.na(cohortDT$species)),
    " / ",
    nrow(cohortDT)
  )
  
  unmappedSpecies <- unique(
    cohortDT[is.na(species), speciesCode]
  )
  
  if (length(unmappedSpecies) > 0L) {
    warning(
      "Unmapped LandR species: ",
      paste(unmappedSpecies, collapse = ", ")
    )
  }
    # ---------------------------------------------------------------------------
    # 5. Attach National RSYC ecodistricts to cohort records
    # ---------------------------------------------------------------------------
    
  
  # ---------------------------------------------------------------------------
  # 5. Attach National RSYC ecodistricts to cohort records
  # ---------------------------------------------------------------------------
  
  # Validate unique pixelGroup IDs in spatial lookup
  if (anyDuplicated(pixelGroupEco$pixelGroup)) {
    stop("Duplicate pixelGroup IDs in pixelGroupEco.")
  }
  
  # Preserve original cohort record count
  nCohortsBeforeEco <- nrow(cohortDT)
  
  cohortEco <- merge(
    cohortDT,
    pixelGroupEco[, .(pixelGroup, ecodistrict_id)],
    by = "pixelGroup",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Validate record count
  if (nrow(cohortEco) != nCohortsBeforeEco) {
    stop("Cohort record count changed during ecodistrict matching.")
  }
  
  message(
    "Cohort records matched to ecodistricts: ",
    sum(!is.na(cohortEco$ecodistrict_id)),
    " / ",
    nrow(cohortEco)
  )
  
  # Report cohorts without ecodistrict
  nUnmatchedEcoCohorts <- cohortEco[
    is.na(ecodistrict_id),
    .N
  ]
  
  message(
    "Cohort records without ecodistrict: ",
    nUnmatchedEcoCohorts
  )
  
    # ---------------------------------------------------------------------------
    # 6. Match cohorts to species-specific National RSYC curves
    # ---------------------------------------------------------------------------
    
  
  # ---------------------------------------------------------------------------
  # 6. Match cohorts to species-specific National RSYC curves
  # ---------------------------------------------------------------------------
  
  curveLookup <- data.table::as.data.table(
    data.table::copy(nationalResult$curveLookup)
  )
  
  speciesCurves <- curveLookup[
    model_group == "species",
    .(ecodistrict_id, species, curveID)
  ]
  
  # Each ecodistrict-species combination must have one curve
  if (anyDuplicated(
    speciesCurves[, .(ecodistrict_id, species)]
  )) {
    stop(
      "Duplicate species-specific curves for an ecodistrict-species combination."
    )
  }
  nCohortsBeforeCurves <- nrow(cohortEco)
    
    cohortCurves <- merge(
      cohortEco,
      speciesCurves,
      by = c("ecodistrict_id", "species"),
      all.x = TRUE,
      sort = FALSE
    )
    
    if (nrow(cohortCurves) != nCohortsBeforeCurves) {
      stop("Cohort record count changed during RSYC curve matching.")
    }
    
    message(
      "Cohort records with species-specific curves: ",
      sum(!is.na(cohortCurves$curveID)),
      " / ",
      nrow(cohortCurves)
    )
    # Validate cohort record counts
    message(
      "Record count validation: ",
      "cohortDT = ", nrow(cohortDT),
      ", cohortEco = ", nrow(cohortEco),
      ", cohortCurves = ", nrow(cohortCurves)
    )
    
    # ---------------------------------------------------------------------------
    # 7. Calculate species biomass within each pixelGroup
    # ---------------------------------------------------------------------------
    
    # Validate biomass values
    if (!is.numeric(cohortCurves$B)) {
      stop("Cohort biomass column B must be numeric.")
    }
    
    if (anyNA(cohortCurves$B) ||
        any(!is.finite(cohortCurves$B)) ||
        any(cohortCurves$B < 0)) {
      stop("Invalid biomass values in cohortData.")
    }
    if (anyNA(cohortCurves$speciesCode)) {
      stop("Missing original LandR speciesCode in cohortData.")
    }
    # Calculate biomass for each original LandR species
    speciesBiomass <- cohortCurves[
      ,
      .(
        speciesB = sum(B)
      ),
      by = .(pixelGroup, speciesCode)
    ]
    
    # Calculate total biomass for each pixelGroup
    totalBiomass <- speciesBiomass[
      ,
      .(
        totalB = sum(speciesB)
      ),
      by = pixelGroup
    ]
    
    # Attach total biomass to each species
    speciesBiomass <- merge(
      speciesBiomass,
      totalBiomass,
      by = "pixelGroup",
      all.x = TRUE,
      sort = FALSE
    )
    
    # Calculate species biomass proportions
    speciesBiomass[
      ,
      biomassShare := data.table::fifelse(
        totalB > 0,
        speciesB / totalB,
        NA_real_
      )
    ]
    
    message(
      "Species biomass calculated for ",
      data.table::uniqueN(speciesBiomass$pixelGroup),
      " pixel groups."
    )
    
    # ---------------------------------------------------------------------------
    # 8. Identify dominant LandR species using 70% biomass threshold
    # ---------------------------------------------------------------------------
    
    dominanceThreshold <- 0.70
    
    # Rank species by biomass within each pixelGroup
    data.table::setorder(
      speciesBiomass,
      pixelGroup,
      -speciesB,
      speciesCode
    )
    
    # Select the species with the highest biomass
    dominantSpecies <- speciesBiomass[
      ,
      .SD[1L],
      by = pixelGroup
    ]
    
    # Determine whether the dominant species meets the threshold
    dominantSpecies[
      ,
      meets70 := !is.na(biomassShare) &
        biomassShare >= dominanceThreshold
    ]
    
    # Assign preliminary classification status
    dominantSpecies[
      ,
      dominanceStatus := data.table::fifelse(
        is.na(biomassShare),
        "no_biomass",
        data.table::fifelse(
          meets70,
          "species_dominant",
          "mixed_stand"
        )
      )
    ]
    
    # Report results
    message(
      "Pixel groups with dominant species >= 70%: ",
      dominantSpecies[
        dominanceStatus == "species_dominant", .N
      ]
    )
    
    message(
      "Mixed pixel groups (< 70%): ",
      dominantSpecies[
        dominanceStatus == "mixed_stand", .N
      ]
    )
    
    message(
      "Pixel groups without positive biomass: ",
      dominantSpecies[
        dominanceStatus == "no_biomass", .N
      ]
    )
    
    # ---------------------------------------------------------------------------
    # 9. Inspect available National RSYC curve groups
    # ---------------------------------------------------------------------------
    
    availableModelGroups <- sort(
      unique(as.character(curveLookup$model_group))
    )
    
    message(
      "Available National RSYC model groups: ",
      paste(availableModelGroups, collapse = ", ")
    )
    
    # Count curves by model group
    curveGroupSummary <- curveLookup[
      ,
      .(
        nCurves = .N,
        nEcodistricts = data.table::uniqueN(ecodistrict_id)
      ),
      by = model_group
    ]
    
    data.table::setorder(curveGroupSummary, model_group)
    
    print(curveGroupSummary)
    
    # ---------------------------------------------------------------------------
    # 10. Match dominant species to National RSYC curves
    # ---------------------------------------------------------------------------
    
    # Preserve dominant LandR species and biomass information
    dominantCurveLookup <- dominantSpecies[
      ,
      .(
        pixelGroup,
        speciesCode,
        biomassShare,
        dominanceStatus
      )
    ]
    nDominantBefore <- nrow(dominantCurveLookup)
    # Attach the National RSYC species and ecodistrict
    dominantCurveLookup <- merge(
      dominantCurveLookup,
      unique(cohortEco[
        ,
        .(pixelGroup, speciesCode, species, ecodistrict_id)
      ]),
      by = c("pixelGroup", "speciesCode"),
      all.x = TRUE,
      sort = FALSE
    )
    
    if (anyDuplicated(dominantCurveLookup$pixelGroup)) {
      stop("Duplicate pixelGroup records in dominant curve lookup.")
    }
    
    # Attach available species-specific RSYC curves
    dominantCurveLookup <- merge(
      dominantCurveLookup,
      speciesCurves,
      by = c("ecodistrict_id", "species"),
      all.x = TRUE,
      sort = FALSE
    )
    
    # Species curves are eligible only when dominance >= 70%
    dominantCurveLookup[
      dominanceStatus != "species_dominant",
      curveID := NA_character_
    ]
    
    message(
      "Dominant pixel groups with species-specific curves: ",
      dominantCurveLookup[
        dominanceStatus == "species_dominant" &
          !is.na(curveID), .N
      ]
    )
    
    # ---------------------------------------------------------------------------
    # 11. Calculate coniferous and broadleaf biomass shares
    # ---------------------------------------------------------------------------
    
    # Define functional groups for the 28 National RSYC species
    coniferSpecies <- c(
      "ABIE.AMA", "ABIE.BAL", "ABIE.LAS",
      "LARI.OCC",
      "PICE.ABI", "PICE.ENG", "PICE.GLA",
      "PICE.MAR", "PICE.RUB", "PICE.SIT",
      "PINU.BAN", "PINU.CON", "PINU.PON",
      "PINU.RES", "PINU.STR",
      "PSEU.MEN",
      "THUJ.OCC", "THUJ.PLI",
      "TSUG.HET", "TSUG.MER"
    )
    
    broadleafSpecies <- c(
      "ACER.RUB", "ACER.SAH", "ALNU.RUB",
      "BETU.ALL", "BETU.PAP",
      "POPU.BAL", "POPU.GRA", "POPU.TRE"
    )
    
    functionalGroups <- data.table::data.table(
      species = c(coniferSpecies, broadleafSpecies),
      functionalGroup = c(
        rep("coniferous", length(coniferSpecies)),
        rep("broadleaf", length(broadleafSpecies))
      )
    )
    
    # Validate functional group definitions
    stopifnot(
      nrow(functionalGroups) == 28L,
      !anyDuplicated(functionalGroups$species),
      setequal(functionalGroups$species, nationalMapping$species)
    )
    
    # Connect LandR species codes to functional groups
    functionalMapping <- merge(
      nationalMapping[, .(LandR, species)],
      functionalGroups,
      by = "species",
      all.x = TRUE,
      sort = FALSE
    )
    
    if (anyNA(functionalMapping$functionalGroup)) {
      stop("Some LandR species have no functional group.")
    }
    # Experimental functional mapping for Larix laricina
    
    if (
      isTRUE(experimentalLarix) &&
      "Lari_lar" %in% speciesBiomass$speciesCode &&
      !("Lari_lar" %in% functionalMapping$LandR)
    ) {
      
      functionalMapping <- data.table::rbindlist(
        list(
          functionalMapping,
          data.table::data.table(
            species = NA_character_,
            LandR = "Lari_lar",
            functionalGroup = "coniferous"
          )
        ),
        use.names = TRUE
      )
      
      message(
        "EXPERIMENTAL: Lari_lar included ",
        "in the coniferous functional group."
      )
      
    }
    
    stopifnot(
      !anyDuplicated(functionalMapping$LandR)
    )
    # Attach functional groups to species biomass
    groupBiomass <- merge(
      speciesBiomass,
      functionalMapping[
        , .(speciesCode = LandR, functionalGroup)
      ],
      by = "speciesCode",
      all.x = TRUE,
      sort = FALSE
    )
    
    if (nrow(groupBiomass) != nrow(speciesBiomass)) {
      stop("Species biomass row count changed during mapping.")
    }
    
    if (anyNA(groupBiomass$functionalGroup)) {
      stop("Some species biomass records have no functional group.")
    }
    
    # Sum biomass by pixelGroup and functional group
    groupBiomassSummary <- groupBiomass[
      ,
      .(groupB = sum(speciesB)),
      by = .(pixelGroup, functionalGroup)
    ]
    
    # Calculate functional group biomass shares
    groupBiomassSummary <- merge(
      groupBiomassSummary,
      totalBiomass,
      by = "pixelGroup",
      all.x = TRUE,
      sort = FALSE
    )
    
    groupBiomassSummary[
      ,
      groupShare := data.table::fifelse(
        totalB > 0,
        groupB / totalB,
        NA_real_
      )
    ]
    
    # Report group coverage
    print(
      groupBiomassSummary[
        ,
        .(
          nPixelGroups = data.table::uniqueN(pixelGroup)
        ),
        by = functionalGroup
      ]
    )
    
    # ---------------------------------------------------------------------------
    # 12. Assign final National RSYC curve to each pixelGroup
    # ---------------------------------------------------------------------------
    
    # Preserve the results of species-specific classification
    finalCurveLookup <- data.table::copy(dominantCurveLookup)
    
    nPixelGroupsBefore <- nrow(finalCurveLookup)
    
    # Extract coniferous biomass shares
    coniferShares <- groupBiomassSummary[
      functionalGroup == "coniferous",
      .(pixelGroup, coniferShare = groupShare)
    ]
    
    # Extract broadleaf biomass shares
    broadleafShares <- groupBiomassSummary[
      functionalGroup == "broadleaf",
      .(pixelGroup, broadleafShare = groupShare)
    ]
    
    # Attach functional group shares
    finalCurveLookup <- merge(
      finalCurveLookup,
      coniferShares,
      by = "pixelGroup",
      all.x = TRUE,
      sort = FALSE
    )
    
    finalCurveLookup <- merge(
      finalCurveLookup,
      broadleafShares,
      by = "pixelGroup",
      all.x = TRUE,
      sort = FALSE
    )
    
    if (nrow(finalCurveLookup) != nPixelGroupsBefore ||
        anyDuplicated(finalCurveLookup$pixelGroup)) {
      stop("PixelGroup records changed during functional group matching.")
    }
    
    # Missing functional groups have zero share when total biomass is positive
    finalCurveLookup[
      !is.na(biomassShare) & is.na(coniferShare),
      coniferShare := 0
    ]
    
    finalCurveLookup[
      !is.na(biomassShare) & is.na(broadleafShare),
      broadleafShare := 0
    ]
    
    # Determine the required RSYC model group
    finalCurveLookup[
      ,
      selectedModelGroup := data.table::fifelse(
        dominanceStatus == "species_dominant",
        "species",
        data.table::fifelse(
          dominanceStatus == "no_biomass",
          NA_character_,
          data.table::fifelse(
            coniferShare >= dominanceThreshold,
            "coniferous",
            data.table::fifelse(
              broadleafShare >= dominanceThreshold,
              "broadleaf",
              "generic"
            )
          )
        )
      )
    ]
    
    # Prepare functional group and generic curve lookup
    groupCurves <- curveLookup[
      model_group %in% c("coniferous", "broadleaf", "generic"),
      .(
        ecodistrict_id,
        selectedModelGroup = model_group,
        groupCurveID = curveID
      )
    ]
    
    # Validate unique group curves per ecodistrict
    if (anyDuplicated(
      groupCurves[, .(ecodistrict_id, selectedModelGroup)]
    )) {
      stop("Duplicate functional group curves within an ecodistrict.")
    }
    
    # Attach functional group or generic curves
    finalCurveLookup <- merge(
      finalCurveLookup,
      groupCurves,
      by = c("ecodistrict_id", "selectedModelGroup"),
      all.x = TRUE,
      sort = FALSE
    )
    
    if (nrow(finalCurveLookup) != nPixelGroupsBefore ||
        anyDuplicated(finalCurveLookup$pixelGroup)) {
      stop("PixelGroup records changed during final curve matching.")
    }
    
    # Assign group curves only to mixed stands
    finalCurveLookup[
      dominanceStatus == "mixed_stand",
      curveID := groupCurveID
    ]
    
    # Keep species-specific curves for species-dominant stands
    # Keep missing curves as NA for unresolved cases
    
    # Identify classification results
    finalCurveLookup[
      ,
      classificationResult := data.table::fifelse(
        is.na(selectedModelGroup),
        "no_biomass",
        data.table::fifelse(
          is.na(curveID),
          "missing_curve",
          "assigned"
        )
      )
    ]
    # -------------------------------------------------------------------------
    # 12.1 Experimental fallback for missing species-specific curves
    # -------------------------------------------------------------------------
    
    finalCurveLookup[
      ,
      `:=`(
        originalCurveID = curveID,
        originalModelGroup = selectedModelGroup,
        originalClassificationResult = classificationResult,
        fallbackApplied = FALSE
      )
    ]
    
    if (isTRUE(experimentalFallback)) {
      
      # Identify eligible unresolved pixel groups
      
      fallbackCandidates <- data.table::copy(
        finalCurveLookup[
          classificationResult == "missing_curve" &
            selectedModelGroup == "species" &
            !is.na(ecodistrict_id) &
            (
              !is.na(species) |
                (
                  isTRUE(experimentalLarix) &
                    speciesCode == "Lari_lar"
                )
            )
        ]
      )
      
      # Determine fallback model group using biomass shares
      
      fallbackCandidates[
        ,
        fallbackModelGroup := data.table::fifelse(
          coniferShare >= dominanceThreshold,
          "coniferous",
          data.table::fifelse(
            broadleafShare >= dominanceThreshold,
            "broadleaf",
            "generic"
          )
        )
      ]
      
      # Prepare fallback curve lookup
      
      fallbackCurves <- curveLookup[
        model_group %in% c(
          "coniferous",
          "broadleaf",
          "generic"
        ),
        .(
          ecodistrict_id,
          fallbackModelGroup = model_group,
          fallbackCurveID = curveID
        )
      ]
      
      stopifnot(
        !anyDuplicated(
          fallbackCurves[
            , .(ecodistrict_id, fallbackModelGroup)
          ]
        )
      )
      
      # Match fallback candidates to available curves
      
      fallbackCandidates <- merge(
        fallbackCandidates,
        fallbackCurves,
        by = c(
          "ecodistrict_id",
          "fallbackModelGroup"
        ),
        all.x = TRUE,
        sort = FALSE
      )
      
      # Retain only available fallback curves
      
      combinedFallback <- fallbackCandidates[
        !is.na(fallbackCurveID),
        .(
          pixelGroup,
          fallbackModelGroup,
          fallbackCurveID
        )
      ]
      
      # Validate fallback assignments
      
      stopifnot(
        !anyDuplicated(combinedFallback$pixelGroup),
        !anyNA(combinedFallback$fallbackCurveID),
        all(
          combinedFallback$pixelGroup %in%
            finalCurveLookup[
              classificationResult == "missing_curve",
              pixelGroup
            ]
        )
      )
      
      # Apply fallback assignments
      
      finalCurveLookup[
        combinedFallback,
        on = "pixelGroup",
        `:=`(
          curveID = i.fallbackCurveID,
          selectedModelGroup = i.fallbackModelGroup,
          classificationResult = "assigned",
          fallbackApplied = TRUE
        )
      ]
      
      # Ensure previously assigned curves remain unchanged
      
      stopifnot(
        finalCurveLookup[
          originalClassificationResult == "assigned",
          all(
            curveID == originalCurveID &
              selectedModelGroup == originalModelGroup &
              classificationResult == originalClassificationResult &
              !fallbackApplied
          )
        ],
        nrow(finalCurveLookup) == nPixelGroupsBefore,
        !anyDuplicated(finalCurveLookup$pixelGroup)
      )
      
      message(
        "EXPERIMENTAL: Fallback applied to ",
        sum(finalCurveLookup$fallbackApplied),
        " pixel groups."
      )
      
    }
    
    # Report selected model groups
    message("National RSYC classification summary:")
    
    print(
      finalCurveLookup[
        ,
        .(
          nPixelGroups = .N,
          nAssigned = sum(!is.na(curveID)),
          nMissing = sum(is.na(curveID))
        ),
        by = .(selectedModelGroup, classificationResult)
      ][order(selectedModelGroup, classificationResult)]
    )
    
    # Validate final number of pixel groups
    stopifnot(
      nrow(finalCurveLookup) == nPixelGroupsBefore,
      !anyDuplicated(finalCurveLookup$pixelGroup)
    )
    
    message(
      "Pixel groups assigned a National RSYC curve: ",
      sum(!is.na(finalCurveLookup$curveID)),
      " / ",
      nrow(finalCurveLookup)
    )
    
    # ---------------------------------------------------------------------------
    # 13. Validate and return National RSYC classification results
    # ---------------------------------------------------------------------------
    
    # Validate required output columns
    requiredOutputCols <- c(
      "pixelGroup",
      "ecodistrict_id",
      "curveID",
      "selectedModelGroup",
      "classificationResult"
    )
    
    if (!all(requiredOutputCols %in% names(finalCurveLookup))) {
      stop("Missing required columns in finalCurveLookup.")
    }
    
    # Ensure each pixelGroup has exactly one classification record
    if (anyNA(finalCurveLookup$pixelGroup)) {
      stop("Missing pixelGroup IDs in final classification.")
    }
    
    if (anyDuplicated(finalCurveLookup$pixelGroup)) {
      stop("Duplicate pixelGroup IDs in final classification.")
    }
    
    # Check consistency between curve assignment and status
    if (any(
      finalCurveLookup$classificationResult == "assigned" &
      is.na(finalCurveLookup$curveID)
    )) {
      stop("Assigned pixel groups contain missing curveIDs.")
    }
    
    if (any(
      finalCurveLookup$classificationResult != "assigned" &
      !is.na(finalCurveLookup$curveID)
    )) {
      stop("Unresolved pixel groups contain assigned curveIDs.")
    }
    
    # Prepare pixelGroup-to-curve lookup
    pixelGroupCurveLookup <- data.table::copy(
      finalCurveLookup[
        ,
        .(
          pixelGroup,
          ecodistrict_id,
          curveID,
          selectedModelGroup,
          classificationResult
        )
      ]
    )
    
    data.table::setorder(
      pixelGroupCurveLookup,
      pixelGroup
    )
    
    # Summarize classification results
    classificationSummary <- pixelGroupCurveLookup[
      ,
      .(nPixelGroups = .N),
      by = .(selectedModelGroup, classificationResult)
    ]
    
    data.table::setorder(
      classificationSummary,
      selectedModelGroup,
      classificationResult
    )
    
    message("Final National RSYC classification summary:")
    print(classificationSummary)
    
    # Report unresolved pixel groups
    nUnresolved <- pixelGroupCurveLookup[
      classificationResult != "assigned",
      .N
    ]
    
    message(
      "Unresolved pixel groups: ",
      nUnresolved
    )
    # -------------------------------------------------------------------------
    # 14. Build standardized Analysis Units
    # -------------------------------------------------------------------------
    
    # Identify successfully assigned RSYC curves
    
    usedCurveIDs <- unique(
      finalCurveLookup[
        classificationResult == "assigned",
        curveID
      ]
    )
    
    # Extract the corresponding national curve definitions
    
    auDefinition <- data.table::copy(
      curveLookup[
        curveID %in% usedCurveIDs
      ]
    )
    
    # Define Analysis Units using species and ecodistrict
    
    auDefinition[
      ,
      AU := paste0(species, "_", ecodistrict_id)
    ]
    
    # Validate Analysis Unit definitions
    
    stopifnot(
      !anyNA(auDefinition$species),
      !anyNA(auDefinition$ecodistrict_id),
      !anyNA(auDefinition$AU),
      !anyNA(auDefinition$curveID),
      !anyDuplicated(auDefinition$AU),
      !anyDuplicated(auDefinition$curveID),
      setequal(
        auDefinition$curveID,
        usedCurveIDs
      )
    )
    
    # Build standardized AU-to-curve lookup
    
    sim$AUtoCurve <- unique(
      auDefinition[, .(AU, curveID)]
    )
    
    data.table::setorder(
      sim$AUtoCurve,
      AU
    )
    
    # -------------------------------------------------------------------------
    # 15. Assign Analysis Units to pixel groups
    # -------------------------------------------------------------------------
    
    pixelAU <- merge(
      pixelGroupCurveLookup,
      sim$AUtoCurve,
      by = "curveID",
      all.x = TRUE,
      sort = FALSE
    )
    
    sim$pixelGroupToAU <- pixelAU[
      ,
      .(
        pixelGroup,
        analysisUnit = AU
      )
    ]
    
    data.table::setorder(
      sim$pixelGroupToAU,
      pixelGroup
    )
    
    # Validate pixelGroup-to-AU assignment
    
    stopifnot(
      nrow(sim$pixelGroupToAU) ==
        nrow(pixelGroupCurveLookup),
      
      !anyDuplicated(
        sim$pixelGroupToAU$pixelGroup
      ),
      
      all(
        is.na(pixelAU$AU) ==
          is.na(pixelAU$curveID)
      )
    )
    
    message(
      "Number of Analysis Units: ",
      nrow(sim$AUtoCurve)
    )
    
    message(
      "Pixel groups assigned to Analysis Units: ",
      sum(!is.na(sim$pixelGroupToAU$analysisUnit)),
      " / ",
      nrow(sim$pixelGroupToAU)
    )
    # -------------------------------------------------------------------------
    # 16. Build standardized classification table
    # -------------------------------------------------------------------------
    
    sim$classification <- pixelAU[
      ,
      .(
        pixelGroup,
        AU,
        curveID
      )
    ]
    
    data.table::setorder(
      sim$classification,
      pixelGroup
    )
    
    # Validate classification table
    
    stopifnot(
      nrow(sim$classification) ==
        nrow(pixelGroupCurveLookup),
      
      !anyDuplicated(
        sim$classification$pixelGroup
      ),
      
      all(
        is.na(sim$classification$AU) ==
          is.na(sim$classification$curveID)
      )
    )
    
    message("PASS: Classification table validated.")
    
    # -------------------------------------------------------------------------
    # 17. Build stand table for AAC calculations
    # -------------------------------------------------------------------------
    
    # Temporary stand age definition:
    # Use the modal cohort age, consistent with Ontario.
    
    standDT <- cohortDT[
      ,
      .(
        age = as.numeric(
          names(which.max(table(age)))
        )
      ),
      by = pixelGroup
    ]
    
    # Attach Analysis Units and yield curves
    
    standDT <- merge(
      standDT,
      sim$classification[
        ,
        .(
          pixelGroup,
          AU,
          curveID
        )
      ],
      by = "pixelGroup",
      all.x = TRUE
    )
    
    # Retain successfully classified stands
    
    standDT <- standDT[
      !is.na(AU) &
        !is.na(curveID)
    ]
    
    # Validate stand table
    
    stopifnot(
      !anyDuplicated(standDT$pixelGroup),
      !anyNA(standDT$age),
      nrow(standDT) ==
        sum(!is.na(sim$classification$AU))
    )
    
    sim$standDT <- standDT
    
    message(
      "Number of classified stands: ",
      nrow(sim$standDT)
    )
    
    # -------------------------------------------------------------------------
    # 18. Build Analysis Unit Map
    # -------------------------------------------------------------------------
    
    analysisUnitMap <- sim$pixelGroupMap
    
    lookup <- sim$pixelGroupToAU
    
    pg_vals <- terra::values(
      sim$pixelGroupMap
    )
    
    idx <- match(
      pg_vals,
      lookup$pixelGroup
    )
    
    terra::values(analysisUnitMap) <-
      lookup$analysisUnit[idx]
    
    sim$analysisUnitMap <- analysisUnitMap
    
    # Validate spatial alignment
    
    stopifnot(
      terra::compareGeom(
        sim$pixelGroupMap,
        sim$analysisUnitMap,
        stopOnError = FALSE
      )
    )
    
    message("PASS: Analysis Unit Map created.")
    # -------------------------------------------------------------------------
    # 19. Calculate effective area and Analysis Unit summary
    # -------------------------------------------------------------------------
    
    if (!is.null(sim$harvestableFraction)) {
      
      # Check spatial alignment
      
      if (!terra::compareGeom(
        sim$pixelGroupMap,
        sim$harvestableFraction,
        stopOnError = FALSE
      )) {
        
        stop(
          "pixelGroupMap and harvestableFraction are not aligned. ",
          "Effective area cannot be calculated."
        )
        
      }
      
      # Cell area in hectares
      
      cellArea_ha <- prod(
        terra::res(sim$pixelGroupMap)
      ) / 10000
      
      # Extract raster values
      
      hf <- as.vector(
        terra::values(sim$harvestableFraction)
      )
      
      pg <- as.vector(
        terra::values(sim$pixelGroupMap)
      )
      
      # Build pixel-level area table
      
      pixel_area_dt <- data.table::data.table(
        pixelGroup = pg,
        harvestableFraction = hf
      )
      
      pixel_area_dt <- pixel_area_dt[
        pixelGroup > 0
      ]
      
      # Aggregate effective area by pixelGroup
      
      pixel_area_dt <- pixel_area_dt[
        ,
        .(
          effectiveArea = sum(
            harvestableFraction * cellArea_ha,
            na.rm = TRUE
          )
        ),
        by = pixelGroup
      ]
      
      # Attach Analysis Units
      
      pixel_area_dt <- merge(
        pixel_area_dt,
        sim$pixelGroupToAU,
        by = "pixelGroup",
        all.x = TRUE
      )
      
      pixel_area_dt <- pixel_area_dt[
        !is.na(analysisUnit)
      ]
      
      sim$pixelAreaDT <- pixel_area_dt
      
      # Add effective area to standDT
      
      standDT <- merge(
        standDT,
        sim$pixelAreaDT[
          ,
          .(
            pixelGroup,
            effectiveArea
          )
        ],
        by = "pixelGroup",
        all.x = TRUE
      )
      
      sim$standDT <- standDT
      
      # Calculate effective area by Analysis Unit
      
      sim$areaByAU <- sim$pixelAreaDT[
        !is.na(analysisUnit),
        .(
          nPixelGroups = .N,
          effectiveArea = sum(
            effectiveArea,
            na.rm = TRUE
          )
        ),
        by = .(AU = analysisUnit)
      ]
      
      # Build Analysis Unit summary
      
      sim$analysisUnitSummary <- merge(
        sim$areaByAU,
        sim$AUtoCurve,
        by = "AU",
        all.x = TRUE
      )
      
      sim$analysisUnitSummary[
        ,
        percentArea :=
          100 * effectiveArea /
          sum(effectiveArea, na.rm = TRUE)
      ]
      
      data.table::setorder(
        sim$analysisUnitSummary,
        -effectiveArea
      )
      
      message("PASS: Effective area calculated.")
      
    } else {
      
      message(
        "SKIPPED: Effective area calculations ",
        "will be completed during the main run."
      )
      
    }
    sim$pixelGroupCurveLookup <- pixelGroupCurveLookup
    sim$classificationSummary <- classificationSummary
    sim$finalCurveLookup <- finalCurveLookup
    sim$curveLookup <- curveLookup
    sim$nationalResult <- nationalResult
    
    return(sim)

    
}
