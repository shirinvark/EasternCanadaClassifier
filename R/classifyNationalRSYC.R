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
    # Return classification results and supporting data
    return(
      list(
        pixelGroupCurveLookup = pixelGroupCurveLookup,
        classificationSummary = classificationSummary,
        finalCurveLookup = finalCurveLookup,
        curveLookup = curveLookup,
        nationalResult = nationalResult
      )
    )
    
}
# Check consistency with cohortData
cohortPG <- unique(sim$cohortData$pixelGroup)

length(setdiff(
  cohortPG,
  pixelGroupEco$pixelGroup
))

setdiff(
  pixelGroupEco$pixelGroup,
  cohortPG
)

# Inspect unmatched pixelGroups
unmatchedPG <- pixelGroupEco[
  pixelGroup > 0 & is.na(ecodistrict_id)
]

summary(unmatchedPG$nCells)

sum(unmatchedPG$nCells)

# Inspect multiple-ecodistrict pixelGroups
multiEcoPG <- pixelGroupEco[
  pixelGroup > 0 & nEcodistricts > 1
]

# Dominant ecodistrict share
summary(
  multiEcoPG$nMatchedCells /
    multiEcoPG$nCells
)

# Count low-dominance assignments
multiEcoPG[
  ,
  .(
    total = .N,
    below50 = sum(nMatchedCells / nCells < 0.50),
    equal50 = sum(nMatchedCells / nCells == 0.50),
    above50 = sum(nMatchedCells / nCells > 0.50)
  )
]

# Inspect the 10 lowest-dominance assignments
multiEcoPG[
  order(nMatchedCells / nCells)
][1:10]


# Identify unmatched pixel groups
unmatchedPG <- pixelGroupEco[
  pixelGroup > 0 & is.na(ecodistrict_id)
]

# Number of unmatched pixel groups
nrow(unmatchedPG)

# Distribution of cells per unmatched pixel group
summary(unmatchedPG$nCells)

# Total cells in unmatched pixel groups
sum(unmatchedPG$nCells)

# Nominal area of each raster cell (ha)
cellAreaHa <- prod(
  terra::res(sim$pixelGroupMap)
) / 10000

cellAreaHa

# Total nominal area of unmatched pixel groups (ha)
sum(unmatchedPG$nCells) * cellAreaHa

# Percentage of valid LandR raster area
100 * sum(unmatchedPG$nCells) /
  sum(pixelGroupEco[pixelGroup > 0, nCells])

# Extract affected cohorts
unmatchedCohorts <- sim$cohortData[
  pixelGroup %in% unmatchedPG$pixelGroup
]

# Number of affected cohorts
nrow(unmatchedCohorts)

# Percentage of affected cohorts
100 * nrow(unmatchedCohorts) /
  nrow(sim$cohortData)

# Species composition of affected cohorts
unmatchedCohorts[
  ,
  .(nCohorts = .N),
  by = speciesCode
][order(-nCohorts)]

# Check consistency between cohort and lookup IDs
cohortPG <- unique(sim$cohortData$pixelGroup)

length(setdiff(cohortPG, pixelGroupEco$pixelGroup))

setdiff(pixelGroupEco$pixelGroup, cohortPG)


# Create a working copy of cohortData
cohortDT <- data.table::as.data.table(
  data.table::copy(sim$cohortData)
)

# Identify pixelGroup column
pgCol <- grep(
  "^pixelgroup$",
  names(cohortDT),
  ignore.case = TRUE,
  value = TRUE
)

stopifnot(length(pgCol) == 1L)

if (pgCol != "pixelGroup") {
  data.table::setnames(
    cohortDT,
    pgCol,
    "pixelGroup"
  )
}

# Validate required columns
requiredCols <- c(
  "pixelGroup",
  "speciesCode",
  "age",
  "B"
)

setdiff(requiredCols, names(cohortDT))

# Standardize species code type
cohortDT[, speciesCode := as.character(speciesCode)]

# Inspect cohort data
dim(cohortDT)

str(cohortDT)

# Check missing values
cohortDT[
  ,
  lapply(.SD, function(x) sum(is.na(x))),
  .SDcols = requiredCols
]

# Check duplicate mapping keys
anyDuplicated(nationalMapping$LandR)


# Standardize national mapping
nationalMapping <- data.table::as.data.table(
  data.table::copy(nationalMapping)
)

nationalMapping[, LandR := as.character(LandR)]
nationalMapping[, species := as.character(species)]

# Validate unique mapping keys
if (anyDuplicated(nationalMapping$LandR)) {
  stop("Duplicate LandR species codes in National mapping.")
}

# Preserve original cohort record count
nCohortsBefore <- nrow(cohortDT)

if ("species" %in% names(cohortDT)) {
  stop("cohortData already contains a species column; inspect before mapping.")
}

# Attach National RSYC species
cohortDT <- merge(
  cohortDT,
  nationalMapping,
  by.x = "speciesCode",
  by.y = "LandR",
  all.x = TRUE,
  sort = FALSE
)

# Validate record count
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


# Inspect output
dim(cohortDT)

names(cohortDT)

# Summarize mapping by species
cohortDT[
  ,
  .(
    nCohorts = .N,
    nPixelGroups = uniqueN(pixelGroup)
  ),
  by = .(speciesCode, species)
][order(speciesCode)]

# Overall mapping coverage
cohortDT[
  ,
  .(
    totalCohorts = .N,
    mappedCohorts = sum(!is.na(species)),
    unmappedCohorts = sum(is.na(species)),
    mappingPercentage = 100 * mean(!is.na(species))
  )
]

# Validate unique pixelGroup IDs in spatial lookup
if (anyDuplicated(pixelGroupEco$pixelGroup)) {
  stop("Duplicate pixelGroup IDs in pixelGroupEco.")
}

# Preserve original cohort record count
nCohortsBeforeEco <- nrow(cohortDT)

# Attach ecodistrict IDs
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

# Create a working copy of the national curve lookup
curveLookup <- data.table::as.data.table(
  data.table::copy(nationalResult$curveLookup)
)

# Extract species-specific curves
speciesCurves <- curveLookup[
  model_group == "species",
  .(ecodistrict_id, species, curveID)
]

# Validate uniqueness of ecodistrict-species combinations
if (anyDuplicated(
  speciesCurves[, .(ecodistrict_id, species)]
)) {
  stop(
    "Duplicate species-specific curves for an ecodistrict-species combination."
  )
}

# Inspect species-specific curves
dim(speciesCurves)

head(speciesCurves)

str(speciesCurves)

# Check duplicate combinations
anyDuplicated(
  speciesCurves[, .(ecodistrict_id, species)]
)

# Check missing values
speciesCurves[
  ,
  lapply(.SD, function(x) sum(is.na(x)))
]

# Number of available curves by species
speciesCurves[
  ,
  .(
    nCurves = .N,
    nEcodistricts = uniqueN(ecodistrict_id)
  ),
  by = species
][order(species)]

# Extract Quebec ecodistrict IDs
qcEcoIDs <- unique(
  cohortEco[!is.na(ecodistrict_id), ecodistrict_id]
)

# Inspect species-specific curves available
# within Quebec LandR ecodistricts
speciesCurves[
  ecodistrict_id %in% qcEcoIDs,
  .(
    nCurves = .N,
    nEcodistricts = uniqueN(ecodistrict_id)
  ),
  by = species
][order(species)]
# Preserve original cohort record count
nCohortsBeforeCurves <- nrow(cohortEco)

# Attach species-specific RSYC curves
cohortCurves <- merge(
  cohortEco,
  speciesCurves,
  by = c("ecodistrict_id", "species"),
  all.x = TRUE,
  sort = FALSE
)

# Validate record count
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


# Confirm reasons for missing curves
cohortCurves[
  is.na(curveID),
  .(
    missingBoth = sum(
      is.na(species) & is.na(ecodistrict_id)
    ),
    missingSpeciesOnly = sum(
      is.na(species) & !is.na(ecodistrict_id)
    ),
    missingEcodistrictOnly = sum(
      !is.na(species) & is.na(ecodistrict_id)
    ),
    missingSpeciesSpecificCurve = sum(
      !is.na(species) & !is.na(ecodistrict_id)
    )
  )
]

# Curve matching coverage by species
cohortCurves[
  ,
  .(
    totalCohorts = .N,
    matchedCurves = sum(!is.na(curveID)),
    unmatchedCurves = sum(is.na(curveID)),
    matchingPercentage = round(
      100 * mean(!is.na(curveID)), 4
    )
  ),
  by = .(speciesCode, species)
][order(speciesCode)]

# Missing curves despite valid species and ecodistrict
missingCurveSummary <- cohortCurves[
  is.na(curveID) &
    !is.na(species) &
    !is.na(ecodistrict_id),
  .(
    nCohorts = .N,
    nPixelGroups = uniqueN(pixelGroup)
  ),
  by = .(speciesCode, species, ecodistrict_id)
][order(-nCohorts)]

# Number of missing species-ecodistrict combinations
nrow(missingCurveSummary)

# Inspect the 20 most affected combinations
head(missingCurveSummary, 20)
sum(missingCurveSummary$nCohorts)

missingCurveSummary[
  ,
  .(
    nMissingCombinations = .N,
    nAffectedCohorts = sum(nCohorts)
  ),
  by = speciesCode
][order(-nAffectedCohorts)]
dim(speciesBiomass)

head(speciesBiomass)

uniqueN(speciesBiomass$pixelGroup)

uniqueN(speciesBiomass$speciesCode)

summary(speciesBiomass$speciesB)

# Biomass conservation
sum(cohortCurves$B)

sum(speciesBiomass$speciesB)

sum(cohortCurves$B) - sum(speciesBiomass$speciesB)

# Check Larix biomass
speciesBiomass[
  speciesCode == "Lari_lar",
  .(
    nPixelGroups = .N,
    sumB = sum(speciesB)
  )
]
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
# Inspect output
dim(speciesBiomass)

head(speciesBiomass)

names(speciesBiomass)

# Check unique pixelGroups and species
uniqueN(speciesBiomass$pixelGroup)

uniqueN(speciesBiomass$speciesCode)

# Check duplicate combinations
anyDuplicated(
  speciesBiomass[, .(pixelGroup, speciesCode)]
)

# Biomass distribution
summary(speciesBiomass$speciesB)

# Biomass conservation
sum(cohortCurves$B)

sum(speciesBiomass$speciesB)

sum(cohortCurves$B) - sum(speciesBiomass$speciesB)

# Check Lari_lar biomass preservation
speciesBiomass[
  speciesCode == "Lari_lar",
  .(
    nPixelGroups = .N,
    sumB = sum(speciesB)
  )
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
# Inspect total biomass
dim(totalBiomass)

head(totalBiomass)

summary(totalBiomass$totalB)

# Check unique pixelGroups
anyDuplicated(totalBiomass$pixelGroup)

# Check biomass conservation
sum(totalBiomass$totalB)

sum(totalBiomass$totalB) - sum(cohortCurves$B)

# Check species biomass table
dim(speciesBiomass)

head(speciesBiomass)

# Inspect biomass shares
summary(speciesBiomass$biomassShare)

# Check missing and invalid shares
speciesBiomass[
  ,
  .(
    missingShares = sum(is.na(biomassShare)),
    negativeShares = sum(biomassShare < 0, na.rm = TRUE),
    sharesAboveOne = sum(biomassShare > 1, na.rm = TRUE)
  )
]

# Check pixelGroups with zero biomass
totalBiomass[totalB == 0, .N]

# Validate that biomass shares sum to one
shareCheck <- speciesBiomass[
  ,
  .(
    sumShare = sum(biomassShare)
  ),
  by = pixelGroup
]

summary(shareCheck$sumShare)

# Check deviations from one
shareCheck[
  abs(sumShare - 1) > 1e-10,
  .N
]

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
# Validate dimensions
dim(dominantSpecies)

# Check duplicate pixelGroups
anyDuplicated(dominantSpecies$pixelGroup)

# Inspect dominant biomass shares
summary(dominantSpecies$biomassShare)

# Classification summary
dominantSpecies[
  ,
  .(nPixelGroups = .N),
  by = dominanceStatus
]

# Distribution of dominant species
dominantSpecies[
  dominanceStatus == "species_dominant",
  .(
    nPixelGroups = .N
  ),
  by = speciesCode
][order(-nPixelGroups)]

# Check Larix dominance
dominantSpecies[
  speciesCode == "Lari_lar",
  .(
    nPixelGroups = .N
  ),
  by = dominanceStatus
]

# Check pixelGroup conservation
nrow(dominantSpecies) -
  uniqueN(speciesBiomass$pixelGroup)
# Inspect available National RSYC curve groups

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
# Identify Quebec ecodistricts represented in LandR
qcEcoIDs <- unique(
  cohortEco[
    !is.na(ecodistrict_id),
    ecodistrict_id
  ]
)

length(qcEcoIDs)

# Summarize curve availability in Quebec
qcCurveGroupSummary <- curveLookup[
  ecodistrict_id %in% qcEcoIDs,
  .(
    nCurves = .N,
    nEcodistricts = uniqueN(ecodistrict_id)
  ),
  by = model_group
][order(model_group)]

print(qcCurveGroupSummary)

# Identify Quebec ecodistricts without broadleaf curves
missingBroadleafEco <- setdiff(
  qcEcoIDs,
  curveLookup[
    model_group == "broadleaf",
    unique(ecodistrict_id)
  ]
)

length(missingBroadleafEco)

print(missingBroadleafEco)

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
# Validate pixelGroup conservation
nrow(dominantCurveLookup) - nDominantBefore

anyDuplicated(dominantCurveLookup$pixelGroup)

# Extract unresolved dominant pixelGroups
missingDominantCurves <- dominantCurveLookup[
  dominanceStatus == "species_dominant" &
    is.na(curveID)
]

nrow(missingDominantCurves)

# Identify reasons for missing curves
missingDominantCurves[
  ,
  .(
    missingBoth = sum(
      is.na(species) & is.na(ecodistrict_id)
    ),
    missingSpeciesOnly = sum(
      is.na(species) & !is.na(ecodistrict_id)
    ),
    missingEcodistrictOnly = sum(
      !is.na(species) & is.na(ecodistrict_id)
    ),
    missingSpeciesSpecificCurve = sum(
      !is.na(species) & !is.na(ecodistrict_id)
    )
  )
]

# Missing curves by dominant species
missingDominantCurves[
  ,
  .(nPixelGroups = .N),
  by = speciesCode
][order(-nPixelGroups)]

# Missing curves by species and ecodistrict
missingDominantCurves[
  ,
  .(nPixelGroups = .N),
  by = .(speciesCode, species, ecodistrict_id)
][order(-nPixelGroups)]

# Confirm mixed stands have no species curves
dominantCurveLookup[
  dominanceStatus == "mixed_stand" &
    !is.na(curveID),
  .N
]
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
# Inspect functional groups
print(functionalGroups)

functionalGroups[
  ,
  .(nSpecies = .N),
  by = functionalGroup
]

# Inspect functional mapping
print(functionalMapping)

# Identify LandR species without functional mapping
unmappedFunctionalSpecies <- setdiff(
  unique(speciesBiomass$speciesCode),
  functionalMapping$LandR
)

print(unmappedFunctionalSpecies)

# Quantify affected biomass records
speciesBiomass[
  speciesCode %in% unmappedFunctionalSpecies,
  .(
    nRecords = .N,
    nPixelGroups = uniqueN(pixelGroup),
    totalSpeciesB = sum(speciesB)
  ),
  by = speciesCode
]

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
# Preserve original functional mapping
functionalMapping_test <- data.table::copy(
  functionalMapping
)

# Add Larix laricina to the coniferous functional group
functionalMapping_test <- data.table::rbindlist(
  list(
    functionalMapping_test,
    data.table::data.table(
      species = NA_character_,
      LandR = "Lari_lar",
      functionalGroup = "coniferous"
    )
  ),
  use.names = TRUE
)

# Inspect the new record
functionalMapping_test[
  LandR == "Lari_lar"
]

# Check duplicate LandR species
anyDuplicated(functionalMapping_test$LandR)

# Check functional mapping coverage
setdiff(
  unique(speciesBiomass$speciesCode),
  functionalMapping_test$LandR
)
# Attach functional groups using the test mapping
groupBiomass_test <- merge(
  speciesBiomass,
  functionalMapping_test[
    , .(speciesCode = LandR, functionalGroup)
  ],
  by = "speciesCode",
  all.x = TRUE,
  sort = FALSE
)

# Validate record count
if (nrow(groupBiomass_test) != nrow(speciesBiomass)) {
  stop("Species biomass row count changed during mapping.")
}

# Validate functional mapping
if (anyNA(groupBiomass_test$functionalGroup)) {
  stop("Some species biomass records have no functional group.")
}

# Check dimensions
dim(groupBiomass_test)

# Check missing functional groups
sum(is.na(groupBiomass_test$functionalGroup))

# Check record conservation
nrow(groupBiomass_test) - nrow(speciesBiomass)

# Check biomass conservation
sum(groupBiomass_test$speciesB) -
  sum(speciesBiomass$speciesB)

# Check duplicate combinations
anyDuplicated(
  groupBiomass_test[, .(pixelGroup, speciesCode)]
)

# Validate Larix functional classification
groupBiomass_test[
  speciesCode == "Lari_lar",
  .(
    nRecords = .N,
    nPixelGroups = uniqueN(pixelGroup),
    totalSpeciesB = sum(speciesB)
  ),
  by = .(speciesCode, functionalGroup)
]
# Sum biomass by pixelGroup and functional group
groupBiomassSummary <- groupBiomass_test[
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
# Check dimensions
dim(groupBiomassSummary)

# Check unique pixelGroups
uniqueN(groupBiomassSummary$pixelGroup)

# Check duplicate combinations
anyDuplicated(
  groupBiomassSummary[
    , .(pixelGroup, functionalGroup)
  ]
)

# Check biomass conservation
sum(groupBiomassSummary$groupB) -
  sum(speciesBiomass$speciesB)

# Check missing shares
sum(is.na(groupBiomassSummary$groupShare))

# Validate functional group biomass shares
groupShareCheck <- groupBiomassSummary[
  ,
  .(sumShare = sum(groupShare)),
  by = pixelGroup
]

summary(groupShareCheck$sumShare)

groupShareCheck[
  abs(sumShare - 1) > 1e-10,
  .N
]

# Check functional group composition
groupBiomassSummary[
  ,
  .(nFunctionalGroups = .N),
  by = pixelGroup
][
  ,
  .(nPixelGroups = .N),
  by = nFunctionalGroups
][order(nFunctionalGroups)]
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

# Inspect output
dim(finalCurveLookup)

head(finalCurveLookup)

# Check pixelGroup conservation
nrow(finalCurveLookup) - nPixelGroupsBefore

# Check duplicates
anyDuplicated(finalCurveLookup$pixelGroup)

# Check missing shares
sum(is.na(finalCurveLookup$coniferShare))

sum(is.na(finalCurveLookup$broadleafShare))

# Check share distributions
summary(finalCurveLookup$coniferShare)

summary(finalCurveLookup$broadleafShare)

# Validate total functional group share
finalCurveLookup[
  abs(coniferShare + broadleafShare - 1) > 1e-10,
  .N
]

# Check pure functional groups
finalCurveLookup[
  ,
  .(
    coniferOnly = sum(coniferShare == 1),
    broadleafOnly = sum(broadleafShare == 1),
    bothGroups = sum(
      coniferShare > 0 & broadleafShare > 0
    )
  )
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

# Check dimensions
dim(finalCurveLookup)

# Summarize selected model groups
modelGroupSummary <- finalCurveLookup[
  ,
  .(nPixelGroups = .N),
  by = selectedModelGroup
]

modelGroupSummary[
  ,
  percentage := 100 * nPixelGroups /
    sum(nPixelGroups)
]

print(modelGroupSummary[order(-nPixelGroups)])

# Check classification by dominance status
finalCurveLookup[
  ,
  .(nPixelGroups = .N),
  by = .(dominanceStatus, selectedModelGroup)
][order(dominanceStatus, selectedModelGroup)]

# Check missing model groups
sum(is.na(finalCurveLookup$selectedModelGroup))

# Validate species-dominant classification
finalCurveLookup[
  dominanceStatus == "species_dominant" &
    selectedModelGroup != "species",
  .N
]

# Validate coniferous classification
finalCurveLookup[
  selectedModelGroup == "coniferous" &
    coniferShare < dominanceThreshold,
  .N
]

# Validate broadleaf classification
finalCurveLookup[
  selectedModelGroup == "broadleaf" &
    broadleafShare < dominanceThreshold,
  .N
]

# Validate generic classification
finalCurveLookup[
  selectedModelGroup == "generic" &
    (
      coniferShare >= dominanceThreshold |
        broadleafShare >= dominanceThreshold
    ),
  .N
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
classificationResult_QC <- list(
  pixelGroupCurveLookup = pixelGroupCurveLookup,
  classificationSummary = classificationSummary,
  finalCurveLookup = finalCurveLookup,
  curveLookup = curveLookup,
  nationalResult = nationalResult
)

# Inspect final result structure
names(classificationResult_QC)

# Check dimensions
dim(classificationResult_QC$pixelGroupCurveLookup)

dim(classificationResult_QC$classificationSummary)

dim(classificationResult_QC$finalCurveLookup)

dim(classificationResult_QC$curveLookup)

# Inspect final lookup
head(classificationResult_QC$pixelGroupCurveLookup)

# Inspect National RSYC components
names(classificationResult_QC$nationalResult)
outputPath <- file.path(
  getwd(),
  "Classification_outputs",
  "QC",
  "Initial_validation"
)

dir.create(
  outputPath,
  recursive = TRUE,
  showWarnings = FALSE
)

normalizePath(outputPath)
# Save complete classification result
saveRDS(
  classificationResult_QC,
  file.path(outputPath, "QC_NationalRSYC_Classification.rds")
)

# Save pixelGroup-to-curve lookup
data.table::fwrite(
  pixelGroupCurveLookup,
  file.path(outputPath, "QC_pixelGroupCurveLookup.csv")
)

# Save classification summary
data.table::fwrite(
  classificationSummary,
  file.path(outputPath, "QC_classificationSummary.csv")
)

# Save detailed classification results
data.table::fwrite(
  finalCurveLookup,
  file.path(outputPath, "QC_finalCurveLookup.csv")
)

# Save national curve lookup
data.table::fwrite(
  curveLookup,
  file.path(outputPath, "National_curveLookup.csv")
)

# Verify saved files
savedFiles <- list.files(
  outputPath,
  full.names = TRUE
)

data.table::data.table(
  file = basename(savedFiles),
  size_MB = round(
    file.info(savedFiles)$size / 1024^2,
    2
  )
)
# Save spatial matching results
saveRDS(
  pixelGroupEco,
  file.path(outputPath, "QC_pixelGroupEco.rds")
)

# Save ecodistrict raster
terra::writeRaster(
  ecodistrictMap,
  file.path(outputPath, "QC_ecodistrictMap.tif"),
  overwrite = FALSE
)

# Save species biomass shares
saveRDS(
  speciesBiomass,
  file.path(outputPath, "QC_speciesBiomass.rds")
)

# Save dominant species results
saveRDS(
  dominantSpecies,
  file.path(outputPath, "QC_dominantSpecies.rds")
)

# Save functional group biomass shares
saveRDS(
  groupBiomassSummary,
  file.path(outputPath, "QC_groupBiomassSummary.rds")
)

# Save experimental functional mapping
data.table::fwrite(
  functionalMapping_test,
  file.path(outputPath, "QC_functionalMapping_TEST.csv")
)

# Save missing species-specific curve combinations
data.table::fwrite(
  missingCurveSummary,
  file.path(outputPath, "QC_missingSpeciesCurves.csv")
)

# Save unresolved pixelGroups
data.table::fwrite(
  finalCurveLookup[
    classificationResult != "assigned"
  ],
  file.path(outputPath, "QC_unresolvedPixelGroups.csv")
)
savedFiles <- list.files(
  outputPath,
  full.names = TRUE
)

savedFileSummary <- data.table::data.table(
  file = basename(savedFiles),
  size_MB = round(
    file.info(savedFiles)$size / 1024^2,
    2
  )
)

print(savedFileSummary)

# Number of saved files
length(savedFiles)

# Total storage size in MB
sum(file.info(savedFiles)$size) / 1024^2

# Check for empty files
savedFiles[file.info(savedFiles)$size == 0]
# Create spatial points from unmatched cells
unmatchedPoints <- terra::vect(
  unmatchedXY,
  type = "points",
  crs = terra::crs(sim$pixelGroupMap)
)

# Convert to geographic coordinates
unmatchedPoints_LL <- terra::project(
  unmatchedPoints,
  "EPSG:4326"
)

# Extract longitude and latitude
unmatchedCoords_LL <- terra::crds(
  unmatchedPoints_LL
)

# Geographic extent
summary(unmatchedCoords_LL)

# Mean geographic coordinates
colMeans(unmatchedCoords_LL)

# Check intersection with original RSYC polygons
polygonCheck <- terra::extract(
  ecoProjected,
  unmatchedPoints
)

# Summarize polygon coverage
table(
  is.na(polygonCheck$ecodistrict_id)
)

# Calculate distance to the nearest RSYC polygon
nearestEcoDistance <- terra::distance(
  unmatchedPoints,
  ecoProjected,
  pairwise = FALSE
)

# Extract minimum distance for each unmatched cell
minEcoDistance_m <- apply(
  nearestEcoDistance,
  1,
  min,
  na.rm = TRUE
)

# Summarize distances in meters
summary(minEcoDistance_m)

# Summarize distances in kilometers
summary(minEcoDistance_m / 1000)

# Create diagnostic table
distanceSummary <- data.table::data.table(
  distanceClass = c(
    "0-240 m",
    "240-1000 m",
    "1-5 km",
    "5-10 km",
    "More than 10 km"
  ),
  nCells = c(
    sum(minEcoDistance_m <= 240),
    sum(minEcoDistance_m > 240 &
          minEcoDistance_m <= 1000),
    sum(minEcoDistance_m > 1000 &
          minEcoDistance_m <= 5000),
    sum(minEcoDistance_m > 5000 &
          minEcoDistance_m <= 10000),
    sum(minEcoDistance_m > 10000)
  )
)

distanceSummary[
  ,
  percentage := 100 * nCells / sum(nCells)
]

print(distanceSummary)

# Validate total number of cells
sum(distanceSummary$nCells)
missingSpeciesCurvePG <- finalCurveLookup[
  classificationResult == "missing_curve" &
    selectedModelGroup == "species" &
    !is.na(ecodistrict_id) &
    !is.na(species)
]

# Check dimensions
dim(missingSpeciesCurvePG)

# Summarize by species
missingSpeciesCurvePG[
  ,
  .(nPixelGroups = .N),
  by = .(speciesCode, species)
][order(-nPixelGroups)]

# Summarize by species and ecodistrict
missingSpeciesCurvePG[
  ,
  .(nPixelGroups = .N),
  by = .(speciesCode, species, ecodistrict_id)
][order(-nPixelGroups)]

# Check dominant species biomass shares
summary(missingSpeciesCurvePG$biomassShare)

# Check functional group biomass shares
summary(missingSpeciesCurvePG$coniferShare)

summary(missingSpeciesCurvePG$broadleafShare)
# Extract unresolved dominant Lari_lar pixelGroups
lariFallbackTest <- data.table::copy(
  finalCurveLookup[
    speciesCode == "Lari_lar" &
      dominanceStatus == "species_dominant" &
      classificationResult == "missing_curve"
  ]
)

# Check dimensions
dim(lariFallbackTest)

# Inspect ecodistrict distribution
lariFallbackTest[
  ,
  .(nPixelGroups = .N),
  by = ecodistrict_id
]

# Check biomass shares
summary(lariFallbackTest$biomassShare)

summary(lariFallbackTest$coniferShare)

# Determine potential fallback model group
lariFallbackTest[
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

# Match potential fallback curves
lariFallbackTest <- merge(
  lariFallbackTest,
  fallbackCurves,
  by = c(
    "ecodistrict_id",
    "fallbackModelGroup"
  ),
  all.x = TRUE,
  sort = FALSE
)

# Validate row conservation
stopifnot(
  nrow(lariFallbackTest) == 58L,
  !anyDuplicated(lariFallbackTest$pixelGroup)
)

# Summarize fallback availability
lariFallbackTest[
  ,
  .(
    nPixelGroups = .N,
    nAvailable = sum(!is.na(fallbackCurveID)),
    nMissing = sum(is.na(fallbackCurveID))
  ),
  by = .(
    ecodistrict_id,
    fallbackModelGroup,
    fallbackCurveID
  )
]
lariFallbackTest[
  ,
  .(
    nPixelGroups = .N,
    nAvailable = sum(!is.na(fallbackCurveID)),
    nMissing = sum(is.na(fallbackCurveID)),
    nUniqueCurves = data.table::uniqueN(
      fallbackCurveID,
      na.rm = TRUE
    )
  )
]

data.table::fwrite(
  lariFallbackTest,
  file.path(
    outputPath,
    "QC_Lari_lar_Fallback_TEST.csv"
  )
)

# Combine both fallback candidate tables
combinedFallback <- data.table::rbindlist(
  list(
    fallbackTest[
      ,
      .(
        pixelGroup,
        fallbackModelGroup,
        fallbackCurveID
      )
    ],
    lariFallbackTest[
      ,
      .(
        pixelGroup,
        fallbackModelGroup,
        fallbackCurveID
      )
    ]
  ),
  use.names = TRUE
)

# Validate combined fallback candidates
dim(combinedFallback)

anyDuplicated(combinedFallback$pixelGroup)

sum(is.na(combinedFallback$fallbackCurveID))

# Summarize fallback groups
combinedFallback[
  ,
  .(nPixelGroups = .N),
  by = fallbackModelGroup
]

# Ensure all candidates were previously unresolved
stopifnot(
  all(
    combinedFallback$pixelGroup %in%
      finalCurveLookup[
        classificationResult == "missing_curve",
        pixelGroup
      ]
  )
)

# Validate fallback candidates
stopifnot(
  nrow(combinedFallback) == 422L,
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

# Create an independent copy
finalCurveLookup_TEST <- data.table::copy(
  finalCurveLookup
)

# Preserve original classification information
finalCurveLookup_TEST[
  ,
  `:=`(
    originalCurveID = curveID,
    originalModelGroup = selectedModelGroup,
    originalClassificationResult = classificationResult,
    fallbackApplied = FALSE
  )
]

# Apply fallback curves
finalCurveLookup_TEST[
  combinedFallback,
  on = "pixelGroup",
  `:=`(
    curveID = i.fallbackCurveID,
    selectedModelGroup = i.fallbackModelGroup,
    classificationResult = "assigned",
    fallbackApplied = TRUE
  )
]

# Summarize updated classification
finalCurveLookup_TEST[
  ,
  .(nPixelGroups = .N),
  by = .(
    selectedModelGroup,
    classificationResult
  )
][order(selectedModelGroup, classificationResult)]

# Count applied fallbacks
table(finalCurveLookup_TEST$fallbackApplied)

# Check overall classification coverage
finalCurveLookup_TEST[
  ,
  .(
    totalPixelGroups = .N,
    nAssigned = sum(!is.na(curveID)),
    nUnresolved = sum(is.na(curveID)),
    coverage = 100 * mean(!is.na(curveID))
  )
]
# Identify previously assigned pixelGroups
previouslyAssigned <- finalCurveLookup_TEST[
  originalClassificationResult == "assigned"
]

# Validate original successful assignments
stopifnot(
  nrow(previouslyAssigned) == 3169155L,
  all(
    previouslyAssigned$curveID ==
      previouslyAssigned$originalCurveID
  ),
  all(
    previouslyAssigned$selectedModelGroup ==
      previouslyAssigned$originalModelGroup
  ),
  all(
    previouslyAssigned$classificationResult ==
      previouslyAssigned$originalClassificationResult
  ),
  !any(previouslyAssigned$fallbackApplied)
)

message("PASS: All original assignments preserved.")

# Validate fallback assignments
fallbackAssigned <- finalCurveLookup_TEST[
  fallbackApplied == TRUE
]

stopifnot(
  nrow(fallbackAssigned) == 422L,
  all(is.na(fallbackAssigned$originalCurveID)),
  all(!is.na(fallbackAssigned$curveID)),
  all(fallbackAssigned$originalModelGroup == "species"),
  all(fallbackAssigned$classificationResult == "assigned")
)

message("PASS: All fallback assignments validated.")

# Validate remaining unresolved pixelGroups
remainingUnresolved <- finalCurveLookup_TEST[
  is.na(curveID)
]

stopifnot(
  nrow(remainingUnresolved) == 2509L,
  all(is.na(remainingUnresolved$ecodistrict_id)),
  !any(remainingUnresolved$fallbackApplied)
)

message("PASS: Remaining unresolved pixelGroups lack ecodistrict IDs.")

# Validate row conservation and uniqueness
stopifnot(
  nrow(finalCurveLookup_TEST) == nrow(finalCurveLookup),
  !anyDuplicated(finalCurveLookup_TEST$pixelGroup),
  identical(
    finalCurveLookup_TEST$pixelGroup,
    finalCurveLookup$pixelGroup
  )
)

message("PASS: PixelGroup integrity preserved.")

# Summarize remaining unresolved records
remainingUnresolved[
  ,
  .(
    nPixelGroups = .N,
    nMissingEcodistrict = sum(is.na(ecodistrict_id))
  ),
  by = selectedModelGroup
][order(selectedModelGroup)]
# Create output directory
testOutputPath <- file.path(
  getwd(),
  "Classification_outputs",
  "QC",
  "Fallback_validation"
)

dir.create(
  testOutputPath,
  recursive = TRUE,
  showWarnings = FALSE
)

# Prepare pixelGroup lookup
pixelGroupCurveLookup_TEST <- data.table::copy(
  finalCurveLookup_TEST[
    ,
    .(
      pixelGroup,
      ecodistrict_id,
      curveID,
      selectedModelGroup,
      classificationResult,
      fallbackApplied
    )
  ]
)

data.table::setorder(
  pixelGroupCurveLookup_TEST,
  pixelGroup
)

# Prepare classification summary
classificationSummary_TEST <- pixelGroupCurveLookup_TEST[
  ,
  .(nPixelGroups = .N),
  by = .(
    selectedModelGroup,
    classificationResult
  )
]

# Prepare complete classification result
classificationResult_QC_TEST <- list(
  pixelGroupCurveLookup = pixelGroupCurveLookup_TEST,
  classificationSummary = classificationSummary_TEST,
  finalCurveLookup = finalCurveLookup_TEST,
  curveLookup = curveLookup,
  nationalResult = nationalResult
)

# Save complete RDS result
saveRDS(
  classificationResult_QC_TEST,
  file.path(
    testOutputPath,
    "QC_NationalRSYC_Classification_TEST.rds"
  )
)

# Save CSV outputs
data.table::fwrite(
  pixelGroupCurveLookup_TEST,
  file.path(
    testOutputPath,
    "QC_pixelGroupCurveLookup_TEST.csv"
  )
)

data.table::fwrite(
  classificationSummary_TEST,
  file.path(
    testOutputPath,
    "QC_classificationSummary_TEST.csv"
  )
)

data.table::fwrite(
  finalCurveLookup_TEST,
  file.path(
    testOutputPath,
    "QC_finalCurveLookup_TEST.csv"
  )
)

data.table::fwrite(
  fallbackAssigned,
  file.path(
    testOutputPath,
    "QC_appliedFallbacks_TEST.csv"
  )
)

data.table::fwrite(
  remainingUnresolved,
  file.path(
    testOutputPath,
    "QC_remainingUnresolved_TEST.csv"
  )
)

data.table::fwrite(
  combinedFallback,
  file.path(
    testOutputPath,
    "QC_combinedFallback_TEST.csv"
  )
)

# Verify output directory
normalizePath(testOutputPath)

# Verify saved files
savedFiles_TEST <- list.files(
  testOutputPath,
  full.names = TRUE
)

data.table::data.table(
  file = basename(savedFiles_TEST),
  size_MB = round(
    file.info(savedFiles_TEST)$size / 1024^2,
    2
  )
)

# Validate saved files
stopifnot(
  length(savedFiles_TEST) == 7L,
  all(file.info(savedFiles_TEST)$size > 0)
)

message("PASS: All experimental outputs saved.")

library(terra)
library(data.table)

landrPath <- "E:/EasternCanadaClassifier/LandR_outputs/QC"

sim <- list(
  cohortData = fread(
    file.path(landrPath, "cohortData.csv")
  ),
  pixelGroupMap = rast(
    file.path(landrPath, "pixelGroupMap.tif")
  )
)

# Validate imported objects
names(sim)
dim(sim$cohortData)
uniqueN(sim$cohortData$pixelGroup)
# Build standardized AU-to-curve lookup
sim$AUtoCurve <- unique(
  auDefinition[, .(AU, curveID)]
)

setorder(sim$AUtoCurve, AU)

# Validate outputs
dim(sim$AUtoCurve)

head(sim$AUtoCurve, 20)

uniqueN(sim$AUtoCurve$AU)

anyDuplicated(sim$AUtoCurve$AU)

anyDuplicated(sim$AUtoCurve$curveID)

# Step 21.3 - Assign Analysis Units to pixel groups

pixelAU <- merge(
  qcLookup,
  sim$AUtoCurve,
  by = "curveID",
  all.x = TRUE,
  sort = FALSE
)

sim$pixelGroupToAU <- pixelAU[
  ,
  .(pixelGroup, analysisUnit = AU)
]

setorder(sim$pixelGroupToAU, pixelGroup)

# Check results
dim(sim$pixelGroupToAU)

head(sim$pixelGroupToAU)

anyDuplicated(sim$pixelGroupToAU$pixelGroup)

sim$pixelGroupToAU[
  ,
  .(
    nPixelGroups = .N,
    nAssigned = sum(!is.na(analysisUnit)),
    nUnassigned = sum(is.na(analysisUnit)),
    nAnalysisUnits = uniqueN(analysisUnit, na.rm = TRUE)
  )
]
# =====================================================
# Step 21.4 - Build classification table
# =====================================================

sim$classification <- pixelAU[
  ,
  .(
    pixelGroup,
    AU,
    curveID
  )
]

setorder(sim$classification, pixelGroup)

# Validate results
dim(sim$classification)

head(sim$classification, 20)

anyDuplicated(sim$classification$pixelGroup)

sum(is.na(sim$classification$AU))

sum(is.na(sim$classification$curveID))

uniqueN(sim$classification$AU, na.rm = TRUE)
# Step 21.4 - Correct missing curve IDs

sim$classification[
  curveID == "",
  curveID := NA_character_
]

# Validate classification

stopifnot(
  nrow(sim$classification) == 3172086L,
  !anyDuplicated(sim$classification$pixelGroup),
  sum(is.na(sim$classification$AU)) == 2509L,
  sum(is.na(sim$classification$curveID)) == 2509L,
  all(is.na(sim$classification$AU) ==
        is.na(sim$classification$curveID)),
  uniqueN(sim$classification$AU, na.rm = TRUE) == 295L
)

message("PASS: Classification table successfully validated.")
# =====================================================
# Step 21.5 - Build standDT for AAC
# =====================================================

# Temporary stand age definition

standDT <- sim$cohortData[
  ,
  .(
    age = as.numeric(names(which.max(table(age))))
  ),
  by = pixelGroup
]

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

standDT <- standDT[
  !is.na(AU) &
    !is.na(curveID)
]

# Validate results

dim(standDT)

head(standDT, 20)

summary(standDT$age)

anyDuplicated(standDT$pixelGroup)

sum(is.na(standDT$age))

uniqueN(standDT$AU)
# =====================================================
# Step 21.6 - Build Analysis Unit Map
# =====================================================

analysisUnitMap <- sim$pixelGroupMap

lookup <- sim$pixelGroupToAU

pg_vals <- terra::values(sim$pixelGroupMap)

idx <- match(
  pg_vals,
  lookup$pixelGroup
)

terra::values(analysisUnitMap) <-
  lookup$analysisUnit[idx]

sim$analysisUnitMap <- analysisUnitMap

# Validate Analysis Unit Map

print(sim$analysisUnitMap)

terra::compareGeom(
  sim$pixelGroupMap,
  sim$analysisUnitMap,
  stopOnError = FALSE
)

terra::freq(sim$analysisUnitMap)

plot(
  sim$analysisUnitMap,
  main = "Quebec - Analysis Unit Map",
  legend = FALSE
)
# =====================================================
# Step 21.7 - Effective Area and Analysis Unit Summary
# =====================================================

sim$standDT <- standDT

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
  
  # Raster values
  
  hf <- as.vector(
    terra::values(sim$harvestableFraction)
  )
  
  pg <- as.vector(
    terra::values(sim$pixelGroupMap)
  )
  
  # Build pixel-level area table
  
  pixel_area_dt <- data.table(
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
  
  # Join Analysis Units
  
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
  
  # ===================================================
  # Area by Analysis Unit
  # ===================================================
  
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
  
  # ===================================================
  # Analysis Unit Summary
  # ===================================================
  
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
  
  setorder(
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
# =====================================================
# Step 21.8 - Save Quebec Classification Outputs
# =====================================================

outputPath <- file.path(
  "E:/EasternCanadaClassifier",
  "Classification_outputs",
  "QC",
  "AU_validation_TEST"
)

dir.create(
  outputPath,
  recursive = TRUE,
  showWarnings = FALSE
)

# AU to Curve

fwrite(
  sim$AUtoCurve,
  file.path(outputPath, "QC_AUtoCurve.csv")
)

# PixelGroup to AU

fwrite(
  sim$pixelGroupToAU,
  file.path(outputPath, "QC_pixelGroupToAU.csv")
)

# Classification

fwrite(
  sim$classification,
  file.path(outputPath, "QC_classification.csv")
)

# Stand table

fwrite(
  sim$standDT,
  file.path(outputPath, "QC_standDT.csv")
)

# Analysis Unit Map

terra::writeRaster(
  sim$analysisUnitMap,
  file.path(outputPath, "QC_analysisUnitMap.tif"),
  overwrite = TRUE
)

# Save area-related outputs when available

if (!is.null(sim$pixelAreaDT)) {
  
  fwrite(
    sim$pixelAreaDT,
    file.path(outputPath, "QC_pixelAreaDT.csv")
  )
  
}

if (!is.null(sim$areaByAU)) {
  
  fwrite(
    sim$areaByAU,
    file.path(outputPath, "QC_areaByAU.csv")
  )
  
}

if (!is.null(sim$analysisUnitSummary)) {
  
  fwrite(
    sim$analysisUnitSummary,
    file.path(outputPath, "QC_analysisUnitSummary.csv")
  )
  
}

# Check saved files

list.files(
  outputPath,
  full.names = TRUE
)

message("PASS: Classification outputs saved.")

