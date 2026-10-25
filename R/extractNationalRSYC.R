# ============================================================
# Extract National RSYC Yield Curves for One Province
# ============================================================

extractNationalRSYC <- function(
    provinceCode = "ON",
    rsycFile,
    inputPath = "E:/EasternCanadaClassifier/data",
    maxAge = 150
) {
  
  # ----------------------------------------------------------
  # 1. Packages and settings
  # ----------------------------------------------------------
  
  required <- c("sf", "DBI", "RSQLite", "rnaturalearth")
  
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing) > 0) {
    stop("Missing packages: ", paste(missing, collapse = ", "))
  }
  
  provinceCode <- toupper(provinceCode)
  
  supported <- c(
    "ON", "QC", "NL", "NB", "NS", "PE",
    "AB", "BC", "MB", "SK"
  )
  
  if (!provinceCode %in% supported) {
    stop("Unsupported province: ", provinceCode)
  }
  
  if (!file.exists(rsycFile)) {
    stop("RSYC file not found: ", rsycFile)
  }
  
  if (length(maxAge) != 1 ||
      is.na(maxAge) ||
      maxAge < 0 ||
      maxAge != as.integer(maxAge)) {
    stop("maxAge must be a non-negative integer.")
  }
  
  outputDir <- file.path(
    inputPath, provinceCode, "National"
  )
  
  dir.create(
    outputDir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  message("Extracting National RSYC for: ", provinceCode)
  
  # ----------------------------------------------------------
  # 2. Read national RSYC model parameters
  # ----------------------------------------------------------
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    rsycFile
  )
  
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  params <- DBI::dbReadTable(
    con,
    "RSYC_params_ecosystem_levels"
  )
  
  requiredCols <- c(
    "y_var", "scale", "stratif", "parameter_level",
    "join_key", "ecodistrict", "species1",
    "b1", "b2", "b3", "b4", "model_version"
  )
  
  if (!all(requiredCols %in% names(params))) {
    stop(
      "Missing RSYC columns: ",
      paste(setdiff(requiredCols, names(params)), collapse = ", ")
    )
  }
  
  models <- params[
    !is.na(params$y_var) &
      !is.na(params$scale) &
      !is.na(params$stratif) &
      !is.na(params$parameter_level) &
      params$y_var == "volume" &
      params$scale == "national" &
      params$stratif == "ecosystems" &
      params$parameter_level == "ecodistrict",
    ,
    drop = FALSE
  ]
  
  if (nrow(models) == 0) {
    stop("No national ecodistrict volume models found.")
  }
  
  # ----------------------------------------------------------
  # 3. Read RSYC ecodistrict polygons
  # ----------------------------------------------------------
  
  ecodistricts <- sf::st_read(
    rsycFile,
    layer = "ecodistricts",
    quiet = TRUE
  )
  
  if (!"join_key" %in% names(ecodistricts)) {
    stop("RSYC ecodistrict layer has no join_key.")
  }
  
  if (is.na(sf::st_crs(ecodistricts))) {
    stop("RSYC ecodistrict CRS is missing.")
  }
  
  # ----------------------------------------------------------
  # 4. Select province boundary
  # ----------------------------------------------------------
  
  canada <- rnaturalearth::ne_states(
    country = "Canada",
    returnclass = "sf"
  )
  
  provinceID <- paste0("CA-", provinceCode)
  
  province <- canada[
    !is.na(canada$iso_3166_2) &
      canada$iso_3166_2 == provinceID,
  ]
  
  if (nrow(province) != 1) {
    stop("Could not uniquely identify province: ", provinceID)
  }
  
  # ----------------------------------------------------------
  # 5. Identify intersecting ecodistricts
  # ----------------------------------------------------------
  
  provinceRSYC <- sf::st_transform(
    province,
    sf::st_crs(ecodistricts)
  )
  
  hits <- lengths(
    sf::st_intersects(ecodistricts, provinceRSYC)
  ) > 0
  
  selected <- ecodistricts[hits, ]
  
  if (nrow(selected) == 0) {
    stop("No intersecting RSYC ecodistricts.")
  }
  
  # ----------------------------------------------------------
  # 6. Calculate positive-area overlap
  # ----------------------------------------------------------
  
  targetCRS <- 3347
  
  selectedArea <- sf::st_transform(
    selected,
    targetCRS
  )
  
  provinceArea <- sf::st_transform(
    province,
    targetCRS
  )
  
  selectedArea$ecodistrictArea_m2 <- as.numeric(
    sf::st_area(selectedArea)
  )
  
  overlap <- suppressWarnings(
    sf::st_intersection(
      selectedArea,
      provinceArea
    )
  )
  
  overlap$areaInProvince_m2 <- as.numeric(
    sf::st_area(overlap)
  )
  
  overlap <- overlap[
    !is.na(overlap$areaInProvince_m2) &
      overlap$areaInProvince_m2 > 0,
  ]
  
  if (nrow(overlap) == 0) {
    stop("No positive-area ecodistrict overlaps.")
  }
  
  overlap$pctInProvince <-
    100 * overlap$areaInProvince_m2 /
    overlap$ecodistrictArea_m2
  
  provinceKeys <- unique(
    as.character(overlap$join_key)
  )
  
  # ----------------------------------------------------------
  # 7. Select province-specific models
  # ----------------------------------------------------------
  
  provinceModels <- models[
    models$join_key %in% provinceKeys,
    ,
    drop = FALSE
  ]
  
  if (nrow(provinceModels) == 0) {
    stop("No RSYC volume models for: ", provinceCode)
  }
  
  missingKeys <- setdiff(
    provinceKeys,
    unique(provinceModels$join_key)
  )
  
  if (length(missingKeys) > 0) {
    warning(
      length(missingKeys),
      " intersecting ecodistricts have no volume models."
    )
  }
  
  modelKey <- paste(
    provinceModels$join_key,
    provinceModels$species1,
    sep = " | "
  )
  
  if (anyDuplicated(modelKey)) {
    stop("Duplicate ecodistrict/species models found.")
  }
  
  if (any(!is.finite(as.matrix(
    provinceModels[, c("b1", "b2", "b3", "b4")]
  )))) {
    stop("Non-finite RSYC model parameters found.")
  }
  
  # Deterministic ordering and province-specific curve IDs
  provinceModels <- provinceModels[
    order(
      provinceModels$join_key,
      provinceModels$species1
    ),
    ,
    drop = FALSE
  ]
  
  provinceModels$curveID <- paste0(
    provinceCode,
    "_RSYC_",
    seq_len(nrow(provinceModels))
  )
  
  # ----------------------------------------------------------
  # 8. RSYC prediction equation
  # ----------------------------------------------------------
  
  CRdeclining2 <- function(age, b1, b2, b3, b4) {
    b1 * exp(-b4 * age) *
      (1 - exp(-b2 * age))^b3
  }
  
  # ----------------------------------------------------------
  # 9. Generate annual yield curves
  # ----------------------------------------------------------
  
  ages <- 0:maxAge
  
  curves <- do.call(
    rbind,
    lapply(seq_len(nrow(provinceModels)), function(i) {
      
      model <- provinceModels[i, ]
      
      volume <- CRdeclining2(
        age = ages,
        b1 = model$b1,
        b2 = model$b2,
        b3 = model$b3,
        b4 = model$b4
      )
      
      data.frame(
        curveID = model$curveID,
        join_key = model$join_key,
        ecodistrict = model$ecodistrict,
        species1 = model$species1,
        age = ages,
        volume_m3_ha = volume,
        model_version = model$model_version,
        stringsAsFactors = FALSE
      )
    })
  )
  
  rownames(curves) <- NULL
  
  # ----------------------------------------------------------
  # 10. Build lookup tables
  # ----------------------------------------------------------
  
  curveLookup <- unique(
    curves[
      ,
      c(
        "curveID",
        "join_key",
        "ecodistrict",
        "species1",
        "model_version"
      )
    ]
  )
  
  overlapLookup <- sf::st_drop_geometry(overlap)
  
  overlapLookup <- overlapLookup[
    ,
    c(
      "join_key",
      "ecodistrict",
      "areaInProvince_m2",
      "ecodistrictArea_m2",
      "pctInProvince"
    )
  ]
  
  # ----------------------------------------------------------
  # 11. Validate extracted curves
  # ----------------------------------------------------------
  
  expectedRows <- nrow(provinceModels) * length(ages)
  
  stopifnot(
    nrow(curves) == expectedRows,
    length(unique(curves$curveID)) == nrow(provinceModels),
    all(is.finite(curves$volume_m3_ha)),
    all(curves$volume_m3_ha >= 0),
    !anyNA(curves$join_key)
  )
  
  # ----------------------------------------------------------
  # 12. Save results
  # ----------------------------------------------------------
  
  prefix <- paste0(provinceCode, "_RSYC")
  
  utils::write.csv(
    curves,
    file.path(outputDir, paste0(prefix, "_yield_curves.csv")),
    row.names = FALSE
  )
  
  utils::write.csv(
    curveLookup,
    file.path(outputDir, paste0(prefix, "_curve_lookup.csv")),
    row.names = FALSE
  )
  
  utils::write.csv(
    overlapLookup,
    file.path(outputDir, paste0(prefix, "_ecodistrict_overlap.csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 13. Summary
  # ----------------------------------------------------------
  
  message("--------------------------------------")
  message("Province: ", provinceCode)
  message("Ecodistricts: ", length(provinceKeys))
  message("Models: ", nrow(provinceModels))
  message("Curve rows: ", nrow(curves))
  message("Age range: 0-", maxAge)
  message("Missing ecodistrict models: ", length(missingKeys))
  message("Output: ", outputDir)
  message("--------------------------------------")
  
  invisible(list(
    curves = curves,
    curveLookup = curveLookup,
    overlapLookup = overlapLookup,
    missingKeys = missingKeys
  ))
}