# =============================
# HELPER FUNCTIONS (NL)
# ============================
rewrite_yld_curve <- function(curve_data, mapGroups) {
  
  # invert mapping (group → YLD members)
  group_map <- list()
  
  for (k in names(mapGroups)) {
    grp <- mapGroups[[k]]
    
    if (is.null(group_map[[grp]])) {
      group_map[[grp]] <- c()
    }
    
    group_map[[grp]] <- c(group_map[[grp]], k)
  }
  
  # جمع زدن داخل گروه‌ها
  new_curve <- list()
  
  for (grp in names(group_map)) {
    
    members <- group_map[[grp]]
    
    vecs <- lapply(members, function(m) {
      v <- curve_data[[m]]
      if (is.null(v)) return(numeric(0))
      return(v)
    })
    
    max_len <- max(sapply(vecs, length))
    
    vecs <- lapply(vecs, function(v) {
      c(v, rep(0, max_len - length(v)))
    })
    
    new_curve[[grp]] <- Reduce(`+`, vecs)
  }
  
  # هم‌طول کردن همه گروه‌ها
  max_len_all <- max(sapply(new_curve, length))
  
  new_curve <- lapply(new_curve, function(v) {
    c(v, rep(0, max_len_all - length(v)))
  })
  
  return(new_curve)
}
#--------------------------------
parse_curve <- function(curve_lines) {  # Function to parse .yld curve lines into species-wise vectors.3  ÷÷÷÷÷÷÷÷÷÷÷÷÷÷÷÷÷÷÷÷÷÷
  
  res <- list()                        # Initialize output list to store species data
  current_sp <- NULL                   # Variable to track current species
  
  for (line in curve_lines) {          # Loop through each line of the file
    if (grepl("^\\*Y", line) && length(res) > 0) {
      break
    }
    line <- trimws(line)               # Remove leading/trailing whitespace
    if (line == "") next               # Skip empty lines
    
    parts <- strsplit(line, "\\s+")[[1]]  # Split line into tokens by whitespace
    
    # اگر species line بود (مثل BSv)
    if (grepl("^[A-Z]{2}v$", parts[1])) {   # Check if line defines a species (e.g., BSv)
      current_sp <- parts[1]                # Set current species
      res[[current_sp]] <- numeric()       # Initialize empty numeric vector for this species
      
    } else if (!is.null(current_sp)) {     # If numeric data line and species already defined
      
      nums <- suppressWarnings(as.numeric(parts))  # Convert tokens to numeric (suppress warnings)
      nums <- nums[!is.na(nums)]                   # Remove NA values
      
      res[[current_sp]] <- c(res[[current_sp]], nums)  # Append values to species vector
    }
  }
  
  return(res)   # Return list of species → numeric vectors
}

# -----------------------------

read_species_groups <- function(file) {  # Read species groups mapping file
  lines <- readLines(file)              # Read all lines from file
  res <- list()                         # Initialize output list
  
  for (ln in lines) {                   # Loop over lines
    parts <- strsplit(ln, ":")[[1]]     # Split group and species
    grp <- trimws(parts[1])             # Extract group name
    spp <- trimws(unlist(strsplit(parts[2], ",")))  # Extract species list
    res[[grp]] <- spp                   # Store mapping
  }
  
  return(res)   # Return group → species mapping
}

# -----------------------------

read_curve_mapping <- function(file) {   # Read curve species → group mapping
  lines <- readLines(file)               # Read file
  lines <- trimws(lines)                 # Trim whitespace
  lines <- lines[lines != ""]            # Remove empty lines
  
  mapping <- list()                      # Initialize mapping list
  
  for (line in lines) {                  # Loop through lines
    parts <- strsplit(line, ":")[[1]]    # Split key and values
    key <- trimws(parts[1])              # Species key (e.g., BSv)
    
    vals <- strsplit(parts[2], ",")[[1]] # Group(s)
    vals <- trimws(vals)                 # Trim whitespace
    
    mapping[[key]] <- vals               # Store mapping
  }
  
  return(mapping)   # Return species → group mapping
}

# -----------------------------

get_curve_vector <- function(curve_data, age_index, mapSpeciesToGroup_NL, cols) {
  
  vals <- rep(0, length(cols))
  names(vals) <- cols
  
  for (sp in names(curve_data)) {
    
    # فقط speciesهایی که در cols هستن
    #if (!(sp %in% cols)) next
    if (!(sp %in% names(vals))) next
    vec <- curve_data[[sp]]
    
    age_index2 <- min(age_index, length(vec))
    
    val <- vec[age_index2]
    if (is.na(val)) val <- 0
    
    vals[sp] <- val
  }
  
  # normalize
  if (sum(vals) > 0) {
    vals <- vals / sum(vals)
  }
  
  return(vals)
}
# -----------------------------

find_best_curve <- function(
    p,
    region_curves,
    age,
    mapSpeciesToGroup_NL,
    cols
) {
  
  curve_ids <- unique(region_curves$CURVENO)
  
  best_curve <- NA
  best_diff  <- Inf
  
  for (curve_name in curve_ids) {
    
    curve_data <- region_curves[
      CURVENO == curve_name
    ]
    
    age_diff <- abs(curve_data$AC10 - age)
    
    row_i <- which.min(age_diff)
    
    y <- as.numeric(
      curve_data[
        row_i,
        ..cols
      ]
    )
    
    if (sum(y) > 0) {
      y <- y / sum(y)
    }
    
    diff <- sqrt(sum((p - y)^2))
    
    if (diff < best_diff) {
      
      best_diff  <- diff
      best_curve <- curve_name
    }
  }
  
  return(best_curve)
}

# -----------------------------

get_region_from_name <- function(name) {
  
  if (grepl("BarNS", name)) return("NPen")
  if (grepl("Central", name)) return("Central")
  if (grepl("West", name)) return("West")
  if (grepl("District", name)) return("Central")
  if (grepl("NpMainLong", name)) return("NPen")
  
  return(NA)
}
######################################################
# classifier
######################################################

classifyProvince_NL <- function(sim){    # Main classifier function
  
  library(data.table)   # Load data.table
  library(terra)        # Load terra
  # =========================================================
  # BUILD YCF raster (region)
  # =========================================================
  mapSpeciesGroups <- read_curve_mapping("data/NL/mapSpeciesGroups.txt")
  speciesGroup <- read_curve_mapping(
    "E:/EasternCanadaClassifier/data/NL/speciesGroups.txt"
  )  
  ycf_vect <- terra::vect("data/NL/NL_YCF.shp")   # Load YCF shapefile
  
  # 🔥
  if (terra::crs(sim$pixelGroupMap) == "") {      # If raster has no CRS
    terra::crs(sim$pixelGroupMap) <- terra::crs(ycf_vect)  # Assign CRS
  }
  ####just for now
  ycf_vect <- terra::project(ycf_vect, sim$pixelGroupMap)  # Reproject to match raster
  ycf_vect <- terra::crop(ycf_vect, sim$pixelGroupMap)     # Crop to extent
  
  ycf_vect$YCF <- as.factor(ycf_vect$YCF)   # Ensure YCF is factor
  cat("\n===== EXTENT CHECK =====\n")
  print(ext(sim$pixelGroupMap))
  print(ext(ycf_vect))
  print(crs(sim$pixelGroupMap))
  print(ext(ycf_vect))
  ycf_raster <- terra::rasterize(           # Rasterize polygons
    ycf_vect,
    sim$pixelGroupMap,
    field = "YCF"
  )
  
  sim$ycfRaster <- ycf_raster   # Store in sim
  
  # =========================================================
  # BUILD yield_by_region FROM YLD FILES
  # =========================================================
  
  yld_files <- sim$yieldFiles   # Get list of .yld files
  
  curves_by_region <- list()
  
  for (f in yld_files) {
    
    curve_name <- tools::file_path_sans_ext(basename(f))
    
    # 🔥 اینجا اصلاح مهم
    curve_name <- tools::file_path_sans_ext(basename(f))
    
    region <- get_region_from_name(curve_name)    # اگر region پیدا نشد → رد کن
    if (is.na(region)) next
    
    lines <- readLines(f)
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    
    #curve_data <- parse_curve(lines)
    curve_data_raw <- parse_curve(lines)
    
    mapGroups <- read_curve_mapping("data/NL/mapSpeciesGroups.txt")
    cols <- unique(unlist(mapSpeciesGroups))
    curve_data <- rewrite_yld_curve(curve_data_raw, mapGroups)
    print("BEFORE REWRITE:")
    print(names(curve_data_raw))
    
    print("AFTER REWRITE:")
    print(names(curve_data))
    print("COLS:")
    print(cols)
   # if (is.null(curves_by_region[[region]])) {
    #  curves_by_region[[region]] <- list()
    #}
    
    #curves_by_region[[region]][[curve_name]] <- curve_data
    n_age <- length(curve_data[[1]])
    
    ages <- seq(
      0,
      by = 10,
      length.out = n_age
    )
    
    dt_curve <- data.table(
      CURVENO = curve_name,
      AC10 = ages
    )
    data.table::setDT(dt_curve)
    data.table::setalloccol(dt_curve)
    for (sp in names(curve_data)) {
      
      #dt_curve[[sp]] <- curve_data[[sp]]
      dt_curve[[sp]] <- rev(curve_data[[sp]])
    }
    
    dt_curve[, AU := curve_name]
    
    dt_curve[, zone := region]
    
    if (is.null(curves_by_region[[region]])) {
      
      curves_by_region[[region]] <- dt_curve
      
    } else {
      
      curves_by_region[[region]] <- rbind(
        curves_by_region[[region]],
        dt_curve,
        fill = TRUE
      )
    }
  }
  
  yield_by_region <- curves_by_region
  sim$rawYieldTables$NL <- yield_by_region
  cat("\n===== NUMBER OF CURVES PER REGION =====\n")
  print(lapply(yield_by_region, length))
  print("Regions found:")
  print(names(yield_by_region))
  
  print("Yield loaded:")
  print(lapply(yield_by_region, length))
  # ---------------------------
  # inputs
  # ---------------------------
  cohortDT <- as.data.table(sim$cohortData)
  #mapSpeciesGroups <- read_curve_mapping("data/NL/mapSpeciesGroups.txt")
  cohortDT[, final_group := sapply(speciesCode, function(x) {
    
    grp <- speciesGroup[[x]]   # 👈 فقط این
    
    if (is.null(grp)) return(NA_character_)
    
    return(as.character(grp))
  })]
  
  cohortDT <- cohortDT[!is.na(final_group)]
  cat("\n===== DEBUG final_group STRUCTURE =====\n")
  print(str(cohortDT$final_group))
  str(cohortDT$final_group)
  cohortDT <- cohortDT[!is.na(final_group)]
  
  print(table(is.na(cohortDT$final_group)))
  cohortDT <- cohortDT[!is.na(final_group)]
  
  print("species groups assigned")
  
  # -------------------------
  # aggregation
  # ---------------------------
  pixelAgeGroup <- cohortDT[
    , .(B = sum(B)),                  # Sum biomass
    by = .(pixelGroup, age, final_group)
  ]
  
  pixelAgeWide <- data.table::dcast(   # Convert to wide format
    pixelAgeGroup,
    pixelGroup + age ~ final_group,
    value.var = "B",
    fill = 0
  )
  print("CHECK FINAL GROUPS:")
  print(names(pixelAgeWide))
  pixelAgeWide <- pixelAgeWide[!is.na(pixelGroup) & !is.na(age)]  # Remove NA rows

  print("aggregation done")   # Debug: confirm aggregation step completed
  
  # =========================================================
  # ADD region to pixel Group
  # =========================================================
  
  pg_vals  <- terra::values(sim$pixelGroupMap)[,1]   # Extract pixelGroup values from raster
  reg_vals <- terra::values(sim$ycfRaster)[,1]       # Extract region (YCF) values from raster
  
  # 🔥 تبدیل NaN → NA
  pg_vals[is.nan(pg_vals)]   <- NA   # Replace NaN with NA for pixel groups
  reg_vals[is.nan(reg_vals)] <- NA   # Replace NaN with NA for regions
  
  # 🔥 تبدیل ID → region
  reg_names <- as.character(reg_vals)   # Convert region IDs to character
  
  reg_names[reg_names == "9"]  <- "NPen"     # Map numeric codes to region names
  reg_names[reg_names == "8"]  <- "Main"
  reg_names[reg_names == "7"]  <- "Long"
  reg_names[reg_names == "11"] <- "West"
  reg_names[reg_names == "10"] <- "NShore"
  reg_names[reg_names == "6"]  <- "Central"
  reg_names[reg_names == "5"]  <- "CentVic"
  reg_names[reg_names == "4"]  <- "CentRed"
  reg_names[reg_names == "3"]  <- "BarEast"
  reg_names[reg_names == "2"]  <- "BarCent"
  reg_names[reg_names == "1"]  <- "Avalon"
  reg_names[reg_names == "0"]  <- "Aphid"
  
  # NA درست کن
  reg_names[is.na(reg_vals)] <- NA   # Ensure NA stays NA
  
  reg_names <- sub("NL_", "", reg_names)   # Remove "NL_" prefix if present
  
  regionDT <- data.table(
    pixelGroup = pg_vals,   # Pixel group IDs
    region = reg_names      # Region names
  )
  
  regionDT <- regionDT[!is.na(pixelGroup)]   # Remove rows with NA pixelGroup
  
  regionDT <- regionDT[, .(
    region = {
      tbl <- table(region)   # Count region frequency per pixelGroup
      if (length(tbl) == 0) "NPen"   # Fallback if no region found
      else names(tbl)[which.max(tbl)]   # Select most frequent region
    }
  ), by = pixelGroup]
  
  pixelAgeWide <- merge(pixelAgeWide, regionDT, by = "pixelGroup", all.x = TRUE)   # Attach region to data
  
  # ---------------------------
  # proportion
  # ---------------------------
  cols <- names(pixelAgeWide)[sapply(pixelAgeWide, is.numeric)]   # Select numeric columns
  cols <- setdiff(cols, c("pixelGroup", "age", "totalB"))         # Remove non-species columns
  
  pixelAgeWide[, totalB := rowSums(.SD), .SDcols = cols]   # Compute total biomass per row
  pixelAgeWide <- pixelAgeWide[totalB > 0]                 # Remove rows with zero biomass
  
  pixelAgeWide[, (cols) := lapply(.SD, function(x) x / totalB), .SDcols = cols]   # Normalize to proportions
  
  print("proportion done")   # Debug: confirm normalization
  
  # ---------------------------
  # mapping
  # ---------------------------

  # mapping مستقیم species → group
  mapSpeciesToGroup_NL <- mapSpeciesGroups   # Direct mapping
  
  print("mapping file:")
  print(mapSpeciesGroups)   # Debug: show mapping
  
  print("example species key:")
  print(names(mapSpeciesGroups))   # Debug: list species keys
  
  print("mapping built")
  
  # ---------------------------
  # test curve vector
  # ---------------------------
 # curve_data <- yield_by_region[[1]][[1]]   # Take first curve for testing
  curve_data <- yield_by_region[[1]]
  
  curve_data <- curve_data[
    CURVENO == unique(curve_data$CURVENO)[1]
  ]
  if (is.null(curve_data)) {
    stop("No curves found in NPen")   # Stop if no curve found
  }  
  
  print("curve species:")
  print(names(curve_data))   # Debug: show species in curve
  
  test_vec <- get_curve_vector(
    curve_data,
    age_index = 1,
    mapSpeciesToGroup_NL = mapSpeciesToGroup_NL,
    cols = cols
  )
  
  print("curve vector test:")
  print(test_vec)   # Debug: show computed curve vector
  
  # ---------------------------
  # test best curve
  # ---------------------------
  # ---------------------------
  # test best curve
  # -------------
  
  if (nrow(pixelAgeWide) == 0) {
    stop("pixelAgeWide is empty")
  }
  
  region_i <- pixelAgeWide$region[1]
  region_curves <- yield_by_region[[region_i]]
  #region_curves <- yield_by_region[[1]]
  age_val <- pixelAgeWide$age[1]
  p <- as.numeric(pixelAgeWide[1, ..cols])
  
  print(cols)
  best <- find_best_curve(
    p,
    region_curves,
    age_val,
    mapSpeciesToGroup_NL,
    cols
  )
  
  print("best curve test:")
  print(best)
 
  
  # Mapping region to allowed curves
  #region_curve_map <- list(
   # NPen = c("BarNS_sub_all", "Central_Sub_all"),
    #Main = c("NpMainLong_sub_all"),
    #Long = c("NpMainLong_sub_all"),
    #West = c("West_sub_all")
  #)
  
  # ---------------------------
  # assign best curve to all pixels
  # ---------------------------
  pixelAgeWide[, bestCurve := mapply(function(i) {
    
    p <- as.numeric(pixelAgeWide[i, cols, with = FALSE])
    age_val <- pixelAgeWide$age[i]
    
    region_i <- pixelAgeWide$region[i]
    
    region_map <- list(
      Main    = "NPen",
      Long    = "NPen",
      
      Avalon  = "Central",
      Aphid   = "Central",
      CentVic = "Central",
      CentRed = "Central",
      
      BarCent = "NPen",
      BarEast = "NPen",
      
      NShore  = "West"
    )
    
    if (!region_i %in% names(yield_by_region)) {
      
      mapped_region <- region_map[[region_i]]
      
      if (is.null(mapped_region)) {
        return(NA)
      }
      
      region_i <- mapped_region
    }    
    region_curves <- yield_by_region[[region_i]]
    
    if (is.null(region_curves) || length(region_curves) == 0) {
      return(NA)
    }
    
    # اگر فقط یک curve داشت
   # if (length(region_curves) == 1) {
     # return(names(region_curves)[1])
    #}
    if (uniqueN(region_curves$CURVENO) == 1) {
      
      return(unique(region_curves$CURVENO))
      
    }
    # اگر چند curve داشت → انتخاب با distance
    find_best_curve(
      p,
      region_curves,
      age_val,
      mapSpeciesToGroup_NL,
      cols
    )
    
  }, 1:nrow(pixelAgeWide))]
  
  # ---------------------------
  # build analysisUnitMap
  # ---------------------------
  pixelGroupMap <- sim$pixelGroupMap   # Base raster
  
 # lookup <- pixelAgeWide[, .(pixelGroup, bestCurve)]   # Lookup table
  lookup <- unique(
    pixelAgeWide[
      , .(pixelGroup, bestCurve)
    ]
  )
  lookup[, AU_id := as.numeric(as.factor(bestCurve))]   # Convert curves to numeric IDs
  pixelAgeWide <- merge(
    pixelAgeWide,
    lookup,
    by = c("pixelGroup", "bestCurve"),
    all.x = TRUE
  )
  analysisUnitMap <- pixelGroupMap   # Initialize output raster
  vals <- terra::values(pixelGroupMap)   # Extract pixel values
  
  match_idx <- match(vals, lookup$pixelGroup)   # Match pixelGroup to lookup
  
  analysis_vals <- lookup$AU_id[match_idx]      # Assign AU IDs
  analysis_vals[is.na(analysis_vals)] <- NA     # Keep NA
  
  terra::values(analysisUnitMap) <- analysis_vals   # Write values to raster
  
  print("analysisUnitMap built")   # Debug
  
  sim$analysisUnitDT <- pixelAgeWide   # Store detailed table
  sim$analysisUnitMap <- analysisUnitMap   # Store raster
  sim$AUtoCurve <- unique(
    pixelAgeWide[
      , .(
        AU = AU_id,
        curveID = bestCurve
      )
    ]
  )
  # ---------------------------
  # area summary
  # ---------------------------
  areaByAU <- as.data.table(terra::freq(analysisUnitMap))   # Count pixels per AU
  
  setnames(areaByAU, c("value", "count"), c("AU_id", "nPixels"))   # Rename columns
  
  areaByAU[, area_ha := nPixels * (250 * 250) / 10000]   # Convert pixels to hectares
  
  sim$areaByAU <- areaByAU   # Store area summary
  
  return(sim)   # Return updated simulation object
}
