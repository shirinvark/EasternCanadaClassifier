# =============================
# HELPER FUNCTIONS (NL)
# ============================

parse_curve <- function(curve_lines) {
  
  res <- list()
  current_sp <- NULL
  
  for (line in curve_lines) {
    
    line <- trimws(line)
    if (line == "") next
    
    parts <- strsplit(line, "\\s+")[[1]]
    
    # اگر species line بود (مثل BSv)
    if (grepl("^[A-Z]{2}v$", parts[1])) {
      current_sp <- parts[1]
      res[[current_sp]] <- numeric()
      
    } else if (!is.null(current_sp)) {
      
      nums <- suppressWarnings(as.numeric(parts))
      nums <- nums[!is.na(nums)]
      
      res[[current_sp]] <- c(res[[current_sp]], nums)
    }
  }
  
  return(res)
}

# -----------------------------

read_species_groups <- function(file) {
  lines <- readLines(file)
  res <- list()
  
  for (ln in lines) {
    parts <- strsplit(ln, ":")[[1]]
    grp <- trimws(parts[1])
    spp <- trimws(unlist(strsplit(parts[2], ",")))
    res[[grp]] <- spp
  }
  
  return(res)
}

# -----------------------------

read_curve_mapping <- function(file) {
  lines <- readLines(file)
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  
  mapping <- list()
  
  for (line in lines) {
    parts <- strsplit(line, ":")[[1]]
    key <- trimws(parts[1])
    
    vals <- strsplit(parts[2], ",")[[1]]
    vals <- trimws(vals)
    
    mapping[[key]] <- vals
  }
  
  return(mapping)
}

# -----------------------------

get_curve_vector <- function(curve_data, age_index, mapSpeciesToGroup_NL, cols) {  
  group_vals <- list()
  
  for (sp in names(curve_data)) {
    
    grp <- mapSpeciesToGroup_NL[[sp]]
    if (is.null(grp)) next
    
    vec <- curve_data[[sp]]
    
    age_index2 <- min(age_index, length(vec))
    
    val <- vec[age_index2]
    
    if (is.na(val)) val <- 0
    if (is.null(group_vals[[grp]])) {
      group_vals[[grp]] <- val
    } else {
      group_vals[[grp]] <- group_vals[[grp]] + val
    }
  }
  
  vals <- unlist(lapply(cols, function(g) {
    if (is.null(group_vals[[g]])) {
      0
    } else {
      group_vals[[g]]
    }
  }))
  
  if (sum(vals) > 0) {
    vals <- vals / sum(vals)
  } else {
    vals <- rep(0, length(vals))
  }
  
  return(vals)
}

# -----------------------------

find_best_curve <- function(p, region_curves, age, mapSpeciesToGroup_NL, cols) {
  
  age_index <- round(age / 10) + 1
  
  best_curve <- NA
  best_diff  <- Inf
  
  for (curve_name in names(region_curves)) {
    
    curve_data <- region_curves[[curve_name]]
    
    age_index2 <- min(age_index, length(curve_data[[1]]))
    
    y <- get_curve_vector(
      curve_data,
      age_index = age_index2,
      mapSpeciesToGroup_NL = mapSpeciesToGroup_NL,
      cols = cols
    )
    
    diff <- sqrt(sum((p - y)^2))
    
    if (diff < best_diff) {
      best_diff  <- diff
      best_curve <- curve_name
    }
  }
  
  return(best_curve)
}


get_region_from_name <- function(name) {
  if (grepl("_NPen$", name)) return("NPen")
  if (grepl("_Main$", name)) return("Main")
  if (grepl("_Long$", name)) return("Long")
  return(NA)
}
######################################################
# classifier
######################################################
classifyProvince_NL <- function(sim){    
  
  library(data.table)
  library(terra)
  # =========================================================
  # BUILD yield_by_region FROM YLD FILES
  # =========================================================
  
  yld_files <- sim$yieldFiles
  
  curves_by_region <- list(
    NPen = list(),
    Main = list(),
    Long = list()
  )
  
  for (f in yld_files) {
    
    curve_name <- tools::file_path_sans_ext(basename(f))
    
    region <- get_region_from_name(curve_name)
    
    # 🔥 fallback
    if (is.na(region)) {
      region <- "NPen"
    }
    
    lines <- readLines(f)
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    
    curve_data <- parse_curve(lines)
    
    curves_by_region[[region]][[curve_name]] <- curve_data
  }
  
  yield_by_region <- curves_by_region
  browser()
  print("Yield loaded:")
  print(lapply(yield_by_region, length))
  # ---------------------------
  # inputs
  # ---------------------------
  cohortDT <- as.data.table(sim$cohortData)

  # ---------------------------
  # species groups
  # ---------------------------
  speciesGroups <- read_species_groups("data/NL/speciesGroups.txt")
  
  cohortDT[, group := NA_character_]
  
  for (g in names(speciesGroups)) {
    cohortDT[speciesCode %in% speciesGroups[[g]], group := g]
  }
  
  print(table(is.na(cohortDT$group)))
  
  cohortDT <- cohortDT[!is.na(group)]
  
  print("species groups assigned")
  
  # ---------------------------
  # aggregation
  # ---------------------------
  pixelAgeGroup <- cohortDT[
    , .(B = sum(B)),
    by = .(pixelGroup, age, group)
  ]
  
  pixelAgeWide <- data.table::dcast(
    pixelAgeGroup,
    pixelGroup + age ~ group,
    value.var = "B",
    fill = 0
  )
  
  pixelAgeWide <- pixelAgeWide[!is.na(pixelGroup) & !is.na(age)]
  
  print("aggregation done")
  
  # ---------------------------
  # proportion
  # ---------------------------
  cols <- setdiff(names(pixelAgeWide), c("pixelGroup", "age", "totalB"))
  pixelAgeWide[, totalB := rowSums(.SD), .SDcols = cols]
  pixelAgeWide <- pixelAgeWide[totalB > 0]
  
  pixelAgeWide[, (cols) := lapply(.SD, function(x) x / totalB), .SDcols = cols]
  
  print("proportion done")
  
  # ---------------------------
  # mapping
  # ---------------------------
  mapSpeciesGroups <- read_curve_mapping("data/NL/mapSpeciesGroups.txt")
  
  # mapping مستقیم species → group
  mapSpeciesToGroup_NL <- mapSpeciesGroups
  
  print("mapping file:")
  print(mapSpeciesGroups)
  
  print("example species key:")
  print(names(mapSpeciesGroups))
  
  print("mapping built")
  
  # ---------------------------
  # test curve vector
  # ---------------------------
  curve_data <- yield_by_region$NPen[[1]]
  
  if (is.null(curve_data)) {
    stop("No curves found in NPen")
  }  
  print("curve species:")
  print(names(curve_data))
  
  test_vec <- get_curve_vector(
    curve_data,
    age_index = 1,
    mapSpeciesToGroup_NL = mapSpeciesToGroup_NL,
    cols = cols
  )
  
  print("curve vector test:")
  print(test_vec)
  
  # ---------------------------
  # test best curve
  # ---------------------------
  region_curves <- yield_by_region$NPen
  
  if (nrow(pixelAgeWide) == 0) {
    stop("pixelAgeWide is empty")
  } 
  age_val <- pixelAgeWide$age[1]
  p <- as.numeric(pixelAgeWide[1, ..cols])
  best <- find_best_curve(
    p,
    region_curves,
    age_val,
    mapSpeciesToGroup_NL,
    cols
  )
  
  print("best curve test:")
  print(best)
  
  # ---------------------------
  # assign best curve to all pixels
  # ---------------------------
  pixelAgeWide[, bestCurve := mapply(function(i) {
    
    p <- as.numeric(pixelAgeWide[i, cols, with = FALSE])
    age_val <- pixelAgeWide$age[i]
    
    find_best_curve(
      p,
      yield_by_region$NPen,
      age_val,
      mapSpeciesToGroup_NL,
      cols
    )
    
  }, 1:nrow(pixelAgeWide))]
  
  print("all pixels classified")
  
  # ---------------------------
  # build analysisUnitMap
  # ---------------------------
  pixelGroupMap <- sim$pixelGroupMap
  
  lookup <- pixelAgeWide[, .(pixelGroup, bestCurve)]
  
  lookup[, AU_id := as.numeric(as.factor(bestCurve))]
  
  analysisUnitMap <- pixelGroupMap
  vals <- terra::values(pixelGroupMap)
  
  match_idx <- match(vals, lookup$pixelGroup)
  
  analysis_vals <- lookup$AU_id[match_idx]
  analysis_vals[is.na(analysis_vals)] <- NA  
  terra::values(analysisUnitMap) <- analysis_vals
  
  print("analysisUnitMap built")
  
  sim$analysisUnitDT <- pixelAgeWide
  sim$analysisUnitMap <- analysisUnitMap
  
  # area summary
  areaByAU <- as.data.table(terra::freq(analysisUnitMap))
  setnames(areaByAU, c("value", "count"), c("AU_id", "nPixels"))
  areaByAU[, area_ha := nPixels * (250 * 250) / 10000]
  
  sim$areaByAU <- areaByAU
  
  return(sim)}