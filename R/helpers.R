parseYLDHeaders <- function(lines) {
  
  stopifnot(is.character(lines))
  
  header_idx <- grep("^\\*Y\\s+", lines)
  
  if (length(header_idx) == 0) {
    stop("No *Y headers found.")
  }
  
  headers <- data.table::data.table(
    curveID    = seq_along(header_idx),
    lineNumber = header_idx,
    community  = character(length(header_idx)),
    quality    = character(length(header_idx)),
    density    = character(length(header_idx)),
    region      = character(length(header_idx))
  )
  
  pattern <- "^\\*Y\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+\\?\\s+(\\S+)"
  
  for (i in seq_along(header_idx)) {
    
    x <- regexec(pattern, trimws(lines[header_idx[i]]))
    m <- regmatches(trimws(lines[header_idx[i]]), x)[[1]]
    
    if (length(m) != 5) {
      
      stop(
        sprintf(
          "Cannot parse header at line %d:\n%s",
          header_idx[i],
          lines[header_idx[i]]
        )
      )
      
    }
    
    headers$community[i] <- m[2]
    headers$quality[i]   <- m[3]
    headers$density[i]   <- m[4]
    headers$region[i]    <- m[5]
    
  }
  
  headers
}


#############################################
parseSpecies <- function(lines, startLine) {
  
  stopifnot(
    is.character(lines),
    length(startLine) == 1,
    !is.na(startLine)
  )
  
  species <- character()
  lineStart <- integer()
  
  i <- startLine + 1
  
  while (i <= length(lines)) {
    
    line <- trimws(lines[i])
    
    ## End of this yield table
    if (startsWith(line, "*Y")) {
      break
    }
    
    ## Skip blank lines
    if (line == "") {
      i <- i + 1
      next
    }
    
    fields <- strsplit(line, "\\s+")[[1]]
    
    ## A species line should start with a species code
    ## followed by numeric values.
    if (
      length(fields) >= 2 &&
      grepl("^[A-Za-z][A-Za-z0-9]*$", fields[1])
    ) {
      
      ## A valid species must have two continuation lines
      if (i + 2 > length(lines)) {
        break
      }
      
      if (
        trimws(lines[i + 1]) == "" ||
        trimws(lines[i + 2]) == ""
      ) {
        break
      }
      
      species <- c(species, fields[1])
      lineStart <- c(lineStart, i)
      
      ## Skip the next two continuation lines
      i <- i + 3
      
    } else {
      
      ## Metadata or unexpected line
      break
      
    }
    
  }
  
  list(
    species = species,
    lineStart = lineStart
  )
  
}

###############################parse volume 

parseVolumes <- function(lines, speciesInfo) {
  
  stopifnot(
    is.character(lines),
    is.list(speciesInfo),
    all(c("species", "lineStart") %in% names(speciesInfo))
  )
  
  curve <- list()
  
  for (i in seq_along(speciesInfo$species)) {
    
    lineStart <- speciesInfo$lineStart[i]
    
    block <- paste(
      lines[lineStart:(lineStart + 2)],
      collapse = " "
    )
    
    volumes <- regmatches(
      block,
      gregexpr("[0-9]+\\.[0-9]+", block)
    )[[1]]
    
    volumes <- as.numeric(volumes)
    
    if (length(volumes) != 42) {
      
      stop(
        sprintf(
          "Expected 42 volumes but found %d for species %s (line %d).",
          length(volumes),
          speciesInfo$species[i],
          lineStart
        )
      )
      
    }
    
    curve[[speciesInfo$species[i]]] <- volumes
    
  }
  
  curve
}


# =============================
# HELPER FUNCTIONS (NL)
# ============================
rewrite_yld_curve <- function(curve_data, mapGroups) {
  
  stopifnot(
    is.list(curve_data),
    is.list(mapGroups)
  )
  # invert mapping (group → YLD members)
  group_map <- list()
  
  for (k in names(mapGroups)) {
    grp <- mapGroups[[k]]
    
    if (is.null(group_map[[grp]])) {
      group_map[[grp]] <- c()
    }
    
    group_map[[grp]] <- c(group_map[[grp]], k)
  }
  
  # Sum all species belonging to the same species group
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
  
  # Pad all groups to the same vector length
  max_len_all <- max(sapply(new_curve, length))
  
  new_curve <- lapply(new_curve, function(v) {
    c(v, rep(0, max_len_all - length(v)))
  })
  
  return(new_curve)
}
#--------------------------------

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

##################################

get_region_from_name <- function(name) {
  
  if (grepl("BarNS", name)) return("NPen")
  if (grepl("Central", name)) return("Central")
  if (grepl("West", name)) return("West")
  if (grepl("District", name)) return("Central")
  if (grepl("NpMainLong", name)) return("NPen")
  
  return(NA)
}