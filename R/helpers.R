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
extract_curve_block <- function(
    lines,
    community,
    region,
    site = "medium",
    density = "d2"
) {
  
  pattern <- paste0(
    "^\\*Y ",
    community,
    " ",
    site,
    " ",
    density,
    " \\? ",
    region
  )
  
  start <- grep(pattern, lines)
  
  if (length(start) == 0) {
    stop("Curve block not found")
  }
  
  start <- start[1]
  
  curve_starts <- grep("^\\*Y ", lines)
  
  next_start <- curve_starts[curve_starts > start][1]
  
  if (is.na(next_start)) {
    end <- length(lines)
  } else {
    end <- next_start - 1
  }
  
  lines[start:end]
}
##################################

get_region_from_name <- function(name) {
  
  if (grepl("BarNS", name)) return("NPen")
  if (grepl("Central", name)) return("Central")
  if (grepl("West", name)) return("West")
  if (grepl("District", name)) return("Central")
  if (grepl("NpMainLong", name)) return("NPen")
  
  return(NA)
}