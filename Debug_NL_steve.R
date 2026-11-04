# lines <- readLines(
#   "E:/EasternCanadaClassifier/data/NL/Central_Sub_all.yld",
#   warn = FALSE
# )
# length(lines)
# lines[1:20]
# ####fin all the *y 
# y_idx <- grep("^\\*Y", lines)
# 
# length(y_idx)
# ####print the first to third header
# lines[y_idx[1]]
# lines[y_idx[1:3]]
# ####count the comuntities
# table(
#   sub("^\\*Y\\s+(\\S+).*", "\\1", headers)
# )
# # NOTE:
# # Do not rely on the "Total Curves" or "Forest Community Breakdown"
# # information in the file header. In some NL YLD files (e.g. Central_Sub_all.yld)
# # the header is outdated (198 curves reported), while the actual *Y sections
# # contain 216 yield tables. Always parse the *Y records directly.
# #read one table
# start <- y_idx[1]
# 
# end <- y_idx[2] - 1
# lines[start:end]
# #is the number of species the same?
# species_lines <- lines[(start + 1):(end - 1)]
# 
# grep("^\\s*[A-Za-z]", species_lines, value = TRUE)
# 
# species_count <- integer(length(y_idx))
# 
# for (i in seq_along(y_idx)) {
#   
#   start <- y_idx[i]
#   
#   end <- if (i < length(y_idx)) {
#     y_idx[i + 1] - 1
#   } else {
#     length(lines)
#   }
#   
#   tbl <- lines[start:end]
#   
#   species_count[i] <- sum(
#     grepl("^\\s+[A-Za-z]{2,3}v\\s", tbl)
#   )
# }
# 
# table(species_count)
# # ------------------------------------------------------------
# # YLD file structure observations
# #
# # - The file contains 216 yield tables (*Y records), although
# #   the file header reports only 198 curves. The header is
# #   outdated and should not be used to determine the number of
# #   yield tables. Always parse the *Y records directly.
# #
# # - Each yield table starts with a "*Y" header and ends with a
# #   blank line.
# #
# # - Each species is represented by three consecutive text lines:
# #     1) species code + first set of values
# #     2) continuation of values
# #     3) continuation of values
# #
# # - The number of species is NOT constant across communities:
# #       bF, bS, bSoS, HS     -> 9 species
# #       bFoH, bFoS, bSoH, wB -> 8 species
# #
# # Therefore, the parser must detect species dynamically from the
# # file rather than assuming a fixed number of species.
# # ------------------------------------------------------
# #what species is removed from softwoods?
# start <- y_idx[28]
# end   <- y_idx[29] - 1
# 
# lines[start:end]
# # ------------------------------------------------------------
# # Community-specific species composition
# #
# # Softwood communities (e.g., bF, bS, bSoS, HS) contain 9 species,
# # including WPv.
# #
# # Hardwood/mixed communities (e.g., bFoH, bFoS, bSoH, wB) contain
# # only 8 species because the WPv species block is absent.
# #
# # Therefore, species must be detected from the file rather than
# # assuming every table contains the same species list.
# # ------------------------------------------------------------
# ###############second yield table
# lines2 <- readLines(
#   "E:/EasternCanadaClassifier/data/NL/BarNS_sub_all.yld",
#   warn = FALSE
# )
# 
# length(lines2)
# 
# y_idx2 <- grep("^\\*Y", lines2)
# 
# length(y_idx2)
# 
# table(
#   sub("^\\*Y\\s+(\\S+).*", "\\1", lines2[y_idx2])
# )
# i <- which(
#   grepl("^\\*Y bF", lines2[y_idx2])
# )[1]
# 
# countSpecies <- function(header_line, lines, y_idx){
#   
#   start <- header_line
#   
#   idx <- which(y_idx == header_line)
#   
#   end <- if(idx < length(y_idx)){
#     y_idx[idx + 1] - 1
#   } else {
#     length(lines)
#   }
#   
#   sum(
#     grepl("^\\s+[A-Za-z]{2,3}v\\s",
#           lines[start:end])
#   )
# }
# sapply(
#   bf_headers,
#   countSpecies,
#   lines = lines2,
#   y_idx = y_idx2
# )
# 
# 
# 
# 
# 
# # ============================================================
# # Third YLD file: District1_sub_all.yld
# # ============================================================
# 
# lines3 <- readLines(
#   "E:/EasternCanadaClassifier/data/NL/District 1.yld",
#   warn = FALSE
# )
# 
# length(lines3)
# 
# # ------------------------------------------------------------
# # Find all yield table headers
# # ------------------------------------------------------------
# 
# y_idx3 <- grep("^\\*Y", lines3)
# 
# length(y_idx3)
# 
# # ------------------------------------------------------------
# # Count communities
# # ------------------------------------------------------------
# 
# table(
#   sub("^\\*Y\\s+(\\S+).*", "\\1", lines3[y_idx3])
# )
# 
# # ------------------------------------------------------------
# # Select only bF yield tables
# # ------------------------------------------------------------
# 
# bf_headers3 <- y_idx3[
#   sub("^\\*Y\\s+(\\S+).*", "\\1", lines3[y_idx3]) == "bF"
# ]
# 
# length(bf_headers3)
# 
# # ------------------------------------------------------------
# # Count species in each bF table
# # ------------------------------------------------------------
# 
# countSpecies <- function(header_line, lines, y_idx){
#   
#   start <- header_line
#   
#   idx <- which(y_idx == header_line)
#   
#   end <- if(idx < length(y_idx)){
#     y_idx[idx + 1] - 1
#   } else {
#     length(lines)
#   }
#   
#   sum(
#     grepl(
#       "^\\s+[A-Za-z]{2,3}v\\s",
#       lines[start:end]
#     )
#   )
# }
# 
# sapply(
#   bf_headers3,
#   countSpecies,
#   lines = lines3,
#   y_idx = y_idx3
# )
# 
# # ------------------------------------------------------------
# # Inspect the first bF table
# # ------------------------------------------------------------
# 
# start <- bf_headers3[1]
# 
# idx <- which(y_idx3 == start)
# 
# end <- y_idx3[idx + 1] - 1
# 
# lines3[start:end]
# 
# # ------------------------------------------------------------
# # List species names in the first bF table
# # ------------------------------------------------------------
# 
# grep(
#   "^\\s+[A-Za-z]{2,3}v\\s",
#   lines3[start:end],
#   value = TRUE
# )
# 
# 
# 
# 
# 
# 
# 
# # ============================================================
# # Fourth YLD file: NpMainLong_sub_all.yld
# # ============================================================
# 
# lines4 <- readLines(
#   "E:/EasternCanadaClassifier/data/NL/NpMainLong_sub_all.yld",
#   warn = FALSE
# )
# 
# length(lines4)
# 
# # ------------------------------------------------------------
# # Find all yield table headers
# # ------------------------------------------------------------
# 
# y_idx4 <- grep("^\\*Y", lines4)
# 
# length(y_idx4)
# 
# # ------------------------------------------------------------
# # Count communities
# # ------------------------------------------------------------
# 
# table(
#   sub("^\\*Y\\s+(\\S+).*", "\\1", lines4[y_idx4])
# )
# 
# # ------------------------------------------------------------
# # Select only bF yield tables
# # ------------------------------------------------------------
# 
# bf_headers4 <- y_idx4[
#   sub("^\\*Y\\s+(\\S+).*", "\\1", lines4[y_idx4]) == "bF"
# ]
# 
# length(bf_headers4)
# 
# # ------------------------------------------------------------
# # Count species in each bF table
# # ------------------------------------------------------------
# 
# sapply(
#   bf_headers4,
#   countSpecies,
#   lines = lines4,
#   y_idx = y_idx4
# )
# 
# # ------------------------------------------------------------
# # Inspect first bF table
# # ------------------------------------------------------------
# 
# start <- bf_headers4[1]
# 
# idx <- which(y_idx4 == start)
# 
# end <- y_idx4[idx + 1] - 1
# 
# lines4[start:end]
# 
# grep(
#   "^\\s+[A-Za-z]{2,3}v\\s",
#   lines4[start:end],
#   value = TRUE
# )
# 
# 
# 
# 
# # ============================================================
# # Fifth YLD file: West_sub_all.yld
# # ============================================================
# 
# lines5 <- readLines(
#   "E:/EasternCanadaClassifier/data/NL/West_sub_all.yld",
#   warn = FALSE
# )
# 
# length(lines5)
# 
# # ------------------------------------------------------------
# # Find all yield table headers
# # ------------------------------------------------------------
# 
# y_idx5 <- grep("^\\*Y", lines5)
# 
# length(y_idx5)
# 
# # ------------------------------------------------------------
# # Count communities
# # ------------------------------------------------------------
# 
# table(
#   sub("^\\*Y\\s+(\\S+).*", "\\1", lines5[y_idx5])
# )
# 
# # ------------------------------------------------------------
# # Select only bF yield tables
# # ------------------------------------------------------------
# 
# bf_headers5 <- y_idx5[
#   sub("^\\*Y\\s+(\\S+).*", "\\1", lines5[y_idx5]) == "bF"
# ]
# 
# length(bf_headers5)
# 
# # ------------------------------------------------------------
# # Count species in each bF table
# # ------------------------------------------------------------
# 
# sapply(
#   bf_headers5,
#   countSpecies,
#   lines = lines5,
#   y_idx = y_idx5
# )
# 
# # ------------------------------------------------------------
# # Inspect first bF table
# # ------------------------------------------------------------
# 
# start <- bf_headers5[1]
# 
# idx <- which(y_idx5 == start)
# 
# end <- y_idx5[idx + 1] - 1
# 
# lines5[start:end]
# 
# grep(
#   "^\\s+[A-Za-z]{2,3}v\\s",
#   lines5[start:end],
#   value = TRUE
# )
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
######ta inja dorost
parseSpecies <- function(lines, startLine) {
  
  stopifnot(
    is.character(lines),
    length(startLine) == 1
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
######parse volume
parseVolumes <- function(lines, speciesInfo) {
  
  stopifnot(
    is.character(lines),
    is.list(speciesInfo)
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
#################################rewrite yiled table
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
