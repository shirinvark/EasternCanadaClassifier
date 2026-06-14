# ======================================================
# STANDARDIZE YIELD TABLES
# ---------------------------------------------------------
# Converts heterogeneous yield tables into a standardized
# annual format for AAC/Hanzlik calculations.
#
# Input:
#   sim$rawYieldTables
#
# Output:
#   sim$yieldTables
#
# This function:
#   - loops through jurisdictions
#   - loops through regions
#   - loops through curves
#   - identifies numeric yield columns
#   - aggregates total stand volume
#   - standardizes age classes to annual ages
#
# File reading and jurisdiction-specific processing
# should occur upstream in EasternCanadaClassifier.
# =========================================================

standardizeYieldTables <- function(
    sim,
    maxAge
) {
  
  
  # -------------------------------------------------------
  # raw processed yield tables
  # -------------------------------------------------------
  
  rawYieldTables <- sim$rawYieldTables
  # -------------------------------------------------------
  # jurisdiction names
  # -------------------------------------------------------
  
  jurisdictions <- names(rawYieldTables)
  
  # -------------------------------------------------------
  # output object
  # -------------------------------------------------------
  
  yieldTables <- list()
  
  dropZeroSpecies <- function(dt) {
    
    species_cols <- setdiff(
      names(dt),
      "age"
    )
    
    keep_species <- species_cols[
      sapply(
        dt[, ..species_cols],
        function(x) any(x > 0)
      )
    ]
    
    dt[
      ,
      c("age", keep_species),
      with = FALSE
    ]
  }
  
  
  isListStyleYield <- function(x) {
    
    is.list(x[[1]]) &&
      is.numeric(x[[1]][[1]])
  }
  # -------------------------------------------------------
  # loop through jurisdictions
  # -------------------------------------------------------
  
  for (jur in jurisdictions) {
    
    jur_tables <- rawYieldTables[[jur]]
    
    # -----------------------------------------------------
    # loop through regions
    # -----------------------------------------------------
    
    regions <- names(jur_tables)
    
    for (reg in regions) {
      
      region_tables <- jur_tables[[reg]]
      
      # =====================================================
      # LIST-STYLE CURVES
      # =====================================================
      
      if (isListStyleYield(region_tables)) {
        
        curve_ids <- names(region_tables)
        
        for (cid in curve_ids) {
          
          curve_list <- region_tables[[cid]]
          
          # -------------------------------------------------
          # build ages
          # -------------------------------------------------
          
          n <- length(curve_list[[1]])
          
          ages <- seq(
            10,
            by = 10,
            length.out = n
          )
          
          # -------------------------------------------------
          # create curve table
          # -------------------------------------------------
          
          curve_dt <- data.table(
            age = ages
          )
          
          for (sp_col in names(curve_list)) {
            
            curve_dt[[sp_col]] <- curve_list[[sp_col]]
          }
          
          # -------------------------------------------------
          # standardize each species
          # -------------------------------------------------
          
          species_tables <- list()
          
          for (sp_col in names(curve_list)) {
            
            yt_standard <- standardizeYieldCurve(
              ages = curve_dt$age,
              volumes = curve_dt[[sp_col]],
              maxAge = maxAge
            )
            
            species_tables[[sp_col]] <- yt_standard$volume
          }
          
          # -------------------------------------------------
          # final annual table
          # -------------------------------------------------
          
          yt_standard <- data.table(
            age = 1:maxAge
          )
          
          for (sp_col in names(species_tables)) {
            
            yt_standard[[sp_col]] <- species_tables[[sp_col]]
          }
          
          reduced_dt <- dropZeroSpecies(
            yt_standard
          )         
          # -------------------------------------------------
          # store
          # -------------------------------------------------
          
          yieldTables[[as.character(cid)]] <- reduced_dt
        }
        
        next
      }
      # ---------------------------------------------------
      # unique curve IDs
      # ---------------------------------------------------
      
      # ---------------------------------------------------
      # curve identifier
      # ---------------------------------------------------
      
      if ("CURVENO" %in% names(region_tables)) {
        
        curve_ids <- unique(
          region_tables$CURVENO
        )
        
        subsetCurve <- function(dt, cid) {
          dt[CURVENO == cid]
        }
        
      } else if ("AU" %in% names(region_tables)) {
        
        curve_ids <- unique(
          region_tables$AU
        )
        
        subsetCurve <- function(dt, cid) {
          dt[AU == cid]
        }
        
      } else {
        
        stop(
          "No curve identifier found"
        )
        
      }
      
      # ---------------------------------------------------
      # loop through curves
      # --------------------------------------------
      
      for (cid in curve_ids) {
        
        curve_dt <- subsetCurve(
          region_tables,
          cid
        )
        
        # -------------------------------------------------
        # first column assumed to be ages
        # -------------------------------------------------
        
        age_candidates <- names(curve_dt)[
          grepl(
            "age|ac10",
            names(curve_dt),
            ignore.case = TRUE
          )
        ]
        
        age_col <- age_candidates[1]
        
        if (is.na(age_col)) {
          stop("No age column found")
        }       
        # -------------------------------------------------
        # numeric columns
        # -------------------------------------------------
        
        numeric_cols <- names(curve_dt)[
          sapply(curve_dt, is.numeric)
        ]
        
        exclude_cols <- c(
          "CURVENO",
          age_col,
          "total",
          "age_diff"
        )
        
        volume_cols <- setdiff(
          numeric_cols,
          exclude_cols
        )
        
        # -------------------------------------------------
        # sort by age
        # -------------------------------------------------
        
        curve_dt <- curve_dt[
          order(get(age_col))
        ]
        # ------------------------------------------------
        # standardize each species column separately
        # -------------------------------------------------
        
        species_tables <- list()
        
        if (nrow(curve_dt) < 2) {
          
          message(
            "Skipping curve with <2 rows: ",
            cid
          )
          
          next
        }
        
        for (sp_col in volume_cols) {
          yt_standard <- standardizeYieldCurve(
            ages = curve_dt[[age_col]],
            volumes = curve_dt[[sp_col]],
            maxAge = maxAge
          )
          
          species_tables[[sp_col]] <- yt_standard$volume
        }
        
        # -------------------------------------------------
        # create standardized annual table
        # -------------------------------------------------
        
        yt_standard <- data.table(
          age = 1:maxAge
        )
        
        for (sp_col in names(species_tables)) {
          
          yt_standard[[sp_col]] <- species_tables[[sp_col]]
        }
        
        
        
        
        
        
        # -------------------------------------------------
        # -------------------------------------------------
        # remove all-zero species columns
        # -------------------------------------------------
        reduced_dt <- dropZeroSpecies(
          yt_standard
        )
        # -------------------------------------------------
        # -------------------------------------------------
        # store standardized curve
        # -------------------------------------------------
        
        
        yieldTables[[as.character(cid)]] <- reduced_dt
      }
    }
  }
  
  # --------------------------------------------------
    # save standardized tables
    # -------------------------------------------------------
  
  sim$yieldTables <- yieldTables
  
  # -------------------------------------------------------
  # return output
  # -------------------------------------------------------
  
  return(yieldTables)
}