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

standardizeYieldTables <- function(sim,
                                   maxAge = 200) {
  
  print("===== START STANDARDIZE =====")
  
  print(names(sim$rawYieldTables))
  
  print(names(sim$rawYieldTables$ON))
  
  print(class(sim$rawYieldTables$ON$`3e`))
  
  print(head(sim$rawYieldTables$ON$`3e`))
  
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
      
      # ---------------------------------------------------
      # unique curve IDs
      # ---------------------------------------------------
      
      curve_ids <- unique(region_tables$CURVENO)
      
      # ---------------------------------------------------
      # loop through curves
      # ---------------------------------------------------
      
      for (cid in curve_ids) {
        
        curve_dt <- region_tables[
          CURVENO == cid
        ]
        print("===== DEBUG STANDARDIZE =====")
        
        print(jur)
        
        print(reg)
        
        print(cid)
        
        print(class(curve_dt))
        
        print(dim(curve_dt))
        
        print(names(curve_dt))
        
        print(head(curve_dt))
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
        # exclude non-volume numeric columns
        # -------------------------------------------------
        
        exclude_cols <- c(
          "CURVENO",
          age_col,
          "total"
        )
        
        volume_cols <- setdiff(
          numeric_cols,
          exclude_cols
        )
        
        # -------------------------------------------------
        # -------------------------------------------------
        
       # curve_dt[
         # ,
        #  total_volume := rowSums(
          #  .SD,
           # na.rm = TRUE
          #),
          #.SDcols = volume_cols
        #]
        # -------------------------------------------------
        # sort by age
        # -------------------------------------------------
        
        curve_dt <- curve_dt[
          order(get(age_col))
        ]
        # -------------------------------------------------
        # standardize each species column separately
        # -------------------------------------------------
        
        species_tables <- list()
        
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
        # map species columns to analysis groups
        # -------------------------------------------------
        
        mapSpeciesGroups <- sim$mapSpeciesGroups        
        # -------------------------------------------------
        # validate columns
        # -------------------------------------------------
        
        # -------------------------------------------------------
        # ON tables are already grouped
        # so grouped species columns are valid
        # -------------------------------------------------------
        
        unknown_cols <- character(0)
        # -------------------------------------------------
        # unique analysis groups
        # -------------------------------------------------
        
        group_names <- unique(
          unname(mapSpeciesGroups)
        )
        
        # -------------------------------------------------
        # initialize reduced table
        # -------------------------------------------------
        
        reduced_dt <- data.table(
          age = yt_standard$age
        )
        
        for (grp in group_names) {
          
          reduced_dt[[grp]] <- 0
        }
        
        # -------------------------------------------------
        # aggregate species into groups
        # -------------------------------------------------
        
        for (sp_col in volume_cols) {
          
          if (!is.null(mapSpeciesGroups[[sp_col]])) {
            
            grp <- mapSpeciesGroups[[sp_col]]
            
          } else {
            
            grp <- sp_col
          }
          
          reduced_dt[[grp]] <-
            reduced_dt[[grp]] +
            yt_standard[[sp_col]]
        }
        
        # -------------------------------------------------
        # remove all-zero groups
        # -------------------------------------------------
        
        keep_species <- names(reduced_dt)[
          names(reduced_dt) != "age" &
            sapply(
              reduced_dt[, !("age"), with = FALSE],
              function(x) any(x > 0)
            )
        ]
        
        reduced_dt <- reduced_dt[
          ,
          c("age", keep_species),
          with = FALSE
        ]
        # -------------------------------------------------
        # -------------------------------------------------
        
       # yt_standard <- standardizeYieldCurve(
        #  ages = curve_dt[[age_col]],
         # volumes = curve_dt$total_volume,
          #maxAge = maxAge
        #)
        
        # -------------------------------------------------
        # store standardized curve
        # -------------------------------------------------
        
        yieldTables[[jur]][[reg]][[as.character(cid)]] <- 
          reduced_dt
      }
    }
  }
  
  # -------------------------------------------------------
  # save standardized tables
  # -------------------------------------------------------
  
  sim$yieldTables <- yieldTables
  
  # -------------------------------------------------------
  # return output
  # -------------------------------------------------------
  
  return(yieldTables)
}