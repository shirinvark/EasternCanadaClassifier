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
  
  print("===== START STANDARDIZE =====")
  
  print(names(sim$rawYieldTables))
  
  print(names(sim$rawYieldTables$ON))
  
  print(class(sim$rawYieldTables$ON$`3e`))
  
  print(head(sim$rawYieldTables$ON$`3e`))
  
  # -------------------------------------------------------
  # raw processed yield tables
  # -------------------------------------------------------
  
  rawYieldTables <- list(
    ON = sim$yield_by_region
  )
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
    cat("\n===== JUR DEBUG =====\n")
    
    print(jur)
    
    print(class(jur_tables))
    
    print(names(jur_tables))
    regions <- names(jur_tables)
    
    for (reg in regions) {
      
      region_tables <- jur_tables[[reg]]
      # =====================================================
      # detect list-style yield tables
      # =====================================================
      
      isListStyleYield <- function(x) {
        
        is.list(x[[1]]) &&
          is.numeric(x[[1]][[1]])
      }
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
          
          # -------------------------------------------------
          # remove zero columns
          # -------------------------------------------------
          
          species_cols <- setdiff(
            names(yt_standard),
            "age"
          )
          
          keep_species <- species_cols[
            sapply(
              yt_standard[
                ,
                ..species_cols
              ],
              function(x) any(x > 0)
            )
          ]
          
          reduced_dt <- yt_standard
          
          # -------------------------------------------------
          # store
          # -------------------------------------------------
          
          yieldTables[[jur]][[reg]][[cid]] <- 
            reduced_dt
        }
        
        next
      }
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
        
        # remove all-zero species columns
        
        species_cols <- setdiff(
          names(yt_standard),
          "age"
        )
        
        keep_species <- species_cols[
          sapply(
            yt_standard[
              ,
              ..species_cols
            ],
            function(x) any(x > 0)
          )
        ]
        
        reduced_dt <- yt_standard[
          ,
          c("age", keep_species),
          with = FALSE
        ]        # -------------------------------------------------
        # -------------------------------------------------
        
       # yt_standard <- standardizeYieldCurve(
        #  ages = curve_dt[[age_col]],
         # volumes = curve_dt$total_volume,
          #maxAge = maxAge
        #)
        
        # -------------------------------------------------
        # store standardized curve
        # -------------------------------------------------
        AU <- unique(curve_dt$AU)
        
        AU <- AU[!is.na(AU)][1]
        
        yieldTables[[jur]][[reg]][[as.character(cid)]] <- reduced_dt
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