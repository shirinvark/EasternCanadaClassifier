# =========================================================
# STANDARDIZE YIELD CURVE
# ------------------------------------------------------
# Converts yield tables with irregular age classes
# (e.g. 5-year, 10-year, mixed intervals)
# into a standardized annual time-step format.
#
# Output format:
#   age | volume
#
# This standardized structure is required by the
# AAC/Hanzlik workflow.
#
# Example:
# Original:
#   Age = 0,10,20,30...
#
# Standardized:
#   Age = 1,2,3,4,...,200
#
# Linear interpolation is used between observed
# yield table ages.
#
# rule = 1:
# values outside observed age range become NA
# and are later converted to 0
# =========================================================

standardizeYieldCurve <- function(
    ages,
    volumes,
    maxAge = 200
){
  # ------------------------------------------------------
  # assumptions:
  # - ages are sorted
  # - ages are unique
  # - no missing values
  # -------------------------------------------------------
  annual <- approx(
    x = ages,
    y = volumes,
    xout = 1:maxAge,
    method = "linear",
    rule = 1
  )$y
  # -------------------------------------------------------
  # replace ages outside observed range with 0
  # -------------------------------------------------------
  
  annual[is.na(annual)] <- 0
  
  # -------------------------------------------------------
  # prevent negative interpolated values
  # -------------------------------------------------------
  
  annual[annual < 0] <- 0
  # -------------------------------------------------------
  # return standardized annual yield table
  # -------------------------------------------------------
  
  return(
    data.table(
      age = 1:maxAge,
      volume = annual
    )
  )
}