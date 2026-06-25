standardizeYieldCurve <- function(
    ages,
    volumes,
    maxAge
){
  
  # ------------------------------------------------------
  # assumptions:
  # - ages are sorted
  # - ages are unique
  # - no missing values
  # ------------------------------------------------------
  browser()
  annual <- approx(
    x = ages,
    y = volumes,
    xout = 1:maxAge,
    method = "linear",
    rule = 1
  )$y
  
  annual[is.na(annual)] <- 0
  # ------------------------------------------------------
  # ages before first observed age are zero
  # ------------------------------------------------------
  
  first_age <- min(ages)
  
  if (first_age > 1) {
    annual[1:(first_age - 1)] <- 0
  }
  
  # ------------------------------------------------------
  # prevent negative interpolated values
  # ------------------------------------------------------
  
  annual[annual < 0] <- 0
  
  # ------------------------------------------------------
  # return standardized annual yield table
  # ------------------------------------------------------
  
  return(
    data.table(
      age = 1:maxAge,
      volume = annual
    )
  )
}