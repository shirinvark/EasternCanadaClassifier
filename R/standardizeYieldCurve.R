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
  if (ages[1] > 1 && volumes[1] == 0) {
    ages <- c(1, ages[-1])
    volumes <- c(0, volumes[-1])
  }
  cat("AGES AFTER FIX:\n")
  print(ages[1:5])
  
  cat("VOLUMES AFTER FIX:\n")
  print(volumes[1:5])
  
  sp <- splinefun(
    x = ages,
    y = volumes,
    method = "hyman"
  )
  
  annual <- sp(1:maxAge)
  
  lastAge <- max(ages)
  lastVol <- tail(volumes, 1)
  
  if (lastAge < maxAge) {
    annual[(lastAge + 1):maxAge] <- lastVol
  }
  
  annual[annual < 0] <- 0
  
  annual[is.na(annual)] <- 0
  # ------------------------------------------------------
  # ages before first observed age are zero
  # ------------------------------------------------------
  
  first_age <- min(ages)
  
  if (first_age > 1) {
    annual[1:(first_age - 1)] <- 0
  }
  
 
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