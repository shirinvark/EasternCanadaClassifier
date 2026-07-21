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
 
  
  
  #in barfaye ontaro javab dad vali vase Nl na.bayad avaz konim bebinim Nl dorost mishavad ya na
  # sp <- splinefun(
  #   x = ages,
  #   y = volumes,
  #   method = "natural"
  # )
  # 
  # annual <- sp(1:maxAge)
  
  positive <- which(volumes > 0)
  
  if (length(positive) >= 2 && positive[1] > 1) {
    
    firstPositive <- positive[1]
    
    annual <- numeric(maxAge)
    
    sp <- splinefun(
      x = ages[firstPositive:length(ages)],
      y = volumes[firstPositive:length(volumes)],
      method = "natural"
    )
    
    annual[ages[firstPositive]:maxAge] <-
      sp(ages[firstPositive]:maxAge)
    
  } else {
    
    sp <- splinefun(
      x = ages,
      y = volumes,
      method = "natural"
    )
    
    annual <- sp(1:maxAge)
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
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