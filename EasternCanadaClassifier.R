## Everything in this file and any files in the R directory are sourced during `simInit()`
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "EasternCanadaClassifier",
  description = "Classifies harvestable landbase into analysis units and computes area summaries.",
  keywords = c("analysis units", "classification", "harvestable landbase", "AAC"),
  authors = structure(list(list(given = c("Shirin"), family = "Varkouhi", role = c("aut", "cre"), email = "shirin.varkuhi@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(EasternCanadaClassifier = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "EasternCanadaClassifier.Rmd"),
  reqdPkgs = c(
    "SpaDES.core (>= 3.0.4)",
    "terra",
    "data.table"
  ),
  parameters = bindrows(
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric",NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    expectsInput(
      "cohortData",
      objectClass = "data.frame",
      desc = "LandR cohort data containing species biomass"
    ),
    expectsInput(
      "pixelGroupMap",
      objectClass = "SpatRaster",
      desc = "Raster identifying pixelGroup IDs"
    ),
    expectsInput(
      "harvestableFraction",
      objectClass = "SpatRaster",
      desc = "Fraction of each pixel that is harvestable (from Landbase module)"
    )
  ),
  
  outputObjects = bindrows(
    createsOutput(
      "analysisUnitMap",
      "SpatRaster",
      "Raster assigning harvestable pixels to analysis units."
    )
  ))
   )
doEvent.EasternCanadaClassifier <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(noEventWarning(sim))
  )
  invisible(sim)
}




.inputObjects <- function(sim) {
  
  if (!("pixelGroupMap" %in% names(sim))) {
    
    message("Creating fake pixelGroupMap")
    
    r <- terra::rast(nrows=10, ncols=10,
                     xmin=0, xmax=1000,
                     ymin=0, ymax=1000)
    
    terra::values(r) <- sample(1:20, 100, replace = TRUE)
    
    sim$pixelGroupMap <- r
  }
  
  
  if (!("cohortData" %in% names(sim))) {
    
    message("Creating fake cohortData")
    
    sim$cohortData <- data.table::data.table(
      pixelGroup = sample(1:20, 200, replace = TRUE),
      speciesCode = sample(
        c("Abie_bal","Pice_mar","Pinu_ban","Pinu_res","Pinu_str","Acer_sah"),
        200,
        replace = TRUE
      ),
      age = sample(1:120, 200, replace = TRUE),
      B = runif(200, 1, 50)
    )
    
  }
  
  return(sim)
}  # ! ----- STOP EDITING ----- ! #
  


