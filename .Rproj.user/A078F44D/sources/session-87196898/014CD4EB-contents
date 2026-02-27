library(terra)

# Create a simple 10x10 raster template
planningRaster <- rast(nrows=10, ncols=10, xmin=0, xmax=1000, ymin=0, ymax=1000)
values(planningRaster) <- 1

# Fake landcover (classes 210, 220, 230)
landcover <- planningRaster
values(landcover) <- sample(c(210,220,230), 100, replace=TRUE)

# Fake stand age
standAge <- planningRaster
values(standAge) <- sample(1:120, 100, replace=TRUE)

# Fake harvestable fraction (0–1)
harvestable <- planningRaster
values(harvestable) <- runif(100)

# Create fake Landbase structure
Landbase <- list(
  baseData = list(
    planningRaster = planningRaster,
    landcover = landcover,
    standAge = standAge
  ),
  fractional = list(
    harvestableFraction = harvestable
  )
)

library(SpaDES.core)

sim <- simInit(
  times = list(start=0, end=1),
  params = list(),
  modules = "EasternCanadaClassifier",
  objects = list(Landbase = Landbase),
  paths = list(modulePath = "E:/yourModulePath")  # مسیر ماژولت
)
