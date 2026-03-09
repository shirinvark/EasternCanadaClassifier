list.files("D:/GrowthSurves_STEVE")
list.files("D:/GrowthSurves_STEVE", recursive = TRUE)
readLines(
  "D:/GrowthSurves_STEVE/yieldTables/data/AB/AlPac AME Mixedwood VolTabs.vol",
  n = 10
)
file <- "D:/GrowthSurves_STEVE/yieldTables/data/AB/AlPac AME Mixedwood VolTabs.vol"

lines <- readLines(file)

header <- strsplit(lines[1], " ")[[1]]

nCurves <- as.numeric(gsub("#","",header[1]))
nAges <- as.numeric(header[2])

yieldTables <- do.call(
  rbind,
  lapply(lines[2:(nCurves+1)], function(x)
    as.numeric(strsplit(trimws(x), "\\s+")[[1]])
  )
)

yieldTables
matplot(t(yieldTables), type="l", lty=1)
