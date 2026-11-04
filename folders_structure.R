base_dir <- "E:/EasternCanadaClassifier/data"

provinces <- c(
  "ON", "NL", "QC",
  "NB", "NS", "PE",
  "AB", "BC", "MB", "SK"
)

for (prov in provinces) {
  
  dir.create(
    file.path(base_dir, prov, "Provincial"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    file.path(base_dir, prov, "National"),
    recursive = TRUE,
    showWarnings = FALSE
  )
}
base_dir <- "E:/EasternCanadaClassifier/data"

old_dir <- file.path(base_dir, "NL", "YTF")
new_dir <- file.path(base_dir, "NL", "Provincial", "YTF")

dir.create(new_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(
  old_dir,
  pattern = "\\.yld$",
  full.names = TRUE
)

stopifnot(length(files) == 5)

copied <- file.copy(
  from = files,
  to = new_dir,
  overwrite = FALSE
)

stopifnot(all(copied | file.exists(file.path(new_dir, basename(files)))))

list.files(new_dir)
base_dir <- "E:/EasternCanadaClassifier/data"

old_dir <- file.path(base_dir, "ON", "YTF")

new_dir <- file.path(
  base_dir, "ON", "Provincial", "YTF"
)

dir.create(
  new_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

files <- list.files(
  old_dir,
  pattern = "\\.(csv|txt)$",
  full.names = TRUE,
  ignore.case = TRUE
)

stopifnot(length(files) == 6)

copied <- file.copy(
  from = files,
  to = new_dir,
  overwrite = FALSE
)

stopifnot(
  all(file.exists(file.path(new_dir, basename(files))))
)

list.files(new_dir)