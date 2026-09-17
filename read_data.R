library(readxl)

folder_path <-  "C:/Users/rkrum/Documents/Summer 2026/Consulting Center/UDOT/Data"

excel_files <- list.files(
  path = folder_path,
  pattern = "\\.(xlsx|xls)$",
  full.names = TRUE,
  ignore.case = TRUE
)

for (file in excel_files) {
  file_name <- basename(file)
   first_four <- substr(file_name, 1, 4)
  if (!grepl("^\\d{4}$", first_four)) {
    warning("Skipped file because its first four characters are not numbers: ",
            file_name)
    next
  }
  object_name <- paste0("int_", first_four)
  assign(
    x = object_name,
    value = read_excel(file, sheet = "R Consolidated"),
    envir = .GlobalEnv
  )
}


dataset_names <- ls(pattern = "^int_\\d{4}$")

column_names <- lapply(dataset_names, function(x) {
  colnames(get(x))
})

names(column_names) <- dataset_names

