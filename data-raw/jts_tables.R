# Aim: get relevant summaries at LA level

library(rvest)
library(tidyverse)

u = "https://www.gov.uk/government/statistical-data-sets/journey-time-statistics-data-tables-jts"
html = xml2::read_html(u)

links = html %>%
  rvest::xml_nodes(".attachment-inline .govuk-link")

table_title = rvest::html_text(links)
table_url = rvest::html_attr(links, "href")

accessibility_table = tibble::tibble(
  table_title,
  table_url
)
table_codes_url = str_sub(accessibility_table$table_url, start = 104, 110)
table_codes_url[1:2]
accessibility_table$table_code = table_codes_url

data_dir = jts:::jts_download_directory()
i = 1
# pre-downloaded from prior step, do not run
# for(i in i:nrow(accessibility_table)) {
#   u = accessibility_table$table_url[i]
#   file_to_download = basename(u)
#   message("Downloading ", file_to_download)
#   download.file(u, file.path(data_dir, file_to_download))
# }

f = file.path(data_dir, "jts0101.ods")
o = jts:::jts_download_directory()
old_directory = setwd(o)
system("libreoffice --headless --convert-to csv --outdir . *.xlsx")
system("libreoffice --headless --convert-to csv --outdir . 1*.ods") # only converts 1st sheet
# system("libreoffice --headless --convert-to xlsx --outdir . *.ods")

list.dirs()
list.files("jts1010-xlsxcsv/")

system("xlsx2csv -a jts0102.xlsx xls2csv") # works, duplicate file names present...

i = 1
setwd("~/hd/data/jts/csvs/")
for(i in 1:nrow(accessibility_table)) {
  u = accessibility_table$table_url[i]
  ods_file_name = basename(u)
  xlsx_file_name = gsub(pattern = ".ods", replacement = ".xlsx", x = ods_file_name)
  folder_name = gsub(pattern = ".ods", "", ods_file_name)
  msg = paste0("xlsx2csv -a ", xlsx_file_name, " ", folder_name)
  message("Running ", msg)
  system(msg)
  f = list.files(folder_name)
  print(f)
}


list.files(data_dir, pattern = "csv")

folders = list.dirs()
nchar_folders = nchar(folders)
folders_csv = folders[nchar_folders == 9]

dir.create("jts_csv_files")
i = folders_csv[1]
for(i in folders_csv) {
  f = list.files(path = i, full.names = TRUE)
  base_names = basename(f)
  new_file_names = file.path("jts_csv_files", basename(paste0(i, "-", base_names)))
  file.copy(f, new_file_names)
}

list.files("jts_csv_files/")
zip(zipfile = "jts_csv_files.zip", files = "jts_csv_files/")
file.size("jts_csv_files.zip") / 1e6 # 52 MB
d = readr::read_csv("jts_csv_files/jts0101-JTS0101.csv", skip = 7)
d_meta = readr::read_csv("jts_csv_files/jts0501-Metadata.csv")
View(d_meta)
# confirmed: useful data
d = readr::read_csv("jts_csv_files/jts0501-2017.csv", skip = 6)


piggyback::pb_upload("jts_csv_files.zip", repo = "cyipt/acton")

jts_csv_path = file.path(jts:::jts_download_directory(), "jts_csv_files")
f = list.files(jts_csv_path)
piggyback::pb_upload(f, repo = "cyipt/acton")


# join onto access tables dataset -----------------------------------------

accessibility_table
accessibility_table$table_url[19]
table_codes_csv = basename(f)
accessibility_table$table_code = table_codes_url
accessibility_table_csvs = tibble::tibble(
  table_code = str_sub(f, start = 1, end = 7),
  csv_file_name = f
)

length(table_codes)
nrow(accessibility_table)
accessibility_table$table_url
jts_tables = left_join(accessibility_table_csvs, accessibility_table)

csv_url = paste0("https://github.com/itsleeds/jts/releases/download/1/", jts_tables$csv_file_name)

# readr::read_csv(csv_url[2])

jts_tables$csv_url = csv_url


jts_tables$csv_file_name
jts_tables$csv_url
jts_tables$table_url
jts_tables$table_code



remove_jts_content = function(
  x,
  p1 = "Travel time, destination and origin indicators ",
  p2 = " by mode of travel",
  p3 = " and local authority, England+.*",
  p4 = ", Lower Super Output Area \\(LSOA\\), England+.*",
  p5 = " by cycle and car, local authority, England",
  p6 = "to |for ",
  p7 = "Average minimum travel time reach the nearest key services",
  p8 = "Number of sites available within selected journey times by key services",
  p9 = "Service users with access key services within selected journey times"
) {
  x = gsub(pattern = p1, replacement = "", x = x, fixed = TRUE)
  x = gsub(pattern = p2, replacement = "", x = x)
  x = gsub(pattern = p3, replacement = "", x = x)
  x = gsub(pattern = p4, replacement = "", x = x)
  x = gsub(pattern = p5, replacement = "", x = x)
  x = gsub(pattern = p6, replacement = "", x = x)
  x = gsub(pattern = p7, replacement = "Time", x = x)
  x = gsub(pattern = p8, replacement = "N sites", x = x)
  x = gsub(pattern = p9, replacement = "Service users", x = x)
  x
}

# remove_jts_content(x)
jts_tables_updated = jts_tables %>%
  mutate(service = remove_jts_content(table_title))
jts_tables_updated
table(jts_tables_updated$service)
stringr::str_extract(string = jts_tables$csv_file_name, pattern = "-\\d{4}") # works
jts_tables_updated$year = as.integer(stringr::str_extract(string = jts_tables$csv_file_name, pattern = "(?<=-)\\d{4}"))
jts_tables = jts_tables_updated
# jts_tables$year[grepl(pattern = "meta", x = jts_tables$csv_file_name, ignore.case = TRUE)] = "meta"
jts_tables$year[is.na(jts_tables$year)] = "meta"
table(jts_tables$table_code, jts_tables$year) # all good, no duplicates
usethis::use_data(jts_tables, overwrite = TRUE)
