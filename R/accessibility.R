#' Get accessibility data for particular year and table number
#'
#' The DfT's Journey Time Statistics are outlined here on the
#' [gov.uk website](https://www.gov.uk/government/statistical-data-sets/journey-time-statistics-data-tables-jts).
#'
#' The function uses a data frame of existing tables, created by the script 'accessibility_tables.R' in the data-raw folder.
#'
#' The tables starting JTS01 to JTS03 provide national overview data.
#' The tables JTS0401 to JTS0409 provide data at the Local Authority level.
#' The tables JTS0501 to JTS0509 provide the same data at the LSOA level.
#' The tables beginning JTS09 provide data on accessibility to transport hubs.
#' And the tables beginning JTS10 contain add other variables.
#'
#' Data is provided every year from 2014 to 2019 in many cases
#' @param table The title of the table, e.g. "jts0501"
#' @param year The year, e.g. 2017. If "meta" is supplied, get metadata.
#' @param u_csv The base url of the files
#' @param clean Should the dataset be cleaned with `jts_clean`?
#' @param ods Download and read-in raw ODS files? `FALSE` by default
#' @param output_format Which file format should be returned?
#'   `data_frame` by default; `sf` optional.
#' @export
#' @examples
#' head(jts_tables)
#' head(jts_tables$table_title)
#' metadata = get_jts_data(table = "jts0101", year = "meta")
#' head(metadata)
#' # uncomment on released version
#' jts0401_2017 = get_jts_data(table = "jts0401", year = 2017)
#' head(jts0401_2017[1:7])
#' jts0401_2014 = get_jts_data(table = "jts0401", year = 2014)
#' head(jts0401_2014[1:7])
#' # jts0401_2017_raw = get_jts_data(table = "jts0401", year = 2017, clean = FALSE)
#' # head(jts0401_2017_raw[1:7])
#' # jts0501_2017 = get_jts_data(table = "jts0501", year = 2017)
#' # head(jts0501_2017[1:7])
#' # jts0501_2017 = get_jts_data(table = "jts0501", year = 2017, output_format = "sf")
#' # head(jts0501_2017)
#' jts0501_meta = get_jts_data(table = "jts0501", year = "meta")
#' head(jts0501_meta)
get_jts_data = function(table, year = 2017, u_csv = jts_url(), clean = TRUE, ods = FALSE, output_format = "data_frame") {
  if(ods) {
    d = read_jts_local(table, sheet = as.character(year), clean = clean)
    message("Reading in file ", u_csv)
  } else {
    # browser()
    s = jts::jts_tables$year == year & jts::jts_tables$table_code == table
    csv_filename = jts::jts_tables$csv_file_name[s]
    full_csv = file.path(u_csv, csv_filename)
    suppressMessages({
      suppressWarnings({
        d = readr::read_csv(full_csv)
      })
    })

    if(clean) {
      d = clean_jts(d)
    }
    res = janitor::remove_empty(d, which = c("rows", "cols"))
    res
  }

  # names(res) = gsub(pattern = "100", replacement = "Jobs100", names(res))
  # names(res) = gsub(pattern = "500", replacement = "Jobs500", names(res))

  if(output_format == "sf") {
    #do something
    type = "lsoa" # todo make this change for LA data
    geo_data = get_geo_data(type = type)
    if("LSOA11CD" %in% names(geo_data)) {
      geo_data = dplyr::rename(geo_data, LSOA_code = LSOA11CD)
    }
    res = dplyr::inner_join(geo_data, res)
  }
  res
}

# d1 = read_jts_local()
read_jts_local = function(
  jts_code = "jts0101",
  sheet = "2017",
  download_dir = jts_download_directory(),
  clean = TRUE
) {
  fn = paste0(jts_code, ".ods")
  ffn = file.path(download_dir, fn)
  if(!file.exists(ffn)) {
    if(interactive()) {
      dl_yes = utils::askYesNo(msg = "File not found. Download all files (100 MB+)?")
    }
    if(dl_yes) {
      download_jts_all()
    }
  }
  d = readODS::read_ods(ffn, sheet = sheet)
  if(clean) {
    d = clean_jts(d)
  }
  d
}

# f = download_jts()
# metadata = readODS::read_ods(f[1])
download_jts = function(
  u = "/attachment_data/file/873633/jts0101-to-jts0408.zip",
  bu = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads",
  download_dir = jts_download_directory(),
  rename_files = TRUE
) {
  fu = paste0(bu, u)
  bn = basename(fu)
  fp = file.path(download_dir, bn)
  message("Downloading ", fu, " to ", fp)
  utils::download.file(url = fu, destfile = fp)
  udf = utils::unzip(fp, list = TRUE)
  uf = udf$Name
  uff = file.path(download_dir, uf)
  message("Unzipping the files to ", download_dir)
  utils::unzip(fp, exdir = download_dir)
  if(rename_files) {
    uff_old = uff
    uff = gsub(pattern = "_revised", replacement = "", x = uff)
    file.rename(uff_old, uff)
  }
  uff
}

# download_jts_all()
download_jts_all = function(u = c(
  "/attachment_data/file/873633/jts0101-to-jts0408.zip",
  "/attachment_data/file/853579/jts0501-to-jts0503.zip",
  "/attachment_data/file/883549/jts0504-to-jts0505.zip",
  "/attachment_data/file/883551/jts0506-to-jts0508.zip"
)) {
  lapply(u, download_jts)
}

download_accessibility_files = function(download_dir = jts_download_directory()) {
  u1 = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853574/journey-time-statistics-2017.pdf"
  utils::download.file(u1, file.path(download_dir, "journey-time-statistics-2017.pdf"))
  u2 = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853576/jts0101-to-jts0408.zip"
  utils::download.file(u2, file.path(download_dir, "jts0101-to-jts0408.zip"))
  u3 = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853579/jts0501-to-jts0503.zip"
  utils::download.file(u3, file.path(download_dir, "jts0501-to-jts0503.zip"))
  u4 = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853581/jts0504-to-jts0508.zip"
  utils::download.file(u4, file.path(download_dir, "jts0504-to-jts0508.zip"))
}


get_accessibility_data_overview = function(download_dir = jts_download_directory(), table_name = "jts0101.ods") {
  u = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853576/jts0101-to-jts0408.zip"
  f = file.path(download_dir, "jts0101-to-jts0408.zip")
  utils::download.file(u, f)
  utils::unzip(zipfile = f, exdir = download_dir)
  list.files(download_dir)
  readODS::read_ods(file.path(download_dir, table_name))[[1]]
}
# utils::globalVariables("jts_tables")

# u = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853147/jts0401.ods"
# u = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853147/jts0501.ods"
# read_ods_url(u)

read_ods_url = function(u, download_dir = jts_download_directory(), sheet = "2017") {
  f = basename(u)
  ff = file.path(download_dir, f)
 utils::download.file(url = u, destfile = ff)
  s = readODS::list_ods_sheets(ff)
  message("The following sheets are available: ", paste0(s, collapse = " "))
  d = readODS::read_ods(ff, sheet = sheet)
  d
}
# remotes::install_github("chainsawriot/readODS")
# u = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853155/jts0501.ods"
# download_dir = tempdir()
# f = basename(u)
# ff = file.path(download_dir, f)
#utils::download.file(url = u, destfile = ff)
# d = readODS::read_ods(ff, sheet = "2017")
# message("The following sheets are available: ", paste0(s, collapse = " "))
# s = readODS::list_ods_sheets(ff)

jts_download_directory = function() {
  Sys.getenv("JTS_DIRECTORY", tempdir())
}

jts_url = function() {
  csv_file_path = file.path(jts_download_directory(), "jts_csv_files")
  if(dir.exists(csv_file_path)) {
    files_in_csv_path = list.files(csv_file_path)
    message(length(files_in_csv_path), " files found in path")
    return(csv_file_path)
  } else {
    csv_file_path = "https://github.com/itsleeds/jts/releases/download/1/"
  }
  csv_file_path
}


# test with la data
# library(acton)
# d = get_jts_data(table = "jts0401", year = 2017, skip = 0)
# d = get_jts_data(table = "jts0501", year = 2017, skip = 0)
# head(d)
# d_clean = clean_jts(d)
# head(d_clean)
clean_jts = function(d) {
  # browser()
  code_cols = paste0(d[[1]], d[[2]])
  col2_contains_Code = grepl("code", code_cols, ignore.case = TRUE)
  column_name_row = which(col2_contains_Code)
  if(length(column_name_row) != 1) {
    warning("Multiple column headings found: ", column_name_row, ": ")
    col2_contains_Code = grepl("year", code_cols, ignore.case = TRUE)
    column_name_row = which(col2_contains_Code)
  }
  names(d) = d[column_name_row, ]
  d_filtered = d[(column_name_row + 1):(nrow(d)), ]
  d_with_type = utils::type.convert(d_filtered)
  # head(d_with_type)
  # janitor::remove_empty(d_with_type)
  d_with_type
}

#' get_geo_data()
get_geo_data = function(type = "lsoa") {
  if(type == "lsoa") {
    u = "https://opendata.arcgis.com/datasets/c892586698ad4d268f9288f1df20ab77_0.zip"
  } else if(type == "la") {
    # url to la data
  }
  geo_data = duraz(u)
}
#' u = "https://opendata.arcgis.com/datasets/c892586698ad4d268f9288f1df20ab77_0.zip"
#' lsoas = duraz(u_oas_cents)
#' plot(lsoas$geometry)
duraz = function(u, delete_zip = FALSE, rename_zip = TRUE, data_dir = jts_download_directory()) {
  if(!grepl(pattern = "http", u)) {
    if(!grepl(pattern = ".zip", x = u)) {
      warning("This does not have a .zip suffix by trying to unzip anyway")
    }
    unzip(u)
  } else {
    zip_name = basename(u)
    if(file.exists(zip_name)) {
      warning(".zip already exists. Not downloading new data.\nYou may want to delete or rename it before proceeding")
    } else {
      zip_filepath = file.path(data_dir, zip_name)
      download.file(u, destfile = zip_filepath)
    }
    unzip_directory = file.path(data_dir, gsub(pattern = ".zip", "", zip_name))
    dir.create(unzip_directory)
    unzip(zip_filepath, exdir = unzip_directory)
  }
  f = list.files(path =  unzip_directory, pattern = ".shp", full.names = TRUE)
  if(length(f) > 1) {
    stop("Warning: more than one shapefile downloaded - load them manually")
  }
  res = sf::st_read(f)
  return(res)
}
