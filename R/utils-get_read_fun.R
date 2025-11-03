#' Get Read Function Based on File Extension
#'
#' Returns the appropriate reading function for a file based on its extension.
#' This function maps common file extensions to their corresponding R reading
#' functions from various packages.
#'
#' @param path [character] File path including the extension.
#'
#' @return A function object that can be used to read the file type.
#'
#' @details
#' The function supports the following file types:
#' \itemize{
#'   \item \strong{Delimited files}: .csv, .tsv, .dat (uses \code{readr::read_delim})
#'   \item \strong{INI files}: .ini (uses \code{ini::read.ini})
#'   \item \strong{Excel files}: .xls, .xlsx (uses \code{readxl::read_excel})
#'   \item \strong{OpenDocument Spreadsheet}: .ods (uses \code{readODS::read_ods})
#'   \item \strong{NetCDF files}: .nc (uses \code{stars::read_mdim})
#'   \item \strong{R objects}: .RDS, .rds (uses \code{readRDS})
#'   \item \strong{Shapefiles}: .shp (uses \code{sf::read_sf})
#'   \item \strong{YAML files}: .yml, .yaml (uses \code{yaml::read_yaml})
#' }
#'
#' @examples
#' \dontrun{
#' # Get the appropriate read function for a CSV file
#' read_fn <- get_read_fun("data/myfile.csv")
#' data <- read_fn("data/myfile.csv")
#'
#' # Get the appropriate read function for an Excel file
#' read_fn <- get_read_fun("data/myfile.xlsx")
#' data <- read_fn("data/myfile.xlsx")
#' }
#'
#' @export
#'
get_read_fun <- function(path) {
  ext <- tools::file_ext(path)
  if (ext %in% c("csv", "tsv", "dat")) return(read_table)
  if (ext == "ini") return(ini::read.ini)
  if (ext %in% c("xls", "xlsx")) return(read_excel)
  if (ext == "ods") return(readODS::read_ods)
  if (ext == "nc") return(stars::read_mdim)
  if (ext %in% c("RDS", "rds")) return(readRDS)
  if (ext == "shp") return(sf::read_sf)
  if (ext %in% c("yml", "yaml")) return(yaml::read_yaml)
  stop("Unsupported file extension: ", ext)
}
