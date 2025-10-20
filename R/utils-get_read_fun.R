#' @export
#'
get_read_fun <- function(path) {
  ext <- tools::file_ext(path)
  if (ext %in% c("csv", "tsv", "dat")) return(readr::read_delim)
  if (ext == "ini") return(ini::read.ini)
  if (ext %in% c("xls", "xlsx")) return(readxl::read_excel)
  if (ext == "ods") return(readODS::read_ods)
  if (ext == "nc") return(stars::read_mdim)
  if (ext %in% c("RDS", "rds")) return(readRDS)
  if (ext == "shp") return(sf::read_sf)
  if (ext %in% c("yml", "yaml")) return(yaml::read_yaml)
  stop("Unsupported file extension: ", ext)
}
