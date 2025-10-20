#' @export
#'
read <- function(path, ..., FUN = get_read_fun(path)) {
  FUN(path, ...)
}

read_table <- function(path, ..., show_col_types = FALSE) {
  readr::read_delim(path, ..., show_col_types = show_col_types)
}

read_excel <- function(
  path, sheets = readxl::excel_sheets(path), ..., simplify = TRUE
) {
  args <- list(...)
  x <- lapply(
    sheets,
    function(s) {
      if (s %in% names(args)) a <- args[[s]] else a <- args
      do.call(readxl::read_excel, c(list(path, sheet = s), a))
    }
  )
  if (length(x) == 1 && simplify) x <- x[[1]]
  return(x)
}
