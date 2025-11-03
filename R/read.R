#' Read a file with automatic format detection
#'
#' This function reads a file by automatically detecting the appropriate reading
#' function based on the file extension. It delegates the actual reading to the
#' appropriate function (e.g., `readr::read_delim` for CSV files,
#' `readxl::read_excel` for Excel files, etc.).
#'
#' @param path Character string specifying the path to the file to read.
#' @param ... Additional arguments passed to the reading function determined by
#'   `FUN`.
#' @param FUN Function to use for reading the file. By default, this is 
#'   automatically determined by [get_read_fun()] based on the file extension.
#'
#' @return The return value depends on the file type and the function used to
#'   read it. Typically returns a data frame, tibble, or other appropriate R
#'   object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a CSV file
#' data_csv <- read("data.csv")
#'
#' # Read an Excel file
#' data_excel <- read("data.xlsx")
#'
#' # Read with additional arguments
#' data_csv2 <- read("data.csv", delim = ";", col_names = TRUE)
#'
#' # Specify a custom reading function
#' data_custom <- read("data.csv", FUN = readr::read_csv)
#' }
#'
read <- function(path, ..., FUN = get_read_fun(path)) {
  FUN(path, ...)
}

#' Read a delimited table file
#'
#' This function reads a delimited file (CSV, TSV, or other delimited formats)
#' using `readr::read_delim()`. By default, column type messages are suppressed.
#'
#' @param path Character string specifying the path to the delimited file to
#'   read.
#' @param ... Additional arguments passed to [readr::read_delim()].
#' @param show_col_types Logical indicating whether to show column type
#'   messages. Default is `FALSE` to suppress these messages.
#'
#' @return A tibble containing the data from the delimited file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a CSV file
#' data <- read_table("data.csv")
#'
#' # Read a TSV file with custom delimiter
#' data_tsv <- read_table("data.tsv", delim = "\t")
#'
#' # Read with column types shown
#' data_verbose <- read_table("data.csv", show_col_types = TRUE)
#'
#' # Read with specific column names
#' data_named <- read_table("data.csv", col_names = c("ID", "Name", "Value"))
#' }
#'
read_table <- function(path, ..., show_col_types = FALSE) {
  readr::read_delim(path, ..., show_col_types = show_col_types)
}

#' Read Excel files with support for multiple sheets
#'
#' This function reads one or more sheets from an Excel file (.xls or .xlsx).
#' It can return either a single data frame (if one sheet is read) or a named
#' list of data frames (if multiple sheets are read). Sheet-specific arguments
#' can be provided by naming them after the sheet names.
#'
#' @param path Character string specifying the path to the Excel file.
#' @param sheets Character vector of sheet names to read. By default, reads all
#'   sheets in the file using [readxl::excel_sheets()].
#' @param ... Additional arguments passed to [readxl::read_excel()]. Arguments
#'   can be named after specific sheets to apply different options per sheet.
#'   For example, `sheet1 = list(skip = 2)` will apply `skip = 2` only to
#'   the sheet named "sheet1".
#' @param simplify Logical indicating whether to simplify the result. If `TRUE`
#'   (default) and only one sheet is read, returns the data frame directly
#'   instead of a list with one element.
#'
#' @return If `simplify = TRUE` and one sheet is read: a tibble/data frame.
#'   Otherwise: a named list of tibbles/data frames, one per sheet.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read all sheets from an Excel file
#' data_all <- read_excel("workbook.xlsx")
#'
#' # Read specific sheets
#' data_subset <- read_excel("workbook.xlsx", sheets = c("Sheet1", "Sheet3"))
#'
#' # Read a single sheet (returns a data frame, not a list)
#' data_one <- read_excel("workbook.xlsx", sheets = "Sheet1")
#'
#' # Read with custom arguments
#' data_custom <- read_excel("workbook.xlsx", skip = 1, col_names = TRUE)
#'
#' # Apply different arguments to different sheets
#' data_mixed <- read_excel(
#'   "workbook.xlsx",
#'   sheets = c("Sheet1", "Sheet2"),
#'   Sheet1 = list(skip = 2),
#'   Sheet2 = list(skip = 0, col_types = "text")
#' )
#'
#' # Read multiple sheets without simplification
#' data_list <- read_excel("workbook.xlsx", simplify = FALSE)
#' }
read_excel <- function(
  path, sheets = readxl::excel_sheets(path), ..., simplify = TRUE
) {
  args <- list(...)
  # Identify sheet-specific arguments (those matching sheet names)
  sheet_specific_args <- intersect(names(args), sheets)
  # Get common arguments (those not matching any sheet name)
  common_args <- args[!names(args) %in% sheets]

  x <- lapply(
    setNames(nm = sheets),
    function(s) {
      # If there's a sheet-specific argument, use it; otherwise use common args
      if (s %in% sheet_specific_args) {
        a <- args[[s]]
      } else {
        a <- common_args
      }
      do.call(readxl::read_excel, c(list(path, sheet = s), a))
    }
  )
  if (length(x) == 1 && simplify) x <- x[[1]]
  return(x)
}
