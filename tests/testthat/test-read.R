# Tests for read.R functions

# Helper function to create temporary test files
create_test_csv <- function() {
  tmp <- tempfile(fileext = ".csv")
  # Use readr::write_csv to create proper CSV files
  readr::write_csv(data.frame(a = 1:3, b = letters[1:3]), tmp)
  return(tmp)
}

create_test_tsv <- function() {
  tmp <- tempfile(fileext = ".tsv")
  write.table(data.frame(x = 1:3, y = 4:6), tmp, sep = "\t", row.names = FALSE)
  return(tmp)
}

# Tests for read() function
test_that("read() works with CSV files", {
  tmp_csv <- create_test_csv()
  on.exit(unlink(tmp_csv))

  result <- read(tmp_csv, delim = ",")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("read() accepts additional arguments", {
  tmp_csv <- create_test_csv()
  on.exit(unlink(tmp_csv))

  # Test with delim argument
  result <- read(tmp_csv, delim = ",")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("read() works with custom FUN parameter", {
  tmp_csv <- create_test_csv()
  on.exit(unlink(tmp_csv))

  # Use readr::read_csv explicitly
  result <- read(tmp_csv, FUN = readr::read_csv)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("read() automatically detects file type", {
  tmp_csv <- create_test_csv()
  on.exit(unlink(tmp_csv))

  # Should automatically use readr::read_delim for CSV
  result <- read(tmp_csv, delim = ",")
  expect_s3_class(result, "data.frame")
})

test_that("read() works with RDS files", {
  tmp_rds <- tempfile(fileext = ".rds")
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  saveRDS(test_data, tmp_rds)
  on.exit(unlink(tmp_rds))

  result <- read(tmp_rds)
  expect_equal(result, test_data)
})


# Tests for read_table() function
test_that("read_table() reads CSV files correctly", {
  tmp_csv <- create_test_csv()
  on.exit(unlink(tmp_csv))

  result <- read_table(tmp_csv, delim = ",")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("read_table() reads TSV files correctly", {
  tmp_tsv <- create_test_tsv()
  on.exit(unlink(tmp_tsv))

  result <- read_table(tmp_tsv, delim = "\t")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("read_table() suppresses column type messages by default", {
  tmp_csv <- create_test_csv()
  on.exit(unlink(tmp_csv))

  # Should not produce messages
  expect_silent(read_table(tmp_csv, delim = ","))
})

test_that("read_table() shows column types when requested", {
  tmp_csv <- create_test_csv()
  on.exit(unlink(tmp_csv))

  # Should show column specification
  expect_message(
    read_table(tmp_csv, delim = ",", show_col_types = TRUE), "Rows|cols"
  )
})

test_that("read_table() accepts additional readr arguments", {
  tmp_csv <- create_test_csv()
  on.exit(unlink(tmp_csv))

  # Read with skip argument
  result <- read_table(tmp_csv, delim = ",", skip = 1)
  expect_s3_class(result, "tbl_df")
  # Should have fewer rows due to skip
  expect_equal(nrow(result), 2)
})

test_that("read_table() handles different delimiters", {
  # Create semicolon-delimited file
  tmp_semi <- tempfile(fileext = ".csv")
  write.table(data.frame(a = 1:3, b = 4:6), tmp_semi, 
              sep = ";", row.names = FALSE)
  on.exit(unlink(tmp_semi))

  result <- read_table(tmp_semi, delim = ";")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})


# Tests for read_excel() function
test_that("read_excel() reads single sheet Excel files", {
  skip_if_not_installed("readxl")

  # Use readxl's example file
  path <- readxl::readxl_example("datasets.xlsx")

  result <- read_excel(path, sheets = "mtcars")
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) > 0)
  expect_true(nrow(result) > 0)
})

test_that("read_excel() reads multiple sheets", {
  skip_if_not_installed("readxl")

  path <- readxl::readxl_example("datasets.xlsx")

  result <- read_excel(path, sheets = c("mtcars", "chickwts"))
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_named(result, c("mtcars", "chickwts"))
})

test_that("read_excel() simplifies single sheet to data frame", {
  skip_if_not_installed("readxl")

  path <- readxl::readxl_example("datasets.xlsx")

  # Single sheet with simplify = TRUE (default)
  result <- read_excel(path, sheets = "mtcars", simplify = TRUE)
  expect_s3_class(result, "data.frame")
  expect_false(is.list(result) && !is.data.frame(result))
})

test_that("read_excel() does not simplify when simplify = FALSE", {
  skip_if_not_installed("readxl")

  path <- readxl::readxl_example("datasets.xlsx")

  result <- read_excel(path, sheets = "mtcars", simplify = FALSE)
  expect_type(result, "list")
  expect_equal(length(result), 1)
})

test_that("read_excel() reads all sheets by default", {
  skip_if_not_installed("readxl")

  path <- readxl::readxl_example("datasets.xlsx")
  all_sheets <- readxl::excel_sheets(path)

  result <- read_excel(path)
  expect_type(result, "list")
  expect_equal(length(result), length(all_sheets))
})

test_that("read_excel() passes additional arguments to read_excel", {
  skip_if_not_installed("readxl")

  path <- readxl::readxl_example("datasets.xlsx")

  # Read with skip argument
  result <- read_excel(path, sheets = "mtcars", skip = 5)
  expect_s3_class(result, "data.frame")
  # Should have fewer rows due to skip
})

test_that("read_excel() handles sheet-specific arguments", {
  skip_if_not_installed("readxl")

  path <- readxl::readxl_example("datasets.xlsx")

  # Apply different arguments to different sheets
  result <- read_excel(
    path,
    sheets = c("mtcars", "chickwts"),
    mtcars = list(skip = 1),
    simplify = FALSE
  )

  expect_type(result, "list")
  expect_equal(length(result), 2)
})

test_that("read_excel() returns named list with proper structure", {
  skip_if_not_installed("readxl")

  path <- readxl::readxl_example("datasets.xlsx")

  result <- read_excel(path, sheets = c("mtcars", "chickwts"), simplify = FALSE)
  expect_type(result, "list")
  expect_equal(length(result), 2)
  # Each element should be a data frame
  expect_true(all(sapply(result, is.data.frame)))
})

test_that("read_excel() handles non-existent sheets gracefully", {
  skip_if_not_installed("readxl")

  path <- readxl::readxl_example("datasets.xlsx")

  # Attempting to read a non-existent sheet should error
  expect_error(read_excel(path, sheets = "nonexistent_sheet"))
})
