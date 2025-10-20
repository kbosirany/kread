test_that("get_read_fun returns correct function for delimited files", {
  expect_identical(get_read_fun("file.csv"), read_table)
  expect_identical(get_read_fun("file.tsv"), read_table)
  expect_identical(get_read_fun("file.dat"), read_table)
})

test_that("get_read_fun returns correct function for ini files", {
  expect_identical(get_read_fun("file.ini"), ini::read.ini)
})

test_that("get_read_fun returns correct function for Excel files", {
  expect_identical(get_read_fun("file.xls"), read_excel)
  expect_identical(get_read_fun("file.xlsx"), read_excel)
})

test_that("get_read_fun returns correct function for ODS files", {
  expect_identical(get_read_fun("file.ods"), readODS::read_ods)
})

test_that("get_read_fun returns correct function for NetCDF files", {
  expect_identical(get_read_fun("file.nc"), stars::read_mdim)
})

test_that("get_read_fun returns correct function for RDS files", {
  expect_identical(get_read_fun("file.RDS"), readRDS)
  expect_identical(get_read_fun("file.rds"), readRDS)
})

test_that("get_read_fun returns correct function for shapefiles", {
  expect_identical(get_read_fun("file.shp"), sf::read_sf)
})

test_that("get_read_fun returns correct function for YAML files", {
  expect_identical(get_read_fun("file.yml"), yaml::read_yaml)
  expect_identical(get_read_fun("file.yaml"), yaml::read_yaml)
})

test_that("get_read_fun throws error for unsupported file extensions", {
  expect_error(get_read_fun("file.txt"), "Unsupported file extension: txt")
  expect_error(get_read_fun("file.json"), "Unsupported file extension: json")
  expect_error(get_read_fun("file.pdf"), "Unsupported file extension: pdf")
  expect_error(get_read_fun("file.docx"), "Unsupported file extension: docx")
})

test_that("get_read_fun handles file paths with directories", {
  expect_identical(get_read_fun("/path/to/file.csv"), read_table)
  expect_identical(get_read_fun("./relative/path/file.xlsx"), read_excel)
  expect_identical(get_read_fun("../parent/dir/file.rds"), readRDS)
})

test_that("get_read_fun handles files with no extension", {
  expect_error(get_read_fun("file_without_extension"), "Unsupported file extension: ")
})

test_that("get_read_fun is case-sensitive for extensions", {
  # RDS and rds are both supported
  expect_identical(get_read_fun("file.RDS"), readRDS)
  expect_identical(get_read_fun("file.rds"), readRDS)
  
  # CSV is uppercase, should fail
  expect_error(get_read_fun("file.CSV"), "Unsupported file extension: CSV")
})
