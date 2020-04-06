test_that("File Import", {
  expect_equal(object = import_chron("ex_urnfield_periods.csv", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = ","),
               expected = read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods2.csv", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = ";"),
               expected = read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods.xlsx", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add"),
               expected = read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods.xls", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add"),
               expected = read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods_@.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "@"),
               expected = read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods_q.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "q"),
               expected = read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods_tab.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               expected = read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods_@.xyz", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "@"),
               expected = read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods_tab_unsec.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               expected = read_excel("ex_urnfield_periods_unsec_reference.xlsx", col_types = c("text", "text", "text", "text", "numeric", "logical")))

#  expect_error(object = import_chron("ex_urnfield_periods_tab_err1.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
#               regexp = "One*")
  expect_error(object = import_chron("ex_urnfield_periods_tab_err2.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               regexp = "One*")

  expect_error(object = import_chron("ex_urnfield_periods_tab_err3_level.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               regexp = "Wrong*")
  })

test_that("File Conversion", {
