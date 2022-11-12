suppressMessages(
  test_that("File Import", {

  expect_equal(object = import_chron("ex_urnfield_periods.csv", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = ","),
               expected = readr::read_csv("ex_urnfield_periods_reference.csv", col_types = "ccnnnl"))
  expect_equal(object = import_chron("ex_urnfield_periods2.csv", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = ";"),
               expected = readr::read_csv("ex_urnfield_periods_reference.csv", col_types = "ccnnnl"))
  expect_equal(object = import_chron("ex_urnfield_periods.xlsx", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add"),
               expected = readxl::read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods.xls", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add"),
               expected = readxl::read_excel("ex_urnfield_periods_reference.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "logical")))
  expect_equal(object = import_chron("ex_urnfield_periods_@.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "@"),
               expected = readr::read_csv("ex_urnfield_periods_reference.csv", col_types = "ccnnnl"))
  expect_equal(object = import_chron("ex_urnfield_periods_q.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "q"),
               expected = readr::read_csv("ex_urnfield_periods_reference.csv", col_types = "ccnnnl"))
  expect_equal(object = import_chron("ex_urnfield_periods_tab.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               expected = readr::read_csv("ex_urnfield_periods_reference.csv", col_types = "ccnnnl"))
  expect_equal(object = import_chron("ex_urnfield_periods_@.xyz", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "@"),
               expected = readr::read_csv("ex_urnfield_periods_reference.csv", col_types = "ccnnnl"))
  expect_equal(object = import_chron("ex_urnfield_periods_tab_unsec.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               expected = readr::read_csv("ex_urnfield_periods_unsec_reference.csv", col_types = "ccccnl"))

  expect_error(object = import_chron("ex_urnfield_periods2.csv", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add"),
               regexp = "Missing argument:*")
  expect_error(object = import_chron("ex_urnfield_periods2.csv", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "|"),
               regexp = "No valid separator for*")
  expect_error(object = import_chron("ex_urnfield_periods_tab_err1.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               regexp = "Wrong input format: *")
  expect_error(object = import_chron("ex_urnfield_periods_tab_err2.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               regexp = "One*")
  expect_error(object = import_chron("ex_urnfield_periods_tab_err3_level.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               regexp = "Wrong*")
  expect_error(object = import_chron("NOT-EXISTENT/NOT-EXISTENT.txt", region = "Region", name = "Name", start = "Start", end = "End", level = "Level", add = "add", delim = "\t"),
               regexp = "The file path *")
  })
)

test_that("File Conversion", {
  expect_equal(object = convert_to_chron(data = test,
                                         region = Area,
                                         name = Title,
                                         start = Begin,
                                         end = End,
                                         level = Subunit,
                                         add = Switch),
               expected = test_reference)
  expect_error(object = convert_to_chron(test_err_Input,
                                         region = Area,
                                         name = Title,
                                         start = Begin,
                                         end = End,
                                         level = Subunit,
                                         add = Switch),
               regexp = "Wrong*")
  expect_error(object = convert_to_chron(test_err2_Format,
                                         region = "Area",
                                         name = "Title",
                                         start = "Begin",
                                         end = "End",
                                         level = "Subunit",
                                         add = "Switch"),
               regexp = "One*")
  expect_error(object = convert_to_chron(test_err3_Level,
                                         region = "Area",
                                         name = "Title",
                                         start = "Begin",
                                         end = "End",
                                         level = "Subunit",
                                         add = "Switch"),
               regexp = "Wrong input format: level*")
  expect_error(object = convert_to_chron(not_existent,
                                         region = "Area",
                                         name = "Title",
                                         start = "Begin",
                                         end = "End",
                                         level = "Subunit",
                                         add = "Switch"),
               regexp = "object *")

  })
