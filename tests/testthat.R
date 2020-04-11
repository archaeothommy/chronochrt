Sys.setenv("R_TESTS" = "")

library(testthat)
library(readxl)
library(tibble)
library(ChronochRt)


test_check("ChronochRt")
