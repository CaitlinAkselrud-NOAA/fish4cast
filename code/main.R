#' Random forest regression forecasting with eco time series data
#' Code by: Caitlin Allen Akselrud
#' Contact: caitlin.allen_akselrud at noaa.gov
#' Initiation date: 2024-07-22
#'


# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(randomForest)


# functions ---------------------------------------------------------------

# read in functions from the 'functions' folder
functions <- list.files(here::here("functions"))
purrr::walk(functions, ~ source(here::here("functions", .x)))

# specified parameters ----------------------------------------------------

# any pre-specified or fixed values in the code should be set by the user
#  in this section



# input data --------------------------------------------------------------


# random forest -----------------------------------------------------------


# # setup -----------------------------------------------------------------


# # training --------------------------------------------------------------


# # testing ---------------------------------------------------------------


# # output ----------------------------------------------------------------


