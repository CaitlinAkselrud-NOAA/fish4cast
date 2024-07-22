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

file.create(here::here("output", "warnings.txt"), showWarnings = F)

# specified parameters ----------------------------------------------------

# any pre-specified or fixed values in the code should be set by the user
#  in this section


# # about input data ------------------------------------------------------

in_ts <- T #T/F input is a time series


# # random forest settings ------------------------------------------------

# n trees

# check

# do you want to test every hyperparam combo? (computationally intensive)
# TRUE means test every combo
# FALSE mean test a max number of combos; default = 100.
setup_hgrid <- F



# input data --------------------------------------------------------------


# random forest -----------------------------------------------------------

# # setup -----------------------------------------------------------------

# auto-detect min_n, mtry

# create grid
if(setup_hgrid == T) {}
if(setup_hgrid == F) {setup_gridmax = gridmax()}
if(!is.logical(setup_hgrid)) {
  print("ERROR: designate TRUE or FALSE for 'setup-hgrid' for a full or reduced hyperparamter grid size (respectively)")
  # append to warnings.txt file
  }

# # training --------------------------------------------------------------


# # testing ---------------------------------------------------------------


# # output ----------------------------------------------------------------


