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
library(future)
library(dials)
library(magrittr)

# functions ---------------------------------------------------------------

# read in functions from the 'functions' folder
functions <- list.files(here::here("functions"))
purrr::walk(functions, ~ source(here::here("functions", .x)))

file.create(here::here("output", "warnings.txt"), showWarnings = F)
file.create(here::here("output", "info.txt"), showWarnings = F)

# specified parameters ----------------------------------------------------

# any pre-specified or fixed values in the code should be set by the user
#  in this section

#set a maximum number of hyperparameter combos here if desired
user_hparam_grid_max <- 100

# set the vector of numbers of trees to test in training
user_treevec <- c(10, 100, 500)

# set the splitrule you want to use for tree construction
user_splitrule <- c("extratrees")

# uncertainty check:
# # a vector of numbers or single value over which to re-run trained model to assess uncertainty
user_checklen <- c(50, 100, 150)

# testing data method
# 1 = all training data together
# 2 = k-fold training data
# 3 = both
user_trainmethod = 3

# model name
user_modelname <- "base_simple"


# * about input data ------------------------------------------------------

in_ts <- TRUE #T/F input is a time series

# input file to use
in_filename <- "sim_input_data_base_simple2024-07-20.csv"

# read in input file
in_data <- read_csv(here::here("input", in_filename)) %>%
  janitor::clean_names()

# get column names
in_cols <- colnames(in_data)
print("Input column names read in as: ")
print(in_cols)
write(paste("Cleaned input column names read in as: ", in_cols), file = here::here("output", "info.txt"), append = TRUE)

# designate which input columns are features
in_features <- in_cols[1:7]

# designate which input column is the target
in_target <- in_cols[8]

# designate which column is time (if using)
# CIA: currently required, can add option later
in_time <- in_cols[9]

# future: spatial?

# data splits:
# # tell cross-validation how to structure k-folds
k_init_time_train = NA # set to NA to use default values; number of data rows in first k-fold training data
k_assess_train = NA # set to NA to use default values;
k_cumulative_train = NA # set to NA to use default values;
k_skip_train = NA # set to NA to use default values;
k_lag_train = NA # set to NA to use default values;

if(user_trainmethod > 1)
{
  k_init_time_test = NA # set to NA to use default values; number of data rows in first k-fold training data
  k_assess_test = NA # set to NA to use default values;
  k_cumulative_test = NA # set to NA to use default values;
  k_skip_test = NA # set to NA to use default values;
  k_lag_test = NA # set to NA to use default values;
}


# * random forest settings ------------------------------------------------

# n trees

# do you want to test every hyperparam combo? (computationally intensive)
# TRUE means test every combo
# FALSE mean test a max number of combos; default = 100.
setup_hgrid <- FALSE

# if you are setting a max number of combinations, you can generate your grid once and pull it in
# TRUE means generate new hyparparmeter grid
# FALSE will call in an existing grid
setup_newgrid <- TRUE

# if you want to read in a specific grid file, set to TRUE and put in the .csv filename (fle must be saved in inputs folder)
setup_gridfile <- FALSE
setup_gridfilename <- "grid.csv"

# do you want a standard data split based on optimal splitting work by Joseph (2022) or custom split?
# TRUE = standard split
# FALSE = custom split
setup_datasplit <- TRUE

# if you have a custom split, is it based on data results or user setting?
# TRUE = based on automated data results
# FALSE = user specified ratio
setup_customsplit <- FALSE #TRUE scenario not yet operational

# a custom ratio must be > 0 and < 1; 0.60 to 0.75 is standard in the literature
# the custom ratio represents the proportion of training data
setup_customratio <- 0.67


# * processing settings ---------------------------------------------------
# Test mode
# TRUE means code will run using a single computing core
# FALSE means the code will run in parallel using (total cores available - 2)
test_mode <- TRUE
# this is for the parallelization of the design set, tree check, and final check
if(test_mode == TRUE) {n_workers <- 1
} else{n_workers <- (availableCores()-2)}
future::plan(future::multicore, workers = n_workers)


# END OF USER SETTINGS SECTION --------------------------------------------
# -> THE FOLLOWING CODE IS AUTOMATED.

# input data --------------------------------------------------------------

# if your data is a time series, make sure it is arranged chronologically
#   this is important later when you do your training/testing splits
if(in_ts == TRUE){in_data <- in_data %>% arrange(in_time) }

# need consistent naming of target and time vars
in_data %<>%  dplyr::rename(target = in_target)
in_data %<>%  dplyr::rename(time = in_time)

# * input data exploration ------------------------------------------------
# CIA: Detmer and Eric Ward paper: GAMs as ecosystem threshold detection tool
# - jacknife resampling
# - if >[some fraction] jackknife iters detected a threshold,
#    calc mean and CI of location (here- time point)

# train/test split (regime breakpoints?) in target

# non-stationarity in target

# ARIMA in target

# extremes detection in target

# collinearity in features

# output detected elements; default = use best settings; user can modify

# based on results, train/test split recommended
split_recommended <- NA #CIA fill in here

# CIA: notes- ideally, train/test is approx 60-75% data, AND not on a regime shift

# * input data setup & splits ------------------------------------------------------

# split training and testing section

# ** training vs testing -------------------------------------------------

# CIA: currently all based around initial_time_split; future: add feature for random splits

# updated science on splitting, Joseph 2022
# optimal ratio is: sqrt(num_params)/(sqrt(num_params)+1)
# this implementation uses 3 hyperparameters- n_trees, min_n, mtry

if(setup_datasplit == TRUE) # standard optimal split based on Joseph (2022)
{
  split_ratio <- sqrt(3)/(sqrt(3)+1) #based on Joseph (2022) and 3 hparams
  # CIA: modify with: if data is a time series, non-stationary, etc: use time_split.
  dat_split <- rsample::initial_time_split(in_data, prop = split_ratio)
}else if(setup_datasplit == FALSE)
{
  if(setup_customsplit == TRUE) # use data exploration results to set split
  {
    # if there is a regime, etc: split based on data recommendation
    # CIA; this feature is not yet operational; need to setup data exploration above
    split_ratio <- split_recommended
    dat_split <- rsample::initial_time_split(in_data, prop = split_ratio)
  }
  if(setup_customsplit == FALSE)
  {
    split_ratio <- setup_customratio
    dat_split <- rsample::initial_time_split(in_data, prop = split_ratio)
  }
}

write_csv(training(dat_split), path = here::here("output", "training.csv"))
write_csv(testing(dat_split), path = here::here("output", "testing.csv"))
write(paste("Train/test splitting ratio: ", split_ratio), file = here::here("output", "info.txt"), append = TRUE)


# ** training cross-validation -------------------------------------------

kfold_train <- get_kfold(k_data_in = training(dat_split),
                         k_init_time = k_init_time_train,
                         k_assess = k_assess_train,
                         k_cumulative = k_cumulative_train,
                         k_skip = k_skip_train,
                         k_lag= k_lag_train)

train_slices <- dim(kfold_train)[1]

train_baked <- list()

# CIA: convert this to an apply statement
for(i in 1:(train_slices))
{
  train_baked[[i]] <- get_baked(kfold = kfold_train,
                                splitN=i)
}

# ** optional testing cross-validation ---------------------------------------

if(user_trainmethod > 1)
{
  kfold_test <- get_kfold(k_data_in = testing(dat_split),
                           k_init_time = k_init_time_test,
                           k_assess = k_assess_test,
                           k_cumulative = k_cumulative_test,
                           k_skip = k_skip_test,
                           k_lag= k_lag_test)
  test_slices <- dim(kfold_test)[1]

  test_baked <- list()

  for(i in 1:(test_slices))
  {
    test_baked[[i]] <- get_baked(kfold = kfold_test,
                                 splitN=i)
  }
}



# random forest -----------------------------------------------------------

# * setup -----------------------------------------------------------------

# * hyperparameters -------------------------------------------------------

# auto-detect min_n, mtry

# min_n_maximum <- #detect min n in smallest fold

# CIA: modify following:
# tune_grid <- parameters(min_n(),mtry())  %>%
#   dials::finalize(mtry(), x = baked_data[[1]]$baked_squid %>% dplyr::select(-year, -seas, -cdfg_4))
# tune_grid$object[[1]]$range$upper <-min_n_maximum
tune_grid <- dials::parameters(min_n(),mtry())  %>% #CIA: problem with parameters() function
    dials::finalize(mtry(), x = in_data  %>% dplyr::select(-in_target, -in_time)) #CIA: if time incl; ALSO, using whole data set or just training data- check
tune_grid$object[[1]]$range$upper <-min_n_maximum

# alt method, get max entropy with range of tree numbers incl:
# tune_grid <- parameters(min_n(),mtry(), trees(range(test_options$tree)))  %>%
#   dials::finalize(mtry(), x = baked_data[[1]]$baked_squid %>% dplyr::select(-year, -seas, -cdfg_4))
# tune_grid$object[[1]]$range$upper <-min_n_maximum

# * create grid -----------------------------------------------------------

# grid size
if(setup_hgrid == F) {setup_gridmax = user_hparam_grid_max}
if(!is.logical(setup_hgrid)) {
  err_hparam_grid <- "ERROR: designate TRUE or FALSE for 'setup_hgrid' for a full or reduced hyperparamter grid size (respectively)"
  print(err_hparam_grid)
  write(err_hparam_grid, file = here::here("output", "warnings.txt"), append = TRUE)
}

# new or import grid
if(setup_newgrid == TRUE) #create a new grid
{
  if(setup_hgrid == TRUE) # full design set, all hyperparam combos
  {
    design_set <- expand_grid(min_n = seq(from = tune_grid$object[[1]]$range$lower, to = min_n_maximum, by = 1),
                              mtry = seq(from = tune_grid$object[[2]]$range$lower, to = tune_grid$object[[2]]$range$upper, by = 1)) %>%
      rowid_to_column(var = 'combo') %>%
      expand_grid(trees = user_treevec) %>%
      expand_grid(splitrule = user_splitrule)
    write_csv(design_set, file = here("input", paste0("grid.csv")))
    write_csv(design_set, file = here("input", paste0("grid", Sys.Date(),".csv")))
  }
  if(setup_hgrid == FALSE) # max size design set
  {
    design_set <- grid_max_entropy(tune_grid, size = user_hparam_grid_max) %>%
      expand_grid(trees = user_treevec) %>%
      expand_grid(splitrule = user_splitrule)
    write_csv(design_set, path = here("input", paste0("grid.csv")))
    write_csv(design_set, path = here("input", paste0("grid", Sys.Date(),".csv")))
  }

}else if(setup_newgrid == FALSE) #import a grid
{
  if(setup_gridfile == TRUE) #specify named file
  {
    design_set <- read_csv(file = here::here("input", setup_gridfilename))
  } else #pull in auto-gen file (default)
  {
    design_set <- read_csv(file = here::here("input", "grid.csv"))
  }

}

if(!is.logical(setup_newgrid)){
  err_hparam_grid <- "ERROR: designate TRUE or FALSE for 'setup_newgrid' for creating new or reading in a hyperparameter (respectively)"
  print(err_hparam_grid)
  write(err_hparam_grid, file = here::here("output", "warnings.txt"), append = TRUE)
}

# * training --------------------------------------------------------------


# * testing ---------------------------------------------------------------


# * output ----------------------------------------------------------------


