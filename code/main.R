#' Random forest regression forecasting with eco time series data
#' Code by: Caitlin Allen Akselrud
#' Contact: caitlin.allen_akselrud at noaa.gov
#' Initiation date: 2024-07-22
#'

# Notes:
#  in_ are user specified variables for the input data
#  setup_ are user specified TRUE/FALSE settings
#  user_ are user specified numeric values or vectors

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(randomForest)
library(future)
library(dials)
library(magrittr)
library(ranger)
library(hmmTMB) #for HMM models
library(patchwork) #for plotting

# functions ---------------------------------------------------------------

# read in functions from the 'functions' folder
functions <- list.files(here::here("functions"))
purrr::walk(functions, ~ source(here::here("functions", .x)))


# model name
user_modelname <- "base_simple"

dir.create(here::here("input"), showWarnings = F)
dir.create(here::here("output"), showWarnings = F)
dir.create(here::here("output", user_modelname), showWarnings = F)
dir.create(here::here("output", user_modelname, "diagnostic_plots"), showWarnings = F)


file.create(here::here("output", user_modelname, "warnings.txt"), showWarnings = F)
file.create(here::here("output", user_modelname, "info.txt"), showWarnings = F)

# specified parameters ----------------------------------------------------

# any pre-specified or fixed values in the code should be set by the user
#  in this section

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
write(paste("Cleaned input column names read in as: ", in_cols),
      file = here::here("output", user_modelname,"info.txt"), append = TRUE)

# designate which input columns are features
in_features <- in_cols[1:7]

# designate which input column is the target
in_target <- in_cols[8]

# designate which column is time (if using)
# CIA note: currently required, can add option later
in_time <- in_cols[9]

# future: spatial?

# data splits:

# * random forest settings ------------------------------------------------

# * * hyperparameters -----------------------------------------------------
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

#set a maximum number of hyperparameter combos here if desired
user_hparam_grid_max <- 100

# set the vector of numbers of trees to test in training
user_treevec <- c(10, 100, 500)

# set the splitrule you want to use for tree construction
user_splitrule <- c("extratrees")

# uncertainty check:
# # a vector of numbers or single value over which to re-run trained model to assess uncertainty
user_checklen <- c(50, 100, 150)

user_treecheck <- c(10, 50, seq(from = 100, to = 5000, by = 100))
# * * cross-validation structure ------------------------------------------

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
user_customratio <- 0.67

# # tell cross-validation how to structure k-folds
k_init_time_train = NA # set to NA to use default values; number of data rows in first k-fold training data
k_assess_train = NA # set to NA to use default values;
k_cumulative_train = NA # set to NA to use default values;
k_skip_train = NA # set to NA to use default values;
k_lag_train = NA # set to NA to use default values;

# testing data method
# 1 = all training data together
# 2 = k-fold training data
# 3 = both
user_testmethod = 3

if(user_testmethod > 1)
{
  k_init_time_test = NA # set to NA to use default values; number of data rows in first k-fold training data
  k_assess_test = NA # set to NA to use default values;
  k_cumulative_test = NA # set to NA to use default values;
  k_skip_test = NA # set to NA to use default values;
  k_lag_test = NA # set to NA to use default values;
}

# what metric do you want to use to select the best hparam set?
# current options:
# 1 = rmse; root mean squared error, same units as orig data
# 2 = mae; mean absolute error, same units as orig data
# 3 = rsq; coeeficent of determination using correlation (not traditional SSQ method)
# 4 = rpd; ratio of performance to deviation  measures of consistency/correlation between observed and predicted values (and not of accuracy)
user_hp_select <- 1
# CIA: consider- MSE, MASE, MAPE, SMAPE

# * * uncertainty setup  --------------------------------------------------


# how many times do you want to re-run the trained model to ascertain uncertainty?
user_uncertainty <- 100

# which metric are you using for uncertainty cutoff?
# current options:
# 1 = rmse; root mean squared error, same units as orig data
# 2 = mae; mean absolute error, same units as orig data
# 3 = rsq; coeeficent of determination using correlation (not traditional SSQ method)
# 4 = rpd; ratio of performance to deviation  measures of consistency/correlation between observed and predicted values (and not of accuracy)
user_uncertainty_metric <- 1

# what is your cutoff for uncertainty?
user_uncertain_cutoff <- 2


# * * model selection -----------------------------------------------------

# which metric are you using for selecting the best model?
# current options:
# 1 = rmse; root mean squared error, same units as orig data
# 2 = mae; mean absolute error, same units as orig data
# 3 = rsq; coeeficent of determination using correlation (not traditional SSQ method)
# 4 = rpd; ratio of performance to deviation  measures of consistency/correlation between observed and predicted values (and not of accuracy)
user_selex_metric <- 1

# * processing settings ---------------------------------------------------
# Test mode
# TRUE means code will run using a single computing core
# FALSE means the code will run in parallel using (total cores available - 2)
test_mode <- TRUE
# this is for the parallelization of the design set, tree check, and final check
if(test_mode == TRUE) {n_workers <- 1
} else{n_workers <- (availableCores()-2)}
future::plan(future::multicore, workers = n_workers)

# CIA: add output for all user-specified settings for tracking projects

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
# regime shift: assume discrete process (distinct shift)
# based on Zoe Rand: HMMs
# CIA; YOU ARE HERE
get_regimes(dat, dat_dist, n_states, n_iters = 200)
# CIA: dat = just factors;
# dist = dist type (eg normal) for each factor as a list;
# n-states-- here is wher you can test multiple state settings, then later compare with AIC
# niters = number repeats within one hmm fit test

# CIA: Detmer and Eric Ward paper: GAMs as ecosystem threshold detection tool
# - jacknife resampling
# - if >[some fraction] jackknife iters detected a threshold,
#    calc mean and CI of location (here- time point)

# train/test split (regime breakpoints?) in target
# try mgcv

# non-stationarity in target (assume continuous process for change through time)
# check: https://feasts.tidyverts.org/

# ARIMA in target
# check: https://www.rdocumentation.org/packages/forecast/versions/8.23.0

# extremes detection in target
# check: https://github.com/fate-ewi/bayesdfa/blob/main/R/find_swans.R

# collinearity in features
# effective number of predictors, adjusted for collinearity (E Ward suggestion)
# might be straightforward for continuous values cases, but challenge for sparse data

# output detected elements; default = use best settings; user can modify

# based on results, train/test split recommended
split_recommended <- NA #CIA fill in here

# CIA: notes- ideally, train/test is approx 60-75% data, AND not on a regime shift

# * input data setup & splits ------------------------------------------------------

# split training and testing section

# ** training vs testing -------------------------------------------------

# future: currently all based around initial_time_split; future: add feature for random splits

# updated science on splitting, Joseph 2022
# optimal ratio is: sqrt(num_params)/(sqrt(num_params)+1)
# this implementation uses 3 hyperparameters- n_trees, min_n, mtry

if(setup_datasplit == TRUE) # standard optimal split based on Joseph (2022)
{
  split_ratio <- sqrt(3)/(sqrt(3)+1) #based on Joseph (2022) and 3 hparams
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
    split_ratio <- user_customratio
    dat_split <- rsample::initial_time_split(in_data, prop = split_ratio)
  }
}

write_csv(training(dat_split), file = here::here("output", user_modelname, "training.csv"))
write_csv(testing(dat_split), file = here::here("output", user_modelname, "testing.csv"))
write(paste("Train/test splitting ratio: ", split_ratio),
      file = here::here("output", user_modelname,"info.txt"), append = TRUE)


# ** training cross-validation -------------------------------------------

kfold_train <- get_kfold(k_data_in = training(dat_split),
                         k_init_time = k_init_time_train,
                         k_assess = k_assess_train,
                         k_cumulative = k_cumulative_train,
                         k_skip = k_skip_train,
                         k_lag= k_lag_train)

train_slices <- dim(kfold_train)[1]

train_baked <- list()

for(i in 1:(train_slices))
{
  train_baked[[i]] <- get_baked(kfold = kfold_train,
                                splitN=i)
}

# * * training single fit -------------------------------------------------

test_simple <- get_test_bake(training(dat_split))

# ** optional testing cross-validation ---------------------------------------

if(user_testmethod > 1)
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

min_n_maximum <- dim(train_baked[[1]]$baked_squid)[1]


tune_grid <- dials::parameters(min_n(),mtry())  %>%
    dials::finalize(mtry(), x = in_data  %>% dplyr::select(-target, -time))
tune_grid$object[[1]]$range$upper <-min_n_maximum

# alt method, get params with range of tree numbers incl:
# tune_grid <- parameters(min_n(),mtry(), trees(range(test_options$tree)))  %>%
#   dials::finalize(mtry(), x = baked_data[[1]]$baked_squid %>% dplyr::select(-year, -seas, -target))
# tune_grid$object[[1]]$range$upper <-min_n_maximum

# * create grid -----------------------------------------------------------

# grid size
if(setup_hgrid == F) {setup_gridmax = user_hparam_grid_max}
if(!is.logical(setup_hgrid)) {
  err_hparam_grid <- "ERROR: designate TRUE or FALSE for 'setup_hgrid' for a full or reduced hyperparamter grid size (respectively)"
  print(err_hparam_grid)
  write(err_hparam_grid, file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
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
  write(err_hparam_grid, file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
}

# * training --------------------------------------------------------------

train_time <- system.time({
  print("starting hyperparam tuning-- please be patient")
  print(paste("start time: ", Sys.time()))
  squid_forests <- list()
  # parallel processing of the forests
  for(i in 1:(train_slices)) #for each k-fold
  { #in the following step, you are generating every hparam combo for a single k-fold
    squid_forests[[i]] <- furrr::future_pmap(list(mtry = (design_set$mtry),
                                                  ntrees = (design_set$trees),
                                                  minn = (design_set$min_n),
                                                  splitrule = (design_set$splitrule)),
                                             get_forest,
                                             analy_data = train_baked[[i]]$baked_squid,
                                             assm_data =  train_baked[[i]]$baked_assessment_squid,
                                             .progress = TRUE)

  }
  # plan(sequential)
  gc()
  # arrange forest results, and extract rmse
  print("almost done with hyperparam tuning....")
  print(paste("start saving time: ", Sys.time()))
  for(j in 1:dim(design_set)[1])
  {
    if(j == 1)
    {
      r_forest_all <- list()
      rmse_slice <- vector(length = dim(design_set)[1]) #rmse across all slices for each hyperparam combo
      mae_slice <- vector(length = dim(design_set)[1])
      rsq_slice <- vector(length = dim(design_set)[1])
      rpd_slice <- vector(length = dim(design_set)[1])
    }
    obsv_pred_assm <- NULL  #re-set for each hyperparam set
    for(i in 1:train_slices)
    {
      if(i ==1){r_forests <- list()}
      r_forests[[i]] <- squid_forests[[i]][[j]]
      obsv_pred_assm <- obsv_pred_assm %>% bind_rows(r_forests[[i]]$assm_pred)
    }
    r_forest_all[[j]] <- r_forests
    # rmse is across all k-folds for each unique hparam set
    rmse_slice[j] <-  yardstick::rmse_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
    mae_slice[j] <-  yardstick::mae_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
    rsq_slice[j] <-  yardstick::rsq_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
    rpd_slice[j] <-  yardstick::rpd_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
    # future dev: see yardstick package for more metric options: https://yardstick.tidymodels.org/articles/metric-types.html

  }
  print(paste("end hyperparam tuning time: ", Sys.time()))
})

# FIND THE BEST PARAMETER SET
# 1 = rmse
# 2 = mae
# 3 = rsq
# 4 = rpd

best_metric <- dplyr::case_when(user_hp_select == 1 ~ min(rmse_slice),
                                user_hp_select == 2 ~ min(mae_slice),
                                user_hp_select == 3 ~ max(rsq_slice),
                                user_hp_select == 4 ~ min(rpd_slice))

best_hyperparam_set <- dplyr::case_when(user_hp_select == 1 ~ which.min(rmse_slice),
                                        user_hp_select == 2 ~ which.min(mae_slice),
                                        user_hp_select == 3 ~ which.max(rsq_slice),
                                        user_hp_select == 4 ~ which.min(rpd_slice))
# SAVE BEST TUNED MODEL
best_ntrees <- r_forest_all[[best_hyperparam_set]][[1]]$model$fit$num.trees
best_mtry <- r_forest_all[[best_hyperparam_set]][[1]]$model$fit$mtry
best_minn <-r_forest_all[[best_hyperparam_set]][[1]]$model$fit$min.node.size
best_splitrule <-r_forest_all[[best_hyperparam_set]][[1]]$model$fit$splitrule

train_results <- bind_cols(ntrees = best_ntrees,
                           mtry = best_mtry,
                           min_n = best_minn,
                           splitrule = best_splitrule)

# SAVE VARIABLE IMPORTANCE FOR ALL FOLDS IN TUNED MODEL
for(i in 1: train_slices)
{
  if(i == 1){var_import_slices_train <- NULL}
  var_import <- r_forest_all[[best_hyperparam_set]][[i]]$var_importance %>%
    as_tibble() %>%
    bind_cols(names = names(r_forest_all[[best_hyperparam_set]][[i]]$var_importance)) %>%
    pivot_wider(values_from = value, names_from = names)
  var_import_slices_train <- bind_rows(var_import_slices_train, var_import)
}

# trained model info:
write(paste("Time elapsed for model training (mins): ", round(train_time[3], digits = 2)),
      file = here::here("output", user_modelname, "info.txt"), append = TRUE)
write(paste("Design set number of combos: ", dim(design_set)[1]),
      file = here::here("output", user_modelname, "info.txt"), append = TRUE)
write_csv(train_results, path = here::here("output", user_modelname, "train_hparam_results.csv"))

# save best model info
dir_path <-  here::here("output", user_modelname, "tuned_model.txt")
sink(dir_path)
print(r_forest_all[[best_hyperparam_set]])
sink()


# * uncertainty check -----------------------------------------------------

check_grid <- bind_cols(mtry = rep(best_mtry, times = user_uncertainty),
                        ntrees = rep(best_ntrees, times = user_uncertainty),
                        minn = rep(best_minn, times = user_uncertainty),
                        splitrule = rep(best_splitrule, times = user_uncertainty))

ptime_check <- system.time({

  print("checking the forest for bears-- please be patient")
  print(paste("start time: ", Sys.time()))
  squid_forests_check <- list()
  # parallel processing of the forests
  for(i in 1:(train_slices))
  { squid_forests_check[[i]] <- furrr::future_pmap(list(mtry = (check_grid$mtry), ntrees = (check_grid$ntrees),
                                                        minn = (check_grid$minn), splitrule = (check_grid$splitrule)),
                                                   get_forest,
                                                   analy_data = train_baked[[i]]$baked_squid,
                                                   assm_data =  train_baked[[i]]$baked_assessment_squid,
                                                   .progress = TRUE)
  }
  # plan(sequential)
  gc()
  print("almost done checking for bears....")
  print(paste("start saving time: ", Sys.time()))
  # arrange forest results, and extract rmse
  for(j in 1:dim(check_grid)[1])
  {
    if(j == 1)
    {
      r_forest_all_checks <- list()
      rmse_slice_checks <- vector(length = dim(check_grid)[1]) #rmse across all slices for each check
      mae_slice_checks <- vector(length = dim(check_grid)[1])
      rsq_slice_checks <- vector(length = dim(check_grid)[1])
      rpd_slice_checks <- vector(length = dim(check_grid)[1])

      obsv_pred <- list()
      obsv_pred_assm_only  <- list()
      obsv_pred_analy_only <- list()
    }
    obsv_pred_assm_tmp <- NULL
    obsv_pred_analy_tmp <- NULL
    obsv_pred_tmp <- NULL   #re-set for each hyperparam set
    for(i in 1:train_slices)
    {
      if(i ==1){r_forests <- list()}
      r_forests[[i]] <- squid_forests_check[[i]][[j]]
      # obsv_pred_assm_checks <- obsv_pred_assm_checks %>% bind_rows(r_forests[[i]]$assm_pred)
      obsv_pred_assm_tmp <- obsv_pred_assm_tmp %>% bind_rows(r_forests[[i]]$assm_pred)
      obsv_pred_analy_tmp <- obsv_pred_analy_tmp %>% bind_rows(r_forests[[i]]$analy_pred)
      obsv_pred_tmp <- obsv_pred_tmp %>% bind_rows(obsv_pred_analy_tmp, obsv_pred_assm_tmp)
    }
    r_forest_all_checks[[j]] <- r_forests #note: r_forest_all_checks[[check number]][[slice]]
    rmse_slice_checks[j] <-  yardstick::rmse_vec(truth = obsv_pred_assm_tmp$target, estimate = obsv_pred_assm_tmp$.pred)
    mae_slice_checks[j] <- yardstick::mae_vec(truth = obsv_pred_assm_tmp$target, estimate = obsv_pred_assm_tmp$.pred)
    rsq_slice_checks[j] <- yardstick::rsq_vec(truth = obsv_pred_assm_tmp$target, estimate = obsv_pred_assm_tmp$.pred)
    rpd_slice_checks[j] <- yardstick::rpd_vec(truth = obsv_pred_assm_tmp$target, estimate = obsv_pred_assm_tmp$.pred)

    obsv_pred_assm_only <- obsv_pred_assm_only %>% bind_rows(obsv_pred_assm_tmp)
    obsv_pred_analy_only <- obsv_pred_analy_only %>% bind_rows(obsv_pred_analy_tmp)
    obsv_pred <- obsv_pred %>% bind_rows(obsv_pred_tmp)
  }
})
print(paste("end uncertainty time: ", Sys.time()))

obsv_pred_assm_only <- obsv_pred_assm_only %>% rename(obsv_cdfg = target, pred = .pred)
obsv_pred_analy_only <- obsv_pred_analy_only %>% rename(obsv_cdfg = target, pred = .pred)
obsv_pred <- obsv_pred %>% rename(obsv_cdfg = target, pred = .pred)

uncertainty_split <- case_when(user_uncertainty_metric == 1 ~ max(rmse_slice_checks) - min(rmse_slice_checks),
                               user_uncertainty_metric == 2 ~ max(mae_slice_checks) - min(mae_slice_checks),
                               user_uncertainty_metric == 3 ~ max(rsq_slice_checks) - min(rsq_slice_checks),
                               user_uncertainty_metric == 4 ~ max(rpd_slice_checks) - min(rpd_slice_checks))

write(paste("Uncertainty split: ", uncertainty_split),
      file = here::here("output", user_modelname,"info.txt"), append = TRUE)

if(uncertainty_split > user_uncertain_cutoff)
{
  write(paste("WARNING: The uncertainty split is > your assigned cutoff value"), file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
  write(paste("uncertainty split = ", uncertainty_split), file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
  write(paste("user assigned cutoff value = ", user_uncertain_cutoff), file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
  write(paste("Check that your assigned cutoff value is correct; if so, check features chosen,  feature sparseness, and model configurations; consider more hyperparameter combinations"),
        file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
}

# set best model based on best metric
best_check_set <-   case_when( user_selex_metric == 1 ~ which.min(rmse_slice_checks),
                               user_selex_metric == 2 ~ which.min(mae_slice_checks),
                               user_selex_metric == 3 ~ which.max(rsq_slice_checks),
                               user_selex_metric == 4 ~ which.min(rpd_slice_checks))


for(i in 1:train_slices)
{
  print(r_forest_all_checks[[best_check_set]][[i]]$model)
}
# set fitted model to last fold of best check set
best_squid_rf <- r_forest_all_checks[[best_check_set]][[train_slices]]$model


# * trees check -----------------------------------------------------------

# CIA note: Goldstein et all 2011 (genetics): data sparseness problem in ecology

# test your best fitting model on increasing numbers of trees; assess range

ptime_trees <- system.time({
  print("planting trees in the forest-- please be patient")
  print(paste("start time: ", Sys.time()))
  squid_forests_trees <- list()
  # parallel processing of the forests
  for(i in 1:(train_slices))
  { squid_forests_trees[[i]] <- furrr::future_pmap(list(mtry = best_mtry, ntrees = (user_treecheck),
                                                        minn = best_minn, splitrule = best_splitrule),
                                                   get_forest,
                                                   analy_data = train_baked[[i]]$baked_squid,
                                                   assm_data =  train_baked[[i]]$baked_assessment_squid,
                                                   .progress = TRUE)
  }
  # plan(sequential)
  gc()
  print("almost done planting trees....")
  print(paste("start saving time: ", Sys.time()))
  # arrange forest results, and extract rmse
  for(j in 1:length(user_treecheck))
  {
    if(j == 1)
    {
      r_forest_alltrees <- list()
      rmse_slice_trees <- vector(length = length(user_treecheck)) #rmse across all slices for each hyperparam combo
    }
    obsv_pred_assm_trees <- NULL  #re-set for each hyperparam set
    for(i in 1:train_slices)
    {
      if(i ==1){r_forests <- list()}
      r_forests[[i]] <- squid_forests_trees[[i]][[j]]
      obsv_pred_assm_trees <- obsv_pred_assm_trees %>% bind_rows(r_forests[[i]]$assm_pred)
    }
    r_forest_alltrees[[j]] <- r_forests #note: r_forest_alltrees[[tree number]][[slice]]
    rmse_slice_trees[j] <-  yardstick::rmse_vec(truth = obsv_pred_assm_tmp$target, estimate = obsv_pred_assm_tmp$.pred)
  }
})
print(paste("end trees time: ", Sys.time()))

# * testing ---------------------------------------------------------------


# * * single fit ----------------------------------------------------------
# predict with best rf fit

test_pred <- as_tibble(predict(object = best_squid_rf, new_data = test_simple)) %>%
  bind_cols(target = test_simple$target) %>%
  rename(pred = .pred)

# RESIDUALS OUT OF THE APPLIED RF MODEL TO GET RMSE
test_results <- test_pred %>%
  summarise(rsquared = yardstick::rsq_vec(truth = target, estimate = pred),
            rmse = yardstick::rmse_vec(truth = target, estimate = pred))

# save:
test_single_save <- list("test_results" = test_results, "test_predictions" = test_pred,
                              "squid_test_data" = test_simple)

dir_path <-  here::here("output", user_modelname,"test_predictions_singlefit.txt")

sink(dir_path)
print(test_single_save)
sink()

# * * k-fold fit ----------------------------------------------------------

# test_baked[[i]]
# test_slices

ptime_test <- system.time({
for(i in 1:(test_slices))
{
  if(i == 1){squid_forests <- list()}
  print(paste("re-tuning with time step: ", test_baked[[i]]$baked_assessment_squid$time[1]))
  print(paste("start time: ", Sys.time()))
  # bind the full training data set to the first fold of the testing set
  new_tune <- bind_rows(train_baked[[train_slices]]$baked_squid,
                        train_baked[[train_slices]]$baked_assessment_squid,
                        test_baked[[i]]$baked_squid)

  squid_forests[[i]] <- furrr::future_pmap(list(mtry = (design_set$mtry), ntrees = (design_set$trees),
                                                minn = (design_set$min_n), splitrule = (design_set$splitrule)),
                                           get_forest,
                                           analy_data = new_tune,
                                           assm_data =  test_baked[[i]]$baked_assessment_squid,
                                           .progress = TRUE)
}


gc()
# arrange forest results, and extract rmse
print("almost done with testing analysis....")
print(paste("start saving time: ", Sys.time()))

# FIND THE BEST PARAMETER SET **FOR EACH SLICE**
for(i in 1:test_slices)
{
  if(i == 1){best_rmse = vector(length = test_slices)
  best_hyperparam_set = vector(length = test_slices)}
  # within every slice, there is each hyperparam set
  for(j in 1:dim(design_set)[1])
  {
    if(j == 1) {rmse_each = vector(length = dim(design_set)[1])}
    rmse_each[j] = yardstick::rmse_vec(truth = squid_forests[[i]][[j]]$assm_pred$target,
                                       estimate = squid_forests[[i]][[j]]$assm_pred$.pred)
  }
  best_rmse[i] <- min(rmse_each)
  best_hyperparam_set[i] <- which.min(rmse_each)
}

# SAVE THE BEST FIT FOR EACH SLICE (lowest rmse of any hyperparam set for each set of years predicted)
for(i in 1:test_slices)
{
  if(i == 1){best_ofeach_slice <- list()}
  # save info from each slice and best set
  best_ofeach_slice[[i]] <- squid_forests[[i]][[best_hyperparam_set[i]]]
}
})

# * output ----------------------------------------------------------------

# training output:
# hyperparameter stuff:

# * * training diagnostic plots --------------------------------------------

# save histogram
p_hist_rmse <- ggplot() +
  geom_histogram(aes(rmse_slice), fill = "black", color = "white") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0))+
  labs(x = "RMSE",
       y = "Frequency")
p_hist_mae <- ggplot() +
  geom_histogram(aes(mae_slice), fill = "black", color = "white") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0))+
  labs(x = "MAE",
       y = "Frequency")
p_hist_rsq <- ggplot() +
  geom_histogram(aes(rsq_slice), fill = "black", color = "white") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0))+
  labs(x = "R^2",
       y = "Frequency")
p_hist_rpd <- ggplot() +
  geom_histogram(aes(rpd_slice), fill = "black", color = "white") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0))+
  labs(x = "RPD",
       y = "Frequency")

# save ggplots
p_hyperparams_trees <- ggplot() +
  geom_point(aes(x = design_set$trees, y = rmse_slice, color = as.factor(design_set$min_n)), size = 2) +
  theme_classic() +
  labs(x = "Number of trees",
       y = "RMSE",
       color = "Min num nodes") #+
# scale_color_manual(values=c("#762a83", "#1b7837"))

p_hyperparams_trees2 <- ggplot() +
  geom_point(aes(x = design_set$trees, y = rmse_slice, color = as.factor(design_set$mtry)), size = 2) +
  theme_classic() +
  labs(x = "Number of trees",
       y = "RMSE",
       color = "Num predictors") #+
# scale_color_manual(values=c("#762a83", "#1b7837"))


p_hyperparams_minn <- ggplot() +
  geom_point(aes(x = design_set$min_n, y = rmse_slice, color = as.factor(design_set$trees)), size = 2) +
  theme_classic() +
  labs(x = "Minimum number of data points per node",
       y = "RMSE",
       color = "Number of trees") #+
# scale_color_manual(values=c("#762a83", "#1b7837"))


p_hyperparams_mtry <- ggplot() +
  geom_point(aes(x = design_set$mtry, y = rmse_slice, color = as.factor(design_set$trees)), size = 2) +
  theme_classic() +
  labs(x = "Number of predictors randomly sampled",
       y = "RMSE",
       color = "Number of trees") #+
# scale_color_manual(values=c("#762a83", "#1b7837"))

# trees

tree_plot <-
  bind_cols(design_set, rmse_slice=rmse_slice) %>%
  ggplot()+
  geom_line(aes(x = trees, y = rmse_slice)) +
  geom_point(aes(x = trees, y = rmse_slice)) +
  theme_classic()+
  xlab("Number of trees")+
  ylab("RMSE")+
  # facet_wrap(~combo)
  facet_wrap(vars(min_n, mtry), labeller = "label_both") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
# tree_plot
if(test_mode == TRUE) {tree_plot}

# SAVE hyperparameter plots

get_plot_save(plot = p_hist_rmse, plotname_png = "p_hist_rmse.png", model_name = user_modelname)
get_plot_save(plot = p_hist_mae, plotname_png = "p_hist_mae.png", model_name = user_modelname)
get_plot_save(plot = p_hist_rsq, plotname_png = "p_hist_rsq.png", model_name = user_modelname)
get_plot_save(plot = p_hist_rpd, plotname_png = "p_hist_rpd.png", model_name = user_modelname)
get_plot_save(plot = p_hyperparams_trees, plotname_png = "p_hyperparams_trees.png", model_name = user_modelname)
get_plot_save(plot = p_hyperparams_trees2, plotname_png = "p_hyperparams_trees2.png", model_name = user_modelname)
get_plot_save(plot = p_hyperparams_minn, plotname_png = "p_hyperparams_minn.png", model_name = user_modelname)
get_plot_save(plot = p_hyperparams_mtry, plotname_png = "p_hyperparams_mtry.png", model_name = user_modelname)
get_plot_save(plot = tree_plot, plotname_png = "tree_plot.png", width = 8, height = 20, model_name = user_modelname)


# tree check with best training model:
tree_check_plot <- ggplot()+
  geom_line(aes(x = user_treecheck, y = rmse_slice_trees)) +
  geom_point(aes(x = user_treecheck, y = rmse_slice_trees)) +
  theme_classic()+
  xlab("Number of trees")+
  ylab("RMSE")

get_plot_save(plot = tree_check_plot, plotname_png = "tree_effect.png", width = 6, height = 5, model_name = user_modelname)


# * * variable importance -------------------------------------------------

# variable importance:
write_csv(var_import_slices_train, path = here::here("output", user_modelname, "train_var_import.csv"))


# * * model-agnostic methods ----------------------------------------------

# CIA: here: https://christophm.github.io/interpretable-ml-book/global-methods.html
