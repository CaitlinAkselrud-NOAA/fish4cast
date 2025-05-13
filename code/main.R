#' Random forest regression forecasting with eco time series data
#' Code by: Caitlin Allen Akselrud
#' Contact: caitlin.allen_akselrud at noaa.gov
#' Initiation date: 2024-07-22
#'

# Notes:
#  in_ are user specified variables for the input data
#  setup_ are user specified TRUE/FALSE settings
#  user_ are user specified numeric values or vectors

# CIA: put wrapper function around all 'get_...' functions

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
library(iml)
library(glmnet)
library(partykit)

# required for functions (add to DESCRIPTION dependencies; then run devtools::document to add to NAMESPACE file)

# required for this script

# load functions
devtools::load_all() #load package functions fish4cast; development

# functions ---------------------------------------------------------------

# read in functions from the 'functions' folder
# functions <- list.files(here::here("R"))
# purrr::walk(functions, ~ source(here::here("R", .x)))

# sk_explain1 <- list.files(here::here("helpers", "scikit-explain-0.1.4",
#                                      "skexplain", "main", "PermutationImportance"))
# purrr::walk(functions, ~ py_run_file(here::here("helpers", "scikit-explain-0.1.4",
                                                # "skexplain", "main", "PermutationImportance", .x)))

# model name
user_modelname <- "base_simple"

# set up files for input/output data

get_file_folders(user_modelname)

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
in_features <- in_cols[1:7] #simple example

# designate which input column is the target
in_target <- in_cols[8] #simple example

# designate which column is time (if using)
# CIA note: currently required, can add option later
in_time <- in_cols[9] #simple example

# need consistent naming of target and time vars
in_data %<>%  dplyr::rename(target = base_simple) #simple example
in_data %<>%  dplyr::rename(time = sim_year) #simple example

# additional data cleaning
# in_data %<>% dplyr::select()

# future: spatial?

# data splits:
# set distribution for each feature
all_features <- in_data %>% dplyr::select(-time, -target)
dists_feat<-lapply(all_features, function(x){x = "norm"})

# how many states to test for in regime testing (max):
max_states_test <- 2
# CIA: currently error in clusters when >2 (test and add tryCatch to return NA when fails)
# how many interations per HMM for regime testing:
n_iters <- 200

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
setup_newgrid <- FALSE

# if you want to read in a specific grid file, set to TRUE and put in the .csv filename (fle must be saved in inputs folder)
setup_gridfile <- TRUE
setup_gridfilename <- "grid.csv"

#set a maximum number of hyperparameter combos here if desired
user_hparam_grid_max <- 100

# set the vector of numbers of trees to test in training
# user_treevec <- c(10, 100, 500) #simple example
user_treevec <- c(10, 50, seq(from = 100, to = 500, by = 100))

# set the splitrule you want to use for tree construction
user_splitrule <- c("extratrees")

# uncertainty check:
# # a vector of numbers or single value over which to re-run trained model to assess uncertainty
# user_checklen <- c(50, 100, 150) #simple example
user_checklen <- c(50, 100, 200, 400)

user_treecheck <- c(10, 50, seq(from = 100, to = 5000, by = 100))
# * * cross-validation structure ------------------------------------------

# do you want a standard data split based on optimal splitting work by Joseph (2022) or custom split?
# TRUE = standard split
# FALSE = custom split
setup_datasplit <- FALSE

# if you have a custom split, is it based on data results or user setting?
# TRUE = based on automated data results
# FALSE = user specified ratio
setup_customsplit <- TRUE

# a custom ratio must be > 0 and < 1; 0.60 to 0.75 is standard in the literature
# the custom ratio represents the proportion of training data
user_customratio <- 0.67

# # tell cross-validation how to structure k-folds
k_init_time_train = NA # set to NA to use default values; number of data rows in first k-fold training data; default setting = 1/3 training data
k_assess_train = NA # set to NA to use default values; number of samples for each assessment resample; default = 1
k_cumulative_train = NA # set to NA to use default values; continue to build years in each subsequent step; default = T
k_skip_train = NA # set to NA to use default values; don't skip any resamples (thins thins data); default = 0
k_lag_train = NA # set to NA to use default values; lag btw assessment and analysis sets...); default = 0

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
# user_uncertainty <- 100 #simple example
user_uncertainty <- 500

# which metric are you using for uncertainty cutoff?
# current options:
# 1 = rmse; root mean squared error, same units as orig data
# 2 = mae; mean absolute error, same units as orig data
# 3 = rsq; coeeficent of determination using correlation (not traditional SSQ method)
# 4 = rpd; ratio of performance to deviation measures of consistency/correlation between observed and predicted values (and not of accuracy)
user_uncertainty_metric <- 1

# what is your cutoff for uncertainty?
# user_uncertain_cutoff <- 2 #simple example
user_uncertain_cutoff <- round(0.05*min(in_data$target),0) #within 5% of the lowest fished value


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
test_mode <- FALSE
# this is for the parallelization of the design set, tree check, and final check
if(test_mode == TRUE) {n_workers <- 1
} else{n_workers <- (availableCores()-2)}
future::plan(future::multicore, workers = n_workers)

# CIA: add output for all user-specified settings for tracking projects

# END OF USER SETTINGS SECTION --------------------------------------------

# Insert wrapper functions here

# -> THE FOLLOWING CODE IS AUTOMATED.

all_input <- get_input_data(in_ts = in_ts, in_data = in_data, max_states_test = max_states_test,
               all_features = all_features, dists_feat = dists_feat, n_iters = n_iters,
               setup_datasplit = setup_datasplit, setup_customsplit = setup_customsplit)

all_input

# shift detected 2007-2008; std recommended split btw 2013-2014

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

# Variance Inflation Factor (VIF) is a statistical measure used in regression analysis to quantify the severity of multicollinearity, or the correlation between independent variables, and its impact on the variance of estimated regression coefficients
# https://cran.r-project.org/web/packages/car/car.pdf

# output detected elements; default = use best settings; user can modify

# * input data setup & splits ------------------------------------------------------

# split training and testing section

# ** training vs testing -------------------------------------------------

# future: currently all based around initial_time_split; future: add feature for random splits

# ** training cross-validation -------------------------------------------

folds <- get_folds(input_data = all_input,
           k_init_time_train,
           k_assess_train,
           k_cumulative_train,
           k_skip_train,
           k_lag_train,
           user_testmethod,
           k_init_time_test,
           k_assess_test,
           k_cumulative_test,
           k_skip_test,
           k_lag_test)



# random forest -----------------------------------------------------------

# * setup -----------------------------------------------------------------

# * hyperparameters -------------------------------------------------------

# auto-detect min_n, mtry
design_set <- get_hyperparameters(train_baked = folds$train_baked,
                    in_data = all_input$in_data,
                    setup_hgrid = setup_hgrid,
                    setup_newgrid = setup_newgrid,
                    setup_gridfile = setup_gridfile,
                    setup_gridfilename = setup_gridfilename,
                    user_hparam_grid_max = user_hparam_grid_max,
                    user_treevec = user_treevec,
                    user_splitrule = user_splitrule)
# cia: check default settings

# * training --------------------------------------------------------------

train <- get_training(train_slices = folds$train_slices,
             design_set = design_set,
             train_baked = folds$train_baked,
             user_hp_select = user_hp_select,
             user_modelname = user_modelname)


# cia: side note here - thinking about model selex

# * uncertainty check -----------------------------------------------------

uncertainty <- get_check_uncertainty(train_mod = train,
                                     train_baked = folds$train_baked,
                                     train_slices = folds$train_slices,
                                     user_uncertainty = user_uncertainty,
                                     user_uncertainty_metric = user_uncertainty_metric,
                                     user_uncertain_cutoff = user_uncertain_cutoff,
                                     user_selex_metric = user_selex_metric,
                                     user_modelname = user_modelname)

# * trees check -----------------------------------------------------------

# CIA note: Goldstein et all 2011 (genetics): data sparseness problem in ecology

# test your best fitting model on increasing numbers of trees; assess range

check_tree <- get_check_trees(train_mod = train,
                              train_baked = folds$train_baked,
                              train_slices = folds$train_slices,
                              user_treecheck = user_treecheck,
                              user_modelname = user_modelname)

# * testing ---------------------------------------------------------------
test_single <- get_test_single(uncertainty = uncertainty,
                folds = folds,
                user_modelname = user_modelname)
# cia: fiddle with output obsv v exp plot

# * * k-fold fit ----------------------------------------------------------

# test_baked[[i]]
test_folds <- get_test_folds(test_baked = folds$test_baked,
                             train_baked = folds$train_baked,
                             design_set = design_set,
                             test_slices = folds$test_slices)
# cia: need to add selex metric to this fxn and plot


plot(x = test_folds$target, y = test_folds$.pred)
test_folds_p <- ggplot(test_folds, aes(x = target, y = .pred, label = time)) +
  # geom_point(aes(x = target, y = .pred, alpha = time, size = time), color = "darkblue") +
  geom_point(aes(size = time, color = as.factor(time))) +
  geom_text( size = 1.5) +
  geom_abline()+
  theme_classic()+
  xlab("Observed")+
  ylab("Predicted")
test_folds_p
get_plot_save(plot = test_folds_p, plotname_png = "test_folds.png", width = 6, height = 5, model_name = user_modelname)

test_folds_p2 <- test_folds %>%
  group_by(slice) %>%
  arrange(time) %>%
  dplyr::filter(row_number()==n()) %>%
  mutate(diff = abs(target - .pred),
         diff_label = case_when(diff < 0.1 ~ "< 0.1",
                                diff >= 0.1 & diff <= 0.5 ~ "0.1-0.5",
                                diff > 0.5 ~ "> 0.5")) %>%
  ggplot(aes(x = target, y = .pred, label = time)) +
  # geom_point(aes(x = target, y = .pred, alpha = time, size = time), color = "darkblue") +
  geom_point(aes(color = diff_label), size = 10) +
  scale_color_manual(values=c("#a1d76a", "#e9a3c9", "#ffffbf")) +
  geom_text( size = 3) +
  geom_abline()+
  theme_classic()+
  xlab("Observed")+
  ylab("Predicted")
test_folds_p2
get_plot_save(plot = test_folds_p2, plotname_png = "test_folds_finalfold.png", width = 6, height = 5, model_name = user_modelname)







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


# * * variable importance -------------------------------------------------

# variable importance:
write_csv(var_import_slices_train, path = here::here("output", user_modelname, "train_var_import.csv"))


# * * model-agnostic methods ----------------------------------------------

# CIA: here: https://christophm.github.io/interpretable-ml-book/global-methods.html
#   "Global methods describe how features affect the prediction on average.
#   In contrast, local methods aim to explain individual predictions." (Molnar, IML book)

# fitted model
best_squid_rf
# test data:
test_simple
# train data:
train_final <- bind_rows(train_baked[[train_slices]]$baked_squid,
                         train_baked[[train_slices]]$baked_assessment_squid)
# predicted values
test_pred
# features
test_features <- test_simple[which(names(test_simple) != "target")]

rng <- ranger(target ~ ., data = in_data,
              num.trees = best_squid_rf$fit$num.trees,
              mtry = best_squid_rf$fit$mtry,
              min.node.size = best_squid_rf$fit$min.node.size,
              importance = "permutation",
              splitrule = best_squid_rf$fit$splitrule)

in_data <- test_simple[which(names(test_simple) != "target")]
pfun <- function(object, newdata) predict(object, data = newdata)$predictions
predictor <- Predictor$new(model = rng, data = in_data,
                           y = as.data.frame(test_simple$target %>% as.data.frame() %>% rename(target = '.')),
                           predict.fun = pfun)

imp_mae <- FeatureImp$new(predictor, loss = "mae")
imp_mse <- FeatureImp$new(predictor, loss = "mse")

imp_rmse <- FeatureImp$new(predictor, loss = "rmse")
p_imp_rmse <- plot(imp_rmse)
get_plot_save(plot = p_imp_rmse, plotname_png = "p_FeatureImp_rmse.png", model_name = user_modelname)

# CIA: also try scikit packages (maybe isntead of iml?)
# https://www.johannesbgruber.eu/post/2022-03-29-scikit-learn-models-in-r-with-reticulate/#fnref3
# CIA; maybe follow up chat with Monte Flora for asst. once set up to use in R


# SURROGATE MODELS (aka parsimony)

# FEATURES:
# 3) Partial dependence plots (PDP)
# 4) accumulated local effects plot
# 5) H-stat (feature interxn)
# 6) fxnal decomposition
# 7) permutation feature importance (importance plots)
# CIA: adapt for type of metric used: rmse, mse, mae, etc
# 8) ICE
# 9) anchors
# 10) Shapely
# 11) SHAP: In R, there are the shapper and fastshap packages. SHAP is also included in the R xgboost package.

# EXAMPLES/DATA:
# 12) example-based explanations: is there a subset that provides a good example
#     of ML results? also, analogies to illustrate results
#     (humans think in stories)
# 13) prototype/criticism points

