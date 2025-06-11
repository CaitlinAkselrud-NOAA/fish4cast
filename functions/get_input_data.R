
#' get_input_data
#' wrapper function for generating input data (around get_regime and get_split_data)
#' @param in_ts
#' @param in_data
#' @param max_states_test
#' @param all_features
#' @param max_states_test
#'
#' @return
#' @export
#'
#' @examples
get_input_data <- function(in_ts, in_data, in_features, max_states_test, all_features, dists_feat, n_iters = 200,
                           setup_datasplit = TRUE, setup_customsplit = FALSE)
{
  # input data --------------------------------------------------------------

  # if your data is a time series, make sure it is arranged chronologically
  #   this is important later when you do your training/testing splits
  if(in_ts == TRUE){in_data <- in_data %>% arrange(time) }

  features <- in_features
  # features <- test_simple[which(names(test_simple) != "target")]
  # features_notime <- features[which(names(features) != "time")]

  # * input data exploration ------------------------------------------------
  # regime shift: assume discrete process (distinct shift)
  # based on Zoe Rand: HMMs
  if(setup_datasplit == FALSE)
  {if(setup_customsplit == TRUE) #test for regimes
  {
    regimes_aic <- vector(length = length(2:max_states_test))
    regimes_shift <- matrix(nrow = length(2:max_states_test),
                            ncol = dim(all_features)[1])
    for(i in 2:max_states_test)
    {
      # dat = just feature data;
      # dist = dist type (eg normal) for each feature as a list;
      # n-states-- here is where you can test multiple state settings, then later compare with AIC
      # niters = number repeats within one hmm fit test
      regimes <- get_regimes(dat = all_features,
                             dat_dist = dists_feat,
                             n_states = i, n_iters = n_iters)
      # get_regimes already does model selection for the best fit given a specific n_state
      regimes_aic[i-1] <- regimes$best_aic
      regimes_shift[i-1, ] <- regimes$best_states
    }

    # this is model selection for the best number of states, if you are testing for more than one state (e.g. 2-5 states possible)
    aic_selex <- which.min(regimes_aic) # what is the best fit number of regimes, if testing for more than one
    fit_regime <- regimes_shift[aic_selex, ]
    regime_periods <- bind_cols(in_data, regime_detect = fit_regime) %>%
      group_by(regime_detect) %>%
      summarise(regime_start = min(time),
                regime_end = max(time))

    reg_change <- which(diff(fit_regime) != 0) + 1

  }} else{reg_change <- -1; fit_regime <- NA; regime_periods <- NA}

  split_data <- get_split_data(setup_datasplit, setup_customsplit,
                               in_data, reg_change)

  return(list(in_data = in_data,
              all_features = in_data %>% dplyr::select(any_of(in_features)),
              reg_change = reg_change,
              fit_regime = fit_regime,
              regime_periods = regime_periods,
              split_data = split_data,
              training = training(split_data),
              testing = testing(split_data)))
}
