#' split training and testing data
#' use regime info to split if selected option
#' @param setup_datasplit
#' @param setup_customsplit
#' @param in_data
#' @param reg_change
#'
#' @return
#' @export
#'
#' @examples
get_split_data <- function(setup_datasplit, setup_customsplit,
                           in_data, reg_change)
{
  # updated science on splitting, Joseph 2022
  # optimal ratio is: sqrt(num_params)/(sqrt(num_params)+1)
  # this implementation uses 3 hyperparameters- n_trees, min_n, mtry

  split_ratio <- sqrt(3)/(sqrt(3)+1) #based on Joseph (2022) and 3 hparams

  if(setup_datasplit == TRUE) # standard optimal split based on Joseph (2022)
  {
    dat_split <- rsample::initial_time_split(in_data, prop = split_ratio)
  }else if(setup_datasplit == FALSE)
  {
    if(setup_customsplit == TRUE) # use data exploration results to set split
    {
      split_check <- round(split_ratio*dim(in_data)[1], 0)
      if(any(split_check == reg_change)) #if you ARE splitting on a regime change
      {
        split_range <- seq(from = round(0.6*dim(in_data)[1],0),
                           to = round(0.75*dim(in_data)[1],0),
                           by = 1)
        split_update <- split_range[!split_range %in% split_check]
        if(any(reg_change %in% split_update))
        {
          # produce a warning that multiple regimes are detected within the splitting range
          # recommend custom user input after data analysis
          split_fix <- split_range[!split_range %in% reg_change]
          new_split <- split_fix[round(length(split_fix)/2, 0)]
          new_split_ratio <- new_split/dim(in_data)[1]
          dat_split <- rsample::initial_time_split(in_data, prop = new_split_ratio)
          write(paste("WARNING: multiple regime shifts detected in the train/test split;",
                      "RECOMMEND user reviews data, regimes, and number of regime states detected",
                      "regimes detected in time(s):",in_data$time[reg_change]),
                file = here::here("output", user_modelname,"info.txt"), append = TRUE)
          write(paste("Optimal split range is between times:",
                      min(in_data$time[split_range]), max(in_data$time[split_range])),
                file = here::here("output", user_modelname,"info.txt"), append = TRUE)
          write(paste("Current train/test split set to", in_data$time[new_split]),
                file = here::here("output", user_modelname,"info.txt"), append = TRUE)

        }else { #set the split halfway between the regime shift and upper bound of std splitting
          split_fix <- (reg_change+1):max(split_range)
          new_split <- split_fix[round(length(split_fix)/2, 0)]
          new_split_ratio <- new_split/dim(in_data)[1]
          dat_split <- rsample::initial_time_split(in_data, prop = new_split_ratio)
          write(paste("ATTENTION: a regime shift was detected in the train/test split;",
                      "regimes detected in time:",in_data$time[reg_change]),
                file = here::here("output", user_modelname,"info.txt"), append = TRUE)
          write(paste("Optimal split range is between times:",
                      min(in_data$time[split_range]), max(in_data$time[split_range])),
                file = here::here("output", user_modelname,"info.txt"), append = TRUE)
          write(paste("Current train/test split set to", in_data$time[new_split]),
                file = here::here("output", user_modelname,"info.txt"), append = TRUE)
        }

      }else { #if not, then use std. split
        dat_split <- rsample::initial_time_split(in_data, prop = split_ratio)}
      # split_ratio <- split_recommended
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


  return(dat_split)
}
