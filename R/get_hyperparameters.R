#' Set up hyperparameter grid
#'
#' @param train_baked
#' @param in_data
#' @param setup_hgrid defaults to creating a grid
#' @param setup_newgrid defaults to creating a FULL grid
#' @param setup_gridfile defaults to creating grid, not reading in
#' @param setup_gridfilename defaults to 'grid.csv'
#' @param user_hparam_grid_max defaults to 100
#' @param user_treevec default seq 10 to 500
#' @param user_splitrule defaults to 'extratrees' method, can also specify 'variance'
#'
#' @return final hyperparameter grid
#' @export
#'
#' @examples
get_hyperparameters <- function(train_baked,
                                in_data,
                                setup_hgrid = TRUE,
                                setup_newgrid = TRUE,
                                setup_gridfile = FALSE,
                                setup_gridfilename = "grid.csv",
                                user_hparam_grid_max = 100,
                                user_treevec = c(10, 50, seq(from = 100, to = 500, by = 100)),
                                user_splitrule = "extratrees")
{
  min_n_maximum <- dim(train_baked[[1]]$baked_squid)[1]


  tune_grid <- dials::parameters(min_n(),mtry(), trees(range(user_treevec)))  %>%
    dials::finalize(mtry(), x = in_data  %>% dplyr::select(-target, -time))
  tune_grid <- dials::parameters(min_n(),mtry(), trees(range(user_treevec)))  %>%
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
                                mtry = seq(from = tune_grid$object[[2]]$range$lower, to = tune_grid$object[[2]]$range$upper, by = 1),
                                trees = user_treevec,
                                splitrule = user_splitrule) %>%
        rowid_to_column(var = 'combo')
      write_csv(design_set, file = here("input", paste0("grid.csv")))
      write_csv(design_set, file = here("input", paste0("grid", Sys.Date(),".csv")))
    }
    if(setup_hgrid == FALSE) # max size design set
    {
      design_set <- dials::grid_space_filling(tune_grid, size = user_hparam_grid_max) %>%
        # expand_grid(trees = user_treevec) %>%
        expand_grid(splitrule = user_splitrule)
      write_csv(design_set, file = here("input", paste0("grid.csv")))
      write_csv(design_set, file = here("input", paste0("grid", Sys.Date(),".csv")))
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

  return(design_set = design_set)
}
