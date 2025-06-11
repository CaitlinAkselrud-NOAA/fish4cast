#' Create file folders to read input and put output
#'
#' @param user_modelname
#'
#' @return
#' @export
#'
#' @examples
get_file_folders <- function(user_modelname = "default")
{
  dir.create(here::here("input"), showWarnings = F)
  dir.create(here::here("output"), showWarnings = F)
  dir.create(here::here("output", user_modelname), showWarnings = F)
  dir.create(here::here("output", user_modelname, "diagnostic_plots"), showWarnings = F)


  file.create(here::here("output", user_modelname, "warnings.txt"), showWarnings = F)
  file.create(here::here("output", user_modelname, "info.txt"), showWarnings = F)
}
