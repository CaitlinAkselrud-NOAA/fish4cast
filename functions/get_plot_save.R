get_plot_save <- function(plot, plotname_png, model_name, width = 5, height = 6)
{
  ggsave(plot, filename = plotname_png,
         path = here::here("output", user_modelname, "diagnostic_plots"),
         width = width, height = height)
}
