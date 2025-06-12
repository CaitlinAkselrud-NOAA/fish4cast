#' Observed vs predicted points plot
#'
#' @param data should include the diff column (target-pred) and the
#'              time_label column (how you want to name time points)
#' @param label_title title label
#' @param label_units units for rmse
#' @param label_legend label for the legend
#' @param plot_name save name for the plot
#' @param user_modelname
#'
#' @return
#' @export
#'
#' @examples
get_obsvpred_plot <- function(data,
                              sd_data,
                          label_title,
                          label_units,
                          label_legend,
                          plot_name,
                          user_modelname)
{
  sd_obsv_n <- round(sd_data, digits = 1)

  data_d <- data %>% ungroup() %>%
    arrange(time) %>%
    mutate(
      # sd_obsv = sd(target),
      sd_obsv = sd_data,
      diff_label = case_when(abs(diff) < sd_obsv ~ "< 1 SD",
                             abs(diff) >= sd_obsv & diff <2*sd_obsv ~ "> 1 SD",
                             abs(diff) >= 2*sd_obsv ~ "> 2 SD"),
      diff_color = case_when(diff_label == "< 1 SD" ~ "#B1DC6B",
                             diff_label == "> 1 SD" ~ "#FFAB38",
                             diff_label == "> 2 SD" ~ "#FF6C57"))

  limits <- c(min(c(data_d$target, data_d$pred)),
              max(c(data_d$target, data_d$pred)))

  data_p <-
    ggplot(data_d, aes(x = target, y = pred, label = time_label)) +
    geom_point( size = 10, aes(color = diff_label)) +
    scale_color_manual(values = c("< 1 SD" = "#B1DC6B", "> 1 SD" = "#FFAB38", "> 2 SD" = "#FF6C57"),
                       breaks = c("< 1 SD" , "> 1 SD", "> 2 SD"))+
    geom_text( size = 3, color = "black") + #, position=position_jitter(width=1,height=1)) +
    geom_abline()+
    theme_classic()+
    xlab("Observed")+
    ylab("Predicted") +
    ylim(limits[1], limits[2]) +
    labs(caption = paste("SD observed data =", sd_obsv_n, label_units),
         title = label_title) +
    guides(color = guide_legend(title =label_legend)) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8))
  data_p
  get_plot_save(plot = data_p, plotname_png = paste0(plot_name,".png"),
                width = 6, height = 7, model_name = user_modelname)

  return(data_p)
}
