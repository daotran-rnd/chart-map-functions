#' @title Plot Interactive Scatter Chart with Linear Trend
#'
#' @description
#' This function generates an interactive scatter plot using echarts4r and 
#'      adds a linear regression trend line.
#' @author daotq      
#'
#' @param data A data frame or tibble containing the variables to plot.
#' @param x_var The unquoted name of the column for the X-axis (e.g., income).
#' @param y_var The unquoted name of the column for the Y-axis (e.g., spending).
#' @export

PlotScatterChart <- function(data, x_var, y_var) {
  
  # setup
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(echarts4r, dplyr, rlang, broom)
  
  # variables
  x_nm <- as_name(enquo(x_var))
  y_nm <- as_name(enquo(y_var))
  
  # scatter chart
  data %>%
    e_charts_(x_nm) %>% 
    e_scatter_(y_nm, symbol_size = 7) %>%
    e_lm(as.formula(paste(y_nm, "~", x_nm)), name = "Linear Trend") %>% 
    e_tooltip(trigger = "axis") %>% 
    e_title(
      text = "Scatter Trend Analysis",
      subtext = paste("Relationship:", x_nm, "vs", y_nm)
    ) %>%
    e_x_axis(name = x_nm) %>%
    e_y_axis(name = y_nm) %>%
    e_datazoom(type = "inside") %>% 
    e_theme("walden") %>% 
    e_legend(bottom = 0)
}
