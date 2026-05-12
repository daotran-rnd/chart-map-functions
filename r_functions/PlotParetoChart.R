#' @title Plot a Pareto Chart (ABC Analysis) 
#'
#' @description
#' Generates an interactive Pareto chart using echarts4r with a dual-axis system: 
#' columns for individual values and a line for cumulative percentage.
#' The function sorts the data automatically by the value column in descending order 
#' and treats the category column as a character to ensure proper visualization 
#' on the x-axis. 
#' @author daotq
#'
#' @param data A data frame or tibble.
#' @param cate_col The column name for the x-axis (e.g., ProductKey or SupplierName).
#' @param val_col The column name for the columns (e.g., SalesValue or QuantitySold).
#' @param cum_col The column name for the cumulative percentage line (0-100).
#' @param title Chart title. Default is "Pareto Chart".
#'
#' @export

PlotParetoChart <- function(data, 
                            cate_col, 
                            val_col, 
                            cum_col, 
                            title = "Pareto Chart" ) {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(echarts4r, dplyr, htmlwidgets)
  
  # Preprocess data
  plot_data <- data %>%
    arrange(desc(!!sym(val_col))) %>%
    mutate(!!sym(cate_col) := as.character(!!sym(cate_col)))
  
  # Chart
  chart <- plot_data %>%
    e_charts_alpha(!!sym(cate_col)) %>% 
    
    ## Primary Axis
    e_bar_alpha(!!sym(val_col), name = val_col) %>%
    
    ## Secondary Axis
    e_line_alpha(!!sym(cum_col), name = "Cumulative %", y_index = 1, symbol = "none") %>%
    
    ## Configure Axes
    e_y_axis(
      name = val_col,
      splitLine = list(show = FALSE)
    ) %>%
    e_y_axis(
      name = "Cumulative %",
      index = 1,
      max = 100,
      min = 0,
      interval = 20,
      formatter = htmlwidgets::JS("function(value){ return value + '%'; }")
    ) %>%
    
    ## Interactivity and Styling
    e_tooltip(
      trigger = "axis",
      backgroundColor = "rgba(255, 255, 255, 1)",
      borderColor = "#333",
      borderWidth = 1,
      textStyle = list(color = "#000", fontSize = 12),
      extraCssText = "box-shadow: 0 0 10px rgba(0, 0, 0, 0.3); padding: 10px;"
    ) %>%
    e_title(title) %>%
    e_legend(bottom = 0) %>%
    e_theme("infographic") %>%
    e_datazoom(type = "slider", start = 0, end = 100) %>%
    e_x_axis(
      name = cate_col,
      axisLabel = list(
        interval = "auto",
        hideOverlap = TRUE,
        rotate = 45,
        fontSize = 8
      )
    )
  
  return(chart)
}
