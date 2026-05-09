#' @title Plot Interactive Box and Whisker (or Boxplot)
#'
#' @description Draw a single boxplot for a numeric column, or multiple boxplots
#' for a numeric value (y-axis) and a categorical value (x-axis)  
#' @author daotq
#' 
#' @param data A data frame.
#' @param value_col Unquoted name of the numeric column.
#' @param category_col Unquoted name of the categorical column (Optional).
#' @param chart_title Character. Title of the chart. Defaults to NULL.
#' @param color Character. Hex code for the boxplot color. Defaults to "#003366".
#'
#' @export

PlotBoxAndWhisker <- function(data, 
                              value_col, 
                              category_col, 
                              chart_title = NULL, 
                              color = "#003366") {
  
  # Setup
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(echarts4r, dplyr, rlang)
  
  # Capture variables 
  val_enquo <- rlang::enquo(value_col)
  cat_enquo <- rlang::enquo(category_col)
  val_name  <- rlang::as_name(val_enquo)
  
  # Data preparation 
  has_category <- !rlang::quo_is_null(cat_enquo)
  
  if (has_category) {
    cat_name <- rlang::as_name(cat_enquo)
    
    plot_data <- data %>%
      select(all_of(cat_name), all_of(val_name)) %>%
      filter(!is.na(!!cat_enquo), !is.na(!!val_enquo)) %>%
      group_by(!!cat_enquo)
    
    p <- plot_data %>%
      e_charts() %>%
      e_boxplot_(val_name, name = "Distribution")
    
  } else {
    # Value column name 
    plot_data <- data %>%
      select(all_of(val_name)) %>%
      filter(!is.na(!!val_enquo)) %>%
      mutate(!!val_name := val_name) %>% 
      group_by(!!val_enquo)
    
    p <- plot_data %>%
      e_charts() %>%
      e_boxplot_(val_name, name = val_name)
  }
  
  # Styling
  p <- p %>%
    e_theme("macarons") %>%
    e_color(color) %>%
    e_tooltip(trigger = "item") %>%
    e_toolbox_feature("saveAsImage") %>%
    e_toolbox_feature("restore") %>%
    e_datazoom(type = "slider", y_index = 0) %>% 
    e_datazoom(type = "inside", y_index = 0) %>% 
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
    e_grid(bottom = 120, left = "10%") 
  
  if (!is.null(chart_title)) {
    p <- p %>% e_title(chart_title)
  }
  
  return(p)
}
