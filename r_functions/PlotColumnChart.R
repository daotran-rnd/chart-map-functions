#' @title Plot Interactive Column Chart (Grouped or Stacked)
#' 
#' @description
#' Generates an interactive column chart. Supports both 
#' non-stacked (default) and stacked layouts with custom color palettes.
#' @author daotq 
#'
#' @param data A data frame containing the variables.
#' @param x_var The unquoted column name for the X-axis (categorical).
#' @param y_var The unquoted column name for the Y-axis (numeric).
#' @param group_var Optional: The unquoted column name for grouping (stacking/side-by-side).
#' @param stacked Logical, if TRUE, columns are stacked. Default is FALSE.
#' @param colors A character vector of hex colors. If NULL, uses theme defaults.
#' @export

PlotColumnChart <- function(data, x_var, y_var, 
                            group_var = NULL, 
                            title = "Column Chart", 
                            stacked = FALSE, 
                            colors = NULL) {
  
  # Setup
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(echarts4r, dplyr, rlang)
  
  # Capture vars
  x_nm <- as_name(enquo(x_var))
  y_nm <- as_name(enquo(y_var))
  group_enquo <- enquo(group_var)
  data_name <- deparse(substitute(data))
  
  # Initialize chart
    ## If group_var is provided, we need to group the data first
  if (!quo_is_null(group_enquo)) {
    p <- data %>%
      group_by(!!group_enquo) %>%
      e_charts_(x_nm)
    
    if (stacked) {
      p <- p %>% e_bar_(y_nm, stack = "grp") # "grp" is an arbitrary string to link stacks
    } else {
      p <- p %>% e_bar_(y_nm)
    }
  } else {
    
    ## Simple column chart without grouping
    p <- data %>%
      e_charts_(x_nm) %>%
      e_bar_(y_nm)
  }
  
  # Apply custom colors if provided
  if (!is.null(colors)) {
    p <- p %>% e_color(colors)
  }
  
  # Styling and Interactivity
  p %>%
    e_tooltip(trigger = "axis", axisPointer = list(type = "shadow")) %>%
    e_title( title, paste("Dataset:", data_name)) %>%
    e_x_axis(axisLabel = list(interval = 0, rotate = 30)) %>% # Rotate labels for readability
    e_theme("walden") %>%
    e_legend(bottom = 0) %>%
    e_grid(bottom = "15%") %>%
    e_datazoom(type = "inside")
}
