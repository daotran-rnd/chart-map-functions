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
  
  # Setup
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(echarts4r, dplyr, rlang)
  
  # Variables
  x_enquo <- enquo(x_var)
  y_enquo <- enquo(y_var)
  x_nm <- as_name(x_enquo)
  y_nm <- as_name(y_enquo)
  
  # Data
  plot_data <- data %>% ungroup() %>% filter(!is.na(!!x_enquo), !is.na(!!y_enquo))
  fit <- lm(as.formula(paste(y_nm, "~", x_nm)), data = plot_data)
  
  # Metrics
  intercept <- round(coef(fit)[1], 3)
  slope <- round(coef(fit)[2], 3)
  r_squared <- round(summary(fit)$r.squared, 3)
  eq_label <- paste0("y = ", slope, "x + ", intercept, " (R² = ", r_squared, ")")
  
  # Chart
  plot_data %>%
    e_charts_(x_nm) %>% 
    e_scatter_(y_nm, symbol_size = 7, name = "Data Points") %>% 
    e_lm(as.formula(paste(y_nm, "~", x_nm)), name = "Linear Trend", symbol = "none") %>%
    e_tooltip(
      trigger = "axis",
      axisPointer = list(type = "cross"),
      formatter = htmlwidgets::JS(paste0("
        function(params) {
          // Filtering series 'Data Points'
          var points = params.filter(p => p.seriesName === 'Data Points');
          var trend = params.find(p => p.seriesName === 'Linear Trend');
          
          var res = '<b>Group Analysis at X = ' + params[0].value[0] + '</b><br/>';
          
          if (points.length > 0) {
            var y_vals = points.map(p => p.value[1]);
            var count = y_vals.length;
            var avg = (y_vals.reduce((a, b) => a + b, 0) / count).toFixed(2);
            res += '● Sample size (n): ' + count + '<br/>';
            res += '● Average of ", y_nm, ": ' + avg + '<br/>';
          }
          
          if (trend) {
            res += '<hr style=\"margin: 5px 0\">';
            res += '<span style=\"color:#2ec7c9\">● Trendline: ", eq_label, "</span>';
          }
          
          return res;
        }
      "))
    ) %>% 
    e_title("Scatter Trend Analysis", subtext = paste("Total N =", nrow(plot_data))) %>%
    e_x_axis(name = x_nm, nameLocation = "center", nameGap = 30) %>%
    e_y_axis(name = y_nm) %>%
    e_theme("walden") %>% 
    e_legend(bottom = 0) %>%
    e_datazoom(type = "inside")
}
