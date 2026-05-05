#' @title Plot interactive Donut Chart with a Total card
#'
#' @description
#' Generates an interactive Donut chart. Slices are sorted largest to smallest (clockwise).
#' @author daotq
#'
#' @param data A data frame containing the variables.
#' @param category_var The unquoted column name for categories.
#' @param value_var The unquoted column name for numeric values.
#' @param decimal_value Integer, number of decimal places to display (default is 2).
#'
#' @export

PlotDonutChart <- function(data, 
                           category_var, 
                           value_var, 
                           decimal_value = 2,
                           colors = NULL) {
  
  # Setup
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(echarts4r, dplyr, rlang, htmlwidgets)
  
  # Capture variable names and dataset name
  cat_nm <- as_name(enquo(category_var))
  val_nm <- as_name(enquo(value_var))
  data_name <- deparse(substitute(data)) 
  
  # Sort data descending for clockwise priority
  processed_data <- data %>%
    arrange(desc(!!enquo(value_var)))
  
  # Calculate total
  total_val <- sum(processed_data[[val_nm]], na.rm = TRUE)
  formatted_total <- format(total_val, big.mark = ",", scientific = FALSE)
  
  # Chart
  p <- processed_data %>%
    e_charts_(cat_nm) %>%
    e_pie_(
      val_nm, 
      radius = c("45%", "70%"),
      startAngle = 90
    ) %>%
    
    ## Labels with fixed decimal places
    e_labels(
      formatter = htmlwidgets::JS(paste0("
        function(params){
          return(params.name + ': ' + params.percent.toFixed(", decimal_value, ") + '%');
        }
      "))
    ) %>%
    
    ## Tooltip
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(paste0("
        function(params){
          // Format value with thousand separator and fixed decimals
          var val = parseFloat(params.value).toLocaleString(undefined, {
            minimumFractionDigits: ", decimal_value, ", 
            maximumFractionDigits: ", decimal_value, "
          });
          var pct = parseFloat(params.percent).toFixed(", decimal_value, ");
          return('<b>' + params.name + '</b><br/>' + 
                 'Value: ' + val + '<br/>' + 
                 'Percent: ' + pct + '%');
        }
      "))
    ) %>%
    e_graphic_g(
      list(
        type = "group",
        left = "center",
        top = "center",
        children = list(
          list(
            type = "text",
            z = 100,
            left = "center",
            top = "middle",
            style = list(
              fill = "#333",
              text = paste0("Total\n", formatted_total),
              font = "bold 18px sans-serif",
              textAlign = "center"
            )
          )
        )
      )
    ) %>%
    
    ## Subtitle name
    e_title("Composition Chart", paste("Dataset:", data_name)) %>% 
    e_theme("walden") %>%
    e_legend(orient = "vertical", 
             right = "5%",       
             top = "middle",     
             type = "scroll"      
             )
  
  # Apply custom colors if provided
  if (!is.null(colors)) {
    p <- p %>% 
      e_color(colors)
  }
  
  return(p)
}
