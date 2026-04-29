#' @title Save charts with timestamp
#'
#' @description This function saves chart objects as .png or .html. 
#'      If no path is provided, it defaults to a "visuals" folder in the working directory. 
#'      If user inputs a path, it saves directly to that location.
#' @author daotq
#'
#' @param plot_obj A plot object (ggplot, plotly, leaflet, echarts4r, etc.).
#' @param name Character string for the output filename.
#' @param time_stamp Logical. If TRUE (default), adds "_YYYY.MM.DD" to the name.
#' @param width Numeric. Width of the output image (inches). Default is 7.
#' @param height Numeric. Height of the output image (inches). Default is 5.
#' @param path Character string for the directory path.
#'
#' @export

SaveChart <- function(plot_obj, 
                      name, 
                      time_stamp = TRUE, 
                      width = 7, height = 5, 
                      path = NULL) {
  
  # Ensure dependencies
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(htmlwidgets, ggplot2, dplyr, rlang)
  
  # Path handling 
  if (is.null(path)) {
    ## Default: save to "visuals" in the current working directory
    path <- "visuals"
  } 
    ## If path is provided by user, use the path directly as given.
  
    ## Create the folder if it doesn't exist
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  
  # Date stamp (YYYY.MM.DD)
  suffix <- ""
  if (time_stamp) {
    suffix <- paste0("_", format(Sys.time(), "%Y.%m.%d"))
  }
  
  # Handle interactive plots (htmlwidgets)
  if (inherits(plot_obj, "htmlwidget")) {
    file_name <- file.path(path, paste0(name, suffix, ".html"))
    
    tryCatch({
      htmlwidgets::saveWidget(plot_obj, file_name, selfcontained = TRUE)
      message(paste0(">>> [HTML] Saved to: ", file_name))
    }, error = function(e) stop("Error saving HTML: ", e$message))
    
  } else if (inherits(plot_obj, "ggplot") || inherits(plot_obj, "grob")) {
    
  # Handle static plots (ggplot2/grid)
    file_name <- file.path(path, paste0(name, suffix, ".png"))
    
    ggplot2::ggsave(
      filename = file_name, 
      plot = plot_obj, 
      width = width, 
      height = height, 
      dpi = 300
    )
    message(paste0(">>> [PNG] Saved to: ", file_name))
    
  } else {
    warning("!!! Unsupported object type. Expected ggplot or htmlwidget.")
    return(NULL)
  }
  
  return(invisible(file_name))
}
