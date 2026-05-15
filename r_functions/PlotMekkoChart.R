#' @title Plot Interactive Mekko (Marimekko) Chart
#' 
#' @description
#' Generates an interactive Mekko chart. The height of each bar represents 
#' the total scale of the main category, while the width represents 
#' the proportion (market share) of sub-groups within that category.
#' @author daotq, anh2bao
#'
#' @param data A data frame containing the variables.
#' @param cat_var The unquoted column name for the main categories (Y-axis groups).
#' @param group_var The unquoted column name for the sub-groups (fill color).
#' @param value_var The unquoted column name for the numeric values.
#' @param title Chart title.
#' @param subtitle Chart subtitle. If NULL, displays the dataset name.
#' @param x_title Title for the X-axis (percentage).
#' @param y_title Title for the Y-axis (categories).
#' @param colors A character vector of hex colors. If NULL, uses default palette.
#' @export

PlotMekkoChart <- function(data, cat_var, group_var, value_var,
                           title = "Mekko Chart",
                           subtitle = NULL,
                           x_title = "Percentage (%)",
                           y_title = "Categorty",
                           colors = NULL) {
  
  # Setup libraries
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, ggiraph, rlang, scales)
  
  # Capture variables
  cat_nm    <- enquo(cat_var)
  group_nm  <- enquo(group_var)
  value_nm  <- enquo(value_var)
  data_name <- deparse(substitute(data))
  sub_text  <- if(is.null(subtitle)) paste("Dataset:", data_name) else subtitle
  
  # --- Data Wrangling --- (Logic Mekko)
  df_base <- data %>%
    rename(cat = !!cat_nm, group = !!group_nm, value = !!value_nm) %>%
    group_by(cat) %>%
    mutate(cat_total = sum(value, na.rm = TRUE)) %>%
    ungroup()
  
  df_y <- df_base %>%
    distinct(cat, cat_total) %>%
    arrange(desc(cat_total)) %>%
    mutate(
      y_rel = cat_total / sum(cat_total),
      ymax = cumsum(y_rel),
      ymin = ymax - y_rel,
      y_mid = ymin + (y_rel / 2)
    ) %>%
    select(cat, ymin, ymax, y_mid)
  
  df_final <- df_base %>%
    left_join(df_y, by = "cat") %>%
    group_by(cat) %>%
    arrange(desc(group)) %>%
    mutate(
      prop_in_cat = value / cat_total,
      xmax = cumsum(prop_in_cat),
      xmin = xmax - prop_in_cat,
      tooltip = paste0("<b>", cat, "</b><br>", 
                       group, ": ", round(prop_in_cat * 100, 1), "%<br>",
                       "Value: ", format(value, big.mark = ",")),
      data_id = paste0(as.character(cat), "_", as.character(group)) 
    ) %>%
    ungroup()
  
  # --- Plotting ---
  p <- ggplot(df_final) +
    geom_rect_interactive(aes(
      xmin = xmin, xmax = xmax, 
      ymin = ymin, ymax = ymax, 
      fill = group,
      tooltip = tooltip, 
      data_id = data_id  
    ), color = "white", size = 0.2) +
    scale_y_continuous(breaks = df_y$y_mid, labels = df_y$cat, expand = c(0, 0)) +
    scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
    labs(title = title, subtitle = sub_text, x = x_title, y = y_title, fill = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, color = "grey30", size = 11, margin = margin(b = 15)),
      axis.title.x = element_text(size = 10, face = "italic"),
      axis.title.y = element_text(size = 10, face = "italic"),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )
  # Apply custom colors if provided
  if (!is.null(colors)) { p <- p + scale_fill_manual(values = colors) }
  
  # --- Rendering ---
  girafe(
    ggobj = p,
    options = list(
      opts_hover(css = "stroke-width:2.5;stroke:black;cursor:pointer;"),
      opts_tooltip(css = "background:white;padding:5px;border-radius:5px;"),
      opts_toolbar(saveaspng = TRUE)
    ),
    width_svg = 8,
    height_svg = 6
  )
}