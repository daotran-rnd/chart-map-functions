#' @title Plot Interactive Mekko (Marimekko) Chart
#' 
#' @description
#' Generates an interactive vertical Mekko chart. The width of each column 
#' represents the total scale of the main category (X-axis), while the height 
#' represents the proportion of sub-groups within that category (Y-axis as percentage).
#' @author daotq, anh2bao
#'
#' @param data A data frame containing the variables.
#' @param cat_var The unquoted column name for the main categories (X-axis groups).
#' @param group_var The unquoted column name for the sub-groups (fill color).
#' @param value_var The unquoted column name for the numeric values.
#' @param title Chart title.
#' @param subtitle Chart subtitle. If NULL, displays the dataset name.
#' @param x_title Title for the X-axis (Categories).
#' @param y_title Title for the Y-axis (Percentage).
#' @param colors A character vector of hex colors. If NULL, uses default palette.
#' @export

PlotMekkoChart <- function(data, cat_var, group_var, value_var,
                           title = "Mekko Chart",
                           subtitle = NULL,
                           x_title = "Category",
                           y_title = "Percentage (%)",
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
  
  # --- Data Wrangling ---
  df_base <- data %>%
    rename(cat = !!cat_nm, group = !!group_nm, value = !!value_nm) %>%
    group_by(cat) %>%
    mutate(cat_total = sum(value, na.rm = TRUE)) %>%
    ungroup()
  
  df_x <- df_base %>%
    distinct(cat, cat_total) %>%
    arrange(desc(cat_total)) %>%
    mutate(
      x_rel = cat_total / sum(cat_total),
      xmax = cumsum(x_rel),
      xmin = xmax - x_rel,
      x_mid = xmin + (x_rel / 2) # Điểm giữa để đặt nhãn tên Trình duyệt
    ) %>%
    select(cat, xmin, xmax, x_mid)
  
  df_final <- df_base %>%
    left_join(df_x, by = "cat") %>%
    group_by(cat) %>%
    arrange(desc(group)) %>%
    mutate(
      prop_in_cat = value / cat_total,
      ymax = cumsum(prop_in_cat),
      ymin = ymax - prop_in_cat,
      tooltip = paste0("<b>", cat, "</b><br>", 
                       group, ": ", round(prop_in_cat * 100, 1), "%<br>",
                       "Giá trị: ", format(value, big.mark = ",")),
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
    scale_x_continuous(breaks = df_x$x_mid, labels = df_x$cat, expand = c(0, 0)) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    labs(
      title = title, 
      subtitle = sub_text,
      x = x_title, 
      y = y_title, 
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, color = "grey30", size = 11, margin = margin(b = 15)),
      axis.title.x = element_text(size = 10, face = "italic", margin = margin(t = 10)),
      axis.title.y = element_text(size = 10, face = "italic", margin = margin(r = 10)),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )
  
  # Apply custom colors if provided
  if (!is.null(colors)) {
    p <- p + scale_fill_manual(values = colors)
  }
  
  # --- Rendering ---
  girafe(
    ggobj = p,
    options = list(
      opts_hover(css = "stroke-width:2.5;stroke:black;cursor:pointer;"),
      opts_tooltip(css = "background:white;padding:5px;border-radius:5px;box-shadow: 2px 2px 5px rgba(0,0,0,0.1);"),
      opts_toolbar(saveaspng = TRUE)
    ),
    width_svg = 8,
    height_svg = 6
  )
}
