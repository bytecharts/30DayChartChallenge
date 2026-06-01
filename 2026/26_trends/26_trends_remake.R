# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(geomtextpath)
  library(ggpattern)
  library(dplyr)
  library(jsonlite)
  library(scales)
  library(stringr)
  library(purrr)
  library(ggtext)
})

# ---- Theme ----
source("../utils/theme.R")

# ---- Data ----
json_files <- list.files(
  path = ".",
  pattern = "-production-china-vs-world\\.json$",
  full.names = TRUE
)

if (length(json_files) == 0) {
  stop("No comparison JSON files found in 26_trends/")
}

trend_df <- map_dfr(json_files, function(path) {
  raw <- fromJSON(path, flatten = TRUE)$data

  as.data.frame(raw$data) %>%
    transmute(
      year = as.integer(date),
      china_share = as.numeric(china_share),
      source = raw$source,
      commodity = str_remove(raw$title, " Production: China vs World$")
    )
}) %>%
  filter(year >= 2000)

facet_lookup <- data.frame(
  commodity = c(
    "Aluminum",
    "Rare Earth",
    "Paper & Pulp",
    "Cement",
    "Glass",
    "Apple",
    "Automobile",
    "Steel Pipe",
    "Textile",
    "Toys",
    "Washing Machines",
    "LED"
  ),
  category = c(
    "Metals",
    "Metals",
    "Raw materials",
    "Raw materials",
    "Raw materials",
    "Raw materials",
    "Products",
    "Products",
    "Products",
    "Products",
    "Products",
    "Products"
  ),
  stringsAsFactors = FALSE
)

growth_scale <- function(growth_val) {
  scaled <- scales::rescale(growth_val, to = c(1, 4), from = c(4, 5))
  round(scaled)
}

growth_breaks <- seq(0, 12, by = 0.5)
growth_labels <- ifelse(growth_breaks %% 2 == 0, paste0(growth_breaks, "x"), "")
viridis_theme_anchors <- c(
  night_owlish_cat[1],
  night_owlish_cat[5],
  night_owlish_cat[4],
  night_owlish_cat[3]
)
growth_band_colors <- grDevices::colorRampPalette(viridis_theme_anchors)(length(
  growth_breaks
))
growth_band_colors_alpha <- scales::alpha(growth_band_colors, 0.5)

band_breaks <- seq(0, 100, by = 5)
band_labels <- ifelse(band_breaks %% 10 == 0, paste0(band_breaks, "%"), "")
viridis_theme_anchors <- c(
  night_owlish_cat[5],
  night_owlish_cat[1],
  night_owlish_cat[4],
  night_owlish_cat[3]
)
band_colors <- grDevices::colorRampPalette(viridis_theme_anchors)(
  length(band_breaks) - 1
)

slope_palette <- c(
  night_owlish_cat[1],
  night_owlish_cat[5],
  night_owlish_cat[8],
  night_owlish_cat[2],
  night_owlish_cat[4],
  night_owlish_cat[6],
  night_owlish_cat[9],
  night_owlish_cat[7],
  night_owlish_cat[3]
)

facet_lookup$category <- factor(
  facet_lookup$category,
  levels = c("Metals", "Raw materials", "Products")
)
facet_lookup$facet_label <- paste0(facet_lookup$commodity)
facet_lookup <- facet_lookup[
  order(facet_lookup$category, facet_lookup$commodity),
]

trend_df <- trend_df %>%
  left_join(facet_lookup, by = "commodity") %>%
  filter(!is.na(category)) %>%
  mutate(
    category = factor(
      category,
      levels = c("Metals", "Raw materials", "Products")
    ),
    facet_label = factor(facet_label, levels = facet_lookup$facet_label)
  )

facet_data <- trend_df %>%
  group_by(commodity, category, facet_label) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    first_share = china_share[which.min(year)],
    last_year = max(year, na.rm = TRUE),
    last_share = china_share[which.max(year)],
    above_50 = last_share > 50,
    growth = round(last_share / first_share, 1),
    .groups = "drop"
  ) %>%
  left_join(facet_lookup %>% select(commodity), by = "commodity") %>%
  mutate(growth_band = round(growth * 2) / 2)

commodity_colors <- c(
  "Aluminum" = night_owlish_cat[3],
  "Rare Earth" = night_owlish_light$fg,
  "Paper & Pulp" = night_owlish_light$fg,
  "Cement" = night_owlish_light$fg,
  "Glass" = night_owlish_light$fg,
  "Apple" = night_owlish_light$fg,
  "Automobile" = night_owlish_cat[3],
  "Steel Pipe" = night_owlish_light$fg,
  "Textile" = night_owlish_light$fg,
  "Toys" = night_owlish_light$fg,
  "Washing Machines" = night_owlish_light$fg,
  "LED" = night_owlish_light$fg
)

facet_data <- facet_data %>%
  mutate(commodity_color = unname(commodity_colors[commodity]))

slope_df <- bind_rows(
  facet_data %>%
    transmute(
      commodity,
      category,
      facet_label,
      year = first_year,
      china_share = first_share,
      point = "Start"
    ),
  facet_data %>%
    transmute(
      commodity,
      category,
      facet_label,
      year = last_year,
      china_share = last_share,
      point = "End"
    )
) %>%
  arrange(commodity, year) %>%
  mutate(point = factor(point, levels = c("Start", "End"))) %>%
  left_join(
    facet_data %>% select(commodity, growth, commodity_color),
    by = "commodity"
  )

growth_paths <- facet_data %>%
  transmute(
    commodity,
    category,
    facet_label,
    year = first_year,
    china_share = first_share,
    growth,
    commodity_color,
    growth_label = paste0(round(growth, 1), "x")
  ) %>%
  bind_rows(
    facet_data %>%
      transmute(
        commodity,
        category,
        facet_label,
        year = last_year,
        china_share = last_share,
        growth,
        commodity_color,
        growth_label = paste0(round(growth, 1), "x")
      )
  )

base_area_data <- slope_df

image_map <- c(
  "Aluminum" = "images/aluminum.jpeg",
  "Rare Earth" = "images/rare_earth.jpg",
  "Paper & Pulp" = "images/paper.jpeg",
  "Cement" = "images/cement.jpeg",
  "Glass" = "images/glass.jpeg",
  "Apple" = "images/apple.jpeg",
  "Automobile" = "images/automobile.jpg",
  "Steel Pipe" = "images/steel_pipe.jpeg",
  "Textile" = "images/textile2.jpeg",
  "Toys" = "images/toys.jpg",
  "Washing Machines" = "images/washing_machine.jpg",
  "LED" = "images/LED.jpg"
)

missing_images <- image_map[!file.exists(image_map)]
if (length(missing_images) > 0) {
  stop("Missing image(s): ", paste(missing_images, collapse = ", "))
}

pattern_config <- data.frame(
  commodity = c(
    "Aluminum",
    "Rare Earth",
    "Paper & Pulp",
    "Cement",
    "Glass",
    "Apple",
    "Automobile",
    "Steel Pipe",
    "Textile",
    "Toys",
    "Washing Machines",
    "LED"
  ),
  pattern_scale = c(
    4.25,   # Aluminum
    8.8,   # Rare Earth
    1, # Paper & Pulp
    1.4, # Cement
    1.2,   # Glass
    2,   # Apple
    1.35, # Automobile
    6, # Steel Pipe
    3, # Textile
    3.1, # Toys
    3.0, # Washing Machines
    2  # LED
  ),
  pattern_gravity = c(
    "north", # Aluminum
    "north",  # Rare Earth
    "center", # Paper & Pulp
    "center", # Cement
    "south",  # Glass
    "north", # Apple
    "south",  # Automobile
    "center", # Steel Pipe
    "north", # Textile
    "center", # Toys
    "center", # Washing Machines
    "center"   # LED
  ),
  pattern_xoffset = rep(0, 12),
  pattern_yoffset = rep(0, 12),
  pattern_alpha = rep(0.85, 12),
  stringsAsFactors = FALSE
)

pattern_config <- pattern_config %>%
  mutate(
    pattern_filename = unname(image_map[commodity]),
    pattern_type = "none",
    commodity = factor(commodity, levels = facet_lookup$commodity)
  )

pattern_data <- base_area_data %>%
  left_join(pattern_config, by = "commodity") %>%
  mutate(commodity = factor(commodity, levels = facet_lookup$commodity))

# Add growth for coloring
facet_data <- facet_data %>%
  mutate(rank = rank(-growth, ties.method = "first"))

# Join colors from facet_lookup
trend_df <- trend_df %>%
  left_join(facet_lookup %>% select(commodity), by = "commodity") %>%
  left_join(facet_data %>% select(commodity, growth_band), by = "commodity")

plot_title_base <- "Rise of China: Half of Global Production"
plot_title_metals <- paste0(plot_title_base, " #1 ")
plot_title_products <- paste0(plot_title_base, " #2 ")
plot_source <- "China Association of Automobile Manufacturers; China National Bureau of Statistics; FAO; International Aluminium Institute; LEDinside; OICA; Statista; UNIDO; <br>USGS; USGS (United States Geological Survey); World Bank; World Steel Association; Google Images (Creative Commons)"
plot_caption <- caption_general(paste0("Sources: ", plot_source))

build_plot_subtitle <- function(facet_subset) {
  n_above_50 <- sum(facet_subset$last_share > 50, na.rm = TRUE)
  n_total <- nrow(facet_subset)
  top_growth <- facet_subset %>%
    slice_max(order_by = growth, n = 1, with_ties = FALSE)
  top_share <- facet_subset %>%
    slice_max(order_by = last_share, n = 1, with_ties = FALSE)

  paste0(
    "<span style='color:",
    night_owlish_cat[3],
    ";'><b>",
    top_growth$commodity,
    " rose ",
    sprintf("%.1f", top_growth$growth),
    "x</b></span>",
    " while <span style='color:",
    night_owlish_cat[3],
    ";'><b>",
    top_share$commodity,
    " reached ",
    sprintf("%.1f", top_share$last_share),
    "%</b></span> in 2025."
  )
}

build_plot_data <- function(commodities) {
  list(
    slope = slope_df %>% filter(commodity %in% commodities),
    base_area = base_area_data %>% filter(commodity %in% commodities),
    pattern = pattern_data %>% filter(commodity %in% commodities),
    growth_paths = growth_paths %>% filter(commodity %in% commodities),
    facet_data = facet_data %>% filter(commodity %in% commodities)
  )
}

build_share_plot <- function(plot_data, plot_title, plot_subtitle, highlight = NULL) {
  plot_data$slope <- plot_data$slope %>%
    mutate(linewidth_val = if_else(commodity %in% highlight, 2.2, 0.8))
  plot_data$growth_paths <- plot_data$growth_paths %>%
    mutate(textline_size = if_else(commodity %in% highlight, 12, 7.5))

  label_map <- plot_data$facet_data %>%
    distinct(facet_label, commodity_color) %>%
    mutate(
      facet_label = as.character(facet_label),
      label = paste0(
        "<span style='color:",
        commodity_color,
        ";'>",
        facet_label,
        "</span>"
      )
    )
  label_map <- setNames(label_map$label, label_map$facet_label)

  ggplot(
    plot_data$slope,
    aes(x = year, y = china_share, group = commodity)
  ) +
    geom_hline(
      yintercept = 50,
      linetype = "dashed",
      linewidth = 0.5,
      color = night_owlish_cat[3]
    ) +
    geom_area(
      data = plot_data$base_area,
      fill = alpha(night_owlish_light$fg, 0.16),
      color = NA
    ) +
    geom_area_pattern(
      data = plot_data$pattern,
      aes(
        pattern_filename = pattern_filename,
        pattern_type = pattern_type,
        pattern_scale = pattern_scale,
        pattern_gravity = pattern_gravity,
        pattern_xoffset = pattern_xoffset,
        pattern_yoffset = pattern_yoffset,
        pattern_alpha = pattern_alpha
      ),
      pattern = "image",
      fill = "white",
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(
      aes(color = commodity_color, linewidth = linewidth_val),
      lineend = "round"
    ) +
    geom_point(
      aes(color = commodity_color),
      size = 1.6
    ) +
    geom_textline(
      data = plot_data$growth_paths,
      aes(
        x = year,
        y = china_share,
        label = growth_label,
        color = commodity_color,
        size = textline_size
      ),
      family = "FiraSans",
      vjust = -0.15,
      fontface = "bold",
      text_only = TRUE,
      upright = TRUE,
      linewidth = 0,
      show.legend = FALSE
    ) +
    geom_text(
      data = plot_data$facet_data,
      aes(
        x = last_year,
        y = last_share,
        label = paste0(round(last_share), "%"),
        color = commodity_color
      ),
      inherit.aes = FALSE,
      family = "FiraSans",
      size = 4.5,
      hjust = +0.2,
      vjust = -0.5,
      show.legend = FALSE
    ) +
    geom_text(
      data = plot_data$facet_data,
      aes(
        x = first_year,
        y = first_share,
        label = paste0(round(first_share), "%"),
        color = commodity_color
      ),
      inherit.aes = FALSE,
      family = "FiraSans",
      size = 5.5,
      hjust = 0.7,
      vjust = -1.5,
      show.legend = FALSE
    ) +
    scale_color_identity(guide = "none") +
    scale_linewidth_identity(guide = "none") +
    scale_size_identity(guide = "none") +
    scale_pattern_filename_identity() +
    scale_pattern_type_identity() +
    scale_pattern_scale_identity() +
    scale_pattern_gravity_identity() +
    scale_pattern_xoffset_identity() +
    scale_pattern_yoffset_identity() +
    scale_pattern_alpha_identity() +
    facet_wrap(
      ~facet_label,
      ncol = 6,
      dir = "v",
      shrink = TRUE,
      strip.position = "bottom",
      labeller = labeller(facet_label = label_map)
    ) +
    scale_x_continuous(
      breaks = c(min(plot_data$facet_data$first_year), max(plot_data$facet_data$last_year)),
      labels = c(min(plot_data$facet_data$first_year), max(plot_data$facet_data$last_year)),
      expand = expansion(mult = c(0.15, 0.15))
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.03))
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL,
      y = "Global Production",
      caption = plot_caption
    ) +
    theme_base() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = alpha(night_owlish_light$fg_soft, 0.08),
        linewidth = 0
      ),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(0.5, "lines"),
      strip.background = element_blank(),
      strip.text = ggtext::element_markdown(
        family = "SpaceGrotesk",
        face = "bold",
        size = 22,
        margin = margin(10, 10, 10, 10)
      ),
      axis.text.x = element_text(size = 14, color = night_owlish_light$fg),
      axis.text.y = element_text(size = 15, color = night_owlish_light$fg),
      axis.title.y = element_text(size = 22,angle= 90, color = night_owlish_light$fg),
      plot.title = element_markdown(
        family = theme_title_family,
        face = "bold",
        size = 40,
        color = night_owlish_light$fg,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_markdown(
        family = "FiraSans",
        size = 26,
        color = theme_muted,
        margin = margin(b = 18)
      ),
      plot.caption = element_markdown(
        family = theme_caption_family,
        size = 10,
        color = theme_muted,
        margin = margin(t = 14)
      ),
      plot.margin = margin(20, 20, 20, 20)
    )
}

finished_products <- facet_lookup %>%
  filter(category == "Products") %>%
  pull(commodity) %>%
  unique()

finished_products <- unique(c(finished_products, "Paper & Pulp"))
finished_products <- setdiff(finished_products, c("Washing Machines", "Toys"))

metals_raw <- facet_lookup %>%
  filter(category %in% c("Metals", "Raw materials"), commodity != "Paper & Pulp") %>%
  pull(commodity)

metals_raw <- setdiff(metals_raw, c("Washing Machines", "Toys"))

plot_data_metals <- build_plot_data(metals_raw)
plot_data_products <- build_plot_data(finished_products)

plot_subtitle_metals <- build_plot_subtitle(plot_data_metals$facet_data)
plot_subtitle_products <- build_plot_subtitle(plot_data_products$facet_data)

share_plot_metals <- build_share_plot(
  plot_data_metals,
  plot_title_metals,
  plot_subtitle_metals,
  highlight = "Aluminum"
)

share_plot_products <- build_share_plot(
  plot_data_products,
  plot_title_products,
  plot_subtitle_products,
  highlight = "Automobile"
)

ggsave(
  "26_china_half_everything_metals_raw.png",
  share_plot_metals,
  width = 14,
  height = 16,
  dpi = 340,
  bg = "#fff"
)

ggsave(
  "26_china_half_everything_finished_products.png",
  share_plot_products,
  width = 14,
  height = 16,
  dpi = 340,
  bg = "#fff"
)
