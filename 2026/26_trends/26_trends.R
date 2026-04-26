# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
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
  night_owlish_cat[1],  # blue
  night_owlish_cat[5],  # aqua (low growth)
  night_owlish_cat[4],  # orange
  night_owlish_cat[3]   # red (high growth)
)
growth_band_colors <- grDevices::colorRampPalette(viridis_theme_anchors)(length(growth_breaks))
growth_band_colors_alpha <- scales::alpha(growth_band_colors, 0.5)

band_breaks <- seq(0, 100, by = 5)
band_labels <- ifelse(band_breaks %% 10 == 0, paste0(band_breaks, "%"), "")
viridis_theme_anchors <- c(
  night_owlish_cat[5],
  night_owlish_cat[1],
  night_owlish_cat[4],
  night_owlish_cat[3]
)
band_colors <- grDevices::colorRampPalette(viridis_theme_anchors)(length(band_breaks) - 1)

facet_lookup$category <- factor(facet_lookup$category, levels = c("Metals", "Raw materials", "Products"))
facet_lookup$facet_label <- paste0(facet_lookup$commodity)
facet_lookup <- facet_lookup[order(facet_lookup$category, facet_lookup$commodity), ]

trend_df <- trend_df %>%
  left_join(facet_lookup, by = "commodity") %>%
  filter(!is.na(category)) %>%
  mutate(
    category = factor(category, levels = c("Metals", "Raw materials", "Products")),
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

# Add growth for coloring
facet_data <- facet_data %>%
  mutate(rank = rank(-growth, ties.method = "first"))

# Join colors from facet_lookup
trend_df <- trend_df %>%
  left_join(facet_lookup %>% select(commodity), by = "commodity") %>%
  left_join(facet_data %>% select(commodity, growth_band), by = "commodity")

 plot_title <- "China’s Share of Global Production Approaches 50% in Major Sectors"
n_above_50 <- sum(facet_data$last_share > 50, na.rm = TRUE)
n_total <- nrow(facet_data)
top_growth <- facet_data %>% slice_max(order_by = growth, n = 1, with_ties = FALSE)
top_share <- facet_data %>% slice_max(order_by = last_share, n = 1, with_ties = FALSE)

plot_subtitle <- paste0(
  "Since 2000, China moved above <span style='color:", night_owlish_cat[9], ";'><b>50% share of global production in ", n_above_50, " of ", n_total, " sectors</b></span>.",
  "<br><span style='color:", night_owlish_cat[9], ";'><b>", top_growth$commodity, " rose ", sprintf("%.1f", top_growth$growth), "x</b></span>",
  " while <span style='color:", night_owlish_cat[9], ";'><b>", top_share$commodity, " reached ", sprintf("%.1f", top_share$last_share), "%</b></span> in 2025."
)
plot_source <- "China Association of Automobile Manufacturers; China National Bureau of Statistics; FAO; International Aluminium Institute; LEDinside; OICA; Statista; UNIDO; <br>USGS; USGS (United States Geological Survey); World Bank; World Steel Association"
plot_caption <- caption_global(
  source = paste0("Sources: ", plot_source ),
  day = "Day 26",
  topic = "Trends"
)

share_plot <- ggplot(trend_df, aes(x = year, y = china_share, group = commodity)) +
  geom_rect(
    data = facet_data,
    inherit.aes = FALSE,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = growth_band),
    alpha = 0.9
  ) +
  geom_point(data = facet_data, aes(x = first_year, y = first_share, color = growth), alpha = 0.001, show.legend = TRUE) +
  geom_hline(yintercept = 50, linetype = "dashed", linewidth = 0.35, color = night_owlish_light$fg) +
  geom_line(linewidth = 0.8,     color = alpha(night_owlish_light$fg, 0.8)                 ) +
  geom_point(size = 1.6,     color = alpha(night_owlish_light$fg, 0.8)                 ) +
  geom_text(
    data = facet_data,
    aes(x = last_year, y = last_share, label = paste0(round(last_share), "%")),
    inherit.aes = FALSE,
    family = "FiraSans",
    size = 4,
    hjust = +0.2,
    vjust = -0.5,
    color = alpha(night_owlish_light$fg, 0.8)
  ) +
  geom_text(
    data = facet_data,
    aes(x = first_year, y = first_share, label = paste0(round(first_share), "%")),
    inherit.aes = FALSE,
    family = "FiraSans",
    size = 4,
    hjust = 0.18,
    vjust = -1.2,
    color = alpha(night_owlish_light$fg, 0.9)
  ) +
geom_text(
    data = facet_data,
    aes(x = last_year, y = 95, label = paste0(round(growth, 1), "x")),
    inherit.aes = FALSE,
    family = "FiraSans",
    size = 6,
    hjust = 1,
    vjust = 0,
    color = alpha(night_owlish_light$fg, 0.9)
  ) +
  scale_fill_gradientn(colors = growth_band_colors_alpha, breaks = growth_breaks, labels = growth_labels, guide = "none") +
  scale_color_gradientn(
    colors = growth_band_colors,
    breaks = growth_breaks,
    labels = growth_labels,
    guide = guide_colorbar(
      title = "Growth",
      barheight = grid::unit(90, "pt"),
      barwidth = grid::unit(12, "pt")
    )
  ) +
  facet_wrap(~facet_label, ncol = 4, dir = "v", shrink= TRUE) +
  scale_x_continuous(
    breaks = seq(floor(min(trend_df$year) / 5) * 5, ceiling(max(trend_df$year) / 5) * 5, by = 5),
    expand = expansion(mult = c(0.02, 0.08))
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
    y = NULL,
    caption = plot_caption
  ) +
  theme_base() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = alpha(night_owlish_light$fg_soft, 0.08), linewidth = 0),
    panel.spacing.x = unit(0.5, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.text = ggtext::element_markdown(
      family = "SpaceGrotesk",
      face = "bold",
      size = 16,
      margin = margin(10, 10, 10,10)
    ),
    axis.text.x = element_text(size = 8.5, color = night_owlish_light$fg),
    axis.text.y = element_text(size = 8.5, color = night_owlish_light$fg),
    plot.title = element_markdown(
      family = theme_title_family,
      face = "bold",
      size = 32,
      color = night_owlish_light$fg,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = "FiraSans",
      size = 18,
      color = theme_muted,
      margin = margin(b = 18)
    ),
    plot.caption = element_markdown(
      family = theme_caption_family,
      size = 10,
      color = theme_muted,
      margin = margin(t = 14)
    ),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "right",
    legend.key.size = unit(10, "mm"),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

ggsave(
  "26_china_half_everything_facets.png",
  share_plot,
  width = 14,
  height = 12,
  dpi = 340,
  bg = "#fff"
)
