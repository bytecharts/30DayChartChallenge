# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(stringr)
  library(patchwork)
})

# ---- Source Theme ----
source("../utils/theme.R")

# ---- Helpers ----
parse_year <- function(x) {
  x <- str_trim(x)
  year_value <- rep(NA_real_, length(x))

  single_year <- str_detect(x, "^\\d{4}$")
  year_value[single_year] <- as.numeric(x[single_year])

  year_range <- str_detect(x, "^\\d{4}-\\d{4}$")
  if (any(year_range)) {
    bounds <- str_split_fixed(x[year_range], "-", 2)
    year_value[year_range] <- (as.numeric(bounds[, 1]) + as.numeric(bounds[, 2])) / 2
  }

  year_value
}

parse_percentage <- function(x) {
  x <- str_trim(x)
  pct_value <- rep(NA_real_, length(x))

  single_pct <- str_detect(x, "^\\d+(?:\\.\\d+)?$")
  pct_value[single_pct] <- as.numeric(x[single_pct])

  pct_range <- str_detect(x, "^\\d+(?:\\.\\d+)?-\\d+(?:\\.\\d+)?$")
  if (any(pct_range)) {
    bounds <- str_split_fixed(x[pct_range], "-", 2)
    pct_value[pct_range] <- (as.numeric(bounds[, 1]) + as.numeric(bounds[, 2])) / 2
  }

  pct_value
}

# ---- Load Data ----
data_path <- "../data/elephant_tusklessness_with_baselines.csv"

tusk_raw <- read_csv(data_path, show_col_types = FALSE)

tusk_numeric <- tusk_raw %>%
  mutate(
    year_value = parse_year(Year),
    tusklessness_value = parse_percentage(Tusklessness_Percentage)
  ) %>%
  filter(!is.na(tusklessness_value))

baseline_value <- 3

country_latest <- tusk_numeric %>%
  filter(!is.na(year_value), !Country %in% c("Baseline", "Africa", "Kenya")) %>%
  group_by(Country, year_value) %>%
  summarise(
    tusklessness_value = mean(tusklessness_value),
    .groups = "drop"
  ) %>%
  group_by(Country) %>%
  summarise(
    latest_year = max(year_value),
    latest_value = mean(tusklessness_value[year_value == latest_year]),
    .groups = "drop"
  )

dumbbell_data <- country_latest %>%
  mutate(
    baseline = baseline_value,
    baseline_point_label = paste0(scales::number(baseline, accuracy = 0.1), "%"),
    baseline_label_x = pmax(baseline - 1.2, 0.4),
    latest_label = paste0(
      scales::number(latest_value, accuracy = 0.1),
      "% (",
      as.integer(latest_year),
      ")"
    ),
    label_x = latest_value + 4
  ) %>%
  arrange(latest_value) %>%
  mutate(Country = factor(Country, levels = Country))

x_max <- max(dumbbell_data$label_x) + 2
top_country <- tail(levels(dumbbell_data$Country), 1)
baseline_label <- paste0("Natural baseline: ", scales::number(baseline_value, accuracy = 0.1), "%")
plot_title <- "Ivory Poaching and Its Impact: Tusklessness by Country"
plot_subtitle <- "Intense hunting and ivory poaching over the past century have driven the loss of tusks in female elephants.<br>It marks a striking case of rapid evolution."

plot_caption <- caption_global("Sources: Campbell-Staton et al. (2021) Science; Whitehouse (2002) S. Afr. J. Zool.; Jachmann et al. (1995) Afr. J. Ecol.; <br>National Geographic (2021); African Wildlife Foundation (2015)", "Day 19", "Evolution")

annotation_theme <- theme(
  plot.background = element_rect(fill = theme_bg, color = NA),
  plot.title = element_markdown(
    family = theme_title_family,
    face = "bold",
    size = 24,
    color = theme_fg,
    hjust = 0,
    margin = margin(t = 12, b = 6)
  ),
  plot.subtitle = element_markdown(
    size = 14,
    hjust = 0,
    color = theme_muted,
    margin = margin(b = 25),
    lineheight = 1.5
  ),
  plot.caption = element_markdown(
    size = 8,
    color = theme_muted,
    hjust = 0,
    family = theme_caption_family,
    lineheight = 1.5,
    margin = margin(t = 15)
  )
)

# ---- Plot Data ----
dumbbell_countries <- dumbbell_data %>%
  transmute(
    country_label = as.character(Country),
    country_key = tolower(country_label),
    latest_value
  )

world_map <- map_data("world") %>%
  filter(region != "Antarctica") %>%
  mutate(country_key = tolower(region)) %>%
  left_join(dumbbell_countries, by = "country_key") %>%
  mutate(has_data = !is.na(latest_value))

map_labels <- world_map %>%
  filter(has_data) %>%
  group_by(country_label) %>%
  summarise(
    label_long = median(long),
    label_lat = median(lat),
    .groups = "drop"
  )

map_plot <- ggplot() +
  geom_polygon(
    data = world_map %>% filter(!has_data),
    aes(x = long, y = lat, group = group),
    fill = scales::alpha(night_owlish_light$gray, 0.16),
    color = scales::alpha(night_owlish_light$fg_soft, 0.18),
    linewidth = 0.1
  ) +
  geom_polygon(
    data = world_map %>% filter(has_data),
    aes(x = long, y = lat, group = group, fill = latest_value),
    color = scales::alpha(night_owlish_light$fg, 0.4),
    linewidth = 0.18
  ) +
  geom_text(
    data = map_labels,
    aes(x = label_long, y = label_lat, label = country_label),
    inherit.aes = FALSE,
    family = "FiraCode",
    size = 2.6,
    color = night_owlish_light$fg
  ) +
  scale_fill_gradientn(
    name = "Tusklessness",
    colours = c("#5DA5DA", "#F6C667", "#E15759"),
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = scales::label_number(suffix = "%"),
    oob = scales::squish,
    guide = guide_colorbar(
      title.position = "top",
      label.position = "right",
      barwidth = grid::unit(7, "pt"),
      barheight = grid::unit(95, "pt"),
      title.theme = element_text(
        margin = margin(b = 6)
      ),
      label.theme = element_text(
        margin = margin(l = 6)
      )
    )
  ) +
  coord_quickmap(xlim = c(-20, 56), ylim = c(-38, 20), expand = FALSE) +
  labs() +
  theme_base() +
  theme(
    axis.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 8.5, color = night_owlish_light$fg_soft),
    legend.text = element_text(size = 8, color = night_owlish_light$fg_soft),
    legend.key.height = grid::unit(18, "pt"),
    plot.margin = margin(30, 8, 30, 8)
  )

dumbbell_plot <- ggplot(dumbbell_data, aes(y = Country)) +
  geom_vline(
    xintercept = baseline_value,
    linetype = "dotted",
    linewidth = 0.65,
    color = scales::alpha(night_owlish_light$fg_soft, 0.7)
  ) +
  annotate(
    "label",
    x = baseline_value + 1.2,
    y = top_country,
    label = baseline_label,
    hjust = 0,
    vjust = -0.7,
    size = 2.7,
    family = "FiraCode",
    linewidth = 0.15,
    fill = scales::alpha(night_owlish_light$bg, 0.9),
    color = night_owlish_light$fg_soft
  ) +
  geom_segment(
    aes(x = baseline, xend = latest_value, yend = Country),
    linewidth = 1.7,
    lineend = "round",
    color = scales::alpha(night_owlish_light$gray, 0.55)
  ) +
  geom_point(
    aes(x = baseline, color = "Baseline"),
    size = 3.3,
    shape = 16
  ) +
  geom_text(
    aes(x = baseline_label_x, label = baseline_point_label),
    hjust = 1,
    vjust = 0.4,
    size = 2.4,
    color = night_owlish_light$fg_soft,
    family = "FiraCode"
  ) +
  geom_point(
    aes(x = latest_value, color = "Latest"),
    size = 3.8,
    shape = 18
  ) +
  geom_text(
    aes(x = label_x, label = latest_label),
    hjust = 0,
    size = 3,
    color = night_owlish_cat[2],
    fontface = "bold",
    family = "FiraCodeMedium"
  ) +
  scale_color_manual(
    values = c(
      "Baseline" = night_owlish_cat[1],
      "Latest" = night_owlish_cat[2]
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    limits = c(0, x_max),
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    x = "Tuskless females (%)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_base() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = scales::alpha(night_owlish_light$gray, 0.2), linewidth = 0.35),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10, color = night_owlish_light$fg),
    legend.position = c(0.83, 0.18),
    legend.background = element_rect(fill = scales::alpha(night_owlish_light$bg, 0.85), color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(size = 8.5, color = night_owlish_light$fg_soft),
    plot.margin = margin(40, 90, 30, 10)
  )

plot <- (
  map_plot + dumbbell_plot +
    plot_layout(widths = c(1, 1)) +
    plot_annotation(
      title = plot_title,
      subtitle = plot_subtitle,
      caption = plot_caption,
      theme = annotation_theme
    )
) +
  theme(
    plot.background = element_rect(fill = theme_bg, color = NA),
    panel.background = element_rect(fill = theme_bg, color = NA)
  )

# ---- Save Plot ----
output_path <- "19_evolution.png"
ggsave(output_path, plot, width = 13, height = 8.5, type = "cairo", dpi = 300, bg = theme_bg)
