# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(patchwork)
})

# ---- Source Theme ----
source("../utils/theme.R")

# ---- Resolve IHME Files ----
ihme_candidates <- c(
  Sys.glob("../data/IHME-GBD*/*.csv"),
  Sys.glob("../data/IHME-GBD*.csv")
)
ihme_candidates <- ihme_candidates[file.exists(ihme_candidates)]

if (length(ihme_candidates) == 0) {
  stop("No IHME GBD CSV file found in ../data.")
}

citation_candidates <- Sys.glob("../data/IHME-GBD*/citation.txt")
citation_candidates <- citation_candidates[file.exists(citation_candidates)]

if (length(citation_candidates) == 0) {
  stop("No citation.txt file found for IHME GBD data.")
}

# ---- Load and Prepare Data ----
gbd_raw <- bind_rows(lapply(ihme_candidates, function(path) {
  read_csv(path, show_col_types = FALSE)
}))

gbd_deaths_rate <- gbd_raw %>%
  filter(
    measure_name == "Deaths",
    location_name == "Global",
    sex_name == "Both",
    age_name == "All ages",
    metric_name == "Rate"
  ) %>%
  transmute(
    Disorder = cause_name,
    Year = as.integer(year),
    Death_Rate = as.numeric(val),
    lower = as.numeric(lower),
    upper = as.numeric(upper)
  ) %>%
  filter(!is.na(Disorder), !is.na(Year), !is.na(Death_Rate)) %>%
  group_by(Disorder, Year) %>%
  summarise(
    Death_Rate = mean(Death_Rate),
    lower = mean(lower, na.rm = TRUE),
    upper = mean(upper, na.rm = TRUE),
    .groups = "drop"
  )

overall_name <- "Drug use disorders"

individual_trends <- gbd_deaths_rate %>%
  filter(Disorder != overall_name)

overall_trend <- gbd_deaths_rate %>%
  filter(Disorder == overall_name) %>%
  arrange(Year)

if (nrow(individual_trends) == 0) {
  stop("No individual drug disorder rows found after filtering.")
}

if (nrow(overall_trend) == 0) {
  stop("No total 'Drug use disorders' rows found after filtering.")
}

# ---- Group Mapping ----
group_lookup <- tibble(
  Disorder = c(
    "Alcohol use disorders",
    "Opioid use disorders",
    "Cocaine use disorders",
    "Amphetamine use disorders",
    "Cannabis use disorders",
    "Other drug use disorders"
  ),
  Group = c(
    "Depressants",
    "Depressants",
    "Stimulants",
    "Stimulants",
    "Others",
    "Others"
  )
)

group_order <- c("Depressants", "Stimulants", "Others")

individual_trends <- individual_trends %>%
  left_join(group_lookup, by = "Disorder") %>%
  mutate(Group = if_else(is.na(Group), "Others", Group))

# ---- Color Mapping ----
custom_drug_colors <- c(
  "Alcohol use disorders" = night_owlish_cat[1],
  "Opioid use disorders" = night_owlish_cat[5],
  "Cannabis use disorders" = night_owlish_cat[4],
  "Amphetamine use disorders" = night_owlish_cat[3],
  "Cocaine use disorders" = night_owlish_cat[2],
  "Other drug use disorders" = night_owlish_cat[6],
  "Drug use disorders" = theme_fg
)

all_disorders <- unique(c(individual_trends$Disorder, overall_name))
fallback_colors <- setNames(
  rep(c(night_owlish_cat, night_owlish_light$gray), length.out = length(all_disorders)),
  all_disorders
)

color_lookup <- fallback_colors
override_keys <- intersect(names(custom_drug_colors), names(color_lookup))
color_lookup[override_keys] <- custom_drug_colors[override_keys]

# ---- Headline Metrics ----
opioid_series <- individual_trends %>%
  filter(Disorder == "Opioid use disorders") %>%
  arrange(Year)

if (nrow(opioid_series) == 0) {
  stop("No opioid rows found in filtered data.")
}

opioid_start_year <- dplyr::first(opioid_series$Year)
opioid_end_year <- dplyr::last(opioid_series$Year)
opioid_start_rate <- dplyr::first(opioid_series$Death_Rate)
opioid_end_rate <- dplyr::last(opioid_series$Death_Rate)
opioid_pct_increase <- ((opioid_end_rate - opioid_start_rate) / opioid_start_rate) * 100
opioid_pct_label <- format(round(opioid_pct_increase), big.mark = ",", trim = TRUE)

total_rate_series <- overall_trend %>%
  filter(!is.na(Year), !is.na(Death_Rate)) %>%
  arrange(Year)

if (nrow(total_rate_series) == 0) {
  stop("No total drug-use death rate rows found.")
}

total_start_year <- dplyr::first(total_rate_series$Year)
total_end_year <- dplyr::last(total_rate_series$Year)
total_year_span <- total_end_year - total_start_year
total_start_rate <- dplyr::first(total_rate_series$Death_Rate)
total_end_rate <- dplyr::last(total_rate_series$Death_Rate)
total_rate_pct_increase <- ((total_end_rate - total_start_rate) / total_start_rate) * 100
total_rate_pct_label <- format(round(total_rate_pct_increase), big.mark = ",", trim = TRUE)

alcohol_color <- unname(color_lookup["Alcohol use disorders"])
opioid_color <- unname(color_lookup["Opioid use disorders"])
overall_color <- unname(color_lookup[overall_name])

# ---- Caption / Sources ----
citation_blocks <- lapply(citation_candidates, function(path) {
  lines <- read_lines(path)
  if (length(lines) >= 6) lines[4:6] else lines
})


#plot_caption <- caption_global(
#  source = "Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2023 (GBD 2023) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2024. Available from https://vizhub.healthdata.org/gbd-results/.",
#  day = "Day 21",
#  topic = "Historical"
#)

plot_caption <- caption_global(
  source = paste(
    "Global Burden of Disease Collaborative Network.",
    "Global Burden of Disease Study 2023 (GBD 2023) Results.<br>",
    "Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2024.",
    "Available from: vizhub.healthdata.org/gbd-results/."
  ),
  day = "Day 21",
  topic = "Historical"
)

# ---- Text ----

title_text <- paste0(
  "Drugs & Death Rates: <span style='color:", alcohol_color, ";'>Alcohol</span> still leads, ",
  "while <span style='color:", opioid_color, ";'>opioid</span> death<br> rates rise sharply"
)
subtitle_text <- paste0(
  "Global death rate (per 100,000), both sexes, all ages, ", opioid_start_year, "-", opioid_end_year, ".<br>",
  "<span style='color:", opioid_color, ";'>Opioid</span> death rate increased ", opioid_pct_label,
  "%, while the total <span style='color:", overall_color, ";'>drug-use death rate</span> rose ",
  total_rate_pct_label, "% over ", total_year_span, " years."
)


# ---- Group Panel Builder ----
build_group_plot <- function(group_name) {
  label_offset <- 0.24

  panel_data <- individual_trends %>%
    filter(Group == group_name)

  if (nrow(panel_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No data for this group", family = "FiraSans", color = theme_muted, size = 4) +
        labs(title = group_name) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#ffffff", color = NA),
          panel.background = element_rect(fill = "#ffffff", color = NA),
          plot.title = element_markdown(family = "SpaceGrotesk", size = 12, color = theme_fg, hjust = 0),
          plot.margin = margin(8, 10, 8, 10)
        )
    )
  }

  recent_cutoff <- max(panel_data$Year, na.rm = TRUE) - 4
  disorder_order <- panel_data %>%
    filter(Year >= recent_cutoff) %>%
    group_by(Disorder) %>%
    summarise(recent_rate = mean(Death_Rate, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(recent_rate)) %>%
    pull(Disorder)

  panel_data <- panel_data %>%
    mutate(Disorder = factor(Disorder, levels = disorder_order)) %>%
    arrange(Disorder, Year)

  start_points <- panel_data %>%
    group_by(Disorder) %>%
    slice_min(Year, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      point_label = paste0(scales::number(Death_Rate, accuracy = 0.01)),
      label_x = Year + label_offset,
      label_hjust = 0
    )

  end_points <- panel_data %>%
    group_by(Disorder) %>%
    slice_max(Year, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      point_label = paste0(scales::number(Death_Rate, accuracy = 0.01)),
      label_x = Year - label_offset,
      label_hjust = 1
    )

  endpoint_points <- bind_rows(start_points, end_points)

  panel_colors <- color_lookup[disorder_order]

  ggplot(panel_data, aes(x = Year, y = Death_Rate, color = Disorder, fill = Disorder)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper, group = Disorder),
      alpha = 0.14,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(linewidth = 1.02, lineend = "round") +
    geom_point(data = endpoint_points, size = 2, show.legend = FALSE) +
    geom_text(
      data = start_points,
      aes(x = label_x, label = point_label, hjust = label_hjust),
      vjust = -0.15,
      size = 3.5,
      family = "FiraSans",
      show.legend = FALSE,
      check_overlap = TRUE
    ) +
    geom_text(
      data = end_points,
      aes(x = label_x, label = point_label, hjust = label_hjust),
      vjust = -0.15,
      size = 3.5,
      family = "FiraSans",
      show.legend = FALSE,
      check_overlap = TRUE
    ) +
    scale_color_manual(values = panel_colors, breaks = disorder_order) +
    scale_fill_manual(values = panel_colors, breaks = disorder_order) +
    scale_x_continuous(
      breaks = seq(min(panel_data$Year), max(panel_data$Year), by = 5),
      expand = expansion(mult = c(0.04, 0.04))
    ) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(
      title = group_name,
      x = NULL,
      y = "Death rate",
      color = NULL
    ) +
    theme_base() +
    theme(
      plot.title = element_markdown(family = "SpaceGrotesk", size = 16, color = theme_fg, margin = margin(b = 6)),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      panel.grid.major.y = element_line(color = scales::alpha(night_owlish_light$gray, 0.25), linewidth = 0.3),
      panel.grid.major.x = element_line(color = scales::alpha(night_owlish_light$gray, 0.12), linewidth = 0.25),
      axis.text.x = element_text(size = 9.5, color = night_owlish_light$fg_soft),
      axis.text.y = element_text(size = 9.5, color = night_owlish_light$fg_soft),
      axis.title.y = element_text(angle =90, size = 11, color = night_owlish_light$fg_soft, margin = margin(r = 8)),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 8.2, color = night_owlish_light$fg_soft),
      legend.key.width = grid::unit(12, "pt"),
      plot.margin = margin(8, 10, 8, 10)
    )
}

depressants_plot <- build_group_plot("Depressants")
stimulants_plot <- build_group_plot("Stimulants")
others_plot <- build_group_plot("Others")

overall_start <- overall_trend %>%
  slice_min(Year, n = 1, with_ties = FALSE) %>%
  mutate(
    point_label = paste0(scales::number(Death_Rate, accuracy = 0.01)),
    label_x = Year + 0.24,
    label_hjust = 0
  )

overall_end <- overall_trend %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  mutate(
    point_label = paste0(scales::number(Death_Rate, accuracy = 0.01)),
    label_x = Year - 0.24,
    label_hjust = 1
  )

overall_endpoints <- bind_rows(overall_start, overall_end)

overall_plot <- ggplot(overall_trend, aes(x = Year, y = Death_Rate)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = scales::alpha(color_lookup[overall_name], 0.24),
    color = NA
  ) +
  geom_line(
    color = color_lookup[overall_name],
    linewidth = 1.15,
    lineend = "round"
  ) +
  geom_point(data = overall_endpoints, color = color_lookup[overall_name], size = 2.4) +
  geom_text(
    data = overall_start,
    aes(x = label_x, label = point_label, hjust = label_hjust),
    vjust = -0.15,
    size = 3.5,
    family = "FiraSans",
    color = color_lookup[overall_name],
    show.legend = FALSE
  ) +
  geom_text(
    data = overall_end,
    aes(x = label_x, label = point_label, hjust = label_hjust),
    vjust = -0.15,
    size = 3.5,
    family = "FiraSans",
    color = color_lookup[overall_name],
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(min(overall_trend$Year), max(overall_trend$Year), by = 5),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(
    title = "Total drug use disorders",
    x = NULL,
    y = "Death rate"
  ) +
  theme_base() +
  theme(
    plot.title = element_markdown(family = "SpaceGrotesk", size = 16, color = theme_fg, margin = margin(b = 6)),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    panel.grid.major.y = element_line(color = scales::alpha(night_owlish_light$gray, 0.25), linewidth = 0.3),
    panel.grid.major.x = element_line(color = scales::alpha(night_owlish_light$gray, 0.12), linewidth = 0.25),
    axis.text.x = element_text(size = 9.5, color = night_owlish_light$fg_soft),
    axis.text.y = element_text(size = 9.5, color = night_owlish_light$fg_soft),
    axis.title.y = element_text( angle =90, size = 11, color = night_owlish_light$fg_soft, margin = margin(r = 8)),
    plot.margin = margin(8, 10, 8, 10)
  )

grid_plot <- wrap_plots(
  list(depressants_plot, stimulants_plot, others_plot, overall_plot),
  ncol = 2
)

title_block <- ggplot() +
  labs(title = title_text, subtitle = subtitle_text) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    plot.title = element_markdown(
      family = theme_title_family,
      face = "bold",
      size = 32,
      color = theme_fg,
      hjust = 0,
      lineheight = 1.05,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      family = "FiraSans",
      size = 18,
      color = theme_muted,
      hjust = 0,
      lineheight = 1.35,
      margin = margin(b = 4, t =6)
    ),
    plot.margin = margin(20, 12, 6, 12)
  )

caption_block <- ggplot() +
  labs(caption = plot_caption) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    plot.caption = element_markdown(
      family = "FiraSansRegular",
      size = 9,
      color = theme_muted,
      hjust = 0,
      lineheight = 1.5,
      margin = margin(t = 8)
    ),
    plot.margin = margin(0, 12, 12, 12)
  )

final_plot <- title_block /
  grid_plot /
  caption_block +
  plot_layout(heights = c(0.5, 10, 0.5))

# ---- Save Plot ----
output_path <- "21_historical.png"
#ggsave(output_path, final_plot, width = 14, height = 12, dpi = 300, bg = theme_bg)
ggsave(output_path, final_plot, width = 14, height = 12, dpi = 300 )
