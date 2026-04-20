# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(countrycode)
  library(gifski)
  library(patchwork)
})

# ---- Source Theme ----
source("../utils/theme.R")

# ---- Load Data ----
data_path <- "../data/EG.ELC.ACCS.json"
wb_raw <- fromJSON(data_path, simplifyDataFrame = TRUE)

electricity_long <- wb_raw[[2]] %>%
  jsonlite::flatten() %>%
  transmute(
    Country = country.value,
    ISO3 = as.character(countryiso3code),
    Year = as.integer(date),
    Electricity_Access_Pct = as.numeric(value)
  ) %>%
  filter(
    !is.na(Year),
    !is.na(Electricity_Access_Pct),
    !is.na(ISO3),
    ISO3 != "",
    nchar(ISO3) == 3
  )

world_map <- map_data("world") %>%
  filter(region != "Antarctica") %>%
  mutate(
    iso3c = countrycode(
      region,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Bolivia" = "BOL",
        "Brunei" = "BRN",
        "Cape Verde" = "CPV",
        "Czech Republic" = "CZE",
        "Democratic Republic of the Congo" = "COD",
        "Ivory Coast" = "CIV",
        "Kosovo" = "XKX",
        "Laos" = "LAO",
        "Micronesia" = "FSM",
        "Moldova" = "MDA",
        "North Korea" = "PRK",
        "Palestine" = "PSE",
        "Russia" = "RUS",
        "South Korea" = "KOR",
        "Swaziland" = "SWZ",
        "Syria" = "SYR",
        "Tanzania" = "TZA",
        "UK" = "GBR",
        "USA" = "USA",
        "Vatican" = "VAT",
        "Venezuela" = "VEN",
        "Vietnam" = "VNM"
      ),
      warn = FALSE
    )
  )

country_label_points <- world_map %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>%
  summarise(
    long = median(long, na.rm = TRUE),
    lat = median(lat, na.rm = TRUE),
    .groups = "drop"
  )

valid_iso3 <- unique(country_label_points$iso3c)
electricity_long <- electricity_long %>%
  filter(ISO3 %in% valid_iso3)

all_years <- electricity_long %>%
  distinct(Year) %>%
  arrange(Year) %>%
  pull(Year)

latest_year <- max(all_years)
start_year <- latest_year - 49
years <- all_years[all_years >= start_year]
min_year <- min(years)
max_year <- max(years)
tick_years <- seq(min_year, max_year, by = 5)
if (tail(tick_years, 1) != max_year) {
  tick_years <- c(tick_years, max_year)
}

frames_dir <- "electricity_access_frames"
if (!dir.exists(frames_dir)) {
  dir.create(frames_dir, recursive = TRUE)
}

frame_paths <- file.path(frames_dir, paste0("electricity_access_", years, ".png"))
plot_caption <- caption_global_dark("Source: World Bank WDI, indicator EG.ELC.ACCS.ZS (Access to electricity, % of population).", "Day 20", "Global Change")
large_drop_threshold_pp <- 3

# ---- Build Yearly Annotation Data ----
yearly_change <- electricity_long %>%
  filter(Year %in% years) %>%
  arrange(ISO3, Year) %>%
  group_by(ISO3, Country) %>%
  mutate(
    prev_year = lag(Year),
    prev_access = lag(Electricity_Access_Pct),
    year_gap = Year - prev_year,
    delta_pp = Electricity_Access_Pct - prev_access,
    crossed_50 = !is.na(prev_access) & prev_access < 50 & Electricity_Access_Pct >= 50,
    crossed_60 = !is.na(prev_access) & prev_access < 60 & Electricity_Access_Pct >= 60,
    crossed_70 = !is.na(prev_access) & prev_access < 70 & Electricity_Access_Pct >= 70,
    crossed_75 = !is.na(prev_access) & prev_access < 75 & Electricity_Access_Pct >= 75,
    crossed_80 = !is.na(prev_access) & prev_access < 80 & Electricity_Access_Pct >= 80,
    crossed_90 = !is.na(prev_access) & prev_access < 90 & Electricity_Access_Pct >= 90,
    crossed_99 = !is.na(prev_access) & prev_access < 99 & Electricity_Access_Pct >= 99,
    fell_below_50 = !is.na(prev_access) & prev_access >= 50 & Electricity_Access_Pct < 50,
    fell_below_70 = !is.na(prev_access) & prev_access >= 70 & Electricity_Access_Pct < 70,
    fell_below_80 = !is.na(prev_access) & prev_access >= 80 & Electricity_Access_Pct < 80,
    fell_below_99 = !is.na(prev_access) & prev_access >= 99 & Electricity_Access_Pct < 99
  ) %>%
  ungroup() %>%
  filter(year_gap == 1, !is.na(delta_pp)) %>%
  group_by(Year) %>%
  mutate(
    rank_delta = min_rank(desc(delta_pp)),
    countries_compared = n()
  ) %>%
  ungroup()

primary_candidates <- yearly_change %>%
  mutate(
    is_large_drop = delta_pp <= -large_drop_threshold_pp
  ) %>%
  group_by(Year) %>%
  arrange(
    desc(is_large_drop),
    if_else(is_large_drop, delta_pp, Inf),
    desc(crossed_75),
    desc(delta_pp),
    rank_delta,
    Country,
    .by_group = TRUE
  ) %>%
  mutate(candidate_rank = row_number()) %>%
  ungroup()

selected_primary_rows <- list()
used_primary_iso3 <- character(0)

for (yr in sort(unique(primary_candidates$Year), decreasing = TRUE)) {
  candidate_pool <- primary_candidates %>%
    filter(Year == yr, !(ISO3 %in% used_primary_iso3))

  if (nrow(candidate_pool) == 0) {
    next
  }

  chosen <- candidate_pool %>% slice(1)
  selected_primary_rows[[length(selected_primary_rows) + 1]] <- chosen
  used_primary_iso3 <- c(used_primary_iso3, chosen$ISO3)
}

selected_country_by_year <- bind_rows(selected_primary_rows) %>%
  arrange(Year) %>%
  mutate(
    event_type = case_when(
      is_large_drop ~ "largest_yearly_decline",
      crossed_75 ~ "moved_above_75",
      TRUE ~ "largest_yearly_improvement"
    ),
    threshold_note = case_when(
      event_type == "largest_yearly_decline" & fell_below_50 & fell_below_70 & fell_below_80 & fell_below_99 ~ "fell below 50%, 70%, 80%, and 99%",
      event_type == "largest_yearly_decline" & fell_below_50 & fell_below_70 & fell_below_99 ~ "fell below 50%, 70%, and 99%",
      event_type == "largest_yearly_decline" & fell_below_50 & fell_below_80 & fell_below_99 ~ "fell below 50%, 80%, and 99%",
      event_type == "largest_yearly_decline" & fell_below_70 & fell_below_80 & fell_below_99 ~ "fell below 70%, 80%, and 99%",
      event_type == "largest_yearly_decline" & fell_below_50 & fell_below_70 & fell_below_80 ~ "fell below 50%, 70%, and 80%",
      event_type == "largest_yearly_decline" & fell_below_50 & fell_below_70 ~ "fell below 50% and 70%",
      event_type == "largest_yearly_decline" & fell_below_70 & fell_below_80 ~ "fell below 70% and 80%",
      event_type == "largest_yearly_decline" & fell_below_50 & fell_below_80 ~ "fell below 50% and 80%",
      event_type == "largest_yearly_decline" & fell_below_50 & fell_below_99 ~ "fell below 50% and 99%",
      event_type == "largest_yearly_decline" & fell_below_70 & fell_below_99 ~ "fell below 70% and 99%",
      event_type == "largest_yearly_decline" & fell_below_80 & fell_below_99 ~ "fell below 80% and 99%",
      event_type == "largest_yearly_decline" & fell_below_50 ~ "fell below 50%",
      event_type == "largest_yearly_decline" & fell_below_70 ~ "fell below 70%",
      event_type == "largest_yearly_decline" & fell_below_80 ~ "fell below 80%",
      event_type == "largest_yearly_decline" & fell_below_99 ~ "fell below 99%",
      crossed_50 & crossed_70 & crossed_80 & crossed_99 ~ "crossed 50%, 70%, 80%, and 99%",
      crossed_50 & crossed_70 & crossed_99 ~ "crossed 50%, 70%, and 99%",
      crossed_50 & crossed_80 & crossed_99 ~ "crossed 50%, 80%, and 99%",
      crossed_70 & crossed_80 & crossed_99 ~ "crossed 70%, 80%, and 99%",
      crossed_50 & crossed_70 & crossed_80 ~ "crossed 50%, 70%, and 80%",
      crossed_50 & crossed_70 ~ "crossed 50% and 70%",
      crossed_70 & crossed_80 ~ "crossed 70% and 80%",
      crossed_50 & crossed_80 ~ "crossed 50% and 80%",
      crossed_50 & crossed_99 ~ "crossed 50% and 99%",
      crossed_70 & crossed_99 ~ "crossed 70% and 99%",
      crossed_80 & crossed_99 ~ "crossed 80% and 99%",
      crossed_50 ~ "crossed 50%",
      crossed_70 ~ "crossed 70%",
      crossed_80 ~ "crossed 80%",
      crossed_99 ~ "crossed 99%",
      TRUE ~ ""
    )
  ) %>%
  rowwise() %>%
  mutate(
    annotation = if (event_type == "largest_yearly_decline") {
      paste0(
        Country,
        " recorded a sharp yearly decline (",
        scales::number(delta_pp, accuracy = 0.1),
        " pp).",
        ifelse(threshold_note == "", "", paste0(" It ", threshold_note, "."))
      )
    } else if (event_type == "moved_above_75") {
      paste0(
        Country,
        " moved above 75% access (+",
        scales::number(delta_pp, accuracy = 0.1),
        " pp).",
        ifelse(threshold_note == "", "", paste0(" Also ", threshold_note, "."))
      )
    } else {
      paste0(
        Country,
        " posted the largest yearly gain (+",
        scales::number(delta_pp, accuracy = 0.1),
        " pp).",
        ifelse(threshold_note == "", "", paste0(" It ", threshold_note, "."))
      )
    }
  ) %>%
  ungroup()

positive_candidates <- yearly_change %>%
  filter(delta_pp > 0, crossed_60 | crossed_70 | crossed_90) %>%
  mutate(
    positive_threshold_note = case_when(
      crossed_60 & crossed_70 & crossed_90 ~ "crossed 60%, 70%, and 90%",
      crossed_70 & crossed_90 ~ "crossed 70% and 90%",
      crossed_60 & crossed_90 ~ "crossed 60% and 90%",
      crossed_60 & crossed_70 ~ "crossed 60% and 70%",
      crossed_90 ~ "crossed 90%",
      crossed_70 ~ "crossed 70%",
      TRUE ~ "crossed 60%"
    ),
    positive_level = case_when(
      crossed_90 ~ 90,
      crossed_70 ~ 70,
      TRUE ~ 60
    )
  ) %>%
  group_by(Year) %>%
  arrange(desc(positive_level), desc(delta_pp), Country, .by_group = TRUE) %>%
  mutate(candidate_rank = row_number()) %>%
  ungroup()

selected_positive_rows <- list()
used_positive_iso3 <- character(0)

for (yr in sort(unique(positive_candidates$Year), decreasing = TRUE)) {
  candidate_pool <- positive_candidates %>%
    filter(Year == yr, !(ISO3 %in% used_positive_iso3))

  if (nrow(candidate_pool) == 0) {
    next
  }

  chosen <- candidate_pool %>% slice(1)
  selected_positive_rows[[length(selected_positive_rows) + 1]] <- chosen
  used_positive_iso3 <- c(used_positive_iso3, chosen$ISO3)
}

positive_milestone_by_year <- bind_rows(selected_positive_rows) %>%
  transmute(
    Year,
    positive_country = Country,
    positive_iso3 = ISO3,
    positive_delta_pp = delta_pp,
    positive_threshold_note
  )

annotation_data <- tibble(Year = years) %>%
  left_join(selected_country_by_year, by = "Year") %>%
  left_join(positive_milestone_by_year, by = "Year") %>%
  mutate(
    annotation = if_else(is.na(annotation), "", annotation),
    delta_label = if_else(
      is.na(delta_pp),
      "",
      paste0(if_else(delta_pp >= 0, "+", ""), scales::number(delta_pp, accuracy = 0.1), " pp")
    ),
    highlight_color = if_else(delta_pp < 0, "#F07178", "#7FDBCA"),
    country_span = if_else(
      is.na(Country),
      "",
      paste0("<span style='color:", highlight_color, ";font-family:FiraCodeMedium;'>", Country, "</span>")
    ),
    delta_span = if_else(
      delta_label == "",
      "",
      paste0("<span style='color:", highlight_color, ";font-family:FiraCodeMedium;'>", delta_label, "</span>")
    ),
    map_annotation = case_when(
      is.na(Country) ~ "",
      event_type == "largest_yearly_decline" ~ paste0(
        Country,
        " sharp decline (",
        scales::number(delta_pp, accuracy = 0.1),
        " pp",
        ")"
      ),
      event_type == "moved_above_75" ~ paste0(
        Country,
        " moved above 75% (+",
        scales::number(delta_pp, accuracy = 0.1),
        " pp",
        ")"
      ),
      TRUE ~ paste0(
        Country,
        " posted top gain (+",
        scales::number(delta_pp, accuracy = 0.1),
        " pp",
        ")"
      )
    ),
    map_annotation_rich = case_when(
      is.na(Country) ~ "",
      event_type == "largest_yearly_decline" ~ paste0(
        country_span,
        " sharp decline (",
        delta_span,
        ")"
      ),
      event_type == "moved_above_75" ~ paste0(
        country_span,
        " moved above 75% (",
        delta_span,
        ")"
      ),
      TRUE ~ paste0(
        country_span,
        " posted top gain (",
        delta_span,
        ")"
      )
    ),
    map_annotation = if_else(
      threshold_note == "" | is.na(threshold_note),
      map_annotation,
      paste0(map_annotation, "; ", threshold_note)
    ),
    map_annotation_rich = if_else(
      threshold_note == "" | is.na(threshold_note),
      map_annotation_rich,
      paste0(map_annotation_rich, "; ", threshold_note)
    ),
    primary_line_color = if_else(delta_pp < 0, "#F07178", "#7FDBCA"),
    positive_delta_label = if_else(
      is.na(positive_delta_pp),
      "",
      paste0("+", scales::number(positive_delta_pp, accuracy = 0.1), " pp")
    ),
    positive_country_span = if_else(
      is.na(positive_country),
      "",
      paste0("<span style='color:#7FDBCA;font-family:FiraCodeMedium;'>", positive_country, "</span>")
    ),
    positive_delta_span = if_else(
      positive_delta_label == "",
      "",
      paste0("<span style='color:#7FDBCA;font-family:FiraCodeMedium;'>", positive_delta_label, "</span>")
    ),
    positive_map_annotation = if_else(
      is.na(positive_country),
      "",
      paste0(
        positive_country,
        " improved and ",
        positive_threshold_note,
        " (",
        positive_delta_label,
        ")"
      )
    ),
    positive_map_annotation_rich = if_else(
      is.na(positive_country),
      "",
      paste0(
        positive_country_span,
        " improved and ",
        positive_threshold_note,
        " (",
        positive_delta_span,
        ")"
      )
    ),
    positive_line_color = if_else(is.na(positive_delta_pp), "", "#7FDBCA")
  )

annotations_path <- "../data/electricity_access_yearly_annotations.csv"
write_csv(
  annotation_data %>%
    transmute(
      Year,
      Country,
      ISO3,
      positive_country,
      positive_iso3,
      event_type,
      is_large_drop,
      prev_access,
      current_access = Electricity_Access_Pct,
      delta_pp,
      rank_delta,
      countries_compared,
      crossed_50,
      crossed_70,
      crossed_75,
      crossed_80,
      crossed_99,
      fell_below_50,
      fell_below_70,
      fell_below_80,
      fell_below_99,
      positive_threshold_note,
      positive_delta_pp,
      positive_map_annotation_rich,
      positive_map_annotation,
      primary_line_color,
      positive_line_color,
      map_annotation_rich,
      map_annotation,
      annotation
    ),
  annotations_path,
  na = ""
)

dark_bg <- night_owlish_light$fg
dark_panel <- dark_bg
dark_fg <- night_owlish_light$bg
dark_muted <- night_owlish_light$bg_alt
gray_band_colors <- c(
  "#2B2B2B", "#3A3A3A", "#494949", "#585858", "#686868",
  "#797979", "#8A8A8A", "#9C9C9C", "#AEAEAE", "#C1C1C1",
  "#D5D5D5", "#E9E9E9", "#F2E8D5"
)
no_data_fill <- gray_band_colors[1]
no_data_legend_df <- tibble(long = 0, lat = 0, data_status = "No data")
legend_breaks <- seq(0, 100, by = 10)
legend_labels <- ifelse(legend_breaks %% 20 == 0, paste0(legend_breaks, "%"), "")

# ---- Build Frame Per Year ----
for (i in seq_along(years)) {
  year_i <- years[i]
  year_note <- annotation_data %>% filter(Year == year_i)
  year_map_note <- year_note$map_annotation_rich[1]
  year_positive_note <- year_note$positive_map_annotation_rich[1]
  year_primary_line_color <- year_note$primary_line_color[1]
  year_positive_line_color <- year_note$positive_line_color[1]

  year_data <- electricity_long %>%
    filter(Year == year_i) %>%
    transmute(iso3c = ISO3, access_pct = Electricity_Access_Pct)

  map_year <- world_map %>%
    left_join(year_data, by = "iso3c")

  map_with_data <- map_year %>% filter(!is.na(access_pct))
  map_without_data <- map_year %>% filter(is.na(access_pct))

  map_plot <- ggplot() +
    geom_polygon(
      data = map_without_data,
      aes(x = long, y = lat, group = group),
      fill = dark_bg,
      color = scales::alpha(dark_muted, 0.2),
      linewidth = 0.1
    ) +
    geom_polygon(
      data = map_with_data,
      aes(x = long, y = lat, group = group, fill = access_pct),
      color = scales::alpha(dark_bg, 0.45),
      linewidth = 0.1
    ) +
    geom_point(
      data = no_data_legend_df,
      aes(x = long, y = lat, color = data_status),
      inherit.aes = FALSE,
      alpha = 0,
      size = 3,
      show.legend = TRUE
    ) +
    scale_fill_stepsn(
      name = "Electricity access",
      colours = gray_band_colors,
      limits = c(0, 100),
      breaks = legend_breaks,
      labels = legend_labels,
      oob = scales::squish,
      show.limits = TRUE,
      guide = guide_colorbar(
        direction = "vertical",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "right",
        barwidth = grid::unit(8, "pt"),
        barheight = grid::unit(118, "pt"),
        ticks.colour = scales::alpha(dark_muted, 0.85),
        frame.colour = scales::alpha(dark_muted, 0.35),
        title.theme = element_text(
          size = 9,
          color = dark_fg,
          margin = margin(b = 6)
        ),
        label.theme = element_text(
          size = 8.5,
          color = dark_muted,
          lineheight = 1.12,
          margin = margin(l = 5, t = 1.5, b = 1.5)
        ),
        order = 1
      )
    ) +
    scale_color_manual(
      values = c("No data" = scales::alpha(dark_bg, 0)),
      breaks = "No data",
      guide = guide_legend(
        title = NULL,
        order = 2,
        override.aes = list(
          alpha = 1,
          shape = 22,
          size = 4.4,
          fill = dark_bg,
          color = scales::alpha(dark_muted, 0.8),
          stroke = 0.25
        )
      )
    ) +
    coord_quickmap() +
    labs(
      title = "Global Access to Household Electricity",
      subtitle = paste0(
        "Electricity access has expanded over time, but conflicts and instability can quickly cut household electricity access.<br>",
        "Share of population with access in ",
        year_i
      )
    ) +
    annotate(
      "text",
      x = 20,
      y = -50,
      label = as.character(year_i),
      family = "SpaceGrotesk",
      fontface = "bold",
      size = 14,
      color = scales::alpha(dark_fg, 0.17)
    ) +
    theme_base() +
    theme(
      plot.background = element_rect(fill = dark_bg, color = NA),
      panel.background = element_rect(fill = dark_panel, color = NA),
      plot.title = element_markdown(color = dark_fg),
      plot.subtitle = element_markdown(color = dark_muted),
      plot.caption = element_markdown(color = dark_muted),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.direction = "vertical",
      legend.key = element_rect(fill = dark_panel, color = NA),
      legend.background = element_rect(fill = dark_bg, color = NA),
      legend.title = element_text(size = 9, color = dark_fg, margin = margin(b = 7)),
      legend.text = element_text(size = 8.5, color = dark_muted, lineheight = 1.12, margin = margin(l = 2, t = 1.5, b = 1.5)),
      legend.key.height = grid::unit(12, "pt"),
      legend.spacing.y = grid::unit(4, "pt"),
      legend.margin = margin(t = 8, r = 2, b = 4, l = 2),
      legend.box.spacing = grid::unit(10, "pt"),
      plot.margin = margin(22, 8, 2, 8)
    )

  if (nzchar(year_map_note)) {
    map_callout <- year_note %>%
      transmute(
        ISO3,
        map_annotation_rich,
        line_color = primary_line_color
      ) %>%
      filter(!is.na(ISO3), nzchar(map_annotation_rich)) %>%
      left_join(country_label_points, by = c("ISO3" = "iso3c")) %>%
      filter(!is.na(long), !is.na(lat)) %>%
      mutate(
        label_x_raw = if_else(long <= 20, long + 46, long - 46),
        label_x = pmax(pmin(label_x_raw, 170), -170),
        label_y = pmax(pmin(lat + 18, 82), -55),
        label_hjust = if_else(label_x_raw >= long, 0, 1)
      )

    if (nrow(map_callout) > 0) {
      map_plot <- map_plot +
        geom_curve(
          data = map_callout,
          aes(x = long, y = lat, xend = label_x, yend = label_y),
          inherit.aes = FALSE,
          curvature = 0.2,
          linewidth = 0.62,
          color = year_primary_line_color,
          alpha = 0.98,
          lineend = "round"
        ) +
        geom_point(
          data = map_callout,
          aes(x = long, y = lat),
          inherit.aes = FALSE,
          size = 1.4,
          color = year_primary_line_color
        ) +
        ggtext::geom_richtext(
          data = map_callout,
          aes(x = label_x, y = label_y, label = map_annotation_rich, hjust = label_hjust),
          inherit.aes = FALSE,
          family = "FiraCode",
          size = 2.6,
          lineheight = 1.12,
          color = dark_muted,
          fill = scales::alpha("#1F242F", 0.94),
          label.color = NA,
          label.padding = grid::unit(0.36, "lines")
        )
    }
  }

  if (!is.na(year_positive_note) && nzchar(year_positive_note)) {
    positive_callout <- year_note %>%
      transmute(
        ISO3 = positive_iso3,
        positive_map_annotation_rich,
        line_color = positive_line_color
      ) %>%
      filter(!is.na(ISO3), nzchar(positive_map_annotation_rich)) %>%
      left_join(country_label_points, by = c("ISO3" = "iso3c")) %>%
      filter(!is.na(long), !is.na(lat)) %>%
      mutate(
        label_x_raw = if_else(long <= 5, long + 38, long - 38),
        label_x = pmax(pmin(label_x_raw, 170), -170),
        label_y = pmax(pmin(lat - 20, 72), -55),
        label_hjust = if_else(label_x_raw >= long, 0, 1)
      )

    if (nrow(positive_callout) > 0) {
      map_plot <- map_plot +
        geom_curve(
          data = positive_callout,
          aes(x = long, y = lat, xend = label_x, yend = label_y),
          inherit.aes = FALSE,
          curvature = -0.22,
          linewidth = 0.58,
          color = year_positive_line_color,
          alpha = 0.96,
          lineend = "round"
        ) +
        geom_point(
          data = positive_callout,
          aes(x = long, y = lat),
          inherit.aes = FALSE,
          size = 1.3,
          color = year_positive_line_color
        ) +
        ggtext::geom_richtext(
          data = positive_callout,
          aes(x = label_x, y = label_y, label = positive_map_annotation_rich, hjust = label_hjust),
          inherit.aes = FALSE,
          family = "FiraCode",
          size = 2.45,
          lineheight = 1.1,
          color = dark_muted,
          fill = scales::alpha("#1F242F", 0.9),
          label.color = NA,
          label.padding = grid::unit(0.32, "lines")
        )
    }
  }

  timeline_plot <- ggplot() +
    geom_segment(
      aes(x = min_year, xend = max_year, y = 0, yend = 0),
      linewidth = 0.45,
      lineend = "round",
      color = scales::alpha(dark_muted, 0.55)
    ) +
    geom_segment(
      aes(x = min_year, xend = year_i, y = 0, yend = 0),
      linewidth = 1,
      lineend = "round",
      color = dark_fg
    ) +
    geom_point(
      aes(x = year_i, y = 0),
      size = 2.2,
      shape = 21,
      stroke = 0.2,
      fill = dark_fg,
      color = dark_fg
    ) +
    geom_text(
      data = data.frame(x = tick_years, y = 0, label = as.character(tick_years)),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      family = "FiraCode",
      size = 2.35,
      nudge_y = -0.22,
      color = dark_muted
    ) +
    annotate(
      "label",
      x = year_i,
      y = 0.26,
      label = as.character(year_i),
      family = "FiraCodeMedium",
      size = 2.45,
      color = dark_bg,
      fill = dark_fg,
      linewidth = 0,
      label.padding = grid::unit(0.15, "lines")
    ) +
    scale_x_continuous(
      limits = c(min_year - 0.8, max_year + 0.8),
      breaks = NULL
    ) +
    coord_cartesian(ylim = c(-0.5, 0.5), clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = dark_bg, color = NA),
      panel.background = element_rect(fill = dark_bg, color = NA),
      plot.margin = margin(0, 48, 10, 12)
    )

  caption_plot <- ggplot() +
    labs(caption = plot_caption) +
    theme_void() +
    theme(
      plot.caption = element_markdown(
        size = 8,
        color = dark_muted,
        hjust = 0,
        family = theme_caption_family,
        lineheight = 1.5,
        margin = margin(t = 0)
      ),
      plot.background = element_rect(fill = dark_bg, color = NA),
      panel.background = element_rect(fill = dark_bg, color = NA),
      plot.margin = margin(0, 12, 12, 12)
    )

  plot <- map_plot / timeline_plot / caption_plot +
    plot_layout(heights = c(22, 3, 2))

  ggsave(frame_paths[i], plot, width = 11.5, height = 7.5, dpi = 300, bg = dark_bg)
}

# ---- Stitch GIFs ----
gif_path_5x <- "20_global_change.gif"

frames_5x <- rep(frame_paths, each = 5)


gifski(
  png_files = frames_5x,
  gif_file = gif_path_5x,
  width = 3450,
  height = 2250,
  delay = 0.37,
  loop = TRUE
)
