# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(ggtext)
})

# ---- Source Theme ----
source("../utils/theme.R")

# ---- Data ----
falls_raw <- read.csv(
  "IHME-GBD_2023_DATA-de485fc0-1.csv",
  stringsAsFactors = FALSE
)

falls_rates <- falls_raw %>%
  filter(
    population_group_name == "All Population",
    measure_name == "Deaths",
    metric_name == "Rate",
    cause_name == "Falls",
    sex_name == "Both",
    age_name == "55+ years",
    location_name != "Western Europe"
  ) %>%
  select(year, location_name, val)

western_europe_rates <- falls_raw %>%
  filter(
    population_group_name == "All Population",
    measure_name == "Deaths",
    metric_name == "Rate",
    cause_name == "Falls",
    sex_name == "Both",
    age_name == "55+ years",
    location_name == "Western Europe"
  ) %>%
  select(year, val)

year_range <- range(falls_rates$year, na.rm = TRUE)
start_year <- year_range[1]
end_year <- year_range[2]

falls_start_end <- falls_rates %>%
  filter(year %in% c(start_year, end_year)) %>%
  group_by(location_name) %>%
  summarise(
    start = val[year == start_year][1],
    end = val[year == end_year][1],
    .groups = "drop"
  ) %>%
  filter(!is.na(start), !is.na(end)) %>%
  mutate(
    pct_change = if_else(start > 0, (end / start - 1) * 100, NA_real_)
  ) %>%
  arrange(desc(end)) %>%
  mutate(location_name = factor(location_name, levels = rev(location_name)))

top_growth <- falls_start_end %>%
  arrange(desc(pct_change)) %>%
  slice_head(n = 7)

overall_median <- median(falls_start_end$pct_change, na.rm = TRUE)

western_europe_growth <- western_europe_rates %>%
  filter(year %in% c(start_year, end_year)) %>%
  summarise(
    start = val[year == start_year][1],
    end = val[year == end_year][1],
    .groups = "drop"
  ) %>%
  mutate(pct_change = if_else(start > 0, (end / start - 1) * 100, NA_real_))

worst_offender <- top_growth %>% slice_head(n = 1)

x_range <- range(c(falls_start_end$start, falls_start_end$end), na.rm = TRUE)
label_offset <- diff(x_range) * 0.03

end_labels <- falls_start_end %>%
  mutate(
    label_x = if_else(end >= start, end + label_offset, end - label_offset),
    label_hjust = if_else(end >= start, 0, 1),
    label_color = if_else(
      pct_change >= 0,
      alpha(met.brewer("VanGogh2", n = 8)[1], 0.6),
      alpha(met.brewer("VanGogh2", n = 8)[5], 0.6)
    ),
    label = sprintf(
      "<span style='color:%s'>%+.0f%%</span>",
      label_color,
      pct_change
    )
  )

top_growth_text <- paste(
  sprintf(
    "%s (%+.1f%%)",
    as.character(top_growth$location_name),
    top_growth$pct_change
  ),
  collapse = ", "
)

year_palette <- c(
  setNames(alpha(met.brewer("VanGogh1", n = 7)[4], 1), as.character(start_year)),
  setNames(alpha(met.brewer("VanGogh1", n = 7)[1], 0.78), as.character(end_year))
)

year_shape <- c(
  setNames(16, as.character(start_year)),
  setNames(16, as.character(end_year))
)

# ---- Plot ----
plot <- ggplot(falls_start_end, aes(y = location_name)) +
  geom_segment(
    aes(x = start, xend = end, yend = location_name),
    color = alpha(theme_fg, 0.2),
    linewidth = 1.2
  ) +
  geom_point(
    data = falls_start_end %>%
      transmute(location_name, year_type = as.character(start_year), value = start),
    aes(x = value, shape = year_type, color = year_type),
    size = 3.6
  ) +
  geom_point(
    data = falls_start_end %>%
      transmute(location_name, year_type = as.character(end_year), value = end),
    aes(x = value, shape = year_type, color = year_type),
    size = 3.6
  ) +
  ggtext::geom_richtext(
    data = end_labels,
    aes(x = label_x, y = location_name, label = label, hjust = label_hjust),
    family = "FiraSans",
    color = alpha(theme_fg, 0.65),
    size = 4.2,
    vjust = 0.5,
    fill = NA,
    label.color = NA
  ) +
  scale_color_manual(values = year_palette, guide = "none") +
  scale_shape_manual(values = year_shape, breaks = names(year_shape)) +
  scale_x_continuous(
    labels = label_number(accuracy = 0.1, big.mark = ""),
    expand = expansion(mult = c(0.14, 0.2))
  ) +
  guides(
    shape = guide_legend(
      order = 1,
      override.aes = list(color = unname(year_palette))
    )
  ) +
  labs(
    title = "Fall death rates rise across Western Europe's 55+",
    subtitle = paste0(
      "Percent change in fall death rates (55+ years) from ",
      start_year, " to ", end_year, "; ",
      "Western Europe overall ",
      sprintf("%+.0f%%", western_europe_growth$pct_change[1]),
      ", worst: ", as.character(worst_offender$location_name), " ",
      sprintf("%+.0f%%", worst_offender$pct_change), "."
    ),
    x = "Death rate",
    y = NULL,
    shape = "Year",
    caption = caption_global(
      "IHME GBD 2023 Falls data (GHDx), deaths rate (55+ years, both sexes).",
      "Day 30",
      "Global Health Data Exchange"
    )
  ) +
  theme_base() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = alpha(theme_fg, 0.08), linewidth = 0.3),
    plot.title = element_markdown(
      family = theme_title_family,
      face = "bold",
      size = 32,
      color = theme_fg,
      hjust = 0,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_textbox(
      family = "FiraSans",
      size = 18,
      color = night_owlish_light$fg_soft,
      hjust = 0,
      lineheight = 1.15,
      width = unit(0.9, "npc"),
      margin = margin(b = 14),
      padding = margin(6, 0, 0, 0)
    ),
    axis.title.x = element_text(
      size = 16,
      color = alpha(theme_fg, 0.8),
      margin = margin(t = 8)
    ),
    axis.text.x = element_text(size = 11, color = alpha(theme_fg, 0.8)),
    axis.text.y = element_text(size = 11, color = alpha(theme_fg, 0.8)),
    plot.caption = element_markdown(
      family = theme_caption_family,
      size = 10,
      color = night_owlish_light$fg_soft,
      hjust = 0,
      margin = margin(t = 10)
    ),
    plot.margin = margin(16, 16, 16, 16),
    legend.position = "top",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10, color = theme_fg),
    legend.key = element_blank()
  )

# ---- Save Plot ----
ggsave(
  "30_ghde_falls_western_europe_dumbbell.png",
  plot,
  width = 14,
  height = 12,
  dpi = 320,
  type = "cairo",
  bg = "#fff"
)
