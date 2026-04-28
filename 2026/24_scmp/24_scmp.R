source("../utils/theme.R")

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(ggimage)
})

steel_raw <- fromJSON(
  "../data/steel-production-china-vs-world.json",
  flatten = TRUE
)$data$data
cement_raw <- fromJSON(
  "../data/cement-production-china-vs-world.json",
  flatten = TRUE
)$data$data

steel_df <- as.data.frame(steel_raw) %>%
  mutate(year = as.integer(date), commodity = "Steel") %>%
  select(year, commodity, china, world_minus_china)

cement_df <- as.data.frame(cement_raw) %>%
  mutate(year = as.integer(date), commodity = "Cement") %>%
  select(year, commodity, china, world_minus_china)

plot_data <- bind_rows(steel_df, cement_df) %>%
  pivot_longer(
    cols = c(china, world_minus_china),
    names_to = "region",
    values_to = "value"
  ) %>%
  mutate(
    region = recode(
      region,
      china = "China",
      world_minus_china = "Rest of world"
    ),
    commodity = factor(commodity, levels = c("Steel", "Cement")),
    region = factor(region, levels = c("China", "Rest of world")),
  ) %>%
  arrange(year, commodity, region) %>%
  group_by(year, commodity) %>%
  mutate(
    ymax = if_else(commodity == "Steel", -cumsum(value), cumsum(value)),
    ymin = if_else(commodity == "Steel", ymax + value, ymax - value)
  ) %>%
  ungroup()

max_y <- plot_data %>%
  group_by(year, commodity) %>%
  summarise(total = sum(value), .groups = "drop") %>%
  summarise(max_total = max(total, na.rm = TRUE)) %>%
  pull(max_total)
y_breaks <- pretty(c(-max_y, max_y), n = 8)

end_labels <- plot_data %>%
  group_by(commodity, year) %>%
  summarise(
    china = sum(value[region == "China"]),
    total = sum(value),
    .groups = "drop"
  ) %>%
  group_by(commodity) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    china_share = china / total,
    y = if_else(commodity == "Steel", -china, china),
    label = paste0(
      if_else(
        commodity == "Steel",
        "<span style='color:#F8AA8F;'><b>Steel</b></span>",
        "<span style='color:#72CAC1;'><b>Cement</b></span>"
      ),
      "<br>China ",
      percent(china_share, accuracy = 0.1),
      "<br>",
      comma(total),
      " Mt"
    )
  )

bridge_raw <- fromJSON(
  "../data/bridge-construction-china-vs-world.json",
  flatten = TRUE
)$data$data
railway_raw <- fromJSON(
  "../data/railway-length-china-vs-world.json",
  flatten = TRUE
)$data$data

cement_2000_china <- plot_data %>%
  filter(year == 2000, commodity == "Cement", region == "China") %>%
  pull(value)

event_labels <- bind_rows(
  as.data.frame(railway_raw) %>%
    mutate(year = as.integer(date), type = "railway") %>%
    filter(year %in% c(2000, 2020, 2025)),
  as.data.frame(bridge_raw) %>%
    mutate(year = as.integer(date), type = "bridge") %>%
    filter(year %in% c(2010, 2025))
) %>%
  mutate(
    china_share = 100 * china / (china + world_minus_china),
    icon_file = case_when(
      type == "railway" ~ "train-track.png",
      TRUE ~ "golden-gate-bridge.png"
    ),
    label = case_when(
      type == "railway" ~ paste0(round(china_share), "%"),
      TRUE ~ paste0(round(china_share), "%")
    ),
    y_anchor = case_when(
      type == "railway" & year == 2000 ~ cement_2000_china + 18,
      type == "railway" ~ max_y * 1.02,
      type == "bridge" & year == 2010 ~ -max_y * 1.02,
      TRUE ~ -max_y * 1.12
    ),
    y_label = case_when(
      type == "railway" & year == 2000 ~ cement_2000_china + 44,
      type == "railway" ~ max_y * 1.10,
      type == "bridge" & year == 2010 ~ -max_y * 1.06,
      TRUE ~ -max_y * 1.15
    )
  )

railway_final_label <- event_labels %>%
  filter(type == "railway", year == max(year)) %>%
  mutate(
    label = "",
    y_label = y_label + max_y * 0.07
  )

plot_title <- "Mega Urbanization"
plot_subtitle <- "Urbanization surged from 18% to 67% since 1978, powered by over half of the world’s cement and steel output, <br>a 16-fold expansion in expressways, and more than 60% of global bridge construction"
plot_caption <- caption_global(
  source = paste0(
    "World Steel Association; USGS; China National Bureau of Statistics | <b>Icons: </b> ",
    "Railway icons created by Adury5711 - Flaticon: flaticon.com/free-icons/railway;<br>",
    "San francisco icons created by DinosoftLabs - Flaticon: flaticon.com/free-icons/san-francisco"
  ),
  day = "Day 24",
  topic = "Theme: South China Morning Post"
)

title_color <- "#001246"
accent_color <- "#FFCB05"
fill_colors <- c(
  "Steel - China" = "#F8AA8F",
  "Steel - Rest of world" = "#FBD6CA",
  "Cement - China" = "#72CAC1",
  "Cement - Rest of world" = "#C8ECE8"
)

urban_raw <- fromJSON(
  "../data/china-urbanization-history.json",
  flatten = TRUE
)$data$data

urban_df <- as.data.frame(urban_raw) %>%
  mutate(year = as.integer(date)) %>%
  filter(year %in% c(1995, 2005, 2015, max(year)))

urban_df <- urban_df %>%
  mutate(fill_key = if_else(year == max(year), "Last year", "Earlier years"))

urban_plot <- ggplot(urban_df, aes(year, 0, size = value, color = fill_key)) +
  geom_point(
    shape = 16,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = paste0(round(value), "%")),
    color = title_color,
    family = "FiraSans",
    size = 3.5,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = c(1995, 2005, 2015, 2024),
    limits = c(1993, 2028),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_color_manual(
    values = c("Earlier years" = "#8A93A6", "Last year" = accent_color)
  ) +
  scale_size_continuous(range = c(10, 28)) +
  labs(
    title = "Urbanization",
    subtitle = ""
  ) +
  theme_minimal(base_size = 6) +
  theme(
    plot.background = element_rect(
      fill = alpha(theme_bg, 0.0),
      color = alpha(title_color, 0.15),
      linewidth = 0.3
    ),
    panel.background = element_rect(fill = alpha(theme_bg, 0.0), color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.ticks.y = element_blank(),
    axis.text = element_text(
      family = "FiraSans",
      color = title_color,
      size = 5
    ),
    legend.position = "none",
    plot.subtitle = element_markdown(
      family = "FiraSansRegular",
      size = 10,
      margin = margin(t = 3)
    ),
    plot.title = element_markdown(
      family = theme_title_family,
      face = "bold",
      color = title_color,
      size = 13,
      margin = margin(b = 2)
    ),
    plot.margin = margin(2, 0, 2, 0)
  )

urban_grob <- ggplotGrob(urban_plot)

expressway_raw <- fromJSON(
  "../data/china-expressway-length.json",
  flatten = TRUE
)$data$data

expressway_df <- as.data.frame(expressway_raw) %>%
  mutate(year = as.integer(date)) %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2020, 2024)) %>%
  mutate(fill_key = if_else(year == max(year), "Last year", "Earlier years"))

expressway_plot <- ggplot(
  expressway_df,
  aes(x = value, y = factor(year), fill = fill_key)
) +
  geom_col(width = 0.72) +
  geom_text(
    aes(label = comma(value)),
    family = "FiraSansRegular",
    hjust = -0.1,
    size = 3,
    color = title_color
  ) +
  scale_fill_manual(
    values = c(
      "Earlier years" = alpha(title_color, 0.25),
      "Last year" = "#DB5E49"
    ),
    guide = "none"
  ) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.14))) +
  labs(
    title = "Expressway",
    subtitle = "km built"
  ) +
  theme_minimal(base_size = 6) +
  theme(
    plot.background = element_rect(
      fill = alpha(theme_bg, 0),
      color = alpha(title_color, 0.0),
      linewidth = 0
    ),
    panel.background = element_rect(fill = alpha(theme_bg, 0), color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(
      family = "FiraSans",
      color = title_color,
      size = 9
    ),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_markdown(
      family = theme_title_family,
      face = "bold",
      color = title_color,
      size = 13,
      margin = margin(b = 1)
    ),
    plot.subtitle = element_text(
      color = title_color,
      size = 8,
      margin = margin(b = 1, t = 2)
    ),
    plot.margin = margin(2, 2, 2, 2)
  )

expressway_grob <- ggplotGrob(expressway_plot)

china_plot <- ggplot() +
  geom_hline(yintercept = 0, color = accent_color, linewidth = 0.5) +
  geom_ribbon(
    data = filter(plot_data, commodity == "Steel"),
    aes(
      x = year,
      ymin = ymin,
      ymax = ymax,
      fill = interaction(commodity, region, sep = " - "),
      group = interaction(commodity, region)
    ),
    alpha = 0.95,
    color = NA
  ) +
  geom_ribbon(
    data = filter(plot_data, commodity == "Cement"),
    aes(
      x = year,
      ymin = ymin,
      ymax = ymax,
      fill = interaction(commodity, region, sep = " - "),
      group = interaction(commodity, region)
    ),
    alpha = 0.95,
    color = NA
  ) +
  geom_label(
    data = data.frame(year = seq(1990, 2025, 5)),
    aes(x = year, y = 0, label = year),
    inherit.aes = FALSE,
    size = 2.7,
    label.size = 0,
    label.padding = unit(0.08, "lines"),
    fill = alpha("#FFFFFF", 0.92),
    color = title_color,
    vjust = 0.5
  ) +
  geom_textbox(
    data = filter(end_labels, commodity == "Steel"),
    aes(x = year + 0.55, y = y, label = label),
    family = "FiraSans",
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    size = 4.2,
    width = unit(1.75, "in"),
    halign = 0,
    fill = NA,
    box.color = NA,
    color = title_color,
    lineheight = 1.05
  ) +
  geom_textbox(
    data = filter(end_labels, commodity == "Cement"),
    aes(x = year + 0.55, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    family = "FiraSans",
    vjust = 0.5,
    size = 4.2,
    width = unit(1.75, "in"),
    halign = 0,
    fill = NA,
    box.color = NA,
    color = title_color,
    lineheight = 1.05
  ) +
  geom_segment(
    data = filter(event_labels, type == "bridge"),
    aes(x = year, xend = year, y = 0, yend = -2000),
    inherit.aes = FALSE,
    color = title_color,
    linewidth = 0.35,
    linetype = "dotted"
  ) +
  geom_segment(
    data = filter(event_labels, type == "railway"),
    aes(x = year, xend = year, y = 0, yend = y_anchor),
    inherit.aes = FALSE,
    color = title_color,
    linewidth = 0.35,
    linetype = "dotted"
  ) +
  geom_label(
    data = filter(event_labels, type == "bridge"),
    aes(x = year + 0.2, y = y_label + 2000, label = label),
    inherit.aes = FALSE,
    vjust = 0.5,
    hjust = 0.5,
    size = 7,
    linewidth = 0,
    label.padding = unit(0.22, "lines"),
    fill = alpha(theme_bg, 0.92),
    color = title_color
  ) +
  geom_image(
    data = filter(event_labels, type == "bridge"),
    aes(x = year - 0.05, y = y_label + 2500, image = icon_file),
    inherit.aes = FALSE,
    size = 0.032
  ) +
  geom_text(
    data = filter(event_labels, type == "bridge"),
    aes(
      x = year,
      y = y_label + 1750,
      label = " of global bridge construction takes place in China."
    ),
    inherit.aes = FALSE,
    size = 3.25,
    color = title_color,
    vjust = 0.5
  ) +
  annotation_custom(
    grob = urban_grob,
    xmin = 1990.4,
    xmax = 2010,
    ymin = -max_y * 1.31,
    ymax = -max_y * 0.82
  ) +
  annotation_custom(
    grob = expressway_grob,
    xmin = 1990.4,
    xmax = 2004.8,
    ymin = max_y * 0.46,
    ymax = max_y * 1.18
  ) +
  geom_label(
    data = filter(event_labels, type == "railway"),
    aes(x = year, y = y_label + 600, label = label),
    inherit.aes = FALSE,
    vjust = 0,
    size = 7,
    linewidth = 0,
    label.padding = unit(0.22, "lines"),
    fill = alpha(theme_bg, 0.92),
    color = title_color,
    hjust = 0.5
  ) +
  geom_image(
    data = filter(event_labels, type == "railway"),
    aes(x = year, y = y_label, image = icon_file),
    inherit.aes = FALSE,
    size = 0.032
  ) +
  geom_text(
    data = filter(event_labels, type == "railway"),
    aes(x = year, y = y_label + 500, label = "of global railway length"),
    inherit.aes = FALSE,
    size = 3.25,
    color = title_color,
    vjust = 0.5
  ) +
  geom_label(
    data = railway_final_label,
    aes(x = year, y = y_label, label = label),
    inherit.aes = FALSE,
    size = 2.8,
    linewidth = 0,
    label.padding = unit(0.14, "lines"),
    fill = alpha(theme_bg, 0.92),
    color = title_color,
    hjust = 0.5
  ) +
  scale_fill_manual(values = fill_colors) +
  scale_x_continuous(
    breaks = seq(1990, 2025, 5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = y_breaks,
    labels = function(x) comma(abs(x)),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  coord_cartesian(
    xlim = c(1990, 2027.5),
    ylim = c(-max_y * 1.34, max_y * 1.24),
    clip = "off"
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = NULL,
    y = "Million tonnes",
    caption = plot_caption,
    fill = NULL
  ) +
  theme_base() +
  theme(
    plot.background = element_rect(fill = "#fff", color = NA),
    panel.background = element_rect(fill = "#fff", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = alpha("#D7E3E8", 0.85),
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.y = element_line(color = title_color, linewidth = 0.45),
    axis.text = element_text(color = title_color),
    axis.title.y = element_text(color = title_color, margin = margin(r = 10)),
    legend.family = "FiraSans",
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(color = title_color),
    legend.title = element_blank(),
    plot.title = element_markdown(
      family = theme_title_family,
      face = "bold",
      color = title_color,
      size = 32,
      lineheight = 1.05
    ),
    plot.subtitle = element_markdown(
      family = "FiraSans",
      color = "grey50",
      size = 18,
      lineheight = 1.35,
      margin = margin(b = 12)
    ),
    plot.caption = element_markdown(
      family = theme_caption_family,
      color = theme_muted,
      size = 9,
      hjust = 0,
      margin = margin(t = 14)
    ),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(20, 30, 16, 30)
  )

ggsave(
  "24_china_cement_steel_production.png",
  china_plot,
  width = 14,
  height = 12,
  dpi = 330
)
