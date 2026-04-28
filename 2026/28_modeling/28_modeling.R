suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(ggtext)
})

source("../utils/theme.R")
years <- 1980:2025
rates_map <- c(
  `1980` = 2.82,
  `1981` = 4.26, # was 4.21
  `1982` = 3.28, # was 3.31
  `1983` = 3.63, # was 3.52
  `1984` = 4.41, # was 4.50
  `1985` = 5.16, # was 5.23
  `1986` = 3.29, # was 3.33
  `1987` = 4.65, # was 4.73
  `1988` = 6.66, # was 6.79
  `1989` = 4.93, # was 4.86
  `1990` = 4.84, # was 4.89
  `1991` = 3.52, # was 3.42
  `1992` = 0.90, # was 0.85
  `1993` = -0.46, # was -0.52
  `1994` = 1.08, # was 0.99
  `1995` = 2.63,
  `1996` = 3.13,
  `1997` = 0.98,
  `1998` = -1.27,
  `1999` = -0.33,
  `2000` = 2.76,
  `2001` = 0.39,
  `2002` = 0.04,
  `2003` = 1.54,
  `2004` = 2.19,
  `2005` = 1.80,
  `2006` = 1.37,
  `2007` = 1.48,
  `2008` = -1.22,
  `2009` = -5.69,
  `2010` = 4.10,
  `2011` = 0.02,
  `2012` = 1.37,
  `2013` = 2.01,
  `2014` = 0.30,
  `2015` = 1.56,
  `2016` = 0.75,
  `2017` = 1.68,
  `2018` = 0.64, # was 0.56
  `2019` = -0.40, # was 0.27 ← biggest fix
  `2020` = -4.17, # was -4.59
  `2021` = 2.70, # was 2.56
  `2022` = 0.94,
  `2023` = 1.48, # was 1.90
  `2024` = 0.10,
  `2025` = 0.10 # WB not yet published; keep as estimate
)

actual_index <- numeric(length(years))
actual_index[1] <- 100
for (i in 2:length(years)) {
  yr <- as.character(years[i])
  actual_index[i] <- actual_index[i - 1] * (1 + rates_map[[yr]] / 100)
}
actual_index <- round(actual_index, 2)

optimistic_45 <- round(100 * (1.045^(0:(length(years) - 1))), 2)
baseline_33 <- round(100 * (1.033^(0:(length(years) - 1))), 2)
china_level <- 530

plot_df <- tibble::tibble(
  year = rep(years, 3),
  value = c(actual_index, optimistic_45, baseline_33),
  series = factor(
    rep(
      c(
        "Actual Japan GDP",
        "Optimistic scenario (4.5%/yr)",
        "Baseline scenario (3.3%/yr)"
      ),
      each = length(years)
    ),
    levels = c(
      "Actual Japan GDP",
      "Optimistic scenario (4.5%/yr)",
      "Baseline scenario (3.3%/yr)"
    )
  )
)


event_df <- tibble::tibble(
  year = c(1985, 1989, 1991, 2008, 2020),
  y = c(150, 210, 260, 165, 220),
  label = c(
    "Plaza Accord (1985)\nSharp yen appreciation",
    "Bubble peak (1989)\nStocks & real estate at extreme valuations",
    "Bubble burst (1991)\nBanking crisis begins",
    "Global Financial Crisis (2008)\nExternal shock hits exports",
    "COVID-19 (2020)"
  )
)
actual_2025 <- actual_index[length(actual_index)]
optimistic_2025 <- optimistic_45[length(optimistic_45)]
baseline_2025 <- round(baseline_33[length(baseline_33)])

plot_title <- "What if <span style='color:#F07178;'>Japan</span> kept its <span style='color:#2E86AB;'>1980s momentum</span>?"
plot_subtitle <- paste0(
  "Japan was one of the fastest-growing economies in the 1980s, seemingly ",
  "on track to rival the United States. But the asset bubble collapse and ",
  "banking crisis halted that momentum, leaving growth largely stuck at ",
  "levels reached by the early 1990s."
)


plot_caption <- caption_global(
  source = "World Bank | Calculations based on real GDP growth rates, indexed to 1980 = 100.",
  day = "Day 28",
  topic = "Modeling"
)

modeling_plot <- ggplot() +
  geom_rect(
    aes(xmin = 1991, xmax = 2003, ymin = 0, ymax = 175),
    fill = alpha(night_owlish_light$fg_soft, 0.06),
    color = alpha(night_owlish_light$fg_soft, 0.18),
    linewidth = 0.4
  ) +
  geom_line(
    data = dplyr::filter(plot_df, series == "Actual Japan GDP"),
    aes(x = year, y = value),
    color = night_owlish_cat[3],
    linewidth = 1.15,
    lineend = "round"
  ) +
  geom_line(
    data = dplyr::filter(plot_df, series == "Optimistic scenario (4.5%/yr)"),
    aes(x = year, y = value),
    color = night_owlish_cat[1],
    linewidth = 1.0,
    linetype = "dashed",
    lineend = "round"
  ) +
  geom_line(
    data = dplyr::filter(plot_df, series == "Baseline scenario (3.3%/yr)"),
    aes(x = year, y = value),
    color = alpha(night_owlish_cat[1], 0.75),
    linewidth = 0.95,
    linetype = "dotted",
    lineend = "round"
  ) +
  geom_point(
    data = subset(
      plot_df,
      series == "Actual Japan GDP" & year %in% c(1985, 1989, 1991, 2009, 2020)
    ),
    aes(x = year, y = value),
    color = night_owlish_cat[3],
    size = 2.4
  ) +
  geom_label(
    data = event_df[1, ],
    aes(x = year, y = y + 25, label = label),
    inherit.aes = FALSE,
    fill = alpha("white", 0),
    color = alpha(theme_fg, 0.8),
    linewidth = 0,
    size = 4.5,
    family = "FiraSans",
    fontface = "bold",
    label.r = unit(0.12, "lines"),
    label.padding = unit(0.18, "lines")
  ) +
  geom_label(
    data = event_df[2, ],
    aes(x = year, y = y + 50, label = label),
    inherit.aes = FALSE,
    fill = alpha("white", 0),
    color = alpha(theme_fg, 0.8),
    linewidth = 0,
    size = 4.5,
    family = "FiraSans",
    fontface = "bold",
    label.r = unit(0.12, "lines"),
    label.padding = unit(0.18, "lines")
  ) +

  geom_label(
    data = event_df[3, ],
    aes(x = year, y = y + 150, label = label),
    inherit.aes = FALSE,
    fill = alpha("white", 0),
    color = alpha(theme_fg, 0.8),
    linewidth = 0,
    size = 4.5,
    family = "FiraSans",
    fontface = "bold",
    label.r = unit(0.12, "lines"),
    label.padding = unit(0.18, "lines")
  ) +
  geom_label(
    data = event_df[4, ],
    aes(x = year, y = y - 14, label = label),
    inherit.aes = FALSE,
    fill = alpha("white", 0),
    linewidth = 0,
    color = alpha(theme_fg, 0.8),
    hjust = 0.25,
    size = 4.5,
    family = "FiraSans",
    fontface = "bold",
    label.r = unit(0.12, "lines"),
    label.padding = unit(0.18, "lines")
  ) +
  geom_label(
    data = event_df[5, ],
    aes(x = year, y = y - 45, label = label),
    inherit.aes = FALSE,
    fill = alpha("white", 0),
    color = alpha(theme_fg, 0.8),
    linewidth = 0,
    hjust = 0.5,
    size = 4.5,
    family = "FiraSans",
    fontface = "bold",
    label.r = unit(0.12, "lines"),
    label.padding = unit(0.18, "lines")
  ) +
  geom_segment(
    data = event_df[1, ],
    aes(x = year, xend = year, y = y - 20, yend = y - 2),
    color = alpha(night_owlish_light$fg_soft, 0.5),
    linewidth = 0.4
  ) +
  geom_segment(
    data = event_df[2, ],
    aes(x = year, xend = year, y = 155, yend = 230),
    color = alpha(night_owlish_light$fg_soft, 0.5),
    linewidth = 0.4
  ) +
  geom_segment(
    data = event_df[3, ],
    aes(x = year, xend = year, y = 165, yend = 380),
    color = alpha(night_owlish_light$fg_soft, 0.5),
    linewidth = 0.4
  ) +

  geom_segment(
    aes(x = 1991, xend = 2012, y = 115, yend = 115),
    color = alpha(night_owlish_light$fg_soft, 0.5),
    linewidth = 0.4
  ) +
  geom_segment(
    aes(x = 1991, xend = 1991, y = 115, yend = 95),
    color = alpha(night_owlish_light$fg_soft, 0.5),
    linewidth = 0.4
  ) +
  geom_segment(
    aes(x = 2012, xend = 2012, y = 115, yend = 95),
    color = alpha(night_owlish_light$fg_soft, 0.5),
    linewidth = 0.4
  ) +

  geom_label(
    aes(x = 2022, y = actual_2025 + 20, label = "Japan actual (~$4.3T)"),
    fill = alpha("white", 0),
    color = night_owlish_cat[3],
    linewidth = 0,
    size = 5.5,
    family = "FiraSans",
    fontface = "bold"
  ) +
  geom_label(
    aes(
      x = 2021.5,
      y = optimistic_2025 + 15,
      label = "4.5% scenario (~$14–16T)"
    ),
    fill = alpha("white", 0),
    color = night_owlish_cat[1],
    linewidth = 0,
    size = 5.5,
    family = "FiraSans",
    fontface = "bold"
  ) +
  geom_label(
    data = tibble::tibble(
      x = 2001,
      y = 115,
      label = "Lost Decades (1991–2012)\nLow growth + deflation"
    ),
    aes(x = x, y = y, label = label),
    fill = alpha("#fff", 1),
    color = alpha(theme_fg, 0.8),
    linewidth = 0,
    size = 4.5,
    family = "FiraSans",
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = seq(1980, 2025, by = 5),
    limits = c(1980, 2025),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(80, 800),
    breaks = seq(100, 800, by = 100),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = NULL,
    y = "GDP index (1980 = 100)",
    caption = plot_caption
  ) +
  geom_label(
    aes(
      x = 2017.5,
      y = baseline_2025 + 18,
      label = "3.3% scenario (~$8–10T)"
    ),
    fill = alpha("white", 0),
    color = alpha(night_owlish_cat[1], 0.8),
    linewidth = 0,
    size = 5.5,
    family = "FiraSans",
    fontface = "bold",
    hjust = 0
  ) +
  theme_base() +
  theme(
    plot.background = element_rect(fill = night_owlish_light$bg, color = NA),
    panel.background = element_rect(fill = night_owlish_light$bg, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = alpha(night_owlish_light$gray, 0.14),
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(
      size = 14,
      angle = 90,
      color = alpha(theme_fg, 0.8),
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(size = 10, color = alpha(theme_fg, 0.8)),
    axis.text.y = element_text(size = 10, color = alpha(theme_fg, 0.8)),
    legend.position = "top",
    legend.justification = "left",
    legend.box = "horizontal",
    legend.text = element_text(size = 10, color = night_owlish_light$fg_soft),
    legend.key.width = unit(16, "pt"),
    legend.key.height = unit(8, "pt"),
    legend.margin = margin(b = 6),
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
      lineheight = 1.2,
      width = unit(0.85, "npc"), # controls wrapping width
      margin = margin(b = 14),
      padding = margin(0, 0, 0, 0)
    ),
    plot.caption = element_markdown(
      family = theme_caption_family,
      size = 10,
      color = night_owlish_light$fg_soft,
      hjust = 0,
      margin = margin(t = 12)
    ),
    plot.margin = margin(18, 18, 18, 18)
  )

ggsave(
  filename = "28_modeling.png",
  plot = modeling_plot,
  width = 14,
  height = 12,
  dpi = 320,
  bg = night_owlish_light$bg
)
