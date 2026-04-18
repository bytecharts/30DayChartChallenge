# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
})

# ---- Source Theme ----
source("../utils/theme.R")

data_path <- "../data/haber_bosch_population_fertilizer_use.csv"
hb_data <- read_csv(data_path, show_col_types = FALSE)

# ---- Scale Fertilizer for Secondary Axis ----
scale_factor <- max(hb_data$population_billions) / max(hb_data$nitrogen_fertilizer_mtN)

# ---- Adoption Milestones ----
milestones <- tibble::tribble(
  ~year, ~label,
  1913, "1913\nInvention of Haber-Bosch process",
  1960, "1960s\nIndia's Green Revolution",
  1972, "1970s\nChina scales up fertilizer use",
  2005, "2000s\nIndia and China dominate demand"
) %>%
  mutate(
    population = approx(hb_data$year, hb_data$population_billions, xout = year)$y,
    nitrogen = approx(hb_data$year, hb_data$nitrogen_fertilizer_mtN, xout = year)$y,
    family ="FiraCode",
    y = if_else(year <= 1965, population, nitrogen * scale_factor),
    nudge_y = case_when(
      year <= 1950 ~ 0.35,
      year <= 1978 ~ -0.45,
      year <= 2005 ~ 0.35,
      TRUE ~ -0.4
    )
  )

# ---- Plot Data ----
plot <- ggplot(hb_data, aes(x = year)) +
  geom_vline(xintercept = 1913, linetype = "dashed", linewidth = 0.8, color = "#F07178") +
  geom_line(aes(y = population_billions, color = "Population"), linewidth = 1.2) +
  geom_point(aes(y = population_billions, color = "Population"), size = 2) +
  geom_line(aes(y = nitrogen_fertilizer_mtN * scale_factor, color = "Nitrogen fertilizer"), linewidth = 1.2) +
  geom_point(aes(y = nitrogen_fertilizer_mtN * scale_factor, color = "Nitrogen fertilizer"), size = 2) +
  ggrepel::geom_label_repel(
    data = milestones,
    aes(x = year, y = y, label = label),
    nudge_y = milestones$nudge_y,
    size = 3,
    label.size = 0.15,
    fill = scales::alpha("white", 0.85),
    color = "gray20",
    family = "FiraCode",
    segment.color = "gray70",
    min.segment.length = 0,
    seed = 1,
    box.padding = 0.35,
    point.padding = 0.2,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c(
    "Population" = night_owlish_cat[1],
    "Nitrogen fertilizer" = night_owlish_cat[4]
  )) +
  scale_y_continuous(
    name = "Population (billions)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Nitrogen fertilizer (Mt N)")
  ) +
  labs(
    title = "Haber-Bosch: The Process That Feeds Billions",
    subtitle = "How one chemical process made modern population levels possible",
    caption = caption_global("<b>Sources:</b> FAO, UN WPP, Smil (2001), Lu & Tian (2017)", "Day 16", "Causation"),
    x = "Year",
    y = "Population (billions)",
    sec.axis = dup_axis(name = "Nitrogen fertilizer (Mt N)"),
    color = NULL
  ) +
  theme_base() +
  theme(

    legend.position = "top",
    axis.title.y = element_text(
      margin = margin(r = 8), angle =90
    ),
    axis.title.y.right = element_text(
      margin = margin(l = 8)
    )
  )

# ---- Save Plot ----
output_path <- "16_causation.png"
ggsave(output_path, plot, width = 12, height = 8, type = "cairo", dpi = 300)
