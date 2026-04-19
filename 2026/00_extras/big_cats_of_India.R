# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(showtext)
  library(stringr)
  library(scales)
})

# ---- Source Theme ----
source("../utils/theme.R")

if (!"NotoEmoji" %in% sysfonts::font_families()) {
  sysfonts::font_add("NotoEmoji", "../utils/fonts/NotoEmoji.ttf")
}
font_add("NotoEmoji",  "../utils/fonts/NotoEmoji.ttf")

# ---- Load Data ----
data_path <- "../data/big_cats.csv"

big_cats <- read_csv(data_path, show_col_types = FALSE) %>%
  filter(Species %in% c("Tiger", "Asiatic Lion")) %>%
  transmute(
    species = recode(Species, "Asiatic Lion" = "Lion", "Tiger" = "Tiger"),
    year = as.integer(Year),
    population = parse_number(Population),
    estimated = str_detect(Population, "\\(est\\)"),
    source = `Data Source`
  ) %>%
  mutate(emoji = if_else(species == "Tiger", "🐯", "🦁")) %>%
  arrange(species, year)

growth <- big_cats %>%
  group_by(species) %>%
  summarise(
    start_year = first(year),
    start_population = first(population),
    end_year = last(year),
    end_population = last(population),
    absolute_change = end_population - start_population,
    relative_change = (end_population / start_population) - 1,
    .groups = "drop"
  ) %>%
  mutate(
    change_sign = if_else(absolute_change >= 0, "+", ""),
    pct_sign = if_else(relative_change >= 0, "+", "")
  )

latest_points <- big_cats %>%
  group_by(species) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  left_join(
    growth %>%
      select(species, start_year, absolute_change, relative_change, change_sign, pct_sign),
    by = "species"
  ) %>%
  mutate(
    label = paste0(comma(population), " (", pct_sign, percent(relative_change, accuracy = 1), ")"),
    label_nudge_y = if_else(species == "Tiger", 90, -45)
  )

starting_points <- big_cats %>%
  filter(!estimated) %>%
  group_by(species) %>%
  slice_min(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    start_label = paste0(comma(population), " (", year, ")"),
    label_nudge_y = if_else(species == "Tiger", -95, 55)
  )

tiger_estimation_note <- big_cats %>%
  filter(species == "Tiger", estimated) %>%
  slice_min(year, n = 1, with_ties = FALSE) %>%
  mutate(note = "Estimated baseline")

plot_title <- "India's Big Cat Recovery: Tigers and Asiatic Lions"
plot_subtitle <- paste0(
  "Population trends show sustained gains linked to long-running protection, habitat management, and census efforts."
)
plot_caption <- caption_general(
  "Sources: NTCA/WII all-India tiger estimates; Gujarat lion census reports; MoEFCC compilations in project dataset<br>"
)

plot <- ggplot(big_cats, aes(x = year, y = population, color = species)) +
  geom_line(linewidth = 0.55, linetype = "solid") +
  geom_label(
    data = tiger_estimation_note,
    aes(x = year + 1.2, y = population + 230, label = note),
    inherit.aes = FALSE,
    family = "FiraCode",
    size = 2.5,
    linewidth = 0.15,
    label.padding = unit(0.13, "lines"),
    fill = alpha("white", 0.85),
    color = "#E76F00",
    show.legend = FALSE
  ) +
  geom_text(
    data = starting_points,
    aes(x = year - 0.18, y = population + label_nudge_y, label = start_label, color = species),
    inherit.aes = FALSE,
    family = "FiraCode",
    hjust = 1,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = emoji),
    family = "NotoEmoji",
    vjust = 0.5,
    size = 5.2,
    show.legend = FALSE
  ) +
  geom_text(
    data = latest_points,
    aes(x = year + 0.5, y = population + label_nudge_y, label = label, color = species),
    inherit.aes = FALSE,
    family = "FiraCodeMedium",
    fontface = "bold",
    hjust = 0,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Tiger" = "#E76F00", "Lion" = "#D4A017"),
    name = NULL
  ) +
  guides(
    color = guide_legend(override.aes = list(linetype = "solid", linewidth = 0.55))
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2025, by = 5),
    limits = c(1999.5, 2028)
  ) +
  scale_y_continuous(
    labels = comma,
    limits = c(250, 3850),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = NULL,
    y = "Population",
    caption = plot_caption
  ) +
  theme_base() +
  theme(
    plot.title = element_markdown(margin = margin(b = 5)),
    plot.subtitle = element_markdown(size = 12.8, margin = margin(b = 16)),
    panel.grid.major.y = element_line(color = alpha(night_owlish_light$gray, 0.22), linewidth = 0.35),
    panel.grid.major.x = element_line(color = alpha(night_owlish_light$gray, 0.12), linewidth = 0.25),
    axis.title.y = element_text(size = 11.5, angle = 90, margin = margin(r = 8)),
    legend.position = "right",
    legend.direction = "vertical",
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    plot.margin = margin(28, 72, 26, 12)
  )

# ---- Save Plot ----
output_path <- "big_cats_of_india_conservation.png"
ggsave(output_path, plot, width = 13, height = 8, type = "cairo", dpi = 320, bg = theme_bg)
