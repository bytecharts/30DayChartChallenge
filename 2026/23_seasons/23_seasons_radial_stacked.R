# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(geomtextpath)
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(ggtext)
  library(scales)
})

# ---- Source Theme ----
source("../utils/theme.R")

# ---- Load Data ----
stats_path <- "stats/fuck_stats.json"
if (!file.exists(stats_path)) {
  stop("stats/fuck_stats.json not found.")
}

stats_raw <- fromJSON(stats_path, simplifyDataFrame = FALSE)
if (length(stats_raw$episodes) == 0) {
  stop("No episode data found in stats/fuck_stats.json")
}

labels_path <- "stats/episode_labels.json"
label_map <- list()
if (file.exists(labels_path)) {
  labels_raw <- fromJSON(labels_path, simplifyDataFrame = FALSE)
  if (length(labels_raw$episodes) > 0) {
    label_map <- setNames(
      vapply(labels_raw$episodes, function(x) as.character(x$label), character(1)),
      vapply(labels_raw$episodes, function(x) as.character(x$episode_code), character(1))
    )
  }
}

# ---- Flatten Episode Character Counts ----
episode_character_rows <- lapply(stats_raw$episodes, function(ep) {
  if (length(ep$by_character) == 0) {
    return(NULL)
  }

  bind_rows(lapply(ep$by_character, function(ch) {
    data.frame(
      season = as.character(ep$season),
      episode = as.character(ep$episode),
      episode_code = as.character(ep$episode_code),
      character = as.character(ch$character),
      fucks = as.numeric(ch$fucks),
      stringsAsFactors = FALSE
    )
  }))
})

character_stats <- bind_rows(episode_character_rows)
if (nrow(character_stats) == 0) {
  stop("No character counts found in stats/fuck_stats.json")
}

character_stats <- character_stats |>
  mutate(
    season_num = as.integer(str_extract(season, "[0-9]+")),
    episode_num = as.integer(str_match(episode_code, "E([0-9]+)")[, 2]),
    season_label = paste0("Season ", season_num)
  )

character_totals <- character_stats |>
  summarise(total_fucks = sum(fucks), .by = character) |>
  arrange(desc(total_fucks), character)

roy_chars <- c("LOGAN", "KENDALL", "ROMAN", "SHIV", "TOM")
highlight_chars <- character_totals |>
  filter(character %in% roy_chars) |>
  arrange(total_fucks) |>
  pull(character)
character_levels <- c("Others", highlight_chars)

stacked_stats <- character_stats |>
  mutate(character_stack = if_else(character %in% highlight_chars, character, "Others")) |>
  summarise(
    fucks = sum(fucks),
    .by = c(season_num, season_label, episode_num, episode_code, episode, character_stack)
  ) |>
  mutate(character_stack = factor(character_stack, levels = character_levels))

episode_totals <- stacked_stats |>
  summarise(
    total_fucks = sum(fucks),
    .by = c(season_num, season_label, episode_num, episode_code, episode)
  ) |>
  arrange(season_num, episode_num) |>
  mutate(
    episode_index = row_number(),
    episode_label = str_replace(episode_code, "^S[0-9]+_", "")
  )

if (nrow(episode_totals) == 0) {
  stop("No episode totals computed.")
}

plot_data <- stacked_stats |>
  left_join(
    episode_totals |>
      select(season_num, episode_num, episode_code, episode, episode_index),
    by = c("season_num", "episode_num", "episode_code", "episode")
  )

stacked_plot_data <- plot_data |>
  arrange(season_num, episode_num, fucks, character_stack) |>
  group_by(episode_index, season_num, season_label, episode_num, episode_code, episode) |>
  mutate(
    ymin = lag(cumsum(fucks), default = 0),
    ymax = cumsum(fucks)
  ) |>
  ungroup()

season_spans <- episode_totals |>
  summarise(
    start = min(episode_index) - 0.5,
    end = max(episode_index) + 0.5,
    center = mean(episode_index),
    .by = season_label
  ) |>
  arrange(season_label)

top_episode_labels <- episode_totals |>
  group_by(season_label) |>
  slice_max(total_fucks, n = 2, with_ties = FALSE) |>
  ungroup() |>
  mutate(plot_label = if_else(
    episode_code %in% names(label_map),
    unname(label_map[episode_code]),
    paste0(episode_label, "\n", total_fucks)
  ))

max_total <- max(episode_totals$total_fucks)
label_pad <- max(6, max_total * 0.1)
ring_breaks <- pretty(c(0, max_total), n = 4)
ring_breaks <- ring_breaks[ring_breaks > 0 & ring_breaks < max_total]
ring_paths <- bind_rows(lapply(ring_breaks, function(break_value) {
  tibble(
    x = c(0.5, max(episode_totals$episode_index) + 0.5),
    y = break_value - 0.35,
    label = comma(break_value),
    ring = as.character(break_value)
  )
}))

top_episode_labels <- top_episode_labels |>
  mutate(
    plot_label = str_wrap(plot_label, width = 28),
    label_y = max_total + label_pad * -0.08
  )

season_label_y <- max_total + label_pad * 1.00

season_line_labels <- season_spans |>
  mutate(
    label = season_label,
    label_y = max_total + label_pad * 1.24
  )

plot_title <- "Who uses the \"f-word\" most in Succession?"
plot_subtitle <- paste0(
  "<span style='color:", night_owlish_cat[5], ";'><b>Roman</b></span> leads with 445, ",
  "<span style='color:", night_owlish_cat[4], ";'><b>Kendall</b></span> follows with 396, and ",
  "<span style='color:", night_owlish_cat[3], ";'><b>Shiv</b></span> is close at 386.<br> ",
  "2,993 total uses across 39 episodes."
)

plot_caption <- caption_global(
  source = "springfieldspringfield.co.uk ; Curse words counted from cleaned speaker-attributed subtitle across all episodes.",
  day = "Day 23",
  topic = "Seasons"
)

palette_values <- c(
  LOGAN = night_owlish_cat[2],
  KENDALL = night_owlish_cat[4],
  ROMAN = night_owlish_cat[5],
  SHIV = night_owlish_cat[3],
  TOM = night_owlish_cat[1],
  Others = night_owlish_light$gray
)

polar_plot <- ggplot() +
  geom_rect(
    data = stacked_plot_data,
    aes(
      xmin = episode_index - 0.41,
      xmax = episode_index + 0.41,
      ymin = ymin,
      ymax = ymax,
      fill = character_stack
    ),
    alpha = 0.96,
    color = night_owlish_light$bg
  ) +
  geom_path(
    data = ring_paths,
    aes(x = x, y = y, group = ring),
    inherit.aes = FALSE,
    color = alpha(theme_fg, 0.28),
    linewidth = 0.8,
    lineend = "round"
  ) +
  geom_textpath(
    data = ring_paths,
    aes(x = x, y = y, label = label, group = ring),
    inherit.aes = FALSE,
    colour = alpha(theme_fg,0.6),
    family = "FiraSans",
    size = 3.8,
    upright = TRUE,
    gap = TRUE,
    text_only = TRUE
  ) +
#  geom_segment(
    #data = top_episode_labels,
    #aes(
      #x = episode_index,
      #xend = episode_index,
      #y = total_fucks,
      #yend = label_y
    #),
    #inherit.aes = FALSE,
    #linewidth = 0.35,
    #color = alpha(theme_fg, 0.45)
  #) +
#  geom_text(
    #data = top_episode_labels,
    #aes(
      #x = episode_index,
      #y = label_y,
      #label = plot_label
    #),
    #inherit.aes = FALSE,
    #family = "FiraSans",
    #size = 3.5,
    #color = theme_fg,
    #fontface = "bold",
    #vjust = 0
  #) +
  geom_vline(
    xintercept = season_spans$start,
    linewidth = 1.15,
    color = alpha(theme_fg, 0.46)
  ) +
  geom_text(
    data = season_line_labels,
    aes(x = start, y = label_y, label = label),
    inherit.aes = FALSE,
    family = "SpaceGrotesk",
    fontface = "bold",
    size = 5,
    color = alpha(theme_fg, 0.46),
    hjust = -0.15,
    vjust = -0.35
  ) +
  scale_fill_manual(values = palette_values, breaks = names(palette_values), name = "Character") +
  scale_x_continuous(
    breaks = episode_totals$episode_index,
    labels = rep("", nrow(episode_totals))
  ) +
  scale_y_continuous(
    limits = c(-label_pad, season_label_y + label_pad * 0.3),
    expand = expansion(mult = c(0, 0.08))
  ) +
  coord_polar(start = -pi / 2, clip = "off") +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = plot_caption
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE)) +
  theme_base() +
  theme(
    plot.background = element_rect(fill = night_owlish_light$bg, color = NA),
    panel.background = element_rect(fill = night_owlish_light$bg, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, color = theme_fg),
    legend.text = element_text(size = 9, family = "FiraSans", color = theme_muted),
    legend.key.height = unit(0.34, "cm"),
    legend.key.width = unit(0.9, "cm"),
    plot.title = element_markdown(
      family = theme_title_family,
      face = "bold",
      size = 32,
      color = theme_fg,
      lineheight = 1.03,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = "FiraSans",
      size = 18,
      color = theme_muted,
      lineheight = 1.35,
      margin = margin(b = 14)
    ),
    plot.caption = element_markdown(
      family = "FiraSansRegular",
      size = 9,
      color = theme_muted,
      lineheight = 1.4,
      margin = margin(t = 16)
    ),
    plot.margin = margin(22, 26, 18, 26)
  )

output_path <- "23_seasons_radial_stacked.png"
ggsave(output_path, polar_plot, width = 12.6, height = 12.6, dpi = 300, bg = night_owlish_light$bg)
