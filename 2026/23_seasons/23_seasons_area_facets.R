# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(geomtextpath)
  library(ggpattern)
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

target_chars <- c("ROMAN", "KENDALL", "SHIV", "CONNOR", "LOGAN")
character_names <- c(
  ROMAN = "Roman",
  KENDALL = "Kendall",
  SHIV = "Shiv",
  CONNOR = "Connor",
  LOGAN = "Logan"
)

season_character_totals <- character_stats |>
  filter(character %in% target_chars) |>
  summarise(fucks = sum(fucks), .by = c(character, season_num)) |>
  right_join(
    expand.grid(
      character = target_chars,
      season_num = c(1, 4),
      stringsAsFactors = FALSE
    ),
    by = c("character", "season_num")
  ) |>
  mutate(
    fucks = coalesce(fucks, 0),
    season_label = paste0("Season ", season_num),
    character = factor(character, levels = target_chars)
  )

if (nrow(season_character_totals) == 0) {
  stop("No season totals computed for area chart.")
}

ratio_labels <- season_character_totals |>
  summarise(
    s1 = fucks[season_num == 1],
    s4 = fucks[season_num == 4],
    .by = character
  ) |>
  mutate(
    ratio = if_else(s1 > 0, s4 / s1, NA_real_),
    ratio_label = if_else(is.na(ratio), "n/a", paste0(number(ratio, accuracy = 0.1), "x"))
  )

ratio_paths <- ratio_labels |>
  mutate(season_num = 1, fucks = s1) |>
  select(character, season_num, fucks, ratio_label) |>
  bind_rows(
    ratio_labels |>
      mutate(season_num = 4, fucks = s4) |>
      select(character, season_num, fucks, ratio_label)
  )

palette_values <- c(
  LOGAN = "#2E86AB",
  KENDALL = "#F07178",
  ROMAN = "#FFB454",
  SHIV = "#8B7CF0",
  CONNOR = "#5CBFA8"
)

character_labels <- setNames(
  paste0(
    "<span style='color:",
    palette_values[names(character_names)],
    ";'>",
    character_names,
    "</span>"
  ),
  names(character_names)
)

image_map <- c(
  ROMAN = "cast/Roman.jpg",
  KENDALL = "cast/Kendall.jpg",
  SHIV = "cast/Shiv.jpg",
  LOGAN = "cast/Logan.jpg",
  CONNOR = "cast/Connor.jpg"
)

missing_images <- image_map[!file.exists(image_map)]
if (length(missing_images) > 0) {
  stop("Missing image(s): ", paste(missing_images, collapse = ", "))
}

pattern_config <- data.frame(
  character = factor(target_chars, levels = target_chars),
  pattern_filename = unname(image_map[target_chars]),
  pattern_type = rep("none", length(target_chars)),
  pattern_scale = c(1.5, 5, 4.5, 3.8, 2),
  pattern_gravity = c("north", "center", "north", "north", "north"),
  pattern_xoffset = c(0, 0, 0, 0, 0),
  pattern_yoffset = c(0, 0, 0, 0, 0),
  pattern_alpha = rep(1, length(target_chars)),
  stringsAsFactors = FALSE
)

pattern_data <- season_character_totals |>
  left_join(pattern_config, by = "character") |>
  mutate(character = factor(character, levels = target_chars))

if (any(is.na(pattern_data$pattern_filename))) {
  stop("Missing pattern configuration for one or more characters.")
}

base_area_data <- season_character_totals

plot_title <- "Succession Seasons 1 to 4, the Roys Swear more"
plot_subtitle <- "F-word counts in Season 1 vs Season 4, by character. <span style='color:#8B7CF0;'>Shiv</span> shows the biggest rise."

plot_caption <- caption_general( "springfieldspringfield.co.uk (excluding S1E07, S2E05, S3E02, S4E01, S4E06).")

max_total <- max(season_character_totals$fucks, na.rm = TRUE)
label_pad <- max(6, max_total * 0.12)

area_plot <- ggplot(season_character_totals, aes(x = season_num, y = fucks)) +
  geom_area(
    data = base_area_data,
    aes(fill = character),
    alpha = 0.35
  ) +
  geom_area_pattern(
    data = pattern_data,
    aes(
      pattern_filename = pattern_filename,
      pattern_type = pattern_type,
      pattern_scale = pattern_scale,
      pattern_gravity = pattern_gravity,
      pattern_xoffset = pattern_xoffset,
      pattern_yoffset = pattern_yoffset,
      pattern_alpha = pattern_alpha
    ),
    pattern = "image",
    fill = "white",
    color = NA,
    show.legend = FALSE
  ) +
  geom_line(
    aes(color = character),
    linewidth = 1.7,
    lineend = "round"
  ) +
  geom_point(
    aes(color = character),
    size = 2.3
  ) +
  geom_text(
    aes(label = comma(fucks), color= character),
    vjust = -1.75,
    hjust = 0.75,
    family = "FiraSans",
    size = 5
  ) +
  geom_textline(
    data = ratio_paths,
    aes(x = season_num, y = fucks, label = ratio_label, color = character),
    family = "FiraSans",
    vjust = -0.15,
    size = 7.6,
    fontface = "bold",
    text_only = TRUE,
    upright = TRUE,
    linewidth = 0,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = c(1, 4),
    labels = c("S1", "S4"),
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.18)),
    limits = c(0, max_total + label_pad)
  ) +
  scale_fill_manual(values = palette_values) +
  scale_color_manual(values = palette_values) +
  scale_pattern_filename_identity() +
  scale_pattern_type_identity() +
  scale_pattern_scale_identity() +
  scale_pattern_gravity_identity() +
  scale_pattern_xoffset_identity() +
  scale_pattern_yoffset_identity() +
  scale_pattern_alpha_identity() +
  facet_wrap(
    ~character,
    ncol = 5,
    labeller = labeller(character = character_labels),
    strip.position = "bottom",
    drop = FALSE
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = NULL,
    y = "F-word count",
    fill = NULL,
    color = NULL,
    caption = plot_caption
  ) +
  theme_base() +
  theme(
    plot.background = element_rect(fill = night_owlish_light$bg, color = NA),
    panel.background = element_rect(fill = night_owlish_light$bg, color = NA),
    panel.grid.major.y = element_line(color = alpha(night_owlish_light$gray, 0.25), linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, color = theme_fg),
    axis.text.y = element_text(size = 9, color = night_owlish_light$fg_soft),
    axis.title.y = element_text(size = 18, color = theme_fg, margin = margin(r = 10), angle =90),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    strip.placement = "outside",
    strip.text = element_markdown(
      family = theme_title_family,
      face = "bold",
      size = 18
    ),
    strip.background = element_rect(fill = night_owlish_light$bg_alt, color = NA),
    plot.title = element_markdown(
      family = theme_title_family,
      face = "bold",
      size = 38,
      color = theme_fg,
      lineheight = 1.05,
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
      size = 11,
      color = theme_muted,
      lineheight = 1.4,
      margin = margin(t = 16)
    ),
    plot.margin = margin(22, 26, 18, 26)
  )

output_path <- "23_seasons_area_facets.png"
ggsave(output_path, area_plot, width = 16, height = 14, dpi = 320, bg = night_owlish_light$bg)
