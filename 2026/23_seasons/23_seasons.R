# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(jsonlite)
  library(ggtext)
  library(scales)
  library(stringr)
})

# ---- Source Theme ----
source("../utils/theme.R")

# ---- Load Data ----
stats_path <- "stats/season_stats.json"
if (!file.exists(stats_path)) {
  stop("stats/season_stats.json not found. Run parse_dialogue.py on the data folder first.")
}

stats_raw <- fromJSON(stats_path, simplifyDataFrame = FALSE)
if (length(stats_raw$seasons) == 0) {
  stop("No season data found in season_stats.json")
}

# ---- Flatten Nested JSON ----
season_rows <- lapply(stats_raw$seasons, function(season_entry) {
  season_name <- season_entry$season
  season_num <- as.integer(str_extract(season_name, "[0-9]+"))

  if (length(season_entry$characters) == 0) {
    return(NULL)
  }

  bind_rows(lapply(season_entry$characters, function(ch) {
    data.frame(
      season = season_name,
      season_num = season_num,
      character_raw = as.character(ch$character),
      words = as.numeric(ch$words),
      stringsAsFactors = FALSE
    )
  }))
})

character_stats <- bind_rows(season_rows)
if (nrow(character_stats) == 0) {
  stop("No character rows found in season_stats.json")
}

# ---- Character Cleanup ----
alias_map <- c(
  "SIOBHAN" = "SHIV",
  "SHIV ROY" = "SHIV",
  "KENDALL ROY" = "KENDALL",
  "ROMAN ROY" = "ROMAN",
  "LOGAN ROY" = "LOGAN",
  "CONNOR ROY" = "CONNOR",
  "TOM WAMBSGANS" = "TOM",
  "TOM WAMSGANS" = "TOM",
  "GREG HIRSCH" = "GREG",
  "MARCIA ROY" = "MARCIA",
  "RAVA ROY" = "RAVA",
  "GERRI KELLMAN" = "GERRI",
  "FRANK VERNON" = "FRANK",
  "KARL MULLER" = "KARL",
  "KAROLINA NOVOTNEY" = "KAROLINA",
  "HUGO BAKER" = "HUGO",
  "WILLA FERREYRA" = "WILLA",
  "LUKAS MATSSON" = "LUKAS",
  "STEWY HOSSEINI" = "STEWY",
  "NATE SOFRELLI" = "NATE",
  "RHEA JARRELL" = "RHEA",
  "CAROLINE COLLINGWOOD" = "CAROLINE",
  "CYD PEACH" = "CYD",
  "GIL EAVIS" = "GIL",
  "JESS JORDAN" = "JESS"
)

cleanup_character <- function(x) {
  value <- toupper(trimws(x))
  value <- gsub("\\s+", " ", value)
  ifelse(value %in% names(alias_map), alias_map[value], value)
}

non_character_regex <- paste(
  c(
    "^ALL$",
    "ANCHOR",
    "NEWSCASTER",
    "NARRATOR",
    "^WOMAN$",
    "^MAN$",
    "^BOY$",
    "^GIRL$",
    "ON TV",
    "ON VIDEO",
    "ON PHONE",
    "SERVER",
    "SOUND TECH"
  ),
  collapse = "|"
)

cleaned_stats <- character_stats %>%
  mutate(character = cleanup_character(character_raw)) %>%
  filter(!str_detect(character, non_character_regex)) %>%
  group_by(season_num, season, character) %>%
  summarise(words = sum(words), .groups = "drop")

if (nrow(cleaned_stats) == 0) {
  stop("No rows left after cleanup.")
}

# ---- Rank by Season ----
ranked_stats <- cleaned_stats %>%
  group_by(season_num, season) %>%
  arrange(desc(words), character, .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# ---- Choose Characters For Bump Chart ----
# Keep only characters that survive all 4 seasons, then rerank within each season.
season_characters <- ranked_stats %>%
  distinct(season_num, character)

featured_chars <- Reduce(
  intersect,
  split(season_characters$character, season_characters$season_num)
)
featured_chars <- sort(featured_chars)

bump_data <- ranked_stats %>%
  filter(character %in% featured_chars) %>%
  group_by(season_num, season) %>%
  arrange(desc(words), character, .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

character_priority <- bump_data %>%
  summarise(total_words = sum(words), .by = character) %>%
  arrange(desc(total_words), character)

color_chars <- head(character_priority$character, 8)

bump_data <- bump_data %>%
  mutate(color_group = if_else(character %in% color_chars, character, "Others"))

season_lookup <- ranked_stats %>%
  distinct(season_num, season) %>%
  arrange(season_num)

first_labels <- bump_data %>%
  group_by(character) %>%
  slice_min(season_num, n = 1, with_ties = FALSE) %>%
  ungroup()

last_labels <- bump_data %>%
  group_by(character) %>%
  slice_max(season_num, n = 1, with_ties = FALSE) %>%
  ungroup()

# ---- Missing-Name Episode Note ----
missing_episode_rows <- bind_rows(lapply(stats_raw$episodes_without_character_name, function(x) {
  if (length(x$episodes) == 0) {
    return(NULL)
  }

  data.frame(
    season = x$season,
    episode = unlist(x$episodes),
    stringsAsFactors = FALSE
  )
}))

missing_note <- if (nrow(missing_episode_rows) > 0) {
  short_names <- missing_episode_rows %>%
    mutate(short_episode = str_replace(episode, ".*/", "")) %>%
    pull(short_episode)

  paste0("Episodes without speaker cues: ", paste(short_names, collapse = ", "), ".")
} else {
  "All episodes include speaker cues."
}

# ---- Build Bump Chart ----
title_text <- "Who speaks the most in Succession?"
subtitle_text <- paste0(
 "Only characters that appear across all four seasons are included. "
)

plot_caption <- caption_global(
 source <- "springfieldspringfield.co.uk; Words counted from cleaned, speaker-attributed subtitle text across all seasons (excluding S1E07, S2E05, S3E02, S4E01, S4E06).",
  day = "Day 23",
  topic = "Seasons"
)

rank_breaks <- sort(unique(bump_data$rank))

palette_values <- c(
  setNames(
    alpha(
      rep(night_owlish_cat, length.out = length(color_chars)),
      seq(1, 0.45, length.out = length(color_chars))
    ),
    color_chars
  ),
  Others = night_owlish_light$gray
)

bump_plot <- ggplot(
  bump_data,
  aes(x = season_num, y = rank, group = character, color = color_group)
) +
  geom_line(linewidth = 1.05, lineend = "round") +
  geom_point(size = 2.4) +
  geom_text(
    data = first_labels,
    aes(label = character),
    hjust = 1.08,
    size = 3.2,
    family = "FiraSans",
    show.legend = FALSE
  ) +
  geom_text(
    data = last_labels,
    aes(label = character),
    hjust = -0.08,
    size = 3.2,
    family = "FiraSans",
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = season_lookup$season_num,
    labels = season_lookup$season,
    expand = expansion(mult = c(0.18, 0.2))
  ) +
  scale_y_reverse(
    breaks = rank_breaks,
    labels = rank_breaks,
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_color_manual(values = palette_values, breaks = c(color_chars, "Others"), name = "Character") +
  coord_cartesian(clip = "off") +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    x = NULL,
    y = "Rank by words spoken",
    caption = plot_caption,
    color = "Character"
  ) +
  theme_base() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.y = element_line(color = alpha(night_owlish_light$gray, 0.24), linewidth = 0.28),
    panel.grid.major.x = element_line(color = alpha(night_owlish_light$gray, 0.14), linewidth = 0.25),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(size = 10, color = theme_fg),
    axis.text.y = element_text(size = 9, color = night_owlish_light$fg_soft),
    axis.title.y = element_text(angle=90, size = 11, color = theme_fg, margin = margin(r = 10)),
    plot.title = element_markdown(
      size = 30,
      family = theme_title_family,
      face = "bold",
      color = theme_fg,
      lineheight = 1.04
    ),
    plot.subtitle = element_markdown(
      size = 12.5,
      family = "FiraSans",
      color = theme_muted,
      lineheight = 1.35,
      margin = margin(b = 14)
    ),
    plot.caption = element_markdown(
      size = 8.5,
      family = "FiraSansRegular",
      color = theme_muted,
      hjust = 0,
      lineheight = 1.45
    ),
    plot.margin = margin(24, 48, 20, 48)
  )

# ---- Save Plot ----
output_path <- "23_seasons_bump.png"
ggsave(output_path, bump_plot, width = 12, height = 9, dpi = 320, bg = "#FFFFFF")

# ---- Profanity Bar Chart (Top 10 Speakers) ----
if (!dir.exists("data") || length(list.files("data", pattern = "\\.html$", recursive = TRUE, full.names = TRUE)) == 0) {
  message("No transcript HTML files found under ./data; skipping profanity bar chart.")
  quit(save = "no")
}

top10_speakers <- ranked_stats %>%
  group_by(character) %>%
  summarise(total_words = sum(words), .groups = "drop") %>%
  arrange(desc(total_words), character) %>%
  slice_head(n = 10)

html_to_text <- function(raw) {
  text <- str_replace_all(raw, regex("<br\\s*/?>", ignore_case = TRUE), "\n")
  text <- str_replace_all(text, "<[^>]+>", " ")
  text <- str_replace_all(text, fixed("&nbsp;"), " ")
  text <- str_replace_all(text, fixed("&amp;"), "&")
  text <- str_replace_all(text, fixed("&quot;"), "\"")
  text <- str_replace_all(text, fixed("&#39;"), "'")
  text
}

speaker_cue_pattern <- "(?<![A-Za-z])([A-Z][A-Z]+(?:\\s+[A-Z][A-Z]+)*)(?:\\s*\\([^)]*\\))?:\\s*"
curse_token_pattern <- "^(fuck(?:er|ers|ed|ing|in|s)?|shit(?:ty|show|s)?|bullshit|asshole(?:s)?|bitch(?:es)?|bastard(?:s)?|dick(?:head|heads)?|motherfuck(?:er|ers|ing)?|cunt(?:s)?|prick(?:s)?|pussy)$"

count_curses_in_file <- function(file_path) {
  raw <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  text <- html_to_text(raw)

  cue_locs <- str_locate_all(text, regex(speaker_cue_pattern))[[1]]
  if (nrow(cue_locs) == 0) {
    return(data.frame(character = character(), words = integer(), curses = integer(), stringsAsFactors = FALSE))
  }

  cue_matches <- str_match_all(text, regex(speaker_cue_pattern))[[1]]
  speakers <- cue_matches[, 2]
  text_len <- str_length(text)
  rows <- vector("list", nrow(cue_locs))

  for (i in seq_len(nrow(cue_locs))) {
    speaker <- cleanup_character(speakers[i])
    if (str_detect(speaker, non_character_regex)) {
      next
    }

    start_idx <- cue_locs[i, 2] + 1
    end_idx <- if (i < nrow(cue_locs)) cue_locs[i + 1, 1] - 1 else text_len
    if (end_idx < start_idx) {
      next
    }

    segment <- str_sub(text, start = start_idx, end = end_idx)
    segment <- str_replace_all(segment, "\\([^)]*\\)", " ")
    segment <- str_replace_all(segment, "-", " ")
    segment <- str_squish(segment)

    tokens <- str_extract_all(str_to_lower(segment), "[a-z]+(?:'[a-z]+)?")[[1]]
    if (length(tokens) == 0) {
      next
    }

    rows[[i]] <- data.frame(
      character = speaker,
      words = length(tokens),
      curses = sum(str_detect(tokens, regex(curse_token_pattern))),
      stringsAsFactors = FALSE
    )
  }

  bind_rows(rows)
}

transcript_files <- list.files("data", pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
if (length(transcript_files) == 0) {
  stop("No transcript HTML files found under ./data")
}

curse_counts <- bind_rows(lapply(transcript_files, count_curses_in_file)) %>%
  group_by(character) %>%
  summarise(words = sum(words), curses = sum(curses), .groups = "drop") %>%
  mutate(rate_per_1k = if_else(words > 0, (curses / words) * 1000, 0))

top10_curse <- top10_speakers %>%
  select(character) %>%
  left_join(curse_counts, by = "character") %>%
  mutate(
    words = coalesce(words, 0),
    curses = coalesce(curses, 0),
    rate_per_1k = coalesce(rate_per_1k, 0)
  ) %>%
  arrange(desc(curses), desc(rate_per_1k), character) %>%
  mutate(
    character = factor(character, levels = rev(character)),
    highlight = if_else(row_number() == 1, "Most", "Others"),
    label = paste0(comma(curses), " (", number(rate_per_1k, accuracy = 0.1), "/1k)")
  )

bar_title <- "Who curses most among top 10 speakers?"
bar_subtitle <- paste0(
  "Characters are top-10 by total spoken words across seasons. Bars show profanity count; labels show rate per 1,000 words. ",
  "Most frequent curser: ",
  as.character(top10_curse$character[nrow(top10_curse)]),
  "."
)

bar_caption <- caption_global(
  source = paste0(
    "Curse words counted from cleaned speaker-attributed subtitle text across all seasons. ",
    missing_note
  ),
  day = "Day 23",
  topic = "Seasons"
)

curse_bar_plot <- ggplot(top10_curse, aes(x = character, y = curses, fill = highlight)) +
  geom_col(width = 0.72, alpha = 0.95) +
  geom_text(
    aes(label = label),
    hjust = -0.06,
    family = "FiraSans",
    size = 3.2,
    color = theme_fg
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("Most" = night_owlish_cat[2], "Others" = night_owlish_cat[1])) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = bar_title,
    subtitle = bar_subtitle,
    x = NULL,
    y = "Profanity count",
    caption = bar_caption,
    fill = NULL
  ) +
  theme_base() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = alpha(night_owlish_light$gray, 0.22), linewidth = 0.28),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 10, color = theme_fg),
    axis.text.x = element_text(size = 9.5, color = night_owlish_light$fg_soft),
    axis.title.x = element_text(size = 11, color = theme_fg, margin = margin(t = 10)),
    plot.title = element_markdown(size = 30, family = theme_title_family, face = "bold", color = theme_fg, lineheight = 1.05),
    plot.subtitle = element_markdown(size = 12.5, family = "FiraSans", color = theme_muted, lineheight = 1.35, margin = margin(b = 12)),
    plot.caption = element_markdown(size = 8.5, family = "FiraSansRegular", color = theme_muted, hjust = 0, lineheight = 1.45),
    plot.margin = margin(24, 42, 20, 24)
  )

bar_output_path <- "23_seasons_curses_top10.png"
ggsave(bar_output_path, curse_bar_plot, width = 14, height = 8, dpi = 320, bg = "#FFFFFF")
