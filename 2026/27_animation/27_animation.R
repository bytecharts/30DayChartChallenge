suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(scales)
  library(gifski)
})

source("../utils/theme.R")

state_files <- tibble::tribble(
  ~state, ~file,
  "California", "930-data-export-california.csv",
  "New York", "930-data-export-new-york.csv",
  "Florida", "930-data-export-Florida.csv",
  "Texas", "930-data-export-Texas.csv"
)

parse_hour_ending <- function(x) {
  cleaned <- x
  cleaned <- gsub(" (EDT|EST|CDT|CST)$", "", cleaned)
  cleaned <- gsub("a\\.m\\.", "AM", cleaned)
  cleaned <- gsub("p\\.m\\.", "PM", cleaned)
  as.POSIXct(cleaned, format = "%m/%d/%Y %I %p", tz = "UTC")
}

read_state <- function(state, file) {
  raw <- readr::read_csv(file, show_col_types = FALSE)
  names(raw) <- trimws(names(raw))

  ts_col <- grep("^Timestamp", names(raw), value = TRUE)[1]
  demand_col <- grep("^Demand \\(MWh\\)$", names(raw), value = TRUE)[1]

  if (is.na(ts_col) || is.na(demand_col)) {
    stop("Missing expected columns in ", file)
  }

  raw %>%
    transmute(
      state = state,
      timestamp = parse_hour_ending(.data[[ts_col]]),
      demand_mwh = as.numeric(.data[[demand_col]])
    ) %>%
    filter(!is.na(timestamp), !is.na(demand_mwh)) %>%
    mutate(
      date = as.Date(timestamp),
      hour = as.integer(format(timestamp, "%H"))
    )
}

demand_hourly <- purrr::map2_dfr(state_files$state, state_files$file, read_state)

date_bounds <- demand_hourly %>%
  group_by(state) %>%
  summarise(max_date = max(date), .groups = "drop")

complete_state_days <- demand_hourly %>%
  group_by(state, date) %>%
  summarise(
    n_hours = n_distinct(hour),
    has_all_hours = all(0:23 %in% hour),
    .groups = "drop"
  ) %>%
  filter(n_hours == 24, has_all_hours)

common_complete_days <- complete_state_days %>%
  group_by(date) %>%
  summarise(states_complete = n_distinct(state), .groups = "drop") %>%
  filter(states_complete == nrow(state_files)) %>%
  arrange(date)

if (nrow(common_complete_days) == 0) {
  stop("No full common days found across all 4 states.")
}

date_window <- tail(common_complete_days$date, 30)
start_date <- min(date_window)
common_end <- max(date_window)

full_grid <- tidyr::expand_grid(
  state = state_files$state,
  date = date_window,
  hour = 0:23
)

plot_data <- full_grid %>%
  left_join(
    demand_hourly %>% select(state, date, hour, demand_mwh),
    by = c("state", "date", "hour")
  ) %>%
  mutate(day_index = match(date, date_window))

demand_limits <- range(plot_data$demand_mwh, na.rm = TRUE)
n_days <- length(date_window)
y_breaks <- unique(round(c(1, n_days / 3, 2 * n_days / 3, n_days)))
y_labels <- as.character(y_breaks)

frames_dir <- "state_polar_frames"
gif_path <- "27_animation.gif"

if (!dir.exists(frames_dir)) {
  dir.create(frames_dir, recursive = TRUE)
}

frame_paths <- file.path(frames_dir, sprintf("polar_%s.png", format(date_window, "%Y%m%d")))

plot_caption <- caption_global(
  source = "U.S. EIA balancing-authority hourly demand exports.",
  day = "Day 27",
  topic = "Animation"
)

fill_palette <- c(
  "#12314B",
  "#1E5E85",
  "#35A0A8",
  "#8FD6A7",
  "#F7D26A",
  "#F39C4A",
  "#D75B43"
)

for (i in seq_along(date_window)) {
  current_date <- date_window[i]

  frame_df <- plot_data %>%
    mutate(demand_frame = if_else(date <= current_date, demand_mwh, NA_real_))

  subtitle_text <- paste0(
    "Real hourly demand across last ", length(date_window), " full days.",
    "<br>Inner ring oldest ", format(start_date, "%d %b %Y"),
    " | Outer ring latest ", format(common_end, "%d %b %Y"),
    " | Animated to: ", format(current_date, "%d %b %Y")
  )

  p <- ggplot(frame_df, aes(x = hour, y = day_index, fill = demand_frame)) +
    geom_tile(width = 1, height = 0.95) +
    facet_wrap(~state, ncol = 2) +
    coord_polar(theta = "x", start = -pi / 24, clip = "off") +
    scale_x_continuous(
      limits = c(-0.5, 23.5),
      breaks = NULL,
      labels = NULL,
      minor_breaks = NULL,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0.5, n_days + 0.5),
      breaks = NULL,
      labels = NULL,
      minor_breaks = NULL,
      expand = c(0, 0)
    ) +
    scale_fill_gradientn(
      colours = fill_palette,
      limits = demand_limits,
      labels = label_number(accuracy = 1),
      name = "Demand (MWh)",
      na.value = alpha("#E4E9F0", 0.45),
      guide = guide_colorbar(
        direction = "vertical",
        title.position = "top",
        barheight = grid::unit(120, "pt"),
        barwidth = grid::unit(10, "pt")
      )
    ) +
    labs(
      title = "State Electricity Demand Rhythms in 30-Day Polar Rings",
      subtitle = subtitle_text,
      x = NULL,
      y = NULL,
      caption = plot_caption
    ) +
    theme_base() +
    theme(
      plot.background = element_rect(fill = night_owlish_light$bg, color = NA),
      panel.background = element_rect(fill = night_owlish_light$bg_alt, color = NA),
      plot.title = element_markdown(color = night_owlish_light$fg, hjust =0.5, size=28),
      plot.subtitle = element_markdown(color = night_owlish_light$fg_soft, hjust = 0.5, size = 10),
      plot.caption = element_markdown(color = night_owlish_light$fg_soft, hjust = 0.5),
      strip.background = element_rect(fill = alpha("#FFFFFF", 0.78), color = NA),
      strip.text = element_text(
        family = "SpaceGrotesk",
        face = "bold",
        size = 14,
        color = night_owlish_light$fg
      ),
      axis.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_line(color = alpha(night_owlish_light$gray, 0.18), linewidth = 0.18),
      panel.grid.major.x = element_line(color = alpha(night_owlish_light$gray, 0.14), linewidth = 0.16),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.direction = "horizontal",
      legend.background = element_rect(fill = alpha("#FFFFFF", 0.84), color = NA),
      legend.title = element_text(size = 10, color = night_owlish_light$fg, hjust = 0.5, vjust = 1),
      legend.text = element_text(size = 8, color = night_owlish_light$fg_soft, hjust = 0.5),
      panel.spacing = grid::unit(12, "pt"),
plot.margin = margin(15, 40, 15, 40)
    )
 ggsave(
  filename = frame_paths[i],
  plot = p,
  width = 14,
  height = 7.875,
  dpi = 320,
  bg = night_owlish_light$bg
)
}

frames_for_gif <- c(
  rep(frame_paths[1], 3),
  rep(frame_paths, each = 2),
  rep(frame_paths[length(frame_paths)], 9)
)



gifski::gifski(
  png_files = frames_for_gif,
  gif_file = gif_path,
  width = 1920,
  height = 1080,
  delay = 0.2,
  loop = TRUE
)

message("Saved GIF to: ", gif_path)
