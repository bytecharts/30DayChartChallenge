# ---- Packages ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(showtext)
  library(MetBrewer)
  library(ggtext)
  library(sysfonts)
  library(scales)
})

# ---- Fonts ----
# Get the current working directory (where your R script is)

#font_add_google("Space Grotesk", "SpaceGrotesk")
#font_add_google("Fira Code", "FiraCode")

font_add("SpaceGrotesk", "../utils/fonts/SpaceGrotesk.ttf")
font_add("FiraSans", "../utils/fonts/FiraSans-Medium.ttf")
font_add("FiraSansRegular", "../utils/fonts/FiraSans-Regular.ttf")
font_add("FiraCode", "../utils/fonts/FiraCode-Medium.ttf")
font_add("FiraCodeMedium", "../utils/fonts/FiraCode-Medium.ttf")
font_add("NotoEmoji",  "../utils/fonts/NotoEmoji.ttf")
showtext_auto()
showtext_opts(dpi = 300)

# ---- Base colors ----
night_owlish_light <- list(
  bg = "#FBFBFB",
  bg_alt = "#F2F4F8",
  bg_soft = "#E8ECF2",
  fg = "#2A2F3A",
  fg_soft = "#5A6270",
  gray = "#8A93A6"
)


night_owlish_cat <- c(
  "#2E86AB", # blue
  "#4A6984",
  "#F07178", # red
  "#FFB454", # orange
  "#7FDBCA", # aqua/green
  "#6C5CE7", # purple (keep only one blue-adjacent)
  "#8A93A6"  # neutral gray (replacement for extra blue)
)

base_colors <- list(
  primary   = "#2A2F3A",
  secondary = "#2E86AB",
  light     = "#F2F4F8",
  canvas    = "#FBFBFB",
  bg        = "#FBFBFB",
  neutral   = "#8A93A6"
)

theme_bg <- night_owlish_light$bg_soft
theme_fg <- base_colors$primary
theme_muted <- "gray20"
theme_title_family <- "SpaceGrotesk"
theme_caption_family <- "FiraSansRegular"

# ---- Plot Theme ----
theme_base <- function(base_size = 12, base_family = "FiraSans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA),
      plot.title = element_markdown(
        family = theme_title_family,
        face = "bold",
        size = 32,
        color = theme_fg,
        hjust = 0,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_markdown(
        size = 18,
        family = "FiraSans",
        hjust = 0,
        color = theme_muted,
        margin = margin(b = 25),
        lineheight = 1.5
      ),
      axis.title.x = element_text(
        size = 12,
        color = theme_fg,
        margin = margin(t = 10)
      ),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 11, color = theme_fg),
      axis.text.y = element_text(size = 11, color = theme_fg),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.caption = element_markdown(
        size = 9,
        color = theme_muted,
        hjust = 0,
        family = theme_caption_family,
        lineheight = 1.5,
        margin = margin(t = 15)
      ),
      plot.margin = margin(30, 10, 30, 10),
      legend.title = element_text(
        size = 12,
        color = theme_muted,
        margin = margin(r = 10)
      ),
      legend.text = element_text(
        size = 10,
        color = theme_muted,
        margin = margin(r = 10)
      )
    )

}
# ---- Plot Caption ----
caption_global <- function(source, day, topic) {
  paste(
        "<span style='font-family: FiraSans;'><b>Viz</b></span> : Byte Charts | <span style='font-family: FiraSans;'>Source</span> : ", source,
        paste0("<br>#30DayChartChallenge 2026 \u2022 ", day, " \u2022 ", topic, "<br>",
               "<img src='", "../utils/bluesky.png", "' width='7' style='vertical-align:bottom;'/>  byte-charts&nbsp;&nbsp;",
               "   <img src='", "../utils/github.png", "' width='7'  style='vertical-align:bottom;'/>  byte-charts&nbsp;&nbsp;"
               ),
        sep = "\n"
  )
}
# ---- Plot Caption ----
caption_global_dark <- function(source, day, topic) {
  paste(
        "<span style='font-family: FiraCodeMedium;'>Viz</span>: Byte Charts | ", source,
        paste0("<br>#30DayChartChallenge 2026 \u2022 ", day, " \u2022 ", topic, "<br>",
               "<img src='", "../utils/bluesky_dark.png", "' width='7' style='vertical-align:bottom;'/>  byte-charts&nbsp;&nbsp;",
               "   <img src='", "../utils/github_dark.png", "' width='7'  style='vertical-align:bottom;'/>  byte-charts&nbsp;&nbsp;"
               ),
        sep = "\n"
  )
}


caption_general <- function(source) {
  paste(
        "<span style='font-family: FiraCodeMedium;'>Viz</span>: Byte Charts | ", source,
        paste0( "<img src='", "../utils/bluesky.png", "' width='7' style='vertical-align:bottom;'/>  byte-charts&nbsp;&nbsp;",
               "   <img src='", "../utils/github.png", "' width='7'  style='vertical-align:bottom;'/>  byte-charts&nbsp;&nbsp;"
               ),
        sep = "\n"
  )
}
