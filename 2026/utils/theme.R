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
font_add("FiraCode", "../utils/fonts/FiraCode-Regular.ttf")
font_add("FiraCodeMedium", "../utils/fonts/FiraCode-Medium.ttf")
showtext_auto()
showtext_opts(dpi = 300)

# ---- Base colors ----
night_owlish_light <- list(
  bg        = "#FBFBFB",  # clean light background
  bg_alt    = "#F2F4F8",
  bg_soft   = "#E8ECF2",

  fg        = "#2A2F3A",  # primary text (cool dark gray)
  fg_soft   = "#5A6270",

  gray      = "#8A93A6"
)


night_owlish_cat <- c(
  "#2E86AB", # blue
  "#F07178", # red
  "#FFB454", # orange
  "#7FDBCA", # aqua/green
  "#6C5CE7", # purple (keep only one blue-adjacent)
  "#8A93A6"  # neutral gray (replacement for extra blue)
)

base_colors <- list(
  primary   = "#2A2F3A",
  secondary = "#2E86AB",  # main accent
  light     = "#F2F4F8",
  canvas    = "#FBFBFB",
  bg        = "#FBFBFB",
  neutral   = "#8A93A6"
)
# ---- Plot Theme ----
theme_base <- function(base_size = 12, base_family = "FiraCode") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
     plot.background  = element_rect(fill = "#FBFBFB", color = NA),
      panel.background = element_rect(fill = "#FBFBFB", color = NA),
          # Titles
          plot.title = element_text(
                                    family = "SpaceGrotesk",
                                    face   = "bold",
                                    size   = 24,
                                    color  = base_colors$primary,
                                    hjust  = 0,
                                    margin = margin(b = 6)
                                    ),
          plot.subtitle = element_text(
                                       size   = 14,
                                       hjust  = 0,
                                       color  = "gray20",
                                       margin = margin(b = 25)
                                       ),

          # Axis - Titles

          axis.title.x = element_text(
                                      size = 12,
 color = "#2A2F3A",
                                      margin = margin(t = 10)   # space ABOVE x-axis title
                                      ),

          axis.title.y = element_blank(),

          # Axis - Text
          axis.text.x = element_text(size = 10,
 color = "#2A2F3A"
                                     ),
          axis.text.y = element_text(size = 10,
 color = "#2A2F3A"
                                     ),

          # Grid
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),

          # Caption
          plot.caption = element_markdown(
                                          size = 8,
                                          color = "gray20",
                                          hjust = 0,
                                          family  = "FiraCode",
                                          lineheight = 1.5,
                                          margin = margin(t=15)
                                          ),
          # Margins
          plot.margin = margin(30, 10, 30, 10),
          legend.title = element_text(
                                     size = 10,
                                     color = "gray20",
                                     margin = margin(r = 10)   # space to the RIGHT of y-axis title
          ),
           legend.text = element_text(
                                     size = 8,
                                     color = "gray20",
                                     margin = margin(r = 10)   # space to the RIGHT of y-axis title
          )
    )

}
# ---- Plot Caption ----
caption_global <- function(source, day, topic) {
  paste(
        "<span style='font-family: FiraCodeMedium;'>Viz</span>: Byte Charts | ", source,
        paste0("<br>#30DayChartChallenge 2026 \u2022 ", day, " \u2022 ", topic, "<br>",
               "<img src='", "../utils/bluesky.png", "' width='7' style='vertical-align:bottom;'/>  byte-charts&nbsp;&nbsp;",
               "   <img src='", "../utils/github.png", "' width='7'  style='vertical-align:bottom;'/>  byte-charts&nbsp;&nbsp;"
               ),
        sep = "\n"
  )
}
