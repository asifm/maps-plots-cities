# =============================================
# Load frequently used packages
# =============================================
# Load all these through tidyverse
# v ggplot2 3.1.0       v purrr   0.3.2
# v tibble  2.1.1       v dplyr   0.8.0.1
# v tidyr   0.8.3       v stringr 1.4.0
# v readr   1.3.1       v forcats 0.4.0
library(tidyverse)

# furrr makes purrr run in parallel processes, making calculations much faster
# library(furrr)
# future::plan(multiprocess)

#  ---------------------------------------------
# Visualization
# ---------------------------------------------
library(highcharter)
library(scales)
# allows control over multi-plot layout
library(cowplot)
# lots of extra themes
library(ggthemes)
# allows using system fonts
library(extrafont)
library(animation)

# =============================================
# Customize Theme
# =============================================

theme_custom_econ <- theme_economist(base_family = "Open Sans") +
  theme(
    panel.grid.major.x = element_line(colour = "white", size = rel(0.75)),
    panel.grid.major.y = element_line(colour = "white", size = rel(0.75))
  )

# For all graphs
theme_set(theme_custom_econ)

# For drawing maps
theme_custom_minimal <- theme_minimal(base_family = "Open Sans") +
  theme(
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_blank()
  )
# theme_set(theme_custom_minimal)

# When doing facet wraps; this allows enough padding
theme_custom_facet <-
  theme(
    plot.background = element_rect(fill = "#F6E481"),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "steelblue"),
    panel.spacing = unit(1, "lines")
  )
