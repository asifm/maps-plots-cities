# Load frequently used packages

# Load all these through tidyverse
# v ggplot2 3.1.0       v purrr   0.3.2
# v tibble  2.1.1       v dplyr   0.8.0.1
# v tidyr   0.8.3       v stringr 1.4.0
# v readr   1.3.1       v forcats 0.4.0
library(tidyverse)

# furrr makes purrr run in parallel processes, making calculations much faster
library(furrr)
future::plan(multiprocess)


# Visualization

library(scales)
# allows control over multi-plot layout
library(cowplot)
# lots of extra themes
library(ggthemes)
# allows using system fonts
library(extrafont)

# =========
# theme_set(theme_economist(base_family = "Segoe UI"))
theme_set(theme_fivethirtyeight(base_family = "Open Sans"))

theme_custom_facet <-
  theme(
    plot.background = element_rect(fill = "#F6E481"),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "steelblue"), panel.spacing = unit(1, "lines")
  )

# ====
library(data.world)
sql_msa15 <-
  qry_sql("SELECT * FROM msa15 WHERE msa_type15 = 'Metro'")
msa15_df <- query(sql_msa15, "asifmeh/citee-geodata")

get_msacode_fromregex <- function(msa_name_regex) {
  msa15_df %>%
    filter(str_detect(msa_name15, msa_name_regex))
}
