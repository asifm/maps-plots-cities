# Helper function to draw plots based on acs and decennial census data
# Author: Asif Mehedi
# Created: 20190331l

# =============================================
# Setup
# =============================================
source(here::here("helpers/setup.R"))

plot_acsestimate_withmoe <-
  function(acsdata_df,
           title = NULL,
           survey_year = NULL) {
    acsdata_df %>%
      ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
      geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe), alpha = 0.3) +
      geom_point(color = "red", size = 3) +
      labs(
        title = title,
        subtitle = str_glue("American Community Survey ", survey_year),
        y = "",
        x = "ACS estimate (bars represent margin of error)"
      ) +
      theme_minimal(base_family = "IBM Plex Sans") +
      # geom_rangeframe(size = 3, color = "blue", alpha = 0.1, sides = 'b') +
      theme(
        axis.text.y = element_text(color = "#030303"),
        plot.title = element_text(face = "bold", color = "#003366"),
        plot.subtitle = element_text(color = "#121212", face = "italic")
      )
  }

# test
# 
# cocapci <-
#   get_acsdata_counties_fromstates(variables = "B19301_001", state = c("VA"))
# 
# cocapci %>%
#   top_n(20, estimate) %>%
#   plot_acsestimate_withmoe(title = "Per Capita Income", survey_year = "2013–17")

