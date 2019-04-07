# FRED API key saved in .Renviron.
# https://cran.r-project.org/web/packages/fredr/vignettes/fredr-series.html

library(fredr)

# fredr_set_key("097c3070b9b02cf3e45acf4bba815787")

fredr(frequency = "m",units = "pc1",
      series_id = "UNRATE",
      observation_start = as.Date("1920-01-01")
) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_point() +
  ylim(c(0, NA)) +
  theme_economist()

fredr_series("430")
gdpmsa <- fredr_series_search_text("GDP.*MSA")
gdpmsa['id']


fredr_series("MAGDP2") %>% 
  transpose()


