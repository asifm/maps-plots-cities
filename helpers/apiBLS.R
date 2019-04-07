# BLS API key saved in .Renviron

library(blsAPI)
# https://rpubs.com/josezuniga/232013

library(blscrapeR)
# https://github.com/keberwein/blscrapeR


df <- bls_api(c("LNS12000000", "LNS13000000", "LNS14000000"),
              startyear = 2008, endyear = 2017, Sys.getenv("BLS_KEY")) %>%
  # Add time-series dates
  dateCast()
qplot(x = date, y = value, data = df, facets = ~periodName) +
  geom_line() +
  theme_economist()
