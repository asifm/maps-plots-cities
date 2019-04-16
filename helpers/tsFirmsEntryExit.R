# =============================================
# Explores Business Dynamics Statistics data to answer these questions:
#
#
#
#

# Author: Asif Mehedi
# Created on: 20190405

# Data documentation
# https://www.census.gov/data/developers/data-sets/business-dynamics.html
# https://www.census.gov/ces/pdf/BDS_2014_Codebook.pdf
# =============================================


# =============================================
# Initialize
# =============================================
# Loads tidyverse and other packages
source(here::here('helpers/setup.R'))

library(entrydatar)
# https://github.com/eloualiche/entrydatar
# https://github.com/eloualiche/entrydatar/blob/master/vignettes/bds.Rmd

firm_age_msa <- get_bds_cut(1977, 2014, "firm", "agemsa")
firm_age_msa %>% as_tibble
firm_age_msa$fage4 = as_factor(firm_age_msa$fage4)
glimpse(firm_age_msa)

get_msacode_fromregex("Austin")

firm_age_msa %>% 
  filter((msa == 12420) | (msa == 19820), fage4 == "b) 1") %>% 
  # group_by(msa, year, fage4) %>% 
  # summarise(total_emp = sum(Emp)) %>% 
  ggplot(aes(x = year, y = Estabs_Entry_Rate)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  facet_wrap(~msa)


firm_age_all <- get_bds_cut(aggregation = "age")
firm_age_all %>% as_tibble()

firm_age_all %>%
  filter(year > 1985, fage4 != "l) Left Censored") %>%
  group_by(year, fage4)%>%
  summarize(total_death_emp = sum(firmdeath_emp, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(
    aes(x = year, y = total_death_emp),
    fill = "steelblue",
    color = "black",
    width = 0.1
  ) +
  facet_wrap( ~ fage4, ncol = 4) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_rect(fill = NA, color = "red"))

firm_size_all = get_bds_cut(aggregation = "sz") %>% as_tibble()

firm_size_all %>% 
  group_by(year, fsize)%>%
  summarize(total = sum(Job_Creation_Continuers, na.rm = TRUE)) %>%
  ggplot() +
  geom_area(
    aes(x = year, y = total),
    fill = "steelblue",
    color = "black",
    width = 0.1
  ) +
  facet_wrap( ~ fsize, ncol = 2)
  
  theme_custom_facet

firm_age_msa %>% 
  filter((msa == 12420) | (msa == 19820) | (msa == 16820)) %>% 
  group_by(year, msa) %>% 
  summarize(total = sum(Job_Creation_Births, na.rm = TRUE)) %>% 
  ggplot() +
  geom_col(aes(x = year, y = total), fill = "steelblue", color = "black", width = 0.1) +
  facet_wrap(~msa, scales = "free_y", ncol = 1)


firm_size_all
