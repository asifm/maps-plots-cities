source(here::here('helpers/setup.R'))

# data source: https://seer.cancer.gov/popdata/popdic.html
# checked that data conforms with census bureau's intercensal data
# not clear which year's county definitions were used

df <-
  read_csv(
    "C:/Users/mehedia/Downloads/us.1969_2017.19ages.adjusted.txt",
    col_names = "rawdata",
    trim_ws = TRUE,
    
  )
df2 <- df %>%
  mutate(
    year = as.integer(str_sub(rawdata, 1, 4)),
    state_abbr = str_sub(rawdata, 5, 6),
    state_code = str_sub(rawdata, 7, 8),
    county_3dcode = str_sub(rawdata, 9, 11),
    county_5dcode = str_sub(rawdata, 7, 11),
    race = str_sub(rawdata, 14, 14),
    sex = str_sub(rawdata, 16, 16),
    age_group = str_sub(rawdata, 17, 18),
    population = as.integer(str_sub(rawdata, 19, 26))
  )


#  ---------------------------------------------
# County
# ---------------------------------------------

county_pop <- df2 %>%
  select(-rawdata) %>%
  group_by(year, county_5dcode) %>%
  summarise(pop = sum(population))


# write_csv(df3, path = here::here("data_/processed/county_total-population_1969-2017.csv") )

#  ---------------------------------------------
# County and 19 Age Groups
# ---------------------------------------------


county_agegroup_population <- county_pop %>% 
  group_by(county_5dcode, age_group, year) %>% 
  summarize(agegroup_pop = sum(population))

glimpse(county_agegroup_population)

# Age
# 19 Age group data:
# 00 = 0 years
# 01 = 1-4 years
# 02 = 5-9 years
# 03 = 10-14 years
# 04 = 15-19 years
# …
# 17 = 80-84 years
# 18 = 85+ years


#  ---------------------------------------------
# County and Working Age Population
# ---------------------------------------------

working_age_groups <- c("04", "05", "06", "07", "08", "09", "10", "11", "12", "13")


county_workingage_population <- county_agegroup_population %>% 
  filter(age_group %in% working_age_groups) %>% 
  group_by(county_5dcode, year) %>% 
  summarise(workingage_pop = sum(agegroup_pop))

glimpse(county_workingage_population)


#  ---------------------------------------------
# County and Pre-Working-Age Population
# ---------------------------------------------

pre_working_age_groups <- c("00", "01", "02", "03")

county_pre_workingage_population <- county_agegroup_population %>% 
  filter(age_group %in% pre_working_age_groups) %>% 
  group_by(county_5dcode, year) %>% 
  summarise(pre_workingage_pop = sum(agegroup_pop))

glimpse(county_pre_workingage_population)


#  ---------------------------------------------
# County and Post-Working-Age Population
# ---------------------------------------------

post_working_age_groups <- c("14", "15", "16", "17", "18")

county_post_workingage_population <- county_agegroup_population %>% 
  filter(age_group %in% post_working_age_groups) %>% 
  group_by(county_5dcode, year) %>% 
  summarise(post_workingage_pop = sum(agegroup_pop))

#  ---------------------------------------------
# Combine Into One DataFrame
# ---------------------------------------------

county_population <- county_workingage_population %>% 
  left_join(county_pre_workingage_population) %>% 
  left_join(county_post_workingage_population)

county_population <- county_population %>% 
  mutate(total_pop = pre_workingage_pop + workingage_pop + post_workingage_pop)



#  ---------------------------------------------
# Change in County Counts per Year
# ---------------------------------------------

county_count_change_year <- county_population %>% 
  group_by(year) %>% 
  summarize(county_count = n()) %>% 
  mutate(lagged_count = lag(county_count)) %>% 
  mutate(change = county_count - lagged_count)

county_count_change_year %>% 
  ggplot(aes(x = year, y = change)) +
  geom_line(size = 2, color = "darkred")

# This establishes that our MSA reconstruction based on current correspondence will be far from perfect
# But it should still be useful, as long as we keep this issue in mind.

#  ---------------------------------------------
# MSA Total Population
# ---------------------------------------------

# https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html
# Bedford (independent) city, Virginia (51-515):
#   Changed to town status and added to Bedford County (51-019) effective July 1, 2013.

# Bedford city doesn't exist in either of the dataframes.
# Bedford county exists in county_code14 but is missing in population longitudinal data
# So Population of Lynchburg, VA, which includes Bedford county, will be undercounted. 
# Since it's consistent across the full timeline, not a major concern

glimpse(county_population)
glimpse(counties_m1_df)

msa_counties_joined <- counties_m1_df %>% 
  select(county_code14, msa_code15, msa_name15, county_pop10) %>% 
  inner_join(county_population, by = c("county_code14" = "county_5dcode"))

glimpse(msa_counties_joined)

msa_population <- msa_counties_joined %>% 
  group_by(msa_code15, msa_name15, year) %>% 
  summarize(msa_pre_workingage_pop = sum(pre_workingage_pop), 
            msa_post_workingage_pop = sum(post_workingage_pop), 
            msa_workingage_pop = sum(workingage_pop))

glimpse(msa_population)

# =============================================
# Charts
# =============================================

# get_msacode_fromregex("Oklahoma")

# Wayne county, MI (in Detroit urban area): 26163
wayne_df <- 
  county_population %>% 
  filter(county_5dcode == "26163")

plot_county_pop(wayne_df, "workingage_pop", "Wayne County")

detroit_df <-
  msa_population %>% 
  filter(msa_code15 == "19820")
plot_county_pop(detroit_df, "msa_post_workingage_pop", "Detroit MSA")

southbend_df <-
  msa_population %>% 
  filter(msa_code15 == "43780")
plot_county_pop(southbend_df, "msa_pre_workingage_pop",
                "South Bend-Mishawaka, IN-MI")


cville_df <- msa_population %>%
  filter(msa_code15 == "36420")

glimpse(cville_df)

plot_county_pop(cville_df,  "msa_post_workingage_pop", "Charlottesville, VA MSA")

# =============================================
# Plotting Functions
# =============================================

plot_location_popupation_trend <-
  function(location_data_df, population_column_name, title = NULL, subtitle = NULL, caption = NULL) {
    annot_data <- rbind(location_df[1,], tail(location_df, 1))
    
    ggplot(location_df)+
      geom_line(
        aes(x = location_df[["year"]], y = location_df[[population_column_name]]),
        size = 1,
        color = "darkblue",
        alpha = 0.4
      ) +
      geom_point(aes(x = location_df[["year"]], y = location_df[[population_column_name]]),
                 color = "#003366",
                 size = 2) +
      geom_label(aes(
        label = scales::comma(annot_data[[population_column_name]]),
        x = annot_data[["year"]],
        y = annot_data[[population_column_name]]), data = annot_data, nudge_x = c(0.5, -1),
        vjust = 1.5, alpha = 0.5
      ) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma) +
      theme(axis.title = element_blank()) +
      labs(title = title,
           subtitle = subtitle,
           caption = caption)
  }

options(highcharter.theme = hc_theme_smpl(), highcharter.debug = TRUE)

highchart() %>% 
  hc_add_series(southbend_df, "point", hcaes(x = year, y = msa_workingage_pop)) %>% 
  hc_add_annotation(labels = list(text = "This is an annotation"), 
                         point = c(x = 1990, y = 900000))



# %>% 
#   hc_add_series(detroit_df, "line", hcaes(x = year, y = msa_pre_workingage_pop))
