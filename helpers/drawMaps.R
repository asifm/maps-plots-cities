# Helper function to draw map based on acs and decennial census data
# Author: Asif Mehedi
# Created: 20190331


# =============================================
# Setup
# =============================================
source(here::here("helpers/getData.R"))

# geo-data/geo-analysis
library(sf)
library(tigris)

# mapping
library(leaflet)
library(tmap)
library(tmaptools)

# Set options
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# ggmap Google API key: set in .Renviron

# =============================================
# Draw Map
# =============================================

#  ---------------------------------------------
# Draw a single MSA choropleth at tract level,
# from a regex (to identify an MSA) and a variable code
# ---------------------------------------------

# Single variable only
draw_acsdata_msatracts <-
  function(msa_name_regex,
           variable,
           survey = "acs5",
           year = latest_year,
           variable_desc = NULL,
           palette_scale_leaflet = colorNumeric,
           palette_name = "viridis",
           legend_pos = "topleft") {
    tracts_acs_sf <-
      get_acsdata_alltracts_frommsa(
        msa_name_regex = msa_name_regex,
        variables = variable,
        survey = survey,
        year = year,
        geometry = TRUE
      )
    
    pal <- palette_scale_leaflet(palette = palette_name,
                                 domain = tracts_acs_sf$estimate)
    
    tracts_acs_sf %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(
        # popup = ~ str_extract(NAME, "^([^,]*)"),
        popup = ~ NAME,
        stroke = FALSE,
        smoothFactor = 0,
        fillOpacity = 0.7,
        color = ~ pal(estimate)
      ) %>%
      addLegend(
        legend_pos,
        pal = pal,
        values = ~ estimate,
        title = variable_desc,
        opacity = 0.8,
        na.label = NULL
      )
  }

# Single variable only
draw_acsdata_allmsas <- function(variable,
                                 survey = "acs5",
                                 year = latest_year,
                                 variable_desc = NULL,
                                 palette_scale_leaflet = colorNumeric,
                                 palette_name = "viridis",
                                 legend_pos = "topleft") {
  acsdata_allmsas_df <- get_acsdata_allmsas(variables = variable)
  
  sf_allmsas <- get_sf_allmsas()
  
  pal <-
    palette_scale_leaflet(palette = "viridis", domain = acsdata_allmsas_df$estimate)
  
  acsdata_allmsas_df %>%
    left_join(sf_allmsas, by = "GEOID") %>%
    st_as_sf() %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet() %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(
      popup = ~ str_extract(NAME.x, "^([^,]*)"),
      stroke = FALSE,
      weight = 1,
      smoothFactor = 0,
      fillOpacity = 0.7,
      color = ~ pal(estimate)
    ) %>%
    addLegend(
      title = variable_desc,
      position = legend_pos,
      pal = pal,
      values = ~ estimate,
      opacity = 0.8,
      na.label = NULL
    )
}

# draw_acsdata_allmsas(variable = "B19301_001")

# single variable only
get_acs_sf_allmsas <- function(variable,
                                  survey = "acs5",
                                  year = latest_year) {
  acsdata_allmsas_df <- get_acsdata_allmsas(variables = variable)
  
  sf_allmsas <- get_sf_allmsas()
  
  acsdata_allmsas_df %>%
    left_join(sf_allmsas, by = "GEOID") %>%
    st_as_sf()
}


# REFACTOR
draw_choropleth_ACS <- 
  function(geo_level = "county", variable_code, variable_desc = NULL, state_name = NULL,
           palette_scale_leaflet = colorNumeric, palette_name = "viridis", legend_pos = "topleft") {
    
    data <- get_acs(geography = geo_level, 
                    variables = variable_code, 
                    state = state_name,
                    geometry = TRUE) 
    
    pal <- palette_scale_leaflet(palette = palette_name, 
                                 domain = data$estimate)
    
    # leaflet_options = leafletOptions()
    
    data %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet(width = "100%") %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal(estimate)) %>%
      addLegend(legend_pos, 
                pal = pal, 
                values = ~ estimate,
                title = variable_desc,
                opacity = 0.8, na.label = NULL)
  }
