# Helper functions to draw maps based on acs and
# decennial census data (other kinds of data to be added later)
# Author: Asif Mehedi
# Created: 20190331

# =============================================
# Setup
# =============================================
source(here::here("helpers/setup.R"))
source(here::here('helpers/apiACS.R'))

# geo-data/geo-analysis
library(sf)
library(tigris)

# mapping
library(leaflet)
library(tmap)
library(tmaptools)

# Set tigris options
options(tigris_class = "sf", tigris_use_cache = TRUE)

tmap_options(
  fontfamily = "Roboto",
  aes.palette  = c(seq = "viridis", div = "RdYiGn", cat = "accent"),
  basemaps.alpha = 0.3, 
  frame.double.line = FALSE,
  bg.color = "#ffffff",
  legend.bg.alpha = 0.5
)


# ggmap Google API key: set in .Renviron

# =============================================
# Draw Map
# =============================================

#  ---------------------------------------------
# Draw a single MSA choropleth at tract level,
# from a regex (to identify an MSA) and a variable code
# ---------------------------------------------

# Single variable only
draw_acsdata_alltracts_frommsa <-
  function(msa_name_regex,
           variable,
           survey = "acs5",
           year = latest_year,
           variable_description = NULL) {
    tracts_acs_sf <-
      get_acsdata_alltracts_frommsa(
        msa_name_regex = msa_name_regex,
        variables = variable,
        survey = survey,
        year = year,
        geometry = TRUE
      )
    
    ggplot(tracts_acs_sf) +
      geom_sf(aes(fill = estimate)) +
      scale_fill_viridis_c(name = variable_description)
  }

# Single variable only
draw_acsdata_allmsas <- function(variable,
                                 survey = "acs5",
                                 year = latest_year,
                                 variable_description = NULL) {
  acsdata_allmsas_df <- get_acsdata_allmsas(variables = variable)
  
  # MSA sf not available through tidycensus::get_acs. Hence, use of tigris
  sf_allmsas <- get_sf_allmsas()
  # To draw state boundary
  sf_allstates <- get_sf_allstates()
  
  acsdata_allmsas_df <- acsdata_allmsas_df %>%
    left_join(sf_allmsas, by = "GEOID") %>%
    st_as_sf() %>%
    st_transform(crs = "+init=epsg:4326")
  
  g <- ggplot() +
    geom_sf(data = sf_allstates,
            fill = "#efefef",
            color = "gray") +
    geom_sf(mapping = aes(fill = estimate), data = acsdata_allmsas_df) +
    scale_fill_viridis_c(name = variable_description, label = scales::comma)
  g
  
}

# next line for test
# draw_acsdata_allmsas(variable = "B08006_015")


# Single variable only
# Multiple states possible for geography = county, but not geography = tract
draw_acsdata_counties_tracts_fromstate <-
  function(geo_level = "county",
           variable,
           variable_description = NULL,
           state_ = NULL,
           map_mode = "plot",
           legend_title = NA,
           number_fmt = scales::comma) {
    acs_sf_data <- get_acs(
      geography = geo_level,
      variables = variable,
      state = state_,
      geometry = TRUE
    )
    
    acs_sf_data <- acs_sf_data %>%
      st_transform(crs = "+init=epsg:4326")
    
    tmap_mode(map_mode)
    
    tm_shape(acs_sf_data) +
      tm_polygons(col = "estimate", title = legend_title) +
      tm_layout(title = variable_description)
    
    # g <- ggplot() +
    #   geom_sf(mapping = aes(fill = estimate), data = acs_sf_data) +
    #   scale_fill_viridis_c(name = variable_description, label = number_fmt) +
    #   theme_custom_minimal
    # g
  }
# latest_year defined earlier
draw_acsdata_alltracts_fromurbanarea <-
  function(urbanarea_name_regex,
           variable,
           variable_description = NULL,
           survey = "acs5",
           year = latest_year,
           map_mode = "plot",
           palette = "viridis",
           legend_title = "",
           key_ = NULL,
           value_ = NULL) {
    acsdata_tracts <- get_acsdata_alltracts_fromurbanarea(
      urbanarea_name_regex = urbanarea_name_regex,
      variables = variable,
      survey = survey,
      year = year,
      geometry = TRUE
    )
    
    glimpse(acsdata_tracts)
    # 
    # Get all urban areas' sf. API doesn't allow selective download
    urbanarea_sf <- urban_areas(cb = TRUE) %>% 
      # Then filter out everything except the urban area of interest
      filter(str_detect(NAME10, urbanarea_name_regex))
    
    acsdata_tracts2 <- urbanarea_sf %>%
      st_intersection(acsdata_tracts) %>%
      st_collection_extract("POLYGON")
    
    # glimpse((acsdata_tracts2))
    
    tmap_mode(map_mode)
    
    tiles_from_osm <-
      read_osm(acsdata_tracts2, type = "stamen-toner")
    
    tmap_out <- tm_shape(tiles_from_osm) +
      tm_rgb(alpha = 0.3) +
      tm_shape(acsdata_tracts2) +
      tm_polygons(
        col = "estimate",
        alpha = 0.5,
        palette = palette,
        title = legend_title, popup.vars = c("Percent:" = "estimate", "Name" = "name")
      ) +
      tm_layout(
        variable_description,
        legend.title.size = 1,
        legend.text.size = 0.7,
        title.size = 2,
        title.fontfamily = "Open Sans"
      )
    
    if (!is.null(key_)) {
      bounding_box <- st_bbox(acsdata_tracts2)
      
      q <- opq(bounding_box) %>%
        add_osm_feature(key = key_, value = value_) %>%
        osmdata_sf()
      
      tmap_out <- tmap_out + tm_shape(q$osm_points) +
        tm_dots(col = "black")
      
    }
    tmap_out
  }

# Next line is for test
# draw_acsdata_alltracts_fromurbanarea(
#   "South Bend.*IN",
#   test_variables[1],
#   variable_description = "Pct of people with a graduate or professional degree",
#   map_mode = "view", legend_title = "Percent of all people in a a census track"
# )

