# Census API key saved in .Renviron
source(here::here('helpers/setup.R'))
source(here::here('helpers/dataWorld.R'))
library(tidycensus)

# ---------------------------------------------
# Initialize some useful variables for easy reference
# ---------------------------------------------
test_variables <-
  c("DP03_0022P",
    "DP03_0021P",
    "DP02_0086",
    "DP02_0152P",
    "DP02_0065P")
# DP03_0022P: walk; DP03_0021P: public transport; DP02_0086: population, 
# DP02_0152P: broadband internet
# DP02_0065P: graduate or professional degree

# latest_year variable is referenced in tidycensus and tigris functions
# Remember to update this here when these packages update their latest year
latest_year = 2017

# Contiguous USA: 48 states and DC
contig_usa <- c(state.abb[!state.abb %in% c("AK", "HI")], "DC")

# Everything outside of contiguous USA
outer_states_territories <-
  c("AK", "HI", "PR", "VI", "AS", "FM", "GU", "MH", "MP")

# US territories and possessions
us_territ_posses <- c("PR", "VI", "AS", "FM", "GU", "MH", "MP")

msa <- "metropolitan statistical area/micropolitan statistical area"

# ---------------------------------------------
# Load all variables from a specified
# table and then perform string search
# ---------------------------------------------
lookup_variable <-
  function(label_lookup = NULL,
           code_lookup = NULL,
           year = latest_year,
           dataset = "acs5",
           ignore_case = TRUE) {
    variable_lookup_df <-
      load_variables(year = year,
                     dataset = dataset,
                     cache = TRUE)
    
    if (!is.null(label_lookup)) {
      variable_lookup_df <- variable_lookup_df %>%
        filter(str_detect(label, regex(label_lookup, ignore_case = ignore_case)))
    }
    
    if (!is.null(code_lookup)) {
      variable_lookup_df <- variable_lookup_df %>%
        filter(str_detect(name, code_lookup))
    }
    
    variable_lookup_df
  }

# =============================================
# Geo Lists
# =============================================

#  ---------------------------------------------
# List of counties
# ---------------------------------------------
# Get the list of counties in an MSA
get_fips5_allcounties_frommsa <-
  function(msa_name_regex, remove_state_fips = FALSE) {
    # msa_regex should include state abbr (e.g. "Austin.*Tx")
    counties_df <- counties_m1_df %>%
      filter(str_detect(msa_name15, msa_name_regex)) %>%
      select(county_code14)
    
    if (remove_state_fips == TRUE) {
      # Remove first two digits (state fips)
      counties_df <- counties_df %>%
        mutate(county_code14 = str_sub(county_code14, 3))
    }
    counties_df[['county_code14']]
  }

# Get the list of counties that an urban area falls in (fully or partly)
get_fips5_allcounties_fromurbanarea <-
  function(urbanarea_name_regex) {
    # urbanarea_name_regex should include state abbr (e.g. "Austin.*Tx")
    counties_df <- counties_urbanarea_df %>%
      filter(str_detect(ua_name12, urbanarea_name_regex)) %>%
      select(county_code14)
    
    counties_df[['county_code14']]
  }


#  ---------------------------------------------
# Boundary Data
# ---------------------------------------------

# Get map data as simple features
# latest_year is defined earlier

# Options: all US MSAs
get_sf_allmsas <-
  function(resolution = "5m",
           year = latest_year,
           M1_only = TRUE,
           contiguous_only = TRUE) {
    #' Resolution options are 500k, 5m, and 20m
    msas <-
      core_based_statistical_areas(cb = TRUE,
                                   resolution = resolution,
                                   year = year)
    
    if (M1_only == TRUE) {
      msas <- msas %>%
        filter(LSAD == "M1")
    }
    
    if (contiguous_only == TRUE) {
      msas <- msas %>%
        filter(!str_detect(string = NAME, pattern = "AK$|HI$|PR$"))
    } else {
      msas <- msas %>%
        filter(!str_detect(string = NAME, pattern = "PR$"))
    }
    msas
  }

# Options: multiple states' all counties,single state's all counties
get_sf_counties_fromstates <-
  function(state = NULL,
           resolution = "5m",
           year = latest_year,
           contiguous_only = TRUE) {
    #' Resolution options are 500k, 5m, and 20m
    
    if (!is.null(state)) {
      contiguous_only = FALSE
    }
    
    counties_ <- counties(
      state = state,
      cb = TRUE,
      resolution = resolution,
      year = year
    )
    
    if (contiguous_only == TRUE) {
      counties_ <- counties_ %>%
        # Remove AK and HI; also PR and territories
        filter(!STATEFP %in% c("02", "15", "72",  "60",  "66",  "69",  "78"))
    } else {
      counties_ <- counties_ %>%
        # Remove PR and territories from data
        filter(!STATEFP %in% c("72",  "60",  "66",  "69",  "78"))
    }
    counties_
  }

get_sf_allstates <- function(contiguous_only = TRUE) {
  states_ <- states(cb = TRUE, resolution = "20m")
  
  if (contiguous_only == TRUE) {
    states_ <- states_ %>%
      # Remove AK and HI; also PR and territories
      filter(!STATEFP %in% c("02", "15", "72",  "60",  "66",  "69",  "78"))
  } else {
    states_ <- states_ %>%
      # Remove PR and territories from data
      filter(!STATEFP %in% c("72",  "60",  "66",  "69",  "78"))
  }
  states_
}

# =============================================
# ACS and Decennial Census Data
# =============================================
#  ---------------------------------------------
# States
# ---------------------------------------------
get_acsdata_allstates <- function(variables,
                                  survey = "acs5",
                                  year = latest_year,
                                  geometry = FALSE,
                                  shift_geo = FALSE,
                                  contiguous_only = TRUE) {
  # Resolve contradictory argument values by overriding
  if (shift_geo == TRUE) {
    contiguous_only <-  FALSE
    geometry <- TRUE
  }
  
  if ((geometry == FALSE) |
      (contiguous_only == TRUE))
    shift_geo <- FALSE
  
  acsdata <- get_acs(
    geography = "state",
    variables = variables,
    survey = survey,
    year = year,
    geometry = geometry,
    shift_geo = shift_geo
  )
  
  if (contiguous_only == TRUE) {
    acsdata <- acsdata %>%
      filter(!GEOID %in% c("02", "15", "72"))
  }
  acsdata
}





# ---------------------------------------------
# MSAs
# ---------------------------------------------

# M1 = Metropolita MSAs
# M2 = Micropolitan MSAs
# latest_year is defined earlier

# Defaults will get metros from contiguous USA
get_acsdata_allmsas <- function(variables,
                                year = latest_year,
                                M1_only = TRUE,
                                state = NULL,
                                contiguous_only = TRUE,
                                geometry = FALSE) {
  msa <- "metropolitan statistical area/micropolitan statistical area"
  
  if (contiguous_only == TRUE) {
    msa_name_pattern = "AK|HI|PR"
  } else {
    msa_name_pattern = "PR"
  }
  
  if (M1_only == TRUE) {
    msa_name_pattern <- str_c(msa_name_pattern, "|Micro Area")
  }
  
  acsdata_allmsas_df <- get_acs(
    geography = msa,
    variables = variables,
    year = year,
    state = state
  ) %>%
    filter(!str_detect(
      NAME,
      pattern = regex(pattern = msa_name_pattern,
                      ignore_case = FALSE)
    )) %>%
    mutate(NAME = str_remove(NAME, " Metro Area| Micro Area"))
  
  
  if (geometry == TRUE) {
    sf_allmsas <- get_sf_allmsas()
    
    acsdata_allmsas_df <- acsdata_allmsas_df %>%
      left_join(sf_allmsas, by = c("GEOID", "NAME")) %>%
      st_as_sf()
    
  }
  acsdata_allmsas_df
}

#  ---------------------------------------------
# Urban Areas
# ---------------------------------------------
get_acsdata_allurbanareas <-
  function(variables,
           survey = "acs5",
           no_urbanclusters = TRUE,
           geometry = FALSE) {
    ua_acsdata <-
      get_acs(geography = "urban area",
              variables = variables,
              survey = survey)
    if (no_urbanclusters == TRUE) {
      ua_acsdata <- ua_acsdata %>%
        filter(str_detect(NAME, "Urbanized Area"))
    }
    
    if (geometry == TRUE) {
      ua_sf <- urban_areas(cb = TRUE)
      glimpse(ua_sf)
      ua_acsdata <- ua_acsdata %>%
        left_join(ua_sf, by = c("GEOID" = "GEOID10")) %>%
        # Keep NAME10 as it's shorter, and drop NAME
        select(-NAME) %>%
        st_as_sf()
    }
    ua_acsdata
  }


# ---------------------------------------------
# Counties
# ---------------------------------------------
# Options: all us counties, single state's all counties, multiples states' all counties,
# single state's multiple counties, single state's single county (state required in all cases)
get_acsdata_counties_fromstates <- function(variables,
                                            year = latest_year,
                                            state = NULL,
                                            county = NULL,
                                            geometry = FALSE,
                                            contiguous_only = TRUE) {
  if (contiguous_only == TRUE) {
    # Alaska, Hawaii, and PR respectively
    geocode_pattern = "^02|^15|^72"
  } else {
    # Only PR
    geocode_pattern = "^72"
  }
  
  get_acs(
    geography = "county",
    variables = variables,
    year = year,
    state = state,
    county = county,
    geometry = geometry
  ) %>%
    filter(!str_detect(GEOID, geocode_pattern)) %>%
    mutate(NAME = str_remove(NAME, " County"))
}

#  ---------------------------------------------
# Tracts
# ---------------------------------------------
get_acsdata_alltracts_fromcounties <-
  function(variables,
           survey = "acs5",
           year = latest_year,
           counties_fips5,
           geometry = FALSE) {
    tracts_acs_list <- map(
      counties_fips5,
      ~
        get_acs(
          geography = "tract",
          variables = variables,
          survey = survey,
          year = year,
          # Expects 3-digit fips
          county = str_sub(., start = 3),
          # Get state fips: first 2 digits
          state = str_sub(., end = 2),
          geometry = geometry
        )
    )
    
    reduce(tracts_acs_list, rbind)
    
  }

get_acsdata_alltracts_frommsa <-
  function(msa_name_regex,
           variables,
           survey = "acs5",
           year = latest_year,
           geometry = FALSE) {
    counties_fips5 <-
      get_fips5_allcounties_frommsa(msa_name_regex = msa_name_regex, remove_state_fips = FALSE)
    
    acsdata_tracts_sf <-
      get_acsdata_alltracts_fromcounties(
        variables = variables,
        survey = survey,
        year = year,
        counties_fips5 = counties_fips5,
        geometry = geometry
      )
    acsdata_tracts_sf
  }

get_acsdata_alltracts_fromurbanarea <-
  function(urbanarea_name_regex,
           variables,
           survey = "acs5",
           year = latest_year,
           geometry = FALSE) {
    # First, get a list of counties which the urban area fully or partially overlap
    counties_fips5 <-
      get_fips5_allcounties_fromurbanarea(urbanarea_name_regex)
    
    # Second, get acs (with sf) data for tracts that are in only those counties
    acsdata_tracts <- get_acsdata_alltracts_fromcounties(
      variables = variables,
      survey = survey,
      year = year,
      counties_fips5 = counties_fips5,
      geometry = geometry
    )
    
    if (geometry == TRUE) {
      # Get all urban areas in the country. API doesn't allow selective download
      urbanarea_sf <- urban_areas(cb = TRUE) %>%
        filter(str_detect(NAME10, urbanarea_name_regex))
      
      acsdata_tracts <- urbanarea_sf %>%
        st_intersection(acsdata_tracts)
    }
    
    acsdata_tracts
  }
