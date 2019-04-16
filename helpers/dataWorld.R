library(data.world)

# Geo dataset: asifmeh/citee-geodata
# County to msa crosswalk tbl: county14_msa15_state

#  ---------------------------------------------
# MSA Counties
# ---------------------------------------------
sql_county_msa <- qry_sql("SELECT * FROM county14_msa15_state")

counties_msa_df <-
  data.world::query(sql_county_msa, "asifmeh/citee-geodata")

counties_m1_df <- counties_msa_df %>%
  filter(msa_type == "Metro")

counties_m2_df <- counties_msa_df %>%
  filter(msa_type == "Micro")

county_rural_df <- counties_msa_df %>%
  filter(is.na(msa_type))

#  ---------------------------------------------
# Urban Area Counties
# ---------------------------------------------
# Convention: uauc = both urbanized area and urban cluster.
# urbanarea = only urbanized area, urbancluster = only urban cluster
sql_county_uauc <- qry_sql("SELECT * FROM urbanarea12_county14") 

counties_uauc_df <- 
  data.world::query(sql_county_uauc, "asifmeh/citee-geodata")

counties_urbanarea_df <- counties_uauc_df %>% 
  filter(str_detect(ua_name12, "Urbanized Area"))

# counties_urbancluster_df <- counties_uauc_df %>% 
#   filter(str_detect(ua_name12, "Urban Cluster"))

#  ---------------------------------------------
# MSAs
# ---------------------------------------------
sql_msa15 <-
  qry_sql("SELECT * FROM msa15 WHERE msa_type15 = 'Metro'")
msa15_df <- query(sql_msa15, "asifmeh/citee-geodata")

get_msacode_fromregex <- function(msa_name_regex) {
  msa15_df %>%
    filter(str_detect(msa_name15, msa_name_regex))
}

