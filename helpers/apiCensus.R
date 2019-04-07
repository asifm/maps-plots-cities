library(censusapi)
# Examples master list: 
# https://cran.r-project.org/web/packages/censusapi/vignettes/example-masterlist.html

censusapis_list <- listCensusApis()
bds_api_name <- "timeseries/bds/firms"
bdsmeta_vars <- listCensusMetadata(bdsname, type = "variables")
bdsmeta_geo <- listCensusMetadata(bdsname, type = "geography")

jcrb <-
  getCensus(
    bds_api_name,
    vars = c("job_creation_rate_births", "sumlev"),
    time = "from 1980 to 2000",
    region = "us:*"
  )

listCensusMetadata(name = bds_api_name)

