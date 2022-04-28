library(HumanitiesDataAnalysis)
library(arrow)
library(tidyverse)
library(sf)

read_shapefile = function() {
  if (!file.exists("data/nyc_streets_historical_20220408.json")) {
    R.utils::gunzip("data/nyc_streets_historical_20220408.json.gz", remove=FALSE)
  }
  street_shapes = read_sf("data/nyc_streets_historical_20220408.json")
}
# 04/11 Monday in class

read_neighborhoods = function() {
  neighborhood_shapes = read_sf("data/2010 Neighborhood Tabulation Areas (NTAs).geojson") 
  neighborhood_shapes = neighborhood_shapes |> st_transform(crs = 2263)
  street_shapes2 = street_shapes |> st_transform(crs = 2263)
  #set CRS of both objects to the same to prepare for join
}

get_neighborhoods_and_streets = function() {
  shapes = read_shapefile()
neighborhoods_and_streets = shapes |>
  #filter(ntaname == “Lower East Side”) |>
  #OPTIONAL filter street shapes object down before join because join takes a long time
  st_join(read_neighborhoods())
}
#Test mapping streets in one neighborhood


