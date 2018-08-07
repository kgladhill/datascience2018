# demo for GIS census data

######### ######### ######### ######### ######### 
### Map ACS 2012-16 Median Household Income by Census Tract
## Install the tidycensus package if you haven't yet
#install.packages("tidycensus")

library(tidycensus)
library(ggplot2)
library(dplyr)
## setup cenus api key
## signup your census api key at http://api.census.gov/data/key_signup.html
census_api_key("1455096ad8326ad439889e10d3dea922a587b7da") 
# next use census_api_key("1455096ad8326ad439889e10d3dea922a587b7da"), install = TRUE) 
portland_tract_medhhinc <- get_acs(geography = "tract", 
                                   year = 2016, # 2012-2016
                                   variables = "B19013_001",  # Median Household Income in the Past 12 Months
                                   state = "OR", 
                                   county = c("Multnomah County", "Washington County", "Clackamas County"),
                                   geometry = TRUE) # load geometry/gis info

## Getting data from the 2012-2016 5-year ACS
## Downloading feature geometry from the Census website.  
##  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.
myplot <- ggplot(portland_tract_medhhinc) + 
  geom_sf(aes(fill = estimate)) +
  coord_sf(datum = NA) + theme_minimal()
ggsave("output/mymap.png",myplot)
#  kg try Yamhill county
 yamhill_tract_medhhinc <- get_acs(geography = "tract", 
                                    year = 2016, # 2012-2016
                                    variables = "B19013_001",  # Median Household Income in the Past 12 Months
                                    state = "OR", 
                                    county = c("Yamhill"),
                                    geometry = TRUE) # load geometry/gis info
 
 ## Getting data from the 2012-2016 5-year ACS
 ## Downloading feature geometry from the Census website.  
 ##  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.
 ggplot(yamhill_tract_medhhinc) + 
   geom_sf(aes(fill = estimate)) +
   coord_sf(datum = NA) + theme_minimal()
##  Successful!
 
######### ######### ######### ######### ######### 
### Interactive Maps of ACS 2012-16 Median Household Income by Census Tract
 ## Install the mapview package if you haven't yet
 #install.packages("mapview")
 library(sf)
 ## Linking to GEOS 3.6.1, GDAL 2.2.3, proj.4 4.9.2
 library(mapview)
 library(tidyverse)
 
 mapview(portland_tract_medhhinc %>% select(estimate), 
         col.regions = sf.colors(10), alpha = 0.1)
#kg try for Yamhill county
 mapview(yamhill_tract_medhhinc %>% select(estimate), 
         col.regions = sf.colors(10), alpha = 0.1)
 
 
 ######### ######### ######### ######### ######### 
 ### Example of spatial analysis: spatial join
 library(sf)
 library(readr)
 # read 1994 Metro TAZ shape file
 taz_sf <- st_read("data/taz1260.shp", crs=2913)
 ## Reading layer `taz1260' from data source `/home/lmwang/datascience2018/data/taz1260.shp' using driver `ESRI Shapefile'
 ## Warning: st_crs<- : replacing crs does not reproject data; use st_transform
 ## for that
 ## Simple feature collection with 1247 features and 1 field
 ## geometry type:  POLYGON
 ## dimension:      XY
 ## bbox:           xmin: 7435706 ymin: 447796.5 xmax: 7904748 ymax: 877394.8
 ## epsg (SRID):    2913
 ## proj4string:    +proj=lcc +lat_1=46 +lat_2=44.33333333333334 +lat_0=43.66666666666666 +lon_0=-120.5 +x_0=2500000.0001424 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs
 # read geocode.raw file that contains X and Y coordinates
 portland94_df <- read_csv("data/portland94_geocode.raw.zip", col_names=c("uid", "X", "Y", "case_id", 
                                                                          "freq", "rtz", "sid", 
                                                                          "totemp94", "retemp94"))
 ## Multiple files in zip: reading 'geocode.raw'
 ## Parsed with column specification:
 ## cols(
 ##   uid = col_double(),
 ##   X = col_double(),
 ##   Y = col_double(),
 ##   case_id = col_integer(),
 ##   freq = col_integer(),
 ##   rtz = col_integer(),
 ##   sid = col_integer(),
 ##   totemp94 = col_double(),
 ##   retemp94 = col_double()
 ## )
 portland94_df <- portland94_df %>% 
   filter(X!=0, Y!=0) %>% 
   sample_n(500)
 
 # create a point geometry with x and y coordinates in the data frame
 portland94_sf <- st_as_sf(portland94_df, coords = c("X", "Y"), crs = 2913)
 
 # spatial join to get TAZ for observations in portland94_sf
 portland94_sf <- st_join(portland94_sf, taz_sf)
 head(portland94_sf)
 ## Simple feature collection with 6 features and 8 fields
 ## geometry type:  POINT
 ## dimension:      XY
 ## bbox:           xmin: 7558722 ymin: 672752.4 xmax: 7682834 ymax: 756533.2
 ## epsg (SRID):    2913
 ## proj4string:    +proj=lcc +lat_1=46 +lat_2=44.33333333333334 +lat_0=43.66666666666666 +lon_0=-120.5 +x_0=2500000.0001424 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs
 ##          uid case_id freq  rtz sid     totemp94   retemp94  TAZ
 ## 1 2095861206   14260   38  796   1 1.235094e+04 2844.48390  796
 ## 2 5004382101   16253   66 1168   2 2.461264e+02   16.67518 1168
 ## 3 5050692201   20980   20 1138   2 9.324988e+02  151.69258 1138
 ## 4 2131881106    8143   44   16   1 1.034557e+05 9351.54390   16
 ## 5 5063221201   19055   10 1159   2 1.368576e+03  230.89846 1159
 ## 6 2264132106     720   17  278   1 2.242726e+00    0.00000  278
 ##                   geometry
 ## 1 POINT (7655054 672752.4)
 ## 2 POINT (7660684 756533.2)
 ## 3 POINT (7682834 726256.9)
 ## 4 POINT (7642134 680112.7)
 ## 5 POINT (7671742 744415.5)
 ## 6 POINT (7558722 692746.9)
 ggplot() +
   geom_sf(data = taz_sf, aes(alpha=0.9)) +
   geom_sf(data = portland94_sf)