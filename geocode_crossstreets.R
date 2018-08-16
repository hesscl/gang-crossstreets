#### Reshape and Geocode Gang Crossstreets ------------------------------------

#dependencies
library(tidyverse) #for dplyr
library(readxl)    #to read in Excel
library(RCurl)     #to handle URL requests
library(rjson)     #to parse URL responses
library(rgdal)     #to work with spatial data
library(ggmap)     #to double check geocodes

#wd should be base dir of the repository
#setwd("H:/gang-crossstreets")

#data (NB: no existing column names so avoid treating first row as such)
gang <- read_excel("Chicago Gang Cross Streets.xlsx", col_names = FALSE)

#shapefile

#load NHGIS tract shape file for USA (is slow given big file)
tract <- readOGR(dsn = paste0("R:/Project/seattle_rental_market/data/geo/US_tract_2010"), 
                 layer = "US_tract_2010",
                 stringsAsFactors = F)


### A. Save geocoding functions to env for later use --------------------------

#API key is Chris Hess's, need hesscl udrive or alt key to run
#these geocoding funtions are adapted from r-bloggers

#this takes starting address strings and structures an appropriate URL call
#this function is nested within geocode() and not directly called
url <- function(address, return.call = "json") {
  key <- readLines("H:/gmaps_key.txt", warn=F)
  root <- "https://maps.google.com/maps/api/geocode/"
  u <- paste0(root, return.call, "?address=", address, "&key=", key)
  return(URLencode(u))
}

#this function takes an address argument and returns parsed fields from
#the JSON that Google Maps API returned.
geocode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(x$status)
  }
}

#e.g.
geocode("Poplar and 26th, Philadelphia, PA")


#### B. Structure data for geocoding ------------------------------------------

#preview the data structure
glimpse(gang)

#key-value pairs---best to first reshape the data by first col (gangname == X__1)
#since there otherwise is variation in the list length per gang
long_gang <- gang %>%
  gather(key = cross_street_n, value = address, -X__1) #syntax is to melt by all cols except X__1

#look at new long structure
glimpse(long_gang)

#remove NA rows so that each gang appears as many times as there are cross-streets
#also mutate a slightly more informative address string with ", IL USA" as a constraint
long_gang <- long_gang %>%
  filter(!is.na(address)) %>%
  mutate(submit_address = paste0(address, " Chicago, IL USA"))

#try geocoding the first row
geocode(long_gang$submit_address[1])


#### C. Geocode the addresses -------------------------------------------------

#submit rows to Google Maps for geocoding (NB: calling this code uses credits)
long_gang <- long_gang %>%
  rowwise() %>%
  mutate(result = list(geocode(submit_address)))

#parse each row's returned fields
processed_gang <- long_gang %>%
  rowwise() %>%
  mutate(lat = result[1],
         lng = result[2],
         matchType = result[3],
         matchAddress = result[4])

#look at the results
#View(processed_gang)

#need to retry a few / set first try to NA
processed_gang <- processed_gang %>%
  mutate(lat = ifelse(matchType == "APPROXIMATE", NA, lat),
         lng = ifelse(matchType == "APPROXIMATE", NA, lng),
         matchAddress = ifelse(matchType == "APPROXIMATE", NA, matchAddress),
         matchType = ifelse(matchType == "APPROXIMATE", NA, matchType)) %>%
  ungroup()


#### D. Append tract geography ------------------------------------------------

#remove no-match cases for now
processed_gang <- processed_gang %>%
  filter(!is.na(matchType)) %>%
  select(-result) %>%
  as.data.frame() %>%
  mutate(lat = type.convert(lat),
         lng = type.convert(lng))

#identify the spatial projection used by NHGIS, save Proj4 string
tractCRS <- proj4string(tract)

#save WGS as a spatial projection to use with CL data lat/lng
WGS.latlon <- CRS("+proj=longlat +datum=WGS84")

#initialize the lat/lon as spatial data
coordinates(processed_gang) <- ~ lng + lat

#assign them the WGS spatial projection (set as NA as default)
proj4string(processed_gang) <- WGS.latlon

#project the tract data to WGS
tract.proj <- spTransform(tract, WGS.latlon)

#spatially intersect tract IDs to polygons
GEOID10 <- over(processed_gang, tract.proj[,"GEOID10"])
processed_gang <- bind_cols(processed_gang@data, GEOID10)

tract_gang <- left_join(processed_gang, tract@data)

get_map(location = c(min(tract_gang$l)))

#### E. Save geocoded and tract'ed table to storage ---------------------------

write_csv(tract_gang, "processed_table.csv")

 


                                  
