
# LOAD PACKAGES

library(cartogram)     #spatial maps w/ tract size bias reduction
library(corrplot)
library(dplyr)         #data wrangling
library(DT)            #formatting output
library(flexdashboard) #dashboard layout, widgets, and uploading
library(geojsonio)     #maps
library(ggmap)         #maps
library(ggplot2)       #graphing 
library(ggthemes)      #nice formats for ggplots
library(knitr)         #formatting output
library(leaflet)       #maps
library(maptools)      #spatial object manipulation
library(mclust)        #cluster analysis 
library(pals)          #maps
library(pander)        #formatting output
library(rgdal)         #maps
library(rgeos)         #Interface to open-source geometry engine (GEOS)
library(rsconnect)     #dashboard layout, widgets, and uploading
library(sf)            #maps
library(shiny)         #dashboard layout, widgets, and uploading
library(sp)            #maps
library(stargazer)     #formatting output
library(tidycensus)
library(tigris)
library(tmap)          #maps
library(viridis)       #maps



# LOAD Census Key
readRenviron("~/.Renviron")
# Data Steps
# Step 1 - Select San Diego
crosswalk <- read.csv( "https://raw.githubusercontent.com/DS4PS/cpp-529-master/master/data/cbsatocountycrosswalk.csv",  stringsAsFactors=F, colClasses="character" )
# search for city names by strings, use the ^ anchor for "begins with" 
# Make sure that you use all caps for the city search
grep( "^SAN ", crosswalk$msaname, value=TRUE ) 
# Step 2 - Select county fips
# Select all of your county fips. To use them in the TidyCensus package you will need to split the state and county:
these.san <- crosswalk$msaname == "SAN DIEGO, CA" 
these.fips <- crosswalk$fipscounty[ these.san ]
these.fips <- na.omit( these.fips )
state.fips <- substr( these.fips, 1, 2 )
county.fips <- substr( these.fips, 3, 5 )
# Step 3 - Create Dorling 
# To create a Dorling cartogram we need a shapefile and a population count. We can get both through the Census download that includes simple features.
URL <- "https://github.com/DS4PS/cpp-529-master/raw/master/data/ltdb_std_2010_sample.rds"
census.dat <- readRDS(gzcon(url( URL )))
#Do not add zeros to the state number 
san.pop <-
get_acs( geography = "tract", variables = "B01003_001",
         state = "06", county = county.fips[state.fips=="06"], geometry = TRUE ) %>% 
         select( GEOID, estimate) %>%
         rename( POP = estimate )
# get a census tract shapefile
# and add census data: 
# can merge an sf object and data.frame
san.pop$GEOID <- substring(san.pop$GEOID, 2)
san1 <- merge( san.pop, census.dat, by.x="GEOID", by.y="tractid" )
# make sure there are no empty polygons
san2 <- san1[ ! st_is_empty( san1 ) , ]
# Step 4: Transform the Shapefile into A Dorling Cartogram
# convert sf map object to an sp version
san.sp <- as_Spatial(san2)
#plot(san.sp)



# project map and remove empty tracts
san.sp <- spTransform( san.sp, CRS("+init=epsg:3395"))
san.sp <- san.sp[ san.sp$POP != 0 & (! is.na( san.sp$POP )) , ]
# convert census tract polygons to dorling cartogram
# no idea why k=0.03 works, but it does - default is k=5
san.sp$pop.w <- san.sp$POP / 9000 # max(msp.sp$POP)   # standardizes it to max of 1.5
san <- cartogram_dorling( x=san.sp, weight="pop.w", k=0.03 )
#plot( san )



tm_shape( san ) + 
tm_polygons( size="POP", col="hinc12", n=7, style="quantile", palette="Spectral" ) 



bb <- st_bbox(c(xmin= -13055498, xmax= -13011238, ymax= 3853073, ymin= 3817112), crs = st_crs("+init=epsg:3395"))

tm_shape( san, bbox = bb ) + 
  tm_polygons( col="hinc12", n=7, style="quantile", palette="Spectral" ) +
  tm_layout("Dorling Cartogram", title.position = c("right", "top"))

