
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


URL1 <- "https://github.com/DS4PS/cpp-529-fall-2020/raw/main/LABS/data/rodeo/LTDB-2000.rds"
d1 <- readRDS( gzcon( url( URL1 ) ) )

URL2 <- "https://github.com/DS4PS/cpp-529-fall-2020/raw/main/LABS/data/rodeo/LTDB-2010.rds"
d2 <- readRDS( gzcon( url( URL2 ) ) )

URLmd <- "https://github.com/DS4PS/cpp-529-fall-2020/raw/main/LABS/data/rodeo/LTDB-META-DATA.rds"
md <- readRDS( gzcon( url( URLmd ) ) )

d1 <- select( d1, - year )
d2 <- select( d2, - year )

d <- merge( d1, d2, by="tractid" )
d <- merge( d, md, by="tractid" )

table( d$urban )

d <- filter( d, urban == "urban" )

d <- select( d, tractid, 
             mhmval00, mhmval12, 
             hinc00, 
             hu00, vac00, own00, rent00, h30old00,
             empclf00, clf00, unemp00, prof00,  
             dpov00, npov00,
             ag25up00, hs00, col00, 
             pop00.x, nhwht00, nhblk00, hisp00, asian00,
             cbsa, cbsaname )


d <- 
  d %>%
  mutate( # percent white in 2000
    p.white = 100 * nhwht00 / pop00.x,
    # percent black in 2000
    p.black = 100 * nhblk00 / pop00.x,
    # percent hispanic in 2000
    p.hisp = 100 * hisp00 / pop00.x, 
    # percent asian in 2000
    p.asian = 100 * asian00 / pop00.x,
    # percent high school grads by age 25 in 2000 
    p.hs = 100 * (hs00+col00) / ag25up00,
    # percent pop with college degree in 2000
    p.col = 100 * col00 / ag25up00,
    # percent employed in professional fields in 2000
    p.prof = 100 * prof00 / empclf00,
    # percent unemployment  in 2000
    p.unemp = 100 * unemp00 / clf00,
    # percent of housing lots in tract that are vacant in 2000
    p.vacant = 100 * vac00 / hu00,
    # dollar change in median home value 2000 to 2010 
    pov.rate = 100 * npov00 / dpov00 )


# adjust 2000 home values for inflation 
mhv.00 <- d$mhmval00 * 1.28855  
mhv.10 <- d$mhmval12

# change in MHV in dollars
mhv.change <- mhv.10 - mhv.00


# drop low 2000 median home values
# to avoid unrealistic growth rates.
#
# tracts with homes that cost less than
# $1,000 are outliers
mhv.00[ mhv.00 < 1000 ] <- NA

# change in MHV in percent
mhv.growth <- 100 * ( mhv.change / mhv.00 )

d$mhv.00 <- mhv.00
d$mhv.10 <- mhv.10
d$mhv.change <- mhv.change
d$mhv.growth <- mhv.growth 

data.dictionary <- 
  structure(list(LABEL = c("tractid", "pnhwht12", "pnhblk12", "phisp12", "pntv12", "pfb12", "polang12", "phs12", "pcol12", "punemp12", "pflabf12", "pprof12", "pmanuf12", "pvet12", "psemp12", "hinc12", "incpc12", "ppov12", "pown12", "pvac12", "pmulti12", "mrent12", "mhmval12", "p30old12", "p10yrs12", "p18und12", "p60up12", "p75up12", "pmar12", "pwds12", "pfhh12"), VARIABLE = c("GEOID", "Percent white, non-Hispanic", "Percent black, non-Hispanic", "Percent Hispanic", "Percent Native American race", "Percent foreign born", "Percent speaking other language at home, age 5 plus", "Percent with high school degree or less", "Percent with 4-year college degree or more", "Percent unemployed", "Percent female labor force participation", "Percent professional employees", "Percent manufacturing employees", "Percent veteran", "Percent self-employed", "Median HH income, total", "Per capita income", "Percent in poverty, total", "Percent owner-occupied units", "Percent vacant units", "Percent multi-family units", "Median rent", "Median home value", "Percent structures more than 30 years old", "Percent HH in neighborhood 10 years or less", "Percent 17 and under, total", "Percent 60 and older, total", "Percent 75 and older, total", "Percent currently married, not separated", "Percent widowed, divorced and separated", "Percent female-headed families with children")), class = "data.frame", row.names = c(NA, -31L))

these.variables <- c("pnhwht12", "pnhblk12", "phisp12", "pntv12", "pfb12", "polang12", 
                     "phs12", "pcol12", "punemp12", "pflabf12", "pprof12", "pmanuf12", 
                     "pvet12", "psemp12", "hinc12", "incpc12", "ppov12", "pown12", 
                     "pvac12", "pmulti12", "mrent12", "mhmval12", "p30old12", "p10yrs12", 
                     "p18und12", "p60up12", "p75up12", "pmar12", "pwds12", "pfhh12")

Detail.names = c("Percent white, non-Hispanic", "Percent black, non-Hispanic", "Percent Hispanic", "Percent Native American race", "Percent foreign born", "Percent speaking other language at home, age 5 plus", "Percent with high school degree or less", "Percent with 4-year college degree or more", "Percent unemployed", "Percent female labor force participation", "Percent professional employees", "Percent manufacturing employees", "Percent veteran", "Percent self-employed", "Median HH income, total", "Per capita income", "Percent in poverty, total", "Percent owner-occupied units", "Percent vacant units", "Percent multi-family units", "Median rent", "Median home value", "Percent structures more than 30 years old", "Percent HH in neighborhood 10 years or less", "Percent 17 and under, total", "Percent 60 and older, total", "Percent 75 and older, total", "Percent currently married, not separated", "Percent widowed, divorced and separated", "Percent female-headed families with children")

d1 <- san@data
d2 <- select( d1, these.variables )
d3 <- apply( d2, 2, scale )
head( d3[,1:6] ) %>% pander()

# library( mclust )
set.seed( 1234 )
fit <- Mclust( d3 )
san$cluster <- as.factor( fit$classification )
summary( fit )

#plot(fit, what = "classification")

# define the bounding box corners 
bb <- st_bbox( c( xmin = -13055498, xmax = -13011238, 
                  ymax = 3853073, ymin = 38171124 ), 
               crs = st_crs("+init=epsg:3395"))

#d2$cluster <- d2$cluster <- as.factor(paste0("Group-", fit$classification))

df.pct <- sapply( d2, ntile, 100 )
d3 <- as.data.frame( df.pct )
d3$cluster <- as.factor( paste0("GROUP-",fit$classification) )

stats <- 
  d3 %>% 
  group_by( cluster ) %>% 
  summarise_each( funs(mean) )

t <- data.frame( t(stats), stringsAsFactors=F )
names(t) <- paste0( "GROUP.", 1:6 ) 
t <- t[-1,]

for( i in 1:6 )
{
  z <- t[,i]
  plot( rep(1,30), 1:30, bty="n", xlim=c(-75,100), 
        type="n", xaxt="n", yaxt="n",
        xlab="Percentile", ylab="",
        main=paste("GROUP",i) )
  abline( v=seq(0,100,25), lty=3, lwd=1.5, col="gray90" )
  segments( y0=1:30, x0=0, x1=100, col="gray70", lwd=2 )
  text( -0.2, 1:30, data.dictionary$VARIABLE[-1], cex=0.85, pos=2 )
  points( z, 1:30, pch=19, col="firebrick", cex=1.5 )
  axis( side=1, at=c(0,50,100), col.axis="gray", col="gray" )
