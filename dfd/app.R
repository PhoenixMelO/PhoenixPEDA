---
  title: "Models of Neighborhood Change in San Diego"
author: "Melanie Phoenix"
date: "2022-12-02"
output:
  flexdashboard::flex_dashboard:
  theme: readable
source: embed
smart: false
runtime: shiny
---

```{r, echo=FALSE}
# LOAD PACKAGES

library(dplyr)         #data wrangling
library(DT)            #formatting output
library(flexdashboard) #dashboard layout, widgets, and uploading
library(geojsonio)     #maps
library(ggmap)         #maps
library(knitr)         #formatting output
library(leaflet)       #maps
library(pals)          #maps
library(pander)        #formatting output
library(rgdal)         #maps
library(rsconnect)     #dashboard layout, widgets, and uploading
library(sf)            #maps
library(shiny)         #dashboard layout, widgets, and uploading
library(sp)            #maps
library(stargazer)     #formatting output
library(tmap)          #maps
library(viridis)       #maps
```

```{r, include=FALSE}
# DATA STEPS
# from local file path
san <- geojson_read( "san.geojson", what="sp" )
# reproject the map
san2 <- spTransform( san, CRS("+init=epsg:3395") )
# convert the sp map format to
# an sf (simple features) format:
# ggmap requires the sf format
san.sf <- st_as_sf( san2 )
# separate out the data frame from the map
sdd <- as.data.frame( san.sf )
URL1 <- "https://github.com/DS4PS/cpp-529-fall-2020/raw/main/LABS/data/rodeo/LTDB-2000.rds"
d5 <- readRDS( gzcon( url( URL1 ) ) )
#
URL2 <- "https://github.com/DS4PS/cpp-529-fall-2020/raw/main/LABS/data/rodeo/LTDB-2010.rds"
d6 <- readRDS( gzcon( url( URL2 ) ) )
#
URLmd <- "https://github.com/DS4PS/cpp-529-fall-2020/raw/main/LABS/data/rodeo/LTDB-META-DATA.rds"
md <- readRDS( gzcon( url( URLmd ) ) )
#
d5 <- select( d5, - year )
d6 <- select( d6, - year )
#
d <- merge( d5, d6, by="tractid" )
d <- merge( d, md, by="tractid" )
#
#table( d$urban )
#
#d <- filter( d, urban == "urban" )
#
d <- select( d, tractid,
             mhmval00, mhmval12,
             hinc00,
             hu00, vac00, own00, rent00, h30old00,
             empclf00, clf00, unemp00, prof00,
             dpov00, npov00,
             ag25up00, hs00, col00,
             pop00.x, nhwht00, nhblk00, hisp00, asian00,
             cbsa, cbsaname )

#
these.variables <- c( "pnhwht12", "pnhblk12", "phisp12", "pntv12", "pfb12", "polang12", "phs12", "pcol12",
                      "punemp12", "pflabf12", "pprof12", "pmanuf12", "pvet12", "psemp12", "hinc12", "incpc12",
                      "ppov12", "pown12", "pvac12", "pmulti12", "mrent12", "mhmval12", "p30old12", "p10yrs12",
                      "p18und12", "p60up12", "p75up12", "pmar12", "pwds12", "pfhh12" )


Short.names <- c( "Caucasian %", "African American %", "Hispanic %", "Native American %", "Foreign-Born %",
                  "Non-English Homes %", "High School %", "College Degree %", "Unemployed %", "Female Worker %",
                  "Professional  %", "Manufacturer %", "Veteran %", "Entrepreneur %", "Med Home Income",
                  "Per Capita Income", "Poverty %", "Home Owner %", "Vacant %", "Multi-Family Homes %", "Med Rent",
                  "Med Home Value", "30+ yr old Homes %", "HH under 10yr Homes %", "Below 17yrs %", "60+ yrs %",
                  "75+ yrs %", "Married %", "Separated %", "Female HH %" )
#
names(these.variables) <- c("Caucasian %", "African American %", "Hispanic %", "Native American %",
                            "Foreign-Born %", "Non-English Homes %", "High School %", "College Degree %",
                            "Unemployed %", "Female Worker %", "Professional  %", "Manufacturer %",
                            "Veteran %", "Entrepreneur %", "Med Home Income", "Per Capita Income",
                            "Poverty %", "Home Owner %", "Vacant %", "Multi-Family Homes %", "Med Rent",
                            "Med Home Value", "30+ yr old Homes %", "HH under 10yr Homes %", "Below 17yrs %",
                            "60+ yrs %", "75+ yrs %", "Married %", "Separated %", "Female HH %")
#
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
#mhv.00[ mhv.00 < 1000 ] <- NA
# change in MHV in percent
mhv.growth <- 100 * ( mhv.change / mhv.00 )
#
d$mhv.00 <- mhv.00
d$mhv.10 <- mhv.10
d$mhv.change <- mhv.change
d$mhv.growth <- mhv.growth
# geoid-01 is the hypothetical name of tract ID in the shapefile
# geoid-02 is the hypothetical name of tract ID in the census dataset
# dorling must be an sp shapefile
dd.URL <- "https://raw.githubusercontent.com/DS4PS/cpp-529-master/master/data/data-dictionary.csv"
data.dictionary <- read.csv( dd.URL, stringsAsFactors=F )
# data.dictionary must have "LABEL" for var name and "VARIABLE" for description
data.dictionary <- data.dictionary[ -1 , ] # drop the original column names
#
data.label <- data.dictionary[ data.dictionary$LABEL %in% these.variables , ]
value <- data.label$LABEL
dd.name <- data.label$VARIABLE
# x <- dd.name
# names(x) <- value
xnames <- dd.name
names(xnames) <- value
#
#
bb <- st_bbox( c( xmin = -13055498, xmax = -13011238,
                  ymax = 3853073, ymin = 3817112 ),
               crs = st_crs("+init=epsg:3395"))
MHV_Values <- c("mhv.00", "mhv.10", "mhv.change", "mhv.growth")
MHV_Labels <- c("Median Home Value 2000","Median Home Value 2010","Value Change 2000-2010","Growth in Home Value")
button.labels <- c("Median Home Value 2000","Median Home Value 2010","Value Change 2000-2010","Growth in Home Value")
button.values <-c("mhv.00", "mhv.10", "mhv.change", "mhv.growth")
names(button.values) <- c("Median Home Value 2000","Median Home Value 2010","Value Change 2000-2010","Growth in Home Value")
MHV.mix <- names(button.values) <- c("Median Home Value 2000","Median Home Value 2010","Value Change 2000-2010","Growth in Home Value")
#
covariates <- these.variables
covariates.labels <- Short.names
dv <- c("mhv.00", "mhv.10", "mhv.change", "mhv.growth")
#
dvcov.labels <- c("Median Home Value 2000","Median Home Value 2010","Value Change 2000-2010","Growth in Home Value",
                  "Caucasian %", "African American %", "Hispanic %", "Native American %", "Foreign-Born %",
                  "Non-English Homes %", "High School %", "College Degree %", "Unemployed %", "Female Worker %",
                  "Professional  %", "Manufacturer %", "Veteran %", "Entrepreneur %", "Med Home Income",
                  "Per Capita Income", "Poverty %", "Home Owner %", "Vacant %", "Multi-Family Homes %", "Med Rent",
                  "Med Home Value", "30+ yr old Homes %", "HH under 10yr Homes %", "Below 17yrs %", "60+ yrs %",
                  "75+ yrs %", "Married %", "Separated %", "Female HH %")

dvcov.values <- c("mhv.00", "mhv.10", "mhv.change", "mhv.growth", "pnhwht12", "pnhblk12", "phisp12", "pntv12",
                  "pfb12", "polang12", "phs12", "pcol12", "punemp12", "pflabf12", "pprof12", "pmanuf12", "pvet12",
                  "psemp12", "hinc12", "incpc12", "ppov12", "pown12", "pvac12", "pmulti12", "mrent12", "mhmval12",
                  "p30old12", "p10yrs12", "p18und12", "p60up12", "p75up12", "pmar12", "pwds12", "pfhh12")

#
#

```


Community Demographics
=====================================

  Inputs {.sidebar}
-------------------------------------

```{r}
radioButtons( inputId="demographics",
              label = h3("Census Variables"),
              choices =  these.variables,
              #              choiceNames= short.names,
              #              choiceValues= these.variables,
              selected="punemp12")
```

Row {.tabset}
-------------------------------------


  ### Choropleth Map San Diego

```{r}
renderPlot({

  # split the selected variable into deciles

  get_data <-
    reactive({
      san.sf <-
        san.sf %>%
        mutate( q = ntile( get(input$demographics), 10 ) )
    })


  ggplot( get_data() ) +
    geom_sf( aes( fill = q ), color=NA ) +
    coord_sf( datum=NA ) +
    labs( title = paste0( "Choropleth of Select Demographics: ", toupper(names(these.variables[which(input$demographics == these.variables)]))),
          caption = "Source: Harmonized Census Files",
          fill = "Population Deciles" ) +
    scale_fill_gradientn( colours=rev(ocean.balance(10)), guide = "colourbar" ) +
    xlim( xmin = -13055498, xmax = -13011238 ) +
    ylim( ymin = 3817112, ymax = 3853073 )

})
```

### Variable Distribution

```{r}
renderPlot({

  # extract vector x from the data frame
  # x <-  d[ "punemp12" ] %>% unlist()


  get_variable_x <- reactive({ sdd[ input$demographics ] })

  x <- get_variable_x() %>% unlist() %>% as.numeric()

  cut.points <- quantile( x, seq( 0, 1, 0.1 ) )

  hist( x, breaks=50,
        col="gray", border="white", yaxt="n",
        main=paste0( "Histogram of variable ", toupper(names(these.variables[which(input$demographics == these.variables)])) ),
        xlab="red lines represent decile cut points" )

  abline( v=cut.points, col="darkred", lty=3, lwd=2 )


})
```


Neighborhoods
=====================================

  ### Clusters

 ```{r}
# define the bounding box corners
bb <- st_bbox( c( xmin = -13055498, xmax = -13011238,
                  ymax = 3853073, ymin = 3817112 ),
               crs = st_crs("+init=epsg:3395"))
# ADD YOUR CUSTOM LABELS TO THE CLUSTERS
san2$cluster[ san2$cluster == "1" ] <- "Native American Veterans"
san2$cluster[ san2$cluster == "2" ] <- "Girl Power"
san2$cluster[ san2$cluster == "3" ] <- "Black Girl Magic"
san2$cluster[ san2$cluster == "4" ] <- "Old School"
san2$cluster[ san2$cluster == "5" ] <- "Habla en Espano"
san2$cluster[ san2$cluster == "6" ] <- "Cali Life"
#
renderTmap({
  #
  tmap_mode("view")
  tm_basemap( "CartoDB.Positron" )
  tm_shape( san2, bbox=bb ) +
    tm_polygons( col="cluster", palette="Accent",
                 title="Community Types" )
  #
})
```


NH Change 2000-2010
=====================================



  Inputs {.sidebar}
-------------------------------------

```{r}
button.labels <- c("Median Home Value 2000","Median Home Value 2010","Value Change 2000-2010","Growth in Home Value")
button.values <- c("mhv.00", "mhv.10", "mhv.change", "mhv.growth")
#button.labels <- MHV_Labels
#button.values <- MHV_Values
names(button.values) <- c("Median Home Value 2000","Median Home Value 2010","Value Change 2000-2010","Growth in Home Value")

radioButtons( inputId="home.value",
              label = h3("Home Values"),
              choices = button.values,
              #              choiceNames=button.labels,
              #              choiceValues=button.values,
              selected="mhv.change")
```




Row {.tabset}
-------------------------------------



  ### Median Home Values

  ```{r}

renderPlot({

  # split the selected variable into deciles


  get_data <-
    reactive({
      san.sf <-
        san.sf %>%
        mutate( q = ntile( get(input$home.value), 10 ) )
    })



  ggplot( get_data() ) +
    geom_sf( aes( fill = q ), color=NA ) +
    coord_sf( datum=NA ) +
    labs( title = paste0( "Spatial Distribution of Home Values: ", toupper(names(these.variables[which(input$demographics == these.variables)]))),
          caption = "Source: Harmonized Census Files",
          fill = "Home Value Deciles" ) +
    scale_fill_gradientn( colours=rev(ocean.balance(10)), guide = "colourbar" ) +
    xlim( xmin = -13055498, xmax = -13011238 ) +
    ylim( ymin = 3817112, ymax = 3853073 )

})


```
```{r}
head(button.values)
```


### Variable Distribution

```{r}



renderPlot({

  # extract vector x from the data frame
  # x <-  d[ "pnhwht12" ] %>% unlist()


  get_variable_x <- reactive({ d[ input$home.value ] })

  x <- get_variable_x() %>% unlist()

  cut.points <- quantile( x, seq( 0, 1, 0.1 ), na.rm = TRUE )

  hist( x, breaks=50,
        col="gray", border="white", yaxt="n",
        main=paste0( "Histogram of ", toupper(names(button.values[which(input$home.value == button.values)]))),
        xlab="red lines represent decile cut points" )

  abline( v=cut.points, col="darkred", lty=3, lwd=2 )


})
```

Drivers of Change
=====================================



  Inputs {.sidebar}
-------------------------------------

  ```{r}
button.labels <- c("Median Home Value 2000","Median Home Value 2010","Value Change 2000-2010","Growth in Home Value")
button.values <- c("mhv.00", "mhv.10", "mhv.change", "mhv.growth")
button.labels <- MHV_Labels
button.values <- MHV_Values
#
radioButtons( inputId="dv",
              label = h3("Select Your Dependent Variable"),
              choiceNames=button.labels,
              choiceValues=button.values,
              selected="mhv.10")
#
checkboxGroupInput( inputId="covariates",
                    label = h3("Select Variables for Your Model"),
                    choices = dvcov.values,
                    #              choiceNames=dvcov.labels,
                    #              choiceValues=dvcov.values,
                    selected=c("pnhwht12","pprof12","pvac12") )
#

#
```

```{r}
head(covariates)
```


Row {.tabset}
-------------------------------------



  ### Predicting Change

  ```{r, results='asis'}
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
    pov.rate = 100 * npov00 / dpov00)


# RUNNING A REGRESSION WITH USER INPUTS
#
# create a formula object
# by constructing the formula from user selections
# as a string then casting as a formula object
# x.s <- c("x1","x2","x3" )
# formula.text <- paste( "y", " ~ ", paste0( x.s, collapse=" + ") )
# formula.object <- as.formula( formula.text )
#
# lm( formula.object, data=d )
#
# make sure all variables are in d
# check boxes return vectors
get_covariates <- reactive({ input$covariates})
#get_covariates <- reactiveValues({ input$covariates })
get_dv <- reactive({ input$dv })

renderUI({

  co_variates <- get_covariates()
  dep_vars <- get_dv()

  formula.text <- paste0( dep_vars, " ~ ", paste( co_variates, collapse=" + " ) )
  fo <- as.formula( formula.text )

  m <- lm( fo, data=d )

  # HTML( "<div style='width: 60%; margin: 0px auto;'>" )
  HTML(
    #
    c("<br><br><br>",
      "<div type='regression' style='width: 60%; margin: 0px auto;'>",
      stargazer( m, type="html", omit.stat=c("rsq","f") ),
      "</div>",
      "<br><br><br>"
    ))
})
#
# HTML( reg.table )
```

### Correlation Plots

```{r}
renderPlot({
  co_variates <- get_covariates()
  dep_vars <- get_dv()
  dcorr <- data.frame(dvcov.labels, dvcov.values)
  pairs( dcorr[dvcov.values] )
})

```






<style>

  .chart-shim { overflow: auto; }

table{
  border-spacing:1px;
  margin-top:30px;
  margin-bottom:30px;
  margin-left: auto;
  margin-right: auto;
  align:center}

td{ padding: 6px 10px 6px 10px }

th{ text-align: left; }

</style>

  ```{r}


```


