#
rm(list=ls())
library(httr)

# the file -credentials.data- is just a tab separated file
# with coloumn names api_key and app_id
credentials <- read.table('data/credentials.data',header = TRUE,as.is=TRUE)
APP_KEY <- credentials$api_key 
APP_ID  <- credentials$app_id

telecom_get_data <- function(showUri=FALSE,...) {
  
  # Making the request
  query <- paste0(
    'https://api.dandelion.eu/datatxt/nex/v1/?',
    'text=','The%20doctor%20says%20an%20apple%20is%20better%20than%20an%20orange',
    '&include=','types%2Cabstract%2Ccategories',
    '&$app_id=',APP_ID,
    '&$app_key=',APP_KEY)
  
  if (showUri) message(query)
  req   <- GET(query)
  
  # Returning the ouput
  statu <- status_code(req)
  if (statu==200) return(content(req))
  else return(NULL)
}

x <- telecom_get_data()

library(spatstat)
library(maptools)
shape.shp <- readShapeSpatial('data/grids/napoli-grid/intersection_Napoli_W_GRIDIT_NEW')
shape     <- as(shape.shp, 'SpatialPolygons')
shape.df  <- shape.shp@data


# File:  _AGE_RANGE - In this file each record gives a count of users with an
# age among these six different age-ranges {[<18],[18-30],[31-40],[41-50],
# [51-60],[>60]} that generates events in a given space/timeframe. Records have
# been generated only if the count resulting was greater than 3.
# 
# Time interval: the beginning of the time interval expressed as the number of
# millisecond elapsed from the Unix Epoch on January 1st, 1970 at UTC. The end
# of the time interval can be obtained by adding 900000 milliseconds (15 minutes
# ) to this value. TYPE: numeric
#
# Square id: id of the square that is part of a grid. TYPE: numeric
# Age-Range: age-range of the users
# Number of users belonging to the gender: the number of users belonging to the
# same age-range

# The square_id is not completely numeric, that's why I have to process it later
demo_napoli_gender <- read.table(
  'data/demographics/callsLM_NA_AGE_RANGE',sep='\t', 
  col.names  = c('time_interval', 'square_id', 'age_range', 'n_users'),
  colClasses = c('integer','character','factor','numeric'),
  as.is      = TRUE, nrows = 1000000)

# Some basic stats
tab <- as.data.frame(table(demo_napoli_gender$age_range), responseName = 'n')
with(tab, barplot(n,names.arg = Var1))

# Fax CoveredCA
# 888 329 3700