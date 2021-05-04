
# remotes::install_github("ropensci/sofa")
# install.packages(c("sofa", "jsonlite", "tidyverse"))
library(sofa)
library("jsonlite")
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(devtools)
devtools::install_github("aquaMetrics/rict")
devtools::install_github("aquaMetrics/macroinvertebratesMetrics")
library(rict)
library(macroinvertebrateMetrics)

# Credentials
credentials <- readLines("credentials.txt", n = 2)
(x <- Cushion$new(user = credentials[1], pwd = credentials[2], transport = "http"))


# Get question
# Get form
# Get task
# Get project
# Get question template
# Get form template
# Get task template
# Get task template form?
# Get task template question?
# Get project template

# Get everything??!

# Get task and all related data? - i.e. calculate a task level metric?
# Move data in relational database?
# Reporting view?
# Go from question to form > task > project? etc? i.e events - need to jump between different levels?
# Get responses etc?


# indexes? Most common search terms? Location, date, catchment, wb..?

# http://map.sepa.org.uk/arcgis/rest/services/WMS_Environmental_Monitoring/MapServer/11/query?where=YEAR%3D2020&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=html


test <- read_file(
  file = "http://map.sepa.org.uk/arcgis/rest/services/WMS_Environmental_Monitoring/MapServer/11/query?where=YEAR%3D2020&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
  locale = default_locale())


data <- jsonlite::fromJSON(test, flatten = T)
data$features


# library(httr)
# library(sf)
# library(tmap)
#
# url <- parse_url("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services")
# url$path <- paste(url$path, "USA_Railroads_1/FeatureServer/0/query", sep = "/")
# url$query <- list(where = "STATE = 'FL'",
#                   outFields = "*",
#                   returnGeometry = "true",
#                   f = "geojson")
# request <- build_url(url)
#
# Florida_Railroads <- st_read(request)
#
# tmap_mode(mode = "view")
# tm_shape(Florida_Railroads)+tm_lines(col="NET_DESC", palette = "Set1", lwd = 5)


task_template <- db_query(
  cushion = x,
  dbname = "kelpie",
  selector = list(data.title = "Demo",
                  data.type = "task-template")
  )



all_forms <- db_query(
  cushion = x,
  dbname = "kelpie",
  selector = list(data.task = list(`$eq` = form$data$task))
)
