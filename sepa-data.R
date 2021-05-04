
# Accessing SEPA open data services using R
library(httr)
library(sf)
library(tmap)

url <- parse_url("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services")
url$path <- paste(url$path, "USA_Railroads_1/FeatureServer/0/query", sep = "/")
url$query <- list(where = "STATE = 'FL'",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)

Florida_Railroads <- st_read(request)

tmap_mode(mode = "view")
tm_shape(Florida_Railroads)+tm_lines(col="NET_DESC", palette = "Set1", lwd = 5)

In R




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
