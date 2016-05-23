## ----setup, echo=FALSE---------------------------------------------------
library(knitr)
opts_chunk$set(eval = FALSE,
               warning = FALSE,
               message = FALSE)


## ---- message=FALSE------------------------------------------------------
#  library(maptools)
#  
#  # shapeFile contains the path to the shp file:
#  shapeFile <- "national-esri-16122011/COM20111216_ELB_region.shp"
#  sF <- readShapeSpatial(shapeFile)
#  class(sF)

## ---- message=FALSE------------------------------------------------------
#  require(rmapshaper)

## ---- message=FALSE------------------------------------------------------
#  sFsmall <- ms_simplify(sF, keep=0.05) # use instead of thinnedSpatialPoly

## ---- cache=TRUE, message=FALSE------------------------------------------
#  plot(sFsmall)

## ---- message=FALSE------------------------------------------------------
#  nat_data <- sF@data
#  head(nat_data)

## ---- message=FALSE------------------------------------------------------
#  nat_data$id <- row.names(nat_data)

## ------------------------------------------------------------------------
#  nat_map <- ggplot2::fortify(sFsmall)
#  head(nat_map)
#  

## ------------------------------------------------------------------------
#  nat_map$group <- paste("g",nat_map$group,sep=".")
#  nat_map$piece <- paste("p",nat_map$piece,sep=".")

## ------------------------------------------------------------------------
#  write.csv(nat_map, "National-map-2013.csv", row.names=FALSE)

## ------------------------------------------------------------------------
#  polys <- as(sF, "SpatialPolygons")
#  class(polys) # should be SpatialPolygons
#  length(polys) # should be 150

## ------------------------------------------------------------------------
#  slotNames(polys)

## ------------------------------------------------------------------------
#  Polygon(polys[1])

## ---- message=FALSE, warning=FALSE---------------------------------------
#  library(dplyr)
#  centroid <- function(i, polys) {
#    ctr <- Polygon(polys[i])@labpt
#    data.frame(long_c=ctr[1], lat_c=ctr[2])
#  }
#  centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
#  head(centroids)

## ---- message=FALSE, warning=FALSE---------------------------------------
#  nat_data <- data.frame(nat_data, centroids)
#  write.csv(nat_data, "National-data-2013.csv", row.names=FALSE)

## ---- message=FALSE, warning=FALSE---------------------------------------
#  library(ggplot2)
#  library(ggthemes)
#  ggplot(aes(map_id=id), data=nat_data) +
#    geom_map(aes(fill=AREA_SQKM), map=nat_map) +
#    expand_limits(x=nat_map$long, y=nat_map$lat) +
#    theme_map()

