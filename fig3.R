library(sf)
library(sp)
library (rgdal)
library(shinyjs)
library(tmaptools) 
library(tmap)  #time-cost expensive library
library(classInt)
library(plyr)


##----------------London Borough boundary and market plot ------------##
london_borough_33_OGR <- readOGR(dsn="./statistical-gis-boundaries-london/ESRI",layer="London_Borough_Excluding_MHW") 
london_borough_33 <- readxl::read_xlsx("./user_data/inner_outer_division_v1.xlsx")
london_borough_33_attr <- london_borough_33[,-c(1)]
london_borough_33_OGR <- append_data(london_borough_33_OGR, london_borough_33_attr, key.shp="GSS_CODE", key.data="GSS_CODE")

HSM <- readxl::read_xlsx("./user_data/Update_HSM_Coordinates.xlsx")
x<-HSM$E
y<-HSM$N
id<-HSM$ID
HSM_geoData <- data.frame(id,x,y)
HSM_geocoord <- st_as_sf(HSM_geoData, coords = c("x", "y"), crs = 4326)

##-------------opportunity areas and areas for intensification in inner London
#draw pattern  https://github.com/r-tmap/tmap/issues/49
library(cartography)
OABoundary <- st_read(dsn="./user_data/14_OA_Boundary",layer="converted_OA")
AOIBoundary <- st_read(dsn="./user_data/15_AoI_Boundary",layer="converted_AoI")

patterns <- c("diamond","grid","hexagon","horizontal", "vertical", "zigzag","left2right","right2left","circle")

OABoundaryHatched <- hatchedLayer(OABoundary,mode= "sfc", pattern = "left2right", density = 10)
AOIBoundaryHatched <- hatchedLayer(AOIBoundary,mode= "sfc", pattern = "right2left", density = 10)

OABoundaryHatchedSF <- st_sf(geometry = OABoundaryHatched)
AOIBoundaryHatchedSF <- st_sf(geometry = AOIBoundaryHatched)



classBreaks <- c(-0.5,0.5,1.5)
classLabels <- c("outer London", "inner London")
classPalette <- c("#dddddd","#ffffff")

tm_shape(london_borough_33_OGR)+
  tm_fill(col = c("INNER_TAG"),
          palette=classPalette,
          breaks = classBreaks,
          labels = classLabels,
          title="London division",
          alpha=1)+
  tm_borders(lty = "solid", lwd = 0.5, col = "#444444")+
  
  tm_shape(OABoundaryHatchedSF)+
  tm_lines(col = "#ed8a0a")+ 
  
  tm_shape(AOIBoundaryHatchedSF)+
  tm_lines(col = "#492b7c")+
  
  tm_shape(OABoundary)+
  tm_borders(lty = "solid", lwd = 1, col = "#ed8a0a") +
  
  tm_shape(AOIBoundary)+
  tm_borders(lty = "solid", lwd = 1, col = "#492b7c")+

  tm_shape(HSM_geocoord) +
  tm_dots(col = c("#f24a72"),
          auto.palette.mapping = FALSE,
          size = 0.125)
