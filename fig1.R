# change the default working directory from RStudio menu: Tools -> Global options -> General -> click on "Browse" ->select a directory and apply
# interactive operation in map with multiple overlay.

##==========================1. data preparing============================
library(sf)

london_borough_33 <- readxl::read_xlsx("./user_data/inner_outer_division_v1.xlsx")
london_borough_33_attr <- london_borough_33[,-c(1)]

HSM <- readxl::read_xlsx("./user_data/Update_HSM_Coordinates.xlsx")
x<-HSM$E
y<-HSM$N
id<-HSM$ID
HSM_geoData <- data.frame(id,x,y)
HSM_geocoord <- st_as_sf(HSM_geoData, coords = c("x", "y"), crs = 4326)

##==========================2. plot regular map ============================
#dependence （install package no newer than 2017）
# 1, remotes::install_github("r-quantities/units", ref="v0.7-0")  https://github.com/r-quantities/units/branches
# 2, remotes::install_github("r-spatial/lwgeom", ref = "minimal_travis") https://github.com/r-spatial/lwgeom/issues/52
# 3, remotes::install_github("r-tmap/tmaptools", ref = "v2") Caution!!! it is very important here to install a certain branch or version of package instead of the default newest.
# 4, remotes::install_github("r-tmap/tmap", ref = "v2"). remotes::install_github("r-spatial/s2") https://github.com/r-spatial/s2/pull/142

library(sp)
library (rgdal)
library(shinyjs)
library(tmaptools) 
library(tmap)  #time-cost expensive library


#import the shp of london lsoa (shp file). download statistical-gis-boundaries-london.zip from https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
london_borough_33_OGR <- readOGR(dsn="./statistical-gis-boundaries-london/ESRI",layer="London_Borough_Excluding_MHW")  #outer london 20 features; inner london 13 features.
london_borough_33_OGR <- append_data(london_borough_33_OGR, london_borough_33_attr, key.shp="GSS_CODE", key.data="GSS_CODE")

#tm_fill can default show 4 facets. And the limits is 6 vis setting tmap_options(limits = c(facets.view = 6))

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
  
  tm_shape(HSM_geocoord) +
  
  tm_dots(col = c("#f24a72"),
          auto.palette.mapping = FALSE,
          size = 0.5) +
  
  tm_text(text = "id", size=0.3, col = "#ffffff")


#rgb color from dec to hex
cols<-c(73,43,124)
print(paste(as.hexmode(cols[1]),as.hexmode(cols[2]),as.hexmode(cols[3])))
