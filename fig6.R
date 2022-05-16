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
HSM <- readxl::read_xlsx("./user_data/Update_HSM_Coordinates.xlsx")
x<-HSM$E
y<-HSM$N
id<-HSM$ID
HSM_geoData <- data.frame(id,x,y)
HSM_geocoord <- st_as_sf(HSM_geoData, coords = c("x", "y"), crs = 4326)

##---------------------house price --------------------------##
london <- readOGR(dsn="./statistical-gis-boundaries-london/ESRI",layer="LSOA_2011_London_gen_MHW")
#london <- readOGR(dsn="./published_data",layer="shp")
housePriceDataFrame <- readxl::read_xlsx("./user_data/fig6_lsoa_data_HousingPrice.xlsx")
atlas <- read.csv("./published_data/atlas.csv")
housePriceDataFrame_4835 <- join(atlas,housePriceDataFrame,by="Codes")

housePrice <- housePriceDataFrame_4835$housePrice
naturalClassLabel <- seq(1:nrow(housePriceDataFrame_4835))

classificationInfo <- classIntervals(var = housePrice[!is.na(housePrice)], n = 4, style = 'jenks', warnLargeN = F)
classBounds <- classificationInfo$brks

NARowNum <- 0 
for (i in 1:nrow(housePriceDataFrame_4835)) {
  if(is.na(housePrice[i])){
    #print(paste("the ",i,"the element is na"))
    naturalClassLabel[i] = 1
    NARowNum <- NARowNum+1
  }
  else{
    for(j in 2:5){
      if(housePrice[i]>classBounds[j-1] && housePrice[i]<classBounds[j]){
        naturalClassLabel[i] = j
        break
      }
    }
  }
}

LOSA_2011_housePriceWithNaturalBreaksLables <- data.frame(housePriceDataFrame_4835$Codes,
                                                          naturalClassLabel,
                                                          housePriceDataFrame_4835$housePrice,
                                                          housePriceDataFrame_4835$Names)
colnames(LOSA_2011_housePriceWithNaturalBreaksLables) <- c("Codes","naturalClassLabel","housePrice","Names")

london <- append_data(london, LOSA_2011_housePriceWithNaturalBreaksLables, key.shp="LSOA11CD", key.data="Codes")

classBreaks <- c(0,1.5,2.5,3.5,4.5,5.5)
classLabels <- c("missing", "class 1", "class 2","class 3","class 4")
classPalette <- c("#dddddd","#dfd5da","#827397" ,"#4d4c7d","#362d62")
#classPalette <- c("#dddddd","#FF0000","#00FF00" ,"#0000FF","#1a374d")

tm_shape(london) +
  tm_fill(col = c("naturalClassLabel"),
          palette=classPalette,
          breaks = classBreaks,
          labels = classLabels,
          title="housePrice", 
          alpha=1)+
  
  tm_shape(london_borough_33_OGR)+ 
  tm_borders(lty = "solid", lwd = 1, col = "#FFFFFF")+
  
  tm_shape(HSM_geocoord) +
  tm_dots(col = c("#f24a72"),
          auto.palette.mapping = FALSE,
          size = 0.125)

