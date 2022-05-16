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

##---------------------areas of deprivation --------------------------##
london <- readOGR(dsn="./statistical-gis-boundaries-london/ESRI",layer="LSOA_2011_London_gen_MHW")
#london <- readOGR(dsn="./published_data",layer="shp")

IMDDataFrame <- readxl::read_xlsx("./user_data/fig4_ID_2019_for_London.xlsx")
atlas <- read.csv("./published_data/atlas.csv")
IMDDataFrame_4835 <- join(atlas,IMDDataFrame,by="Codes")

IMD <- IMDDataFrame_4835$IMD
classLabel <- seq(1:nrow(IMDDataFrame_4835))

NARowNum <- 0 
for (i in 1:nrow(IMDDataFrame_4835)) {
  if(is.na(IMD[i])){
    #print(paste("the ",i,"the element is na"))
    classLabel[i] = 1
    NARowNum <- NARowNum+1
  }
  else{
    if(IMD[i]<15){
      classLabel[i] = 2
    }
    if(IMD[i]>=15 && IMD[i]<30){
      classLabel[i] = 3
    }
    if(IMD[i]>=30 && IMD[i]<45){
      classLabel[i] = 4
    }
    if(IMD[i]>=45){
      classLabel[i] = 5
    }
  }
}

LOSA_IMDWithClassLabel <- data.frame(IMDDataFrame_4835$Codes,
                                     classLabel,
                                     IMDDataFrame_4835$IMD,
                                     IMDDataFrame_4835$Names)
colnames(LOSA_IMDWithClassLabel) <- c("Codes","classLabel","IMD","Names")

london <- append_data(london, LOSA_IMDWithClassLabel, key.shp="LSOA11CD", key.data="Codes")

classBreaks <- c(0,1.5,2.5,3.5,4.5,5.5)
classLabels <- c("missing", "<15", "15-30","30-45",">45")
classPalette <- c("#dddddd","#789395","#94b49f" ,"#b4cfb0","#e5e3c9")
#classPalette <- c("#dddddd","#FF0000","#00FF00" ,"#0000FF","#1a374d")

tm_shape(london) +
  tm_fill(col = c("classLabel"),
          palette=classPalette,
          breaks = classBreaks,
          labels = classLabels,
          title="IMD", 
          alpha=1)+
  
  tm_shape(london_borough_33_OGR)+ 
  tm_borders(lty = "solid", lwd = 1, col = "#FFFFFF")+
  
  tm_shape(HSM_geocoord) +
  tm_dots(col = c("#f24a72"),
          auto.palette.mapping = FALSE,
          size = 0.125)
