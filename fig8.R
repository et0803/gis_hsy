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

##---------------------higher education--------------------------##
london <- readOGR(dsn="./statistical-gis-boundaries-london/ESRI",layer="LSOA_2011_London_gen_MHW")
#london <- readOGR(dsn="./published_data",layer="shp")
higherEduDataFrame <- readxl::read_xlsx("./user_data/fig8_lsoa_data_Edu_2001.xlsx")
atlas <- read.csv("./published_data/atlas.csv")
higherEduDataFrame_4835 <- join(atlas,higherEduDataFrame,by="Codes")

higherEdu <- higherEduDataFrame_4835$higherEdu
naturalClassLabel <- seq(1:nrow(higherEduDataFrame_4835))

classificationInfo <- classIntervals(var = higherEdu[!is.na(higherEdu)], n = 4, style = 'jenks', warnLargeN = F)
classBounds <- classificationInfo$brks

NARowNum <- 0 
for (i in 1:nrow(higherEduDataFrame_4835)) {
  if(is.na(higherEdu[i])){
    #print(paste("the ",i,"the element is na"))
    naturalClassLabel[i] = 1
    NARowNum <- NARowNum+1
  }
  else{
    for(j in 2:5){
      if(higherEdu[i]>classBounds[j-1] && higherEdu[i]<classBounds[j]){
        naturalClassLabel[i] = j
        break
      }
    }
  }
}

LOSA_2011_higherEduWithNaturalBreaksLables <- data.frame(higherEduDataFrame_4835$Codes,
                                                         naturalClassLabel,
                                                         higherEduDataFrame_4835$higherEdu,
                                                         higherEduDataFrame_4835$Names)
colnames(LOSA_2011_higherEduWithNaturalBreaksLables) <- c("Codes","naturalClassLabel","higherEdu","Names")

london <- append_data(london, LOSA_2011_higherEduWithNaturalBreaksLables, key.shp="LSOA11CD", key.data="Codes")

classBreaks <- c(0,1.5,2.5,3.5,4.5,5.5)
classLabels <- c("missing", "class 1", "class 2","class 3","class 4")
classPalette <- c("#dddddd","#ffe6bc","#e4cda7" ,"#c3b091","#8e806a")
#classPalette <- c("#dddddd","#FF0000","#00FF00" ,"#0000FF","#1a374d")

tm_shape(london) +
  tm_fill(col = c("naturalClassLabel"),
          palette=classPalette,
          breaks = classBreaks,
          labels = classLabels,
          title="higherEdu", 
          alpha=1)+
  
  tm_shape(london_borough_33_OGR)+ 
  tm_borders(lty = "solid", lwd = 1, col = "#FFFFFF")+
  
  tm_shape(HSM_geocoord) +
  tm_dots(col = c("#f24a72"),
          auto.palette.mapping = FALSE,
          size = 0.125)
