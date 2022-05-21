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

# missing data:
# median of education increase
# median of bame population 2011
# median of renters in percentage 2011

##----------Gentrification and displacement typologies for inner London in 2011 at neighbourhood level------------##
london <- readOGR(dsn="./statistical-gis-boundaries-london/ESRI",layer="LSOA_2011_London_gen_MHW")
#london <- readOGR(dsn="./published_data",layer="shp")

#load postcodes for all LSOA region
atlas <- read.csv("./published_data/atlas.csv")
CodeNames <- atlas[,c(1,2)]

# (a) low income group in 2011
income <- readxl::read_xlsx("./user_data/fig9_8_gla-household-income-estimates.xlsx")
income$IncomeLowTag <- income$income2011 < (0.95*38340)
income <- join(CodeNames,income,by="Codes")

# (b) vulnerable in 2011

lsoaHousePriceMedian2011 <- readxl::read_xlsx("./user_data/fig9_9_lsoa-data_Housing Price.xlsx")
lsoaHousePriceMedian2011 <- join(CodeNames, lsoaHousePriceMedian2011, by="Codes")
lsoaHousePriceMedian2001 <- readxl::read_xlsx("./user_data/fig9_12_land-registry-house-prices-LSOA.xlsx")
lsoaHousePriceMedian2001_2011 <- join(lsoaHousePriceMedian2011,lsoaHousePriceMedian2001,by="Codes")
lsoaHousePriceMedian2001_2011$increase <- lsoaHousePriceMedian2001_2011$housePirceMedian2011 - lsoaHousePriceMedian2001_2011$housePirceMedian2001
lsoaHousePriceMedian2001_2011$housePriceLowIncreaseSpeedTag <- (lsoaHousePriceMedian2001_2011$increase/lsoaHousePriceMedian2001_2011$housePirceMedian2001)<((292500-155000)/15500)

lsoaHigherEducationPercentage2011 <- readxl::read_xlsx("./user_data/fig9_11_lsoa-data_Edu.xlsx")
lsoaHigherEducationPercentage2011$education2011LowThanLondonMedianTag <- lsoaHigherEducationPercentage2011$higherEduPercentage < 37.7

totalRented2011 <-  readxl::read_xlsx("./user_data/fig9_10_lsoa-data_Renters.xlsx")
totalRented2011$totalRentedHigherThanLondonMedianTag <- totalRented2011$totalRented2011 >49.4

BAME2011 <- readxl::read_xlsx("./user_data/fig9_4_2011-LSOA_Population.xlsx")
BAME2011$BAME2011HigherThanLondonMedianTag <- BAME2011$BAME2011 >40.2

vulnerable <- join(lsoaHousePriceMedian2001_2011,lsoaHigherEducationPercentage2011,by="Codes")
vulnerable <- join(vulnerable,totalRented2011,by="Codes")
vulnerable <- join(vulnerable,BAME2011,by="Codes")


housePriceLowIncreaseSpeedTag_c <- vulnerable$housePriceLowIncreaseSpeedTag
educationLowThanLondonMedianTag_c <- vulnerable$education2011LowThanLondonMedianTag
totalRentedHigherThanLondonMedianTag_c <- vulnerable$totalRentedHigherThanLondonMedianTag
BAME2011HigherThanLondonMedianTag_c <- vulnerable$BAME2011HigherThanLondonMedianTag
vulnerableTag <- seq(1:nrow(vulnerable))


for (i in 1:nrow(vulnerable)) {
  vulnerableTag[i] = NA
  
  if (!is.na(housePriceLowIncreaseSpeedTag_c[i])) {
    if (housePriceLowIncreaseSpeedTag_c[i]) {
      conditionSatisfiedCout <- 0
      
      if (!is.na(educationLowThanLondonMedianTag_c[i])) {
        if (educationLowThanLondonMedianTag_c[i]) {
          conditionSatisfiedCout <- conditionSatisfiedCout + 1
        }
      }
      
      if (!is.na(totalRentedHigherThanLondonMedianTag_c[i])) {
        if (totalRentedHigherThanLondonMedianTag_c[i]) {
          conditionSatisfiedCout <- conditionSatisfiedCout + 1
        }
      }
      
      if (!is.na(BAME2011HigherThanLondonMedianTag_c[i])) {
        if (BAME2011HigherThanLondonMedianTag_c[i]) {
          conditionSatisfiedCout <- conditionSatisfiedCout + 1
        }
      }
      
      if (conditionSatisfiedCout > 1) {
        vulnerableTag[i] = 1
      }
      else{
        vulnerableTag[i] = 0
      }
    }
    else{
      vulnerableTag[i] = 0
    }
  }
}
vulnerable$vulnerableTag <- vulnerableTag>0

# (c) hotspot tag
lsoaHousePriceMedian2001_2011$hotSpotTag <- (lsoaHousePriceMedian2001_2011$increase/lsoaHousePriceMedian2001_2011$housePirceMedian2001) > ((292500-155000)/155000)


# (d) gentrified between 2001-2011

# missing data statistics
with2011Without2001 <- 0;
without2011With2001 <- 0;
With2011With2001 <- 0

for (i in 1:nrow(lsoaHousePriceMedian2001_2011)){
  if(!is.na(lsoaHousePriceMedian2001_2011$housePirceMedian2011[i]) && is.na(lsoaHousePriceMedian2001_2011$housePirceMedian2001[i])){
    with2011Without2001 <- with2011Without2001+1
  }
  if(is.na(lsoaHousePriceMedian2001_2011$housePirceMedian2011[i]) && !is.na(lsoaHousePriceMedian2001_2011$housePirceMedian2001[i])){
    without2011With2001 <- without2011With2001+1
  }
  if(!is.na(lsoaHousePriceMedian2001_2011$housePirceMedian2011[i]) && !is.na(lsoaHousePriceMedian2001_2011$housePirceMedian2001[i])){
    With2011With2001 <- With2011With2001+1
  }
}

lsoaHigherEducationPercentage2001 <- readxl::read_xlsx("./user_data/fig9_13_lsoa-data_Edu_2001.xlsx")
lsoaHigherEducationPercentage2011_2001 <- join(CodeNames,lsoaHigherEducationPercentage2011, by="Codes")
lsoaHigherEducationPercentage2011_2001  <- join(lsoaHigherEducationPercentage2011_2001,lsoaHigherEducationPercentage2001, by="Codes")
lsoaHigherEducationPercentage2011_2001$higherEducationDifferenceHigerThanLondonMedianTag <- ((lsoaHigherEducationPercentage2011_2001$higherEduPercentage - lsoaHigherEducationPercentage2011_2001$higherEducationPercentage2001)/lsoaHigherEducationPercentage2011_2001$higherEducationPercentage2001) > ((37.7-31)/31)

income$householdDifferenceHigherThanLondonMedianTag <- ((income$income2011-income$income2001)/income$income2001) > ((38340-27110)/27110)

gentrified <- join(vulnerable,lsoaHousePriceMedian2001_2011, by="Codes")
gentrified <- join(gentrified,lsoaHigherEducationPercentage2011_2001, by="Codes")
gentrified <- join(gentrified,income, by="Codes")

vulnerableTag_d <- gentrified$vulnerableTag
hotSpotTag_d <-  gentrified$hotSpotTag
higherEducationDifferenceLargerTag_d <- gentrified$higherEducationDifferenceHigerThanLondonMedianTag
householdIncomeDifferenceLargerTag_d <- gentrified$householdDifferenceHigherThanLondonMedianTag
gentrifiedTag <- seq(1:nrow(gentrified))


for (i in 1:nrow(gentrified)) {
  gentrifiedTag[i] = NA
  if(!is.na(vulnerableTag_d[i]) && !is.na(hotSpotTag_d[i]) && !is.na(higherEducationDifferenceLargerTag_d[i]) && !is.na(householdIncomeDifferenceLargerTag_d[i]) )
  {
    if(vulnerableTag_d[i] && hotSpotTag_d[i] && higherEducationDifferenceLargerTag_d[i] && householdIncomeDifferenceLargerTag_d[i]){
      gentrifiedTag[i] <- 1
    }
    else{
      gentrifiedTag[i] <- 0
    }
  }
}
gentrified$gentrifiedTag <- gentrifiedTag>0

#--------------------------- classification according to (a) ,(b) ,(c),(d)--------------------------------#
# data alignment acording to codes
aCriterionDataFrame <- income[,c(1,5)]
bCriterionDataFrame <- vulnerable[,c(1,15)]
cCriterionDataFrame <- lsoaHousePriceMedian2001_2011[,c(1,7)]
dCriterionDataFrame <- gentrified[,c(1,32)]

allCriteriaDataFrame <- join(aCriterionDataFrame,bCriterionDataFrame,by="Codes")
allCriteriaDataFrame <- join(allCriteriaDataFrame,cCriterionDataFrame,by="Codes")
allCriteriaDataFrame <- join(allCriteriaDataFrame,dCriterionDataFrame,by="Codes")

aCriterion<-allCriteriaDataFrame$IncomeLowTag
bCriterion<-allCriteriaDataFrame$vulnerableTag
cCriterion<-allCriteriaDataFrame$hotSpotTag
dCriterion<-allCriteriaDataFrame$gentrifiedTag

finalClass <- seq(1:nrow(allCriteriaDataFrame))

#class upgrade order 
# 0: missing data, none class
# 1: class 5
# 2: class 1
# 3: class 3
# 4: class 2
# 5: class 4

for(i in 1:nrow(allCriteriaDataFrame))
{
  finalClass[i] <- 0  # missing data. none class
  if(!is.na(aCriterion[i])){
    finalClass[i] <- 1 # at least class 5
  }
  
  if(finalClass[i]>=1){
    if( aCriterion[i]){  # meet (a)
      finalClass[i] <- 2 # at least class 1
    }
  }
  
  if(finalClass[i]>=2){
    if(!is.na(bCriterion[i])){
      if(bCriterion[i]){    # meet (a) (b)
        finalClass[i] <- 3 #at least class 3
      }
    }
  }
  
  if(finalClass[i]>=3){
    if(!is.na(cCriterion[i])){
      if(cCriterion[i]){   # meet (a) (b)(c)
        finalClass[i] <- 4 #at least class 2
      }
    }
  }
  
  if(finalClass[i]>=4){ 
    if(!is.na(dCriterion[i])){
      if(dCriterion[i]){    #meet (a) (b) (c) (d)
        finalClass[i] <- 5 #at least class 4
      }
    }
  }
}

allCriteriaDataFrame$finalClass <- finalClass

# plot

london <- append_data(london, allCriteriaDataFrame, key.shp="LSOA11CD", key.data="Codes")

classBreaks <- c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5)
classLabels <- c("missing", "class 5", "class 1","class 3","class 2","class 4")
classPalette <- c("#dddddd","#e0f0ea","#95adbe" ,"#574f7d","#503a65","#3c2a4d" )

tm_shape(london) +
  tm_fill(col = c("finalClass"),
          palette=classPalette,
          breaks = classBreaks,
          labels = classLabels,
          title="gentrification", 
          alpha=1)+
  
  tm_shape(london_borough_33_OGR)+ 
  tm_borders(lty = "solid", lwd = 1, col = "#FFFFFF")+
  
  tm_shape(HSM_geocoord) +
  tm_dots(col = c("#f24a72"),
          auto.palette.mapping = FALSE,
          size = 0.125)




