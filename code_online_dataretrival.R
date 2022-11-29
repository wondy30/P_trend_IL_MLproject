library(dataRetrieval)
library(tidyverse)
library(lubridate)

#find all available sites in IL with Phosphorous data
phosSites <- whatWQPsites(statecode = "IL",
                          characteristicName = "Phosphorus")

#scope of the available data
phosData <- whatWQPdata(statecode = "IL",
                        characteristicName = "Phosphorus")

windows()
hist(phosData$resultCount, breaks = 20)

#unfiltered, all stations
length(phosData$MonitoringLocationTypeName)
allPhosData <- phosData %>% 
  filter(MonitoringLocationTypeName == c("Stream", "River/Stream"))

#read the data
length(unique(allPhosData$MonitoringLocationIdentifier))

# read in parts to make for the readWQPdata easy access

allPhosData$part <- 1:1370 %% 3 + 1

allPhosData %>% 
  group_by(part) %>% 
  summarise(count=n())

allPhosDataList <- split(allPhosData, allPhosData$part) 


readPart1 <- readWQPdata(siteNumbers = allPhosDataList[[1]]$MonitoringLocationIdentifier, characteristicName = "Phosphorus")

readPart2 <- readWQPdata(siteNumbers = allPhosDataList[[2]]$MonitoringLocationIdentifier, characteristicName = "Phosphorus")

readPart3 <- readWQPdata(siteNumbers = allPhosDataList[[3]]$MonitoringLocationIdentifier, characteristicName = "Phosphorus")

readAllPhosData <- rbind(readPart1, readPart2, readPart3)

write_csv(readAllPhosData, "allPhosData.csv")

unique(readAllPhosData$ResultMeasure.MeasureUnitCode)
summary(readAllPhosData)

#maximum measured value, and number of samples:
siteInfo <- attr(readAllPhosData, "siteInfo")

ilSummaryPhos <- readAllPhosData %>%
  filter(ResultMeasure.MeasureUnitCode %in%
           c("mg/l","mg/l as P")) %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(count=n(), max = max(ResultMeasureValue, na.rm = TRUE)) %>%
  left_join(siteInfo, by = "MonitoringLocationIdentifier")

unique(readAllPhosData$ResultSampleFractionText) #different sample fraction measures

disPhosAsMgL <- readAllPhosData %>%
  filter(ResultMeasure.MeasureUnitCode %in%
           c("mg/l","mg/l as P"), ResultSampleFractionText == "Dissolved") #extract dissolved P in mg/L


#Mean annual Phos
disPhosAsMgL <- disPhosAsMgL %>% 
  select(1:35)
summary(disPhosAsMgL)

#drop rows with no measurements, NA
disPhosAsMgL <- drop_na(disPhosAsMgL, ResultMeasureValue)

#Aggregate conc into year, using mean

disPhosAsMgL <- disPhosAsMgL %>% 
  mutate(Year = year(ActivityStartDate))

write_csv(disPhosAsMgL, "disPhosAsMgL.csv")

#Check sample size for each year and since 2000

sampleSize <- disPhosAsMgL %>% 
  group_by(Year) %>% 
  summarise(count=n())
  
ggplot(sampleSize, aes(x=Year, y=count))  + 
  geom_point()

# Slice the data starting 1981, #match availability of precip data

disPhosAsMgLsince1981 <- disPhosAsMgL %>% 
  filter(Year >= 1981)

#to discover attribute information with the data
names(attributes(disPhosAsMgLsince1981))

siteInfo <- attr(disPhosAsMgLsince1981, "siteInfo")

#compare between siteInfo and dataset, less station in the siteInfo than the dataset

uniqueSite <- distinct(disPhosAsMgLsince1981, MonitoringLocationIdentifier)

compare <- uniqueSite %>% 
  anti_join(siteInfo, by="MonitoringLocationIdentifier")

siteInfo <- attr(disPhosAsMgLsince1981, "siteInfo")

#Only few columns are impt., select

disPhosAsMgLsince1981 <- disPhosAsMgLsince1981[,c(22,7,34)]

summary(disPhosAsMgLsince1981)
length(unique(disPhosAsMgLsince1981$MonitoringLocationIdentifier))



#Mean Annual Dis Phos as MgL since 2000
DisPhosMgL_2000MeanAnnual <- DisPhosMgL_2000Clean %>% 
  group_by(MonitoringLocationIdentifier, Year) %>% 
  summarise(MeanAnnualDisPasMgL = mean(ResultMeasureValue))

write_csv(DisPhosMgL_2000MeanAnnual, "MeanAnnualDisPhosAsMgL_2000.csv")

#Get site information
uniqueSite <- distinct(DisPhosMgL_2000MeanAnnual, MonitoringLocationIdentifier)

DisPhosSiteInfo <- uniqueSite %>% 
  left_join(siteInfo, by = "MonitoringLocationIdentifier")

write_csv(DisPhosSiteInfo, "DisPhosSiteInfo.csv")

#Get the unique year
Years <- sort(unique(DisPhosMgL_2000MeanAnnual$Year))    #Complete in terms of years from 2000 - 2022

# Get location information for the sites
colnames(DisPhosSiteInfo)

attribute <- select(DisPhosSiteInfo, MonitoringLocationIdentifier,
                    station_nm,
                    hucCd,
                    HUCEightDigitCode,
                    DrainageAreaMeasure.MeasureValue,
                    DrainageAreaMeasure.MeasureUnitCode,
                    LatitudeMeasure,
                    LongitudeMeasure)
