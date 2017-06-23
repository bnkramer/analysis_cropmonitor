# This file does the simpilest version of adding NDVI data from MODIS 250m product




library(readstata13)
library(sp)
library(MODISTools)
library(datetime)
library(ggplot2)
library(plyr)

# Read in picture data
setwd('H:\\Projects\\India_Index_Insurance\\Data\\')
picture_data = read.dta13('.\\CropMonitorDatabase\\Pictures Data CLEAN 04_21_17.dta')
picture_date_range = seq.Date(as.Date(range(picture_data$timecreated)[1]),as.Date(range(picture_data$timecreated)[2])  , by='day')



# Read in crop cut data
crop_cut = read.dta13('.\\Crop Cut Data/crop_cutting_master.dta')
head(crop_cut)
crop_cut = crop_cut[!is.na(crop_cut$Latitude),]
#coordinates(crop_cut) = ~Longitude+Latitude
#proj4string(crop_cut) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#plot(crop_cut)

modis_subset= data.frame(ID = crop_cut$HHID,lat=crop_cut$Latitude,long=crop_cut$Longitude,start.date = range(picture_data$timecreated)[1], end.date = range(picture_data$timecreated)[2])

# see available bands and dates 
GetBands(Product = "MOD13Q1")
avail_dates = GetDates(Product = "MOD13Q1", Lat = modis_subset$lat[1], Long = modis_subset$long[1]) # all available dates
avail_dates = format(picture_date_range,'A%Y%j')[format(picture_date_range,'A%Y%j') %in% avail_dates] # available in period of interest
  
# download subsets of modis data 
MODISSubsets(LoadDat = modis_subset, Products = "MOD13Q1",
             Bands = c("250m_16_days_NDVI", "250m_16_days_pixel_reliability"),
             Size = c(0,0),SaveDir = './MODIStimeseries')

# Grab modis data for locations
MODISSummaries(LoadDat = modis_subset, Product = "MOD13Q1", Bands = "250m_16_days_NDVI",
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               QualityScreen = TRUE, QualityBand = "250m_16_days_pixel_reliability",
               QualityThreshold = 0,Mean = T, SD = T, Min = T,Dir = './MODIStimeseries',
               DiagnosticPlot=T )


# Read in Modis  Summaries
MODIS_Summary = read.csv('./MODIStimeseries/MODIS_Summary_MOD13Q1_2017-06-23_h16-m16-s32.csv')
MODIS_Summary$HHID = MODIS_Summary$ID
MODIS_Summary_sub = MODIS_Summary[, names(MODIS_Summary) %in% c('HHID','min.band','max.band','mean.band','sd.band')]
names(MODIS_Summary_sub) =c('NDVI.min.band','NDVI.max.band','NDVI.mean.band','NDVI.sd.band','HHID')
crop_cut = join(crop_cut, MODIS_Summary_sub, by='HHID',type='left')
head(crop_cut)


save.dta13(crop_cut,'.\\Crop Cut Data/crop_cutting_master_NDVI.dta')
summary(lm(LEFT~ NDVI.min.band+NDVI.max.band+NDVI.mean.band+NDVI.sd.band, data=crop_cut))
summary(lm(LEFT~ NDVI.min.band+NDVI.max.band+NDVI.mean.band , data=crop_cut))


