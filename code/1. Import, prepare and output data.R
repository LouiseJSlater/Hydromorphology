#==================================================================
#
#  Louise Slater, July 2019
#  Note: this is a simplified version of slightly old code, designed to
#  explore changes in channel morphology using USGS transect data.
#
#==================================================================


#### 1. Import stream measurements    #############################

# To run the code you will need USGS transect measurements (file "1_measurements") 
# and USGS site descriptions (file "1_sitedesc")

# For the transect measurements, download the data from here
# https://waterdata.usgs.gov/nwis/measurements?search_criteria=huc2_cd&submitted_form=introduction
# 1. Select hydrologic regions 
# 2. Go to 'retrieve measurement data for selected sites' - input date range, then select 'tab-separated data YYYY-MM-DD with channel information, save to compressed file' and hit submit. 
# Extract the data into local data folder. This can take a while depending on data volume and internet connection.
# Rename that file "1_measurements"

# The site description file can be downloaded from the same page: select "Site-description information displayed in tab-separated format, saved to file. Highlight all fields, then click submit. 
# Rename that file "1_sitedesc"

# Clean R before running the code
rm(list=ls(all=TRUE));cat('\014') 

dfx <- read.table("./data/1_measurements",
                  header = FALSE,sep = "\t",  dec = ".", 
                  fill=TRUE,colClasses="character",
                  row.names = NULL, quote = "", stringsAsFactors = F)

# Add the names
names(dfx)<-dfx[1,]

# Sample sites
sites = c("01118500","01195200", "01206900","01208500",
          "01209700","01209901","01174600","01174900","01184100","01187300","01199050","01175670",	
          "01181000","01176000","01101000","01109000",
          "01109070","01105870","01163200","01169900","01171500","01170100","01105600")	
dfx = dfx[dfx$site_no %in% sites ,]


# remove variables that aren't directly useful: 
# see http://help.waterdata.usgs.gov/output-formats#streamflow_measurement_data 
# Note that the meas_type and location_distance columns are NOT reliable! 
df <- subset(dfx, select= -c(agency_cd,measurement_nu,tz_cd,q_meas_used_fg,
                             party_nm, site_visit_coll_agency_cd,
                             measured_rating_diff,meas_type,chan_loc_cd, chan_loc_dist, # control_type_cd, 
                             gage_va_change, gage_va_time,discharge_cd,chan_name,streamflow_method,
                             velocity_method,chan_discharge,
                             chan_stability,chan_material,chan_evenness,long_vel_desc,
                             horz_vel_desc,vert_vel_desc ))

head(df)
rm(dfx)
library(plyr)
# siteN <- ddply(df,.(site_no), summarise, n=length(site_no))


#  Remove first two rows that don't contain data
# df = df[-(1:2),] # not the case in a subsampled dataset

df <- plyr::rename(df, c(site_no="Site",
                         measurement_dt="Date",
                         gage_height_va="GH",
                         discharge_va="Q",
                         chan_nu="chan_nu",
                         chan_width="width",
                         chan_area="area",
                         chan_velocity="velocity"))
length(unique(df$Site))
siteN <- ddply(df,.(Site), summarise, n=length(Site))

# Add the metadata (also downloaded for the same period)
latlon <- read.table("./data/1_sitedesc",
                     header = FALSE,sep = "\t",  dec = ".", 
                     fill=TRUE,colClasses="character",
                     row.names = NULL, quote = "", stringsAsFactors = F)
names(latlon) <- latlon[1,]
latlon <- subset(latlon, select= c(site_no,  dec_lat_va, dec_long_va, alt_va, drain_area_va))
latlon = latlon[-(1:2),]  #  Remove first two rows that don't contain data
names(latlon)<-c("Site","lat","lon","elevation", "DA")
head(latlon)

length(unique(latlon$Site))
length(unique(df$Site))
df <- merge(df,latlon)
length(unique(df$Site))
rm(latlon)
head(df)


# the dates are formatted differently (some have hour-min-sec), so we make sure they are all the same 
df$Date <- substring(df$Date,1,10) #keep only characters 1:10
library(lubridate)
df$Date <- ymd(df$Date) #convert the string to date format
df <- df[df$Date>=ymd("1950-01-01"),]
df <- df[df$Date<=ymd("2018-12-31"),]
# write.csv(df,"./data/1_data_raw.csv",row.names=F)

# Check data length again
siteN <- ddply(df,.(Site), summarise, n=length(Site))

#remove measurements made in ice
df$control_type_cd <-as.factor(df$control_type_cd)
unique(df$control_type_cd)
df <- df[!(df$control_type_cd %in% c("ICE","CICE","SICE","AICE", "IceAnchor","IceShore","IceCover")),]
df <- subset(df, select= -c(control_type_cd))

# Check length of measurements AGAIN
siteN <- ddply(df,.(Site), summarise, n=length(Site))

# check class of different variables; Convert Q, W, A, V, GH to numeric 
# sapply(df,class)
cols=c("Q","area","velocity","width", "GH")
df<-df[!is.na(df$GH),]
df[,cols] <-sapply(df[,cols],as.numeric)
options(scipen=999) # disable scientific notation 

# convert to metric units
# Note that in the original USGS spreadsheet, GH is feet, Q is cfs, A is ft2, V is ft/s, W is ft
df$GH <- df$GH*0.3048 
df$Q <- df$Q*0.02832
df$area <- df$area*0.0929
df$velocity <- df$velocity*0.3048
df$width <- df$width*0.3048 

#Remove flow measurement errors within 10% 
df<-df[ ((df$Q/(df$area*df$velocity))<1.1), ]
df<-df[ ((df$Q/(df$area*df$velocity))>0.9), ]


#remove any individual measurements (rows) that have negative or missing values:
df <-df[!is.na(df$Date),]
df <-df[(df$Q>0),] 
df <-df[(df$width>0),] 
df <-df[(df$area>0),] 

siteN <- ddply(df,.(Site), summarise, n=length(Site))

#compute the natural logs
df$lnQ<-log(df$Q) 
df$lnA<-log(df$area)
df$lnV<-log(df$velocity) 
df$lnW<-log(df$width) 

#remove any infinite values due to ln(zero)
is.na(df) <- do.call(cbind,lapply(df, is.infinite)) 
df<- df[complete.cases(df),] #remove any rows that have only NA values
length(unique(df$Site))

# Count the number of transects
siteN <- ddply(df,.(Site), summarise, n=length(Site))
df <- merge(df,siteN,by="Site") #add the column indicating the count
min(df$n);max(df$n)
# nrow(siteN[siteN$n>100,]) 
# df=df[df$n>100,]
rm(siteN) #remove that dataframe


# Keep only the single longest channel measurement location ('chan_nu') at each site
library(plyr)
myfun <- function(xx) { # xx=df[df$Site=="01118300",] #to test function
  table <- count(xx, 'chan_nu')
  value = table[table$freq==max(table$freq), 'chan_nu']
  xx<-xx[xx$chan_nu==value,]
  xx<-subset(xx, select=-chan_nu)
  return (xx)
}
df = do.call(rbind, by(df, INDICES=list(df$Site), FUN=myfun))

siteN <- ddply(df,.(Site), summarise, n=length(Site))

df$BE=df$GH-(df$area/df$width)


names(df)
length(unique(df$Site))
df[,(3:ncol(df))] <-sapply(df[,(3:ncol(df))],as.numeric)
sapply(df,class)
library(lubridate)
df$Date <- ymd(df$Date)

# Check how many measurements we have for each site
# require(plyr)
# siteN <- ddply(df,.(Site), summarise, n=length(Site))

length(unique(df$Site))
write.csv(df,"./data/2_data_clean_raw.csv",row.names=F)



# ========================== FILTERING =========================

rm(list=ls(all=TRUE));cat('\014') 
library(ggplot2);library(lubridate)
df <- read.csv("./data/2_data_clean_raw.csv",colClasses="character",row.names=NULL)
df[,c(3:ncol(df))]<-sapply(df[,c(3:ncol(df))],as.numeric)
df$Date <- ymd(df$Date) 

df <- df[!is.na(df$Q),]
df <- df[!is.na(df$area),]
df <- df[!is.na(df$width),]
df <- df[!is.na(df$velocity),]
df <- df[!is.na(df$BE),]

sites = unique(df$Site)

# Please note these sites are provided just as examples. 
# The filtering steps suggested below can be improved. 


# ggplot(df[df$Site=="01101000",]) + geom_point(aes(GH, lnQ))
df <- df[!(df$Site=="01101000"& df$BE < 0.25 ),]
df <- df[!(df$Site=="01101000"& df$BE > 0.4 ),]
df <- df[!(df$Site=="01101000"& df$width <4.5),]
df <- df[!(df$Site=="01101000"& df$lnV < -3),]
df <- df[!(df$Site=="01101000"& df$width > 5.5),]

# ggplot(df[df$Site=="01105600",]) + geom_point(aes(GH, BE))
df <- df[!(df$Site=="01105600"& df$width > 7.5),]
df <- df[!(df$Site=="01105600"& df$width < 5 ),]
df <- df[!(df$Site=="01105600"& df$BE<0.6),]
df <- df[!(df$Site=="01105600"& df$BE > 0.8),]
df <- df[!(df$Site=="01105600"& df$GH > 1.5),]

# ggplot(df[df$Site=="01105870",]) + geom_point(aes(GH, area))
df <- df[!(df$Site=="01105870"& df$width < 11),]
df <- df[!(df$Site=="01105870"& df$BE<0.68),]
df <- df[!(df$Site=="01105870"& df$BE> 0.8),]


# ggplot(df[df$Site=="01109000",]) + geom_point(aes(GH, BE))
df <- df[!(df$Site=="01109000"& df$width >12),]
df <- df[!(df$Site=="01109000"& df$width < 6),]
df <- df[!(df$Site=="01109000"& df$BE<1.3),]
df <- df[!(df$Site=="01109000"& df$lnW < 2.06),]

# ggplot(df[df$Site=="01109070",]) + geom_point(aes(GH, width))
df <- df[!(df$Site=="01109070"& df$BE < 0.3),]
df <- df[!(df$Site=="01109070"& df$BE > 0.65),]
df <- df[!(df$Site=="01109070"& df$width < 3),]

# ggplot(df[df$Site=="01118500",]) + geom_point(aes(GH, area))
df <- df[!(df$Site=="01118500"& df$BE< 0.7),]
df <- df[!(df$Site=="01118500"& df$BE > 1.2),]
df <- df[!(df$Site=="01118500"& df$width > 34),]
df <- df[!(df$Site=="01118500"& df$width < 25),]


# ggplot(df[df$Site=="01163200",]) + geom_point(aes(GH, lnW))
df <- df[!(df$Site=="01163200"& df$width >10),]
df <- df[!(df$Site=="01163200"& df$width <4),]
df <- df[!(df$Site=="01163200"& df$BE < -0.2),]
df <- df[!(df$Site=="01163200"& df$BE > 0.1),]
df <- df[!(df$Site=="01163200"& df$area > 5),]
df <- df[!(df$Site=="01163200"& df$lnW > 2.22),]

df <- df[! (df$Site=="01169900"),]
# ggplot(df[df$Site=="01169900",]) + geom_point(aes(Date, BE))
# df <- df[!(df$Site=="01169900"& df$BE > 0.75),]


df <- df[! (df$Site=="01170100"),]
# ggplot(df[df$Site=="01170100",]) + geom_point(aes(GH, BE))
# df <- df[!(df$Site=="01170100"& df$BE<0.1),]
# df <- df[!(df$Site=="01170100"& df$BE > 1.2),]

# ggplot(df[df$Site=="01171500",]) + geom_point(aes(GH, area))
df <- df[!(df$Site=="01171500"& df$BE< 1),]
df <- df[!(df$Site=="01171500"& df$BE > 2),]


# ggplot(df[df$Site=="01175670",]) + geom_point(aes(area, width))
df <- df[!(df$Site=="01175670"& df$BE< 2.4),]
df <- df[!(df$Site=="01175670"& df$width > 7),]
df <- df[!(df$Site=="01175670"& df$width < 2),]
df <- df[!(df$Site=="01175670"& df$area > 5),]


# Wouldn't really trust:
# ggplot(df[df$Site=="01176000",]) + geom_point(aes(area, width))
df <- df[!(df$Site=="01176000"& df$BE< 0.3),]
df <- df[!(df$Site=="01176000"& df$width < 20),]
df <- df[!(df$Site=="01176000"& df$velocity > 1),]
df <- df[!(df$Site=="01176000"& df$area > 30),]

# ggplot(df[df$Site=="01181000",]) + geom_point(aes(Date, width))
df <- df[!(df$Site=="01181000"& df$BE< 0.35),]
df <- df[!(df$Site=="01181000"& df$width >30),]


# ggplot(df[df$Site=="01184100",]) + geom_point(aes(Date, width))
df <- df[!(df$Site=="01184100"& df$BE > 0.4),]
df <- df[!(df$Site=="01184100"& df$lnA < -2),]
df <- df[!(df$Site=="01184100"& df$width >40),]
df <- df[!(df$Site=="01184100"& df$width < 5),]


# ggplot(df[df$Site=="01187300",]) + geom_point(aes(GH, area))
df <- df[!(df$Site=="01187300"& df$BE < -0.4),]
df <- df[!(df$Site=="01187300"& df$lnA < 0),]
df <- df[!(df$Site=="01187300"& df$width >15),]


# ggplot(df[df$Site=="01199050",]) + geom_point(aes(lnA, width))
df <- df[!(df$Site=="01199050"& df$BE < -0.1),]
df <- df[!(df$Site=="01199050"& df$lnA < 7),]
# df <- df[!(df$Site=="01199050"& df$width >40),]


df <- df[! (df$Site=="01206900"),]

# ggplot(df[df$Site=="01208500",]) + geom_point(aes(GH, width))
df <- df[!(df$Site=="01208500"& df$width <33),]
df <- df[!(df$Site=="01208500"& df$BE > 0.4),]
df <- df[!(df$Site=="01208500"& df$BE < 0),]
df <- df[!(df$Site=="01208500"& df$GH > 1.5),]


# ggplot(df[df$Site=="01209700",]) + geom_point(aes(GH, BE))
df <- df[!(df$Site=="01209700"& df$GH > 1),]
df <- df[!(df$Site=="01209700"& df$area > 40),]
df <- df[!(df$Site=="01209700"& df$BE < 0.05),]



# ggplot(df[df$Site=="01209901",]) + geom_point(aes(GH, BE))
df <- df[!(df$Site=="01209901"& df$BE > 0.5),]
df <- df[!(df$Site=="01209901"& df$BE < 0.15),]
df <- df[!(df$Site=="01209901"& df$GH > 1),]


# Check data completeness and length
myfun2 <- function(xx) { # xx=df[df$Site=="04250750",] #to test function
  year <- as.numeric(as.character(substring(xx$Date, 1, 4)))
  miny <-min(year)
  maxy <-max(year)
  nyears = (maxy-miny)+1
  uniqueyears = length(unique(year))
  completeness = uniqueyears /nyears
  nrows = nrow(xx)
  xx$GH50=quantile(as.numeric(xx$GH),.50)
  # if (completeness >=0.80 & nyears>=40 & nrows >= 80) #& maxy>=2016
    return (xx)
}

df2= do.call(rbind, by(df, INDICES=list(df$Site), FUN=myfun2))
length(unique(df2$Site))


saveRDS(sites, "./data/sites")


write.csv(df2, "./data/2_data_clean.csv",row.names=F)




#==========================================================================#
#  
#      FIT LOESS CURVES & CALC. ESTIMATED VALUES AT MID STAGE (GH50)       #

rm(list=ls(all=TRUE))
# cat('\014') 

df <- read.csv("./data/2_data_clean.csv",colClasses="character",row.names=NULL)
df[,c(3:ncol(df))]<-sapply(df[,c(3:ncol(df))],as.numeric)
# library(lubridate);
library(stats)
df$Date <- as.Date(df$Date) 
length(unique(df$Site))
# sites <- readRDS("./data/sites")
# df <- df[df$Site %in% sites, ]

# CREATE LOESS FITS
library(dplyr)
df <-  df %>% group_by(Site) %>% 
  dplyr::select(Site, lnQ, everything()) %>%  #this orders the variables
  mutate(LOWQ = predict(loess(lnQ ~ GH,span=0.25))) %>%  # create the loess (multiple values for each site)
  dplyr::select(Site, lnA, everything()) %>%  #this orders the variables
  mutate(LOWA = predict(loess(lnA ~ GH,span=0.25)))  %>% # create the loess (multiple values for each site)
  dplyr::select(Site, lnW, everything()) %>%  #this orders the variables
  mutate(LOWW = predict(loess(lnW ~ GH,span=0.25)))  %>% # create the loess (multiple values for each site)
  dplyr::select(Site, lnV, everything()) %>%  #this orders the variables
  mutate(LOWV = predict(loess(lnV ~ GH,span=0.25)))  %>% # create the loess (multiple values for each site)
  dplyr::select(Site, BE, everything()) %>%  #this orders the variables
  mutate(LOWBE = predict(loess(BE ~ GH,span=0.25)))   # create the loess (multiple values for each site)




# EXTRACT VALUES @ GH50
df <-  df %>% group_by(Site) %>%
  dplyr::select(Site, lnQ, everything()) %>%  #this orders the variables
  summarise(lnQatGH50 = predict (loess(lnQ~GH, span=0.25), newdata=unique(GH50)) ) %>% 
  inner_join(df, by='Site')

df <-  df %>% group_by(Site) %>%
  dplyr::select(Site, lnA, everything()) %>%  #this orders the variables
  summarise(lnAatGH50 = predict (loess(lnA~GH, span=0.25), newdata=unique(GH50)) ) %>% 
  inner_join(df, by='Site')

df <-  df %>% group_by(Site) %>%
  dplyr::select(Site, lnW, everything()) %>%  #this orders the variables
  summarise(lnWatGH50 = predict (loess(lnW~GH, span=0.25), newdata=unique(GH50)) ) %>%
  inner_join(df, by='Site')

df <-  df %>% group_by(Site) %>%
  dplyr::select(Site, lnV, everything()) %>%  #this orders the variables
  summarise(lnVatGH50 = predict (loess(lnV~GH, span=0.25), newdata=unique(GH50)) ) %>%
  inner_join(df, by='Site')

df <-  df %>% group_by(Site) %>%
  dplyr::select(Site, BE, everything()) %>%  #this orders the variables
  summarise(BEatGH50 = predict (loess(BE~GH, span=0.25), newdata=unique(GH50)) ) %>%
  inner_join(df, by='Site')


# CALCULATE ESTIMATED VALUES AT MID STAGE (GH50)

# QRES = the estimated value of channel capacity at median stage (GH50)
# or estimated Q-at-GH50 at different points in time
# (i.e. at each time a channel transect was measured)

df$QRES <-exp((df$lnQ-df$LOWQ)+df$lnQatGH50) #Antilog
df$ARES <-exp((df$lnA-df$LOWA)+df$lnAatGH50) #Antilog
df$WRES <-exp((df$lnW-df$LOWW)+df$lnWatGH50) #Antilog
df$VRES <-exp((df$lnV-df$LOWV)+df$lnVatGH50) #Antilog
df$BRES <-(df$BE-df$LOWBE)+df$BEatGH50


df <- df[order(df$Site, df$Date),] 
library(dplyr)
library(tidyr)# for unnest and drop_na
df <- df %>% drop_na()

sites =unique(df$Site)
df=na.omit(df)


# Output the cleaned channel data (keep all variables for plotting)
length(unique(df$Site))
write.csv(df,"./data/3_alldata_cleaned.csv", row.names=F )


#==============================================================
# OUTPUT A SHORT, LABELLED SUBSET OF DATA =====================

library(tidyverse)
sitedata <- read_csv("./data/3_alldata_cleaned.csv")

names(sitedata)

sitedata <- subset(sitedata, select=c(
  "Site",  "Date", "lat" , "lon", "elevation","DA" ,   
  "QRES", "ARES", "WRES","VRES" ,"BRES"     
))

names(sitedata)[7:11] <- c("Capacity_m3s", "Area_m2", "Width_m", "Velocity_ms", "Bed_elev_m")

write.csv(sitedata,"./data/4_alldata_short_final.csv",row.names=F)


