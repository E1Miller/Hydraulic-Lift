#Created by: Elise Miller
#Date started: 10/05/2022
#Date last edited: 01/27/23
#Description: QA/QC WIL 1

#Attach dependencies 
library(tidyverse) #I got rid of a lot of libraries, if something doesn't work, add 1 by 1 
library(dplyr)
library(plyr)
library(readr)
library(ggpubr)
library(tidyr)
library(purrr)
library(lubridate, warn.conflicts = FALSE)
library(googledrive)
library(berryFunctions)

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL", 
                        pattern=glob2rx("W1M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console


#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
WIL1_2019_2021 <- data19_21 %>%
  rename_all(funs(c("Date", "Time", "PAR", "WC_15cm", "WC_30cm","WC_100cm", "Date_time"))) %>% #this renames all of the columns
  select(Date_time, PAR, WC_15cm, WC_30cm, WC_100cm) #this keeps only the columns you will use


#2017-2019 FILES
#========================================================================================================================
#Set the data path 
data_path <- "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL"
new_col_name <- c("Date_time", "PAR", "WC_15cm", "WC_30cm", "WC_100cm")

#Call in all the files in this folder with the W1M1 pattern, which excludes the datasheets from 2019-2020
files <- dir(data_path, pattern=glob2rx("W1M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
WIL1_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
WIL1 <- rbind(WIL1_2017_2019, WIL1_2019_2021)

#Write the csv
write.csv(WIL1,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL1.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(WIL1)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
WIL1$Date <- mdy_hms(WIL1$Date_time)

#Put year into a separate column 
WIL1 <- separate(WIL1, Date, c("Year"))

#Subset based on year 


#WIL1 2017
##################################################################################################
WIL1_17 <- subset(WIL1, Year == '2017')

WIL1_17$WC_15cm <- as.numeric(WIL1_17$WC_15cm)
WIL1_17$WC_30cm <- as.numeric(WIL1_17$WC_30cm)
WIL1_17$WC_100cm <- as.numeric(WIL1_17$WC_100cm)
WIL1_17$Date_time<- mdy_hms(WIL1_17$Date_time)

Soil <- ggplot(data = subset(WIL1_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL1 2018
##################################################################################################

#REPLACING NEGATIVE VALUES GREATER THAN A DAY WITH NA
#=================================================================================================
WIL1_18 <- subset(WIL1, Year == '2018')

#Find contiguous blocks of positive and negative values 
WIL1_18$block = cumsum(c(1,abs(diff(WIL1_18$WC_15cm < 0))))
WIL1_18$block_30 = cumsum(c(1,abs(diff(WIL1_18$WC_30cm < 0))))
WIL1_18$block_100 = cumsum(c(1,abs(diff(WIL1_18$WC_100cm < 0))))

#Count how many negative values are in each block 
WIL1_18$block_neg = ave(WIL1_18$WC_15cm, WIL1_18$block, FUN = function(x) {sum(x<0)})
WIL1_18$block_30neg = ave(WIL1_18$WC_30cm, WIL1_18$block_30, FUN = function(x) {sum(x<0)})
WIL1_18$block_100neg = ave(WIL1_18$WC_30cm, WIL1_18$block_100, FUN = function(x) {sum(x<0)})

#Assign NA to blocks with greater than 144 negative values 
WIL1_18$WC_15cm_neg = ifelse(WIL1_18$block_neg>144, NA, WIL1_18$WC_15cm)
WIL1_18$WC_30cm_neg = ifelse(WIL1_18$block_30neg>144, NA, WIL1_18$WC_30cm)
WIL1_18$WC_100cm_neg = ifelse(WIL1_18$block_100neg>144, NA, WIL1_18$WC_100cm)

#Create columns that will be indicators of missing values 
WIL1_18 <- WIL1_18 %>%
  add_column(WC_15cm_M = NA) %>%
  add_column(WC_30cm_M = NA) %>%
  add_column(WC_100cm_M = NA)

#Replace all NA values with M 
WIL1_18$WC_15cm_M[is.na(WIL1_18$WC_15cm_neg)] <- "M"
WIL1_18$WC_30cm_M[is.na(WIL1_18$WC_30cm_neg)] <- "M"
WIL1_18$WC_100cm_M[is.na(WIL1_18$WC_100cm_neg)] <- "M"

#INTERPOLATING 
###########################################################################################

#Interpolate if less than one day of values is missing (i.e. negative)
#==============================================================================

#15 cm
#=================================================================================
#Since 15 cm has no negative values after the ones greater than a day are removed, 
#there is no need to interpolate if less than one day of values is missing 
WIL1_18$WC_15cm_neg[WIL1_18$WC_15cm_neg < 0.2] <- NA

#30cm
WIL1_18$WC_30cm_neg[WIL1_18$WC_30cm_neg < 0] <- NA
missing <- which(is.na(WIL1_18$WC_30cm_M))

if(1 %in% missing){
  WIL1_18$WC_30cm_neg[1] <- head(WIL1_18$WC_30cm_neg[!is.na(WIL1_18$WC_30cm_neg)],1)
}
if(nrow(WIL1_18) %in% missing){
  WIL1_18$WC_30cm_neg[nrow(data)] <- tail(WIL1_18$WC_30cm_neg[!is.na(WIL1_18$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18$WC_30cm_neg))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18$WC_30cm_neg[idx] <- (WIL1_18$WC_30cm_neg[r$starts[i]] + WIL1_18$WC_30cm_neg[r$ends[i]])/2
}

#100 cm
#=================================================================================
#Since 100 cm has no negative values after the ones greater than a day are removed, 
#there is no need to interpolate if less than one day of values is missing 


#Interpolating if value is 20% higher or lower than previous value
#===========================================================================

#Create separate data frames for each soil depth 
WIL1_18_15cm <- select(WIL1_18, Date_time, WC_15cm_neg)
WIL1_18_30cm <- select(WIL1_18, Date_time, WC_30cm_neg)
WIL1_18_100cm <- select(WIL1_18, Date_time, WC_100cm_neg)

#Make the Date_time column POSIX
WIL1_18_15cm$Date_time<- mdy_hms(WIL1_18_15cm$Date_time)
WIL1_18_30cm$Date_time<- mdy_hms(WIL1_18_30cm$Date_time)
WIL1_18_100cm$Date_time<- mdy_hms(WIL1_18_100cm$Date_time)

#15 cm 
#===========================================================================

#Subset problem section for 15 cm 
WIL1_18_fix15 <- WIL1_18_15cm[c(15000:28000), ]  

#Create a new column that is the percent difference between the row below and the row above 
WIL1_18_fix15 <- WIL1_18_fix15 %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm_neg-lag(WC_15cm_neg),
    increase=scales::percent(diff / lag(WC_15cm_neg))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================


#Make increase column not a percent 
WIL1_18_fix15 <- transform(WIL1_18_fix15, incr=as.numeric(gsub('\\%', '', increase))/100)

WIL1_18_fix15 <- transform(WIL1_18_fix15, WC_15cm_neg=ifelse(incr > 0.002, 
                                                             as.numeric(stats::filter(WC_15cm_neg, rep(1/144, 144), sides=2)), 
                                                             WC_15cm_neg))

#Remove the decrease in values and delete up until 2018-11-21 05:40:01
WIL1_18_15cm <- select(WIL1_18_15cm, Date_time, WC_15cm_neg)
WIL1_18_15cm_later <- WIL1_18_15cm[c(28001:52555), ]

WIL1_18_15cm_replace <- WIL1_18_15cm_later[c(15721:18685), ]
WIL1_18_15cm_replace['WC_15cm_neg'] <- NA

WIL1_18_15cm_later <- WIL1_18_15cm_later[c(1:15720, 18686:24555), ]

#Merge back with WIL1_18_15cm_later
WIL1_18_15cm_later <- bind_rows(WIL1_18_15cm_replace, WIL1_18_15cm_later)


#Create dataframe with interpolated values 
WIL1_18_fix15 <- select(WIL1_18_fix15, Date_time, WC_15cm_neg)


WIL1_18_15cm_early <- WIL1_18_15cm[c(1:15000), ]


WIL1_18_15cm <- bind_rows(WIL1_18_15cm_early, WIL1_18_fix15, WIL1_18_15cm_later)

WIL1_18_clean <- WIL1_18_15cm

#Subset and remove drips in July
#===========================================================================
WIL1_18_fix <- filter(WIL1_18_clean, Date_time > "2018-07-05 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-07-11 01:00:01")

Soil <- ggplot(data = subset(WIL1_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm_neg, color = "lightblue"))
Soil 

WIL1_18_fix$WC_15cm_neg[WIL1_18_fix$WC_15cm_neg > 0.269] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm_neg))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm_neg[1] <- head(WIL1_18_fix$WC_15cm_neg[!is.na(WIL1_18_fix$WC_15cm_neg)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm_neg[nrow(data)] <- tail(WIL1_18_fix$WC_15cm_neg[!is.na(WIL1_18_fix$WC_15cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm_neg[idx] <- (WIL1_18_fix$WC_15cm_neg[r$starts[i]] + WIL1_18_fix$WC_15cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_18_early <- filter(WIL1_18_clean, Date_time < "2018-07-05 1:00:01")
WIL1_18_late <- filter(WIL1_18_clean, Date_time > "2018-07-11 01:00:01")

WIL1_18_clean <- bind_rows(WIL1_18_early, WIL1_18_fix, WIL1_18_late)

#30 cm 
#############################################################################
#Remove everything below 0.195 in the WC_30cm column because it should always be above that value 
WIL1_18_30cm$WC_30cm_neg[WIL1_18_30cm$WC_30cm_neg < 0.199] <- NA
missing <- which(is.na(WIL1_18_30cm$WC_30cm_neg)) #Changed from WC_30cm_M to neg

if(1 %in% missing){
  WIL1_18_30cm$WC_30cm_neg[1] <- head(WIL1_18_30cm$WC_30cm_neg[!is.na(WIL1_18_30cm$WC_30cm_neg)],1)
}
if(nrow(WIL1_18_30cm) %in% missing){
  WIL1_18_30cm$WC_30cm_neg[nrow(data)] <- tail(WIL1_18_30cm$WC_30cm_neg[!is.na(WIL1_18_30cm$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_30cm$WC_30cm_neg))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_30cm$WC_30cm_neg[idx] <- (WIL1_18_30cm$WC_30cm_neg[r$starts[i]] + WIL1_18_30cm$WC_30cm_neg[r$ends[i]])/2
}


#For the December period, get rid of anything below 0.28 in the WC_30cm column 
#================================================================================================
WIL1_18Dec <- WIL1_18_30cm[c(47750:52555), ]  


WIL1_18Dec$WC_30cm_neg[WIL1_18Dec$WC_30cm_neg < 0.30] <- NA
missing <- which(is.na(WIL1_18Dec$WC_30cm_neg))

if(1 %in% missing){
  WIL1_18Dec$WC_30cm_neg[1] <- head(WIL1_18Dec$WC_30cm_neg[!is.na(WIL1_18Dec$WC_30cm_neg)],1)
}
if(nrow(WIL1_18Dec) %in% missing){
  WIL1_18Dec$WC_30cm_neg[nrow(data)] <- tail(WIL1_18Dec$WC_30cm_neg[!is.na(WIL1_18Dec$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18Dec$WC_30cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18Dec$WC_30cm_neg[idx] <- (WIL1_18Dec$WC_30cm_neg[r$starts[i]] + WIL1_18Dec$WC_30cm_neg[r$ends[i]])/2
}


#Create a new column that is the percent difference between the row below and the row above 
WIL1_18_fix30 <- WIL1_18Dec %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm_neg-lag(WC_30cm_neg),
    increase=scales::percent(diff / lag(WC_30cm_neg))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================


#Make increase column not a percent 
WIL1_18_fix30 <- transform(WIL1_18_fix30, incr=as.numeric(gsub('\\%', '', increase))/100)


#interp_WIL1_18 <- transform(interp_WIL1_18, WC_30cm_interpolate =ifelse(incr > .01, 
#  as.numeric(stats::filter(WC_30cm_neg, rep(1/3, 3), sides=2)), 
# WC_30cm_neg))

#WIL1_18_fix30 <- transform(WIL1_18_fix30, WC_30cm_neg=ifelse(incr > 0.01, 
#   as.numeric(stats::filter(WC_30cm_neg, rep(1/6, 6), sides=2)), 
#   WC_30cm_neg))

WIL1_18_fix30 <- transform(WIL1_18_fix30, WC_30cm_neg=ifelse(incr < 0.02, 
                                                             as.numeric(stats::filter(WC_30cm_neg, rep(1/6, 6), sides=2)), 
                                                             WC_30cm_neg))

WIL1_18_fix30 <- select(WIL1_18_fix30, Date_time, WC_30cm_neg)
WIL1_18_fix30 <- na.omit(WIL1_18_fix30)

#Create new dataframe 

#Subset to get missing dates 
WIL1_18_early <- WIL1_18_30cm[c(1:47752), ]
WIL1_18_late <- WIL1_18_30cm[c(52553:52555), ]

#Choose the two columns of interest
WIL1_18_early <- select(WIL1_18_early, Date_time, WC_30cm_neg)
WIL1_18_late <- select(WIL1_18_late, Date_time, WC_30cm_neg)

WIL1_18_30cm <- bind_rows(WIL1_18_early, WIL1_18_fix30, WIL1_18_late)

#Fix dip in early March in 30 cm 
#=======================================================
WIL1_18Mar <- WIL1_18_30cm[c(6000:9000), ]  

WIL1_18Mar$WC_30cm_neg[WIL1_18Mar$WC_30cm_neg < 0.30] <- NA
missing <- which(is.na(WIL1_18Mar$WC_30cm_neg))

if(1 %in% missing){
  WIL1_18Mar$WC_30cm_neg[1] <- head(WIL1_18Mar$WC_30cm_neg[!is.na(WIL1_18Mar$WC_30cm_neg)],1)
}
if(nrow(WIL1_18Mar) %in% missing){
  WIL1_18Mar$WC_30cm_neg[nrow(data)] <- tail(WIL1_18Mar$WC_30cm_neg[!is.na(WIL1_18Mar$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18Mar$WC_30cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18Mar$WC_30cm_neg[idx] <- (WIL1_18Mar$WC_30cm_neg[r$starts[i]] + WIL1_18Mar$WC_30cm_neg[r$ends[i]])/2
}

#Subset to get missing dates 
WIL1_18_early <- WIL1_18_30cm[c(1:8301), ]
WIL1_18_late <- WIL1_18_30cm[c(9001:52555), ]

#Choose the two columns of interest
WIL1_18_early <- select(WIL1_18_early, Date_time, WC_30cm_neg)
WIL1_18_late <- select(WIL1_18_late, Date_time, WC_30cm_neg)

WIL1_18_30cm <- bind_rows(WIL1_18_early, WIL1_18Mar, WIL1_18_late)

#Subset to fix March drips 
#============================================================================
WIL1_18_30cm_fix <- filter(WIL1_18_30cm, Date_time > "2018-02-10 1:00:01")
WIL1_18_30cm_fix <- filter(WIL1_18_30cm_fix, Date_time < "2018-03-20 1:00:01")

WIL1_18_30cm_fix$WC_30cm_neg[WIL1_18_30cm_fix$WC_30cm_neg < 0.2999] <- NA
missing <- which(is.na(WIL1_18_30cm_fix$WC_30cm_neg))

if(1 %in% missing){
  WIL1_18_30cm_fix$WC_30cm_neg[1] <- head(WIL1_18_30cm_fix$WC_30cm_neg[!is.na(WIL1_18_30cm_fix$WC_30cm_neg)],1)
}
if(nrow(WIL1_18_30cm_fix) %in% missing){
  WIL1_18_30cm_fix$WC_30cm_neg[nrow(data)] <- tail(WIL1_18_30cm_fix$WC_30cm_neg[!is.na(WIL1_18_30cm_fix$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_30cm_fix$WC_30cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_30cm_fix$WC_30cm_neg[idx] <- (WIL1_18_30cm_fix$WC_30cm_neg[r$starts[i]] + WIL1_18_30cm_fix$WC_30cm_neg[r$ends[i]])/2
}

#Subset back in 
WIL1_18_30cm_early <- filter(WIL1_18_30cm, Date_time < "2018-02-10 1:00:01")
WIL1_18_30cm_late <- filter(WIL1_18_30cm, Date_time > "2018-03-20 1:00:01")

WIL1_18_30cm <- bind_rows(WIL1_18_30cm_early, WIL1_18_30cm_fix, WIL1_18_30cm_late)


#Fix 30 cm glitch
#==============================================================================
WIL1_18_clean$WC_30cm_neg[WIL1_18_clean$WC_30cm_neg == 0.3048] <- NA

#100 cm 
################################################################################################################

#Create new dataframe 

WIL1_18_clean <- merge(WIL1_18_15cm, WIL1_18_30cm, WIL1_18_100cm, by.x = "Date_time", by.y = "Date_time")
WIL1_18_clean <- merge(WIL1_18_clean, WIL1_18_100cm, by.x = "Date_time", by.y = "Date_time")

#Graphing 
#=======================================================================================

WIL1_18_clean$WC_15cm_neg <- as.numeric(WIL1_18_clean$WC_15cm_neg)
WIL1_18_clean$WC_30cm_neg <- as.numeric(WIL1_18_clean$WC_30cm_neg)
WIL1_18_clean$WC_100cm <- as.numeric(WIL1_18_clean$WC_100cm_neg)

Soil <- ggplot(data = subset(WIL1_18_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm_neg, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm_neg, color = "blue")) + 
  geom_line(aes(y = WC_15cm_neg, color = "lightblue"))
Soil 

#WIL1 2019
################################################################################
WIL1_19 <- subset(WIL1, Year == '2019')

#REPLACING NEGATIVE VALUES GREATER THAN A DAY WITH NA
#=================================================================================================
WIL1_19$WC_15cm <- as.numeric(WIL1_19$WC_15cm)
WIL1_19$WC_30cm <- as.numeric(WIL1_19$WC_30cm)
WIL1_19$WC_100cm <- as.numeric(WIL1_19$WC_100cm)

WIL1_19$Date_time<- mdy_hms(WIL1_19$Date_time)

#Find contiguous blocks of positive and negative values 
WIL1_19$block_15 = cumsum(c(1,abs(diff(WIL1_19$WC_15cm < 0))))
WIL1_19$block_30 = cumsum(c(1,abs(diff(WIL1_19$WC_30cm < 0))))
WIL1_19$block_100 = cumsum(c(1,abs(diff(WIL1_19$WC_100cm < 0))))

#Count how many negative values are in each block 

WIL1_19$block_15neg = ave(WIL1_19$WC_15cm, WIL1_19$block_15, FUN = function(x) {sum(x<0)})
WIL1_19$block_30neg = ave(WIL1_19$WC_30cm, WIL1_19$block_30, FUN = function(x) {sum(x<0)})
WIL1_19$block_100neg = ave(WIL1_19$WC_100cm, WIL1_19$block_100, FUN = function(x) {sum(x<0)})

#Assign NA to blocks with greater than 144 negative values 
WIL1_19$WC_15cm_neg = ifelse(WIL1_19$block_15neg>144, NA, WIL1_19$WC_15cm)
WIL1_19$WC_30cm_neg = ifelse(WIL1_19$block_30neg>144, NA, WIL1_19$WC_30cm)
WIL1_19$WC_100cm_neg = ifelse(WIL1_19$block_100neg>144, NA, WIL1_19$WC_100cm)

#Create columns that will be indicators of missing values 
WIL1_19 <- WIL1_19 %>%
  add_column(WC_15cm_M = NA) %>%
  add_column(WC_30cm_M = NA) %>%
  add_column(WC_100cm_M = NA)

#Replace all NA values with M 
WIL1_19$WC_15cm_M[is.na(WIL1_19$WC_15cm_neg)] <- "M"
WIL1_19$WC_30cm_M[is.na(WIL1_19$WC_30cm_neg)] <- "M"
WIL1_19$WC_100cm_M[is.na(WIL1_19$WC_100cm_neg)] <- "M"

#INTERPOLATE LESS THAN ONE DAY MISSING VALUES 
###########################################################################################
#Only applicable for WC_30 cm 
#Remove anything below 0 for less than a day and take the average 
WIL1_19$WC_30cm_neg[WIL1_19$WC_30cm_neg < 0] <- NA
missing <- which(is.na(WIL1_19$WC_30cm_neg)) #Changed from WC_30cm_M to neg

if(1 %in% missing){
  WIL1_19$WC_30cm_neg[1] <- head(WIL1_19$WC_30cm_neg[!is.na(WIL1_19$WC_30cm_neg)],1)
}
if(nrow(WIL1_19) %in% missing){
  WIL1_19$WC_30cm_neg[nrow(data)] <- tail(WIL1_19$WC_30cm_neg[!is.na(WIL1_19$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19$WC_30cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19$WC_30cm_neg[idx] <- (WIL1_19$WC_30cm_neg[r$starts[i]] + WIL1_19$WC_30cm_neg[r$ends[i]])/2
}

#Create separate data frames for each soil depth 
WIL1_19_15cm <- select(WIL1_19, Date_time, WC_15cm_neg)
WIL1_19_30cm <- select(WIL1_19, Date_time, WC_30cm_neg)
WIL1_19_100cm <- select(WIL1_19, Date_time, WC_100cm_neg)

#INTERPOLATING DROPS/INCREASES IN VALUES
#===================================================

#30 cm 
#########################################################################

#Remove the 0.34015 pattern that stays for several days in a row
WIL1_19_30cm$WC_30cm_neg[WIL1_19_30cm$WC_30cm_neg == 0.34015] <- NA


#Remove anything that suddenly drops in 30 cm 
#Subset so that it's only fixing the first part of the year to avoid getting the weird error
WIL1_19_fix30 <- WIL1_19_30cm[c(1:22000), ]

#Anything below 0.28 make into the average of the best value before and after 

#Remove anything below 0 for less than a day and take the average 
WIL1_19_fix30$WC_30cm_neg[WIL1_19_fix30$WC_30cm_neg < 0.290] <- NA
missing <- which(is.na(WIL1_19_fix30$WC_30cm_neg)) #Changed from WC_30cm_M to neg

if(1 %in% missing){
  WIL1_19_fix30$WC_30cm_neg[1] <- head(WIL1_19_fix30$WC_30cm_neg[!is.na(WIL1_19_fix30$WC_30cm_neg)],1)
}
if(nrow(WIL1_19_fix30) %in% missing){
  WIL1_19_fix30$WC_30cm_neg[nrow(data)] <- tail(WIL1_19_fix30$WC_30cm_neg[!is.na(WIL1_19_fix30$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_fix30$WC_30cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_fix30$WC_30cm_neg[idx] <- (WIL1_19_fix30$WC_30cm_neg[r$starts[i]] + WIL1_19_fix30$WC_30cm_neg[r$ends[i]])/2
}

#Fix later half of the 30 cm dataframe 
WIL1_19_30cm_late <- WIL1_19_30cm[c(22001:41920), ]


#Replace anything in the later part of 30 cm above 0.325
WIL1_19_30cm_late$WC_30cm_neg[WIL1_19_30cm_late$WC_30cm_neg > 0.323] <- NA
missing <- which(is.na(WIL1_19_30cm_late$WC_30cm_neg)) #Changed from WC_30cm_M to neg

if(1 %in% missing){
  WIL1_19_30cm_late$WC_30cm_neg[1] <- head(WIL1_19_30cm_late$WC_30cm_neg[!is.na(WIL1_19_30cm_late$WC_30cm_neg)],1)
}
if(nrow(WIL1_19_30cm_late) %in% missing){
  WIL1_19_30cm_late$WC_30cm_neg[nrow(data)] <- tail(WIL1_19_30cm_late$WC_30cm_neg[!is.na(WIL1_19_30cm_late$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_30cm_late$WC_30cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_30cm_late$WC_30cm_neg[idx] <- (WIL1_19_30cm_late$WC_30cm_neg[r$starts[i]] + WIL1_19_30cm_late$WC_30cm_neg[r$ends[i]])/2
}
    
#Recombine with the other part of the dataframe to get a full clean 30 cm dataframe 
#=============================================================
WIL1_19_fix30 <- select(WIL1_19_fix30, Date_time, WC_30cm_neg)

WIL1_19_30cm_late <- select(WIL1_19_30cm_late, Date_time, WC_30cm_neg)

WIL1_19_30cm_clean <- bind_rows(WIL1_19_fix30, WIL1_19_30cm_late)

#100 cm 
##########################################################################

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-03-04 1:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-03-05 01:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.303] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-03-04 1:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-03-05 01:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-03-04 1:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-03-06 01:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.306] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-03-04 1:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-03-06 01:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-03-06 1:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-03-08 01:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.308] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-03-06 1:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-03-08 01:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-03-08 1:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-03-09 01:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.302] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-03-08 1:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-03-09 01:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-03-09 1:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-03-13 01:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.3045] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-03-09 1:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-03-13 01:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-03-23 20:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-03-25 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.3003] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-03-23 20:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-03-25 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-03-27 20:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-03-29 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.300] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-03-27 20:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-03-29 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-04-02 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-04-05 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.2954] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-04-02 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-04-05 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-04-16 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-04-20 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.294] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-04-16 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-04-20 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)


#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-04-23 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-04-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.288] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-04-23 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-04-30 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-04-25 00:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-04-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.285] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-04-25 00:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-04-30 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-04-27 00:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-04-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.283] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-04-27 00:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-04-30 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-05-02 00:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-05-06 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.2785] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-05-02 00:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-05-06 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-05-06 00:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-05-14 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.275] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-05-06 00:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-05-14 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-05-11 00:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-05-14 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.2715] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-05-11 00:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-05-14 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-05-13 00:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-05-15 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.27] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-05-13 00:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-05-15 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-05-24 00:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-05-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.298] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-05-24 00:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-05-30 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-06-06 00:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-06-10 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.2855] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-06-06 00:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-06-10 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-06-16 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-06-20 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.271] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-06-16 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-06-20 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-06-24 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-06-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.253] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-06-24 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-06-30 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-07-05 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-07-08 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.236] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-07-05 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-07-08 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-07-13 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-07-18 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.226] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-07-13 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-07-18 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-07-19 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-07-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.2148] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-07-19 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-07-30 05:00:01")
WIL1_19_100cm_clean <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-08-05 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-08-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.1951] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-08-05 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-08-30 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-08-22 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-08-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.1825] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-08-22 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-08-30 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-08-25 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-08-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.18] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-08-25 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-08-30 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-08-30 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-30 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.177] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-08-30 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-30 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-08-30 10:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-04 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.177] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-08-30 10:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-04 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-01 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-04 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.176] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-01 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-04 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-04 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-14 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.176] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-04 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-14 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-06 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-14 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.175] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-06 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-14 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-09 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-14 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.174] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-09 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-14 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-11 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-14 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.174] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-11 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-14 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-13 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-18 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.1729 | WIL1_19_100cm_clean$WC_100cm_neg < 0.1685] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-13 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-18 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-18 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-28 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.174] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-18 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-28 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-21 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-28 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.1715] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-21 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-28 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-23 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-09-28 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.1705] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-23 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-09-28 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-09-28 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-10-07 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.171] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-09-28 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-10-07 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-10-07 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-10-16 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.169] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-10-07 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-10-16 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-10-16 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-10-26 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.17] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-10-16 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-10-26 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-10-18 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-10-26 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.168] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-10-18 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-10-26 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-10-23 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-10-31 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.167] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-10-23 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-10-31 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-10-25 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-10-31 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.166] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-10-25 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-10-31 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-10-31 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-11-25 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.1652] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-10-31 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-11-25 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)

#Fix 100 cm drips 
#=======================================================================================================
WIL1_19_100cm_clean <- filter(WIL1_19_100cm, Date_time > "2019-12-10 01:00:01")
WIL1_19_100cm_clean <- filter(WIL1_19_100cm_clean, Date_time < "2019-12-11 05:00:01")

WIL1_19_100cm_clean$WC_100cm_neg[WIL1_19_100cm_clean$WC_100cm_neg > 0.2331] <- NA
missing <- which(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

if(1 %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[1] <- head(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}
if(nrow(WIL1_19_100cm_clean) %in% missing){
  WIL1_19_100cm_clean$WC_100cm_neg[nrow(data)] <- tail(WIL1_19_100cm_clean$WC_100cm_neg[!is.na(WIL1_19_100cm_clean$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_19_100cm_clean$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_19_100cm_clean$WC_100cm_neg[idx] <- (WIL1_19_100cm_clean$WC_100cm_neg[r$starts[i]] + WIL1_19_100cm_clean$WC_100cm_neg[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_19_early <- filter(WIL1_19_100cm, Date_time < "2019-12-10 01:00:01")
WIL1_19_late <- filter(WIL1_19_100cm, Date_time > "2019-12-11 05:00:01")
WIL1_19_100cm <- bind_rows(WIL1_19_early, WIL1_19_100cm_clean, WIL1_19_late)


#Recombine all of the different water content columns together 
#===============================================================
WIL1_19_clean <- merge(WIL1_19_15cm, WIL1_19_30cm_clean, by.x = "Date_time", by.y = "Date_time")
WIL1_19_clean <- merge(WIL1_19_clean, WIL1_19_100cm, by.x = "Date_time", by.y = "Date_time")

#Graphing 
#=======================================================================================

WIL1_19_clean$WC_15cm_neg <- as.numeric(WIL1_19_clean$WC_15cm_neg)
WIL1_19_clean$WC_30cm_neg <- as.numeric(WIL1_19_clean$WC_30cm_neg)
WIL1_19_clean$WC_100cm <- as.numeric(WIL1_19_clean$WC_100cm_neg)

Soil <- ggplot(data = subset(WIL1_19_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm_neg, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm_neg, color = "blue")) + 
  geom_line(aes(y = WC_15cm_neg, color = "lightblue"))
Soil 

#WIL1 2020
############################################################################
WIL1_20 <- subset(WIL1, Year == '2020')

#REPLACING NEGATIVE VALUES GREATER THAN A DAY WITH NA
#=================================================================================================
WIL1_20$WC_15cm <- as.numeric(WIL1_20$WC_15cm)
WIL1_20$WC_30cm <- as.numeric(WIL1_20$WC_30cm)
WIL1_20$WC_100cm <- as.numeric(WIL1_20$WC_100cm)

WIL1_20$Date_time<- mdy_hms(WIL1_20$Date_time)

#Find contiguous blocks of positive and negative values 
WIL1_20$block = cumsum(c(1,abs(diff(WIL1_20$WC_15cm < 0))))
WIL1_20$block_30 = cumsum(c(1,abs(diff(WIL1_20$WC_30cm < 0))))

#Count how many negative values are in each block 

WIL1_20$block_neg = ave(WIL1_20$WC_15cm, WIL1_20$block, FUN = function(x) {sum(x<0)})
WIL1_20$block_30neg = ave(WIL1_20$WC_30cm, WIL1_20$block_30, FUN = function(x) {sum(x<0)})

#Assign NA to blocks with greater than 144 negative values 
WIL1_20$WC_15cm_neg = ifelse(WIL1_20$block_neg>144, NA, WIL1_20$WC_15cm)
WIL1_20$WC_30cm_neg = ifelse(WIL1_20$block_30neg>144, NA, WIL1_20$WC_30cm)

#Create columns that will be indicators of missing values 
WIL1_20 <- WIL1_20 %>%
  add_column(WC_15cm_M = NA) %>%
  add_column(WC_30cm_M = NA) %>%
  add_column(WC_100cm_M = NA)

#Replace all NA values with M 
WIL1_20$WC_15cm_M[is.na(WIL1_20$WC_15cm_neg)] <- "M"
WIL1_20$WC_30cm_M[is.na(WIL1_20$WC_30cm_neg)] <- "M"

#INTERPOLATE LESS THAN ONE DAY MISSING VALUES 
###########################################################################################
WIL1_20$WC_30cm_neg[WIL1_20$WC_30cm_neg < 0] <- NA
missing <- which(is.na(WIL1_20$WC_30cm_M))

if(1 %in% missing){
  WIL1_20$WC_30cm_neg[1] <- head(WIL1_20$WC_30cm_neg[!is.na(WIL1_20$WC_30cm_neg)],1)
}
if(nrow(WIL1_20) %in% missing){
  WIL1_20$WC_30cm_neg[nrow(data)] <- tail(WIL1_20$WC_30cm_neg[!is.na(WIL1_20$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_20$WC_30cm_neg))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_20$WC_30cm_neg[idx] <- (WIL1_20$WC_30cm_neg[r$starts[i]] + WIL1_20$WC_30cm_neg[r$ends[i]])/2
}

#100 cm
###################################################################################

#Subset and remove drips
#==================================================================================
WIL1_20_fix <- filter(WIL1_20, Date_time > "2020-07-23 01:00:01")
WIL1_20_fix <- filter(WIL1_20_fix, Date_time < "2020-08-03 01:00:01")

WIL1_20_fix$WC_100cm[WIL1_20_fix$WC_100cm > 0.18] <- NA
missing <- which(is.na(WIL1_20_fix$WC_100cm))

if(1 %in% missing){
  WIL1_20_fix$WC_100cm[1] <- head(WIL1_20_fix$WC_100cm[!is.na(WIL1_20_fix$WC_100cm)],1)
}
if(nrow(WIL1_20_fix) %in% missing){
  WIL1_20_fix$WC_100cm[nrow(data)] <- tail(WIL1_20_fix$WC_100cm[!is.na(WIL1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_20_fix$WC_100cm[idx] <- (WIL1_20_fix$WC_100cm[r$starts[i]] + WIL1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL1_20_early <- filter(WIL1_20, Date_time < "2020-07-23 01:00:01")
WIL1_20_late <- filter(WIL1_20, Date_time > "2020-08-03 01:00:01")
WIL1_20 <- bind_rows(WIL1_20_early, WIL1_20_fix, WIL1_20_late)

#Create new dataframe 
WIL1_20$WC_15cm_neg <- as.numeric(WIL1_20$WC_15cm)
WIL1_20$WC_30cm_neg <- as.numeric(WIL1_20$WC_30cm)
WIL1_20$WC_100cm_neg <- as.numeric(WIL1_20$WC_100cm)

#Create new dataframe 
WIL1_20_clean <- select(WIL1_20, Date_time, WC_15cm_neg, WC_30cm_neg, WC_100cm_neg)

Soil <- ggplot(data = subset(WIL1_20_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm_neg, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm_neg, color = "blue")) + 
  geom_line(aes(y = WC_15cm_neg, color = "lightblue"))
Soil 

#WIL1 2021
##############################################################################################
WIL1_21 <- subset(WIL1, Year == '2021')

WIL1_21$WC_15cm_neg <- as.numeric(WIL1_21$WC_15cm)
WIL1_21$WC_30cm_neg <- as.numeric(WIL1_21$WC_30cm)
WIL1_21$WC_100cm_neg <- as.numeric(WIL1_21$WC_100cm)

WIL1_21$Date_time<- mdy_hms(WIL1_21$Date_time)

#INTERPOLATE LESS THAN ONE DAY MISSING VALUES 
###########################################################################################
#15 cm
#===============
###########################################################################################
WIL1_21$WC_15cm_neg[WIL1_21$WC_15cm_neg <0] <- NA
missing <- which(is.na(WIL1_21$WC_15cm_neg)) #Changed from WC_30cm_M to neg

if(1 %in% missing){
  WIL1_21$WC_15cm_neg[1] <- head(WIL1_21$WC_15cm_neg[!is.na(WIL1_21$WC_15cm_neg)],1)
}
if(nrow(WIL1_21) %in% missing){
  WIL1_21$WC_15cm_neg[nrow(data)] <- tail(WIL1_21$WC_15cm_neg[!is.na(WIL1_21$WC_15cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21$WC_15cm_neg))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21$WC_15cm_neg[idx] <- (WIL1_21$WC_15cm_neg[r$starts[i]] + WIL1_21$WC_15cm_neg[r$ends[i]])/2
}


#30 cm
#=================
WIL1_21$WC_30cm_neg[WIL1_21$WC_30cm_neg < 0] <- NA
missing <- which(is.na(WIL1_21$WC_30cm_neg)) #Changed from WC_30cm_M to neg

if(1 %in% missing){
  WIL1_21$WC_30cm_neg[1] <- head(WIL1_21$WC_30cm_neg[!is.na(WIL1_21$WC_30cm_neg)],1)
}
if(nrow(WIL1_21) %in% missing){
  WIL1_21$WC_30cm_neg[nrow(data)] <- tail(WIL1_21$WC_30cm_neg[!is.na(WIL1_21$WC_30cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21$WC_30cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21$WC_30cm_neg[idx] <- (WIL1_21$WC_30cm_neg[r$starts[i]] + WIL1_21$WC_30cm_neg[r$ends[i]])/2
}

#100 cm
#=================
WIL1_21$WC_100cm_neg[WIL1_21$WC_100cm_neg <0] <- NA
missing <- which(is.na(WIL1_21$WC_100cm_neg)) #Changed from WC_30cm_M to neg

if(1 %in% missing){
  WIL1_21$WC_100cm_neg[1] <- head(WIL1_21$WC_100cm_neg[!is.na(WIL1_21$WC_100cm_neg)],1)
}
if(nrow(WIL1_21) %in% missing){
  WIL1_21$WC_100cm_neg[nrow(data)] <- tail(WIL1_21$WC_100cm_neg[!is.na(WIL1_21$WC_100cm_neg)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21$WC_100cm_neg))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21$WC_100cm_neg[idx] <- (WIL1_21$WC_100cm_neg[r$starts[i]] + WIL1_21$WC_100cm_neg[r$ends[i]])/2
}

#Missing dates and glitch 
#########################################################################
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-06-04 09:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-06-10 01:00:01")

WIL1_21_fix$WC_100cm_neg[WIL1_21_fix$WC_100cm_neg > 0.18] <- NA
WIL1_21_fix$WC_30cm_neg[WIL1_21_fix$WC_30cm_neg > 0.18] <- NA
WIL1_21_fix$WC_15cm_neg[WIL1_21_fix$WC_15cm_neg > 0.18] <- NA

#Recombine 
#============================================================================
WIL1_21_early <- filter(WIL1_21, Date_time < "2021-06-04 09:00:01")
WIL1_21_late <- filter(WIL1_21, Date_time > "2021-06-10 01:00:01")
WIL1_21 <- bind_rows(WIL1_21_early, WIL1_21_fix, WIL1_21_late)

#Replace missing dates with NAs - 06/09 to 06/17
#==================================================================
#First remove extra rows
WIL1_21 <- select(WIL1_21, c(1:2, 6:9))

insertDF <- as.data.frame(matrix(data = NA, nrow = 7, ncol = 5))
colnames(insertDF) <- c("PAR", "Year", "WC_15cm_neg","WC_30cm_neg", "WC_100cm_neg")
Date_time <- seq(as.Date("2021-06-10"), as.Date("2021-06-16"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL1_21 <- insertRows(WIL1_21, c(22950:22956), new = insertDF)

#Plot to check 
Soil <- ggplot(data = subset(WIL1_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm_neg, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm_neg, color = "blue")) + 
  geom_line(aes(y = WC_15cm_neg, color = "lightblue"))
Soil 

#Create a clean dataframe 
WIL1_21_clean <- select(WIL1_21, Date_time, WC_15cm_neg, WC_30cm_neg, WC_100cm_neg)

Soil <- ggplot(data = subset(WIL1_21_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm_neg, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm_neg, color = "blue")) + 
  geom_line(aes(y = WC_15cm_neg, color = "lightblue"))
Soil 

#Combine all of the WIL1 dataframes/years into 1 dataframe 

#Merge 2018 and 2019 
WIL1_clean <- merge(WIL1_18_clean, WIL1_19_clean, by = c("Date_time", "WC_15cm_neg", "WC_30cm_neg", "WC_100cm_neg"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
WIL1_clean <- merge(WIL1_clean, WIL1_20_clean, by = c("Date_time", "WC_15cm_neg", "WC_30cm_neg", "WC_100cm_neg"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
WIL1_clean <- merge(WIL1_clean, WIL1_21_clean, by = c("Date_time", "WC_15cm_neg", "WC_30cm_neg", "WC_100cm_neg"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2017 
WIL1_17$WC_15cm_neg <- as.numeric(WIL1_17$WC_15cm)
WIL1_17$WC_30cm_neg <- as.numeric(WIL1_17$WC_30cm)
WIL1_17$WC_100cm_neg <- as.numeric(WIL1_17$WC_100cm)

WIL1_17_clean <- select(WIL1_17, Date_time, WC_15cm_neg, WC_30cm_neg, WC_100cm_neg)

WIL1_17_clean$Date_time<- mdy_hms(WIL1_17_clean$Date_time)


WIL1_clean <- merge(WIL1_clean, WIL1_17_clean, by = c("Date_time", "WC_15cm_neg", "WC_30cm_neg", "WC_100cm_neg"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(WIL1_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm_neg, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm_neg, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm_neg, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date")) 
Soil

png("WIL1_Salli", width = 4500, height = 2500)

Soil + theme(axis.line = element_line(size = 0.4,
                                      linetype = "solid"), axis.text = element_text(size = 60),
             panel.background = element_rect(fill = NA, 
                                             linetype = "solid"), legend.key = element_rect(fill = NA),
             legend.text = element_text(size = 60), legend.title = element_text(size = 60),
             legend.background = element_rect(fill = NA)) + theme(axis.text = element_text(size = 60))+ 
  theme(axis.title.y = element_text(family = "Times New Roman", size = 70)) +
  theme(axis.title.x = element_text(family = "Times New Roman", size = 70)) 
dev.off()

#Write the csv
write.csv(WIL1_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL1_clean.csv") #this writes a csv file and sends it to the working folder


