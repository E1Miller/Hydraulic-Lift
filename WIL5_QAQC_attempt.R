#Created by: Elise Miller
#Date started: 10/25/2022
#Date last edited: 02/20/2023
#Description: QA/QC WIL 5

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
library(padr)
library(berryFunctions)

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL", 
                        pattern=glob2rx("W5M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console


#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
WIL5_2019_2021 <- data19_21 %>%
  rename_all(funs(c("Date", "Time", "PAR", "WC_15cm", "WC_30cm","WC_100cm", "Date_time"))) %>% #this renames all of the columns
  select(Date_time, PAR, WC_15cm, WC_30cm, WC_100cm) #this keeps only the columns you will use


#2017-2019 FILES
#========================================================================================================================
#If wanting to merge the files, needed to manually delete the extra column in W1M190111 and 
#extra row at the top, and needed to manually delete the extra columns in W1M171228, W1M180201, and W1M180302

#Set the data path 
data_path <- "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL"
new_col_name <- c("Date_time", "PAR", "WC_15cm", "WC_30cm", "WC_100cm")

#Call in all the files in this folder with the W2M1 pattern, which excludes the datasheets from 2019-2020
files <- dir(data_path, pattern=glob2rx("W5M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
WIL5_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
WIL5 <- rbind(WIL5_2017_2019, WIL5_2019_2021)

#Write the csv
write.csv(WIL5,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL5.csv") #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(WIL5)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
WIL5$Date <- mdy_hms(WIL5$Date_time)

#Put year into a separate column 
WIL5 <- separate(WIL5, Date, c("Year"))

#WIL5 2017
##################################################################################################
WIL5_17 <- subset(WIL5, Year == '2017')

#Plotting 
WIL5_17$WC_15cm <- as.numeric(WIL5_17$WC_15cm)
WIL5_17$WC_30cm <- as.numeric(WIL5_17$WC_30cm)
WIL5_17$WC_100cm <- as.numeric(WIL5_17$WC_100cm)
WIL5_17$Date_time<- mdy_hms(WIL5_17$Date_time)

#Plot
Soil <- ggplot(data = subset(WIL5_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL5 2018
##################################################################################################
WIL5_18 <- subset(WIL5, Year == '2018')

#Plotting 
WIL5_18$WC_15cm <- as.numeric(WIL5_18$WC_15cm)
WIL5_18$WC_30cm <- as.numeric(WIL5_18$WC_30cm)
WIL5_18$WC_100cm <- as.numeric(WIL5_18$WC_100cm)
WIL5_18$Date_time<- mdy_hms(WIL5_18$Date_time)

#Get rid of negative values
#15 cm
###########################################
WIL5_18$WC_15cm[WIL5_18$WC_15cm < 0.12] <- NA
missing <- which(is.na(WIL5_18$WC_15cm))

if(1 %in% missing){
  WIL5_18$WC_15cm[1] <- head(WIL5_18$WC_15cm[!is.na(WIL5_18$WC_15cm)],1)
}
if(nrow(WIL5_18) %in% missing){
  WIL5_18$WC_15cm[nrow(data)] <- tail(WIL5_18$WC_15cm[!is.na(WIL5_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18$WC_15cm[idx] <- (WIL5_18$WC_15cm[r$starts[i]] + WIL5_18$WC_15cm[r$ends[i]])/2
}

#Subset and remove drips in June
#===============================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-06-07 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-06-10 12:00:01")

WIL5_18_fix$WC_15cm[WIL5_18_fix$WC_15cm < 0.315 | WIL5_18_fix$WC_15cm > 0.32] <- NA
missing <- which(is.na(WIL5_18_fix$WC_15cm))

if(1 %in% missing){
  WIL5_18_fix$WC_15cm[1] <- head(WIL5_18_fix$WC_15cm[!is.na(WIL5_18_fix$WC_15cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_15cm[nrow(data)] <- tail(WIL5_18_fix$WC_15cm[!is.na(WIL5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_15cm[idx] <- (WIL5_18_fix$WC_15cm[r$starts[i]] + WIL5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-06-07 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-06-10 12:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset and remove drips in June
#===============================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-09-28 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-10-01 12:00:01")

WIL5_18_fix$WC_15cm[WIL5_18_fix$WC_15cm < 0.194] <- NA

#Recombine 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-09-28 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-10-01 12:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset and remove drips in June
#===============================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-10-01 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-10-03 12:00:01")

WIL5_18_fix$WC_15cm[WIL5_18_fix$WC_15cm < 0.195] <- NA
missing <- which(is.na(WIL5_18_fix$WC_15cm))

if(1 %in% missing){
  WIL5_18_fix$WC_15cm[1] <- head(WIL5_18_fix$WC_15cm[!is.na(WIL5_18_fix$WC_15cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_15cm[nrow(data)] <- tail(WIL5_18_fix$WC_15cm[!is.na(WIL5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_15cm[idx] <- (WIL5_18_fix$WC_15cm[r$starts[i]] + WIL5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-10-01 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-10-03 12:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset and remove drips in June
#===============================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-11-19 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-03 12:00:01")

WIL5_18_fix$WC_15cm[WIL5_18_fix$WC_15cm < 0.19] <- NA
missing <- which(is.na(WIL5_18_fix$WC_15cm))

if(1 %in% missing){
  WIL5_18_fix$WC_15cm[1] <- head(WIL5_18_fix$WC_15cm[!is.na(WIL5_18_fix$WC_15cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_15cm[nrow(data)] <- tail(WIL5_18_fix$WC_15cm[!is.na(WIL5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_15cm[idx] <- (WIL5_18_fix$WC_15cm[r$starts[i]] + WIL5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-11-19 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-03 12:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#30 cm 
################################################################
WIL5_18$WC_30cm[WIL5_18$WC_30cm < 0.17] <- NA
missing <- which(is.na(WIL5_18$WC_30cm))

if(1 %in% missing){
  WIL5_18$WC_30cm[1] <- head(WIL5_18$WC_30cm[!is.na(WIL5_18$WC_30cm)],1)
}
if(nrow(WIL5_18) %in% missing){
  WIL5_18$WC_30cm[nrow(data)] <- tail(WIL5_18$WC_30cm[!is.na(WIL5_18$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18$WC_30cm[idx] <- (WIL5_18$WC_30cm[r$starts[i]] + WIL5_18$WC_30cm[r$ends[i]])/2
}

#Subset for drip right before increase in November
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-11-20 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-11-22 08:19:01")

WIL5_18_fix$WC_30cm[WIL5_18_fix$WC_30cm < 0.175] <- NA
missing <- which(is.na(WIL5_18_fix$WC_30cm))

if(1 %in% missing){
  WIL5_18_fix$WC_30cm[1] <- head(WIL5_18_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_30cm[nrow(data)] <- tail(WIL5_18_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_30cm[idx] <- (WIL5_18_fix$WC_30cm[r$starts[i]] + WIL5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-11-20 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-11-22 08:19:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#100 cm 
################################################################
WIL5_18$WC_100cm[WIL5_18$WC_100cm < 0.11] <- NA
missing <- which(is.na(WIL5_18$WC_100cm))

if(1 %in% missing){
  WIL5_18$WC_100cm[1] <- head(WIL5_18$WC_100cm[!is.na(WIL5_18$WC_100cm)],1)
}
if(nrow(WIL5_18) %in% missing){
  WIL5_18$WC_100cm[nrow(data)] <- tail(WIL5_18$WC_100cm[!is.na(WIL5_18$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18$WC_100cm[idx] <- (WIL5_18$WC_100cm[r$starts[i]] + WIL5_18$WC_100cm[r$ends[i]])/2
}


#Fix 100 cm dips 
#=============================================================================
#Subset mid January again 
#===========================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-09-07 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-09-22 12:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.13] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-09-07 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-09-22 12:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Fix 100 cm dips later on in the year 
#================================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-11-07 1:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.22] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-11-07 1:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix)

#Fix 100 cm dips later on in the year 
#================================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-11-30 1:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.26] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-11-30 1:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix)

#Fix October drips 
#==============================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-09-22 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-09-28 12:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.126] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-09-22 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-09-28 12:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for other October dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-10-03 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-10-22 12:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.115] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-10-03 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-10-22 12:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for other October dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-10-23 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-11-05 12:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.1185] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-10-23 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-11-05 12:00:01")

WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-11-28 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-11-29 00:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.25] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-11-28 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-11-29 00:00:01")

WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-01 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-02 00:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.289] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-01 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-02 00:00:01")

WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-02 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-05 00:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.275] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-02 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-05 00:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-06 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-12 00:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.2683] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-06 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-12 00:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-26 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-28 00:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.2825] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-26 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-28 00:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-28 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-31 00:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.2782] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-28 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-31 00:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-11-28 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-11-30 00:00:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.25] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-11-28 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-11-30 00:00:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-28 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-31 23:50:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.278] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-28 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-31 23:50:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)


#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-11-02 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-11-30 23:50:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm == 0.2425] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-11-02 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-11-30 23:50:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-11-24 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-11-24 19:19:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.25] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-11-24 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-11-24 19:19:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-03 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-06 08:19:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.275] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-03 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-06 08:19:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-06 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-08 08:19:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.27] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-06 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-08 08:19:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Subset for November/December dates
#=========================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-10 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-12 08:19:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm < 0.2686] <- NA
missing <- which(is.na(WIL5_18_fix$WC_100cm))

if(1 %in% missing){
  WIL5_18_fix$WC_100cm[1] <- head(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}
if(nrow(WIL5_18_fix) %in% missing){
  WIL5_18_fix$WC_100cm[nrow(data)] <- tail(WIL5_18_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_18_fix$WC_100cm[idx] <- (WIL5_18_fix$WC_100cm[r$starts[i]] + WIL5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-10 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-12 08:19:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)


#Missing dates and glitches
#############################################################################

#Fix glitch before 04/20 missing date
#===============================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-04-19 1:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-04-20 20:50:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm > 0.295] <- NA
WIL5_18_fix$WC_30cm[WIL5_18_fix$WC_30cm > 0.325] <- NA
WIL5_18_fix$WC_15cm[WIL5_18_fix$WC_15cm > 0.3425] <- NA

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-04-19 1:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-04-20 20:50:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Fix glitch before 06/24 missing date
#===============================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-06-23 20:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-06-24 20:50:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm > 0.241] <- NA
WIL5_18_fix$WC_30cm[WIL5_18_fix$WC_30cm > 0.278] <- NA
WIL5_18_fix$WC_15cm[WIL5_18_fix$WC_15cm > 0.296] <- NA

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-06-23 20:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-06-24 20:50:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Fix glitch before 10/14 missing date
#===============================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-10-12 00:00:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-10-14 20:50:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm > 0.128] <- NA
WIL5_18_fix$WC_30cm[WIL5_18_fix$WC_30cm > 0.24] <- NA
WIL5_18_fix$WC_15cm[WIL5_18_fix$WC_15cm > 0.258] <- NA

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-10-12 00:00:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-10-14 20:50:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Fix glitch before 10/14 missing date
#===============================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-08-20 23:29:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-08-22 12:09:01")


WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm > 0] <- NA
WIL5_18_fix$WC_30cm[WIL5_18_fix$WC_30cm > 0] <- NA
WIL5_18_fix$WC_15cm[WIL5_18_fix$WC_15cm > 0] <- NA

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-08-20 23:29:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-08-22 12:09:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Fix glitch before 12/17 missing date
#===============================================================================
WIL5_18_fix <- filter(WIL5_18, Date_time > "2018-12-15 12:59:01")
WIL5_18_fix <- filter(WIL5_18_fix, Date_time < "2018-12-19 12:09:01")

WIL5_18_fix$WC_100cm[WIL5_18_fix$WC_100cm > 0.278] <- NA
WIL5_18_fix$WC_30cm[WIL5_18_fix$WC_30cm > 0.325] <- NA
WIL5_18_fix$WC_15cm[WIL5_18_fix$WC_15cm > 0.332] <- NA

#Recombine July with other dataset 
WIL5_18_later <- filter(WIL5_18, Date_time < "2018-12-15 12:59:01")
WIL5_18_end <- filter(WIL5_18, Date_time > "2018-12-19 12:09:01")
WIL5_18 <- bind_rows(WIL5_18_later, WIL5_18_fix, WIL5_18_end)

#Fix dates
#==========================================================================
#Replace missing dates with NAs - 04/20 to 05/03
insertDF <- as.data.frame(matrix(data = NA, nrow = 12, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2018-04-21"), as.Date("2018-05-02"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL5_18 <- insertRows(WIL5_18, c(15292:15304), new = insertDF)

WIL5_18 <- WIL5_18[-c(15304), ] #Delete the extra 04-21 that appears

#Replace missing dates with NAs - 06/24 to 06/28
insertDF <- as.data.frame(matrix(data = NA, nrow = 3, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2018-06-25"), as.Date("2018-06-27"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL5_18 <- insertRows(WIL5_18, c(22765:22768), new = insertDF)

WIL5_18 <- WIL5_18[-c(22768), ] #Delete the extra 04-21 that appears

#Replace missing dates with NAs - 10/14 to 10/25
insertDF <- as.data.frame(matrix(data = NA, nrow = 10, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2018-10-15"), as.Date("2018-10-24"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL5_18 <- insertRows(WIL5_18, c(38276:38286), new = insertDF)
WIL5_18 <- WIL5_18[-c(38286), ]

#Replace missing dates with NAs - 12/18 to 12/25
insertDF <- as.data.frame(matrix(data = NA, nrow = 8, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2018-12-18"), as.Date("2018-12-25"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL5_18 <- insertRows(WIL5_18, c(45994:46002), new = insertDF)
WIL5_18 <- WIL5_18[-c(46002), ]

#Plot again 
Soil <- ggplot(data = subset(WIL5_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL5 2019
##################################################################################################
WIL5_19 <- subset(WIL5, Year == '2019')

#Plotting 
WIL5_19$WC_15cm <- as.numeric(WIL5_19$WC_15cm)
WIL5_19$WC_30cm <- as.numeric(WIL5_19$WC_30cm)
WIL5_19$WC_100cm <- as.numeric(WIL5_19$WC_100cm)
WIL5_19$Date_time<- mdy_hms(WIL5_19$Date_time)

#15 cm 
################################################################
WIL5_19$WC_15cm[WIL5_19$WC_15cm < 0.19] <- NA
missing <- which(is.na(WIL5_19$WC_15cm))

if(1 %in% missing){
  WIL5_19$WC_15cm[1] <- head(WIL5_19$WC_15cm[!is.na(WIL5_19$WC_15cm)],1)
}
if(nrow(WIL5_19) %in% missing){
  WIL5_19$WC_15cm[nrow(data)] <- tail(WIL5_19$WC_15cm[!is.na(WIL5_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19$WC_15cm[idx] <- (WIL5_19$WC_15cm[r$starts[i]] + WIL5_19$WC_15cm[r$ends[i]])/2
}

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-03-18 1:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-03-21 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm < 0.3316] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-03-18 1:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-03-21 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-05-14 1:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-05-21 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm < 0.309] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-05-14 1:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-05-21 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-07-12 1:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-07-21 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm > 0.256] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-07-12 1:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-07-21 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-07-17 1:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-07-19 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm < 0.247] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-07-17 1:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-07-19 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-02 1:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-04 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm > 0.2285] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-08-02 1:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-08-04 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-02 21:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-04 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm > 0.2275] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-08-02 21:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-08-04 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-02 21:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-13 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm < 0.2195] <- NA

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-08-02 21:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-08-13 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-20 21:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-25 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm < 0.2075] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-08-20 21:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-08-25 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-20 21:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-25 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm == 0.20780] <- NA

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-08-20 21:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-08-25 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-20 21:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-10-25 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm < 0.2] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-08-20 21:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-10-25 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-20 21:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-10-02 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm < 0.215] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-09-20 21:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-10-02 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-10-02 21:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-12-02 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm < 0.205] <- NA
missing <- which(is.na(WIL5_19_fix$WC_15cm))

if(1 %in% missing){
  WIL5_19_fix$WC_15cm[1] <- head(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_15cm[nrow(data)] <- tail(WIL5_19_fix$WC_15cm[!is.na(WIL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_15cm[idx] <- (WIL5_19_fix$WC_15cm[r$starts[i]] + WIL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-10-02 21:00:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-12-02 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#Fix 15 cm drips 
#=======================================================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-10-17 11:50:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-12-02 01:00:01")

WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm == 0.2282] <- NA

#Recombine 
#============================================================================
WIL5_19_early <- filter(WIL5_19, Date_time < "2019-10-17 11:50:01")
WIL5_19_late <- filter(WIL5_19, Date_time > "2019-12-02 01:00:01")
WIL5_19 <- bind_rows(WIL5_19_early, WIL5_19_fix, WIL5_19_late)

#30 cm 
################################################################
WIL5_19$WC_30cm[WIL5_19$WC_30cm < 0.17] <- NA
missing <- which(is.na(WIL5_19$WC_30cm))

if(1 %in% missing){
  WIL5_19$WC_30cm[1] <- head(WIL5_19$WC_30cm[!is.na(WIL5_19$WC_30cm)],1)
}
if(nrow(WIL5_19) %in% missing){
  WIL5_19$WC_30cm[nrow(data)] <- tail(WIL5_19$WC_30cm[!is.na(WIL5_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19$WC_30cm[idx] <- (WIL5_19$WC_30cm[r$starts[i]] + WIL5_19$WC_30cm[r$ends[i]])/2
}

#Subset early April
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-04-15 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-05-01 00:00:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-04-15 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-05-01 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset June
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-05-15 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-06-04 00:00:01")


WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.31] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-05-15 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-06-04 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset June
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-06-05 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-06-23 00:00:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.278] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-06-05 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-06-23 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-07-01 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-07-15 00:00:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.24] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-07-01 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-07-15 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-07-22 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-05 00:00:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.225] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-07-22 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-08-05 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset August
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-07 14:30:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-09-10 00:00:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.22] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-07 14:30:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-09-10 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset August
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-10 14:30:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-09-15 00:00:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.2] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-10 14:30:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-09-15 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset August
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-15 20:30:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-09-19 00:00:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.245] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-15 20:30:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-09-19 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset August
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-09 20:30:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-09-11 00:00:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.19] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-09 20:30:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-09-11 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset December
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-12-23 20:30:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-12-31 00:00:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.315] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-12-23 20:30:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-12-31 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset December
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-12-30 00:30:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-12-31 23:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.30] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-12-30 00:30:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-12-31 23:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-07-03 00:30:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-07-08 23:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.261] <- NA

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-07-03 00:30:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-07-08 23:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-07-25 12:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-08 23:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.215] <- NA

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-07-25 12:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-08-08 23:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-13 12:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-28 23:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.2075] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-08-13 12:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-08-28 23:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-21 12:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-28 23:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.2015] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-08-21 12:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-08-28 23:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-22 02:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-22 08:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.197] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-08-22 02:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-08-22 08:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-08-23 02:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-08-28 08:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.199] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-08-23 02:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-08-28 08:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-16 02:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-09-16 13:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.275] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-16 02:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-09-16 13:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-15 02:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-09-19 13:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.193] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-15 02:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-09-19 13:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-15 19:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-09-16 03:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-15 19:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-09-16 03:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-17 12:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-09-19 03:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm < 0.2752] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-17 12:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-09-19 03:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-23 23:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-10-19 03:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.23] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-23 23:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-10-19 03:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-24 23:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-10-19 03:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.22] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-24 23:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-10-19 03:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-25 23:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-10-19 03:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.219] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-25 23:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-10-19 03:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-26 05:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-10-19 03:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.218] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-26 05:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-10-19 03:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-27 05:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-10-19 03:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.2125] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-26 05:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-10-19 03:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-09-29 12:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-10-19 03:50:01")

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.20] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-29 12:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-10-19 03:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Remove if below a certain decrease 
#=======================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-01 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-12-31 00:00:01")

#Create a new column that is the percent difference between the row below and the row above 
WIL5_19_fix <- WIL5_19_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================
#Make increase column not a percent 
WIL5_19_fix <- transform(WIL5_19_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

WIL5_19_fix <- transform(WIL5_19_fix, WC_30cm=ifelse(incr < -0.02, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/48, 48), sides=2)), 
                                                     WC_30cm))



#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-01 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-12-31 00:00:01")

WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Remove if below a certain increase
#=======================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-01 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-12-31 00:00:01")

#Create a new column that is the percent difference between the row below and the row above 
WIL5_19_fix <- WIL5_19_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================


#Make increase column not a percent 
WIL5_19_fix <- transform(WIL5_19_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

WIL5_19_fix <- transform(WIL5_19_fix, WC_30cm=ifelse(incr > 0.03, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/48, 48), sides=2)), 
                                                     WC_30cm))



#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-01 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-12-31 00:00:01")

WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset July 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-04-23 05:20:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-06-29 03:50:01")

Soil <- ggplot(data = subset(WIL5_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "green"))
Soil 

WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.218] <- NA
missing <- which(is.na(WIL5_19_fix$WC_30cm))

if(1 %in% missing){
  WIL5_19_fix$WC_30cm[1] <- head(WIL5_19_fix$WC_30cm[!is.na(WIL5_19_fix$WC_30cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_30cm[nrow(data)] <- tail(WIL5_19_fix$WC_30cm[!is.na(WIL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_30cm[idx] <- (WIL5_19_fix$WC_30cm[r$starts[i]] + WIL5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-09-26 05:20:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-10-19 03:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)


#100 cm 
################################################################
WIL5_19$WC_100cm[WIL5_19$WC_100cm < 0.25] <- NA
missing <- which(is.na(WIL5_19$WC_100cm))

if(1 %in% missing){
  WIL5_19$WC_100cm[1] <- head(WIL5_19$WC_100cm[!is.na(WIL5_19$WC_100cm)],1)
}
if(nrow(WIL5_19) %in% missing){
  WIL5_19$WC_100cm[nrow(data)] <- tail(WIL5_19$WC_100cm[!is.na(WIL5_19$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19$WC_100cm[idx] <- (WIL5_19$WC_100cm[r$starts[i]] + WIL5_19$WC_100cm[r$ends[i]])/2
}

#Edit 100 cm drips
##########################################

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-01 1:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-28 23:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.27] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-01 1:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-28 23:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-01 1:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-04 12:50:01")


Soil <- ggplot(data = subset(WIL5_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil 

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2738] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-01 1:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-04 12:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-06 02:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-06 12:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.28] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-06 02:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-06 12:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-08 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-09 02:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.295] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-08 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-09 02:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-08 18:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-09 01:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.31] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-08 18:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-09 01:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-08 18:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-09 08:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.305] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-08 18:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-09 08:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-09 08:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-09 20:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.293] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-09 08:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-09 20:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-09 08:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-09 23:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.297] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-09 08:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-09 23:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-09 08:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-10 10:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.29499] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-09 08:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-10 10:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-10 08:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-12 10:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.285] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-10 08:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-12 10:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-10 08:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-10 20:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.293] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-10 08:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-10 20:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-10 18:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-10 23:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.292] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-10 18:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-10 23:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-10 23:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-11 12:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.29] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-10 23:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-11 12:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-11 10:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-13 02:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.286] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-11 10:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-13 02:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-15 20:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-16 22:20:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.292] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-15 20:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-16 22:20:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-16 05:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-16 08:30:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.36] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-16 05:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-16 08:30:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-16 05:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-16 13:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.34] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-16 05:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-16 13:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-16 13:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-16 18:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.305] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-16 05:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-16 18:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)


#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-16 13:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-17 04:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.305] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-16 13:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-17 04:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-17 00:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-19 04:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2974] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-17 00:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-19 04:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-19 00:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-21 04:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.3] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-19 00:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-21 04:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-20 00:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-20 03:10:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.326] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-20 00:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-20 03:10:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-20 09:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-20 13:10:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.325] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-20 09:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-20 13:10:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-20 13:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-20 20:10:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.305] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-20 13:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-20 20:10:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-20 13:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-21 13:10:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.295] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-20 13:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-21 13:10:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-23 03:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-24 19:10:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2857] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-23 03:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-24 19:10:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-23 03:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-24 11:40:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.28615] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-23 03:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-24 11:40:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-23 03:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-30 11:40:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.28] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-23 03:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-30 11:40:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-28 13:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-30 11:40:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2805] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-28 13:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-30 11:40:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-31 03:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-02 11:40:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2769] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-31 03:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-02 11:40:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-01 00:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-02 11:40:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2788] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-01 00:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-02 11:40:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-03 19:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-04 10:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.305] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-03 19:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-04 10:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)
            
#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-03 20:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-03 23:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.325] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-03 20:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-03 23:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-03 22:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-04 00:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.32] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-03 22:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-04 00:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-04 00:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-04 05:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.318] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-04 00:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-04 05:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-05 20:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-06 08:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2924] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-05 20:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-06 08:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-05 20:10:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-07 08:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2892] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-05 20:10:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-07 08:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-12 12:50:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-14 08:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.3] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-12 12:50:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-14 08:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-12 14:50:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-12 23:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.34] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-12 14:50:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-12 23:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-12 19:50:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-12 22:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.375] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-12 19:50:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-12 22:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-12 21:50:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-13 02:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.321] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-12 21:50:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-13 02:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-12 21:50:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-14 02:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.31] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-12 21:50:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-14 02:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-13 22:50:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-14 00:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.33] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-13 22:50:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-14 00:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-14 00:50:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-14 07:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.315] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-14 00:50:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-14 07:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-05 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-14 12:50:01")

Soil <- ggplot(data = subset(WIL5_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil 

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.272] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-05 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-14 12:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-08 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-12 00:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.289] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-08 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-12 00:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)


#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-12 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-16 00:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2825] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-12 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-16 00:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-16 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-19 00:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.29] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-16 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-19 00:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-20 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-21 00:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.30] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-20 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-21 00:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-21 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-25 00:50:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.285] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-21 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-25 00:50:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-25 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-27 00:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2775] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-25 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-27 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-28 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-31 00:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2792] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-28 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-31 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-04 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-09 00:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2855] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-04 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-09 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-09 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-13 00:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.292] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-09 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-13 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-13 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-14 00:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.295] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-13 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-14 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-28 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-01 00:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.277] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-28 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-01 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-04 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-04 19:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.3] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-04 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-04 19:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-01 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-01 19:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.2765] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-01 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-01 19:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-01 19:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-02 19:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.275] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-01 19:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-02 19:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-05 12:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-05 19:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.275] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-05 12:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-05 19:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-05 13:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-05 19:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.276] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-05 13:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-05 19:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-05 16:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-05 19:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.28] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-05 16:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-05 19:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-05 17:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-01-07 19:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.284] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-05 17:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-07 19:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Subset early January/February 
#==========================================================
#WIL5_19_fix <- filter(WIL5_19, Date_time > "2018-12 00:00:01")
WIL5_19_fix <- filter(WIL5_19, Date_time < "2019-01-02 03:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm < 0.276] <- NA
missing <- which(is.na(WIL5_19_fix$WC_100cm))

if(1 %in% missing){
  WIL5_19_fix$WC_100cm[1] <- head(WIL5_19_fix$WC_100cm[!is.na(WIL5_19_fix$WC_100cm)],1)
}
if(nrow(WIL5_19_fix) %in% missing){
  WIL5_19_fix$WC_100cm[nrow(data)] <- tail(WIL5_19_fix$WC_100cm[!is.na(WIL5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_19_fix$WC_100cm[idx] <- (WIL5_19_fix$WC_100cm[r$starts[i]] + WIL5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
#WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-05 17:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-01-02 03:00:01")
WIL5_19 <- bind_rows(WIL5_19_fix, WIL5_19_end)

#Remove if below a certain decrease 
#=======================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-01-01 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-04-17 00:00:01")

#Create a new column that is the percent difference between the row below and the row above 
WIL5_19_fix <- WIL5_19_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_100cm-lag(WC_100cm),
    increase=scales::percent(diff / lag(WC_100cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================


#Make increase column not a percent 
WIL5_19_fix <- transform(WIL5_19_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

WIL5_19_fix <- transform(WIL5_19_fix, WC_100cm=ifelse(incr < -0.1, 
                                                             as.numeric(stats::filter(WC_100cm, rep(1/48, 48), sides=2)), 
                                                      WC_100cm))

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-01-01 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-04-17 00:00:01")

WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Missing dates and fix glitches
###########################################################################

#Glitch before 02/19 date
#=========================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-02-17 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-02-20 00:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm > 0] <- NA
WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0] <- NA
WIL5_19_fix$WC_15cm[WIL5_19_fix$WC_15cm > 0] <- NA

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-02-17 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-02-20 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Glitch before 03/19 date
#=========================================================================
WIL5_19_fix <- filter(WIL5_19, Date_time > "2019-03-13 09:00:01")
WIL5_19_fix <- filter(WIL5_19_fix, Date_time < "2019-03-20 00:00:01")

WIL5_19_fix$WC_100cm[WIL5_19_fix$WC_100cm > 0.35] <- NA
WIL5_19_fix$WC_30cm[WIL5_19_fix$WC_30cm > 0.325] <- NA

#Recombine July with other dataset 
WIL5_19_later <- filter(WIL5_19, Date_time < "2019-03-13 09:00:01")
WIL5_19_end <- filter(WIL5_19, Date_time > "2019-03-20 00:00:01")
WIL5_19 <- bind_rows(WIL5_19_later, WIL5_19_fix, WIL5_19_end)

#Remove extra columns
WIL5_19 <- WIL5_19[c(1:6)]

#Replace missing dates with NAs - 02/19 to 03/14
insertDF <- as.data.frame(matrix(data = NA, nrow = 22, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-02-20"), as.Date("2019-03-13"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL5_19 <- insertRows(WIL5_19, c(7065:7086), new = insertDF)

#Replace missing dates with NAs - 10-14 to 10-25
insertDF <- as.data.frame(matrix(data = NA, nrow = 53, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-10-19"), as.Date("2019-12-10"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL5_19 <- insertRows(WIL5_19, c(38329:38382), new = insertDF)

#Remove 30 cm glitches 
#=====================================================================
WIL5_19$WC_30cm[WIL5_19$WC_30cm > 0.30579 & WIL5_19$WC_30cm < 0.30581] <- NA

#Plot again
Soil <- ggplot(data = subset(WIL5_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL5 2020
##################################################################################################
WIL5_20 <- subset(WIL5, Year == '2020')

#Plotting 
WIL5_20$WC_15cm <- as.numeric(WIL5_20$WC_15cm)
WIL5_20$WC_30cm <- as.numeric(WIL5_20$WC_30cm)
WIL5_20$WC_100cm <- as.numeric(WIL5_20$WC_100cm)
WIL5_20$Date_time<- mdy_hms(WIL5_20$Date_time)

#Get rid of negative values
#15 cm
###########################################################################
WIL5_20$WC_15cm[WIL5_20$WC_15cm < 0] <- NA
missing <- which(is.na(WIL5_20$WC_15cm))

if(1 %in% missing){
  WIL5_20$WC_15cm[1] <- head(WIL5_20$WC_15cm[!is.na(WIL5_20$WC_15cm)],1)
}
if(nrow(WIL5_20) %in% missing){
  WIL5_20$WC_15cm[nrow(data)] <- tail(WIL5_20$WC_15cm[!is.na(WIL5_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20$WC_15cm[idx] <- (WIL5_20$WC_15cm[r$starts[i]] + WIL5_20$WC_15cm[r$ends[i]])/2
}

#Remove 15cm glitches
#=====================================================================
WIL5_20$WC_15cm[WIL5_20$WC_15cm > 0.7183 & WIL5_20$WC_15cm < 0.7185] <- NA

#Fix drips 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-03-11 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-03-15 14:10:01")

WIL5_20fix$WC_15cm[WIL5_20fix$WC_15cm < 0.283] <- NA
missing <- which(is.na(WIL5_20fix$WC_15cm))

if(1 %in% missing){
  WIL5_20fix$WC_15cm[1] <- head(WIL5_20fix$WC_15cm[!is.na(WIL5_20fix$WC_15cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_15cm[nrow(data)] <- tail(WIL5_20fix$WC_15cm[!is.na(WIL5_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_15cm[idx] <- (WIL5_20fix$WC_15cm[r$starts[i]] + WIL5_20fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-03-11 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-03-15 14:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Fix drips 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-03-05 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-03-10 14:10:01")

WIL5_20fix$WC_15cm[WIL5_20fix$WC_15cm < 0.284] <- NA

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-03-05 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-03-10 14:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#30 cm 
################################################################
WIL5_20$WC_30cm[WIL5_20$WC_30cm < 0.19] <- NA
missing <- which(is.na(WIL5_20$WC_30cm))

if(1 %in% missing){
  WIL5_20$WC_30cm[1] <- head(WIL5_20$WC_30cm[!is.na(WIL5_20$WC_30cm)],1)
}
if(nrow(WIL5_20) %in% missing){
  WIL5_20$WC_30cm[nrow(data)] <- tail(WIL5_20$WC_30cm[!is.na(WIL5_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20$WC_30cm[idx] <- (WIL5_20$WC_30cm[r$starts[i]] + WIL5_20$WC_30cm[r$ends[i]])/2
}

#Fix 30 cm drips
#=================================================================================
WIL5_20fix <- WIL5_20 %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)

WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-02 1:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time > "2020-01-04 1:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.32] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

WIL5_20fix <- transform(WIL5_20fix, incr=as.numeric(gsub('\\%', '', increase))/100)

WIL5_20fix <- transform(WIL5_20fix, WC_30cm=ifelse(incr < -0.01, 
                                                   as.numeric(stats::filter(WC_30cm, rep(1/48, 48), sides=2)), 
                                                   WC_30cm))
WIL5_20_fix <- select(WIL5_20fix, "Date_time", "WC_15cm", "WC_30cm", "WC_100cm")


#Recombine datasets
#Recombine July with other dataset 
WIL5_20_later <- filter(WIL5_20, Date_time > "2020-02-02 7:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time < "2020-01-04 1:00:01")
WIL5_20clean <- bind_rows(WIL5_20_fix, WIL5_20_end)
WIL5_20_clean <- select(WIL5_20clean, "Date_time", "WC_15cm", "WC_30cm", "WC_100cm")
WIL5_20clean <- bind_rows(WIL5_20_fix, WIL5_20_later)

WIL5_20 <- WIL5_20clean

#Fix 30 cm drips later in February
#===================================================================================
WIL5_20fix <- filter(WIL5_20, Date_time < "2020-02-12 1:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time > "2020-01-31 1:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.313] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}


#Merge data frame again 

WIL5_20_later <- filter(WIL5_20, Date_time > "2020-02-12 1:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time < "2020-01-31 1:00:01")
WIL5_20_clean <- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

WIL5_20 <- WIL5_20_clean

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time < "2020-03-01 1:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time > "2020-02-04 1:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.313] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-04 1:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-03-01 1:00:01")
WIL5_20_clean <- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)
WIL5_20 <- WIL5_20_clean

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-04 1:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-05 04:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3241] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-04 1:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-05 04:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-05 1:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-06 00:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.323] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-05 1:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-06 00:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-06 1:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-07 13:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.321] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-06 1:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-07 13:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-07 17:10:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-08 13:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.325] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-07 17:10:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-08 13:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-07 20:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-08 01:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.34] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-07 20:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-08 01:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-08 01:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-08 04:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3362] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-08 01:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-08 04:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-08 10:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-09 04:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.331] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-08 10:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-09 04:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-08 19:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-08 22:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.36] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-08 19:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-08 22:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-08 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-08 23:50:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.355] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-08 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-08 23:50:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-08 23:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-09 04:50:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3348] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-08 23:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-09 04:50:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-09 15:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-10 08:50:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.326] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-09 15:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-10 08:50:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-10 05:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-12 08:50:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.325] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-10 05:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-12 08:50:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-10 21:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-11 19:50:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.327] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-10 21:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-11 19:50:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-12 12:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-12 14:50:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.34] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-12 12:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-12 14:50:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-12 14:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-12 23:50:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.333] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-12 14:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-12 23:50:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-13 08:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-13 23:50:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.333] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-13 08:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-13 23:50:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-13 08:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-13 13:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.367] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-13 08:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-13 13:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-13 10:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-13 21:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.34] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-13 10:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-13 21:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-13 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-14 21:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3275] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-13 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-14 21:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-13 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-14 21:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3275] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-13 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-14 21:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-14 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-15 17:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3255 | WIL5_20fix$WC_30cm > 0.332] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-14 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-15 17:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-15 19:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-16 07:30:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.355] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-15 19:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-16 07:30:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-15 19:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-16 01:30:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.365] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-15 19:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-16 01:30:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-16 01:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-16 04:40:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.37] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-16 01:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-16 04:40:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-16 14:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-17 05:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3291] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-16 14:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-17 05:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-17 04:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-18 05:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.326] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-17 04:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-18 05:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-18 04:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-19 05:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3241] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-18 04:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-19 05:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-18 04:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-19 01:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.32475] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-18 04:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-19 01:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-20 09:00:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-20 23:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3212] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-20 09:00:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-20 23:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-20 23:40:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-21 02:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.35] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-20 23:40:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-21 02:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-21 06:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-22 02:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3275 | WIL5_20fix$WC_30cm > 0.343] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-21 06:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-22 02:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-21 07:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-22 07:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm > 0.34] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-21 07:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-22 07:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-22 06:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-23 05:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3242] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-22 06:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-23 05:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-23 18:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-24 05:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.326] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-23 18:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-24 05:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-23 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-24 05:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3288] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-23 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-24 05:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-24 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-25 08:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm > 0.332 | WIL5_20fix$WC_30cm < 0.3285 ] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-24 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-25 08:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-25 09:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-25 13:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.365 ] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-25 09:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-25 13:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-25 11:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-25 21:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.365] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-25 11:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-25 21:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-26 10:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-27 18:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.328] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-26 10:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-27 18:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-27 08:40:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-27 18:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3299] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-27 08:40:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-27 18:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-27 08:40:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-27 18:00:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3299] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-27 08:40:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-27 18:00:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-27 17:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-28 01:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.355] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-27 17:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-28 01:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-28 00:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-30 01:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.326] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-28 00:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-30 01:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-29 00:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-31 00:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3242] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-29 00:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-31 00:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-30 00:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-01 00:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.322] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-30 00:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-01 00:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-01 00:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-02 00:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3215] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-01 00:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-02 00:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-01 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-03 00:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3205] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-01 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-03 00:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-02 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-05 00:30:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3185] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-02 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-05 00:30:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-02 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-04 20:30:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.319] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-02 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-04 20:30:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-04 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-06 20:30:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3179] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-04 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-06 20:30:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-06 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-08 10:30:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3165] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-06 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-08 10:30:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-08 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-11 10:30:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3141] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-08 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-11 10:30:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-01-19 10:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-01-20 22:30:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3215] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-01-19 10:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-01-20 22:30:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-10 10:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-12 04:40:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.31360] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-10 10:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-12 04:40:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-05 10:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-09 04:40:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.316] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-05 10:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-09 04:40:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-08 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-11 02:40:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3147] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-08 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-11 02:40:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-02-08 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-02-09 14:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.3156] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-02-08 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-02-09 14:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)

#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-11-01 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-11-09 14:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.202] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-11-01 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-11-09 14:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)


#Try to fix drips in 30 cm 
#======================================================================
WIL5_20fix <- filter(WIL5_20, Date_time > "2020-11-12 20:50:01")
WIL5_20fix <- filter(WIL5_20fix, Date_time < "2020-11-14 14:10:01")

WIL5_20fix$WC_30cm[WIL5_20fix$WC_30cm < 0.207] <- NA
missing <- which(is.na(WIL5_20fix$WC_30cm))

if(1 %in% missing){
  WIL5_20fix$WC_30cm[1] <- head(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}
if(nrow(WIL5_20fix) %in% missing){
  WIL5_20fix$WC_30cm[nrow(data)] <- tail(WIL5_20fix$WC_30cm[!is.na(WIL5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20fix$WC_30cm[idx] <- (WIL5_20fix$WC_30cm[r$starts[i]] + WIL5_20fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_20_later <- filter(WIL5_20, Date_time < "2020-11-12 20:50:01")
WIL5_20_end <- filter(WIL5_20, Date_time > "2020-11-14 14:10:01")
WIL5_20<- bind_rows(WIL5_20fix, WIL5_20_later, WIL5_20_end)


#100 cm 
################################################################
WIL5_20$WC_100cm[WIL5_20$WC_100cm < 0] <- NA
missing <- which(is.na(WIL5_20$WC_100cm))

if(1 %in% missing){
  WIL5_20$WC_100cm[1] <- head(WIL5_20$WC_100cm[!is.na(WIL5_20$WC_100cm)],1)
}
if(nrow(WIL5_20) %in% missing){
  WIL5_20$WC_100cm[nrow(data)] <- tail(WIL5_20$WC_100cm[!is.na(WIL5_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_20$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_20$WC_100cm[idx] <- (WIL5_20$WC_100cm[r$starts[i]] + WIL5_20$WC_100cm[r$ends[i]])/2
}

#Plot again
Soil <- ggplot(data = subset(WIL5_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL5 2021
##################################################################################################
WIL5_21 <- subset(WIL5, Year == '2021')

#Plotting 
WIL5_21$WC_15cm <- as.numeric(WIL5_21$WC_15cm)
WIL5_21$WC_30cm <- as.numeric(WIL5_21$WC_30cm)
WIL5_21$WC_100cm <- as.numeric(WIL5_21$WC_100cm)
WIL5_21$Date_time<- mdy_hms(WIL5_21$Date_time)

#15 cm
###############################################################################
WIL5_21$WC_15cm[WIL5_21$WC_15cm < 0.22] <- NA
missing <- which(is.na(WIL5_21$WC_15cm))

if(1 %in% missing){
  WIL5_21$WC_15cm[1] <- head(WIL5_21$WC_15cm[!is.na(WIL5_21$WC_15cm)],1)
}
if(nrow(WIL5_21) %in% missing){
  WIL5_21$WC_15cm[nrow(data)] <- tail(WIL5_21$WC_15cm[!is.na(WIL5_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21$WC_15cm[idx] <- (WIL5_21$WC_15cm[r$starts[i]] + WIL5_21$WC_15cm[r$ends[i]])/2
}

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-06 20:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-11 05:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.235] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-06 20:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-11 05:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-10 20:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-15 01:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.2314] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-10 20:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-15 01:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-14 20:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-25 01:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.2250] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-14 20:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-25 01:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-13 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-16 04:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.231] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-13 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-16 04:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-10 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-21 04:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.2275] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-10 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-21 04:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-18 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-21 04:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.22762] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-18 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-21 04:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-18 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-20 14:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.228] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-18 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-20 14:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-18 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-23 04:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.2267] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-18 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-23 04:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-21 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-29 04:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.222] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-21 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-29 04:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-24 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-28 01:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.2228] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-24 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-28 01:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-24 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-27 13:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.223] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-24 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-27 13:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-23 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-26 13:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.22425] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-23 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-26 13:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-26 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-30 13:10:01")

WIL5_21fix$WC_15cm[WIL5_21fix$WC_15cm < 0.2215] <- NA
missing <- which(is.na(WIL5_21fix$WC_15cm))

if(1 %in% missing){
  WIL5_21fix$WC_15cm[1] <- head(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_15cm[nrow(data)] <- tail(WIL5_21fix$WC_15cm[!is.na(WIL5_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_15cm[idx] <- (WIL5_21fix$WC_15cm[r$starts[i]] + WIL5_21fix$WC_15cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-26 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-30 13:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Remove 15 cm glitch
#========================================================
WIL5_21$WC_15cm[WIL5_21$WC_15cm == 0.3361] <- NA

#30 cm 
################################################################
WIL5_21$WC_30cm[WIL5_21$WC_30cm < 0.19] <- NA
missing <- which(is.na(WIL5_21$WC_30cm))

if(1 %in% missing){
  WIL5_21$WC_30cm[1] <- head(WIL5_21$WC_30cm[!is.na(WIL5_21$WC_30cm)],1)
}
if(nrow(WIL5_21) %in% missing){
  WIL5_21$WC_30cm[nrow(data)] <- tail(WIL5_21$WC_30cm[!is.na(WIL5_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21$WC_30cm[idx] <- (WIL5_21$WC_30cm[r$starts[i]] + WIL5_21$WC_30cm[r$ends[i]])/2
}

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-13 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-16 13:10:01")

WIL5_21fix$WC_30cm[WIL5_21fix$WC_30cm < 0.243] <- NA
missing <- which(is.na(WIL5_21fix$WC_30cm))

if(1 %in% missing){
  WIL5_21fix$WC_30cm[1] <- head(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_30cm[nrow(data)] <- tail(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_30cm[idx] <- (WIL5_21fix$WC_30cm[r$starts[i]] + WIL5_21fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-13 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-16 13:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-13 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-14 13:10:01")

WIL5_21fix$WC_30cm[WIL5_21fix$WC_30cm < 0.244] <- NA
missing <- which(is.na(WIL5_21fix$WC_30cm))

if(1 %in% missing){
  WIL5_21fix$WC_30cm[1] <- head(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_30cm[nrow(data)] <- tail(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_30cm[idx] <- (WIL5_21fix$WC_30cm[r$starts[i]] + WIL5_21fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-13 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-14 13:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-14 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-19 08:10:01")

WIL5_21fix$WC_30cm[WIL5_21fix$WC_30cm < 0.24] <- NA
missing <- which(is.na(WIL5_21fix$WC_30cm))

if(1 %in% missing){
  WIL5_21fix$WC_30cm[1] <- head(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_30cm[nrow(data)] <- tail(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_30cm[idx] <- (WIL5_21fix$WC_30cm[r$starts[i]] + WIL5_21fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-14 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-19 08:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-18 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-23 08:10:01")

WIL5_21fix$WC_30cm[WIL5_21fix$WC_30cm < 0.2375] <- NA
missing <- which(is.na(WIL5_21fix$WC_30cm))

if(1 %in% missing){
  WIL5_21fix$WC_30cm[1] <- head(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_30cm[nrow(data)] <- tail(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_30cm[idx] <- (WIL5_21fix$WC_30cm[r$starts[i]] + WIL5_21fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-18 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-23 08:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-22 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-07-30 08:10:01")

WIL5_21fix$WC_30cm[WIL5_21fix$WC_30cm < 0.232] <- NA
missing <- which(is.na(WIL5_21fix$WC_30cm))

if(1 %in% missing){
  WIL5_21fix$WC_30cm[1] <- head(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_30cm[nrow(data)] <- tail(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_30cm[idx] <- (WIL5_21fix$WC_30cm[r$starts[i]] + WIL5_21fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-22 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-07-30 08:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-30 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-08-01 08:10:01")

WIL5_21fix$WC_30cm[WIL5_21fix$WC_30cm < 0.231] <- NA
missing <- which(is.na(WIL5_21fix$WC_30cm))

if(1 %in% missing){
  WIL5_21fix$WC_30cm[1] <- head(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_30cm[nrow(data)] <- tail(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_30cm[idx] <- (WIL5_21fix$WC_30cm[r$starts[i]] + WIL5_21fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-30 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-08-01 08:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-07-30 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-08-03 08:10:01")

WIL5_21fix$WC_30cm[WIL5_21fix$WC_30cm < 0.2303] <- NA
missing <- which(is.na(WIL5_21fix$WC_30cm))

if(1 %in% missing){
  WIL5_21fix$WC_30cm[1] <- head(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_30cm[nrow(data)] <- tail(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_30cm[idx] <- (WIL5_21fix$WC_30cm[r$starts[i]] + WIL5_21fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-30 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-08-03 08:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-08-01 18:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-08-06 08:10:01")

WIL5_21fix$WC_30cm[WIL5_21fix$WC_30cm < 0.2294] <- NA
missing <- which(is.na(WIL5_21fix$WC_30cm))

if(1 %in% missing){
  WIL5_21fix$WC_30cm[1] <- head(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}
if(nrow(WIL5_21fix) %in% missing){
  WIL5_21fix$WC_30cm[nrow(data)] <- tail(WIL5_21fix$WC_30cm[!is.na(WIL5_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21fix$WC_30cm[idx] <- (WIL5_21fix$WC_30cm[r$starts[i]] + WIL5_21fix$WC_30cm[r$ends[i]])/2
}

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-08-01 18:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-08-06 08:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#Fix drips 
#===============================================================================
WIL5_21fix <- filter(WIL5_21, Date_time > "2021-06-11 13:50:01")
WIL5_21fix <- filter(WIL5_21fix, Date_time < "2021-06-20 08:10:01")

WIL5_21fix$WC_30cm[WIL5_21fix$WC_30cm > 0.275] <- NA

#Merge data frame again 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-06-11 13:50:01")
WIL5_21_end <- filter(WIL5_21, Date_time > "2021-06-20 08:10:01")
WIL5_21 <- bind_rows(WIL5_21fix, WIL5_21_later, WIL5_21_end)

#100 cm 
################################################################
WIL5_21$WC_100cm[WIL5_21$WC_100cm < 0] <- NA
missing <- which(is.na(WIL5_21$WC_100cm))

if(1 %in% missing){
  WIL5_21$WC_100cm[1] <- head(WIL5_21$WC_100cm[!is.na(WIL5_21$WC_100cm)],1)
}
if(nrow(WIL5_21) %in% missing){
  WIL5_21$WC_100cm[nrow(data)] <- tail(WIL5_21$WC_100cm[!is.na(WIL5_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL5_21$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL5_21$WC_100cm[idx] <- (WIL5_21$WC_100cm[r$starts[i]] + WIL5_21$WC_100cm[r$ends[i]])/2
}

#Remove 100 cm glitch
#========================================================
WIL5_21$WC_100cm[WIL5_21$WC_100cm == 0.7184] <- NA
WIL5_21$WC_100cm[WIL5_21$WC_100cm == 0.52075] <- NA
WIL5_21$WC_100cm[WIL5_21$WC_100cm > 0.337349 & WIL5_21$WC_100cm < 0.337351 ] <- NA

#Remove percent drops
WIL5_21_fix <- filter(WIL5_21, Date_time > "2021-07-01 09:00:01")

#Create a new column that is the percent difference between the row below and the row above 
WIL5_21_fix <- WIL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================


#Make increase column not a percent 
WIL5_21_fix <- transform(WIL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

WIL5_21_fix <- transform(WIL5_21_fix, WC_15cm=ifelse(incr < -0.015, 
                                                     as.numeric(stats::filter(WC_15cm, rep(1/25, 25), sides=2)), 
                                             WC_15cm))



#Recombine July with other dataset 
WIL5_21_later <- filter(WIL5_21, Date_time < "2021-07-01 09:00:01")

WIL5_21 <- bind_rows(WIL5_21_later, WIL5_21_fix)

#Plot again
Soil <- ggplot(data = subset(WIL5_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
WIL5_clean <- merge(WIL5_18, WIL5_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
WIL5_clean <- merge(WIL5_clean, WIL5_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
WIL5_clean <- merge(WIL5_clean, WIL5_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
WIL5_clean <- merge(WIL5_clean, WIL5_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

WIL5_clean <- select(WIL5_clean, Date_time, WC_15cm, WC_30cm, WC_100cm)
#Graph

Soil <- ggplot(data = subset(WIL5_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 1) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 1) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 1) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date")) 
Soil

png("WIL5_Salli", width = 4500, height = 2500)

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
write.csv(WIL5_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL5_clean.csv" ) #this writes a csv file and sends it to the working folder

