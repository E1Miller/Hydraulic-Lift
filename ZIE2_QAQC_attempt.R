#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/16/2023
#Description: QA/QC ZIE 2

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

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE", 
                        pattern=glob2rx("Copy of Z2M_*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
ZIE2_2019_2021 <- data19_21 %>%
  rename_all(funs(c("Date", "Time", "PAR", "WC_15cm", "WC_30cm","WC_100cm", "Date_time"))) %>% #this renames all of the columns
  select(Date_time, PAR, WC_15cm, WC_30cm, WC_100cm) #this keeps only the columns you will use


#2017-2019 FILES
#========================================================================================================================
#If wanting to merge the files, needed to manually delete the extra column in W1M190111 and 
#extra row at the top, and needed to manually delete the extra columns in W1M171228, W1M180201, and W1M180302

#Set the data path 
data_path <- "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE"
new_col_name <- c("Date_time", "PAR", "WC_15cm", "WC_30cm", "WC_100cm")

#Call in all the files in this folder with the W2M1 pattern, which excludes the datasheets from 2019-2020
files <- dir(data_path, pattern=glob2rx("Copy of Z2M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
ZIE2_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
ZIE2 <- rbind(ZIE2_2017_2019, ZIE2_2019_2021)

#Write the csv
write.csv(ZIE2,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE2.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(ZIE2)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
ZIE2$Date <- mdy_hms(ZIE2$Date_time)

#Put year into a separate column 
ZIE2 <- separate(ZIE2, Date, c("Year"))

#ZIE2 2017
##################################################################################################
ZIE2_17 <- subset(ZIE2, Year == '2017')

#Plotting 
ZIE2_17$WC_15cm <- as.numeric(ZIE2_17$WC_15cm)
ZIE2_17$WC_30cm <- as.numeric(ZIE2_17$WC_30cm)
ZIE2_17$WC_100cm <- as.numeric(ZIE2_17$WC_100cm)
ZIE2_17$Date_time<- mdy_hms(ZIE2_17$Date_time)

Soil <- ggplot(data = subset(ZIE2_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE2 2018
##################################################################################################
ZIE2_18 <- subset(ZIE2, Year == '2018')

#Plotting 
ZIE2_18$WC_15cm <- as.numeric(ZIE2_18$WC_15cm)
ZIE2_18$WC_30cm <- as.numeric(ZIE2_18$WC_30cm)
ZIE2_18$WC_100cm <- as.numeric(ZIE2_18$WC_100cm)
ZIE2_18$Date_time<- mdy_hms(ZIE2_18$Date_time)

#15 cm 
################################################################
#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-11-20 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-11-22 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.109] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_15cm[1] <- head(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_15cm[nrow(data)] <- tail(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_15cm[idx] <- (ZIE2_18_fix$WC_15cm[r$starts[i]] + ZIE2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-11-20 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-11-22 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Calibrate
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-07-12 01:10:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-12-31 23:50:01")

ZIE2_18_fix$WC_15cm <- ZIE2_18_fix$WC_15cm + 0.04

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-07-12 01:10:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-12-31 23:50:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Calibrate
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-08-21 6:10:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-12-31 23:50:01")

ZIE2_18_fix$WC_15cm <- ZIE2_18_fix$WC_15cm + 0.0034

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-08-21 6:10:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-12-31 23:50:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Calibrate
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-10-25 8:30:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-12-31 23:50:01")

ZIE2_18_fix$WC_15cm <- ZIE2_18_fix$WC_15cm + 0.005

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-10-25 8:30:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-12-31 23:50:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Remove glitch 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-10-27 22:50:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-10-31 23:50:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.19] <- NA

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-10-27 22:50:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-10-31 23:50:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Calibrate
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-10-29 22:50:01")

ZIE2_18_fix$WC_15cm <- ZIE2_18_fix$WC_15cm - 0.0082

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-10-29 22:50:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_fix)


#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-09-12 07:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-10-15 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.15] <- NA

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-09-12 07:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-10-15 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-10-12 07:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-10-26 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.181] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_15cm[1] <- head(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_15cm[nrow(data)] <- tail(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_15cm[idx] <- (ZIE2_18_fix$WC_15cm[r$starts[i]] + ZIE2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-10-12 07:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-10-26 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-07-12 07:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-07-26 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.185] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_15cm[1] <- head(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_15cm[nrow(data)] <- tail(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_15cm[idx] <- (ZIE2_18_fix$WC_15cm[r$starts[i]] + ZIE2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-07-12 07:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-07-26 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-11-19 07:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-11-22 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.163] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_15cm[1] <- head(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_15cm[nrow(data)] <- tail(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_15cm[idx] <- (ZIE2_18_fix$WC_15cm[r$starts[i]] + ZIE2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-11-19 07:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-11-22 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-12-09 07:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-12-12 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.308] <- NA

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-12-09 07:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-12-12 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-12-11 07:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-12-13 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.3175] <- NA

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-12-11 07:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-12-13 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-11-22 00:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-12-09 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.193] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_15cm[1] <- head(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_15cm[nrow(data)] <- tail(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_15cm[idx] <- (ZIE2_18_fix$WC_15cm[r$starts[i]] + ZIE2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-11-22 00:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-12-09 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-11-26 00:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-12-09 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.2832] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_15cm[1] <- head(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_15cm[nrow(data)] <- tail(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_15cm[idx] <- (ZIE2_18_fix$WC_15cm[r$starts[i]] + ZIE2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-11-26 00:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-12-09 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)


#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-12-11 00:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-12-15 1:00:01")

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.3065] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_15cm[1] <- head(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_15cm[nrow(data)] <- tail(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_15cm[idx] <- (ZIE2_18_fix$WC_15cm[r$starts[i]] + ZIE2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-12-11 00:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-12-15 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-11-01 00:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-11-09 1:00:01")

Soil <- ggplot(data = subset(ZIE2_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

ZIE2_18_fix$WC_15cm[ZIE2_18_fix$WC_15cm < 0.175] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_15cm[1] <- head(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_15cm[nrow(data)] <- tail(ZIE2_18_fix$WC_15cm[!is.na(ZIE2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_15cm[idx] <- (ZIE2_18_fix$WC_15cm[r$starts[i]] + ZIE2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-11-01 00:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-11-09 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#30 cm 
################################################################

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-09-11 00:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-10-15 1:00:01")

ZIE2_18_fix$WC_30cm[ZIE2_18_fix$WC_30cm < 0.177] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_30cm[1] <- head(ZIE2_18_fix$WC_30cm[!is.na(ZIE2_18_fix$WC_30cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_30cm[nrow(data)] <- tail(ZIE2_18_fix$WC_30cm[!is.na(ZIE2_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_30cm[idx] <- (ZIE2_18_fix$WC_30cm[r$starts[i]] + ZIE2_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-09-11 00:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-10-15 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-10-20 00:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-11-01 1:00:01")

ZIE2_18_fix$WC_30cm[ZIE2_18_fix$WC_30cm < 0.1825] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_30cm[1] <- head(ZIE2_18_fix$WC_30cm[!is.na(ZIE2_18_fix$WC_30cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_30cm[nrow(data)] <- tail(ZIE2_18_fix$WC_30cm[!is.na(ZIE2_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_30cm[idx] <- (ZIE2_18_fix$WC_30cm[r$starts[i]] + ZIE2_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-10-20 00:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-11-01 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-10-27 00:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-10-30 1:00:01")

ZIE2_18_fix$WC_30cm[ZIE2_18_fix$WC_30cm < 0.1835] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_30cm[1] <- head(ZIE2_18_fix$WC_30cm[!is.na(ZIE2_18_fix$WC_30cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_30cm[nrow(data)] <- tail(ZIE2_18_fix$WC_30cm[!is.na(ZIE2_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_30cm[idx] <- (ZIE2_18_fix$WC_30cm[r$starts[i]] + ZIE2_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-10-27 00:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-10-30 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Calibrate 
#===============================================================================
#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-08-21 06:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-09-30 1:00:01")

ZIE2_18_fix$WC_30cm <- ZIE2_18_fix$WC_30cm - 0.0052

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-08-21 06:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-09-30 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#100 cm 
################################################################
ZIE2_18$WC_100cm[ZIE2_18$WC_100cm < 0.26] <- NA
missing <- which(is.na(ZIE2_18$WC_100cm))

if(1 %in% missing){
  ZIE2_18$WC_100cm[1] <- head(ZIE2_18$WC_100cm[!is.na(ZIE2_18$WC_100cm)],1)
}
if(nrow(ZIE2_18) %in% missing){
  ZIE2_18$WC_100cm[nrow(data)] <- tail(ZIE2_18$WC_100cm[!is.na(ZIE2_18$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18$WC_100cm[idx] <- (ZIE2_18$WC_100cm[r$starts[i]] + ZIE2_18$WC_100cm[r$ends[i]])/2
}

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-07-10 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-08-15 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.275] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_100cm[1] <- head(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_100cm[nrow(data)] <- tail(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_100cm[idx] <- (ZIE2_18_fix$WC_100cm[r$starts[i]] + ZIE2_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-07-10 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-08-15 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-07-05 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-07-30 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.279] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_100cm[1] <- head(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_100cm[nrow(data)] <- tail(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_100cm[idx] <- (ZIE2_18_fix$WC_100cm[r$starts[i]] + ZIE2_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-07-19 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-07-30 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-08-01 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-09-05 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.269] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_100cm[1] <- head(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_100cm[nrow(data)] <- tail(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_100cm[idx] <- (ZIE2_18_fix$WC_100cm[r$starts[i]] + ZIE2_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-08-01 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-09-05 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-07-17 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-07-23 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.2835] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_100cm[1] <- head(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_100cm[nrow(data)] <- tail(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_100cm[idx] <- (ZIE2_18_fix$WC_100cm[r$starts[i]] + ZIE2_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-07-17 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-07-23 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-07-23 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-07-26 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.28225] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_100cm[1] <- head(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_100cm[nrow(data)] <- tail(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_100cm[idx] <- (ZIE2_18_fix$WC_100cm[r$starts[i]] + ZIE2_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-07-23 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-07-26 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-07-26 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-07-28 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.282] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_100cm[1] <- head(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_100cm[nrow(data)] <- tail(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_100cm[idx] <- (ZIE2_18_fix$WC_100cm[r$starts[i]] + ZIE2_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-07-26 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-07-28 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-07-28 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-08-02 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.28] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_100cm[1] <- head(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_100cm[nrow(data)] <- tail(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_100cm[idx] <- (ZIE2_18_fix$WC_100cm[r$starts[i]] + ZIE2_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-07-28 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-08-02 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-08-02 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-08-06 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.279] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_100cm[1] <- head(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_100cm[nrow(data)] <- tail(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_100cm[idx] <- (ZIE2_18_fix$WC_100cm[r$starts[i]] + ZIE2_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-08-02 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-08-06 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-08-06 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-08-26 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.275] <- NA
missing <- which(is.na(ZIE2_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE2_18_fix$WC_100cm[1] <- head(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}
if(nrow(ZIE2_18_fix) %in% missing){
  ZIE2_18_fix$WC_100cm[nrow(data)] <- tail(ZIE2_18_fix$WC_100cm[!is.na(ZIE2_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_18_fix$WC_100cm[idx] <- (ZIE2_18_fix$WC_100cm[r$starts[i]] + ZIE2_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-08-06 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-08-26 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-03-05 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-03-09 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.3675] <- NA

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-03-05 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-03-09 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)


#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-09-06 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-10-30 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.2665] <- NA

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-09-06 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-10-30 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Subset and remove drips 
#========================================================================
ZIE2_18_fix <- filter(ZIE2_18, Date_time > "2018-08-26 1:00:01")
ZIE2_18_fix <- filter(ZIE2_18_fix, Date_time < "2018-09-03 1:00:01")

ZIE2_18_fix$WC_100cm[ZIE2_18_fix$WC_100cm < 0.273] <- NA

#Recombine 
ZIE2_18_early <- filter(ZIE2_18, Date_time < "2018-08-26 1:00:01")
ZIE2_18_late <- filter(ZIE2_18, Date_time > "2018-09-03 1:00:01")
ZIE2_18 <- bind_rows(ZIE2_18_early, ZIE2_18_late, ZIE2_18_fix)

#Replace missing dates
#================================================================================================
#Replace missing dates with NAs - 07/09 to 07/12
insertDF <- as.data.frame(matrix(data = NA, nrow = 4, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2018-07-09"), as.Date("2018-07-12"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE2_18<- insertRows(ZIE2_18, c(27881:27884), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(ZIE2_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE2 2019
##################################################################################################
ZIE2_19 <- subset(ZIE2, Year == '2019')

#Plotting 
ZIE2_19$WC_15cm <- as.numeric(ZIE2_19$WC_15cm)
ZIE2_19$WC_30cm <- as.numeric(ZIE2_19$WC_30cm)
ZIE2_19$WC_100cm <- as.numeric(ZIE2_19$WC_100cm)
ZIE2_19$Date_time<- mdy_hms(ZIE2_19$Date_time)

#30 cm 
################################################################
ZIE2_19$WC_30cm[ZIE2_19$WC_30cm < 0.15] <- NA
missing <- which(is.na(ZIE2_19$WC_30cm))

if(1 %in% missing){
  ZIE2_19$WC_30cm[1] <- head(ZIE2_19$WC_30cm[!is.na(ZIE2_19$WC_30cm)],1)
}
if(nrow(ZIE2_19) %in% missing){
  ZIE2_19$WC_30cm[nrow(data)] <- tail(ZIE2_19$WC_30cm[!is.na(ZIE2_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19$WC_30cm[idx] <- (ZIE2_19$WC_30cm[r$starts[i]] + ZIE2_19$WC_30cm[r$ends[i]])/2
}

#Subset and fix drips in early year
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-03-11 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-04-02 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.298] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-04-02 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_fix)

#Subset and fix drips in early year
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-04-05 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-04-07 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.31] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-04-05 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-04-07 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in early year
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-04-24 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-05-06 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.278] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-04-24 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-05-06 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in early year
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-05-08 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-05-20 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.27] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-05-08 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-05-20 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in early year
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-05-20 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-05-30 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-05-20 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-05-30 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in early year
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-06-01 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-06-10 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.281] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-06-01 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-06-10 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in early year
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-07-09 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-07-15 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.22] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-07-09 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-07-15 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in early year
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-10-29 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-11-25 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.165] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-10-29 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-11-25 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in early year
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-05-15 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-06-05 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.29] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-05-15 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-06-05 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-07-15 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-08-01 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.21] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-07-15 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-08-01 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-10-28 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-11-03 1:00:01")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.17] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-10-28 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-11-03 1:00:01")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-04-01 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-04-03 1:00:0")

Soil <- ggplot(data = subset(ZIE2_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.305] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))


if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-04-01 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-04-03 1:00:0")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-04-10 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-04-13 1:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.306] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-04-10 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-04-13 1:00:0")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-04-27 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-04-30 1:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.28905] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-04-27 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-04-30 1:00:0")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-05-06 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-05-13 1:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.272] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-05-06 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-05-13 1:00:0")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-05-23 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-05-25 1:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.3079] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-05-23 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-05-25 1:00:0")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-04-23 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-04-29 1:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.288] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-04-23 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-04-29 1:00:0")

ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-05-06 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-05-13 1:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.275] <- NA

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-05-06 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-05-13 1:00:0")
ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-05-15 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-05-19 1:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.315] <- NA

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-05-15 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-05-19 1:00:0")
ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-06-01 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-06-03 21:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.293] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-06-01 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-06-03 21:00:0")
ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-06-23 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-06-27 21:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.247] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-06-23 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-06-27 21:00:0")
ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-09-01 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-09-18 01:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.182] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-09-01 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-09-18 01:00:0")
ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-09-26 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-09-30 01:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.192] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-09-26 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-09-30 01:00:0")
ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-10-17 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-10-30 01:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm > 0.181] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-10-17 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-10-30 01:00:0")
ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-10-31 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-11-05 01:00:0")

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.175] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-10-31 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-11-05 01:00:0")
ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Subset and fix drips in mid-July/August
#==============================================================================
ZIE2_19_fix <- filter(ZIE2_19, Date_time > "2019-11-05 10:00:01")
ZIE2_19_fix <- filter(ZIE2_19_fix, Date_time < "2019-11-18 01:00:0")

Soil <- ggplot(data = subset(ZIE2_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE2_19_fix$WC_30cm[ZIE2_19_fix$WC_30cm < 0.170] <- NA
missing <- which(is.na(ZIE2_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_19_fix$WC_30cm[1] <- head(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}
if(nrow(ZIE2_19_fix) %in% missing){
  ZIE2_19_fix$WC_30cm[nrow(data)] <- tail(ZIE2_19_fix$WC_30cm[!is.na(ZIE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_19_fix$WC_30cm[idx] <- (ZIE2_19_fix$WC_30cm[r$starts[i]] + ZIE2_19_fix$WC_30cm[r$ends[i]])/2
}

ZIE2_19_early <- filter(ZIE2_19, Date_time < "2019-11-05 10:00:01")
ZIE2_19_late <- filter(ZIE2_19, Date_time > "2019-11-18 01:00:0")
ZIE2_19 <- bind_rows(ZIE2_19_late, ZIE2_19_early, ZIE2_19_fix)

#Add in missing dates for beginning of year
##################################################################################

#Missing dates from 01/01 to 03/11
insertDF <- as.data.frame(matrix(data = NA, nrow = 69, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-01-01"), as.Date("2019-03-10"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE2_19<- insertRows(ZIE2_19, c(1:69), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(ZIE2_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE2 2020
##################################################################################################
ZIE2_20 <- subset(ZIE2, Year == '2020')

#Plotting 
ZIE2_20$WC_15cm <- as.numeric(ZIE2_20$WC_15cm)
ZIE2_20$WC_30cm <- as.numeric(ZIE2_20$WC_30cm)
ZIE2_20$WC_100cm <- as.numeric(ZIE2_20$WC_100cm)
ZIE2_20$Date_time<- mdy_hms(ZIE2_20$Date_time)

#15 cm 
###############################################################################

#Subset drips
#==============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-02-28 04:00:01")

ZIE2_20_fix$WC_15cm <- ZIE2_20_fix$WC_15cm + 0.01

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-02-28 04:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix)

#Subset drips
#==============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-02-26 04:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-03-03 23:50:01")

ZIE2_20_fix$WC_15cm[ZIE2_20_fix$WC_15cm > 0.2223 | ZIE2_20_fix$WC_15cm < 0.210] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_15cm[1] <- head(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_15cm[nrow(data)] <- tail(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_15cm[idx] <- (ZIE2_20_fix$WC_15cm[r$starts[i]] + ZIE2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-02-26 04:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-03-03 23:50:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset drips
#==============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-02-28 04:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-03-03 00:50:01")

ZIE2_20_fix$WC_15cm[ZIE2_20_fix$WC_15cm > 0.22 | ZIE2_20_fix$WC_15cm < 0.212] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_15cm[1] <- head(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_15cm[nrow(data)] <- tail(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_15cm[idx] <- (ZIE2_20_fix$WC_15cm[r$starts[i]] + ZIE2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-02-28 04:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-03-03 00:50:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset drips
#==============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-02-29 04:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-03-02 00:50:01")

ZIE2_20_fix$WC_15cm[ZIE2_20_fix$WC_15cm > 0.218 | ZIE2_20_fix$WC_15cm < 0.213] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_15cm[1] <- head(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_15cm[nrow(data)] <- tail(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_15cm[idx] <- (ZIE2_20_fix$WC_15cm[r$starts[i]] + ZIE2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-02-29 04:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-03-02 00:50:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset drips
#==============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-03-02 04:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-03-05 00:50:01")

ZIE2_20_fix$WC_15cm[ZIE2_20_fix$WC_15cm > 0.215 | ZIE2_20_fix$WC_15cm < 0.209] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_15cm[1] <- head(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_15cm[nrow(data)] <- tail(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_15cm[idx] <- (ZIE2_20_fix$WC_15cm[r$starts[i]] + ZIE2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-03-02 04:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-03-05 00:50:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset drips
#==============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-03-04 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-03-06 00:50:01")

ZIE2_20_fix$WC_15cm[ZIE2_20_fix$WC_15cm > 0.2127 | ZIE2_20_fix$WC_15cm < 0.206] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_15cm[1] <- head(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_15cm[nrow(data)] <- tail(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_15cm[idx] <- (ZIE2_20_fix$WC_15cm[r$starts[i]] + ZIE2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-03-04 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-03-06 00:50:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset drips
#==============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-03-05 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-03-08 00:50:01")

ZIE2_20_fix$WC_15cm[ZIE2_20_fix$WC_15cm > 0.211 | ZIE2_20_fix$WC_15cm < 0.206] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_15cm[1] <- head(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_15cm[nrow(data)] <- tail(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_15cm[idx] <- (ZIE2_20_fix$WC_15cm[r$starts[i]] + ZIE2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-03-05 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-03-08 00:50:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset drips
#==============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-03-07 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-03-12 00:50:01")

ZIE2_20_fix$WC_15cm[ZIE2_20_fix$WC_15cm > 0.2125 | ZIE2_20_fix$WC_15cm < 0.20] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_15cm[1] <- head(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_15cm[nrow(data)] <- tail(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_15cm[idx] <- (ZIE2_20_fix$WC_15cm[r$starts[i]] + ZIE2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-03-07 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-03-12 00:50:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset drips
#==============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-09-21 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-10-04 00:50:01")

Soil <- ggplot(data = subset(ZIE2_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

ZIE2_20_fix$WC_15cm[ZIE2_20_fix$WC_15cm > 0.120] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_15cm[1] <- head(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_15cm[nrow(data)] <- tail(ZIE2_20_fix$WC_15cm[!is.na(ZIE2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_15cm[idx] <- (ZIE2_20_fix$WC_15cm[r$starts[i]] + ZIE2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-09-21 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-10-04 00:50:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#30 cm 
################################################################
ZIE2_20$WC_30cm[ZIE2_20$WC_30cm < 0.15] <- NA
missing <- which(is.na(ZIE2_20$WC_30cm))

if(1 %in% missing){
  ZIE2_20$WC_30cm[1] <- head(ZIE2_20$WC_30cm[!is.na(ZIE2_20$WC_30cm)],1)
}
if(nrow(ZIE2_20) %in% missing){
  ZIE2_20$WC_30cm[nrow(data)] <- tail(ZIE2_20$WC_30cm[!is.na(ZIE2_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20$WC_30cm[idx] <- (ZIE2_20$WC_30cm[r$starts[i]] + ZIE2_20$WC_30cm[r$ends[i]])/2
}

#Remove the glitch 
#======================================================
ZIE2_20$WC_30cm[ZIE2_20$WC_30cm > 0.5] <- NA

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-10 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-30 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.20] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-10 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-30 1:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-08-01 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-08-20 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.185] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-08-01 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-08-20 1:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-15 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.22] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-15 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-15 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-07 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.235] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-15 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-07 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-12-12 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-25 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.253] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-12-12 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-25 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-11 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-13 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.217] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-11 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-13 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-13 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-15 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.215] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-13 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-15 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-19 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-21 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.2098] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-19 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-21 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-21 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-23 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.2085] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-21 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-23 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-25 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-27 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.2037] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-25 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-27 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-29 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-08-04 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1975] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-29 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-08-04 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-08-06 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-08-11 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1925] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-08-06 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-08-11 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-08-11 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-08-13 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.19] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-08-11 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-08-13 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-08-19 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-08-23 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1828] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-08-19 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-08-23 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-08-30 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-09-05 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1765] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-08-30 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-09-05 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_fix, ZIE2_20_late)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-09-05 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-09-08 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1723] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-09-05 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-09-08 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-09-08 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-09-20 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1659] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-09-08 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-09-20 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-09-08 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-09-12 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1705] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-09-08 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-09-12 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-09-20 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-09-22 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1645] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-09-20 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-09-22 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-09-24 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-09-27 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.164] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-09-24 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-09-27 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-09-27 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-10-13 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.161] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-09-27 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-10-13 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-10-14 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-10-16 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1582] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-10-14 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-10-16 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-10-16 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-10-20 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1567] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-10-16 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-10-20 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-10-20 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-10-22 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1562] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-10-20 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-10-22 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-01 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-05 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.15] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-01 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-05 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-07 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-13 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.158] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-07 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-13 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-14 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-16 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.22] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-14 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-16 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-16 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-18 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.265] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-16 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-18 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-17 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-19 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.28] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-17 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-19 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-19 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-20 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.275] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-19 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-20 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-20 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-22 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.265] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-20 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-22 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-22 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-28 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.258] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-22 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-28 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-25 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-26 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.2626] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-25 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-26 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-26 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-30 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.255] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-26 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-30 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-30 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-04 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.2515] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-30 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-04 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-12-04 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-04 18:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.249] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-12-04 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-04 18:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-12-04 20:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-06 10:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.24689] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-12-04 20:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-06 10:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-12-07 08:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-08 06:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.2425] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-12-07 08:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-08 06:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-12-21 08:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-26 06:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.27] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-12-21 08:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-26 06:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-12-25 08:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-26 06:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.29] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-12-25 08:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-26 06:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-12-26 08:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-30 00:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.275] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-12-26 08:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-30 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-12-30 00:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-31 00:00:01")

Soil <- ggplot(data = subset(ZIE2_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-12-30 00:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-31 00:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-08-01 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-08-07 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.195] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-08-01 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-08-07 1:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-08-12 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-08-15 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.189] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-08-12 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-08-15 1:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-15 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-17 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.269] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-15 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-17 1:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-18 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-20 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.276] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-18 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-20 1:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-11-21 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-11-26 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.262] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-11-21 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-11-26 1:00:01")

ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Subset and clean up more dips
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-12-03 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-12-10 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.24] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-12-03 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-12-10 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-02-03 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-03-10 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm > 0.29] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-02-03 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-03-10 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-10 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-12 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.219] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-10 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-12 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-17 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-20 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.211] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-17 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-20 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-19 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-23 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.2085] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-19 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-23 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-23 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-07-26 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.2045] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-23 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-07-26 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-26 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-08-01 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.1992] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-26 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-08-01 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-07-30 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-08-03 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.198] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-07-30 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-08-03 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-01-30 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-02-14 20:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm > 0.2895] <- NA

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-01-30 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-02-14 20:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-01-01 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-02-03 1:00:01")

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-01-01 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-02-03 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)

#Fix glitch before missing time 
#============================================================================
ZIE2_20_fix <- filter(ZIE2_20, Date_time > "2020-09-15 10:00:01")
ZIE2_20_fix <- filter(ZIE2_20_fix, Date_time < "2020-10-03 1:00:01")

Soil <- ggplot(data = subset(ZIE2_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE2_20_fix$WC_30cm[ZIE2_20_fix$WC_30cm > 0.18] <- NA
missing <- which(is.na(ZIE2_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_20_fix$WC_30cm[1] <- head(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}
if(nrow(ZIE2_20_fix) %in% missing){
  ZIE2_20_fix$WC_30cm[nrow(data)] <- tail(ZIE2_20_fix$WC_30cm[!is.na(ZIE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_20_fix$WC_30cm[idx] <- (ZIE2_20_fix$WC_30cm[r$starts[i]] + ZIE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_20_early <- filter(ZIE2_20, Date_time < "2020-09-15 10:00:01")
ZIE2_20_late <- filter(ZIE2_20, Date_time > "2020-10-03 1:00:01")
ZIE2_20 <- bind_rows(ZIE2_20_early, ZIE2_20_late, ZIE2_20_fix)


#100 cm 
################################################################
ZIE2_20$WC_100cm[ZIE2_20$WC_100cm < 0.15] <- NA

#Plot again 
Soil <- ggplot(data = subset(ZIE2_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE2 2021
##################################################################################################
ZIE2_21 <- subset(ZIE2, Year == '2021')

#Plotting 
ZIE2_21$WC_15cm <- as.numeric(ZIE2_21$WC_15cm)
ZIE2_21$WC_30cm <- as.numeric(ZIE2_21$WC_30cm)
ZIE2_21$WC_100cm <- as.numeric(ZIE2_21$WC_100cm)
ZIE2_21$Date_time<- mdy_hms(ZIE2_21$Date_time)

#15 cm
###########################################
ZIE2_21$WC_15cm[ZIE2_21$WC_15cm < 0] <- NA
missing <- which(is.na(ZIE2_21$WC_15cm))

if(1 %in% missing){
  ZIE2_21$WC_15cm[1] <- head(ZIE2_21$WC_15cm[!is.na(ZIE2_21$WC_15cm)],1)
}
if(nrow(ZIE2_21) %in% missing){
  ZIE2_21$WC_15cm[nrow(data)] <- tail(ZIE2_21$WC_15cm[!is.na(ZIE2_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21$WC_15cm[idx] <- (ZIE2_21$WC_15cm[r$starts[i]] + ZIE2_21$WC_15cm[r$ends[i]])/2
}

#30 cm 
################################################################
ZIE2_21$WC_30cm[ZIE2_21$WC_30cm < 0.25] <- NA
missing <- which(is.na(ZIE2_21$WC_30cm))

if(1 %in% missing){
  ZIE2_21$WC_30cm[1] <- head(ZIE2_21$WC_30cm[!is.na(ZIE2_21$WC_30cm)],1)
}
if(nrow(ZIE2_21) %in% missing){
  ZIE2_21$WC_30cm[nrow(data)] <- tail(ZIE2_21$WC_30cm[!is.na(ZIE2_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21$WC_30cm[idx] <- (ZIE2_21$WC_30cm[r$starts[i]] + ZIE2_21$WC_30cm[r$ends[i]])/2
}

#Subset and remove drips in mid-April/May
#=================================================================
#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-04-24 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-04-30 00:00:01")

ZIE2_21_fix$WC_15cm[ZIE2_21_fix$WC_15cm < 0.207] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_15cm[1] <- head(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_15cm[nrow(data)] <- tail(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_15cm[idx] <- (ZIE2_21_fix$WC_15cm[r$starts[i]] + ZIE2_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time <"2021-04-24 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-04-30 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Calibrate
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-02-05 05:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-02-10 04:00:01")

ZIE2_21_fix$WC_15cm  <- ZIE2_21_fix$WC_15cm + 0.0059

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time <"2021-02-05 05:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-02-10 04:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-02-04 22:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-02-11 00:00:01")

ZIE2_21_fix$WC_15cm[ZIE2_21_fix$WC_15cm < 0.2575 | ZIE2_21_fix$WC_15cm > 0.275] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_15cm[1] <- head(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_15cm[nrow(data)] <- tail(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_15cm[idx] <- (ZIE2_21_fix$WC_15cm[r$starts[i]] + ZIE2_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-02-04 22:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-02-11 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-04-23 22:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-05-01 00:00:01")

ZIE2_21_fix$WC_15cm[ZIE2_21_fix$WC_15cm < 0.212] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_15cm[1] <- head(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_15cm[nrow(data)] <- tail(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_15cm[idx] <- (ZIE2_21_fix$WC_15cm[r$starts[i]] + ZIE2_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-04-23 22:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-05-01 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-05-13 22:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-05-26 00:00:01")

ZIE2_21_fix$WC_15cm[ZIE2_21_fix$WC_15cm < 0.1892] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_15cm[1] <- head(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_15cm[nrow(data)] <- tail(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_15cm[idx] <- (ZIE2_21_fix$WC_15cm[r$starts[i]] + ZIE2_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-05-13 22:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-05-26 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-06-22 05:40:01")

ZIE2_21_fix$WC_15cm <- ZIE2_21_fix$WC_15cm - 0.0197

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-06-22 05:40:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-06-21 22:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-06-25 00:00:01")

ZIE2_21_fix$WC_15cm[ZIE2_21_fix$WC_15cm < 0.155 | ZIE2_21_fix$WC_15cm > 0.1595] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_15cm[1] <- head(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_15cm[nrow(data)] <- tail(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_15cm[idx] <- (ZIE2_21_fix$WC_15cm[r$starts[i]] + ZIE2_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-06-21 22:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-06-25 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-19 22:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-25 00:00:01")

ZIE2_21_fix$WC_15cm[ZIE2_21_fix$WC_15cm < 0.143] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_15cm[1] <- head(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_15cm[nrow(data)] <- tail(ZIE2_21_fix$WC_15cm[!is.na(ZIE2_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_15cm[idx] <- (ZIE2_21_fix$WC_15cm[r$starts[i]] + ZIE2_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-19 22:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-25 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#30 cm 
######################################################################

#Subset and remove drips 
#===================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-06-30 10:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-01 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-10 10:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-01 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-10 10:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-09 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-11 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.27325] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-09 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-11 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-10 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-16 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-10 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-16 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-15 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-21 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2683] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-15 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-21 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-10 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-16 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-10 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-16 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-20 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-22 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2672] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-20 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-22 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-22 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-24 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.265] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-22 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-24 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-24 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-30 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-24 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-30 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-26 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-30 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.26525] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-26 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-30 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-30 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-08-30 00:00:01")

#Plot again 
Soil <- ggplot(data = subset(ZIE2_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.263] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-30 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-08-30 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-06-25 10:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-05 10:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.274] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-06-25 10:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-05 10:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-05 00:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-12 10:00:01")

#Plot again 
Soil <- ggplot(data = subset(ZIE2_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2725] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-05 00:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-12 10:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-12 00:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-17 10:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.27] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-12 00:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-17 10:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in July 
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-20 00:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-29 10:00:01")

#Plot again 
Soil <- ggplot(data = subset(ZIE2_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.265] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-20 00:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-29 10:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)


#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-01-01 00:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-01-02 10:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.28] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-01-01 00:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-01-02 10:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-01-02 00:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-01-04 10:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.285] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-01-02 00:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-01-04 10:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-01-04 00:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-01-04 20:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.285] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-01-04 00:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-01-04 20:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-01-04 20:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-01-07 20:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.285] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-01-04 20:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-01-07 20:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-01-08 08:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-01-09 20:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.285] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-01-08 08:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-01-09 20:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-01-09 08:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-01-18 20:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.285] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-01-09 08:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-01-18 20:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-01-12 08:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-01-14 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.29] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-01-12 08:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-01-14 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-01-01 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-01-04 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2858] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time <"2021-01-01 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-01-04 00:00:01")

ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-06-29 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-05 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.276] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time <"2021-06-29 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-05 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-04 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-06 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.27525] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-04 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-06 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-05 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-09 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2745] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-05 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-09 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-08 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-11 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2735] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-08 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-11 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-10 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-13 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2725] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-10 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-13 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-12 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-16 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2715] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-12 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-16 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-15 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-20 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.269] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-15 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-20 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-18 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-23 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2675] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-18 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-23 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-21 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-26 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.2664] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-21 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-26 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-07-25 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-28 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.266] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-07-25 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-28 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Subset and remove drips in early year
#=============================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-06-25 18:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-07-02 00:00:01")

ZIE2_21_fix$WC_30cm[ZIE2_21_fix$WC_30cm < 0.277] <- NA
missing <- which(is.na(ZIE2_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE2_21_fix$WC_30cm[1] <- head(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}
if(nrow(ZIE2_21_fix) %in% missing){
  ZIE2_21_fix$WC_30cm[nrow(data)] <- tail(ZIE2_21_fix$WC_30cm[!is.na(ZIE2_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21_fix$WC_30cm[idx] <- (ZIE2_21_fix$WC_30cm[r$starts[i]] + ZIE2_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-06-25 18:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-07-02 00:00:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#100 cm 
################################################################
ZIE2_21$WC_100cm[ZIE2_21$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE2_19$WC_100cm))

if(1 %in% missing){
  ZIE2_21$WC_100cm[1] <- head(ZIE2_21$WC_100cm[!is.na(ZIE2_21$WC_100cm)],1)
}
if(nrow(ZIE2_21) %in% missing){
  ZIE2_21$WC_100cm[nrow(data)] <- tail(ZIE2_21$WC_100cm[!is.na(ZIE2_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE2_21$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE2_21$WC_100cm[idx] <- (ZIE2_21$WC_100cm[r$starts[i]] + ZIE2_21$WC_100cm[r$ends[i]])/2
}

#Remove glitch 
#=================================================================================
ZIE2_21_fix <- filter(ZIE2_21, Date_time > "2021-01-01 00:00:01")
ZIE2_21_fix <- filter(ZIE2_21_fix, Date_time < "2021-01-21 08:30:01")

Soil <- ggplot(data = subset(ZIE2_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil

ZIE2_21_fix$WC_100cm[ZIE2_21_fix$WC_100cm > 0.25] <- NA

#Recombine
ZIE2_21_early <- filter(ZIE2_21, Date_time < "2021-01-01 00:00:01")
ZIE2_21_late <- filter(ZIE2_21, Date_time > "2021-01-21 08:30:01")
ZIE2_21 <- bind_rows(ZIE2_21_early, ZIE2_21_late, ZIE2_21_fix)

#Plot again 
Soil <- ggplot(data = subset(ZIE2_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
ZIE2_clean <- merge(ZIE2_18, ZIE2_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
ZIE2_clean <- merge(ZIE2_clean, ZIE2_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE2_clean <- merge(ZIE2_clean, ZIE2_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE2_clean <- merge(ZIE2_clean, ZIE2_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(ZIE2_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("ZIE2_Salli", width = 4500, height = 2500)

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
write.csv(ZIE2_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE2_clean.csv" ) #this writes a csv file and sends it to the working folder
