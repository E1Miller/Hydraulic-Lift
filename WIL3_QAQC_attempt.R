#Created by: Elise Miller
#Date started: 10/25/2022
#Date last edited: 01/27/2023
#Description: QA/QC WIL 3  

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
                        pattern=glob2rx("W3M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
WIL3_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("W3M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
WIL3_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
WIL3 <- rbind(WIL3_2017_2019, WIL3_2019_2021)

#Write the csv
write.csv(WIL3,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL3.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(WIL3)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
WIL3$Date <- mdy_hms(WIL3$Date_time)

#Put year into a separate column 
WIL3 <- separate(WIL3, Date, c("Year"))

#WIL3 2017
##################################################################################################
WIL3_17 <- subset(WIL3, Year == '2017')

#Plotting 
WIL3_17$WC_15cm <- as.numeric(WIL3_17$WC_15cm)
WIL3_17$WC_30cm <- as.numeric(WIL3_17$WC_30cm)
WIL3_17$WC_100cm <- as.numeric(WIL3_17$WC_100cm)
WIL3_17$Date_time<- mdy_hms(WIL3_17$Date_time)

Soil <- ggplot(data = subset(WIL3_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL3 2018
##################################################################################################
WIL3_18 <- subset(WIL3, Year == '2018')

#Plotting 
WIL3_18$WC_15cm <- as.numeric(WIL3_18$WC_15cm)
WIL3_18$WC_30cm <- as.numeric(WIL3_18$WC_30cm)
WIL3_18$WC_100cm <- as.numeric(WIL3_18$WC_100cm)
WIL3_18$Date_time<- mdy_hms(WIL3_18$Date_time)

#15 cm
###########################################
WIL3_18$WC_15cm[WIL3_18$WC_15cm < 0] <- NA
missing <- which(is.na(WIL3_18$WC_15cm))

if(1 %in% missing){
  WIL3_18$WC_15cm[1] <- head(WIL3_18$WC_15cm[!is.na(WIL3_18$WC_15cm)],1)
}
if(nrow(WIL3_18) %in% missing){
  WIL3_18$WC_15cm[nrow(data)] <- tail(WIL3_18$WC_15cm[!is.na(WIL3_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18$WC_15cm[idx] <- (WIL3_18$WC_15cm[r$starts[i]] + WIL3_18$WC_15cm[r$ends[i]])/2
}

#Subset and remove dips
#========================================================================
WIL3_18_fix <- filter(WIL3_18, Date_time > "2018-08-20 10:00:01")
WIL3_18_fix <- filter(WIL3_18_fix, Date_time < "2018-08-23 12:00:01")

WIL3_18_fix$WC_15cm[WIL3_18_fix$WC_15cm > 0.125] <- NA
missing <- which(is.na(WIL3_18_fix$WC_15cm))

if(1 %in% missing){
  WIL3_18_fix$WC_15cm[1] <- head(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}
if(nrow(WIL3_18_fix) %in% missing){
  WIL3_18_fix$WC_15cm[nrow(data)] <- tail(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18_fix$WC_15cm[idx] <- (WIL3_18_fix$WC_15cm[r$starts[i]] + WIL3_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL3_18_early <- filter(WIL3_18, Date_time < "2018-08-20 10:00:01")
WIL3_18_late <- filter(WIL3_18, Date_time > "2018-08-23 12:00:01")
WIL3_18 <- bind_rows(WIL3_18_early, WIL3_18_fix, WIL3_18_late)

#Subset and remove dips
#========================================================================
WIL3_18_fix <- filter(WIL3_18, Date_time > "2018-02-25 10:00:01")
WIL3_18_fix <- filter(WIL3_18_fix, Date_time < "2018-02-26 12:00:01")

WIL3_18_fix$WC_15cm[WIL3_18_fix$WC_15cm < 0.21] <- NA
missing <- which(is.na(WIL3_18_fix$WC_15cm))

if(1 %in% missing){
  WIL3_18_fix$WC_15cm[1] <- head(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}
if(nrow(WIL3_18_fix) %in% missing){
  WIL3_18_fix$WC_15cm[nrow(data)] <- tail(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18_fix$WC_15cm[idx] <- (WIL3_18_fix$WC_15cm[r$starts[i]] + WIL3_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL3_18_early <- filter(WIL3_18, Date_time < "2018-02-25 10:00:01")
WIL3_18_late <- filter(WIL3_18, Date_time > "2018-02-26 12:00:01")
WIL3_18 <- bind_rows(WIL3_18_early, WIL3_18_fix, WIL3_18_late)

#Subset and remove dips
#========================================================================
WIL3_18_fix <- filter(WIL3_18, Date_time > "2018-11-01 10:00:01")
WIL3_18_fix <- filter(WIL3_18_fix, Date_time < "2018-11-06 12:00:01")

WIL3_18_fix$WC_15cm[WIL3_18_fix$WC_15cm < 0.0885 | WIL3_18_fix$WC_15cm > 0.091] <- NA
missing <- which(is.na(WIL3_18_fix$WC_15cm))

if(1 %in% missing){
  WIL3_18_fix$WC_15cm[1] <- head(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}
if(nrow(WIL3_18_fix) %in% missing){
  WIL3_18_fix$WC_15cm[nrow(data)] <- tail(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18_fix$WC_15cm[idx] <- (WIL3_18_fix$WC_15cm[r$starts[i]] + WIL3_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL3_18_early <- filter(WIL3_18, Date_time < "2018-11-01 10:00:01")
WIL3_18_late <- filter(WIL3_18, Date_time > "2018-11-06 12:00:01")
WIL3_18 <- bind_rows(WIL3_18_early, WIL3_18_fix, WIL3_18_late)

#Remove glitch 
#==========================================================================================
WIL3_18$WC_15cm[WIL3_18$WC_15cm > 0.7] <- NA
missing <- which(is.na(WIL3_18$WC_15cm))

if(1 %in% missing){
  WIL3_18$WC_15cm[1] <- head(WIL3_18$WC_15cm[!is.na(WIL3_18$WC_15cm)],1)
}
if(nrow(WIL3_18) %in% missing){
  WIL3_18$WC_15cm[nrow(data)] <- tail(WIL3_18$WC_15cm[!is.na(WIL3_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18$WC_15cm[idx] <- (WIL3_18$WC_15cm[r$starts[i]] + WIL3_18$WC_15cm[r$ends[i]])/2
}

#Subset and fix drip 
#=========================================================================================
WIL3_18_fix <- filter(WIL3_18, Date_time > "2018-03-11 12:00:01")
WIL3_18_fix <- filter(WIL3_18_fix, Date_time < "2018-03-12 15:00:01")

WIL3_18_fix$WC_15cm[WIL3_18_fix$WC_15cm > 0.257] <- NA
missing <- which(is.na(WIL3_18_fix$WC_15cm))

if(1 %in% missing){
  WIL3_18_fix$WC_15cm[1] <- head(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}
if(nrow(WIL3_18_fix) %in% missing){
  WIL3_18_fix$WC_15cm[nrow(data)] <- tail(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18_fix$WC_15cm[idx] <- (WIL3_18_fix$WC_15cm[r$starts[i]] + WIL3_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL3_18_early <- filter(WIL3_18, Date_time < "2018-03-11 12:00:01")
WIL3_18_late <- filter(WIL3_18, Date_time > "2018-03-12 15:00:01")

WIL3_18 <- bind_rows(WIL3_18_early, WIL3_18_fix, WIL3_18_late)

#Subset and fix drip 
#=========================================================================================
WIL3_18_fix <- filter(WIL3_18, Date_time > "2018-11-01 12:00:01")
WIL3_18_fix <- filter(WIL3_18_fix, Date_time < "2018-11-22 15:00:01")

WIL3_18_fix$WC_15cm[WIL3_18_fix$WC_15cm < 0.08] <- NA
missing <- which(is.na(WIL3_18_fix$WC_15cm))

if(1 %in% missing){
  WIL3_18_fix$WC_15cm[1] <- head(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}
if(nrow(WIL3_18_fix) %in% missing){
  WIL3_18_fix$WC_15cm[nrow(data)] <- tail(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18_fix$WC_15cm[idx] <- (WIL3_18_fix$WC_15cm[r$starts[i]] + WIL3_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL3_18_early <- filter(WIL3_18, Date_time < "2018-11-01 12:00:01")
WIL3_18_late <- filter(WIL3_18, Date_time > "2018-11-22 15:00:01")
WIL3_18 <- bind_rows(WIL3_18_early, WIL3_18_fix, WIL3_18_late)

#Subset and fix drip 
#=========================================================================================
WIL3_18_fix <- filter(WIL3_18, Date_time > "2018-02-11 12:00:01")
WIL3_18_fix <- filter(WIL3_18_fix, Date_time < "2018-02-13 00:00:01")

WIL3_18_fix$WC_15cm[WIL3_18_fix$WC_15cm > 0.226] <- NA
missing <- which(is.na(WIL3_18_fix$WC_15cm))

if(1 %in% missing){
  WIL3_18_fix$WC_15cm[1] <- head(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}
if(nrow(WIL3_18_fix) %in% missing){
  WIL3_18_fix$WC_15cm[nrow(data)] <- tail(WIL3_18_fix$WC_15cm[!is.na(WIL3_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18_fix$WC_15cm[idx] <- (WIL3_18_fix$WC_15cm[r$starts[i]] + WIL3_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL3_18_early <- filter(WIL3_18, Date_time < "2018-02-11 12:00:01")
WIL3_18_late <- filter(WIL3_18, Date_time > "2018-02-13 00:00:01")
WIL3_18 <- bind_rows(WIL3_18_early, WIL3_18_fix, WIL3_18_late)

#30 cm 
################################################################
WIL3_18$WC_30cm[WIL3_18$WC_30cm < 0] <- NA
missing <- which(is.na(WIL3_18$WC_30cm))

if(1 %in% missing){
  WIL3_18$WC_30cm[1] <- head(WIL3_18$WC_30cm[!is.na(WIL3_18$WC_30cm)],1)
}
if(nrow(WIL3_18) %in% missing){
  WIL3_18$WC_30cm[nrow(data)] <- tail(WIL3_18$WC_30cm[!is.na(WIL3_18$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18$WC_30cm[idx] <- (WIL3_18$WC_30cm[r$starts[i]] + WIL3_18$WC_30cm[r$ends[i]])/2
}

#100 cm 
################################################################
WIL3_18$WC_100cm[WIL3_18$WC_100cm < 0] <- NA
missing <- which(is.na(WIL3_18$WC_100cm))

if(1 %in% missing){
  WIL3_18$WC_100cm[1] <- head(WIL3_18$WC_100cm[!is.na(WIL3_18$WC_100cm)],1)
}
if(nrow(WIL3_18) %in% missing){
  WIL3_18$WC_100cm[nrow(data)] <- tail(WIL3_18$WC_100cm[!is.na(WIL3_18$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_18$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_18$WC_100cm[idx] <- (WIL3_18$WC_100cm[r$starts[i]] + WIL3_18$WC_100cm[r$ends[i]])/2
}

#Fix glitches and missing dates
#################################################################################

#Small missing date from 11/06 to 11/07, but it's also glitching, so remove
#===================================================================================
WIL3_18_fix <- filter(WIL3_18, Date_time > "2018-11-02 18:00:01")
WIL3_18_fix <- filter(WIL3_18_fix, Date_time < "2018-11-07 12:00:01")

WIL3_18_fix$WC_100cm[WIL3_18_fix$WC_100cm > 0] <- NA
WIL3_18_fix$WC_30cm[WIL3_18_fix$WC_30cm > 0] <- NA
WIL3_18_fix$WC_15cm[WIL3_18_fix$WC_15cm > 0] <- NA

#Recombine 
WIL3_18_early <- filter(WIL3_18, Date_time < "2018-11-02 18:00:01")
WIL3_18_late <- filter(WIL3_18, Date_time > "2018-11-07 12:00:01")
WIL3_18 <- bind_rows(WIL3_18_early, WIL3_18_fix, WIL3_18_late)

#Fix glitch before missing date in April 
#========================================================================
WIL3_18_fix <- filter(WIL3_18, Date_time > "2018-04-21 10:00:01")
WIL3_18_fix <- filter(WIL3_18_fix, Date_time < "2018-04-23 12:00:01")

WIL3_18_fix$WC_100cm[WIL3_18_fix$WC_100cm > 0.315] <- NA
WIL3_18_fix$WC_30cm[WIL3_18_fix$WC_30cm > 0.305] <- NA
WIL3_18_fix$WC_15cm[WIL3_18_fix$WC_15cm > 0.2522] <- NA

#Recombine 
WIL3_18_early <- filter(WIL3_18, Date_time < "2018-04-21 10:00:01")
WIL3_18_late <- filter(WIL3_18, Date_time > "2018-04-23 12:00:01")
WIL3_18 <- bind_rows(WIL3_18_early, WIL3_18_fix, WIL3_18_late)

#Replace missing dates with NAs from 04/22 to 05/25
#========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 32, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2018-04-23"), as.Date("2018-05-24"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_18 <- insertRows(WIL3_18, c(16037:16069), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(WIL3_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL 3 2019
##################################################################
WIL3_19 <- subset(WIL3, Year == '2019')

#Plotting 
WIL3_19$WC_15cm <- as.numeric(WIL3_19$WC_15cm)
WIL3_19$WC_30cm <- as.numeric(WIL3_19$WC_30cm)
WIL3_19$WC_100cm <- as.numeric(WIL3_19$WC_100cm)
WIL3_19$Date_time<- mdy_hms(WIL3_19$Date_time)

#15 cm
########################################################################################
WIL3_19$WC_15cm[WIL3_19$WC_15cm < 0.15] <- NA
missing <- which(is.na(WIL3_19$WC_15cm))

if(1 %in% missing){
  WIL3_19$WC_15cm[1] <- head(WIL3_19$WC_15cm[!is.na(WIL3_19$WC_15cm)],1)
}
if(nrow(WIL3_19) %in% missing){
  WIL3_19$WC_15cm[nrow(data)] <- tail(v$WC_15cm[!is.na(WIL3_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_19$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_19$WC_15cm[idx] <- (WIL3_19$WC_15cm[r$starts[i]] + WIL3_19$WC_15cm[r$ends[i]])/2
}

#Subset mid January again 
#===========================================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-09-28 1:00:01")

WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm < 0.22] <- NA
missing <- which(is.na(WIL3_19_fix$WC_15cm))

if(1 %in% missing){
  WIL3_19_fix$WC_15cm[1] <- head(WIL3_19_fix$WC_15cm[!is.na(WIL3_19_fix$WC_15cm)],1)
}
if(nrow(WIL3_19_fix) %in% missing){
  WIL3_19_fix$WC_15cm[nrow(data)] <- tail(WIL3_19_fix$WC_15cm[!is.na(WIL3_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_19_fix$WC_15cm[idx] <- (WIL3_19_fix$WC_15cm[r$starts[i]] + WIL3_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL3_19_later <- filter(WIL3_19, Date_time < "2019-09-28 1:00:01")
WIL3_19 <- bind_rows(WIL3_19_later, WIL3_19_fix)

#Subset mid January again 
#===========================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-09-20 1:00:01")

WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm < 0.22] <- NA
missing <- which(is.na(WIL3_19_fix$WC_15cm))

if(1 %in% missing){
  WIL3_19_fix$WC_15cm[1] <- head(WIL3_19_fix$WC_15cm[!is.na(WIL3_19_fix$WC_15cm)],1)
}
if(nrow(WIL3_19_fix) %in% missing){
  WIL3_19_fix$WC_15cm[nrow(data)] <- tail(WIL3_19_fix$WC_15cm[!is.na(WIL3_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_19_fix$WC_15cm[idx] <- (WIL3_19_fix$WC_15cm[r$starts[i]] + WIL3_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL3_19_later <- filter(WIL3_19, Date_time < "2019-09-20 1:00:01")
WIL3_19 <- bind_rows(WIL3_19_later, WIL3_19_fix)

#Remove 15 cm glitch in mid-August through October
#=======================================================================
WIL3_19$WC_15cm[WIL3_19$WC_15cm == 0.37215] <- NA

#Subset and remove drip in September
#=============================================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-09-17 10:00:01")
WIL3_19_fix <- filter(WIL3_19_fix, Date_time < "2019-09-26 20:00:01")

WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm > 0] <- NA

#Recombine 
WIL3_19_early <- filter(WIL3_19, Date_time < "2019-09-17 10:00:01")
WIL3_19_late <- filter(WIL3_19, Date_time > "2019-09-26 20:00:01")
WIL3_19 <- bind_rows(WIL3_19_early, WIL3_19_fix, WIL3_19_late)

#Subset and remove drip in September
#=============================================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-07-01 10:00:01")
WIL3_19_fix <- filter(WIL3_19_fix, Date_time < "2019-07-18 09:00:01")

WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm > 0] <- NA

#Recombine 
WIL3_19_early <- filter(WIL3_19, Date_time < "2019-07-01 10:00:01")
WIL3_19_late <- filter(WIL3_19, Date_time > "2019-07-18 09:00:01")
WIL3_19 <- bind_rows(WIL3_19_early, WIL3_19_fix, WIL3_19_late)

#30 cm
#############################################################
WIL3_19$WC_30cm[WIL3_19$WC_30cm < 0] <- NA
missing <- which(is.na(WIL3_19$WC_30cm))

if(1 %in% missing){
  WIL3_19$WC_30cm[1] <- head(WIL3_19$WC_30cm[!is.na(WIL3_19$WC_30cm)],1)
}
if(nrow(WIL3_19) %in% missing){
  WIL3_19$WC_30cm[nrow(data)] <- tail(v$WC_30cm[!is.na(WIL3_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_19$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_19$WC_30cm[idx] <- (WIL3_19$WC_30cm[r$starts[i]] + WIL3_19$WC_30cm[r$ends[i]])/2
}

#100 cm 
#############################################################
WIL3_19$WC_100cm[WIL3_19$WC_100cm < 0] <- NA
missing <- which(is.na(WIL3_19$WC_100cm))

if(1 %in% missing){
  WIL3_19$WC_100cm[1] <- head(WIL3_19$WC_100cm[!is.na(WIL3_19$WC_100cm)],1)
}
if(nrow(WIL3_19) %in% missing){
  WIL3_19$WC_100cm[nrow(data)] <- tail(WIL3_19$WC_100cm[!is.na(WIL3_19$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_19$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_19$WC_100cm[idx] <- (WIL3_19$WC_100cm[r$starts[i]] + WIL3_19$WC_100cm[r$ends[i]])/2
}

#Subset and remove drip in August/September
#=============================================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-08-17 10:00:01")
WIL3_19_fix <- filter(WIL3_19_fix, Date_time < "2019-09-21 12:00:01")

WIL3_19_fix$WC_100cm[WIL3_19_fix$WC_100cm > 0.231] <- NA
missing <- which(is.na(WIL3_19_fix$WC_100cm))

if(1 %in% missing){
  WIL3_19_fix$WC_100cm[1] <- head(WIL3_19_fix$WC_100cm[!is.na(WIL3_19_fix$WC_100cm)],1)
}
if(nrow(WIL3_19_fix) %in% missing){
  WIL3_19_fix$WC_100cm[nrow(data)] <- tail(WIL3_19_fix$WC_100cm[!is.na(WIL3_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_19_fix$WC_100cm[idx] <- (WIL3_19_fix$WC_100cm[r$starts[i]] + WIL3_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
WIL3_19_early <- filter(WIL3_19, Date_time < "2019-08-17 10:00:01")
WIL3_19_late <- filter(WIL3_19, Date_time > "2019-09-21 12:00:01")
WIL3_19 <- bind_rows(WIL3_19_early, WIL3_19_fix, WIL3_19_late)

#Fix glitches and replace missing dates
##############################################################################################

#Fix glitch before missing date in January
#========================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-01-19 10:00:01")
WIL3_19_fix <- filter(WIL3_19_fix, Date_time < "2019-01-21 12:00:01")

WIL3_19_fix$WC_100cm[WIL3_19_fix$WC_100cm > 0.3415] <- NA
WIL3_19_fix$WC_30cm[WIL3_19_fix$WC_30cm > 0.33] <- NA
WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm > 0.29] <- NA

#Recombine 
WIL3_19_early <- filter(WIL3_19, Date_time < "2019-01-19 10:00:01")
WIL3_19_late <- filter(WIL3_19, Date_time > "2019-01-21 12:00:01")
WIL3_19 <- bind_rows(WIL3_19_early, WIL3_19_fix, WIL3_19_late)

#Fix glitch before missing date in April
#========================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-03-29 20:00:01")
WIL3_19_fix <- filter(WIL3_19_fix, Date_time < "2019-04-02 12:00:01")

WIL3_19_fix$WC_100cm[WIL3_19_fix$WC_100cm > 0] <- NA
WIL3_19_fix$WC_30cm[WIL3_19_fix$WC_30cm > 0] <- NA
WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm > 0] <- NA

#Recombine 
WIL3_19_early <- filter(WIL3_19, Date_time < "2019-03-29 20:00:01")
WIL3_19_late <- filter(WIL3_19, Date_time > "2019-04-02 12:00:01")
WIL3_19 <- bind_rows(WIL3_19_early, WIL3_19_fix, WIL3_19_late)

#Fix glitch before missing date in June
#========================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-06-16 05:00:01")
WIL3_19_fix <- filter(WIL3_19_fix, Date_time < "2019-06-18 20:00:01")

WIL3_19_fix$WC_100cm[WIL3_19_fix$WC_100cm > 0.3068] <- NA
WIL3_19_fix$WC_30cm[WIL3_19_fix$WC_30cm > 0.268] <- NA
WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm > 0.2] <- NA

#Recombine 
WIL3_19_early <- filter(WIL3_19, Date_time < "2019-06-16 05:00:01")
WIL3_19_late <- filter(WIL3_19, Date_time > "2019-06-18 20:00:01")
WIL3_19 <- bind_rows(WIL3_19_early, WIL3_19_fix, WIL3_19_late)

#Add in NAs for glitch before small missing period from 07/28 to 07/29 
#==========================================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-07-26 15:00:01")
WIL3_19_fix <- filter(WIL3_19_fix, Date_time < "2019-07-29 15:10:01")

WIL3_19_fix$WC_100cm[WIL3_19_fix$WC_100cm > 0] <- NA
WIL3_19_fix$WC_30cm[WIL3_19_fix$WC_30cm > 0] <- NA
WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm > 0] <- NA

#Recombine 
WIL3_19_early <- filter(WIL3_19, Date_time < "2019-07-26 15:00:01")
WIL3_19_late <- filter(WIL3_19, Date_time > "2019-07-29 15:10:01")
WIL3_19 <- bind_rows(WIL3_19_early, WIL3_19_fix, WIL3_19_late)

#Add in NAs for glitch before small missing period from October
#==========================================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-10-18 00:00:01")
WIL3_19_fix <- filter(WIL3_19_fix, Date_time < "2019-10-21 10:10:01")

WIL3_19_fix$WC_100cm[WIL3_19_fix$WC_100cm > 0.218] <- NA
WIL3_19_fix$WC_30cm[WIL3_19_fix$WC_30cm > 0.197] <- NA
WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm > 0.258] <- NA

#Recombine 
WIL3_19_early <- filter(WIL3_19, Date_time < "2019-10-18 00:00:01")
WIL3_19_late <- filter(WIL3_19, Date_time > "2019-10-21 10:10:01")
WIL3_19 <- bind_rows(WIL3_19_early, WIL3_19_fix, WIL3_19_late)


#Remove glitch at the end of the year 
#==============================================================================================
WIL3_19_fix <- filter(WIL3_19, Date_time > "2019-11-03 19:50:01")
WIL3_19_fix <- filter(WIL3_19_fix, Date_time < "2019-12-31 23:50:01")

Soil <- ggplot(data = subset(WIL3_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

WIL3_19_fix$WC_100cm[WIL3_19_fix$WC_100cm > 0] <- NA
WIL3_19_fix$WC_30cm[WIL3_19_fix$WC_30cm > 0] <- NA
WIL3_19_fix$WC_15cm[WIL3_19_fix$WC_15cm > 0] <- NA

#Recombine 
WIL3_19_early <- filter(WIL3_19, Date_time < "2019-11-04 00:00:01")
WIL3_19_late <- filter(WIL3_19, Date_time > "2019-12-31 23:50:01")
WIL3_19 <- bind_rows(WIL3_19_early, WIL3_19_fix, WIL3_19_late)

#Replace missing dates with NAs
#==========================================================================================

#01/21 to 03/13
insertDF <- as.data.frame(matrix(data = NA, nrow = 50, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-01-22"), as.Date("2019-03-12"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_19 <- insertRows(WIL3_19, c(2677:2727), new = insertDF)

#04/01 to 06/03
insertDF <- as.data.frame(matrix(data = NA, nrow = 64, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-04-01"), as.Date("2019-06-03"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_19 <- insertRows(WIL3_19, c(5409:5459), new = insertDF)

#06/18 to 07/12
insertDF <- as.data.frame(matrix(data = NA, nrow = 23, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-06-19"), as.Date("2019-07-11"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_19 <- insertRows(WIL3_19, c(7558:7580), new = insertDF)

#06/18 to 07/12
insertDF <- as.data.frame(matrix(data = NA, nrow = 23, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-06-19"), as.Date("2019-07-11"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_19 <- insertRows(WIL3_19, c(7558:7580), new = insertDF)

#11/05 to 12/31
insertDF <- as.data.frame(matrix(data = NA, nrow = 56, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-11-06"), as.Date("2019-12-31"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_19 <- insertRows(WIL3_19, c(25950:26006), new = insertDF)


#07/28 to 07/29
#There was a period of a few missing hours 

#10/21 to 10/21
#Also missing a period from 09:50:01 until 10:10:01 

#Plot again 
Soil <- ggplot(data = subset(WIL3_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL3 2020
####################################################################################################
WIL3_20 <- subset(WIL3, Year == '2020')

#Plotting 
WIL3_20$WC_15cm <- as.numeric(WIL3_20$WC_15cm)
WIL3_20$WC_30cm <- as.numeric(WIL3_20$WC_30cm)
WIL3_20$WC_100cm <- as.numeric(WIL3_20$WC_100cm)
WIL3_20$Date_time<- mdy_hms(WIL3_20$Date_time)

#15 cm
##############################################################################################
WIL3_20$WC_15cm[WIL3_20$WC_15cm < 0.15] <- NA
missing <- which(is.na(WIL3_20$WC_15cm))

if(1 %in% missing){
  WIL3_20$WC_15cm[1] <- head(WIL3_20$WC_15cm[!is.na(WIL3_20$WC_15cm)],1)
}
if(nrow(WIL3_20) %in% missing){
  WIL3_20$WC_15cm[nrow(data)] <- tail(v$WC_15cm[!is.na(WIL3_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_20$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_20$WC_15cm[idx] <- (WIL3_20$WC_15cm[r$starts[i]] + WIL3_20$WC_15cm[r$ends[i]])/2
}

#Subset and remove drips
#========================================================================================
WIL3_20_fix <- filter(WIL3_20, Date_time > "2020-02-11 20:10:01")
WIL3_20_fix <- filter(WIL3_20_fix, Date_time < "2020-02-14 20:10:01")

WIL3_20_fix$WC_15cm[WIL3_20_fix$WC_15cm < 0.348] <- NA
missing <- which(is.na(WIL3_20_fix$WC_15cm))

if(1 %in% missing){
  WIL3_20_fix$WC_15cm[1] <- head(WIL3_20_fix$WC_15cm[!is.na(WIL3_20_fix$WC_15cm)],1)
}
if(nrow(WIL3_20_fix) %in% missing){
  WIL3_20_fix$WC_15cm[nrow(data)] <- tail(WIL3_20_fix$WC_15cm[!is.na(WIL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_20_fix$WC_15cm[idx] <- (WIL3_20_fix$WC_15cm[r$starts[i]] + WIL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL3_20_later <- filter(WIL3_20, Date_time < "2020-02-11 20:10:01")
WIL3_20_end <- filter(WIL3_20, Date_time > "2020-02-14 20:10:01")
WIL3_20 <- bind_rows(WIL3_20_later, WIL3_20_end, WIL3_20_fix)

#30 cm
#####################################################################################
WIL3_20$WC_30cm[WIL3_20$WC_30cm < 0] <- NA
missing <- which(is.na(WIL3_20$WC_30cm))

if(1 %in% missing){
  WIL3_20$WC_30cm[1] <- head(WIL3_20$WC_30cm[!is.na(WIL3_20$WC_30cm)],1)
}
if(nrow(WIL3_20) %in% missing){
  WIL3_20$WC_30cm[nrow(data)] <- tail(v$WC_30cm[!is.na(WIL3_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_20$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_20$WC_30cm[idx] <- (WIL3_20$WC_30cm[r$starts[i]] + WIL3_20$WC_30cm[r$ends[i]])/2
}

#Subset and fix drips
#================================================================================
WIL3_20_fix <- filter(WIL3_20, Date_time > "2020-08-24 20:10:01")
WIL3_20_fix <- filter(WIL3_20_fix, Date_time < "2020-09-07 20:10:01")

WIL3_20_fix$WC_30cm[WIL3_20_fix$WC_30cm > 0.302] <- NA
missing <- which(is.na(WIL3_20_fix$WC_30cm))

if(1 %in% missing){
  WIL3_20_fix$WC_30cm[1] <- head(WIL3_20_fix$WC_30cm[!is.na(WIL3_20_fix$WC_30cm)],1)
}
if(nrow(WIL3_20_fix) %in% missing){
  WIL3_20_fix$WC_30cm[nrow(data)] <- tail(WIL3_20_fix$WC_30cm[!is.na(WIL3_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_20_fix$WC_30cm[idx] <- (WIL3_20_fix$WC_30cm[r$starts[i]] + WIL3_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL3_20_later <- filter(WIL3_20, Date_time < "2020-08-24 20:10:01")
WIL3_20_end <- filter(WIL3_20, Date_time > "2020-09-07 20:10:01")
WIL3_20 <- bind_rows(WIL3_20_later, WIL3_20_end, WIL3_20_fix)

#100 cm 
#############################################################
WIL3_20$WC_100cm[WIL3_20$WC_100cm < 0] <- NA
missing <- which(is.na(WIL3_20$WC_100cm))

if(1 %in% missing){
  WIL3_20$WC_100cm[1] <- head(WIL3_20$WC_100cm[!is.na(WIL3_20$WC_100cm)],1)
}
if(nrow(WIL3_20) %in% missing){
  WIL3_20$WC_100cm[nrow(data)] <- tail(WIL3_20$WC_100cm[!is.na(WIL3_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_20$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_20$WC_100cm[idx] <- (WIL3_20$WC_100cm[r$starts[i]] + WIL3_20$WC_100cm[r$ends[i]])/2
}

#Missing dates and glitches
#############################################################################################

#Remove glitch in February 
#============================================================================================
WIL3_20_fix <- filter(WIL3_20, Date_time > "2020-02-26 13:00:01")
WIL3_20_fix <- filter(WIL3_20_fix, Date_time < "2020-03-01 10:10:01")

WIL3_20_fix$WC_100cm[WIL3_20_fix$WC_100cm > 0.3134] <- NA
WIL3_20_fix$WC_30cm[WIL3_20_fix$WC_30cm > 0.2774] <- NA
WIL3_20_fix$WC_15cm[WIL3_20_fix$WC_15cm > 0.3344] <- NA

#Recombine 
WIL3_20_early <- filter(WIL3_20, Date_time < "2020-02-26 13:00:01")
WIL3_20_late <- filter(WIL3_20, Date_time > "2020-03-01 10:10:01")
WIL3_20 <- bind_rows(WIL3_20_early, WIL3_20_fix, WIL3_20_late)

#Remove glitch in February 
#============================================================================================
WIL3_20_fix <- filter(WIL3_20, Date_time > "2020-03-22 10:00:01")
WIL3_20_fix <- filter(WIL3_20_fix, Date_time < "2020-03-25 10:10:01")

WIL3_20_fix$WC_100cm[WIL3_20_fix$WC_100cm > 0.297] <- NA
WIL3_20_fix$WC_30cm[WIL3_20_fix$WC_30cm > 0.284] <- NA
WIL3_20_fix$WC_15cm[WIL3_20_fix$WC_15cm > 0.356] <- NA

#Recombine 
WIL3_20_early <- filter(WIL3_20, Date_time < "2020-03-22 10:00:01")
WIL3_20_late <- filter(WIL3_20, Date_time > "2020-03-25 10:10:01")
WIL3_20 <- bind_rows(WIL3_20_early, WIL3_20_fix, WIL3_20_late)

#Replace missing dates with NAs
#==========================================================

#02/28 to 03/11
insertDF <- as.data.frame(matrix(data = NA, nrow = 10, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-03-01"), as.Date("2020-03-10"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_20 <- insertRows(WIL3_20, c(2115:2124), new = insertDF)

#03/24 to 04/13
insertDF <- as.data.frame(matrix(data = NA, nrow = 19, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-03-25"), as.Date("2020-04-12"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_20 <- insertRows(WIL3_20, c(3920:3938), new = insertDF)

#04/16 to 05/08
insertDF <- as.data.frame(matrix(data = NA, nrow = 21, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-04-17"), as.Date("2020-05-07"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_20 <- insertRows(WIL3_20, c(4359:4379), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(WIL3_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL3 2021
##################################################################
WIL3_21 <- subset(WIL3, Year == '2021')

#Plotting 
WIL3_21$WC_15cm <- as.numeric(WIL3_21$WC_15cm)
WIL3_21$WC_30cm <- as.numeric(WIL3_21$WC_30cm)
WIL3_21$WC_100cm <- as.numeric(WIL3_21$WC_100cm)
WIL3_21$Date_time<- mdy_hms(WIL3_21$Date_time)

#15 cm
###########################################
WIL3_21$WC_15cm[WIL3_21$WC_15cm < 0.15] <- NA
missing <- which(is.na(WIL3_21$WC_15cm))

if(1 %in% missing){
  WIL3_21$WC_15cm[1] <- head(WIL3_21$WC_15cm[!is.na(WIL3_21$WC_15cm)],1)
}
if(nrow(WIL3_21) %in% missing){
  WIL3_21$WC_15cm[nrow(data)] <- tail(v$WC_15cm[!is.na(WIL3_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21$WC_15cm[idx] <- (WIL3_21$WC_15cm[r$starts[i]] + WIL3_21$WC_15cm[r$ends[i]])/2
}

#Fix glitch in April before other glitch 
#==========================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-04-25 01:10:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-04-28 10:10:01")

Soil <- ggplot(data = subset(WIL3_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

WIL3_21_fix$WC_15cm[WIL3_21_fix$WC_15cm > 0] <- NA

#Recombine 
WIL3_21_early <- filter(WIL3_21, Date_time < "2021-04-25 01:10:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-04-28 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#30 cm
#############################################################
WIL3_21$WC_30cm[WIL3_21$WC_30cm < 0] <- NA
missing <- which(is.na(WIL3_21$WC_30cm))

if(1 %in% missing){
  WIL3_21$WC_30cm[1] <- head(WIL3_21$WC_30cm[!is.na(WIL3_21$WC_30cm)],1)
}
if(nrow(WIL3_21) %in% missing){
  WIL3_21$WC_30cm[nrow(data)] <- tail(v$WC_30cm[!is.na(WIL3_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21$WC_30cm[idx] <- (WIL3_21$WC_30cm[r$starts[i]] + WIL3_21$WC_30cm[r$ends[i]])/2
}

#100 cm 
#############################################################
WIL3_21$WC_100cm[WIL3_21$WC_100cm < 0] <- NA
missing <- which(is.na(WIL3_21$WC_100cm))

if(1 %in% missing){
  WIL3_21$WC_100cm[1] <- head(WIL3_21$WC_100cm[!is.na(WIL3_21$WC_100cm)],1)
}
if(nrow(WIL3_21) %in% missing){
  WIL3_21$WC_100cm[nrow(data)] <- tail(WIL3_21$WC_100cm[!is.na(WIL3_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21$WC_100cm[idx] <- (WIL3_21$WC_100cm[r$starts[i]] + WIL3_21$WC_100cm[r$ends[i]])/2
}

#Subset and remove drips
#============================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-01-06 10:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-04-01 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm < 0.3] <- NA
missing <- which(is.na(WIL3_21_fix$WC_100cm))

if(1 %in% missing){
  WIL3_21_fix$WC_100cm[1] <- head(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}
if(nrow(WIL3_21_fix) %in% missing){
  WIL3_21_fix$WC_100cm[nrow(data)] <- tail(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21_fix$WC_100cm[idx] <- (WIL3_21_fix$WC_100cm[r$starts[i]] + WIL3_21_fix$WC_100cm[r$ends[i]])/2
}

WIL3_21_early <- filter(WIL3_21, Date_time < "2021-01-06 10:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-04-01 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Subset and remove drips
#============================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-01-06 10:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-01-09 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm < 0.303] <- NA
missing <- which(is.na(WIL3_21_fix$WC_100cm))

if(1 %in% missing){
  WIL3_21_fix$WC_100cm[1] <- head(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}
if(nrow(WIL3_21_fix) %in% missing){
  WIL3_21_fix$WC_100cm[nrow(data)] <- tail(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21_fix$WC_100cm[idx] <- (WIL3_21_fix$WC_100cm[r$starts[i]] + WIL3_21_fix$WC_100cm[r$ends[i]])/2
}

WIL3_21_early <- filter(WIL3_21, Date_time < "2021-01-06 10:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-01-09 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Subset and remove drips
#============================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-01-08 10:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-01-19 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm < 0.302] <- NA
missing <- which(is.na(WIL3_21_fix$WC_100cm))

if(1 %in% missing){
  WIL3_21_fix$WC_100cm[1] <- head(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}
if(nrow(WIL3_21_fix) %in% missing){
  WIL3_21_fix$WC_100cm[nrow(data)] <- tail(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21_fix$WC_100cm[idx] <- (WIL3_21_fix$WC_100cm[r$starts[i]] + WIL3_21_fix$WC_100cm[r$ends[i]])/2
}

WIL3_21_early <- filter(WIL3_21, Date_time < "2021-01-08 10:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-01-19 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Subset and remove drips
#============================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-01-19 10:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-01-29 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm < 0.3008] <- NA
missing <- which(is.na(WIL3_21_fix$WC_100cm))

if(1 %in% missing){
  WIL3_21_fix$WC_100cm[1] <- head(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}
if(nrow(WIL3_21_fix) %in% missing){
  WIL3_21_fix$WC_100cm[nrow(data)] <- tail(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21_fix$WC_100cm[idx] <- (WIL3_21_fix$WC_100cm[r$starts[i]] + WIL3_21_fix$WC_100cm[r$ends[i]])/2
}

WIL3_21_early <- filter(WIL3_21, Date_time < "2021-01-19 10:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-01-29 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Subset and remove drips
#============================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-01-29 10:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-02-28 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm < 0.3025] <- NA
missing <- which(is.na(WIL3_21_fix$WC_100cm))

if(1 %in% missing){
  WIL3_21_fix$WC_100cm[1] <- head(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}
if(nrow(WIL3_21_fix) %in% missing){
  WIL3_21_fix$WC_100cm[nrow(data)] <- tail(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21_fix$WC_100cm[idx] <- (WIL3_21_fix$WC_100cm[r$starts[i]] + WIL3_21_fix$WC_100cm[r$ends[i]])/2
}

WIL3_21_early <- filter(WIL3_21, Date_time < "2021-01-29 10:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-02-28 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Subset and remove drips
#============================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-03-01 10:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-03-28 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm < 0.302] <- NA
missing <- which(is.na(WIL3_21_fix$WC_100cm))

if(1 %in% missing){
  WIL3_21_fix$WC_100cm[1] <- head(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}
if(nrow(WIL3_21_fix) %in% missing){
  WIL3_21_fix$WC_100cm[nrow(data)] <- tail(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21_fix$WC_100cm[idx] <- (WIL3_21_fix$WC_100cm[r$starts[i]] + WIL3_21_fix$WC_100cm[r$ends[i]])/2
}

WIL3_21_early <- filter(WIL3_21, Date_time < "2021-03-01 10:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-03-28 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Subset and remove drips
#============================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-03-08 10:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-03-20 10:10:01")

Soil <- ggplot(data = subset(WIL3_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue"))
Soil 

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm < 0.3035] <- NA
missing <- which(is.na(WIL3_21_fix$WC_100cm))

if(1 %in% missing){
  WIL3_21_fix$WC_100cm[1] <- head(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}
if(nrow(WIL3_21_fix) %in% missing){
  WIL3_21_fix$WC_100cm[nrow(data)] <- tail(WIL3_21_fix$WC_100cm[!is.na(WIL3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL3_21_fix$WC_100cm[idx] <- (WIL3_21_fix$WC_100cm[r$starts[i]] + WIL3_21_fix$WC_100cm[r$ends[i]])/2
}

WIL3_21_early <- filter(WIL3_21, Date_time < "2021-03-08 10:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-03-20 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Missing dates and fix glitches
############################################################################################

#Fix glitch before 04/28 glitch 
#===========================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-04-26 10:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-04-30 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm > 0.295] <- NA
WIL3_21_fix$WC_30cm[WIL3_21_fix$WC_30cm > 0.332] <- NA
WIL3_21_fix$WC_15cm[WIL3_21_fix$WC_15cm > 0.307] <- NA

#Recombine 
WIL3_21_early <- filter(WIL3_21, Date_time < "2021-04-26 10:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-04-30 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Fix glitch before 05/25 missing date
#===========================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-05-22 10:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-05-26 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm > 0.28] <- NA
WIL3_21_fix$WC_30cm[WIL3_21_fix$WC_30cm > 0.319] <- NA
WIL3_21_fix$WC_15cm[WIL3_21_fix$WC_15cm > 0.273] <- NA

#Recombine 
WIL3_21_early <- filter(WIL3_21, Date_time < "2021-05-22 10:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-05-26 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Fix glitch before 06/25 missing date
#===========================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-06-23 23:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-06-26 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm > 0] <- NA
WIL3_21_fix$WC_30cm[WIL3_21_fix$WC_30cm > 0] <- NA
WIL3_21_fix$WC_15cm[WIL3_21_fix$WC_15cm > 0] <- NA

#Recombine 
WIL3_21_early <- filter(WIL3_21, Date_time < "2021-06-23 23:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-06-26 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Fix glitch before 07/14 missing date
#===========================================================================================
WIL3_21_fix <- filter(WIL3_21, Date_time > "2021-07-14 13:00:01")
WIL3_21_fix <- filter(WIL3_21_fix, Date_time < "2021-07-16 10:10:01")

WIL3_21_fix$WC_100cm[WIL3_21_fix$WC_100cm > 0.258] <- NA
WIL3_21_fix$WC_30cm[WIL3_21_fix$WC_30cm > 0.306] <- NA
WIL3_21_fix$WC_15cm[WIL3_21_fix$WC_15cm > 0.233] <- NA

#Recombine 
WIL3_21_early <- filter(WIL3_21, Date_time < "2021-07-14 13:00:01")
WIL3_21_late <- filter(WIL3_21, Date_time > "2021-07-16 10:10:01")
WIL3_21 <- bind_rows(WIL3_21_early, WIL3_21_fix, WIL3_21_late)

#Replace missing dates with NAs
#==============================================================================================

#04/28 to 05/07
insertDF <- as.data.frame(matrix(data = NA, nrow = 8, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-04-29"), as.Date("2021-05-06"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_21 <- insertRows(WIL3_21, c(16962:16969), new = insertDF)

#05/25 to 06/04
insertDF <- as.data.frame(matrix(data = NA, nrow = 9, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-05-26"), as.Date("2021-06-03"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_21 <- insertRows(WIL3_21, c(19520:19528), new = insertDF)

#06/25 to 07/02
insertDF <- as.data.frame(matrix(data = NA, nrow = 6, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-06-26"), as.Date("2021-07-01"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_21 <- insertRows(WIL3_21, c(22529:22534), new = insertDF)

#07/14 to 07/26
insertDF <- as.data.frame(matrix(data = NA, nrow = 13, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-07-14"), as.Date("2021-07-26"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL3_21 <- insertRows(WIL3_21, c(24440:24452), new = insertDF)

#Plot
#===============================================================================
Soil <- ggplot(data = subset(WIL3_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Combine all of the WIL1 dataframes/years into 1 dataframe 

#Merge 2018 and 2019 
WIL3_clean <- merge(WIL3_18, WIL3_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
WIL3_clean <- merge(WIL3_clean, WIL3_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021

WIL3_clean <- merge(WIL3_clean, WIL3_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

WIL3_clean <- merge(WIL3_clean, WIL3_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Clean up WIL3 clean csv
WIL3_clean <- select(WIL3_clean, Date_time, WC_15cm, WC_30cm, WC_100cm)


#Graph

Soil <- ggplot(data = subset(WIL3_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("WIL3_Salli", width = 4500, height = 2500)

Soil + theme(axis.line = element_line(size = 0.4,
                                      linetype = "solid"), axis.text = element_text(size = 70),
             panel.background = element_rect(fill = NA, 
                                             linetype = "solid"), legend.key = element_rect(fill = NA),
             legend.text = element_text(size = 70), legend.title = element_text(size = 70),
             legend.background = element_rect(fill = NA)) + theme(axis.text = element_text(size = 70))+ 
  theme(axis.title.y = element_text(family = "Times New Roman", size = 90)) +
  theme(axis.title.x = element_text(family = "Times New Roman", size = 90)) 
dev.off()

#Write the csv
write.csv(WIL3_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL3_clean.csv" ) #this writes a csv file and sends it to the working folder


