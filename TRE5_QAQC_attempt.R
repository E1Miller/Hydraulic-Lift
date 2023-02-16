#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/15/2023
#Description: QA/QC TRE 5

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

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new", 
                        pattern=glob2rx("Copy of T5M_H*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
TRE5_2019_2021 <- data19_21 %>%
  rename_all(funs(c("Date", "Time", "PAR", "WC_15cm", "WC_30cm","WC_100cm", "Date_time"))) %>% #this renames all of the columns
  select(Date_time, PAR, WC_15cm, WC_30cm, WC_100cm) #this keeps only the columns you will use


#2017-2019 FILES
#========================================================================================================================
#If wanting to merge the files, needed to manually delete the extra column in W1M190111 and 
#extra row at the top, and needed to manually delete the extra columns in W1M171228, W1M180201, and W1M180302

#Set the data path 
data_path <- "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new"
new_col_name <- c("Date_time", "PAR", "WC_15cm", "WC_30cm", "WC_100cm")

#Call in all the files in this folder with the W2M1 pattern, which excludes the datasheets from 2019-2020
files <- dir(data_path, pattern=glob2rx("Copy of T5M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
TRE5_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
TRE5 <- rbind(TRE5_2017_2019, TRE5_2019_2021)

#Write the csv
write.csv(TRE5,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE5.csv") #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(TRE5)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
TRE5$Date <- mdy_hms(TRE5$Date_time)

#Put year into a separate column 
TRE5 <- separate(TRE5, Date, c("Year"))

#TRE5 2017
##################################################################################################
TRE5_17 <- subset(TRE5, Year == '2017')

#Plotting 
TRE5_17$WC_15cm <- as.numeric(TRE5_17$WC_15cm)
TRE5_17$WC_30cm <- as.numeric(TRE5_17$WC_30cm)
TRE5_17$WC_100cm <- as.numeric(TRE5_17$WC_100cm)
TRE5_17$Date_time<- mdy_hms(TRE5_17$Date_time)

Soil <- ggplot(data = subset(TRE5_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE5 2018
##################################################################################################
TRE5_18 <- subset(TRE5, Year == '2018')

#Plotting 
TRE5_18$WC_15cm <- as.numeric(TRE5_18$WC_15cm)
TRE5_18$WC_30cm <- as.numeric(TRE5_18$WC_30cm)
TRE5_18$WC_100cm <- as.numeric(TRE5_18$WC_100cm)
TRE5_18$Date_time<- mdy_hms(TRE5_18$Date_time)

#15 cm 
###################################################################################
#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-06-01 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-06-11 1:00:01")

TRE5_18_fix$WC_15cm[TRE5_18_fix$WC_15cm < 0.309] <- NA
missing <- which(is.na(TRE5_18_fix$WC_15cm))

if(1 %in% missing){
  TRE5_18_fix$WC_15cm[1] <- head(TRE5_18_fix$WC_15cm[!is.na(TRE5_18_fix$WC_15cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_15cm[nrow(data)] <- tail(TRE5_18_fix$WC_15cm[!is.na(TRE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_15cm[idx] <- (TRE5_18_fix$WC_15cm[r$starts[i]] + TRE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-06-01 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-06-11 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-06-28 07:30:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-07-06 06:10:01")

TRE5_18_fix$WC_15cm <- TRE5_18_fix$WC_15cm + 0.002

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-06-28 07:30:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-07-06 06:10:01 ")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Calibrate October 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-10-24 05:30:01")

TRE5_18_fix$WC_15cm <- TRE5_18_fix$WC_15cm + 0.004

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-10-24 05:30:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix)

#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-10-22 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-11-01 1:00:01")

TRE5_18_fix$WC_15cm[TRE5_18_fix$WC_15cm > 0.2649 | TRE5_18_fix$WC_15cm < 0.261] <- NA
missing <- which(is.na(TRE5_18_fix$WC_15cm))

if(1 %in% missing){
  TRE5_18_fix$WC_15cm[1] <- head(TRE5_18_fix$WC_15cm[!is.na(TRE5_18_fix$WC_15cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_15cm[nrow(data)] <- tail(TRE5_18_fix$WC_15cm[!is.na(TRE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_15cm[idx] <- (TRE5_18_fix$WC_15cm[r$starts[i]] + TRE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-10-22 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-11-01 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-11-20 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-11-22 1:00:01")


TRE5_18_fix$WC_15cm[TRE5_18_fix$WC_15cm < 0.26] <- NA
missing <- which(is.na(TRE5_18_fix$WC_15cm))

if(1 %in% missing){
  TRE5_18_fix$WC_15cm[1] <- head(TRE5_18_fix$WC_15cm[!is.na(TRE5_18_fix$WC_15cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_15cm[nrow(data)] <- tail(TRE5_18_fix$WC_15cm[!is.na(TRE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_15cm[idx] <- (TRE5_18_fix$WC_15cm[r$starts[i]] + TRE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-11-20 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-11-22 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#30 cm 
################################################################
TRE5_18$WC_30cm[TRE5_18$WC_30cm < 0.2] <- NA
missing <- which(is.na(TRE5_18$WC_30cm))

if(1 %in% missing){
  TRE5_18$WC_30cm[1] <- head(TRE5_18$WC_30cm[!is.na(TRE5_18$WC_30cm)],1)
}
if(nrow(TRE5_18) %in% missing){
  TRE5_18$WC_30cm[nrow(data)] <- tail(TRE5_18$WC_30cm[!is.na(TRE5_18$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18$WC_30cm[idx] <- (TRE5_18$WC_30cm[r$starts[i]] + TRE5_18$WC_30cm[r$ends[i]])/2
}

#Subset to get rid of drips
#==========================================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-10-20 05:30:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-10-24 23:30:01")

TRE5_18_fix$WC_30cm[TRE5_18_fix$WC_30cm < 0.223] <- NA
missing <- which(is.na(TRE5_18_fix$WC_30cm))

if(1 %in% missing){
  TRE5_18_fix$WC_30cm[1] <- head(TRE5_18_fix$WC_30cm[!is.na(TRE5_18_fix$WC_30cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_30cm[nrow(data)] <- tail(TRE5_18_fix$WC_30cm[!is.na(TRE5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_30cm[idx] <- (TRE5_18_fix$WC_30cm[r$starts[i]] + TRE5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-10-20 05:30:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-10-24 23:30:01")
TRE5_18 <- bind_rows(TRE5_18_fix, TRE5_18_early, TRE5_18_late)

#Subset to get rid of drips
#==========================================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-11-15 05:30:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-11-22 03:30:01")

TRE5_18_fix$WC_30cm[TRE5_18_fix$WC_30cm > 0.212] <- NA
missing <- which(is.na(TRE5_18_fix$WC_30cm))

if(1 %in% missing){
  TRE5_18_fix$WC_30cm[1] <- head(TRE5_18_fix$WC_30cm[!is.na(TRE5_18_fix$WC_30cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_30cm[nrow(data)] <- tail(TRE5_18_fix$WC_30cm[!is.na(TRE5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_30cm[idx] <- (TRE5_18_fix$WC_30cm[r$starts[i]] + TRE5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-11-15 05:30:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-11-22 03:30:01")
TRE5_18 <- bind_rows(TRE5_18_fix, TRE5_18_early, TRE5_18_late)

#Subset to get rid of drips
#==========================================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-10-25 05:30:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-10-31 20:00:01")

TRE5_18_fix$WC_30cm[TRE5_18_fix$WC_30cm < 0.216] <- NA

#Recombine 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-10-25 05:30:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-10-31 20:00:01")
TRE5_18 <- bind_rows(TRE5_18_fix, TRE5_18_early, TRE5_18_late)

#Subset to get rid of drips
#==========================================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-11-19 05:30:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-11-22 00:00:01")

TRE5_18_fix$WC_30cm[TRE5_18_fix$WC_30cm < 0.2050] <- NA
missing <- which(is.na(TRE5_18_fix$WC_30cm))

if(1 %in% missing){
  TRE5_18_fix$WC_30cm[1] <- head(TRE5_18_fix$WC_30cm[!is.na(TRE5_18_fix$WC_30cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_30cm[nrow(data)] <- tail(TRE5_18_fix$WC_30cm[!is.na(TRE5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_30cm[idx] <- (TRE5_18_fix$WC_30cm[r$starts[i]] + TRE5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-11-19 05:30:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-11-22 00:00:01")
TRE5_18 <- bind_rows(TRE5_18_fix, TRE5_18_early, TRE5_18_late)

#Subset to get rid of drips
#==========================================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-11-21 23:30:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-11-30 00:00:01")

Soil <- ggplot(data = subset(TRE5_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "lightblue"))
Soil 

TRE5_18_fix$WC_30cm[TRE5_18_fix$WC_30cm < 0.21] <- NA
missing <- which(is.na(TRE5_18_fix$WC_30cm))

if(1 %in% missing){
  TRE5_18_fix$WC_30cm[1] <- head(TRE5_18_fix$WC_30cm[!is.na(TRE5_18_fix$WC_30cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_30cm[nrow(data)] <- tail(TRE5_18_fix$WC_30cm[!is.na(TRE5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_30cm[idx] <- (TRE5_18_fix$WC_30cm[r$starts[i]] + TRE5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-11-21 23:30:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-11-30 00:00:01")
TRE5_18 <- bind_rows(TRE5_18_fix, TRE5_18_early, TRE5_18_late)

#100 cm 
################################################################
TRE5_18$WC_100cm[TRE5_18$WC_100cm < 0.1] <- NA
missing <- which(is.na(TRE5_18$WC_100cm))

if(1 %in% missing){
  TRE5_18$WC_100cm[1] <- head(TRE5_18$WC_100cm[!is.na(TRE5_18$WC_100cm)],1)
}
if(nrow(TRE5_18) %in% missing){
  TRE5_18$WC_100cm[nrow(data)] <- tail(TRE5_18$WC_100cm[!is.na(TRE5_18$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18$WC_100cm[idx] <- (TRE5_18$WC_100cm[r$starts[i]] + TRE5_18$WC_100cm[r$ends[i]])/2
}

#Subset to get rid of drips
#==========================================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-06-30 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-10-20 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.12] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-06-30 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-10-20 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_fix, TRE5_18_early, TRE5_18_late)

#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-07-01 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-07-10 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.191] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-07-01 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-07-10 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-07-10 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-07-17 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.17] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-07-10 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-07-17 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset end of July
#==============================================================================
#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-08-01 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-08-20 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.16] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-08-01 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-08-20 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-09-01 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-09-10 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.147] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-09-01 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-09-10 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-09-13 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-09-19 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.140] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-09-13 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-09-19 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)


#Subset again 
#==================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-09-20 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-10-31 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.129] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-09-20 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-10-31 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-09-01 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-09-24 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.139] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-09-01 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-09-24 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-08-27 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-09-05 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.148] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-08-27 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-09-05 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-07-06 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-07-13 1:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.1875] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-07-06 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-07-13 1:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-07-06 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-07-09 12:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.193] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-07-06 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-07-09 12:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-07-09 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-07-15 12:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.187] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-07-09 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-07-15 12:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-08-03 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-08-08 12:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.1678] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-08-03 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-08-08 12:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-09-03 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-09-06 12:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.152] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-09-03 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-09-06 12:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-09-17 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-09-18 10:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.144] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-09-17 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-09-18 10:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-09-18 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-09-20 20:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.1423] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-09-18 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-09-20 20:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Subset again 
#=======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-10-03 1:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-10-12 10:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm < 0.138] <- NA
missing <- which(is.na(TRE5_18_fix$WC_100cm))

if(1 %in% missing){
  TRE5_18_fix$WC_100cm[1] <- head(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}
if(nrow(TRE5_18_fix) %in% missing){
  TRE5_18_fix$WC_100cm[nrow(data)] <- tail(TRE5_18_fix$WC_100cm[!is.na(TRE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_18_fix$WC_100cm[idx] <- (TRE5_18_fix$WC_100cm[r$starts[i]] + TRE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-10-03 1:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-10-12 10:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Fix glitches and missing dates
########################################################################

#Fix glitch before May date
#======================================================================
TRE5_18_fix <- filter(TRE5_18, Date_time > "2018-05-12 14:00:01")
TRE5_18_fix <- filter(TRE5_18_fix, Date_time < "2018-05-14 10:00:01")

TRE5_18_fix$WC_100cm[TRE5_18_fix$WC_100cm > 0] <- NA
TRE5_18_fix$WC_30cm[TRE5_18_fix$WC_30cm > 0] <- NA
TRE5_18_fix$WC_15cm[TRE5_18_fix$WC_15cm > 0] <- NA

#Recombine and subset again 
TRE5_18_early <- filter(TRE5_18, Date_time < "2018-05-12 14:00:01")
TRE5_18_late <- filter(TRE5_18, Date_time > "2018-05-14 10:00:01")
TRE5_18 <- bind_rows(TRE5_18_early, TRE5_18_fix, TRE5_18_late)

#Replace missing dates with NAs - 05/12 to 05/25
#========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 12, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2018-05-13"), as.Date("2018-05-24"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE5_18 <- insertRows(TRE5_18, c(23333:23344), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE5_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE5 2019
##################################################################################################
TRE5_19 <- subset(TRE5, Year == '2019')

#Plotting 
TRE5_19$WC_15cm <- as.numeric(TRE5_19$WC_15cm)
TRE5_19$WC_30cm <- as.numeric(TRE5_19$WC_30cm)
TRE5_19$WC_100cm <- as.numeric(TRE5_19$WC_100cm)
TRE5_19$Date_time<- mdy_hms(TRE5_19$Date_time)

#30 cm
##########################################################################

#Subset and remove glitches
#=====================================================================
TRE5_19_fix <- filter(TRE5_19, Date_time > "2019-08-07 10:00:01")
TRE5_19_fix <- filter(TRE5_19_fix, Date_time < "2019-08-25 00:00:01")

TRE5_19_fix$WC_30cm[TRE5_19_fix$WC_30cm  > 0.220] <- NA
missing <- which(is.na(TRE5_19_fix$WC_30cm))

if(1 %in% missing){
  TRE5_19_fix$WC_30cm[1] <- head(TRE5_19_fix$WC_30cm[!is.na(TRE5_19_fix$WC_30cm)],1)
}
if(nrow(TRE5_19_fix) %in% missing){
  TRE5_19_fix$WC_30cm[nrow(data)] <- tail(TRE5_19_fix$WC_30cm[!is.na(TRE5_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_19_fix$WC_30cm[idx] <- (TRE5_19_fix$WC_30cm[r$starts[i]] + TRE5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE5_19_early <- filter(TRE5_19, Date_time < "2019-08-07 10:00:01")
TRE5_19_late <- filter(TRE5_19, Date_time > "2019-08-25 00:00:01")
TRE5_19 <- bind_rows(TRE5_19_early, TRE5_19_late, TRE5_19_fix)

#Subset and remove glitches
#=====================================================================
TRE5_19_fix <- filter(TRE5_19, Date_time > "2019-08-21 10:00:01")
TRE5_19_fix <- filter(TRE5_19_fix, Date_time < "2019-08-25 00:00:01")

TRE5_19_fix$WC_30cm[TRE5_19_fix$WC_30cm  > 0.215] <- NA
missing <- which(is.na(TRE5_19_fix$WC_30cm))

if(1 %in% missing){
  TRE5_19_fix$WC_30cm[1] <- head(TRE5_19_fix$WC_30cm[!is.na(TRE5_19_fix$WC_30cm)],1)
}
if(nrow(TRE5_19_fix) %in% missing){
  TRE5_19_fix$WC_30cm[nrow(data)] <- tail(TRE5_19_fix$WC_30cm[!is.na(TRE5_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_19_fix$WC_30cm[idx] <- (TRE5_19_fix$WC_30cm[r$starts[i]] + TRE5_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE5_19_early <- filter(TRE5_19, Date_time < "2019-08-21 10:00:01")
TRE5_19_late <- filter(TRE5_19, Date_time > "2019-08-25 00:00:01")
TRE5_19 <- bind_rows(TRE5_19_early, TRE5_19_late, TRE5_19_fix)

#100 cm 
#####################################################################

#Subset and remove glitches
#=====================================================================
TRE5_19_fix <- filter(TRE5_19, Date_time > "2019-09-14 10:00:01")
TRE5_19_fix <- filter(TRE5_19_fix, Date_time < "2019-09-17 00:00:01")


TRE5_19_fix$WC_100cm[TRE5_19_fix$WC_100cm  > 0.166] <- NA
missing <- which(is.na(TRE5_19_fix$WC_100cm))

if(1 %in% missing){
  TRE5_19_fix$WC_100cm[1] <- head(TRE5_19_fix$WC_100cm[!is.na(TRE5_19_fix$WC_100cm)],1)
}
if(nrow(TRE5_19_fix) %in% missing){
  TRE5_19_fix$WC_100cm[nrow(data)] <- tail(TRE5_19_fix$WC_100cm[!is.na(TRE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_19_fix$WC_100cm[idx] <- (TRE5_19_fix$WC_100cm[r$starts[i]] + TRE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE5_19_early <- filter(TRE5_19, Date_time < "2019-09-14 10:00:01")
TRE5_19_late <- filter(TRE5_19, Date_time > "2019-09-17 00:00:01")
TRE5_19 <- bind_rows(TRE5_19_early, TRE5_19_late, TRE5_19_fix)

#Remove the missing dates
#=========================================================================
#Replace missing dates with NAs - 09/27 to 12/31
insertDF <- as.data.frame(matrix(data = NA, nrow = 95, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-09-28"), as.Date("2019-12-31"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE5_19 <- insertRows(TRE5_19, c(38789:38884), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE5_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE5 2020
##################################################################################################
TRE5_20 <- subset(TRE5, Year == '2020')

#Plotting 
TRE5_20$WC_15cm <- as.numeric(TRE5_20$WC_15cm)
TRE5_20$WC_30cm <- as.numeric(TRE5_20$WC_30cm)
TRE5_20$WC_100cm <- as.numeric(TRE5_20$WC_100cm)
TRE5_20$Date_time<- mdy_hms(TRE5_20$Date_time)

#15 cm
###########################################
TRE5_20$WC_15cm[TRE5_20$WC_15cm < 0] <- NA
missing <- which(is.na(TRE5_20$WC_15cm))

if(1 %in% missing){
  TRE5_20$WC_15cm[1] <- head(TRE5_20$WC_15cm[!is.na(TRE5_20$WC_15cm)],1)
}
if(nrow(TRE5_20) %in% missing){
  TRE5_20$WC_15cm[nrow(data)] <- tail(TRE5_20$WC_15cm[!is.na(TRE5_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_20$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_20$WC_15cm[idx] <- (TRE5_20$WC_15cm[r$starts[i]] + TRE5_20$WC_15cm[r$ends[i]])/2
}

#Subset and remove glitches
#=====================================================================
TRE5_20_fix <- filter(TRE5_20, Date_time > "2020-09-15 10:00:01")
TRE5_20_fix <- filter(TRE5_20_fix, Date_time < "2020-09-25 00:00:01")

TRE5_20_fix$WC_15cm[TRE5_20_fix$WC_15cm < 0.258] <- NA
missing <- which(is.na(TRE5_20_fix$WC_15cm))

if(1 %in% missing){
  TRE5_20_fix$WC_15cm[1] <- head(TRE5_20_fix$WC_15cm[!is.na(TRE5_20_fix$WC_15cm)],1)
}
if(nrow(TRE5_20_fix) %in% missing){
  TRE5_20_fix$WC_15cm[nrow(data)] <- tail(TRE5_20_fix$WC_15cm[!is.na(TRE5_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_20_fix$WC_15cm[idx] <- (TRE5_20_fix$WC_15cm[r$starts[i]] + TRE5_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE5_20_early <- filter(TRE5_20, Date_time < "2020-09-15 10:00:01")
TRE5_20_late <- filter(TRE5_20, Date_time > "2020-09-25 00:00:01")
TRE5_20 <- bind_rows(TRE5_20_early, TRE5_20_late, TRE5_20_fix)

#Subset and remove glitches
#=====================================================================
TRE5_20_fix <- filter(TRE5_20, Date_time > "2020-09-25 10:00:01")
TRE5_20_fix <- filter(TRE5_20_fix, Date_time < "2020-10-12 00:00:01")

TRE5_20_fix$WC_15cm[TRE5_20_fix$WC_15cm < 0.2525] <- NA
missing <- which(is.na(TRE5_20_fix$WC_15cm))

if(1 %in% missing){
  TRE5_20_fix$WC_15cm[1] <- head(TRE5_20_fix$WC_15cm[!is.na(TRE5_20_fix$WC_15cm)],1)
}
if(nrow(TRE5_20_fix) %in% missing){
  TRE5_20_fix$WC_15cm[nrow(data)] <- tail(TRE5_20_fix$WC_15cm[!is.na(TRE5_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_20_fix$WC_15cm[idx] <- (TRE5_20_fix$WC_15cm[r$starts[i]] + TRE5_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE5_20_early <- filter(TRE5_20, Date_time < "2020-09-25 10:00:01")
TRE5_20_late <- filter(TRE5_20, Date_time > "2020-10-12 00:00:01")
TRE5_20 <- bind_rows(TRE5_20_early, TRE5_20_late, TRE5_20_fix)

#Subset and remove glitches
#=====================================================================
TRE5_20_fix <- filter(TRE5_20, Date_time > "2020-11-02 10:00:01")
TRE5_20_fix <- filter(TRE5_20_fix, Date_time < "2020-11-06 02:00:01")

TRE5_20_fix$WC_15cm[TRE5_20_fix$WC_15cm < 0.2515] <- NA
missing <- which(is.na(TRE5_20_fix$WC_15cm))

if(1 %in% missing){
  TRE5_20_fix$WC_15cm[1] <- head(TRE5_20_fix$WC_15cm[!is.na(TRE5_20_fix$WC_15cm)],1)
}
if(nrow(TRE5_20_fix) %in% missing){
  TRE5_20_fix$WC_15cm[nrow(data)] <- tail(TRE5_20_fix$WC_15cm[!is.na(TRE5_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_20_fix$WC_15cm[idx] <- (TRE5_20_fix$WC_15cm[r$starts[i]] + TRE5_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE5_20_early <- filter(TRE5_20, Date_time < "2020-11-02 10:00:01")
TRE5_20_late <- filter(TRE5_20, Date_time > "2020-11-06 02:00:01")
TRE5_20 <- bind_rows(TRE5_20_early, TRE5_20_late, TRE5_20_fix)

#30 cm 
################################################################
TRE5_20$WC_30cm[TRE5_20$WC_30cm < 0] <- NA
missing <- which(is.na(TRE5_20$WC_30cm))

if(1 %in% missing){
  TRE5_20$WC_30cm[1] <- head(TRE5_20$WC_30cm[!is.na(TRE5_20$WC_30cm)],1)
}
if(nrow(TRE5_20) %in% missing){
  TRE5_20$WC_30cm[nrow(data)] <- tail(TRE5_20$WC_30cm[!is.na(TRE5_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_20$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_20$WC_30cm[idx] <- (TRE5_20$WC_30cm[r$starts[i]] + TRE5_20$WC_30cm[r$ends[i]])/2
}

#100 cm 
################################################################
TRE5_20$WC_100cm[TRE5_20$WC_100cm > 0.305] <- NA

#Remove the missing dates
#=========================================================================
#Replace missing dates with NAs - 01/01 to 08/21
insertDF <- as.data.frame(matrix(data = NA, nrow = 235, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-01-01"), as.Date("2020-08-22"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE5_20 <- insertRows(TRE5_20, c(1:234), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE5_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE5 2021
##################################################################################################
TRE5_21 <- subset(TRE5, Year == '2021')

#Plotting 
TRE5_21$WC_15cm <- as.numeric(TRE5_21$WC_15cm)
TRE5_21$WC_30cm <- as.numeric(TRE5_21$WC_30cm)
TRE5_21$WC_100cm <- as.numeric(TRE5_21$WC_100cm)
TRE5_21$Date_time<- mdy_hms(TRE5_21$Date_time)

#15 cm
###########################################
TRE5_21$WC_15cm[TRE5_21$WC_15cm < 0] <- NA
missing <- which(is.na(TRE5_21$WC_15cm))

if(1 %in% missing){
  TRE5_21$WC_15cm[1] <- head(TRE5_21$WC_15cm[!is.na(TRE5_21$WC_15cm)],1)
}
if(nrow(TRE5_21) %in% missing){
  TRE5_21$WC_15cm[nrow(data)] <- tail(TRE5_21$WC_15cm[!is.na(TRE5_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21$WC_15cm[idx] <- (TRE5_21$WC_15cm[r$starts[i]] + TRE5_21$WC_15cm[r$ends[i]])/2
}

#Subset and remove glitches
#=====================================================================
TRE5_21_fix <- filter(TRE5_21, Date_time > "2021-04-23 10:00:01")
TRE5_21_fix <- filter(TRE5_21_fix, Date_time < "2021-04-24 20:00:01")

TRE5_21_fix$WC_15cm[TRE5_21_fix$WC_15cm > 0.321] <- NA
missing <- which(is.na(TRE5_21_fix$WC_15cm))

if(1 %in% missing){
  TRE5_21_fix$WC_15cm[1] <- head(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}
if(nrow(TRE5_21_fix) %in% missing){
  TRE5_21_fix$WC_15cm[nrow(data)] <- tail(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21_fix$WC_15cm[idx] <- (TRE5_21_fix$WC_15cm[r$starts[i]] + TRE5_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE5_21_early <- filter(TRE5_21, Date_time < "2021-04-23 10:00:01")
TRE5_21_late <- filter(TRE5_21, Date_time > "2021-04-24 20:00:01")
TRE5_21 <- bind_rows(TRE5_21_early, TRE5_21_late, TRE5_21_fix)

#Subset and remove glitches
#=====================================================================
TRE5_21_fix <- filter(TRE5_21, Date_time > "2021-04-30 10:00:01")
TRE5_21_fix <- filter(TRE5_21_fix, Date_time < "2021-05-03 20:00:01")

TRE5_21_fix$WC_15cm[TRE5_21_fix$WC_15cm > 0.334] <- NA
missing <- which(is.na(TRE5_21_fix$WC_15cm))

if(1 %in% missing){
  TRE5_21_fix$WC_15cm[1] <- head(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}
if(nrow(TRE5_21_fix) %in% missing){
  TRE5_21_fix$WC_15cm[nrow(data)] <- tail(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21_fix$WC_15cm[idx] <- (TRE5_21_fix$WC_15cm[r$starts[i]] + TRE5_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE5_21_early <- filter(TRE5_21, Date_time < "2021-04-30 10:00:01")
TRE5_21_late <- filter(TRE5_21, Date_time > "2021-05-03 20:00:01")
TRE5_21 <- bind_rows(TRE5_21_early, TRE5_21_late, TRE5_21_fix)

#Subset and remove glitches
#=====================================================================
TRE5_21_fix <- filter(TRE5_21, Date_time > "2021-05-24 10:00:01")
TRE5_21_fix <- filter(TRE5_21_fix, Date_time < "2021-05-30 20:00:01")

TRE5_21_fix$WC_15cm[TRE5_21_fix$WC_15cm > 0.3075] <- NA
missing <- which(is.na(TRE5_21_fix$WC_15cm))

if(1 %in% missing){
  TRE5_21_fix$WC_15cm[1] <- head(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}
if(nrow(TRE5_21_fix) %in% missing){
  TRE5_21_fix$WC_15cm[nrow(data)] <- tail(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21_fix$WC_15cm[idx] <- (TRE5_21_fix$WC_15cm[r$starts[i]] + TRE5_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE5_21_early <- filter(TRE5_21, Date_time < "2021-05-24 10:00:01")
TRE5_21_late <- filter(TRE5_21, Date_time > "2021-05-30 20:00:01")
TRE5_21 <- bind_rows(TRE5_21_early, TRE5_21_late, TRE5_21_fix)

#Subset and remove glitches
#=====================================================================
TRE5_21_fix <- filter(TRE5_21, Date_time > "2021-06-07 10:00:01")
TRE5_21_fix <- filter(TRE5_21_fix, Date_time < "2021-07-01 20:00:01")

TRE5_21_fix$WC_15cm[TRE5_21_fix$WC_15cm > 0.30] <- NA
missing <- which(is.na(TRE5_21_fix$WC_15cm))

if(1 %in% missing){
  TRE5_21_fix$WC_15cm[1] <- head(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}
if(nrow(TRE5_21_fix) %in% missing){
  TRE5_21_fix$WC_15cm[nrow(data)] <- tail(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21_fix$WC_15cm[idx] <- (TRE5_21_fix$WC_15cm[r$starts[i]] + TRE5_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE5_21_early <- filter(TRE5_21, Date_time < "2021-06-07 10:00:01")
TRE5_21_late <- filter(TRE5_21, Date_time > "2021-07-01 20:00:01")
TRE5_21 <- bind_rows(TRE5_21_early, TRE5_21_late, TRE5_21_fix)

#Subset and remove glitches
#=====================================================================
TRE5_21_fix <- filter(TRE5_21, Date_time > "2021-06-11 10:00:01")
TRE5_21_fix <- filter(TRE5_21_fix, Date_time < "2021-06-15 20:00:01")

TRE5_21_fix$WC_15cm[TRE5_21_fix$WC_15cm > 0.2] <- NA
missing <- which(is.na(TRE5_21_fix$WC_15cm))

if(1 %in% missing){
  TRE5_21_fix$WC_15cm[1] <- head(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}
if(nrow(TRE5_21_fix) %in% missing){
  TRE5_21_fix$WC_15cm[nrow(data)] <- tail(TRE5_21_fix$WC_15cm[!is.na(TRE5_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21_fix$WC_15cm[idx] <- (TRE5_21_fix$WC_15cm[r$starts[i]] + TRE5_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE5_21_early <- filter(TRE5_21, Date_time < "2021-06-11 10:00:01")
TRE5_21_late <- filter(TRE5_21, Date_time > "2021-06-15 20:00:01")
TRE5_21 <- bind_rows(TRE5_21_early, TRE5_21_late, TRE5_21_fix)

#30 cm 
################################################################
TRE5_21$WC_30cm[TRE5_21$WC_30cm > 0.6] <- NA
missing <- which(is.na(TRE5_21$WC_30cm))

if(1 %in% missing){
  TRE5_21$WC_30cm[1] <- head(TRE5_21$WC_30cm[!is.na(TRE5_21$WC_30cm)],1)
}
if(nrow(TRE5_21) %in% missing){
  TRE5_21$WC_30cm[nrow(data)] <- tail(TRE5_21$WC_30cm[!is.na(TRE5_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21$WC_30cm[idx] <- (TRE5_21$WC_30cm[r$starts[i]] + TRE5_21$WC_30cm[r$ends[i]])/2
}

TRE5_21$WC_30cm[TRE5_21$WC_30cm < 0.22] <- NA
missing <- which(is.na(TRE5_21$WC_30cm))

if(1 %in% missing){
  TRE5_21$WC_30cm[1] <- head(TRE5_21$WC_30cm[!is.na(TRE5_21$WC_30cm)],1)
}
if(nrow(TRE5_21) %in% missing){
  TRE5_21$WC_30cm[nrow(data)] <- tail(TRE5_21$WC_30cm[!is.na(TRE5_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21$WC_30cm[idx] <- (TRE5_21$WC_30cm[r$starts[i]] + TRE5_21$WC_30cm[r$ends[i]])/2
}


#Subset to get rid of drips
#==========================================================================
TRE5_21_fix <- filter(TRE5_21, Date_time > "2021-07-15 1:00:01")

TRE5_21_fix$WC_30cm[TRE5_21_fix$WC_30cm > 0.29] <- NA
missing <- which(is.na(TRE5_21_fix$WC_30cm))

if(1 %in% missing){
  TRE5_21_fix$WC_30cm[1] <- head(TRE5_21_fix$WC_30cm[!is.na(TRE5_21_fix$WC_30cm)],1)
}
if(nrow(TRE5_21_fix) %in% missing){
  TRE5_21_fix$WC_30cm[nrow(data)] <- tail(TRE5_21_fix$WC_30cm[!is.na(TRE5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21_fix$WC_30cm[idx] <- (TRE5_21_fix$WC_30cm[r$starts[i]] + TRE5_21_fix$WC_30cm[r$ends[i]])/2
}

#Fix glitches
#===================================================================================
TRE5_21_fix$WC_30cm[TRE5_21_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(TRE5_21_fix$WC_30cm))

if(1 %in% missing){
  TRE5_21_fix$WC_30cm[1] <- head(TRE5_21_fix$WC_30cm[!is.na(TRE5_21_fix$WC_30cm)],1)
}
if(nrow(TRE5_21_fix) %in% missing){
  TRE5_21_fix$WC_30cm[nrow(data)] <- tail(TRE5_21_fix$WC_30cm[!is.na(TRE5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21_fix$WC_30cm[idx] <- (TRE5_21_fix$WC_30cm[r$starts[i]] + TRE5_21_fix$WC_30cm[r$ends[i]])/2
}

TRE5_21_early <- filter(TRE5_21, Date_time < "2021-07-15 1:00:01")
TRE5_21 <- bind_rows(TRE5_21_fix, TRE5_21_early)

#Subset and remove glitches
#==========================================================
TRE5_21_fix <- filter(TRE5_21, Date_time > "2021-07-20 14:00:01")
TRE5_21_fix <- filter(TRE5_21_fix, Date_time < "2021-07-25 20:00:01")

TRE5_21_fix$WC_30cm[TRE5_21_fix$WC_30cm > 0] <- NA

#Recombine
TRE5_21_early <- filter(TRE5_21, Date_time < "2021-07-20 14:00:01")
TRE5_21_late <- filter(TRE5_21, Date_time > "2021-07-25 20:00:01")
TRE5_21 <- bind_rows(TRE5_21_early, TRE5_21_late, TRE5_21_fix)

#100 cm 
################################################################
TRE5_21$WC_100cm[TRE5_21$WC_100cm < 0] <- NA
missing <- which(is.na(TRE5_19$WC_100cm))

if(1 %in% missing){
  TRE5_21$WC_100cm[1] <- head(TRE5_21$WC_100cm[!is.na(TRE5_21$WC_100cm)],1)
}
if(nrow(TRE5_21) %in% missing){
  TRE5_21$WC_100cm[nrow(data)] <- tail(TRE5_21$WC_100cm[!is.na(TRE5_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE5_21$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE5_21$WC_100cm[idx] <- (TRE5_21$WC_100cm[r$starts[i]] + TRE5_21$WC_100cm[r$ends[i]])/2
}

#Missing Dates and Glitches 
#########################################################################
TRE5_21_fix <- filter(TRE5_21, Date_time > "2021-07-04 14:00:01")
TRE5_21_fix <- filter(TRE5_21_fix, Date_time < "2021-07-08 20:00:01")

TRE5_21_fix$WC_100cm[TRE5_21_fix$WC_100cm > 0] <- NA
TRE5_21_fix$WC_30cm[TRE5_21_fix$WC_30cm > 0] <- NA
TRE5_21_fix$WC_15cm[TRE5_21_fix$WC_15cm > 0] <- NA

#Recombine
TRE5_21_early <- filter(TRE5_21, Date_time < "2021-07-04 14:00:01")
TRE5_21_late <- filter(TRE5_21, Date_time > "2021-07-08 20:00:01")
TRE5_21 <- bind_rows(TRE5_21_early, TRE5_21_late, TRE5_21_fix)

#Replace missing dates with NAs - 07/08 to 07/23
#========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 14, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-07-09"), as.Date("2021-07-22"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE5_21 <- insertRows(TRE5_21, c(27071:27084), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE5_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
TRE5_clean <- merge(TRE5_18, TRE5_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
TRE5_clean <- merge(TRE5_clean, TRE5_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
TRE5_clean <- merge(TRE5_clean, TRE5_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
TRE5_clean <- merge(TRE5_clean, TRE5_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(TRE5_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("TRE5", width = 4500, height = 2500)

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
write.csv(TRE5_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE5_clean.csv" ) #this writes a csv file and sends it to the working folder
