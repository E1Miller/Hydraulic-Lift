#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 01/30/2023
#Description: QA/QC TRE 1

#IF THIS ONE STOPS WORKING THEN, IT MEANS NEED TO FIX THE T10303 

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
                        pattern=glob2rx("Copy of T1M_*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Remove additional columns 
data19_21 <- data19_21[c(1:6)]

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
TRE1_2019_2021 <- data19_21 %>%
  rename_all(funs(c("Date", "Time", "PAR", "WC_15cm", "WC_30cm","WC_100cm", "Date_time"))) %>% #this renames all of the columns
  select(Date_time, PAR, WC_15cm, WC_30cm, WC_100cm) #this keeps only the columns you will use

TRE1_2019_2021 <- TRE1_2019_2021[c(1:132548), ]

#2017-2019 FILES
#========================================================================================================================
#If wanting to merge the files, needed to manually delete the extra column in W1M190111 and 
#extra row at the top, and needed to manually delete the extra columns in W1M171228, W1M180201, and W1M180302

#Set the data path 
data_path <- "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new" 
new_col_name <- c("Date_time", "PAR", "WC_15cm", "WC_30cm", "WC_100cm")

#Call in all the files in this folder with the W2M1 pattern, which excludes the datasheets from 2019-2020
files <- dir(data_path, pattern=glob2rx("Copy of T1M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
TRE1_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
TRE1 <- rbind(TRE1_2017_2019, TRE1_2019_2021)

#Write the csv
write.csv(TRE1,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE1.csv") #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(TRE1)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
TRE1$Date <- mdy_hms(TRE1$Date_time)

#Put year into a separate column 
TRE1 <- separate(TRE1, Date, c("Year"))

#TRE1 2017
##################################################################################################
TRE1_17 <- subset(TRE1, Year == '2017') #Subset based on year 

#Plotting 
TRE1_17$WC_15cm <- as.numeric(TRE1_17$WC_15cm)
TRE1_17$WC_30cm <- as.numeric(TRE1_17$WC_30cm)
TRE1_17$WC_100cm <- as.numeric(TRE1_17$WC_100cm)
TRE1_17$Date_time<- mdy_hms(TRE1_17$Date_time)

#15 cm 
################################################################
#Remove weird drop at the start 
TRE1_17$WC_15cm[TRE1_17$WC_15cm < 0.3134] <- NA

Soil <- ggplot(data = subset(TRE1_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE1 2018
##################################################################################################
TRE1_18 <- subset(TRE1, Year == '2018')

#Plotting 
TRE1_18$WC_15cm <- as.numeric(TRE1_18$WC_15cm)
TRE1_18$WC_30cm <- as.numeric(TRE1_18$WC_30cm)
TRE1_18$WC_100cm <- as.numeric(TRE1_18$WC_100cm)
TRE1_18$Date_time<- mdy_hms(TRE1_18$Date_time)

#15 cm
#################################################################################################

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-10-29 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-12-02 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.28] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-10-29 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-12-02 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-03-02 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-03-15 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.321] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-03-02 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-03-15 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-03-15 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-03-26 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.3217] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-03-15 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-03-26 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-03-26 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-04-09 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.32] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-03-26 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-04-09 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-04-11 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-04-12 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.326] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-04-11 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-04-12 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-04-12 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-04-17 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.326] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-04-12 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-04-17 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-01-07 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-01-10 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.3185] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-01-07 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-01-10 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-01-24 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-01-26 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.3219] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-01-24 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-01-26 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-02-20 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-03-01 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.319] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-02-20 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-03-01 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-03-05 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-03-08 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.3216] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-03-05 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-03-08 01:00:01")

TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-03-08 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-03-10 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.3225] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-03-08 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-03-10 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-03-13 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-03-17 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.3243] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-03-13 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-03-17 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Calibrate the dip in June
#=================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-06-12 14:40:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-06-19 05:10:01")
  #Need to go ahead 5 hours to get the plot within the right time frame

#The glitch was it went down by 0.055
TRE1_18_fix$WC_15cm <- (TRE1_18_fix$WC_15cm) + 0.0055

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-06-12 19:40:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-06-19 05:10:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Calibrate the dip in July
#=================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-07-06 04:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-07-18 03:00:01")
#Need to go ahead 5 hours to get the plot within the right time frame

TRE1_18_fix$WC_15cm <- (TRE1_18_fix$WC_15cm) + 0.0105

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-07-06 04:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-07-18 03:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Calibrate the second dip in July 
#=================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-07-18 03:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-08-24 03:30:01")
#Need to go ahead 5 hours to get the plot within the right time frame

TRE1_18_fix$WC_15cm <- (TRE1_18_fix$WC_15cm) + 0.0159

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-07-18 03:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-08-24 03:30:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Calibrate October dip 
#=================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-09-29 05:10:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-10-01 16:30:01")

TRE1_18_fix$WC_15cm <- (TRE1_18_fix$WC_15cm) + 0.004

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-09-29 05:10:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-10-01 16:30:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix new drips from calibrating 
#===================================================================
#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-06-11 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-06-21 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.308] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-06-11 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-06-21 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-06-21 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-07-09 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.297] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-06-21 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-07-09 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 15 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-08-13 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-08-29 01:00:01")

TRE1_18_fix$WC_15cm[TRE1_18_fix$WC_15cm < 0.2725 | TRE1_18_fix$WC_15cm > 0.2777] <- NA
missing <- which(is.na(TRE1_18_fix$WC_15cm))

if(1 %in% missing){
  TRE1_18_fix$WC_15cm[1] <- head(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_15cm[nrow(data)] <- tail(TRE1_18_fix$WC_15cm[!is.na(TRE1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_15cm[idx] <- (TRE1_18_fix$WC_15cm[r$starts[i]] + TRE1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-08-13 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-08-29 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#30 cm 
#################################################################################################
TRE1_18$WC_30cm[TRE1_18$WC_30cm < 0] <- NA
missing <- which(is.na(TRE1_18$WC_30cm))

if(1 %in% missing){
  TRE1_18$WC_30cm[1] <- head(TRE1_18$WC_30cm[!is.na(TRE1_18$WC_30cm)],1)
}
if(nrow(TRE1_18) %in% missing){
  TRE1_18$WC_30cm[nrow(data)] <- tail(TRE1_18$WC_30cm[!is.na(TRE1_18$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18$WC_30cm[idx] <- (TRE1_18$WC_30cm[r$starts[i]] + TRE1_18$WC_30cm[r$ends[i]])/2
}

#Fix 30 cm drips 
#=================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-05-14 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-06-17 01:00:01")

TRE1_18_fix$WC_30cm[TRE1_18_fix$WC_30cm < 0.341] <- NA
missing <- which(is.na(TRE1_18_fix$WC_30cm))

if(1 %in% missing){
  TRE1_18_fix$WC_30cm[1] <- head(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_30cm[nrow(data)] <- tail(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_30cm[idx] <- (TRE1_18_fix$WC_30cm[r$starts[i]] + TRE1_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-05-14 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-06-17 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 30 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-06-18 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-06-20 01:00:01")

TRE1_18_fix$WC_30cm[TRE1_18_fix$WC_30cm < 0.340] <- NA
missing <- which(is.na(TRE1_18_fix$WC_30cm))

if(1 %in% missing){
  TRE1_18_fix$WC_30cm[1] <- head(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_30cm[nrow(data)] <- tail(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_30cm[idx] <- (TRE1_18_fix$WC_30cm[r$starts[i]] + TRE1_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-06-18 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-06-20 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 30 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-11-15 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-11-20 01:00:01")

TRE1_18_fix$WC_30cm[TRE1_18_fix$WC_30cm < 0.29] <- NA
missing <- which(is.na(TRE1_18_fix$WC_30cm))

if(1 %in% missing){
  TRE1_18_fix$WC_30cm[1] <- head(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_30cm[nrow(data)] <- tail(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_30cm[idx] <- (TRE1_18_fix$WC_30cm[r$starts[i]] + TRE1_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-11-15 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-11-20 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 30 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-10-26 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-11-01 01:00:01")

TRE1_18_fix$WC_30cm[TRE1_18_fix$WC_30cm < 0.2945] <- NA
missing <- which(is.na(TRE1_18_fix$WC_30cm))

if(1 %in% missing){
  TRE1_18_fix$WC_30cm[1] <- head(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_30cm[nrow(data)] <- tail(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_30cm[idx] <- (TRE1_18_fix$WC_30cm[r$starts[i]] + TRE1_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-10-26 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-11-01 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 30 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-11-29 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-12-02 01:00:01")

TRE1_18_fix$WC_30cm[TRE1_18_fix$WC_30cm < 0.3557] <- NA
missing <- which(is.na(TRE1_18_fix$WC_30cm))

if(1 %in% missing){
  TRE1_18_fix$WC_30cm[1] <- head(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_30cm[nrow(data)] <- tail(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_30cm[idx] <- (TRE1_18_fix$WC_30cm[r$starts[i]] + TRE1_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-11-29 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-12-02 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 30 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-12-09 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-12-31 01:00:01")

TRE1_18_fix$WC_30cm[TRE1_18_fix$WC_30cm < 0.3435] <- NA
missing <- which(is.na(TRE1_18_fix$WC_30cm))

if(1 %in% missing){
  TRE1_18_fix$WC_30cm[1] <- head(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_30cm[nrow(data)] <- tail(TRE1_18_fix$WC_30cm[!is.na(TRE1_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_30cm[idx] <- (TRE1_18_fix$WC_30cm[r$starts[i]] + TRE1_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-12-09 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-12-31 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Calibrate drip in November
#================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-11-20 16:10:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-11-22 09:40:01")

Soil <- ggplot(data = subset(TRE1_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

TRE1_18_fix$WC_30cm <- TRE1_18_fix$WC_30cm + 0.0034

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-11-20 16:10:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-11-22 09:40:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#100 cm 
####################################################################################

#Fix 100 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-03-24 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-03-29 01:00:01")

TRE1_18_fix$WC_100cm[TRE1_18_fix$WC_100cm < 0.456] <- NA
missing <- which(is.na(TRE1_18_fix$WC_100cm))

if(1 %in% missing){
  TRE1_18_fix$WC_100cm[1] <- head(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_100cm[nrow(data)] <- tail(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_100cm[idx] <- (TRE1_18_fix$WC_100cm[r$starts[i]] + TRE1_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-03-24 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-03-29 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 100 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-01-23 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-01-25 01:00:01")

TRE1_18_fix$WC_100cm[TRE1_18_fix$WC_100cm < 0.45] <- NA
missing <- which(is.na(TRE1_18_fix$WC_100cm))

if(1 %in% missing){
  TRE1_18_fix$WC_100cm[1] <- head(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_100cm[nrow(data)] <- tail(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_100cm[idx] <- (TRE1_18_fix$WC_100cm[r$starts[i]] + TRE1_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-01-23 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-01-25 01:00:01")

TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 100 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-04-01 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-04-25 01:00:01")

TRE1_18_fix$WC_100cm[TRE1_18_fix$WC_100cm < 0.4425] <- NA
missing <- which(is.na(TRE1_18_fix$WC_100cm))

if(1 %in% missing){
  TRE1_18_fix$WC_100cm[1] <- head(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_100cm[nrow(data)] <- tail(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_100cm[idx] <- (TRE1_18_fix$WC_100cm[r$starts[i]] + TRE1_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-04-01 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-04-25 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 100 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-03-16 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-03-25 01:00:01")

TRE1_18_fix$WC_100cm[TRE1_18_fix$WC_100cm < 0.457] <- NA
missing <- which(is.na(TRE1_18_fix$WC_100cm))

if(1 %in% missing){
  TRE1_18_fix$WC_100cm[1] <- head(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_100cm[nrow(data)] <- tail(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_100cm[idx] <- (TRE1_18_fix$WC_100cm[r$starts[i]] + TRE1_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-03-16 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-03-25 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Fix 100 cm drips 
#=======================================================================================================
TRE1_18_fix <- filter(TRE1_18, Date_time > "2018-04-09 1:00:01")
TRE1_18_fix <- filter(TRE1_18_fix, Date_time < "2018-04-16 01:00:01")

TRE1_18_fix$WC_100cm[TRE1_18_fix$WC_100cm < 0.459] <- NA
missing <- which(is.na(TRE1_18_fix$WC_100cm))

if(1 %in% missing){
  TRE1_18_fix$WC_100cm[1] <- head(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}
if(nrow(TRE1_18_fix) %in% missing){
  TRE1_18_fix$WC_100cm[nrow(data)] <- tail(TRE1_18_fix$WC_100cm[!is.na(TRE1_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_18_fix$WC_100cm[idx] <- (TRE1_18_fix$WC_100cm[r$starts[i]] + TRE1_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
TRE1_18_early <- filter(TRE1_18, Date_time < "2018-04-09 1:00:01")
TRE1_18_late <- filter(TRE1_18, Date_time > "2018-04-16 01:00:01")
TRE1_18 <- bind_rows(TRE1_18_early, TRE1_18_fix, TRE1_18_late)

#Plot again 
Soil <- ggplot(data = subset(TRE1_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE1 2019
##################################################################################################
TRE1_19 <- subset(TRE1, Year == '2019')

#Plotting 
TRE1_19$WC_15cm <- as.numeric(TRE1_19$WC_15cm)
TRE1_19$WC_30cm <- as.numeric(TRE1_19$WC_30cm)
TRE1_19$WC_100cm <- as.numeric(TRE1_19$WC_100cm)
TRE1_19$Date_time<- mdy_hms(TRE1_19$Date_time)

#15 cm
###########################################
TRE1_19$WC_15cm[TRE1_19$WC_15cm > 0.6] <- NA
missing <- which(is.na(TRE1_19$WC_15cm))

if(1 %in% missing){
  TRE1_19$WC_15cm[1] <- head(TRE1_19$WC_15cm[!is.na(TRE1_19$WC_15cm)],1)
}
if(nrow(TRE1_19) %in% missing){
  TRE1_19$WC_15cm[nrow(data)] <- tail(TRE1_19$WC_15cm[!is.na(TRE1_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19$WC_15cm[idx] <- (TRE1_19$WC_15cm[r$starts[i]] + TRE1_19$WC_15cm[r$ends[i]])/2
}

#Get rid of spike
#========================================================================
TRE1_19$WC_15cm[TRE1_19$WC_15cm < 0.276] <- NA
missing <- which(is.na(TRE1_19$WC_15cm))

if(1 %in% missing){
  TRE1_19$WC_15cm[1] <- head(TRE1_19$WC_15cm[!is.na(TRE1_19$WC_15cm)],1)
}
if(nrow(TRE1_19) %in% missing){
  TRE1_19$WC_15cm[nrow(data)] <- tail(TRE1_19$WC_15cm[!is.na(TRE1_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19$WC_15cm[idx] <- (TRE1_19$WC_15cm[r$starts[i]] + TRE1_19$WC_15cm[r$ends[i]])/2
}

#Fix 15 cm drips in mid-July
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-07-15 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-08-08 10:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.3] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}


#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-07-15 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-08-08 10:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)


#Fix 15 cm drips in mid-July
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-07-17 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-08-08 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.2936] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-07-17 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-08-08 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-05-04 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-05-15 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.3276] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-05-04 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-05-15 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in mid-July
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-07-27 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-08-08 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.292] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-07-27 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-08-08 00:00:01")

TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-10-26 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-11-15 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.286] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-10-26 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-11-15 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-06-03 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-06-15 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.3321] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-06-03 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-06-15 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-06-05 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-06-15 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.3315] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-06-05 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-06-15 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-06-11 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-06-15 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.326] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-06-11 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-06-15 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-07-05 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-07-10 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.308] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-07-05 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-07-10 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-07-08 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-07-10 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.307] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-07-08 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-07-10 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-07-10 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-07-15 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.304] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-07-10 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-07-15 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-07-30 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-08-15 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.291] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-07-30 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-08-15 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-08-01 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-08-05 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.2896] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-08-01 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-08-05 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-08-07 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-08-10 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.2875] <- NA
missing <- which(is.na(TRE1_19_fix$WC_15cm))

if(1 %in% missing){
  TRE1_19_fix$WC_15cm[1] <- head(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_15cm[nrow(data)] <- tail(TRE1_19_fix$WC_15cm[!is.na(TRE1_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_15cm[idx] <- (TRE1_19_fix$WC_15cm[r$starts[i]] + TRE1_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-08-07 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-08-05 10:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Fix 15 cm drips in May
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-07-01 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-07-25 00:00:01")

TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm == 0.2935] <- NA

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-07-01 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-07-25 00:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Remove glitches in 15 cm
#=============================================================
TRE1_19$WC_15cm[TRE1_19$WC_15cm == 0.3505] <- NA

#30 cm 
################################################################
TRE1_19$WC_30cm[TRE1_19$WC_30cm < 0.285] <- NA
missing <- which(is.na(TRE1_19$WC_30cm))

if(1 %in% missing){
  TRE1_19$WC_30cm[1] <- head(TRE1_19$WC_30cm[!is.na(TRE1_19$WC_30cm)],1)
}
if(nrow(TRE1_19) %in% missing){
  TRE1_19$WC_30cm[nrow(data)] <- tail(TRE1_19$WC_30cm[!is.na(TRE1_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19$WC_30cm[idx] <- (TRE1_19$WC_30cm[r$starts[i]] + TRE1_19$WC_30cm[r$ends[i]])/2
}

#Fix 30 cm drips in mid-March
#=========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-03-04 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-03-30 01:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.32] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-03-04 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-03-30 01:00:01")
TRE1_19 <- bind_rows(TRE1_19_late, TRE1_19_fix, TRE1_19_early)

#Get rid of late year 30 cm drips
#========================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-12-01 1:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.335] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Do increases 
TRE1_19_fix <- TRE1_19_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================


#Make increase column not a percent 
TRE1_19_fix <- transform(TRE1_19_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

TRE1_19_fix <- transform(TRE1_19_fix, WC_30cm=ifelse(incr < -0.01, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/9, 9), sides=2)), 
                                                     WC_30cm))





#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-12-01 1:00:01")

TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early)

#Fix earlier part of December 
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-12-01 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-12-04 1:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.344] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-12-01 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-12-04 1:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix October/November
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-10-01 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-10-14 15:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.288] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-10-01 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-10-14 15:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix October/November
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-10-14 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-10-31 15:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.287] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-10-14 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-10-31 15:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix October/November
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-10-31 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-11-11 15:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.288] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-10-31 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-11-11 15:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix October/November
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-10-31 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-11-11 15:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.288] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-10-31 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-11-11 15:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix October/November
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-11-11 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-11-18 15:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.2875] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-11-11 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-11-18 15:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix October/November
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-11-18 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-11-24 15:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.2875] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-11-18 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-11-23 15:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix October/November
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-11-23 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-11-24 12:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.2873] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-11-23 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-11-24 12:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix October/November
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-12-23 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-12-26 12:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.340] <- NA

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-12-23 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-12-26 12:00:01")

TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix October/November
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-12-30 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-12-31 12:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.34] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-12-30 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-12-31 12:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix later part of December 
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-12-30 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-12-30 15:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.348] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-12-30 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-12-30 15:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix later part of December 
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-06-30 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-07-21 15:00:01")

TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm < 0.315] <- NA
missing <- which(is.na(TRE1_19_fix$WC_30cm))

if(1 %in% missing){
  TRE1_19_fix$WC_30cm[1] <- head(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_30cm[nrow(data)] <- tail(TRE1_19_fix$WC_30cm[!is.na(TRE1_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_30cm[idx] <- (TRE1_19_fix$WC_30cm[r$starts[i]] + TRE1_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-06-30 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time >  "2019-07-21 15:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#100 cm 
################################################################
TRE1_19$WC_100cm[TRE1_19$WC_100cm > 0.7183] <- NA
missing <- which(is.na(TRE1_19$WC_100cm))

if(1 %in% missing){
  TRE1_19$WC_100cm[1] <- head(TRE1_19$WC_100cm[!is.na(TRE1_19$WC_100cm)],1)
}
if(nrow(TRE1_19) %in% missing){
  TRE1_19$WC_100cm[nrow(data)] <- tail(TRE1_19$WC_100cm[!is.na(TRE1_19$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19$WC_100cm[idx] <- (TRE1_19$WC_100cm[r$starts[i]] + TRE1_19$WC_100cm[r$ends[i]])/2
}

#Fix drip in July
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-06-30 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-07-30 15:00:01")

Soil <- ggplot(data = subset(TRE1_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil 

TRE1_19_fix$WC_100cm[TRE1_19_fix$WC_100cm < 0.4335] <- NA
missing <- which(is.na(TRE1_19_fix$WC_100cm))

if(1 %in% missing){
  TRE1_19_fix$WC_100cm[1] <- head(TRE1_19_fix$WC_100cm[!is.na(TRE1_19_fix$WC_100cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_100cm[nrow(data)] <- tail(TRE1_19_fix$WC_100cm[!is.na(TRE1_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_100cm[idx] <- (TRE1_19_fix$WC_100cm[r$starts[i]] + TRE1_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-06-30 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time >  "2019-07-30 15:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Remove 100 cm glitch at the end of the year
#====================================================================
TRE1_19$WC_100cm[TRE1_19$WC_100cm == 0.42695] <- NA

#Calibrate 
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-11-02 15:40:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-12-31 23:50:01")

TRE1_19_fix$WC_100cm <- TRE1_19_fix$WC_100cm + 0.011

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-11-02 15:40:01")
TRE1_19_late <- filter(TRE1_19, Date_time >  "2019-12-31 23:50:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix drip in July
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-10-30 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-11-03 10:00:01")

TRE1_19_fix$WC_100cm[TRE1_19_fix$WC_100cm < 0.4182] <- NA
missing <- which(is.na(TRE1_19_fix$WC_100cm))

if(1 %in% missing){
  TRE1_19_fix$WC_100cm[1] <- head(TRE1_19_fix$WC_100cm[!is.na(TRE1_19_fix$WC_100cm)],1)
}
if(nrow(TRE1_19_fix) %in% missing){
  TRE1_19_fix$WC_100cm[nrow(data)] <- tail(TRE1_19_fix$WC_100cm[!is.na(TRE1_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_19_fix$WC_100cm[idx] <- (TRE1_19_fix$WC_100cm[r$starts[i]] + TRE1_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-10-30 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time >  "2019-11-03 10:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Fix drip in July
#======================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-06-30 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-07-26 10:00:01")

TRE1_19_fix$WC_100cm[TRE1_19_fix$WC_100cm > 0.438] <- NA

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-06-30 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time >  "2019-07-26 10:00:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Remove 08/08 and 08/09 due to glitches
#=============================================================
TRE1_19<- TRE1_19[-c(5621:5687), ]

#Calibration for July 
#===================================================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-07-21 08:50:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-08-08 09:10:01")

TRE1_19_fix$WC_100cm <- TRE1_19_fix$WC_100cm + 0.0062

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-07-21 08:50:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-08-08 09:10:01")
TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Missing dates and glitches before dates
##############################################################################

#Remove glitches before the missing 08/09 dates
#===============================================================
TRE1_19_fix <- filter(TRE1_19, Date_time > "2019-08-07 1:00:01")
TRE1_19_fix <- filter(TRE1_19_fix, Date_time < "2019-08-09 1:00:01")

TRE1_19_fix$WC_100cm[TRE1_19_fix$WC_100cm > 0.43] <- NA
TRE1_19_fix$WC_30cm[TRE1_19_fix$WC_30cm > 0.303] <- NA
TRE1_19_fix$WC_15cm[TRE1_19_fix$WC_15cm > 0.29] <- NA

#Recombine 
TRE1_19_early <- filter(TRE1_19, Date_time < "2019-08-07 1:00:01")
TRE1_19_late <- filter(TRE1_19, Date_time > "2019-08-09 1:00:01")

TRE1_19 <- bind_rows(TRE1_19_fix, TRE1_19_early, TRE1_19_late)

#Remove the missing dates
#=========================================================================
#Replace missing dates with NAs - 08/09 to 09/26
insertDF <- as.data.frame(matrix(data = NA, nrow = 48, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-08-09"), as.Date("2019-09-25"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_19 <- insertRows(TRE1_19, c(5621:5669), new = insertDF)

#Replace missing dates with NAs - 07/15 to 07/24
insertDF <- as.data.frame(matrix(data = NA, nrow = 8, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-07-16"), as.Date("2019-07-23"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_19 <- insertRows(TRE1_19, c(11823:11830), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE1_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE1 2020
##################################################################################################
TRE1_20 <- subset(TRE1, Year == '2020')

#Plotting 
TRE1_20$WC_15cm <- as.numeric(TRE1_20$WC_15cm)
TRE1_20$WC_30cm <- as.numeric(TRE1_20$WC_30cm)
TRE1_20$WC_100cm <- as.numeric(TRE1_20$WC_100cm)
TRE1_20$Date_time<- mdy_hms(TRE1_20$Date_time)

#15 cm
#######################################################################

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-03-01 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-03-09 12:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.303] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-03-01 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-03-09 12:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-05-01 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-13 12:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.298] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-05-01 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-13 12:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-05-11 12:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-13 12:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.3085] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-05-11 12:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-13 12:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-05-15 00:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-18 12:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.315] <- NA

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-05-15 00:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-18 12:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-05-17 00:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-19 04:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.328] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-05-17 00:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-19 04:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-05-19 00:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-29 04:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm > 0.33] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-05-19 00:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-29 04:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-11-01 00:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-11-15 04:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.23] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-11-01 00:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-11-15 04:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-11-06 00:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-11-07 04:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.2538] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-11-06 00:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-11-07 04:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-11-09 00:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-11-16 04:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.24] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-11-09 00:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-11-16 04:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-11-14 00:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-11-16 04:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.275] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-11-14 00:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-11-16 04:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-11-16 20:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-11-19 04:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.288] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-11-16 20:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-11-19 04:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix drips 
#=======================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-12-07 20:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-12-14 04:00:01")

TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm < 0.295] <- NA
missing <- which(is.na(TRE1_20_fix$WC_15cm))

if(1 %in% missing){
  TRE1_20_fix$WC_15cm[1] <- head(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_15cm[nrow(data)] <- tail(TRE1_20_fix$WC_15cm[!is.na(TRE1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_15cm[idx] <- (TRE1_20_fix$WC_15cm[r$starts[i]] + TRE1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-12-07 20:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-12-14 04:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)


#30 cm 
################################################################
TRE1_20$WC_30cm[TRE1_20$WC_30cm < 0.26] <- NA
missing <- which(is.na(TRE1_20$WC_30cm))

if(1 %in% missing){
  TRE1_20$WC_30cm[1] <- head(TRE1_20$WC_30cm[!is.na(TRE1_20$WC_30cm)],1)
}
if(nrow(TRE1_20) %in% missing){
  TRE1_20$WC_30cm[nrow(data)] <- tail(TRE1_20$WC_30cm[!is.na(TRE1_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20$WC_30cm[idx] <- (TRE1_20$WC_30cm[r$starts[i]] + TRE1_20$WC_30cm[r$ends[i]])/2
}

#Fix 30 cm dips in beginning of year
#=====================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time < "2020-04-01 1:00:01")

TRE1_20_fix$WC_30cm[TRE1_20_fix$WC_30cm < 0.324] <- NA
missing <- which(is.na(TRE1_20_fix$WC_30cm))

if(1 %in% missing){
  TRE1_20_fix$WC_30cm[1] <- head(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_30cm[nrow(data)] <- tail(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_30cm[idx] <- (TRE1_20_fix$WC_30cm[r$starts[i]] + TRE1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-04-01 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late)

#Fix 30 cm dips in beginning of year
#=====================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-02-01 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-02-17 12:00:01")

TRE1_20_fix$WC_30cm[TRE1_20_fix$WC_30cm < 0.3355] <- NA
missing <- which(is.na(TRE1_20_fix$WC_30cm))

if(1 %in% missing){
  TRE1_20_fix$WC_30cm[1] <- head(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_30cm[nrow(data)] <- tail(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_30cm[idx] <- (TRE1_20_fix$WC_30cm[r$starts[i]] + TRE1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-02-01 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-02-17 12:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix 30 cm dips in beginning of year
#=====================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-02-17 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-02-27 12:00:01")

TRE1_20_fix$WC_30cm[TRE1_20_fix$WC_30cm < 0.331] <- NA
missing <- which(is.na(TRE1_20_fix$WC_30cm))

if(1 %in% missing){
  TRE1_20_fix$WC_30cm[1] <- head(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_30cm[nrow(data)] <- tail(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_30cm[idx] <- (TRE1_20_fix$WC_30cm[r$starts[i]] + TRE1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-02-17 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-02-27 12:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix 30 cm dips in beginning of year
#=====================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-08-17 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-09-07 12:00:01")

TRE1_20_fix$WC_30cm[TRE1_20_fix$WC_30cm < 0.2825] <- NA
missing <- which(is.na(TRE1_20_fix$WC_30cm))

if(1 %in% missing){
  TRE1_20_fix$WC_30cm[1] <- head(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_30cm[nrow(data)] <- tail(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_30cm[idx] <- (TRE1_20_fix$WC_30cm[r$starts[i]] + TRE1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-08-17 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-09-07 12:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix 30 cm dips in beginning of year
#=====================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-02-17 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-03-01 12:00:01")

TRE1_20_fix$WC_30cm[TRE1_20_fix$WC_30cm < 0.332] <- NA
missing <- which(is.na(TRE1_20_fix$WC_30cm))

if(1 %in% missing){
  TRE1_20_fix$WC_30cm[1] <- head(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_30cm[nrow(data)] <- tail(TRE1_20_fix$WC_30cm[!is.na(TRE1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_30cm[idx] <- (TRE1_20_fix$WC_30cm[r$starts[i]] + TRE1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-02-17 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-03-01 12:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#Fix 30 cm dips in beginning of year
#=====================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-03-02 22:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-04-24 12:00:01")

TRE1_20_fix$WC_30cm[TRE1_20_fix$WC_30cm > 0] <- NA

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-03-02 22:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-04-24 12:00:01")
TRE1_20 <- bind_rows(TRE1_20_fix, TRE1_20_late, TRE1_20_early)

#100 cm 
################################################################
TRE1_20$WC_100cm[TRE1_20$WC_100cm < 0.26] <- NA
missing <- which(is.na(TRE1_19$WC_100cm))

if(1 %in% missing){
  TRE1_20$WC_100cm[1] <- head(TRE1_20$WC_100cm[!is.na(TRE1_20$WC_100cm)],1)
}
if(nrow(TRE1_20) %in% missing){
  TRE1_20$WC_100cm[nrow(data)] <- tail(TRE1_20$WC_100cm[!is.na(TRE1_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20$WC_100cm[idx] <- (TRE1_20$WC_100cm[r$starts[i]] + TRE1_20$WC_100cm[r$ends[i]])/2
}

#Fix weird positive values 
TRE1_20$WC_100cm[TRE1_20$WC_100cm == 0.7184] <- NA
missing <- which(is.na(TRE1_19$WC_100cm))

#Fix 100 cm drips in middle of year
#===========================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-03-01 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-08-04 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.422] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}


#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-03-01 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-08-04 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm drips in middle of year
#===========================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-03-01 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-08-04 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.422] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-03-01 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-08-04 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm dips in October and November
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-10-11 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-11-10 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.2895] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-10-11 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-11-10 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-03-12 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-07-19 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.424] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-03-12 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-07-19 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-03-12 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-07-19 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.424] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-03-12 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-07-19 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-03-17 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-04-19 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.424] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-03-17 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-04-19 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-03-23 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-04-19 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.4244] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-03-23 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-04-19 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-04-19 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-19 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.4254] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-04-19 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-19 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-04-25 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-04-30 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.4265] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-04-25 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-04-30 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-04-30 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-30 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.4265] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-04-30 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-30 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-04-30 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-10 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.42775] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-04-30 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-10 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-05-10 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-20 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.428] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-05-10 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-20 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-05-20 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-05-30 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.42825] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-05-20 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-05-30 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-05-30 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-06-30 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.428] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-05-30 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-06-30 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-06-30 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-07-30 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.428] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-06-30 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-07-30 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-08-17 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-08-30 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.428] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-08-17 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-08-30 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-08-21 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-08-30 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm < 0.425] <- NA
missing <- which(is.na(TRE1_20_fix$WC_100cm))

if(1 %in% missing){
  TRE1_20_fix$WC_100cm[1] <- head(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}
if(nrow(TRE1_20_fix) %in% missing){
  TRE1_20_fix$WC_100cm[nrow(data)] <- tail(TRE1_20_fix$WC_100cm[!is.na(TRE1_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_20_fix$WC_100cm[idx] <- (TRE1_20_fix$WC_100cm[r$starts[i]] + TRE1_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-08-21 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-08-30 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-08-23 15:10:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-08-30 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm > 0] <- NA

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-08-23 15:10:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-08-30 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-02-12 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-02-19 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm > 0.45] <- NA

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-02-12 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-02-19 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Fix 100 cm glitch in February 
#==================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-12-05 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-12-11 16:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm > 0.2] <- NA

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-12-05 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-12-11 16:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Missing dates and glitches
########################################################################

#Remove glitches before missing dates 
#========================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-08-18 1:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-08-21 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm > 0.432] <- NA
TRE1_20_fix$WC_30cm[TRE1_20_fix$WC_30cm > 0.29] <- NA
TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm > 0.25] <- NA

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-08-18 1:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-08-21 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Remove glitches before missing dates 
#========================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-11-27 09:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-11-29 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm > 0] <- NA
TRE1_20_fix$WC_30cm[TRE1_20_fix$WC_30cm > 0] <- NA
TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm > 0] <- NA

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-11-27 09:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-11-29 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Remove glitches before missing dates 
#========================================================================
TRE1_20_fix <- filter(TRE1_20, Date_time > "2020-12-24 19:00:01")
TRE1_20_fix <- filter(TRE1_20_fix, Date_time < "2020-12-26 1:00:01")

TRE1_20_fix$WC_100cm[TRE1_20_fix$WC_100cm > 0] <- NA
TRE1_20_fix$WC_30cm[TRE1_20_fix$WC_30cm > 0] <- NA
TRE1_20_fix$WC_15cm[TRE1_20_fix$WC_15cm > 0] <- NA

#Recombine 
TRE1_20_early <- filter(TRE1_20, Date_time < "2020-12-24 19:00:01")
TRE1_20_late <- filter(TRE1_20, Date_time > "2020-12-26 1:00:01")
TRE1_20 <- bind_rows(TRE1_20_early, TRE1_20_fix, TRE1_20_late)

#Remove missing dates
#========================================================================
#Replace missing dates with NAs - 08/19 to 08/21
insertDF <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-08-20"), as.Date("2020-08-20"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_20 <- insertRows(TRE1_20, c(46451), new = insertDF)

#Replace missing dates with NAs - 11/28 to 12/09
insertDF <- as.data.frame(matrix(data = NA, nrow = 10, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-11-29"), as.Date("2020-12-08"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_20 <- insertRows(TRE1_20, c(44835:44844), new = insertDF)

#Replace missing dates with NAs - 06/30 to 07/30
insertDF <- as.data.frame(matrix(data = NA, nrow = 29, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-07-01"), as.Date("2020-07-29"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_20 <- insertRows(TRE1_20, c(26075:26103), new = insertDF)

#Replace missing dates with NAs - 12/25 to 12/31 
insertDF <- as.data.frame(matrix(data = NA, nrow = 6, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-12-26"), as.Date("2020-12-31"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_20 <- insertRows(TRE1_20, c(47087:47093), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE1_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE1 2021
##################################################################################################
TRE1_21 <- subset(TRE1, Year == '2021')

#Plotting 
TRE1_21$WC_15cm <- as.numeric(TRE1_21$WC_15cm)
TRE1_21$WC_30cm <- as.numeric(TRE1_21$WC_30cm)
TRE1_21$WC_100cm <- as.numeric(TRE1_21$WC_100cm)
TRE1_21$Date_time<- mdy_hms(TRE1_21$Date_time)

#15 cm
###########################################
TRE1_21$WC_15cm[TRE1_21$WC_15cm < 0] <- NA
missing <- which(is.na(TRE1_21$WC_15cm))

if(1 %in% missing){
  TRE1_21$WC_15cm[1] <- head(TRE1_21$WC_15cm[!is.na(TRE1_21$WC_15cm)],1)
}
if(nrow(TRE1_21) %in% missing){
  TRE1_21$WC_15cm[nrow(data)] <- tail(TRE1_21$WC_15cm[!is.na(TRE1_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21$WC_15cm[idx] <- (TRE1_21$WC_15cm[r$starts[i]] + TRE1_21$WC_15cm[r$ends[i]])/2
}

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-07-23 1:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0.3] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-07-23 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-01-11 1:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-01-23 1:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.309] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-01-11 1:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-01-23 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-02-09 1:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-02-15 1:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.312] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-02-09 1:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-02-15 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-02-12 1:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-02-15 1:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.319] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-02-12 1:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-02-15 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-02-15 1:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-02-19 1:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.3175] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-02-15 1:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-02-19 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-03-08 1:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-03-13 1:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.3179] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-03-08 1:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-03-13 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-03-09 02:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-03-09 14:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.321] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-03-09 02:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-03-09 14:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-03-10 10:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-03-11 00:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.3228] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-03-10 10:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-03-11 00:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-06-17 10:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-06-21 00:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0.322] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-06-17 10:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-06-21 00:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-06-30 10:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-07-21 00:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0.315] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-06-30 10:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-07-21 00:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-07-11 10:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-07-21 00:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0.3067] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-07-11 10:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-07-21 00:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-07-14 10:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-07-21 00:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0.304] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-07-14 10:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-07-21 00:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-07-24 10:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-08-21 00:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0.299] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-07-24 10:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-08-21 00:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset 
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time < "2021-01-13 1:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.306] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time > "2021-01-13 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-03-04 10:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-03-07 00:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.32] <- NA

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-03-04 10:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-03-07 00:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-03-04 10:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-03-27 00:00:01")

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm < 0.32] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-03-04 10:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-03-27 00:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#Subset and remove drips
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-04-01 10:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-04-07 00:00:01")

Soil <- ggplot(data = subset(TRE1_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0.312] <- NA
missing <- which(is.na(TRE1_21_fix$WC_15cm))

if(1 %in% missing){
  TRE1_21_fix$WC_15cm[1] <- head(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_15cm[nrow(data)] <- tail(TRE1_21_fix$WC_15cm[!is.na(TRE1_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_15cm[idx] <- (TRE1_21_fix$WC_15cm[r$starts[i]] + TRE1_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-04-01 10:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-04-07 00:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_fix, TRE1_21_late)

#30 cm 
################################################################
TRE1_21$WC_30cm[TRE1_21$WC_30cm < 0] <- NA
missing <- which(is.na(TRE1_21$WC_30cm))

if(1 %in% missing){
  TRE1_21$WC_30cm[1] <- head(TRE1_21$WC_30cm[!is.na(TRE1_21$WC_30cm)],1)
}
if(nrow(TRE1_21) %in% missing){
  TRE1_21$WC_30cm[nrow(data)] <- tail(TRE1_21$WC_30cm[!is.na(TRE1_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21$WC_30cm[idx] <- (TRE1_21$WC_30cm[r$starts[i]] + TRE1_21$WC_30cm[r$ends[i]])/2
}

#100 cm 
################################################################
TRE1_21$WC_100cm[TRE1_21$WC_100cm < 0.3] <- NA
missing <- which(is.na(TRE1_19$WC_100cm))

if(1 %in% missing){
  TRE1_21$WC_100cm[1] <- head(TRE1_21$WC_100cm[!is.na(TRE1_21$WC_100cm)],1)
}
if(nrow(TRE1_21) %in% missing){
  TRE1_21$WC_100cm[nrow(data)] <- tail(TRE1_21$WC_100cm[!is.na(TRE1_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21$WC_100cm[idx] <- (TRE1_21$WC_100cm[r$starts[i]] + TRE1_21$WC_100cm[r$ends[i]])/2
}

#Subset 
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-06-11 1:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-06-15 1:00:01")

TRE1_21_fix$WC_100cm[TRE1_21_fix$WC_100cm < 0.419] <- NA
missing <- which(is.na(TRE1_21_fix$WC_100cm))

if(1 %in% missing){
  TRE1_21_fix$WC_100cm[1] <- head(TRE1_21_fix$WC_100cm[!is.na(TRE1_21_fix$WC_100cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_100cm[nrow(data)] <- tail(TRE1_21_fix$WC_100cm[!is.na(TRE1_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_100cm[idx] <- (TRE1_21_fix$WC_100cm[r$starts[i]] + TRE1_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-06-11 1:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-06-15 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_late, TRE1_21_fix)

#Subset 
#=======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-06-15 1:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-06-25 1:00:01")

TRE1_21_fix$WC_100cm[TRE1_21_fix$WC_100cm < 0.419] <- NA
missing <- which(is.na(TRE1_21_fix$WC_100cm))

if(1 %in% missing){
  TRE1_21_fix$WC_100cm[1] <- head(TRE1_21_fix$WC_100cm[!is.na(TRE1_21_fix$WC_100cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_100cm[nrow(data)] <- tail(TRE1_21_fix$WC_100cm[!is.na(TRE1_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_100cm[idx] <- (TRE1_21_fix$WC_100cm[r$starts[i]] + TRE1_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-06-11 1:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-06-15 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_late, TRE1_21_fix)

#Subset and remove drips
#======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-01-20 1:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-02-25 1:00:01")

TRE1_21_fix$WC_100cm[TRE1_21_fix$WC_100cm < 0.344] <- NA

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-01-20 1:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-02-25 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_late, TRE1_21_fix)

#Subset and remove drips
#======================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-05-20 1:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-07-01 1:00:01")

TRE1_21_fix$WC_100cm[TRE1_21_fix$WC_100cm < 0.42] <- NA
missing <- which(is.na(TRE1_21_fix$WC_100cm))

if(1 %in% missing){
  TRE1_21_fix$WC_100cm[1] <- head(TRE1_21_fix$WC_100cm[!is.na(TRE1_21_fix$WC_100cm)],1)
}
if(nrow(TRE1_21_fix) %in% missing){
  TRE1_21_fix$WC_100cm[nrow(data)] <- tail(TRE1_21_fix$WC_100cm[!is.na(TRE1_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE1_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE1_21_fix$WC_100cm[idx] <- (TRE1_21_fix$WC_100cm[r$starts[i]] + TRE1_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-05-20 1:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-07-01 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_late, TRE1_21_fix)

#Glitches and Missing Dates
#######################################################################

#Get rid of glitch before 01/18 missing dates
#=====================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-01-18 01:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-01-19 1:00:01")

TRE1_21_fix$WC_100cm[TRE1_21_fix$WC_100cm > 0] <- NA
TRE1_21_fix$WC_30cm[TRE1_21_fix$WC_30cm > 0] <- NA
TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0] <- NA

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-01-18 01:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-01-19 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_late, TRE1_21_fix)

#Get rid of glitch before 01/18 missing dates
#=====================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-01-17 01:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-01-19 1:00:01")

TRE1_21_fix$WC_30cm[TRE1_21_fix$WC_30cm > 0.325] <- NA

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-01-17 01:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-01-19 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_late, TRE1_21_fix)

#Get rid of glitch before 01/18 missing dates
#=====================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-02-19 01:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-02-21 1:00:01")

TRE1_21_fix$WC_100cm[TRE1_21_fix$WC_100cm > 0.36] <- NA
TRE1_21_fix$WC_30cm[TRE1_21_fix$WC_30cm > 0.3312] <- NA
TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0.322] <- NA

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-02-19 01:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-02-21 1:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_late, TRE1_21_fix)

#Get rid of glitch before 01/18 missing dates
#=====================================================================
TRE1_21_fix <- filter(TRE1_21, Date_time > "2021-04-07 01:00:01")
TRE1_21_fix <- filter(TRE1_21_fix, Date_time < "2021-04-08 01:00:01")

TRE1_21_fix$WC_100cm[TRE1_21_fix$WC_100cm > 0.35] <- NA
TRE1_21_fix$WC_30cm[TRE1_21_fix$WC_30cm > 0.34] <- NA
TRE1_21_fix$WC_15cm[TRE1_21_fix$WC_15cm > 0.31] <- NA

#Recombine
TRE1_21_early <- filter(TRE1_21, Date_time < "2021-04-07 01:00:01")
TRE1_21_late <- filter(TRE1_21, Date_time > "2021-04-08 01:00:01")
TRE1_21 <- bind_rows(TRE1_21_early, TRE1_21_late, TRE1_21_fix)

#Replace missing dates
#=====================================================================

#Replace missing dates with NAs - 01/18 to 02/08
insertDF <- as.data.frame(matrix(data = NA, nrow = 20, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-01-19"), as.Date("2021-02-07"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_21 <- insertRows(TRE1_21, c(1412:1431), new = insertDF)

#Replace missing dates with NAs - 02/20 to 03/05
insertDF <- as.data.frame(matrix(data = NA, nrow = 12, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-02-21"), as.Date("2021-03-04"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_21 <- insertRows(TRE1_21, c(3081:3092), new = insertDF)

#Replace missing dates with NAs - 03/13 to 04/02
insertDF <- as.data.frame(matrix(data = NA, nrow = 19, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-03-14"), as.Date("2021-04-01"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_21 <- insertRows(TRE1_21, c(4231:4249), new = insertDF)

#Replace missing dates with NAs - 04/07 to 05/03
insertDF <- as.data.frame(matrix(data = NA, nrow = 25, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-04-08"), as.Date("2021-05-02"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_21 <- insertRows(TRE1_21, c(4989:5013), new = insertDF)

#Replace missing dates with NAs - 06/11 to 06/15
insertDF <- as.data.frame(matrix(data = NA, nrow = 3, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-06-12"), as.Date("2021-06-14"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE1_21 <- insertRows(TRE1_21, c(10588:10590), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE1_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
TRE1_clean <- merge(TRE1_18, TRE1_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
TRE1_clean <- merge(TRE1_clean, TRE1_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
TRE1_clean <- merge(TRE1_clean, TRE1_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
TRE1_clean <- merge(TRE1_clean, TRE1_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

TRE1_clean <- select(TRE1_clean, Date_time, WC_15cm, WC_30cm, WC_100cm)

#Graph

Soil <- ggplot(data = subset(TRE1_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date")) 
Soil

png("TRE1_clean", width = 4500, height = 2500)

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
write.csv(TRE1_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE1_clean.csv" ) #this writes a csv file and sends it to the working folder







