#Created by: Elise Miller
#Date started: 12/19/2022
#Date last edited: 02/23/2023
#Description: QA/QC TRE 2

#Attach dependencies 
library(tidyverse) 
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
                        pattern=glob2rx("Copy of T2M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
TRE2_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("Copy of T2M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
TRE2_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
TRE2 <- rbind(TRE2_2017_2019, TRE2_2019_2021)

#Write the csv
write.csv(TRE2,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE2.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(TRE2)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
TRE2$Date <- mdy_hms(TRE2$Date_time)

#Put year into a separate column 
TRE2 <- separate(TRE2, Date, c("Year"))

#TRE2 2017
##################################################################################################
TRE2_17 <- subset(TRE2, Year == '2017')

#Plotting 
TRE2_17$WC_15cm <- as.numeric(TRE2_17$WC_15cm)
TRE2_17$WC_30cm <- as.numeric(TRE2_17$WC_30cm)
TRE2_17$WC_100cm <- as.numeric(TRE2_17$WC_100cm)
TRE2_17$Date_time<- mdy_hms(TRE2_17$Date_time)

Soil <- ggplot(data = subset(TRE2_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE2 2018
##################################################################################################
TRE2_18 <- subset(TRE2, Year == '2018')

#Plotting 
TRE2_18$WC_15cm <- as.numeric(TRE2_18$WC_15cm)
TRE2_18$WC_30cm <- as.numeric(TRE2_18$WC_30cm)
TRE2_18$WC_100cm <- as.numeric(TRE2_18$WC_100cm)
TRE2_18$Date_time<- mdy_hms(TRE2_18$Date_time)

#Get rid of 15 cm glitch that has a random increase and decrease 
TRE2_18_fix <- filter(TRE2_18, Date_time > "2018-10-01 00:00:01")
TRE2_18_fix <- filter(TRE2_18_fix, Date_time < "2018-11-20 01:00:01")

TRE2_18_fix$WC_15cm <- NA

#Recombine
TRE2_18_early <- filter(TRE2_18, Date_time < "2018-10-01 00:00:01")
TRE2_18_late <- filter(TRE2_18, Date_time > "2018-11-20 01:00:01")
TRE2_18 <- bind_rows(TRE2_18_early, TRE2_18_late, TRE2_18_fix)

#30 cm 
###############################################################################

#Subset and remove drips
#=================================================================================
TRE2_18_fix <- filter(TRE2_18, Date_time > "2018-09-05 00:00:01")
TRE2_18_fix <- filter(TRE2_18_fix, Date_time < "2018-09-20 01:00:01")

TRE2_18_fix$WC_30cm[TRE2_18_fix$WC_30cm > 0.1950] <- NA
missing <- which(is.na(TRE2_18_fix$WC_30cm))

if(1 %in% missing){
  TRE2_18_fix$WC_30cm[1] <- head(TRE2_18_fix$WC_30cm[!is.na(TRE2_18_fix$WC_30cm)],1)
}
if(nrow(TRE2_18_fix) %in% missing){
  TRE2_18_fix$WC_30cm[nrow(data)] <- tail(TRE2_18_fix$WC_30cm[!is.na(TRE2_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_18_fix$WC_30cm[idx] <- (TRE2_18_fix$WC_30cm[r$starts[i]] + TRE2_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_18_early <- filter(TRE2_18, Date_time < "2018-09-05 00:00:01")
TRE2_18_late <- filter(TRE2_18, Date_time > "2018-09-20 01:00:01")
TRE2_18 <- bind_rows(TRE2_18_early, TRE2_18_late, TRE2_18_fix)

#Subset and remove drips
#=================================================================================
TRE2_18_fix <- filter(TRE2_18, Date_time > "2018-09-20 00:00:01")
TRE2_18_fix <- filter(TRE2_18_fix, Date_time < "2018-10-20 01:00:01")

TRE2_18_fix$WC_30cm[TRE2_18_fix$WC_30cm < 0.1822] <- NA
missing <- which(is.na(TRE2_18_fix$WC_30cm))

if(1 %in% missing){
  TRE2_18_fix$WC_30cm[1] <- head(TRE2_18_fix$WC_30cm[!is.na(TRE2_18_fix$WC_30cm)],1)
}
if(nrow(TRE2_18_fix) %in% missing){
  TRE2_18_fix$WC_30cm[nrow(data)] <- tail(TRE2_18_fix$WC_30cm[!is.na(TRE2_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_18_fix$WC_30cm[idx] <- (TRE2_18_fix$WC_30cm[r$starts[i]] + TRE2_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_18_early <- filter(TRE2_18, Date_time < "2018-09-20 00:00:01")
TRE2_18_late <- filter(TRE2_18, Date_time > "2018-10-20 01:00:01")
TRE2_18 <- bind_rows(TRE2_18_early, TRE2_18_late, TRE2_18_fix)

#Subset and remove drips
#=================================================================================
TRE2_18_fix <- filter(TRE2_18, Date_time > "2018-10-01 00:00:01")
TRE2_18_fix <- filter(TRE2_18_fix, Date_time < "2018-10-20 01:00:01")

TRE2_18_fix$WC_30cm[TRE2_18_fix$WC_30cm < 0.1842] <- NA
missing <- which(is.na(TRE2_18_fix$WC_30cm))

if(1 %in% missing){
  TRE2_18_fix$WC_30cm[1] <- head(TRE2_18_fix$WC_30cm[!is.na(TRE2_18_fix$WC_30cm)],1)
}
if(nrow(TRE2_18_fix) %in% missing){
  TRE2_18_fix$WC_30cm[nrow(data)] <- tail(TRE2_18_fix$WC_30cm[!is.na(TRE2_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_18_fix$WC_30cm[idx] <- (TRE2_18_fix$WC_30cm[r$starts[i]] + TRE2_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_18_early <- filter(TRE2_18, Date_time < "2018-10-01 00:00:01")
TRE2_18_late <- filter(TRE2_18, Date_time > "2018-10-20 01:00:01")
TRE2_18 <- bind_rows(TRE2_18_early, TRE2_18_late, TRE2_18_fix)

#Subset and remove drips
#=================================================================================
TRE2_18_fix <- filter(TRE2_18, Date_time > "2018-10-03 05:00:01")
TRE2_18_fix <- filter(TRE2_18_fix, Date_time < "2018-10-20 01:00:01")

Soil <- ggplot(data = subset(TRE2_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

TRE2_18_fix$WC_30cm[TRE2_18_fix$WC_30cm < 0.1865] <- NA
missing <- which(is.na(TRE2_18_fix$WC_30cm))

if(1 %in% missing){
  TRE2_18_fix$WC_30cm[1] <- head(TRE2_18_fix$WC_30cm[!is.na(TRE2_18_fix$WC_30cm)],1)
}
if(nrow(TRE2_18_fix) %in% missing){
  TRE2_18_fix$WC_30cm[nrow(data)] <- tail(TRE2_18_fix$WC_30cm[!is.na(TRE2_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_18_fix$WC_30cm[idx] <- (TRE2_18_fix$WC_30cm[r$starts[i]] + TRE2_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_18_early <- filter(TRE2_18, Date_time < "2018-10-03 05:00:01")
TRE2_18_late <- filter(TRE2_18, Date_time > "2018-10-20 01:00:01")
TRE2_18 <- bind_rows(TRE2_18_early, TRE2_18_late, TRE2_18_fix)

#Plot again 
Soil <- ggplot(data = subset(TRE2_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue")) 
Soil 

#TRE2 2019
##################################################################################################
TRE2_19 <- subset(TRE2, Year == '2019')

#Plotting 
TRE2_19$WC_15cm <- as.numeric(TRE2_19$WC_15cm)
TRE2_19$WC_30cm <- as.numeric(TRE2_19$WC_30cm)
TRE2_19$WC_100cm <- as.numeric(TRE2_19$WC_100cm)
TRE2_19$Date_time<- mdy_hms(TRE2_19$Date_time)

#15 cm
################################################################################

#Fix 15 cm glitch at the end 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-09-17 00:00:01")

TRE2_19_fix$WC_15cm <- NA

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-09-17 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-04-01 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-04-17 01:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.343] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-04-01 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-04-17 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-01 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-08 01:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.3465] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-01 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-08 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-08 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-10 01:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.345] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-08 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-10 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-09 02:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-09 20:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.3525] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-09 02:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-09 20:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-10 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-10 20:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.349] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-10 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-10 20:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-18 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-30 20:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.3423] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-18 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-30 20:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-24 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-26 00:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.346] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-24 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-26 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-26 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-28 00:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.3467] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-26 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-28 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-28 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-04-04 00:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.345] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-28 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-04-04 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-04-15 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-04-24 00:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.3433] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-04-15 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-04-24 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-05-18 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-05-19 00:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.352] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-05-18 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-05-19 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-05-19 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-05-21 00:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.351] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-05-19 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-05-21 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 15 cm glitches mid-April through May 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-05-19 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-05-21 00:00:01")

TRE2_19_fix$WC_15cm[TRE2_19_fix$WC_15cm < 0.351] <- NA
missing <- which(is.na(TRE2_19_fix$WC_15cm))

if(1 %in% missing){
  TRE2_19_fix$WC_15cm[1] <- head(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_15cm[nrow(data)] <- tail(TRE2_19_fix$WC_15cm[!is.na(TRE2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_15cm[idx] <- (TRE2_19_fix$WC_15cm[r$starts[i]] + TRE2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-05-19 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-05-21 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)


#30 cm 
################################################################

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-01-04 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-01-07 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.2989] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-01-04 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-01-07 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-01-07 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-01-10 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.305] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-01-07 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-01-10 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-01-14 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-01-16 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3027] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-01-14 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-01-16 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-01-31 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-02-03 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3015] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-01-31 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-02-03 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-02-04 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-02-05 02:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3092] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-02-04 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-02-05 02:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-02-05 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-02-10 02:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.305] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-02-05 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-02-10 02:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-02-19 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-02-22 02:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3055] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-02-19 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-02-22 02:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-01 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-03 02:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3059] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-01 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-03 02:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-05 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-07 02:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.306] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-05 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-07 02:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-08 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-09 08:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.306] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-08 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-09 08:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-10 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-11 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3095] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-10 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-11 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-19 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-21 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3047] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-19 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-21 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-21 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-23 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3092] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-21 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-23 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-24 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-26 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.308] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-24 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-26 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-03-26 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-03-28 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3085] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-03-26 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-03-28 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-04-15 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-04-20 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.3053] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-04-15 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-04-20 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 30 cm glitches 
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-11-15 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-11-27 00:00:01")

TRE2_19_fix$WC_30cm[TRE2_19_fix$WC_30cm < 0.1779] <- NA
missing <- which(is.na(TRE2_19_fix$WC_30cm))

if(1 %in% missing){
  TRE2_19_fix$WC_30cm[1] <- head(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_30cm[nrow(data)] <- tail(TRE2_19_fix$WC_30cm[!is.na(TRE2_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_30cm[idx] <- (TRE2_19_fix$WC_30cm[r$starts[i]] + TRE2_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-11-15 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-11-27 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#100 cm 
################################################################
TRE2_19$WC_100cm[TRE2_19$WC_100cm < 0.27] <- NA
missing <- which(is.na(TRE2_19$WC_100cm))

if(1 %in% missing){
  TRE2_19$WC_100cm[1] <- head(TRE2_19$WC_100cm[!is.na(TRE2_19$WC_100cm)],1)
}
if(nrow(TRE2_19) %in% missing){
  TRE2_19$WC_100cm[nrow(data)] <- tail(TRE2_19$WC_100cm[!is.na(TRE2_19$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19$WC_100cm[idx] <- (TRE2_19$WC_100cm[r$starts[i]] + TRE2_19$WC_100cm[r$ends[i]])/2
}


#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-01 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-06-07 01:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.322] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-01 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-06-07 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)


#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-11 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-06-20 01:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.322] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-11 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-06-20 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-29 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-03 01:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.303] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-29 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-03 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-08 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-10 01:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.298] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-08 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-10 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-10 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-15 01:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.295] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-10 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-15 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-16 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-25 01:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.289] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-16 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-25 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-16 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-18 01:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.293] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-16 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-18 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-20 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-21 01:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.291] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-20 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-21 01:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-04 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-06 :00:01")

#Plot again 
Soil <- ggplot(data = subset(TRE2_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil 

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.2845] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-04 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-06 :00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-12 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-16 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.2815] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-12 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-16 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-15 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-09-01 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.277] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-15 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-09-01 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-12 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-06-20 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.321] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-12 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-06-20 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-23 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-06-24 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.3121 | TRE2_19_fix$WC_100cm < 0.31075] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-23 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-06-24 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-23 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-06-26 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.3121 | TRE2_19_fix$WC_100cm < 0.3086] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-23 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-06-26 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-24 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-06-26 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.312] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-24 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-06-26 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-26 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-06-30 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.31 | TRE2_19_fix$WC_100cm < 0.305] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-26 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-06-30 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-28 20:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-01 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.3075 | TRE2_19_fix$WC_100cm < 0.3045] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-28 20:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-01 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)


#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-06-30 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-04 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.306] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-06-30 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-04 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-03 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-07 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.3035] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-03 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-07 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-04 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-07 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.303] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-04 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-07 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-06 20:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-11 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.301] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-06 20:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-11 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-07 20:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-10 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.3005 | TRE2_19_fix$WC_100cm < 0.299] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-07 20:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-10 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-08 20:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-13 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.3] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-08 20:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-13 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-10 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-13 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2993] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-10 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-13 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-11 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-16 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.298] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-11 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-16 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-12 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-16 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2975] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-12 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-16 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-14 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-20 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2965] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-14 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-20 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-16 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-23 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.295] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-16 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-23 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-18 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-26 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2945] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-18 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-26 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-19 20:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-26 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.293] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-19 20:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-26 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-22 20:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-30 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.292] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-22 20:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-30 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-24 20:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-07-30 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2903] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-24 20:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-07-30 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-26 23:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-02 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.289] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-26 23:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-02 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-27 14:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-08 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.288] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-27 14:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-08 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-29 14:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-08 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2875] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-29 14:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-08 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-07-30 14:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-10 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.287] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-07-30 14:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-10 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-03 14:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-10 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.286 | TRE2_19_fix$WC_100cm < 0.2825] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-03 14:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-10 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-04 14:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-14 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2855 | TRE2_19_fix$WC_100cm < 0.2825] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-04 14:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-14 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-06 14:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-16 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.285] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-06 14:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-16 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-08 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-20 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2845 | TRE2_19_fix$WC_100cm < 0.279] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-08 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-20 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-09 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-20 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2835] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-09 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-20 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-16 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-29 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.282] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-16 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-29 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-18 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-29 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2808] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-18 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-29 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-23 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-29 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2805] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-23 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-29 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-23 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-08-29 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2797 | TRE2_19_fix$WC_100cm < 0.2777] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-23 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-08-29 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-27 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-09-03 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2795] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-27 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-09-03 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-08-27 10:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-09-03 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.279 | TRE2_19_fix$WC_100cm < 0.2772] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-08-27 10:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-09-03 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-09-01 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-09-08 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.27785 | TRE2_19_fix$WC_100cm < 0.2770] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-09-01 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-09-08 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-09-06 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-09-13 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.27785] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-09-06 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-09-13 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-09-09 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-09-23 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2777 | TRE2_19_fix$WC_100cm < 0.275] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-09-09 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-09-23 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-09-09 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-09-30 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.278] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-09-09 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-09-30 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-09-10 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-09-13 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.277 | TRE2_19_fix$WC_100cm < 0.27575] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-09-10 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-09-13 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-09-12 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-09-17 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.277] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-09-12 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-09-17 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-09-23 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-01 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.278 | TRE2_19_fix$WC_100cm < 0.2755] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-09-23 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-01 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-09-30 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-03 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2778 | TRE2_19_fix$WC_100cm < 0.2755] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-09-30 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-03 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-03 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-07 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.27725] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-03 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-07 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-07 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-11 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.277 | TRE2_19_fix$WC_100cm < 0.27475] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-07 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-11 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-11 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-15 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.27675] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-11 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-15 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-15 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-20 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2765] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-15 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-20 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-20 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-25 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.277] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-20 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-25 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-21 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-25 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2765 | TRE2_19_fix$WC_100cm < 0.27425] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-21 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-25 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-22 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-28 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.277] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-22 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-28 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-25 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-31 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.277] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-25 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-31 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-25 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-31 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.277] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-25 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-31 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-27 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-10-31 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2755 | TRE2_19_fix$WC_100cm < 0.2735] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-27 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-10-31 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-10-31 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-11-05 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2755] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-10-31 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-11-05 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-11-05 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-11-10 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.277] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-11-05 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-11-10 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-11-06 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-11-10 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2755] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-11-06 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-11-10 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-11-28 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-11-30 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm < 0.2777] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-11-28 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-11-30 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Fix 100 cm glitches in mid June through July
#==================================================================
TRE2_19_fix <- filter(TRE2_19, Date_time > "2019-11-10 00:00:01")
TRE2_19_fix <- filter(TRE2_19_fix, Date_time < "2019-11-25 00:00:01")

TRE2_19_fix$WC_100cm[TRE2_19_fix$WC_100cm > 0.2775] <- NA
missing <- which(is.na(TRE2_19_fix$WC_100cm))

if(1 %in% missing){
  TRE2_19_fix$WC_100cm[1] <- head(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}
if(nrow(TRE2_19_fix) %in% missing){
  TRE2_19_fix$WC_100cm[nrow(data)] <- tail(TRE2_19_fix$WC_100cm[!is.na(TRE2_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_19_fix$WC_100cm[idx] <- (TRE2_19_fix$WC_100cm[r$starts[i]] + TRE2_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_19_early <- filter(TRE2_19, Date_time < "2019-11-10 00:00:01")
TRE2_19_late <- filter(TRE2_19, Date_time > "2019-11-25 00:00:01")
TRE2_19 <- bind_rows(TRE2_19_early, TRE2_19_late, TRE2_19_fix)

#Remove the missing dates
#=========================================================================
#Replace missing dates with NAs - 08/27 to 09/03
insertDF <- as.data.frame(matrix(data = NA, nrow = 6, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-08-28"), as.Date("2019-09-02"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE2_19 <- insertRows(TRE2_19, c(34248:34254), new = insertDF)

#Replace missing dates with NAs - 09/26 to 12/31
insertDF <- as.data.frame(matrix(data = NA, nrow = 96, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-09-27"), as.Date("2019-12-31"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE2_19 <- insertRows(TRE2_19, c(38530:38625), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE2_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE2 2020
##################################################################################################
TRE2_20 <- subset(TRE2, Year == '2020')

#Plotting 
TRE2_20$WC_15cm <- as.numeric(TRE2_20$WC_15cm)
TRE2_20$WC_30cm <- as.numeric(TRE2_20$WC_30cm)
TRE2_20$WC_100cm <- as.numeric(TRE2_20$WC_100cm)
TRE2_20$Date_time<- mdy_hms(TRE2_20$Date_time)

#15 cm
########################################################################################

#Fix glitch at beginning of year (September)
#==================================================================================
TRE2_20_fix <- filter(TRE2_20, Date_time > "2020-09-01 00:00:01")
TRE2_20_fix <- filter(TRE2_20_fix, Date_time < "2020-09-26 00:00:01")

TRE2_20_fix$WC_15cm[TRE2_20_fix$WC_15cm > 0.252] <- NA

#Recombine
TRE2_20_early <- filter(TRE2_20, Date_time < "2020-09-01 00:00:01")
TRE2_20_late <- filter(TRE2_20, Date_time > "2020-09-26 00:00:01")
TRE2_20 <- bind_rows(TRE2_20_early, TRE2_20_late, TRE2_20_fix)

#30 cm 
################################################################
TRE2_20_fix <- filter(TRE2_20, Date_time > "2020-11-16 18:00:01")
TRE2_20_fix <- filter(TRE2_20_fix, Date_time < "2020-11-18 00:00:01")

TRE2_20_fix$WC_30cm[TRE2_20_fix$WC_30cm < 0.1848] <- NA
missing <- which(is.na(TRE2_20_fix$WC_30cm))

if(1 %in% missing){
  TRE2_20_fix$WC_30cm[1] <- head(TRE2_20_fix$WC_30cm[!is.na(TRE2_20_fix$WC_30cm)],1)
}
if(nrow(TRE2_20_fix) %in% missing){
  TRE2_20_fix$WC_30cm[nrow(data)] <- tail(TRE2_20_fix$WC_30cm[!is.na(TRE2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_20_fix$WC_30cm[idx] <- (TRE2_20_fix$WC_30cm[r$starts[i]] + TRE2_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE2_20_early <- filter(TRE2_20, Date_time < "2020-11-16 18:00:01")
TRE2_20_late <- filter(TRE2_20, Date_time > "2020-11-18 00:00:01")
TRE2_20 <- bind_rows(TRE2_20_early, TRE2_20_late, TRE2_20_fix)

#Replace missing dates
##############################################################################

#Replace missing dates with NAs - 01/01 to 09/02
insertDF <- as.data.frame(matrix(data = NA, nrow = 245, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-01-01"), as.Date("2020-09-01"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE2_20 <- insertRows(TRE2_20, c(1:244), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE2_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE2 2021
##################################################################################################
TRE2_21 <- subset(TRE2, Year == '2021')

#Plotting 
TRE2_21$WC_15cm <- as.numeric(TRE2_21$WC_15cm)
TRE2_21$WC_30cm <- as.numeric(TRE2_21$WC_30cm)
TRE2_21$WC_100cm <- as.numeric(TRE2_21$WC_100cm)
TRE2_21$Date_time<- mdy_hms(TRE2_21$Date_time)

#100 cm 
################################################################

#Removed drips in June/July
#=======================================================================================
TRE2_21_fix <- filter(TRE2_21, Date_time > "2021-06-16 18:00:01")
TRE2_21_fix <- filter(TRE2_21_fix, Date_time < "2021-06-28 00:00:01")

TRE2_21_fix$WC_100cm[TRE2_21_fix$WC_100cm > 0.287] <- NA
missing <- which(is.na(TRE2_21_fix$WC_100cm))

if(1 %in% missing){
  TRE2_21_fix$WC_100cm[1] <- head(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}
if(nrow(TRE2_21_fix) %in% missing){
  TRE2_21_fix$WC_100cm[nrow(data)] <- tail(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_21_fix$WC_100cm[idx] <- (TRE2_21_fix$WC_100cm[r$starts[i]] + TRE2_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_21_early <- filter(TRE2_21, Date_time < "2021-06-16 18:00:01")
TRE2_21_late <- filter(TRE2_21, Date_time > "2021-06-28 00:00:01")
TRE2_21 <- bind_rows(TRE2_21_early, TRE2_21_late, TRE2_21_fix)

#Removed drips in June/July
#=======================================================================================
TRE2_21_fix <- filter(TRE2_21, Date_time > "2021-06-24 18:00:01")
TRE2_21_fix <- filter(TRE2_21_fix, Date_time < "2021-07-05 00:00:01")

TRE2_21_fix$WC_100cm[TRE2_21_fix$WC_100cm > 0.2848] <- NA
missing <- which(is.na(TRE2_21_fix$WC_100cm))

if(1 %in% missing){
  TRE2_21_fix$WC_100cm[1] <- head(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}
if(nrow(TRE2_21_fix) %in% missing){
  TRE2_21_fix$WC_100cm[nrow(data)] <- tail(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_21_fix$WC_100cm[idx] <- (TRE2_21_fix$WC_100cm[r$starts[i]] + TRE2_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_21_early <- filter(TRE2_21, Date_time < "2021-06-24 18:00:01")
TRE2_21_late <- filter(TRE2_21, Date_time > "2021-07-05 00:00:01")
TRE2_21 <- bind_rows(TRE2_21_early, TRE2_21_late, TRE2_21_fix)

#Removed drips in June/July
#=======================================================================================
TRE2_21_fix <- filter(TRE2_21, Date_time > "2021-07-05 18:00:01")
TRE2_21_fix <- filter(TRE2_21_fix, Date_time < "2021-07-13 00:00:01")

TRE2_21_fix$WC_100cm[TRE2_21_fix$WC_100cm > 0.283] <- NA
missing <- which(is.na(TRE2_21_fix$WC_100cm))

if(1 %in% missing){
  TRE2_21_fix$WC_100cm[1] <- head(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}
if(nrow(TRE2_21_fix) %in% missing){
  TRE2_21_fix$WC_100cm[nrow(data)] <- tail(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_21_fix$WC_100cm[idx] <- (TRE2_21_fix$WC_100cm[r$starts[i]] + TRE2_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_21_early <- filter(TRE2_21, Date_time < "2021-07-05 18:00:01")
TRE2_21_late <- filter(TRE2_21, Date_time > "2021-07-13 00:00:01")
TRE2_21 <- bind_rows(TRE2_21_early, TRE2_21_late, TRE2_21_fix)

#Removed drips in June/July
#=======================================================================================
TRE2_21_fix <- filter(TRE2_21, Date_time > "2021-07-09 18:00:01")
TRE2_21_fix <- filter(TRE2_21_fix, Date_time < "2021-07-13 00:00:01")

TRE2_21_fix$WC_100cm[TRE2_21_fix$WC_100cm > 0.2825] <- NA
missing <- which(is.na(TRE2_21_fix$WC_100cm))

if(1 %in% missing){
  TRE2_21_fix$WC_100cm[1] <- head(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}
if(nrow(TRE2_21_fix) %in% missing){
  TRE2_21_fix$WC_100cm[nrow(data)] <- tail(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_21_fix$WC_100cm[idx] <- (TRE2_21_fix$WC_100cm[r$starts[i]] + TRE2_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_21_early <- filter(TRE2_21, Date_time < "2021-07-09 18:00:01")
TRE2_21_late <- filter(TRE2_21, Date_time > "2021-07-13 00:00:01")
TRE2_21 <- bind_rows(TRE2_21_early, TRE2_21_late, TRE2_21_fix)

#Removed drips in June/July
#=======================================================================================
TRE2_21_fix <- filter(TRE2_21, Date_time > "2021-07-13 18:00:01")
TRE2_21_fix <- filter(TRE2_21_fix, Date_time < "2021-07-23 00:00:01")

TRE2_21_fix$WC_100cm[TRE2_21_fix$WC_100cm > 0.282] <- NA
missing <- which(is.na(TRE2_21_fix$WC_100cm))

if(1 %in% missing){
  TRE2_21_fix$WC_100cm[1] <- head(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}
if(nrow(TRE2_21_fix) %in% missing){
  TRE2_21_fix$WC_100cm[nrow(data)] <- tail(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_21_fix$WC_100cm[idx] <- (TRE2_21_fix$WC_100cm[r$starts[i]] + TRE2_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_21_early <- filter(TRE2_21, Date_time < "2021-07-13 18:00:01")
TRE2_21_late <- filter(TRE2_21, Date_time > "2021-07-23 00:00:01")
TRE2_21 <- bind_rows(TRE2_21_early, TRE2_21_late, TRE2_21_fix)

#Removed drips in June/July
#=======================================================================================
TRE2_21_fix <- filter(TRE2_21, Date_time > "2021-07-27 18:00:01")
TRE2_21_fix <- filter(TRE2_21_fix, Date_time < "2021-08-23 00:00:01")

TRE2_21_fix$WC_100cm[TRE2_21_fix$WC_100cm > 0.28] <- NA
missing <- which(is.na(TRE2_21_fix$WC_100cm))

if(1 %in% missing){
  TRE2_21_fix$WC_100cm[1] <- head(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}
if(nrow(TRE2_21_fix) %in% missing){
  TRE2_21_fix$WC_100cm[nrow(data)] <- tail(TRE2_21_fix$WC_100cm[!is.na(TRE2_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE2_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE2_21_fix$WC_100cm[idx] <- (TRE2_21_fix$WC_100cm[r$starts[i]] + TRE2_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE2_21_early <- filter(TRE2_21, Date_time < "2021-07-27 18:00:01")
TRE2_21_late <- filter(TRE2_21, Date_time > "2021-08-23 00:00:01")
TRE2_21 <- bind_rows(TRE2_21_early, TRE2_21_late, TRE2_21_fix)

#Plot again 
Soil <- ggplot(data = subset(TRE2_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Combine all of the WIL1 dataframes/years into 1 dataframe 
#=================================================================

#Merge 2018 and 2019 
TRE2_clean <- merge(TRE2_18, TRE2_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
TRE2_clean <- merge(TRE2_clean, TRE2_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
TRE2_clean <- merge(TRE2_clean, TRE2_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2017 
TRE2_clean <- merge(TRE2_clean, TRE2_18, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

TRE2_clean <- select(TRE2_clean, Date_time, WC_15cm, WC_30cm, WC_100cm)
#Graph
#===================================================================================

Soil <- ggplot(data = subset(TRE2_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date")) 
Soil

png("TRE2_clean", width = 4500, height = 2500)

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
write.csv(TRE2_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE2_clean.csv" ) #this writes a csv file and sends it to the working folder


