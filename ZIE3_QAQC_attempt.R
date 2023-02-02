#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/01/2023
#Description: QA/QC ZIE 3

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
                        pattern=glob2rx("Copy of Z3M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
ZIE3_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("Copy of Z3M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
ZIE3_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
ZIE3 <- rbind(ZIE3_2017_2019, ZIE3_2019_2021)

#Write the csv
write.csv(ZIE3,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE3.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(ZIE3)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
ZIE3$Date <- mdy_hms(ZIE3$Date_time)

#Put year into a separate column 
ZIE3 <- separate(ZIE3, Date, c("Year"))

#ZIE3 2017
##################################################################################################
ZIE3_17 <- subset(ZIE3, Year == '2017')

#Plotting 
ZIE3_17$WC_15cm <- as.numeric(ZIE3_17$WC_15cm)
ZIE3_17$WC_30cm <- as.numeric(ZIE3_17$WC_30cm)
ZIE3_17$WC_100cm <- as.numeric(ZIE3_17$WC_100cm)
ZIE3_17$Date_time<- mdy_hms(ZIE3_17$Date_time)

Soil <- ggplot(data = subset(ZIE3_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE3 2018
##################################################################################################
ZIE3_18 <- subset(ZIE3, Year == '2018')

#Plotting 
ZIE3_18$WC_15cm <- as.numeric(ZIE3_18$WC_15cm)
ZIE3_18$WC_30cm <- as.numeric(ZIE3_18$WC_30cm)
ZIE3_18$WC_100cm <- as.numeric(ZIE3_18$WC_100cm)
ZIE3_18$Date_time<- mdy_hms(ZIE3_18$Date_time)

#15 cm
##################################################################################
ZIE3_18$WC_15cm[ZIE3_18$WC_15cm < 0] <- NA
missing <- which(is.na(ZIE3_18$WC_15cm))

if(1 %in% missing){
  ZIE3_18$WC_15cm[1] <- head(ZIE3_18$WC_15cm[!is.na(ZIE3_18$WC_15cm)],1)
}
if(nrow(ZIE3_18) %in% missing){
  ZIE3_18$WC_15cm[nrow(data)] <- tail(ZIE3_18$WC_15cm[!is.na(ZIE3_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_18$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_18$WC_15cm[idx] <- (ZIE3_18$WC_15cm[r$starts[i]] + ZIE3_18$WC_15cm[r$ends[i]])/2
}

#Subset and remove drips
#=============================================================================
ZIE3_18_fix <- filter(ZIE3_18, Date_time > "2018-02-12 17:40:01")
ZIE3_18_fix <- filter(ZIE3_18_fix, Date_time < "2018-02-26 22:30:01")

Soil <- ggplot(data = subset(ZIE3_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

ZIE3_18_fix$WC_15cm[ZIE3_18_fix$WC_15cm < 0.292] <- NA

#Recombine 
ZIE3_18_early <- filter(ZIE3_18, Date_time < "2018-02-12 17:40:01")
ZIE3_18_late <- filter(ZIE3_18, Date_time > "2018-02-26 22:30:01")
ZIE3_18 <- bind_rows(ZIE3_18_early, ZIE3_18_late, ZIE3_18_fix)

#Plot again 
Soil <- ggplot(data = subset(ZIE3_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE3 2019
##################################################################################################
ZIE3_19 <- subset(ZIE3, Year == '2019')

#Plotting 
ZIE3_19$WC_15cm <- as.numeric(ZIE3_19$WC_15cm)
ZIE3_19$WC_30cm <- as.numeric(ZIE3_19$WC_30cm)
ZIE3_19$WC_100cm <- as.numeric(ZIE3_19$WC_100cm)
ZIE3_19$Date_time<- mdy_hms(ZIE3_19$Date_time)

#15 cm
###########################################
ZIE3_19$WC_15cm[ZIE3_19$WC_15cm < 0.22] <- NA
missing <- which(is.na(ZIE3_19$WC_15cm))

if(1 %in% missing){
  ZIE3_19$WC_15cm[1] <- head(ZIE3_19$WC_15cm[!is.na(ZIE3_19$WC_15cm)],1)
}
if(nrow(ZIE3_19) %in% missing){
  ZIE3_19$WC_15cm[nrow(data)] <- tail(ZIE3_19$WC_15cm[!is.na(ZIE3_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_19$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_19$WC_15cm[idx] <- (ZIE3_19$WC_15cm[r$starts[i]] + ZIE3_19$WC_15cm[r$ends[i]])/2
}

#Remove glitches
#=========================================================================
ZIE3_19$WC_15cm[ZIE3_19$WC_15cm == 0.2688] <- NA
ZIE3_19$WC_15cm[ZIE3_19$WC_15cm > 0.4] <- NA
ZIE3_19$WC_15cm[ZIE3_19$WC_15cm > 0.34649 & ZIE3_19$WC_15cm < 0.34651] <- NA
ZIE3_19$WC_15cm[ZIE3_19$WC_15cm > 0.381449 & ZIE3_19$WC_15cm < 0.381451] <- NA

#Fix weird drop before the data starts behaving again in June
#==================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-05-20 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-06-01 23:20:01")

ZIE3_19_fix$WC_15cm[ZIE3_19_fix$WC_15cm < 0.27] <- NA

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-05-20 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-06-01 23:20:01")

ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#Remove weird upward glitch in July 
#==================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-07-12 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-08-01 22:30:01")

ZIE3_19_fix$WC_15cm[ZIE3_19_fix$WC_15cm > 0.24] <- NA

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-07-12 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-08-01 22:30:01")

ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#Remove weird upward glitch in July 
#==================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-07-12 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-08-01 22:30:01")

ZIE3_19_fix$WC_15cm[ZIE3_19_fix$WC_15cm > 0.24] <- NA

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-07-12 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-08-01 22:30:01")

ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#Remove weird upward glitch in July 
#==================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-09-12 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-11-01 22:30:01")

ZIE3_19_fix$WC_15cm[ZIE3_19_fix$WC_15cm > 0.24] <- NA

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-09-12 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-08-01 22:30:01")

ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#30 cm 
################################################################
ZIE3_19$WC_30cm[ZIE3_19$WC_30cm < 0.15] <- NA
missing <- which(is.na(ZIE3_19$WC_30cm))

if(1 %in% missing){
  ZIE3_19$WC_30cm[1] <- head(ZIE3_19$WC_30cm[!is.na(ZIE3_19$WC_30cm)],1)
}
if(nrow(ZIE3_19) %in% missing){
  ZIE3_19$WC_30cm[nrow(data)] <- tail(ZIE3_19$WC_30cm[!is.na(ZIE3_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_19$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_19$WC_30cm[idx] <- (ZIE3_19$WC_30cm[r$starts[i]] + ZIE3_19$WC_30cm[r$ends[i]])/2
}

#Subset and remove drips in September and October
#=====================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-09-15 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-10-20 22:30:01")

ZIE3_19_fix$WC_30cm[ZIE3_19_fix$WC_30cm < 0.2235 | ZIE3_19_fix$WC_30cm > 0.231] <- NA
missing <- which(is.na(ZIE3_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_19_fix$WC_30cm[1] <- head(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}
if(nrow(ZIE3_19_fix) %in% missing){
  ZIE3_19_fix$WC_30cm[nrow(data)] <- tail(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_19_fix$WC_30cm[idx] <- (ZIE3_19_fix$WC_30cm[r$starts[i]] + ZIE3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-09-15 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-10-20 22:30:01")

ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#Subset and remove drips in September and October
#=====================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-10-20 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-11-20 22:30:01")

ZIE3_19_fix$WC_30cm[ZIE3_19_fix$WC_30cm > 0.227] <- NA
missing <- which(is.na(ZIE3_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_19_fix$WC_30cm[1] <- head(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}
if(nrow(ZIE3_19_fix) %in% missing){
  ZIE3_19_fix$WC_30cm[nrow(data)] <- tail(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_19_fix$WC_30cm[idx] <- (ZIE3_19_fix$WC_30cm[r$starts[i]] + ZIE3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-10-20 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-11-20 22:30:01")

ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#Subset and remove drips in September and October
#=====================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-11-20 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-11-25 22:30:01")

ZIE3_19_fix$WC_30cm[ZIE3_19_fix$WC_30cm < 0.2187] <- NA
missing <- which(is.na(ZIE3_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_19_fix$WC_30cm[1] <- head(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}
if(nrow(ZIE3_19_fix) %in% missing){
  ZIE3_19_fix$WC_30cm[nrow(data)] <- tail(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_19_fix$WC_30cm[idx] <- (ZIE3_19_fix$WC_30cm[r$starts[i]] + ZIE3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-11-20 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-11-25 22:30:01")

ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#Subset and remove drips in September and October
#=====================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-11-25 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-11-27 22:30:01")

ZIE3_19_fix$WC_30cm[ZIE3_19_fix$WC_30cm < 0.215] <- NA
missing <- which(is.na(ZIE3_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_19_fix$WC_30cm[1] <- head(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}
if(nrow(ZIE3_19_fix) %in% missing){
  ZIE3_19_fix$WC_30cm[nrow(data)] <- tail(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_19_fix$WC_30cm[idx] <- (ZIE3_19_fix$WC_30cm[r$starts[i]] + ZIE3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-11-25 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-11-27 22:30:01")
ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#Subset and remove drips in September and October
#=====================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-11-11 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-11-27 22:30:01")

Soil <- ggplot(data = subset(ZIE3_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

ZIE3_19_fix$WC_30cm[ZIE3_19_fix$WC_30cm < 0.219] <- NA
missing <- which(is.na(ZIE3_19_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_19_fix$WC_30cm[1] <- head(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}
if(nrow(ZIE3_19_fix) %in% missing){
  ZIE3_19_fix$WC_30cm[nrow(data)] <- tail(ZIE3_19_fix$WC_30cm[!is.na(ZIE3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_19_fix$WC_30cm[idx] <- (ZIE3_19_fix$WC_30cm[r$starts[i]] + ZIE3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-11-11 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-11-27 22:30:01")
ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#100 cm 
################################################################
ZIE3_19$WC_100cm[ZIE3_19$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE3_19$WC_100cm))

if(1 %in% missing){
  ZIE3_19$WC_100cm[1] <- head(ZIE3_19$WC_100cm[!is.na(ZIE3_19$WC_100cm)],1)
}
if(nrow(ZIE3_19) %in% missing){
  ZIE3_19$WC_100cm[nrow(data)] <- tail(ZIE3_19$WC_100cm[!is.na(ZIE3_19$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_19$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_19$WC_100cm[idx] <- (ZIE3_19$WC_100cm[r$starts[i]] + ZIE3_19$WC_100cm[r$ends[i]])/2
}

#Fix glitch/missing dates from 2019-08-05 17:40:01 to 2019-08-06 10:20 for all depths
#===================================================================================
ZIE3_19_fix <- filter(ZIE3_19, Date_time > "2019-08-01 17:40:01")
ZIE3_19_fix <- filter(ZIE3_19_fix, Date_time < "2019-08-17 23:20:01")

ZIE3_19_fix$WC_100cm[ZIE3_19_fix$WC_100cm > 0.305] <- NA
ZIE3_19_fix$WC_30cm[ZIE3_19_fix$WC_30cm > 0.244] <- NA
ZIE3_19_fix$WC_15cm[ZIE3_19_fix$WC_15cm > 0.243] <- NA

#Recombine 
ZIE3_19_early <- filter(ZIE3_19, Date_time < "2019-08-01 17:40:01")
ZIE3_19_late <- filter(ZIE3_19, Date_time > "2019-08-17 23:20:01")

ZIE3_19 <- bind_rows(ZIE3_19_early, ZIE3_19_late, ZIE3_19_fix)

#Replace missing dates
#================================================================================================
#Replace missing dates with NAs - 08-17 to 09/06
insertDF <- as.data.frame(matrix(data = NA, nrow = 19, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-08-18"), as.Date("2019-09-05"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE3_19<- insertRows(ZIE3_19, c(32862:32880), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(ZIE3_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE3 2020
##################################################################################################
ZIE3_20 <- subset(ZIE3, Year == '2020')

#Plotting 
ZIE3_20$WC_15cm <- as.numeric(ZIE3_20$WC_15cm)
ZIE3_20$WC_30cm <- as.numeric(ZIE3_20$WC_30cm)
ZIE3_20$WC_100cm <- as.numeric(ZIE3_20$WC_100cm)
ZIE3_20$Date_time<- mdy_hms(ZIE3_20$Date_time)

#15 cm
###########################################
ZIE3_20$WC_15cm[ZIE3_20$WC_15cm < 0.2] <- NA
missing <- which(is.na(ZIE3_20$WC_15cm))

if(1 %in% missing){
  ZIE3_20$WC_15cm[1] <- head(ZIE3_20$WC_15cm[!is.na(ZIE3_20$WC_15cm)],1)
}
if(nrow(ZIE3_20) %in% missing){
  ZIE3_20$WC_15cm[nrow(data)] <- tail(ZIE3_20$WC_15cm[!is.na(ZIE3_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20$WC_15cm[idx] <- (ZIE3_20$WC_15cm[r$starts[i]] + ZIE3_20$WC_15cm[r$ends[i]])/2
}

#Subset and remove drips
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-05 10:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-25 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.276] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-05 10:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-25 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drips
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-05 10:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-05-25 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.296] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-05 10:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-05-25 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drips
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-16 10:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-05-25 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.315] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-16 10:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-05-25 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drips
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-16 10:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-07-15 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.255] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-16 10:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-07-15 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drips
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-22 10:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-07-01 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.27] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-22 10:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-07-01 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drips
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-07-12 10:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-08-01 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.235] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}


#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-07-12 10:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-08-01 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-01 10:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-05-06 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.3] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}


#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-01 10:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-05-06 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-06 10:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-05-09 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.2995] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}


#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-06 10:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-05-09 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-09 10:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-05-11 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.2975] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}


#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-09 10:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-05-11 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-12 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-05-15 10:00:01")

Soil <- ggplot(data = subset(ZIE3_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.3] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}


#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-12 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-05-15 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-20 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-05-22 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.318] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}


#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-20 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-05-22 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-25 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-05-27 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.3095] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-25 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-05-27 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-28 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-01 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.305] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-28 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-01 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-01 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-03 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.303] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-01 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-03 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-03 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-06 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.2985] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-03 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-06 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-06 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-11 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.2925] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-06 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-11 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-13 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-15 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.289] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-13 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-15 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-17 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-20 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.283] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-17 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-20 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-21 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-23 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.28] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-21 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-23 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-26 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-27 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.276] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-26 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-27 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-27 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-29 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.2735] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-27 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-29 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-05-20 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-01 10:00:01")

Soil <- ggplot(data = subset(ZIE3_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.305] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-05-20 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-01 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-22 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-06-27 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.276] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-22 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-06-27 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-06-30 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-07-03 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.269] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-06-30 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-07-03 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-07-03 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-07-06 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.267] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-07-03 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-07-06 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-07-09 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-07-11 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.262] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-07-09 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-07-11 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-07-16 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-07-19 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.252] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-07-16 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-07-19 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-07-20 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-07-22 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.25] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-07-20 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-07-22 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-07-22 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-07-24 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.248] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-07-22 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-07-24 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-07-24 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-07-26 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.245] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-07-24 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-07-26 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-07-26 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-08-05 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.236] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-07-26 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-08-05 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-08-15 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-08-25 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.2145] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-08-15 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-08-25 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-08-05 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-08-10 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.231] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-08-05 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-08-10 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-08-10 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-08-17 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.224] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-08-10 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-08-17 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in May/June
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-08-17 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-08-27 10:00:01")

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm < 0.217] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_15cm[1] <- head(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_15cm[nrow(data)] <- tail(ZIE3_20_fix$WC_15cm[!is.na(ZIE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_15cm[idx] <- (ZIE3_20_fix$WC_15cm[r$starts[i]] + ZIE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-08-17 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-08-27 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Remove glitch in 15 cm in September/October
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-09-01 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-11-12 10:00:01")

Soil <- ggplot(data = subset(ZIE3_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm > 0.15] <- NA

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-09-01 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-11-12 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Fix 15 cm glitch in October
#=============================================================================
ZIE3_20$WC_15cm[ZIE3_20$WC_15cm > 0.202749 & ZIE3_20$WC_15cm < 0.202751] <- NA

#30 cm 
################################################################
ZIE3_20$WC_30cm[ZIE3_20$WC_30cm < 0.18] <- NA
missing <- which(is.na(ZIE3_20$WC_30cm))

if(1 %in% missing){
  ZIE3_20$WC_30cm[1] <- head(ZIE3_20$WC_30cm[!is.na(ZIE3_20$WC_30cm)],1)
}
if(nrow(ZIE3_20) %in% missing){
  ZIE3_20$WC_30cm[nrow(data)] <- tail(ZIE3_20$WC_30cm[!is.na(ZIE3_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20$WC_30cm[idx] <- (ZIE3_20$WC_30cm[r$starts[i]] + ZIE3_20$WC_30cm[r$ends[i]])/2
}

#Subset and remove drip in April
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-04-10 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-04-14 10:00:01")

ZIE3_20_fix$WC_30cm[ZIE3_20_fix$WC_30cm > 0.313] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_30cm[1] <- head(ZIE3_20_fix$WC_30cm[!is.na(ZIE3_20_fix$WC_30cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_30cm[nrow(data)] <- tail(ZIE3_20_fix$WC_30cm[!is.na(ZIE3_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_30cm[idx] <- (ZIE3_20_fix$WC_30cm[r$starts[i]] + ZIE3_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-04-10 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-04-14 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in April
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-04-14 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-04-19 10:00:01")

ZIE3_20_fix$WC_30cm[ZIE3_20_fix$WC_30cm > 0.3075] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_30cm[1] <- head(ZIE3_20_fix$WC_30cm[!is.na(ZIE3_20_fix$WC_30cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_30cm[nrow(data)] <- tail(ZIE3_20_fix$WC_30cm[!is.na(ZIE3_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_30cm[idx] <- (ZIE3_20_fix$WC_30cm[r$starts[i]] + ZIE3_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-04-14 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-04-19 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Subset and remove drip in April
#===================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-04-19 00:00:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-04-29 10:00:01")

ZIE3_20_fix$WC_30cm[ZIE3_20_fix$WC_30cm > 0.305] <- NA
missing <- which(is.na(ZIE3_20_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_20_fix$WC_30cm[1] <- head(ZIE3_20_fix$WC_30cm[!is.na(ZIE3_20_fix$WC_30cm)],1)
}
if(nrow(ZIE3_20_fix) %in% missing){
  ZIE3_20_fix$WC_30cm[nrow(data)] <- tail(ZIE3_20_fix$WC_30cm[!is.na(ZIE3_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20_fix$WC_30cm[idx] <- (ZIE3_20_fix$WC_30cm[r$starts[i]] + ZIE3_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-04-19 00:00:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-04-29 10:00:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#100 cm 
################################################################
ZIE3_20$WC_100cm[ZIE3_20$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE3_19$WC_100cm))

if(1 %in% missing){
  ZIE3_20$WC_100cm[1] <- head(ZIE3_20$WC_100cm[!is.na(ZIE3_20$WC_100cm)],1)
}
if(nrow(ZIE3_20) %in% missing){
  ZIE3_20$WC_100cm[nrow(data)] <- tail(ZIE3_20$WC_100cm[!is.na(ZIE3_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_20$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_20$WC_100cm[idx] <- (ZIE3_20$WC_100cm[r$starts[i]] + ZIE3_20$WC_100cm[r$ends[i]])/2
}

#Fix glitch/missing dates from 2020-08-30 10:40:01 to 2020-08-31 11:20:01 for all depths
#===================================================================================
ZIE3_20_fix <- filter(ZIE3_20, Date_time > "2020-08-28 00:40:01")
ZIE3_20_fix <- filter(ZIE3_20_fix, Date_time < "2020-09-02 00:20:01")

ZIE3_20_fix$WC_100cm[ZIE3_20_fix$WC_100cm > 0.273] <- NA
ZIE3_20_fix$WC_30cm[ZIE3_20_fix$WC_30cm > 0.203] <- NA
ZIE3_20_fix$WC_15cm[ZIE3_20_fix$WC_15cm > 0.2025] <- NA

#Recombine 
ZIE3_20_early <- filter(ZIE3_20, Date_time < "2020-08-29 00:40:01")
ZIE3_20_late <- filter(ZIE3_20, Date_time > "2020-09-02 00:20:01")

ZIE3_20 <- bind_rows(ZIE3_20_early, ZIE3_20_late, ZIE3_20_fix)

#Plot again 
Soil <- ggplot(data = subset(ZIE3_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE3 2021
##################################################################################################
ZIE3_21 <- subset(ZIE3, Year == '2021')

#Plotting 
ZIE3_21$WC_15cm <- as.numeric(ZIE3_21$WC_15cm)
ZIE3_21$WC_30cm <- as.numeric(ZIE3_21$WC_30cm)
ZIE3_21$WC_100cm <- as.numeric(ZIE3_21$WC_100cm)
ZIE3_21$Date_time<- mdy_hms(ZIE3_21$Date_time)

#15 cm
###########################################
ZIE3_21$WC_15cm[ZIE3_21$WC_15cm < 0.12] <- NA
missing <- which(is.na(ZIE3_21$WC_15cm))

if(1 %in% missing){
  ZIE3_21$WC_15cm[1] <- head(ZIE3_21$WC_15cm[!is.na(ZIE3_21$WC_15cm)],1)
}
if(nrow(ZIE3_21) %in% missing){
  ZIE3_21$WC_15cm[nrow(data)] <- tail(ZIE3_21$WC_15cm[!is.na(ZIE3_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21$WC_15cm[idx] <- (ZIE3_21$WC_15cm[r$starts[i]] + ZIE3_21$WC_15cm[r$ends[i]])/2
}

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-01 10:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-04-16 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.28] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-01 10:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-04-16 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-01 10:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-04-01 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.296] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-01 10:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-04-01 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-02-01 10:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-04-01 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.296] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-02-01 10:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-04-01 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-02-21 10:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-02-23 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.317] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-02-21 10:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-02-23 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-02-26 10:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-02-28 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.3079] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-02-26 10:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-02-28 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-02-27 10:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-03 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.303] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-02-27 10:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-03 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-02 10:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-05 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.302] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-02 10:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-05 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-07 10:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-09 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.315] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-07 10:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-09 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-09 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-11 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.326] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-09 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-11 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-11 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-14 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.316] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-11 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-14 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-17 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-18 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.315] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-17 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-18 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-18 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-20 10:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.323] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-18 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-20 10:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-20 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-24 00:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.3128] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-20 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-24 00:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-24 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-28 00:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.304] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-24 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-28 00:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-28 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-29 12:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.302] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-28 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-29 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-29 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-31 12:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.2995] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-29 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-31 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-31 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-04-03 12:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.295] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-31 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-04-03 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-04-04 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-04-06 12:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.292] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-04-04 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-04-06 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-15 00:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-20 12:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.3164] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-15 00:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-20 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and remove the drips
#====================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-03-18 12:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-03-19 12:00:01")

ZIE3_21_fix$WC_15cm[ZIE3_21_fix$WC_15cm < 0.3265] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_15cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_15cm[1] <- head(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_15cm[nrow(data)] <- tail(ZIE3_21_fix$WC_15cm[!is.na(ZIE3_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_15cm[idx] <- (ZIE3_21_fix$WC_15cm[r$starts[i]] + ZIE3_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-03-18 12:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-03-19 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#30 cm 
################################################################
ZIE3_21$WC_30cm[ZIE3_21$WC_30cm < 0] <- NA
missing <- which(is.na(ZIE3_21$WC_30cm))

if(1 %in% missing){
  ZIE3_21$WC_30cm[1] <- head(ZIE3_21$WC_30cm[!is.na(ZIE3_21$WC_30cm)],1)
}
if(nrow(ZIE3_21) %in% missing){
  ZIE3_21$WC_30cm[nrow(data)] <- tail(ZIE3_21$WC_30cm[!is.na(ZIE3_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21$WC_30cm[idx] <- (ZIE3_21$WC_30cm[r$starts[i]] + ZIE3_21$WC_30cm[r$ends[i]])/2
}

#Subset and fix drips later in the year 
#================================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-07-07 12:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-07-20 12:00:01")

ZIE3_21_fix$WC_30cm[ZIE3_21_fix$WC_30cm < 0.147] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_30cm[1] <- head(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_30cm[nrow(data)] <- tail(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_30cm[idx] <- (ZIE3_21_fix$WC_30cm[r$starts[i]] + ZIE3_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-07-07 12:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-07-20 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and fix drips later in the year 
#================================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-07-08 12:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-07-11 12:00:01")

ZIE3_21_fix$WC_30cm[ZIE3_21_fix$WC_30cm < 0.15] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_30cm[1] <- head(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_30cm[nrow(data)] <- tail(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_30cm[idx] <- (ZIE3_21_fix$WC_30cm[r$starts[i]] + ZIE3_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-07-08 12:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-07-11 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and fix drips later in the year 
#================================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-07-11 12:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-07-16 12:00:01")

ZIE3_21_fix$WC_30cm[ZIE3_21_fix$WC_30cm < 0.15] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_30cm[1] <- head(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_30cm[nrow(data)] <- tail(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_30cm[idx] <- (ZIE3_21_fix$WC_30cm[r$starts[i]] + ZIE3_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-07-11 12:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-07-16 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and fix drips later in the year 
#================================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-07-16 12:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-07-18 12:00:01")

ZIE3_21_fix$WC_30cm[ZIE3_21_fix$WC_30cm < 0.168] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_30cm[1] <- head(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_30cm[nrow(data)] <- tail(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_30cm[idx] <- (ZIE3_21_fix$WC_30cm[r$starts[i]] + ZIE3_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-07-16 12:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-07-18 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and fix drips later in the year 
#================================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-07-18 12:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-07-22 12:00:01")

ZIE3_21_fix$WC_30cm[ZIE3_21_fix$WC_30cm < 0.144] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_30cm[1] <- head(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_30cm[nrow(data)] <- tail(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_30cm[idx] <- (ZIE3_21_fix$WC_30cm[r$starts[i]] + ZIE3_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-07-18 12:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-07-22 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and fix drips later in the year 
#================================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-07-27 12:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-07-30 12:00:01")

ZIE3_21_fix$WC_30cm[ZIE3_21_fix$WC_30cm < 0.130] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_30cm[1] <- head(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_30cm[nrow(data)] <- tail(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_30cm[idx] <- (ZIE3_21_fix$WC_30cm[r$starts[i]] + ZIE3_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-07-27 12:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-07-30 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#Subset and fix drips later in the year 
#================================================================================
ZIE3_21_fix <- filter(ZIE3_21, Date_time > "2021-08-01 12:00:01")
ZIE3_21_fix <- filter(ZIE3_21_fix, Date_time < "2021-08-30 12:00:01")

ZIE3_21_fix$WC_30cm[ZIE3_21_fix$WC_30cm < 0.125] <- NA
missing <- which(is.na(ZIE3_21_fix$WC_30cm))

if(1 %in% missing){
  ZIE3_21_fix$WC_30cm[1] <- head(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}
if(nrow(ZIE3_21_fix) %in% missing){
  ZIE3_21_fix$WC_30cm[nrow(data)] <- tail(ZIE3_21_fix$WC_30cm[!is.na(ZIE3_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21_fix$WC_30cm[idx] <- (ZIE3_21_fix$WC_30cm[r$starts[i]] + ZIE3_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE3_21_early <- filter(ZIE3_21, Date_time < "2021-08-01 12:00:01")
ZIE3_21_later <- filter(ZIE3_21, Date_time > "2021-08-30 12:00:01")

ZIE3_21 <- bind_rows(ZIE3_21_early, ZIE3_21_later, ZIE3_21_fix)

#100 cm 
################################################################
ZIE3_21$WC_100cm[ZIE3_21$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE3_19$WC_100cm))

if(1 %in% missing){
  ZIE3_21$WC_100cm[1] <- head(ZIE3_21$WC_100cm[!is.na(ZIE3_21$WC_100cm)],1)
}
if(nrow(ZIE3_21) %in% missing){
  ZIE3_21$WC_100cm[nrow(data)] <- tail(ZIE3_21$WC_100cm[!is.na(ZIE3_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE3_21$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE3_21$WC_100cm[idx] <- (ZIE3_21$WC_100cm[r$starts[i]] + ZIE3_21$WC_100cm[r$ends[i]])/2
}

#Add in missing dates in April through June
#================================================================================
#Replace missing dates with NAs - 04/07 to 05/28 
insertDF <- as.data.frame(matrix(data = NA, nrow = 50, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-04-08"), as.Date("2021-05-27"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE3_21<- insertRows(ZIE3_21, c(13860:13909), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(ZIE3_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
ZIE3_clean <- merge(ZIE3_18, ZIE3_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
ZIE3_clean <- merge(ZIE3_clean, ZIE3_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE3_clean <- merge(ZIE3_clean, ZIE3_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE3_clean <- merge(ZIE3_clean, ZIE3_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(ZIE3_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("ZIE3_Salli", width = 4500, height = 2500)

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
write.csv(ZIE3_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE3_clean.csv" ) #this writes a csv file and sends it to the working folder





