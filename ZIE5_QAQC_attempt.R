#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/15/2023
#Description: QA/QC ZIE 5

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

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE", 
                        pattern=glob2rx("Copy of Z5M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
ZIE5_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("Copy of Z5M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
ZIE5_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
ZIE5 <- rbind(ZIE5_2017_2019, ZIE5_2019_2021)

#Write the csv
write.csv(ZIE5,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE5.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(ZIE5)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
ZIE5$Date <- mdy_hms(ZIE5$Date_time)

#Put year into a separate column 
ZIE5 <- separate(ZIE5, Date, c("Year"))

#ZIE5 2017
##################################################################################################
ZIE5_17 <- subset(ZIE5, Year == '2017')

#Plotting 
ZIE5_17$WC_15cm <- as.numeric(ZIE5_17$WC_15cm)
ZIE5_17$WC_30cm <- as.numeric(ZIE5_17$WC_30cm)
ZIE5_17$WC_100cm <- as.numeric(ZIE5_17$WC_100cm)
ZIE5_17$Date_time<- mdy_hms(ZIE5_17$Date_time)

Soil <- ggplot(data = subset(ZIE5_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE5 2018
##################################################################################################
ZIE5_18 <- subset(ZIE5, Year == '2018')

#Plotting 
ZIE5_18$WC_15cm <- as.numeric(ZIE5_18$WC_15cm)
ZIE5_18$WC_30cm <- as.numeric(ZIE5_18$WC_30cm)
ZIE5_18$WC_100cm <- as.numeric(ZIE5_18$WC_100cm)
ZIE5_18$Date_time<- mdy_hms(ZIE5_18$Date_time)

#15 cm
###########################################
ZIE5_18$WC_15cm[ZIE5_18$WC_15cm < 0.15] <- NA
missing <- which(is.na(ZIE5_18$WC_15cm))

if(1 %in% missing){
  ZIE5_18$WC_15cm[1] <- head(ZIE5_18$WC_15cm[!is.na(ZIE5_18$WC_15cm)],1)
}
if(nrow(ZIE5_18) %in% missing){
  ZIE5_18$WC_15cm[nrow(data)] <- tail(ZIE5_18$WC_15cm[!is.na(ZIE5_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18$WC_15cm[idx] <- (ZIE5_18$WC_15cm[r$starts[i]] + ZIE5_18$WC_15cm[r$ends[i]])/2
}

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-03-31 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-25 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.275] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-03-31 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-25 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-26 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-06-25 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.22] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-26 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-06-25 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-07-01 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-08-25 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.17] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-07-01 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-08-25 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-07-01 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-08-25 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.17] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}


#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-07-01 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-08-25 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-08 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-10-12 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.26] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-08 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-10-12 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-15 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-10-22 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.225] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-15 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-10-22 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-20 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-10-31 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.28] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}


#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-20 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-10-31 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-20 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-31 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.281] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-20 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-31 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-03-27 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-04-01 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.3] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-03-27 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-04-01 10:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-27 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-06-11 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.26] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-27 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-06-11 10:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-27 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-06-04 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.276] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-27 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-06-04 10:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-04-23 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-10 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.3] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-04-23 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-10 10:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-07 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-10-15 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.225] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-07 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-10-15 10:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-07-07 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-08-15 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.18] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-07-07 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-08-15 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-07-01 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-07-09 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.206] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-07-01 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-07-09 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-07-10 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-08-01 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.186] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-07-10 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-08-01 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-08-15 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-09-03 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.1735] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-08-15 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-09-03 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-08-15 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-08-20 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.181] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-08-15 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-08-20 10:00:01")

ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-08-20 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-08-30 10:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.176] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-08-20 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-08-30 10:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-03-25 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-03-27 00:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.322] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-03-25 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-03-27 00:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-03-30 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-04-01 00:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.309] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-03-30 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-04-01 00:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-04-02 15:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-04-04 00:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.311] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-04-02 15:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-04-04 00:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-04-03 05:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-04-03 17:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.3195] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-04-03 05:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-04-03 17:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Calibrate
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-04-03 18:20:01")

ZIE5_18_fix$WC_15cm <- ZIE5_18_fix$WC_15cm + 0.005

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-04-03 18:20:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_fix)

#Calibrate
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-04-01 16:40:01")

ZIE5_18_fix$WC_15cm <- ZIE5_18_fix$WC_15cm + 0.0195

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-04-01 16:40:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-04-01 15:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-04-13 17:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.325] <- NA

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-04-01 15:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-04-13 17:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-04-11 15:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-04-20 17:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.342] <- NA

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-04-11 15:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-04-20 17:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-04-20 15:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-02 17:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.333] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-04-20 15:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-02 17:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-04-29 04:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-02 17:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.3375] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-04-29 04:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-02 17:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-07 04:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-10 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.3275] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-07 04:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-10 00:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-21 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-22 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.3095] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-21 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-22 00:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-22 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-30 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.3075] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-22 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-30 00:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-23 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-25 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm == 0.30865] <- NA

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-23 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-25 00:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-24 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-28 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.317] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-24 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-28 00:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-28 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-05-31 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.313] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-28 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-05-31 00:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-05-31 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-06-01 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.3115] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-05-31 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-06-01 00:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-06-01 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-06-02 04:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.3072] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-06-01 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-06-02 04:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-06-02 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-06-10 04:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.291] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-06-02 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-06-10 04:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-06-10 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-06-13 20:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.281] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-06-10 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-06-13 20:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-06-13 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-06-25 20:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.257] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-06-13 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-06-25 20:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-07-15 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-07-18 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.2215] <- NA

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-07-15 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-07-18 00:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-07-18 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-07-23 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.216] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-07-18 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-07-23 00:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-07-23 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-07-28 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.214] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-07-23 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-07-28 00:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-07-27 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-07-28 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.215] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-07-27 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-07-28 00:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-08-03 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-08-11 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.206] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-08-03 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-08-11 00:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-08-03 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-08-11 00:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.206] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-08-03 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-08-11 00:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-16 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-10-19 00:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.256] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-16 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-10-19 00:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-09-25 03:40:01")

ZIE5_18_fix$WC_15cm <- ZIE5_18_fix$WC_15cm + 0.0038

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-09-25 03:40:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-10 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-10-11 00:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.2965] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-10 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-10-11 00:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-11 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-10-11 15:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.2932] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-11 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-10-11 15:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-16 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-10-17 05:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.263] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-16 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-10-17 05:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-16 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-10-20 05:00:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm < 0.256] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_15cm[1] <- head(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_15cm[nrow(data)] <- tail(ZIE5_18_fix$WC_15cm[!is.na(ZIE5_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_15cm[idx] <- (ZIE5_18_fix$WC_15cm[r$starts[i]] + ZIE5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-16 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-10-20 05:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Remove glitches in 15 cm in October/November
#====================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-10-22 15:10:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-11-21 04:50:01")

ZIE5_18_fix$WC_15cm[ZIE5_18_fix$WC_15cm > 0] <- NA

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-10-22 15:10:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-11-21 04:50:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#30 cm 
################################################################
ZIE5_18$WC_30cm[ZIE5_18$WC_30cm < 0] <- NA
missing <- which(is.na(ZIE5_18$WC_30cm))

if(1 %in% missing){
  ZIE5_18$WC_30cm[1] <- head(ZIE5_18$WC_30cm[!is.na(ZIE5_18$WC_30cm)],1)
}
if(nrow(ZIE5_18) %in% missing){
  ZIE5_18$WC_30cm[nrow(data)] <- tail(ZIE5_18$WC_30cm[!is.na(ZIE5_18$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18$WC_30cm[idx] <- (ZIE5_18$WC_30cm[r$starts[i]] + ZIE5_18$WC_30cm[r$ends[i]])/2
}

#Get rid of glitch
#=============================================================
ZIE5_18$WC_30cm[ZIE5_18$WC_30cm > 0.30274 & ZIE5_18$WC_30cm < 0.30276] <- NA

#Get rid of 0.3040 glitch glitch
#=============================================================
ZIE5_18$WC_30cm[ZIE5_18$WC_30cm > 0.30399 & ZIE5_18$WC_30cm < 0.30401] <- NA

#Get rid of 0.3040 glitch glitch
#=============================================================
ZIE5_18$WC_30cm[ZIE5_18$WC_30cm == 0.3406] <- NA

#Get rid of 0.3040 glitch glitch
#=============================================================
ZIE5_18$WC_30cm[ZIE5_18$WC_30cm > 0.306149 & ZIE5_18$WC_30cm < 0.306151] <- NA


#100 cm 
################################################################
ZIE5_18$WC_100cm[ZIE5_18$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE5_18$WC_100cm))

if(1 %in% missing){
  ZIE5_18$WC_100cm[1] <- head(ZIE5_18$WC_100cm[!is.na(ZIE5_18$WC_100cm)],1)
}
if(nrow(ZIE5_18) %in% missing){
  ZIE5_18$WC_100cm[nrow(data)] <- tail(ZIE5_18$WC_100cm[!is.na(ZIE5_18$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18$WC_100cm[idx] <- (ZIE5_18$WC_100cm[r$starts[i]] + ZIE5_18$WC_100cm[r$ends[i]])/2
}

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-12-03 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-12-16 05:00:01")

ZIE5_18_fix$WC_100cm[ZIE5_18_fix$WC_100cm < 0.483] <- NA

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-12-03 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-12-16 05:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-01-08 10:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-01-09 15:00:01")

ZIE5_18_fix$WC_100cm[ZIE5_18_fix$WC_100cm < 0.5] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_100cm[1] <- head(ZIE5_18_fix$WC_100cm[!is.na(ZIE5_18_fix$WC_100cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_100cm[nrow(data)] <- tail(ZIE5_18_fix$WC_100cm[!is.na(ZIE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_100cm[idx] <- (ZIE5_18_fix$WC_100cm[r$starts[i]] + ZIE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-01-08 10:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-01-09 15:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-01-16 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-01-19 15:00:01")

ZIE5_18_fix$WC_100cm[ZIE5_18_fix$WC_100cm < 0.4925] <- NA

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-01-16 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-01-19 15:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Subset to remove drips
#===================================================================
ZIE5_18_fix <- filter(ZIE5_18, Date_time > "2018-01-20 00:00:01")
ZIE5_18_fix <- filter(ZIE5_18_fix, Date_time < "2018-01-29 15:00:01")

ZIE5_18_fix$WC_100cm[ZIE5_18_fix$WC_100cm < 0.495] <- NA
missing <- which(is.na(ZIE5_18_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_18_fix$WC_100cm[1] <- head(ZIE5_18_fix$WC_100cm[!is.na(ZIE5_18_fix$WC_100cm)],1)
}
if(nrow(ZIE5_18_fix) %in% missing){
  ZIE5_18_fix$WC_100cm[nrow(data)] <- tail(ZIE5_18_fix$WC_100cm[!is.na(ZIE5_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_18_fix$WC_100cm[idx] <- (ZIE5_18_fix$WC_100cm[r$starts[i]] + ZIE5_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_18_early <- filter(ZIE5_18, Date_time < "2018-01-20 00:00:01")
ZIE5_18_late <- filter(ZIE5_18, Date_time > "2018-01-29 15:00:01")
ZIE5_18 <- bind_rows(ZIE5_18_early, ZIE5_18_late, ZIE5_18_fix)

#Plot again 
Soil <- ggplot(data = subset(ZIE5_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE5 2019
##################################################################################################
ZIE5_19 <- subset(ZIE5, Year == '2019')

#Plotting 
ZIE5_19$WC_15cm <- as.numeric(ZIE5_19$WC_15cm)
ZIE5_19$WC_30cm <- as.numeric(ZIE5_19$WC_30cm)
ZIE5_19$WC_100cm <- as.numeric(ZIE5_19$WC_100cm)
ZIE5_19$Date_time<- mdy_hms(ZIE5_19$Date_time)

#15 cm
###########################################
ZIE5_19$WC_15cm[ZIE5_19$WC_15cm < 0] <- NA
missing <- which(is.na(ZIE5_19$WC_15cm))

if(1 %in% missing){
  ZIE5_19$WC_15cm[1] <- head(ZIE5_19$WC_15cm[!is.na(ZIE5_19$WC_15cm)],1)
}
if(nrow(ZIE5_19) %in% missing){
  ZIE5_19$WC_15cm[nrow(data)] <- tail(ZIE5_19$WC_15cm[!is.na(ZIE5_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19$WC_15cm[idx] <- (ZIE5_19$WC_15cm[r$starts[i]] + ZIE5_19$WC_15cm[r$ends[i]])/2
}

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-08-21 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-08-25 05:00:01")

Soil <- ggplot(data = subset(ZIE5_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "navyblue")) 
Soil 

ZIE5_19_fix$WC_15cm[ZIE5_19_fix$WC_15cm > 0.209 | ZIE5_19_fix$WC_15cm < 0.199] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_15cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_15cm[1] <- head(ZIE5_19_fix$WC_15cm[!is.na(ZIE5_19_fix$WC_15cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_15cm[nrow(data)] <- tail(ZIE5_19_fix$WC_15cm[!is.na(ZIE5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_15cm[idx] <- (ZIE5_19_fix$WC_15cm[r$starts[i]] + ZIE5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-08-21 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-08-25 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#30 cm 
################################################################
ZIE5_19$WC_30cm[ZIE5_19$WC_30cm < 0.15] <- NA
missing <- which(is.na(ZIE5_19$WC_30cm))

if(1 %in% missing){
  ZIE5_19$WC_30cm[1] <- head(ZIE5_19$WC_30cm[!is.na(ZIE5_19$WC_30cm)],1)
}
if(nrow(ZIE5_19) %in% missing){
  ZIE5_19$WC_30cm[nrow(data)] <- tail(ZIE5_19$WC_30cm[!is.na(ZIE5_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19$WC_30cm[idx] <- (ZIE5_19$WC_30cm[r$starts[i]] + ZIE5_19$WC_30cm[r$ends[i]])/2
}

#Remove glitch at beginning of year for 0.3231 
#============================================================================
ZIE5_19$WC_30cm[ZIE5_19$WC_30cm == 0.3231] <- NA

#100 cm 
################################################################
ZIE5_19$WC_100cm[ZIE5_19$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE5_19$WC_100cm))

if(1 %in% missing){
  ZIE5_19$WC_100cm[1] <- head(ZIE5_19$WC_100cm[!is.na(ZIE5_19$WC_100cm)],1)
}
if(nrow(ZIE5_19) %in% missing){
  ZIE5_19$WC_100cm[nrow(data)] <- tail(ZIE5_19$WC_100cm[!is.na(ZIE5_19$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19$WC_100cm[idx] <- (ZIE5_19$WC_100cm[r$starts[i]] + ZIE5_19$WC_100cm[r$ends[i]])/2
}

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-09-03 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-09-16 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.325] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-09-03 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-09-16 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-09-07 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-09-16 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.316] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-09-07 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-09-16 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-09-10 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-09-16 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.3125] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-09-10 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-09-16 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-09-13 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-09-16 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.3085] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-09-13 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-09-16 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)


#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-09-13 14:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-09-16 05:00:01")

Soil <- ggplot(data = subset(ZIE5_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil 

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.3055] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-09-13 14:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-09-16 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-09-20 14:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-09-26 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.301] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-09-20 14:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-09-26 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-09-23 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-09-26 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.298] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-09-23 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-09-26 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-10-07 10:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-10-30 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.2846] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-10-07 10:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-10-30 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-10-17 10:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-10-30 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.278] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-10-17 10:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-10-30 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-10-20 10:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-10-30 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.276] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-10-20 10:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-10-30 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-10-26 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-10-31 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.272] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-10-26 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-10-31 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-10-29 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-11-07 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.269] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-10-29 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-11-07 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-10-31 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-11-07 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.2672] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-10-31 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-11-07 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-11-01 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-11-07 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.2665] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-11-01 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-11-07 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-11-05 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-11-07 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.2645] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-11-05 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-11-07 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-11-05 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-11-25 05:00:01")

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.266] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-11-05 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-11-25 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Subset and remove glitches 
#=====================================================================
ZIE5_19_fix <- filter(ZIE5_19, Date_time > "2019-03-05 00:00:01")
ZIE5_19_fix <- filter(ZIE5_19_fix, Date_time < "2019-05-25 05:00:01")

Soil <- ggplot(data = subset(ZIE5_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue"))
Soil 

ZIE5_19_fix$WC_100cm[ZIE5_19_fix$WC_100cm > 0.51] <- NA
missing <- which(is.na(ZIE5_19_fix$WC_100cm))

if(1 %in% missing){
  ZIE5_19_fix$WC_100cm[1] <- head(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_19_fix$WC_100cm[nrow(data)] <- tail(ZIE5_19_fix$WC_100cm[!is.na(ZIE5_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_19_fix$WC_100cm[idx] <- (ZIE5_19_fix$WC_100cm[r$starts[i]] + ZIE5_19_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_19_early <- filter(  ZIE5_19, Date_time < "2019-03-05 00:00:01")
ZIE5_19_late <- filter(  ZIE5_19, Date_time > "2019-05-25 05:00:01")
ZIE5_19 <- bind_rows( ZIE5_19_early, ZIE5_19_late, ZIE5_19_fix)

#Plot again 
Soil <- ggplot(data = subset(ZIE5_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE5 2020
##################################################################################################
ZIE5_20 <- subset(ZIE5, Year == '2020')

#Plotting 
ZIE5_20$WC_15cm <- as.numeric(ZIE5_20$WC_15cm)
ZIE5_20$WC_30cm <- as.numeric(ZIE5_20$WC_30cm)
ZIE5_20$WC_100cm <- as.numeric(ZIE5_20$WC_100cm)
ZIE5_20$Date_time<- mdy_hms(ZIE5_20$Date_time)

#15 cm 
################################################################

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-08-07 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-08-24 1:00:01")

ZIE5_20fix$WC_15cm[ZIE5_20fix$WC_15cm < 0.186] <- NA
missing <- which(is.na(ZIE5_20fix$WC_15cm))

if(1 %in% missing){
  ZIE5_20fix$WC_15cm[1] <- head(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_15cm[nrow(data)] <- tail(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_15cm[idx] <- (ZIE5_20fix$WC_15cm[r$starts[i]] + ZIE5_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-08-07 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-08-24 1:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-08-24 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-09-09 1:00:01")

ZIE5_20fix$WC_15cm[ZIE5_20fix$WC_15cm < 0.179] <- NA
missing <- which(is.na(ZIE5_20fix$WC_15cm))

if(1 %in% missing){
  ZIE5_20fix$WC_15cm[1] <- head(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_15cm[nrow(data)] <- tail(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_15cm[idx] <- (ZIE5_20fix$WC_15cm[r$starts[i]] + ZIE5_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-08-24 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-09-09 1:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-09-14 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-09-30 23:00:01")

ZIE5_20fix$WC_15cm[ZIE5_20fix$WC_15cm < 0.174] <- NA
missing <- which(is.na(ZIE5_20fix$WC_15cm))

if(1 %in% missing){
  ZIE5_20fix$WC_15cm[1] <- head(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_15cm[nrow(data)] <- tail(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_15cm[idx] <- (ZIE5_20fix$WC_15cm[r$starts[i]] + ZIE5_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-09-14 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-09-30 23:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-09-30 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-10-17 13:00:01")

ZIE5_20fix$WC_15cm[ZIE5_20fix$WC_15cm < 0.172] <- NA
missing <- which(is.na(ZIE5_20fix$WC_15cm))

if(1 %in% missing){
  ZIE5_20fix$WC_15cm[1] <- head(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_15cm[nrow(data)] <- tail(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_15cm[idx] <- (ZIE5_20fix$WC_15cm[r$starts[i]] + ZIE5_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-09-30 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-10-17 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-11-03 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-11-06 13:00:01")

ZIE5_20fix$WC_15cm[ZIE5_20fix$WC_15cm < 0.16] <- NA
missing <- which(is.na(ZIE5_20fix$WC_15cm))

if(1 %in% missing){
  ZIE5_20fix$WC_15cm[1] <- head(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_15cm[nrow(data)] <- tail(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_15cm[idx] <- (ZIE5_20fix$WC_15cm[r$starts[i]] + ZIE5_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-11-03 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-11-06 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-11-09 10:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-11-10 13:00:01")

ZIE5_20fix$WC_15cm[ZIE5_20fix$WC_15cm > 0.207] <- NA
missing <- which(is.na(ZIE5_20fix$WC_15cm))

if(1 %in% missing){
  ZIE5_20fix$WC_15cm[1] <- head(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_15cm[nrow(data)] <- tail(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_15cm[idx] <- (ZIE5_20fix$WC_15cm[r$starts[i]] + ZIE5_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-11-09 10:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-11-10 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-03-06 10:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-03-13 13:00:01")

ZIE5_20fix$WC_15cm[ZIE5_20fix$WC_15cm > 0.294] <- NA
missing <- which(is.na(ZIE5_20fix$WC_15cm))

if(1 %in% missing){
  ZIE5_20fix$WC_15cm[1] <- head(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_15cm[nrow(data)] <- tail(ZIE5_20fix$WC_15cm[!is.na(ZIE5_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_15cm[idx] <- (ZIE5_20fix$WC_15cm[r$starts[i]] + ZIE5_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-03-06 10:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-03-13 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Remove glitch in September 
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-09-14 10:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-10-13 13:00:01")

ZIE5_20fix$WC_15cm[ZIE5_20fix$WC_15cm == 0.1742] <- NA

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-09-14 10:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-10-13 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Remove glitch in September 
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-06-09 04:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-07-17 11:10:01")

ZIE5_20fix$WC_15cm <- ZIE5_20fix$WC_15cm + 0.0043

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-06-09 04:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-07-17 11:10:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)


#30 cm 
##################################################################

#Calibrate 
#=================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-02-23 18:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-02-29 00:00:01")

ZIE5_20fix$WC_30cm <- ZIE5_20fix$WC_30cm + 0.003

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-02-23 18:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-02-29 00:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Remove glitches from calibration
#======================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-02-23 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-03-01 00:00:01")

ZIE5_20fix$WC_30cm[ZIE5_20fix$WC_30cm > 0.3425] <- NA
missing <- which(is.na(ZIE5_20fix$WC_30cm))

if(1 %in% missing){
  ZIE5_20fix$WC_30cm[1] <- head(ZIE5_20fix$WC_30cm[!is.na(ZIE5_20fix$WC_30cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_30cm[nrow(data)] <- tail(ZIE5_20fix$WC_30cm[!is.na(ZIE5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_30cm[idx] <- (ZIE5_20fix$WC_30cm[r$starts[i]] + ZIE5_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-02-23 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-03-01 00:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Remove glitches from calibration
#======================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-02-28 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-03-01 00:00:01")

Soil <- ggplot(data = subset(ZIE5_20fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "navyblue"))
Soil 

ZIE5_20fix$WC_30cm[ZIE5_20fix$WC_30cm > 0.3405] <- NA
missing <- which(is.na(ZIE5_20fix$WC_30cm))

if(1 %in% missing){
  ZIE5_20fix$WC_30cm[1] <- head(ZIE5_20fix$WC_30cm[!is.na(ZIE5_20fix$WC_30cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_30cm[nrow(data)] <- tail(ZIE5_20fix$WC_30cm[!is.na(ZIE5_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_30cm[idx] <- (ZIE5_20fix$WC_30cm[r$starts[i]] + ZIE5_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-02-28 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-03-01 00:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#100 cm 
################################################################
ZIE5_20$WC_100cm[ZIE5_20$WC_100cm < 0.23] <- NA
missing <- which(is.na(ZIE5_19$WC_100cm))

if(1 %in% missing){
  ZIE5_20$WC_100cm[1] <- head(ZIE5_20$WC_100cm[!is.na(ZIE5_20$WC_100cm)],1)
}
if(nrow(ZIE5_20) %in% missing){
  ZIE5_20$WC_100cm[nrow(data)] <- tail(ZIE5_20$WC_100cm[!is.na(ZIE5_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20$WC_100cm[idx] <- (ZIE5_20$WC_100cm[r$starts[i]] + ZIE5_20$WC_100cm[r$ends[i]])/2
}

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-01-01 10:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-03-30 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm < 0.496 | ZIE5_20fix$WC_100cm > 0.502] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-01-01 10:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-03-30 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-01-01 10:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-01-30 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.5013 | ZIE5_20fix$WC_100cm < 0.498] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-01-01 10:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-01-30 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-01-30 10:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-02-17 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.5015 | ZIE5_20fix$WC_100cm < 0.498] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-01-30 10:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-02-17 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-02-04 10:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-02-10 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.500 | ZIE5_20fix$WC_100cm < 0.498] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-02-04 10:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-02-10 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-06-10 10:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-07-10 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.396] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-06-10 10:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-07-10 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-06-14 10:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-07-10 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.39] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-06-14 10:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-07-10 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-06-22 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-07-10 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.375] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-06-22 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-07-10 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-05-20 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-07-10 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.41] <- NA

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-05-20 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-07-10 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-06-22 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-07-10 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.3725] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-06-22 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-07-10 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)


#Calibrate 
#=====================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-07-01 14:30:01")

ZIE5_20fix$WC_100cm <- ZIE5_20fix$WC_100cm + 0.0042

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-07-01 14:30:01")
ZIE5_20<- bind_rows(ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-07-06 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-07-20 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.36] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time <  "2020-07-06 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-07-20 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Calibrate (potentially caused missing time in the data, but should be fixed)
#=======================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-07-18 21:20:01")

ZIE5_20fix$WC_100cm <- ZIE5_20fix$WC_100cm + 0.01

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-07-18 21:20:01")
ZIE5_20<- bind_rows(ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-08-24 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-09-20 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.2925] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_19_fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-08-24 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-09-20 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-09-07 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-09-20 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.288] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-09-07 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-09-20 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-09-15 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-09-30 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.282] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-09-15 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-09-30 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-09-30 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-10-30 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm > 0.275] <- NA
missing <- which(is.na(ZIE5_20fix$WC_100cm))

if(1 %in% missing){
  ZIE5_20fix$WC_100cm[1] <- head(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}
if(nrow(ZIE5_20fix) %in% missing){
  ZIE5_20fix$WC_100cm[nrow(data)] <- tail(ZIE5_20fix$WC_100cm[!is.na(ZIE5_20fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_20fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_20fix$WC_100cm[idx] <- (ZIE5_20fix$WC_100cm[r$starts[i]] + ZIE5_20fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-09-30 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-10-30 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-04-30 00:00:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-05-30 13:00:01")

ZIE5_20fix$WC_100cm[ZIE5_20fix$WC_100cm < 0.5] <- NA

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-04-30 00:00:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-05-30 13:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Fix upwards drips
#============================================================================
ZIE5_20fix <- filter(ZIE5_20, Date_time > "2020-07-12 17:20:01")
ZIE5_20fix <- filter(ZIE5_20fix, Date_time < "2020-07-19 00:00:01")

Soil <- ggplot(data = subset(ZIE5_20fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil 

ZIE5_20fix$WC_100cm <- ZIE5_20fix$WC_100cm + 0.005

#Recombine
ZIE5_20early <- filter(ZIE5_20, Date_time < "2020-07-12 17:20:01")
ZIE5_20late <- filter(ZIE5_20, Date_time > "2020-07-19 00:00:01")
ZIE5_20<- bind_rows(ZIE5_20late, ZIE5_20early, ZIE5_20fix)

#Plot again 
Soil <- ggplot(data = subset(ZIE5_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE5 2021
##################################################################################################
ZIE5_21 <- subset(ZIE5, Year == '2021')

#Plotting 
ZIE5_21$WC_15cm <- as.numeric(ZIE5_21$WC_15cm)
ZIE5_21$WC_30cm <- as.numeric(ZIE5_21$WC_30cm)
ZIE5_21$WC_100cm <- as.numeric(ZIE5_21$WC_100cm)
ZIE5_21$Date_time<- mdy_hms(ZIE5_21$Date_time)

#30 cm 
########################################################################################
ZIE5_21$WC_30cm[ZIE5_21$WC_30cm < 0] <- NA
missing <- which(is.na(ZIE5_21$WC_30cm))

if(1 %in% missing){
  ZIE5_21$WC_30cm[1] <- head(ZIE5_21$WC_30cm[!is.na(ZIE5_21$WC_30cm)],1)
}
if(nrow(ZIE5_21) %in% missing){
  ZIE5_21$WC_30cm[nrow(data)] <- tail(ZIE5_21$WC_30cm[!is.na(ZIE5_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE5_21$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE5_21$WC_30cm[idx] <- (ZIE5_21$WC_30cm[r$starts[i]] + ZIE5_21$WC_30cm[r$ends[i]])/2
}

#100 cm 
################################################################

#Subset and remove glitch at beginning of year 
#=============================================================================
ZIE5_21_fix <- filter(ZIE5_21, Date_time > "2021-01-01 15:10:01")
ZIE5_21_fix <- filter(ZIE5_21_fix, Date_time < "2021-01-15 04:50:01")

Soil <- ggplot(data = subset(ZIE5_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil 

ZIE5_21_fix$WC_100cm[ZIE5_21_fix$WC_100cm < 0.49] <- NA

#Recombine
ZIE5_21_early <- filter(ZIE5_21, Date_time < "2021-01-01 15:10:01")
ZIE5_21_late <- filter(ZIE5_21, Date_time > "2021-01-15 04:50:01")
ZIE5_21 <- bind_rows(ZIE5_21_early, ZIE5_21_late, ZIE5_21_fix)

#Plot again 
Soil <- ggplot(data = subset(ZIE5_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
ZIE5_clean <- merge(ZIE5_18, ZIE5_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
ZIE5_clean <- merge(ZIE5_clean, ZIE5_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE5_clean <- merge(ZIE5_clean, ZIE5_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE5_clean <- merge(ZIE5_clean, ZIE5_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(ZIE5_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("ZIE5", width = 4500, height = 2500)

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
write.csv(ZIE5_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE5_clean.csv") #this writes a csv file and sends it to the working folder
