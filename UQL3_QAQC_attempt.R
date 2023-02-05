#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/05/2023
#Description: QA/QC UQL 3

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

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL", 
                        pattern=glob2rx("Copy of U3M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
UQL3_2019_2021 <- data19_21 %>%
  rename_all(funs(c("Date", "Time", "PAR", "WC_15cm", "WC_30cm","WC_100cm", "Date_time"))) %>% #this renames all of the columns
  select(Date_time, PAR, WC_15cm, WC_30cm, WC_100cm) #this keeps only the columns you will use


#2017-2019 FILES
#========================================================================================================================
#If wanting to merge the files, needed to manually delete the extra column in W1M190111 and 
#extra row at the top, and needed to manually delete the extra columns in W1M171228, W1M180201, and W1M180302

#Set the data path 
data_path <- "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL" 
new_col_name <- c("Date_time", "PAR", "WC_15cm", "WC_30cm", "WC_100cm")

#Call in all the files in this folder with the W2M1 pattern, which excludes the datasheets from 2019-2020
files <- dir(data_path, pattern=glob2rx("Copy of U3M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
UQL3_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
UQL3 <- rbind(UQL3_2017_2019, UQL3_2019_2021)

#Write the csv
write.csv(UQL3,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL3.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(UQL3)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
UQL3$Date <- mdy_hms(UQL3$Date_time)

#Put year into a separate column 
UQL3 <- separate(UQL3, Date, c("Year"))

#UQL3 2017
##################################################################################################
UQL3_17 <- subset(UQL3, Year == '2017')

#Plotting 
UQL3_17$WC_15cm <- as.numeric(UQL3_17$WC_15cm)
UQL3_17$WC_30cm <- as.numeric(UQL3_17$WC_30cm)
UQL3_17$WC_100cm <- as.numeric(UQL3_17$WC_100cm)
UQL3_17$Date_time<- mdy_hms(UQL3_17$Date_time)

Soil <- ggplot(data = subset(UQL3_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 
##################################################################################################

#UQL3 2018
##################################################################################################
UQL3_18 <- subset(UQL3, Year == '2018')

#Plotting 
UQL3_18$WC_15cm <- as.numeric(UQL3_18$WC_15cm)
UQL3_18$WC_30cm <- as.numeric(UQL3_18$WC_30cm)
UQL3_18$WC_100cm <- as.numeric(UQL3_18$WC_100cm)
UQL3_18$Date_time<- mdy_hms(UQL3_18$Date_time)

#15 cm
#######################################################################
UQL3_18$WC_15cm[UQL3_18$WC_15cm < 0] <- NA
missing <- which(is.na(UQL3_18$WC_15cm))

if(1 %in% missing){
  UQL3_18$WC_15cm[1] <- head(UQL3_18$WC_15cm[!is.na(UQL3_18$WC_15cm)],1)
}
if(nrow(UQL3_18) %in% missing){
  UQL3_18$WC_15cm[nrow(data)] <- tail(UQL3_18$WC_15cm[!is.na(UQL3_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_18$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_18$WC_15cm[idx] <- (UQL3_18$WC_15cm[r$starts[i]] + UQL3_18$WC_15cm[r$ends[i]])/2
}

#Remove glitches
#=================================================================================
UQL3_18_fix <- filter(UQL3_18, Date_time > "2018-02-08 1:00:01")
UQL3_18_fix <- filter(UQL3_18_fix, Date_time < "2018-03-01 1:00:01")

UQL3_18_fix$WC_15cm[UQL3_18_fix$WC_15cm < 0.325] <- NA
missing <- which(is.na(UQL3_18_fix$WC_15cm))

if(1 %in% missing){
  UQL3_18_fix$WC_15cm[1] <- head(UQL3_18_fix$WC_15cm[!is.na(UQL3_18_fix$WC_15cm)],1)
}
if(nrow(UQL3_18_fix) %in% missing){
  UQL3_18_fix$WC_15cm[nrow(data)] <- tail(UQL3_18_fix$WC_15cm[!is.na(UQL3_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_18_fix$WC_15cm[idx] <- (UQL3_18_fix$WC_15cm[r$starts[i]] + UQL3_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_18_later <- filter(UQL3_18, Date_time < "2018-02-08 1:00:01")
UQL3_18_end <- filter(UQL3_18, Date_time  > "2018-03-01 1:00:01")
UQL3_18 <- bind_rows(UQL3_18_fix, UQL3_18_later, UQL3_18_end)

#Remove glitches
#=================================================================================
UQL3_18_fix <- filter(UQL3_18, Date_time > "2018-03-07 1:00:01")
UQL3_18_fix <- filter(UQL3_18_fix, Date_time < "2018-03-08 1:00:01")

UQL3_18_fix$WC_15cm[UQL3_18_fix$WC_15cm < 0.334] <- NA
missing <- which(is.na(UQL3_18_fix$WC_15cm))

if(1 %in% missing){
  UQL3_18_fix$WC_15cm[1] <- head(UQL3_18_fix$WC_15cm[!is.na(UQL3_18_fix$WC_15cm)],1)
}
if(nrow(UQL3_18_fix) %in% missing){
  UQL3_18_fix$WC_15cm[nrow(data)] <- tail(UQL3_18_fix$WC_15cm[!is.na(UQL3_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_18_fix$WC_15cm[idx] <- (UQL3_18_fix$WC_15cm[r$starts[i]] + UQL3_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_18_later <- filter(UQL3_18, Date_time < "2018-03-07 1:00:01")
UQL3_18_end <- filter(UQL3_18, Date_time  > "2018-03-08 1:00:01")
UQL3_18 <- bind_rows(UQL3_18_fix, UQL3_18_later, UQL3_18_end)

#Remove glitches
#=================================================================================
UQL3_18_fix <- filter(UQL3_18, Date_time > "2018-05-15 1:00:01")
UQL3_18_fix <- filter(UQL3_18_fix, Date_time < "2018-05-21 1:00:01")

UQL3_18_fix$WC_15cm[UQL3_18_fix$WC_15cm < 0.322] <- NA

#Recombine July with other dataset 
UQL3_18_later <- filter(UQL3_18, Date_time < "2018-05-15 1:00:01")
UQL3_18_end <- filter(UQL3_18, Date_time  > "2018-05-21 1:00:01")
UQL3_18 <- bind_rows(UQL3_18_fix, UQL3_18_later, UQL3_18_end)

#Remove glitches
#=================================================================================
UQL3_18_fix <- filter(UQL3_18, Date_time > "2018-05-21 1:00:01")
UQL3_18_fix <- filter(UQL3_18_fix, Date_time < "2018-05-26 1:00:01")

UQL3_18_fix$WC_15cm[UQL3_18_fix$WC_15cm < 0.319] <- NA

#Recombine July with other dataset 
UQL3_18_later <- filter(UQL3_18, Date_time < "2018-05-21 1:00:01")
UQL3_18_end <- filter(UQL3_18, Date_time  > "2018-05-26 1:00:01")
UQL3_18 <- bind_rows(UQL3_18_fix, UQL3_18_later, UQL3_18_end)

#Remove glitches
#=================================================================================
UQL3_18_fix <- filter(UQL3_18, Date_time > "2018-09-29 1:00:01")
UQL3_18_fix <- filter(UQL3_18_fix, Date_time < "2018-11-26 1:00:01")

UQL3_18_fix$WC_15cm[UQL3_18_fix$WC_15cm < 0.205] <- NA

#Recombine July with other dataset 
UQL3_18_later <- filter(UQL3_18, Date_time < "2018-09-29 1:00:01")
UQL3_18_end <- filter(UQL3_18, Date_time  > "2018-11-26 1:00:01")
UQL3_18 <- bind_rows(UQL3_18_fix, UQL3_18_later, UQL3_18_end)

#Remove glitches
#=================================================================================
UQL3_18_fix <- filter(UQL3_18, Date_time > "2018-09-29 1:00:01")
UQL3_18_fix <- filter(UQL3_18_fix, Date_time < "2018-10-26 1:00:01")

UQL3_18_fix$WC_15cm[UQL3_18_fix$WC_15cm < 0.21] <- NA

#Recombine July with other dataset 
UQL3_18_later <- filter(UQL3_18, Date_time < "2018-09-29 1:00:01")
UQL3_18_end <- filter(UQL3_18, Date_time  > "2018-10-26 1:00:01")
UQL3_18 <- bind_rows(UQL3_18_fix, UQL3_18_later, UQL3_18_end)

#Remove glitches
#=================================================================================
UQL3_18_fix <- filter(UQL3_18, Date_time > "2018-10-25 1:00:01")
UQL3_18_fix <- filter(UQL3_18_fix, Date_time < "2018-11-01 1:00:01")

UQL3_18_fix$WC_15cm[UQL3_18_fix$WC_15cm < 0.209] <- NA

#Recombine July with other dataset 
UQL3_18_later <- filter(UQL3_18, Date_time < "2018-10-25 1:00:01")
UQL3_18_end <- filter(UQL3_18, Date_time  > "2018-11-01 1:00:01")
UQL3_18 <- bind_rows(UQL3_18_fix, UQL3_18_later, UQL3_18_end)

#Plot again 
Soil <- ggplot(data = subset(UQL3_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL3 2019
##################################################################################################
UQL3_19 <- subset(UQL3, Year == '2019')

#Plotting 
UQL3_19$WC_15cm <- as.numeric(UQL3_19$WC_15cm)
UQL3_19$WC_30cm <- as.numeric(UQL3_19$WC_30cm)
UQL3_19$WC_100cm <- as.numeric(UQL3_19$WC_100cm)
UQL3_19$Date_time<- mdy_hms(UQL3_19$Date_time)

#15 cm
###############################################################################
UQL3_19$WC_15cm[UQL3_19$WC_15cm < 0] <- NA
missing <- which(is.na(UQL3_19$WC_15cm))

if(1 %in% missing){
  UQL3_19$WC_15cm[1] <- head(UQL3_19$WC_15cm[!is.na(UQL3_19$WC_15cm)],1)
}
if(nrow(UQL3_19) %in% missing){
  UQL3_19$WC_15cm[nrow(data)] <- tail(UQL3_19$WC_15cm[!is.na(UQL3_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19$WC_15cm[idx] <- (UQL3_19$WC_15cm[r$starts[i]] + UQL3_19$WC_15cm[r$ends[i]])/2
}

#Fix 15 cm drips
#======================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-09-01 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-10-01 1:00:01")

UQL3_19_fix$WC_15cm[UQL3_19_fix$WC_15cm < 0.280] <- NA
missing <- which(is.na(UQL3_19_fix$WC_15cm))

if(1 %in% missing){
  UQL3_19_fix$WC_15cm[1] <- head(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_15cm[nrow(data)] <- tail(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_15cm[idx] <- (UQL3_19_fix$WC_15cm[r$starts[i]] + UQL3_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-09-01 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-10-01 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Fix 15 cm drips
#======================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-10-01 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-10-21 1:00:01")

UQL3_19_fix$WC_15cm[UQL3_19_fix$WC_15cm < 0.2784] <- NA
missing <- which(is.na(UQL3_19_fix$WC_15cm))

if(1 %in% missing){
  UQL3_19_fix$WC_15cm[1] <- head(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_15cm[nrow(data)] <- tail(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_15cm[idx] <- (UQL3_19_fix$WC_15cm[r$starts[i]] + UQL3_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-10-01 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-10-21 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Fix 15 cm drips
#======================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-09-10 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-10-01 1:00:01")

UQL3_19_fix$WC_15cm[UQL3_19_fix$WC_15cm > 0.284] <- NA
missing <- which(is.na(UQL3_19_fix$WC_15cm))

if(1 %in% missing){
  UQL3_19_fix$WC_15cm[1] <- head(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_15cm[nrow(data)] <- tail(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_15cm[idx] <- (UQL3_19_fix$WC_15cm[r$starts[i]] + UQL3_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-09-10 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-10-01 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Fix 15 cm drips
#======================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-05-02 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-05-11 1:00:01")

UQL3_19_fix$WC_15cm[UQL3_19_fix$WC_15cm < 0.3525] <- NA
missing <- which(is.na(UQL3_19_fix$WC_15cm))

if(1 %in% missing){
  UQL3_19_fix$WC_15cm[1] <- head(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_15cm[nrow(data)] <- tail(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_15cm[idx] <- (UQL3_19_fix$WC_15cm[r$starts[i]] + UQL3_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-05-02 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-05-11 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Fix 15 cm drips
#======================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-10-16 12:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-10-22 1:00:01")

UQL3_19_fix$WC_15cm[UQL3_19_fix$WC_15cm > 0.2815 | UQL3_19_fix$WC_15cm < 0.2805] <- NA
missing <- which(is.na(UQL3_19_fix$WC_15cm))

if(1 %in% missing){
  UQL3_19_fix$WC_15cm[1] <- head(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_15cm[nrow(data)] <- tail(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_15cm[idx] <- (UQL3_19_fix$WC_15cm[r$starts[i]] + UQL3_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-10-16 12:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-10-22 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Fix 15 cm drips
#======================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-11-04 12:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-11-22 1:00:01")

Soil <- ggplot(data = subset(UQL3_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

UQL3_19_fix$WC_15cm[UQL3_19_fix$WC_15cm > 0.278] <- NA
missing <- which(is.na(UQL3_19_fix$WC_15cm))

if(1 %in% missing){
  UQL3_19_fix$WC_15cm[1] <- head(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_15cm[nrow(data)] <- tail(UQL3_19_fix$WC_15cm[!is.na(UQL3_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_15cm[idx] <- (UQL3_19_fix$WC_15cm[r$starts[i]] + UQL3_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-11-04 12:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-11-22 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Remove 15 cm glitch at beginning of year
#=============================================================================
UQL3_19$WC_15cm[UQL3_19$WC_15cm == 0.4102] <- NA

#30 cm 
###############################################################################

#Remove 30 cm glitch at beginning of year
#=============================================================================
UQL3_19$WC_30cm[UQL3_19$WC_30cm == 0.7184] <- NA

#Remove 30 cm drips in July/August 
#=============================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-08-01 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-08-12 1:00:01")

UQL3_19_fix$WC_30cm[UQL3_19_fix$WC_30cm < 0.2317] <- NA
missing <- which(is.na(UQL3_19_fix$WC_30cm))

if(1 %in% missing){
  UQL3_19_fix$WC_30cm[1] <- head(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_30cm[nrow(data)] <- tail(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_30cm[idx] <- (UQL3_19_fix$WC_30cm[r$starts[i]] + UQL3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-08-01 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-08-12 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Remove 30 cm drips in July/August 
#=============================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-08-26 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-08-30 1:00:01")

UQL3_19_fix$WC_30cm[UQL3_19_fix$WC_30cm > 0.225] <- NA
missing <- which(is.na(UQL3_19_fix$WC_30cm))

if(1 %in% missing){
  UQL3_19_fix$WC_30cm[1] <- head(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_30cm[nrow(data)] <- tail(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_30cm[idx] <- (UQL3_19_fix$WC_30cm[r$starts[i]] + UQL3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-08-26 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-08-30 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Remove 30 cm drips in July/August 
#=============================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-10-15 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-10-20 1:00:01")

UQL3_19_fix$WC_30cm[UQL3_19_fix$WC_30cm < 0.2475] <- NA
missing <- which(is.na(UQL3_19_fix$WC_30cm))

if(1 %in% missing){
  UQL3_19_fix$WC_30cm[1] <- head(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_30cm[nrow(data)] <- tail(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_30cm[idx] <- (UQL3_19_fix$WC_30cm[r$starts[i]] + UQL3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-10-15 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-10-20 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Remove glitch in early October 
#=============================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-09-28 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-10-15 1:00:01")

UQL3_19_fix$WC_30cm[UQL3_19_fix$WC_30cm > 0.25] <- NA

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-09-28 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-10-15 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Calibrate
#=============================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-10-08 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-12-31 23:50:01")

#Rises from 0.2211 to 0.2498
UQL3_19_fix$WC_30cm <- UQL3_19_fix$WC_30cm - 0.032

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-10-08 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-12-31 23:50:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Remove 30 cm drips after calibration
#=============================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-11-15 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-12-01 1:00:01")

UQL3_19_fix$WC_30cm[UQL3_19_fix$WC_30cm < 0.21] <- NA
missing <- which(is.na(UQL3_19_fix$WC_30cm))

if(1 %in% missing){
  UQL3_19_fix$WC_30cm[1] <- head(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_30cm[nrow(data)] <- tail(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_30cm[idx] <- (UQL3_19_fix$WC_30cm[r$starts[i]] + UQL3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-11-15 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-12-01 1:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Remove 30 cm drips after calibration
#=============================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-11-15 1:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-11-20 11:00:01")

UQL3_19_fix$WC_30cm[UQL3_19_fix$WC_30cm < 0.211] <- NA
missing <- which(is.na(UQL3_19_fix$WC_30cm))

if(1 %in% missing){
  UQL3_19_fix$WC_30cm[1] <- head(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_30cm[nrow(data)] <- tail(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_30cm[idx] <- (UQL3_19_fix$WC_30cm[r$starts[i]] + UQL3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-11-15 1:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-11-20 11:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Remove 30 cm drips after calibration
#=============================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-11-28 21:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-12-01 11:00:01")

UQL3_19_fix$WC_30cm[UQL3_19_fix$WC_30cm < 0.218] <- NA
missing <- which(is.na(UQL3_19_fix$WC_30cm))

if(1 %in% missing){
  UQL3_19_fix$WC_30cm[1] <- head(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_30cm[nrow(data)] <- tail(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_30cm[idx] <- (UQL3_19_fix$WC_30cm[r$starts[i]] + UQL3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-11-28 21:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-12-01 11:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Remove 30 cm drips after calibration
#=============================================================================
UQL3_19_fix <- filter(UQL3_19, Date_time > "2019-11-30 11:00:01")
UQL3_19_fix <- filter(UQL3_19_fix, Date_time < "2019-12-01 11:00:01")

UQL3_19_fix$WC_30cm[UQL3_19_fix$WC_30cm < 0.2219] <- NA
missing <- which(is.na(UQL3_19_fix$WC_30cm))

if(1 %in% missing){
  UQL3_19_fix$WC_30cm[1] <- head(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}
if(nrow(UQL3_19_fix) %in% missing){
  UQL3_19_fix$WC_30cm[nrow(data)] <- tail(UQL3_19_fix$WC_30cm[!is.na(UQL3_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_19_fix$WC_30cm[idx] <- (UQL3_19_fix$WC_30cm[r$starts[i]] + UQL3_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL3_19_later <- filter(UQL3_19, Date_time < "2019-11-30 11:00:01")
UQL3_19_end <- filter(UQL3_19, Date_time  > "2019-12-01 11:00:01")
UQL3_19 <- bind_rows(UQL3_19_fix, UQL3_19_later, UQL3_19_end)

#Plot again 
Soil <- ggplot(data = subset(UQL3_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL3 2020
##################################################################################################
UQL3_20 <- subset(UQL3, Year == '2020')

#Plotting 
UQL3_20$WC_15cm <- as.numeric(UQL3_20$WC_15cm)
UQL3_20$WC_30cm <- as.numeric(UQL3_20$WC_30cm)
UQL3_20$WC_100cm <- as.numeric(UQL3_20$WC_100cm)
UQL3_20$Date_time<- mdy_hms(UQL3_20$Date_time)

#15 cm 
####################################################################################

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-08 12:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-10-20 1:00:01")

UQL3_20_fix <- UQL3_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL3_20_fix <- transform(UQL3_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL3_20_fix <- transform(UQL3_20_fix, WC_15cm=ifelse(diff < -0.001 | diff > 0.001,  
                                                     as.numeric(stats::filter(WC_15cm, rep(1/24, 24), sides=2)), 
                                                     WC_15cm))

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-08 12:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-10-20 1:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Calibrate 
#====================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-11 08:40:01")

UQL3_20_fix$WC_15cm <- UQL3_20_fix$WC_15cm + 0.012

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-11 08:40:01")
UQL3_20 <- bind_rows(UQL3_20_early, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-04-06 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-04-09 1:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.36] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}


#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-04-06 1:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-04-09 1:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-04-14 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-04-16 1:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.355] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-04-14 1:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-04-16 1:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-04-16 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-04-26 1:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.3483] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-04-16 1:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-04-26 1:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-04-26 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-04-27 1:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.347] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-04-26 1:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-04-27 1:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-04-27 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-04-30 1:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.3465] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-04-27 1:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-04-30 1:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-04-30 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-04-30 21:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.3455] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-04-30 1:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-04-30 21:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-05-01 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-05-04 21:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.345 | UQL3_20_fix$WC_15cm > 0.3524] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-05-01 1:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-05-04 21:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-05-03 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-05-04 10:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.349] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-05-03 1:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-05-04 10:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-05-04 01:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-05-05 15:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.348] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-05-04 01:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-05-05 15:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-05-05 01:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-05-07 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.3465] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-05-05 01:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-05-07 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-05-07 01:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-05-10 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.345] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-05-07 01:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-05-10 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-05-14 01:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-05-16 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.353] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-05-14 01:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-05-16 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-10 01:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-13 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.313 | UQL3_20_fix$WC_15cm > 0.318] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-10 01:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-13 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-13 10:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-16 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.3113 | UQL3_20_fix$WC_15cm > 0.315] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-13 10:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-16 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-15 10:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-20 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.314] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-15 10:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-20 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-16 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-16 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.3124] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-16 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-16 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-18 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-18 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.3115] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-18 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-18 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-19 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-21 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.3112] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-19 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-21 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-21 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-21 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.309] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-21 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-21 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-21 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-23 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.31 | UQL3_20_fix$WC_15cm < 0.3078] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-21 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-23 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-22 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-23 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.3097] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-22 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-23 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-23 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-24 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.3089] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-23 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-24 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-24 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-25 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.308] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-24 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-25 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-25 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-26 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.3077] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-25 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-26 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-26 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-27 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.307] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-26 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-27 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-27 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-28 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.306 | UQL3_20_fix$WC_15cm < 0.304] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-27 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-28 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-28 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-29 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.303 | UQL3_20_fix$WC_15cm > 0.305] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-28 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-29 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-29 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-29 20:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.30475] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-29 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-29 20:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-07-30 10:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-07-31 20:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.3047] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-07-30 10:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-07-31 20:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-01 07:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-01 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.303 | UQL3_20_fix$WC_15cm < 0.3015] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-01 07:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-01 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-02 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-02 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.302] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-02 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-02 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-03 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-03 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.3015 | UQL3_20_fix$WC_15cm < 0.3] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-03 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-03 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-04 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-04 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.301 | UQL3_20_fix$WC_15cm < 0.2995] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-04 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-04 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-06 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-06 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2995] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-06 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-06 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-07 07:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-07 16:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2985 | UQL3_20_fix$WC_15cm < 0.297] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-07 07:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-07 16:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-08 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-08 16:40:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2971 | UQL3_20_fix$WC_15cm < 0.296] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-08 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-08 16:40:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-09 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-09 16:40:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.296] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-09 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-09 16:40:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-10 09:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-10 14:40:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2955 | UQL3_20_fix$WC_15cm < 0.294] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-10 09:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-10 14:40:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-12 01:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-13 04:40:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2935] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-12 01:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-13 04:40:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-13 06:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-13 15:40:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2919 | UQL3_20_fix$WC_15cm > 0.294] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-13 06:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-13 15:40:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-14 06:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-15 00:40:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2897] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-14 06:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-15 00:40:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-15 00:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-15 20:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2879 | UQL3_20_fix$WC_15cm  > 0.2903] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-15 00:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-15 20:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-16 00:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-17 03:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2865] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-16 00:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-17 03:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-18 00:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-20 03:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2848] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-18 00:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-20 03:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-19 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-19 13:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2855] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-19 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-19 13:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-20 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-21 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2855 | UQL3_20_fix$WC_15cm < 0.2838] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-20 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-21 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-21 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-21 18:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2845 | UQL3_20_fix$WC_15cm < 0.283] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-21 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-21 18:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-22 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-22 17:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2855 | UQL3_20_fix$WC_15cm < 0.2825] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-22 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-22 17:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-23 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-23 17:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2855] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-23 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-23 17:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-24 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-24 17:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.284 | UQL3_20_fix$WC_15cm < 0.2825] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-24 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-24 17:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-25 08:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-25 17:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.285] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-25 08:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-25 17:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-27 08:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-27 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2835 | UQL3_20_fix$WC_15cm < 0.282] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-27 08:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-27 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-28 06:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-28 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2845] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-28 06:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-28 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-29 07:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-29 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2835] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-29 07:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-29 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-30 07:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-30 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2835] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-30 07:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-30 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-08-31 07:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-08-31 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.283] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-08-31 07:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-08-31 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-01 08:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-01 10:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.283] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-01 08:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-01 10:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-05 08:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-07 17:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.281 | UQL3_20_fix$WC_15cm < 0.2765] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-05 08:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-07 17:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-06 03:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-06 13:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2781] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-06 03:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-06 13:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-06 04:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-07 03:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.28] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-06 04:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-07 03:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-11 08:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-11 13:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.278] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-11 08:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-11 13:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-12 08:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-12 13:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.278] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-12 08:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-12 13:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-14 01:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-15 13:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2768] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-14 01:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-15 13:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-15 07:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-15 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2775] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-15 07:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-15 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-16 05:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-16 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.278 | UQL3_20_fix$WC_15cm < 0.2755] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-16 05:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-16 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-17 05:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-17 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2770] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-17 05:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-17 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-18 05:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-18 14:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.277] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-18 05:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-18 14:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-19 05:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-19 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.277] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-19 05:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-19 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-22 05:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-27 02:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.275] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-22 05:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-27 02:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-23 05:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-23 20:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.277] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-23 05:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-23 20:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-27 05:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-28 00:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.275] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-27 05:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-28 00:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-27 05:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-27 10:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.274] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-27 05:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-27 10:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-28 05:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-28 22:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.274 | UQL3_20_fix$WC_15cm < 0.272] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-28 05:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-28 22:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-30 07:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-30 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.274] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-30 07:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-30 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-10-01 07:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-10-01 18:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2725] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-10-01 07:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-10-01 18:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-10-02 07:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-10-02 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm > 0.2715] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-10-02 07:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-10-02 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-10-05 00:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-10-10 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2735] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-10-05 00:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-10-10 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-10-10 00:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-10-14 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2725] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-10-10 00:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-10-14 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-10-15 09:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-10-19 12:00:01")

Soil <- ggplot(data = subset(UQL3_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2694] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-10-15 09:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-10-19 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-10-19 09:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-10-23 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.2694] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-10-19 09:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-10-23 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-10-23 09:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-10-29 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.269800] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-10-23 09:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-10-29 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 15 cm drips
#====================================================================================
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-10-29 09:30:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-11-09 12:00:01")

UQL3_20_fix$WC_15cm[UQL3_20_fix$WC_15cm < 0.269800 | UQL3_20_fix$WC_15cm > 0.2708] <- NA
missing <- which(is.na(UQL3_20_fix$WC_15cm))

if(1 %in% missing){
  UQL3_20_fix$WC_15cm[1] <- head(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_15cm[nrow(data)] <- tail(UQL3_20_fix$WC_15cm[!is.na(UQL3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_15cm[idx] <- (UQL3_20_fix$WC_15cm[r$starts[i]] + UQL3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-10-29 09:30:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-11-09 12:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#30 cm 
###################################################################################

#Fix 30 cm drips
#====================================================================================
#Subset 
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-18 12:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-09-28 1:00:01")

UQL3_20_fix$WC_30cm[UQL3_20_fix$WC_30cm < 0.239] <- NA
missing <- which(is.na(UQL3_20_fix$WC_30cm))

if(1 %in% missing){
  UQL3_20_fix$WC_30cm[1] <- head(UQL3_20_fix$WC_30cm[!is.na(UQL3_20_fix$WC_30cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_30cm[nrow(data)] <- tail(UQL3_20_fix$WC_30cm[!is.na(UQL3_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_30cm[idx] <- (UQL3_20_fix$WC_30cm[r$starts[i]] + UQL3_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-18 12:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-09-28 1:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 30 cm drips
#====================================================================================
#Subset 
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-09-28 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-10-18 1:00:01")

UQL3_20_fix$WC_30cm[UQL3_20_fix$WC_30cm < 0.235] <- NA
missing <- which(is.na(UQL3_20_fix$WC_30cm))

if(1 %in% missing){
  UQL3_20_fix$WC_30cm[1] <- head(UQL3_20_fix$WC_30cm[!is.na(UQL3_20_fix$WC_30cm)],1)
}
if(nrow(UQL3_20_fix) %in% missing){
  UQL3_20_fix$WC_30cm[nrow(data)] <- tail(UQL3_20_fix$WC_30cm[!is.na(UQL3_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_20_fix$WC_30cm[idx] <- (UQL3_20_fix$WC_30cm[r$starts[i]] + UQL3_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-09-28 12:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-10-18 1:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Fix 30 cm drips
#====================================================================================
#Subset 
UQL3_20_fix <- filter(UQL3_20, Date_time > "2020-10-28 1:00:01")
UQL3_20_fix <- filter(UQL3_20_fix, Date_time < "2020-12-18 1:00:01")

UQL3_20_fix$WC_30cm[UQL3_20_fix$WC_30cm < 0.23] <- NA

#Recombine and then subset again 
UQL3_20_early <- filter(UQL3_20, Date_time < "2020-10-28 1:00:01")
UQL3_20_late <- filter(UQL3_20, Date_time > "2020-12-18 1:00:01")
UQL3_20 <- bind_rows(UQL3_20_early,UQL3_20_late, UQL3_20_fix )

#Plot
#=======================================================================
Soil <- ggplot(data = subset(UQL3_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL3 2021
##################################################################################################
UQL3_21 <- subset(UQL3, Year == '2021')

#Plotting 
UQL3_21$WC_15cm <- as.numeric(UQL3_21$WC_15cm)
UQL3_21$WC_30cm <- as.numeric(UQL3_21$WC_30cm)
UQL3_21$WC_100cm <- as.numeric(UQL3_21$WC_100cm)
UQL3_21$Date_time<- mdy_hms(UQL3_21$Date_time)

#15 cm
###########################################
UQL3_21$WC_15cm[UQL3_21$WC_15cm < 0] <- NA
missing <- which(is.na(UQL3_21$WC_15cm))

if(1 %in% missing){
  UQL3_21$WC_15cm[1] <- head(UQL3_21$WC_15cm[!is.na(UQL3_21$WC_15cm)],1)
}
if(nrow(UQL3_21) %in% missing){
  UQL3_21$WC_15cm[nrow(data)] <- tail(UQL3_21$WC_15cm[!is.na(UQL3_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_21$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_21$WC_15cm[idx] <- (UQL3_21$WC_15cm[r$starts[i]] + UQL3_21$WC_15cm[r$ends[i]])/2
}

#30 cm 
################################################################
UQL3_21$WC_30cm[UQL3_21$WC_30cm < 0.2] <- NA
missing <- which(is.na(UQL3_21$WC_30cm))

if(1 %in% missing){
  UQL3_21$WC_30cm[1] <- head(UQL3_21$WC_30cm[!is.na(UQL3_21$WC_30cm)],1)
}
if(nrow(UQL3_21) %in% missing){
  UQL3_21$WC_30cm[nrow(data)] <- tail(UQL3_21$WC_30cm[!is.na(UQL3_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_21$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_21$WC_30cm[idx] <- (UQL3_21$WC_30cm[r$starts[i]] + UQL3_21$WC_30cm[r$ends[i]])/2
}

#100 cm 
################################################################
UQL3_21$WC_100cm[UQL3_21$WC_100cm < 0] <- NA
missing <- which(is.na(UQL3_19$WC_100cm))

if(1 %in% missing){
  UQL3_21$WC_100cm[1] <- head(UQL3_21$WC_100cm[!is.na(UQL3_21$WC_100cm)],1)
}
if(nrow(UQL3_21) %in% missing){
  UQL3_21$WC_100cm[nrow(data)] <- tail(UQL3_21$WC_100cm[!is.na(UQL3_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL3_21$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL3_21$WC_100cm[idx] <- (UQL3_21$WC_100cm[r$starts[i]] + UQL3_21$WC_100cm[r$ends[i]])/2
}

#Plot again 
Soil <- ggplot(data = subset(UQL3_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
UQL3_clean <- merge(UQL3_18, UQL3_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
UQL3_clean <- merge(UQL3_clean, UQL3_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
UQL3_clean <- merge(UQL3_clean, UQL3_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
UQL3_clean <- merge(UQL3_clean, UQL3_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

UQL3_clean <- select(UQL3_clean, Date_time, WC_15cm, WC_30cm, WC_100cm)
#Graph
#===================================================================================

Soil <- ggplot(data = subset(UQL3_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date")) 
Soil

png("UQL3_clean", width = 4500, height = 2500)

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
write.csv(UQL3_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL3_clean.csv"  ) #this writes a csv file and sends it to the working folder
