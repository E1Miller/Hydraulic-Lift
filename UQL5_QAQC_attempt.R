#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/20/2022
#Description: QA/QC UQL 5

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
library(zoo)


setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL", 
                        pattern=glob2rx("Copy of U5M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
UQL5_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("Copy of U5M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
UQL5_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
UQL5 <- rbind(UQL5_2017_2019, UQL5_2019_2021)

#Write the csv
write.csv(UQL5,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL5.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(UQL5)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
UQL5$Date <- mdy_hms(UQL5$Date_time)

#Put year into a separate column 
UQL5 <- separate(UQL5, Date, c("Year"))

#UQL5 2017
##################################################################################################
UQL5_17 <- subset(UQL5, Year == '2017')

#Plotting 
UQL5_17$WC_15cm <- as.numeric(UQL5_17$WC_15cm)
UQL5_17$WC_30cm <- as.numeric(UQL5_17$WC_30cm)
UQL5_17$WC_100cm <- as.numeric(UQL5_17$WC_100cm)
UQL5_17$Date_time<- mdy_hms(UQL5_17$Date_time)

Soil <- ggplot(data = subset(UQL5_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL5 2018
##################################################################################################
UQL5_18 <- subset(UQL5, Year == '2018')

#Plotting 
UQL5_18$WC_15cm <- as.numeric(UQL5_18$WC_15cm)
UQL5_18$WC_30cm <- as.numeric(UQL5_18$WC_30cm)
UQL5_18$WC_100cm <- as.numeric(UQL5_18$WC_100cm)
UQL5_18$Date_time<- mdy_hms(UQL5_18$Date_time)

#15 cm
###########################################
UQL5_18$WC_15cm[UQL5_18$WC_15cm < 0.235] <- NA
missing <- which(is.na(UQL5_18$WC_15cm))

if(1 %in% missing){
  UQL5_18$WC_15cm[1] <- head(UQL5_18$WC_15cm[!is.na(UQL5_18$WC_15cm)],1)
}
if(nrow(UQL5_18) %in% missing){
  UQL5_18$WC_15cm[nrow(data)] <- tail(UQL5_18$WC_15cm[!is.na(UQL5_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18$WC_15cm[idx] <- (UQL5_18$WC_15cm[r$starts[i]] + UQL5_18$WC_15cm[r$ends[i]])/2
}

#Fix drips in 15 cm in February 
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-02-01 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-02-20 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.34] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-02-01 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-02-20 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-02-20 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-03-01 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.34] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-02-01 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-03-01 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in May 
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-05-15 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-05-30 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.329] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-05-15 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-05-30 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in May 
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-06-01 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-06-11 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.305] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-06-01 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-06-11 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in July
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-07-01 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-07-09 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.267] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-07-01 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-07-09 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in July
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-07-09 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-07-16 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.2675] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-07-09 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-07-16 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-11-19 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-11-22 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.267] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-11-12 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-11-22 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-11-21 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-11-22 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.2725] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-11-21 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-11-22 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-08-06 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-08-13 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.258 | UQL5_18_fix$WC_15cm > 0.262] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-08-06 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-08-13 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-10-01 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-10-02 00:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.24] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-10-01 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-10-02 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-09-29 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-09-29 18:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.237] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-09-29 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-09-29 18:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-05-07 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-05-10 08:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.351] <- NA

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-05-07 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-05-10 08:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-05-15 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-05-26 08:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.331] <- NA

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-05-15 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-05-26 08:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-06-05 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-06-10 08:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.3074] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-06-05 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-06-10 08:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-06-19 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-06-21 08:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.288] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-06-19 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-06-21 08:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-09-20 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-10-03 08:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.25] <- NA
missing <- which(is.na(UQL5_18_fix$WC_15cm))

if(1 %in% missing){
  UQL5_18_fix$WC_15cm[1] <- head(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix$WC_15cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_15cm[nrow(data)] <- tail(UQL5_18_fix$WC_15cm[!is.na(UQL5_18_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_15cm[idx] <- (UQL5_18_fix$WC_15cm[r$starts[i]] + UQL5_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-09-20 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-10-03 08:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips in 15 cm in Novemberish
#==============================================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-10-25 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-10-30 08:00:01")

UQL5_18_fix$WC_15cm[UQL5_18_fix$WC_15cm < 0.2777] <- NA

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-10-25 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-10-30 08:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#30 cm 
################################################################
UQL5_18$WC_30cm[UQL5_18$WC_30cm < 0.1] <- NA
missing <- which(is.na(UQL5_18$WC_30cm))

if(1 %in% missing){
  UQL5_18$WC_30cm[1] <- head(UQL5_18$WC_30cm[!is.na(UQL5_18$WC_30cm)],1)
}
if(nrow(UQL5_18) %in% missing){
  UQL5_18$WC_30cm[nrow(data)] <- tail(UQL5_18$WC_30cm[!is.na(UQL5_18$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18$WC_30cm[idx] <- (UQL5_18$WC_30cm[r$starts[i]] + UQL5_18$WC_30cm[r$ends[i]])/2
}

#Fix drips
#====================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-02-20 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-03-01 18:00:01")

UQL5_18_fix$WC_30cm[UQL5_18_fix$WC_30cm < 0.319] <- NA
missing <- which(is.na(UQL5_18_fix$WC_30cm))

if(1 %in% missing){
  UQL5_18_fix$WC_30cm[1] <- head(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_30cm[nrow(data)] <- tail(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_30cm[idx] <- (UQL5_18_fix$WC_30cm[r$starts[i]] + UQL5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-02-20 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-03-01 18:00:01")

UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips
#====================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-03-07 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-03-10 18:00:01")

UQL5_18_fix$WC_30cm[UQL5_18_fix$WC_30cm < 0.359] <- NA
missing <- which(is.na(UQL5_18_fix$WC_30cm))

if(1 %in% missing){
  UQL5_18_fix$WC_30cm[1] <- head(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_30cm[nrow(data)] <- tail(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_30cm[idx] <- (UQL5_18_fix$WC_30cm[r$starts[i]] + UQL5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-03-07 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-03-10 18:00:01")

UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips
#====================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-06-08 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-06-10 18:00:01")

UQL5_18_fix$WC_30cm[UQL5_18_fix$WC_30cm < 0.296] <- NA
missing <- which(is.na(UQL5_18_fix$WC_30cm))

if(1 %in% missing){
  UQL5_18_fix$WC_30cm[1] <- head(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_30cm[nrow(data)] <- tail(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_30cm[idx] <- (UQL5_18_fix$WC_30cm[r$starts[i]] + UQL5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-06-08 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-06-10 18:00:01")

UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips
#====================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-09-08 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-10-15 18:00:01")

UQL5_18_fix$WC_30cm[UQL5_18_fix$WC_30cm < 0.273] <- NA
missing <- which(is.na(UQL5_18_fix$WC_30cm))

if(1 %in% missing){
  UQL5_18_fix$WC_30cm[1] <- head(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_30cm[nrow(data)] <- tail(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_30cm[idx] <- (UQL5_18_fix$WC_30cm[r$starts[i]] + UQL5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-09-08 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-10-15 18:00:01")

UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips
#====================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-10-01 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-10-15 18:00:01")

UQL5_18_fix$WC_30cm[UQL5_18_fix$WC_30cm < 0.2735] <- NA
missing <- which(is.na(UQL5_18_fix$WC_30cm))

if(1 %in% missing){
  UQL5_18_fix$WC_30cm[1] <- head(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_30cm[nrow(data)] <- tail(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_30cm[idx] <- (UQL5_18_fix$WC_30cm[r$starts[i]] + UQL5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-10-01 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-10-15 18:00:01")

UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips
#====================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-10-03 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-10-08 18:00:01")

UQL5_18_fix$WC_30cm[UQL5_18_fix$WC_30cm < 0.2745] <- NA
missing <- which(is.na(UQL5_18_fix$WC_30cm))

if(1 %in% missing){
  UQL5_18_fix$WC_30cm[1] <- head(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_30cm[nrow(data)] <- tail(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_30cm[idx] <- (UQL5_18_fix$WC_30cm[r$starts[i]] + UQL5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-10-03 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-10-08 18:00:01")

UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix drips
#====================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-12-09 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-12-11 18:00:01")

UQL5_18_fix$WC_30cm[UQL5_18_fix$WC_30cm < 0.3495] <- NA
missing <- which(is.na(UQL5_18_fix$WC_30cm))

if(1 %in% missing){
  UQL5_18_fix$WC_30cm[1] <- head(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}
if(nrow(UQL5_18_fix) %in% missing){
  UQL5_18_fix$WC_30cm[nrow(data)] <- tail(UQL5_18_fix$WC_30cm[!is.na(UQL5_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18_fix$WC_30cm[idx] <- (UQL5_18_fix$WC_30cm[r$starts[i]] + UQL5_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-12-09 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-12-11 18:00:01")

UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#100 cm 
################################################################
UQL5_18$WC_100cm[UQL5_18$WC_100cm < 0] <- NA
missing <- which(is.na(UQL5_18$WC_100cm))

if(1 %in% missing){
  UQL5_18$WC_100cm[1] <- head(UQL5_18$WC_100cm[!is.na(UQL5_18$WC_100cm)],1)
}
if(nrow(UQL5_18) %in% missing){
  UQL5_18$WC_100cm[nrow(data)] <- tail(UQL5_18$WC_100cm[!is.na(UQL5_18$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_18$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_18$WC_100cm[idx] <- (UQL5_18$WC_100cm[r$starts[i]] + UQL5_18$WC_100cm[r$ends[i]])/2
}

#Remove upward swoop after missing data
#==============================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-07-01 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-08-20 00:00:01")

UQL5_18_fix$WC_100cm[UQL5_18_fix$WC_100cm > 0.304] <- NA

UQL5_18_early <- filter(UQL5_18, Date_time < "2018-07-01 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-08-20 00:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Remove upward swoop after missing data
#==============================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-05-20 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-05-28 12:00:01")

UQL5_18_fix$WC_100cm[UQL5_18_fix$WC_100cm < 0.29] <- NA

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-05-20 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-05-28 12:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Remove upward swoop after missing data
#==============================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-05-28 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-06-02 08:00:01")

UQL5_18_fix$WC_100cm[UQL5_18_fix$WC_100cm < 0.284] <- NA

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-05-28 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-06-02 08:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Remove upward swoop after missing data
#==============================================================================
UQL5_18_fix <- filter(UQL5_18, Date_time > "2018-06-02 00:00:01")
UQL5_18_fix <- filter(UQL5_18_fix, Date_time < "2018-06-22 08:00:01")

UQL5_18_fix$WC_100cm[UQL5_18_fix$WC_100cm < 0.2745] <- NA

#Recombine
UQL5_18_early <- filter(UQL5_18, Date_time < "2018-06-02 00:00:01")
UQL5_18_late <- filter(UQL5_18, Date_time > "2018-06-22 08:00:01")
UQL5_18 <- bind_rows(UQL5_18_late, UQL5_18_early, UQL5_18_fix)

#Fix glitch in 100 cm
#================================================================================================
UQL5_18$WC_100cm[UQL5_18$WC_100cm > 0.339049 & UQL5_18$WC_100cm < 0.339051] <- NA

#Fix other glitch in 100 cm 
#================================================================================
UQL5_18$WC_100cm[UQL5_18$WC_100cm > 0.35649 & UQL5_18$WC_100cm < 0.35656] <- NA
UQL5_18$WC_100cm[UQL5_18$WC_100cm > 0.28029 & UQL5_18$WC_100cm < 0.28031] <- NA

#Replace missing dates
#=============================================================================
#2018-02-01 to 2018-02-20

insertDF <- as.data.frame(matrix(data = NA, nrow = 19, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2018-02-02"), as.Date("2018-02-20"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

UQL5_18 <- insertRows(UQL5_18, c(4500:4518), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(UQL5_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL5 2019
##################################################################################################
UQL5_19 <- subset(UQL5, Year == '2019')

#Plotting 
UQL5_19$WC_15cm <- as.numeric(UQL5_19$WC_15cm)
UQL5_19$WC_30cm <- as.numeric(UQL5_19$WC_30cm)
UQL5_19$WC_100cm <- as.numeric(UQL5_19$WC_100cm)
UQL5_19$Date_time<- mdy_hms(UQL5_19$Date_time)

#15 cm 
################################################################

#Subset and remove upward dips
#========================================================================
UQL5_19_fix <- filter(UQL5_19, Date_time > "2019-08-09 00:00:01")
UQL5_19_fix <- filter(UQL5_19_fix, Date_time < "2019-09-01 18:00:01")

UQL5_19_fix$WC_15cm[UQL5_19_fix$WC_15cm > 0.287] <- NA
missing <- which(is.na(UQL5_19_fix$WC_15cm))

if(1 %in% missing){
  UQL5_19_fix$WC_15cm[1] <- head(UQL5_19_fix$WC_15cm[!is.na(UQL5_19_fix$WC_15cm)],1)
}
if(nrow(UQL5_19_fix) %in% missing){
  UQL5_19_fix$WC_15cm[nrow(data)] <- tail(UQL5_19_fix$WC_15cm[!is.na(UQL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_19_fix$WC_15cm[idx] <- (UQL5_19_fix$WC_15cm[r$starts[i]] + UQL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_19_early <- filter(UQL5_19, Date_time < "2019-08-09 00:00:01")
UQL5_19_late <- filter(UQL5_19, Date_time > "2019-09-01 18:00:01")

UQL5_19 <- bind_rows(UQL5_19_late, UQL5_19_early, UQL5_19_fix)

#Subset and remove upward drips
#======================================================================
UQL5_19_fix <- filter(UQL5_19, Date_time > "2019-08-19 00:00:01")
UQL5_19_fix <- filter(UQL5_19_fix, Date_time < "2019-09-01 18:00:01")

UQL5_19_fix$WC_15cm[UQL5_19_fix$WC_15cm > 0.279] <- NA
missing <- which(is.na(UQL5_19_fix$WC_15cm))

if(1 %in% missing){
  UQL5_19_fix$WC_15cm[1] <- head(UQL5_19_fix$WC_15cm[!is.na(UQL5_19_fix$WC_15cm)],1)
}
if(nrow(UQL5_19_fix) %in% missing){
  UQL5_19_fix$WC_15cm[nrow(data)] <- tail(UQL5_19_fix$WC_15cm[!is.na(UQL5_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_19_fix$WC_15cm[idx] <- (UQL5_19_fix$WC_15cm[r$starts[i]] + UQL5_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_19_early <- filter(UQL5_19, Date_time < "2019-08-19 00:00:01")
UQL5_19_late <- filter(UQL5_19, Date_time > "2019-09-01 18:00:01")
UQL5_19 <- bind_rows(UQL5_19_late, UQL5_19_early, UQL5_19_fix)

#30 cm 
################################################################
UQL5_19$WC_30cm[UQL5_19$WC_30cm < 0] <- NA
missing <- which(is.na(UQL5_19$WC_30cm))

if(1 %in% missing){
  UQL5_19$WC_30cm[1] <- head(UQL5_19$WC_30cm[!is.na(UQL5_19$WC_30cm)],1)
}
if(nrow(UQL5_19) %in% missing){
  UQL5_19$WC_30cm[nrow(data)] <- tail(UQL5_19$WC_30cm[!is.na(UQL5_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_19$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_19$WC_30cm[idx] <- (UQL5_19$WC_30cm[r$starts[i]] + UQL5_19$WC_30cm[r$ends[i]])/2
}

#Remove glitch at the beginning of year 
#====================================================================
UQL5_19$WC_30cm[UQL5_19$WC_30cm == 0.32985] <- NA

#Plot again 
Soil <- ggplot(data = subset(UQL5_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL5 2020
##################################################################################################
UQL5_20 <- subset(UQL5, Year == '2020')

#Plotting 
UQL5_20$WC_15cm <- as.numeric(UQL5_20$WC_15cm)
UQL5_20$WC_30cm <- as.numeric(UQL5_20$WC_30cm)
UQL5_20$WC_100cm <- as.numeric(UQL5_20$WC_100cm)
UQL5_20$Date_time<- mdy_hms(UQL5_20$Date_time)

#15 cm
###########################################
UQL5_20$WC_15cm[UQL5_20$WC_15cm < 0] <- NA
missing <- which(is.na(UQL5_20$WC_15cm))

if(1 %in% missing){
  UQL5_20$WC_15cm[1] <- head(UQL5_20$WC_15cm[!is.na(UQL5_20$WC_15cm)],1)
}
if(nrow(UQL5_20) %in% missing){
  UQL5_20$WC_15cm[nrow(data)] <- tail(UQL5_20$WC_15cm[!is.na(UQL5_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20$WC_15cm[idx] <- (UQL5_20$WC_15cm[r$starts[i]] + UQL5_20$WC_15cm[r$ends[i]])/2
}

#Calibrate 
#=========================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-08-31 08:20:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-09-09 20:20:01")

UQL5_20_fix$WC_15cm <- UQL5_20_fix$WC_15cm + 0.0117

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-08-31 08:20:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-09-09 20:20:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove glitches
#============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-08-16 08:20:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-09-05 00:20:01")

UQL5_20_fix$WC_15cm[UQL5_20_fix$WC_15cm < 0.274 | UQL5_20_fix$WC_15cm > 0.281] <- NA
missing <- which(is.na(UQL5_20_fix$WC_15cm))

if(1 %in% missing){
  UQL5_20_fix$WC_15cm[1] <- head(UQL5_20_fix$WC_15cm[!is.na(UQL5_20_fix$WC_15cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_15cm[nrow(data)] <- tail(UQL5_20_fix$WC_15cm[!is.na(UQL5_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_15cm[idx] <- (UQL5_20_fix$WC_15cm[r$starts[i]] + UQL5_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-08-16 08:20:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-09-05 00:20:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove glitches
#============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-08-31 08:20:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-09-05 00:20:01")

UQL5_20_fix$WC_15cm[UQL5_20_fix$WC_15cm > 0.279] <- NA
missing <- which(is.na(UQL5_20_fix$WC_15cm))

if(1 %in% missing){
  UQL5_20_fix$WC_15cm[1] <- head(UQL5_20_fix$WC_15cm[!is.na(UQL5_20_fix$WC_15cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_15cm[nrow(data)] <- tail(UQL5_20_fix$WC_15cm[!is.na(UQL5_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_15cm[idx] <- (UQL5_20_fix$WC_15cm[r$starts[i]] + UQL5_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-08-31 08:20:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-09-05 00:20:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#30 cm 
################################################################
UQL5_20$WC_30cm[UQL5_20$WC_30cm < 0.2] <- NA
missing <- which(is.na(UQL5_20$WC_30cm))

if(1 %in% missing){
  UQL5_20$WC_30cm[1] <- head(UQL5_20$WC_30cm[!is.na(UQL5_20$WC_30cm)],1)
}
if(nrow(UQL5_20) %in% missing){
  UQL5_20$WC_30cm[nrow(data)] <- tail(UQL5_20$WC_30cm[!is.na(UQL5_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20$WC_30cm[idx] <- (UQL5_20$WC_30cm[r$starts[i]] + UQL5_20$WC_30cm[r$ends[i]])/2
}
#Remove glitch 
#==============================================================================
UQL5_20$WC_30cm[UQL5_20$WC_30cm == 0.30945] <- NA

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-10-14 00:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-10-26 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.22] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}


UQL5_20_early <- filter(UQL5_20, Date_time < "2020-10-05 00:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-10-26 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-10-26 00:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-06 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.215] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-10-26 00:00:01")

UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-06 00:00:01")

UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-07 00:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-13 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.225] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-07 00:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-13 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-20 00:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-13 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.284] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-20 00:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-13 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-12-13 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.314] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-12-13 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-13 12:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-25 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.305] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/48, 48), sides=2)),
                                                     WC_30cm))

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-13 12:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-25 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-23 12:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-30 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.305] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-23 12:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-30 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-29 12:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-07 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.301] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-29 12:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-07 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-07 12:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-09 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.235] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-07 12:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-09 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-13 07:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-19 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-13 07:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-19 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-16 04:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-20 15:00:01")

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.001 | incr > 0.001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))


UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-16 04:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-20 15:00:01")

UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-21 04:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-27 15:00:01")

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.01 | incr > 0.01, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))


UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-21 04:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-27 15:00:01")

UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-28 10:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-30 15:00:01")

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))


UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-28 10:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-30 15:00:01")

UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-30 10:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-05 15:00:01")

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.1 | incr > 0.1, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/100, 100), sides=2)),
                                                     WC_30cm))


UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-30 10:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-05 15:00:01")

UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-12-04 10:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-09 00:00:01")

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/12, 12), sides=2)),
                                                     WC_30cm))


UQL5_20_early <- filter(UQL5_20, Date_time < "2020-12-04 10:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-09 00:00:01")

UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-12-10 10:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-12 00:00:01")

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/12, 12), sides=2)),
                                                     WC_30cm))


UQL5_20_early <- filter(UQL5_20, Date_time < "2020-12-10 10:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-12 00:00:01")

UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-12-12 10:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-22 00:00:01")

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/12, 12), sides=2)),
                                                     WC_30cm))


UQL5_20_early <- filter(UQL5_20, Date_time < "2020-12-12 10:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-22 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-12-22 10:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-31 00:00:01")

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/12, 12), sides=2)),
                                                     WC_30cm))


UQL5_20_early <- filter(UQL5_20, Date_time < "2020-12-22 10:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-31 00:00:01")

UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-12-01 12:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-20 00:00:01")

#Get rid of increases
UQL5_20_fix <- UQL5_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.01, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/48, 48), sides=2)),
                                                     WC_30cm))

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-12-01 12:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-20 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-10-13 07:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-01 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm > 0.245] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-10-13 07:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-01 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-10-16 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-10-26 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm > 0.231 | UQL5_20_fix$WC_30cm < 0.225] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-10-16 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-10-26 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-10-16 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-10-26 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm > 0.231 | UQL5_20_fix$WC_30cm < 0.225] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-10-16 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-10-26 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-10-15 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-05 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm > 0] <- NA

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-10-15 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-05 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-04 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-13 00:00:01")

UQL5_20 <- UQL5_20 %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-04 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-13 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-16 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-26 00:00:01")

UQL5_20 <- UQL5_20 %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_20_fix <- transform(UQL5_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_20_fix <- transform(UQL5_20_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))
#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-16 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-26 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-21 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-24 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm > 0.323 | UQL5_20_fix$WC_30cm < 0.32] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-21 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-24 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-25 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-27 00:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm > 0.319 | UQL5_20_fix$WC_30cm < 0.3165] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-25 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-27 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-26 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-28 10:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm > 0.31775 | UQL5_20_fix$WC_30cm < 0.316] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-26 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-28 10:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-30 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-03 10:00:01")

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.3102] <- NA
missing <- which(is.na(UQL5_20_fix$WC_30cm))

if(1 %in% missing){
  UQL5_20_fix$WC_30cm[1] <- head(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_30cm[nrow(data)] <- tail(UQL5_20_fix$WC_30cm[!is.na(UQL5_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_30cm[idx] <- (UQL5_20_fix$WC_30cm[r$starts[i]] + UQL5_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-30 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-03 10:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-12-07 20:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-10 10:00:01")

Soil <- ggplot(data = subset(UQL5_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

UQL5_20_fix$WC_30cm[UQL5_20_fix$WC_30cm < 0.3075 | UQL5_20_fix$WC_30cm > 0.3079] <- NA

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-12-07 20:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-10 10:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-10-25 23:50:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-31 23:50:01")

UQL5_20_fix$WC_30cm <- UQL5_20_fix$WC_30cm + 0.007

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-10-25 23:50:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-31 23:50:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-10-25 23:50:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-31 23:50:01")

UQL5_20_fix$WC_30cm <- UQL5_20_fix$WC_30cm + 0.007

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-10-25 23:50:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-31 23:50:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_fix, UQL5_20_late)

#100 cm 
################################################################

UQL5_20$WC_100cm[UQL5_20$WC_100cm < 0.25] <- NA
missing <- which(is.na(UQL5_19$WC_100cm))

if(1 %in% missing){
  UQL5_20$WC_100cm[1] <- head(UQL5_20$WC_100cm[!is.na(UQL5_20$WC_100cm)],1)
}
if(nrow(UQL5_20) %in% missing){
  UQL5_20$WC_100cm[nrow(data)] <- tail(UQL5_20$WC_100cm[!is.na(UQL5_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20$WC_100cm[idx] <- (UQL5_20$WC_100cm[r$starts[i]] + UQL5_20$WC_100cm[r$ends[i]])/2
}

#Remove glitch 
#==============================================================================
UQL5_20$WC_100cm[UQL5_20$WC_100cm > 0.30574 & UQL5_20$WC_100cm < 0.30576] <- NA

#Remove drips by subsetting
#=============================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-26 12:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-28 00:00:01")

UQL5_20_fix$WC_100cm[UQL5_20_fix$WC_100cm < 0.275] <- NA
missing <- which(is.na(UQL5_20_fix$WC_100cm))

if(1 %in% missing){
  UQL5_20_fix$WC_100cm[1] <- head(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_100cm[nrow(data)] <- tail(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_100cm[idx] <- (UQL5_20_fix$WC_100cm[r$starts[i]] + UQL5_20_fix$WC_100cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-26 12:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-28 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_late, UQL5_20_fix)


#100 cm 
#######################################################################################

#Remove 100 glitch in November
#================================================================================================
UQL5_20_fix$WC_100cm[UQL5_20_fix$WC_100cm == 0.26570] <- NA

#Remove 100 cm drip in November/December 
#================================================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-26 12:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-29 00:00:01")

UQL5_20_fix$WC_100cm[UQL5_20_fix$WC_100cm < 0.277] <- NA
missing <- which(is.na(UQL5_20_fix$WC_100cm))

if(1 %in% missing){
  UQL5_20_fix$WC_100cm[1] <- head(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_100cm[nrow(data)] <- tail(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_100cm[idx] <- (UQL5_20_fix$WC_100cm[r$starts[i]] + UQL5_20_fix$WC_100cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-26 12:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-29 00:00:01")
UQL5_20 <- bind_rows

#Remove 100 cm drip in November/December 
#================================================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-20 12:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-12-05 00:00:01")

UQL5_20_fix$WC_100cm[UQL5_20_fix$WC_100cm < 0.269] <- NA
missing <- which(is.na(UQL5_20_fix$WC_100cm))

if(1 %in% missing){
  UQL5_20_fix$WC_100cm[1] <- head(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_100cm[nrow(data)] <- tail(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_100cm[idx] <- (UQL5_20_fix$WC_100cm[r$starts[i]] + UQL5_20_fix$WC_100cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-20 12:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-12-05 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_late, UQL5_20_fix)

#Remove 100 cm drip in November/December 
#================================================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-26 02:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-30 00:00:01")

UQL5_20_fix$WC_100cm[UQL5_20_fix$WC_100cm < 0.27 ] <- NA

#Recombine
UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-26 02:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-30 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_late, UQL5_20_fix)
#Remove 100 cm drip in November/December 
#================================================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-11-27 06:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-29 00:00:01")

UQL5_20_fix$WC_100cm[UQL5_20_fix$WC_100cm < 0.285 | UQL5_20_fix$WC_100cm > 0.31] <- NA
missing <- which(is.na(UQL5_20_fix$WC_100cm))

if(1 %in% missing){
  UQL5_20_fix$WC_100cm[1] <- head(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_100cm[nrow(data)] <- tail(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_100cm[idx] <- (UQL5_20_fix$WC_100cm[r$starts[i]] + UQL5_20_fix$WC_100cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-11-27 06:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-29 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_late, UQL5_20_fix)

#Remove 100 cm drip in November/December 
#================================================================================================
UQL5_20_fix <- filter(UQL5_20, Date_time > "2020-10-17 06:00:01")
UQL5_20_fix <- filter(UQL5_20_fix, Date_time < "2020-11-15 00:00:01")

UQL5_20_fix$WC_100cm[UQL5_20_fix$WC_100cm < 0.253 | UQL5_20_fix$WC_100cm > 0.258] <- NA
missing <- which(is.na(UQL5_20_fix$WC_100cm))

if(1 %in% missing){
  UQL5_20_fix$WC_100cm[1] <- head(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}
if(nrow(UQL5_20_fix) %in% missing){
  UQL5_20_fix$WC_100cm[nrow(data)] <- tail(UQL5_20_fix$WC_100cm[!is.na(UQL5_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_20_fix$WC_100cm[idx] <- (UQL5_20_fix$WC_100cm[r$starts[i]] + UQL5_20_fix$WC_100cm[r$ends[i]])/2
}

UQL5_20_early <- filter(UQL5_20, Date_time < "2020-10-17 06:00:01")
UQL5_20_late <- filter(UQL5_20, Date_time > "2020-11-15 00:00:01")
UQL5_20 <- bind_rows(UQL5_20_early, UQL5_20_late, UQL5_20_fix)

#Plot again 
Soil <- ggplot(data = subset(UQL5_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL5 2021
##################################################################################################
UQL5_21 <- subset(UQL5, Year == '2021')

#Plotting 
UQL5_21$WC_15cm <- as.numeric(UQL5_21$WC_15cm)
UQL5_21$WC_30cm <- as.numeric(UQL5_21$WC_30cm)
UQL5_21$WC_100cm <- as.numeric(UQL5_21$WC_100cm)
UQL5_21$Date_time<- mdy_hms(UQL5_21$Date_time)

#15 cm
###########################################
UQL5_21$WC_15cm[UQL5_21$WC_15cm < 0] <- NA
missing <- which(is.na(UQL5_21$WC_15cm))

if(1 %in% missing){
  UQL5_21$WC_15cm[1] <- head(UQL5_21$WC_15cm[!is.na(UQL5_21$WC_15cm)],1)
}
if(nrow(UQL5_21) %in% missing){
  UQL5_21$WC_15cm[nrow(data)] <- tail(UQL5_21$WC_15cm[!is.na(UQL5_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21$WC_15cm[idx] <- (UQL5_21$WC_15cm[r$starts[i]] + UQL5_21$WC_15cm[r$ends[i]])/2
}

#Remove glitch 
#=========================================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-24 16:50:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-25 13:00:01")

UQL5_21_fix$WC_15cm[UQL5_21_fix$WC_15cm > 0] <- NA

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-24 16:50:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-25 13:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Remove glitch 
#=========================================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-09 16:50:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-13 13:00:01")

UQL5_21_fix$WC_15cm[UQL5_21_fix$WC_15cm < 0.253] <- NA

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-09 16:50:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-13 13:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Remove glitch 
#=========================================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-13 16:50:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-15 13:00:01")

UQL5_21_fix$WC_15cm[UQL5_21_fix$WC_15cm < 0.25] <- NA
missing <- which(is.na(UQL5_21_fix$WC_15cm))

if(1 %in% missing){
  UQL5_21_fix$WC_15cm[1] <- head(UQL5_21_fix$WC_15cm[!is.na(UQL5_21_fix$WC_15cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_15cm[nrow(data)] <- tail(UQL5_21_fix$WC_15cm[!is.na(UQL5_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_15cm[idx] <- (UQL5_21_fix$WC_15cm[r$starts[i]] + UQL5_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-13 16:50:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-15 13:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Remove glitch 
#=========================================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-01 16:50:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-09 13:00:01")

UQL5_21_fix$WC_15cm[UQL5_21_fix$WC_15cm < 0.23] <- NA
missing <- which(is.na(UQL5_21_fix$WC_15cm))

if(1 %in% missing){
  UQL5_21_fix$WC_15cm[1] <- head(UQL5_21_fix$WC_15cm[!is.na(UQL5_21_fix$WC_15cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_15cm[nrow(data)] <- tail(UQL5_21_fix$WC_15cm[!is.na(UQL5_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_15cm[idx] <- (UQL5_21_fix$WC_15cm[r$starts[i]] + UQL5_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-01 16:50:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-09 13:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Remove glitch 
#=========================================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-09 16:50:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-27 13:00:01")

UQL5_21_fix$WC_15cm[UQL5_21_fix$WC_15cm < 0.212] <- NA
missing <- which(is.na(UQL5_21_fix$WC_15cm))

if(1 %in% missing){
  UQL5_21_fix$WC_15cm[1] <- head(UQL5_21_fix$WC_15cm[!is.na(UQL5_21_fix$WC_15cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_15cm[nrow(data)] <- tail(UQL5_21_fix$WC_15cm[!is.na(UQL5_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_15cm[idx] <- (UQL5_21_fix$WC_15cm[r$starts[i]] + UQL5_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-09 16:50:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-27 13:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Remove glitch 
#=========================================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-30 16:50:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-08-06 00:00:01")

UQL5_21_fix$WC_15cm[UQL5_21_fix$WC_15cm < 0.204] <- NA
missing <- which(is.na(UQL5_21_fix$WC_15cm))

if(1 %in% missing){
  UQL5_21_fix$WC_15cm[1] <- head(UQL5_21_fix$WC_15cm[!is.na(UQL5_21_fix$WC_15cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_15cm[nrow(data)] <- tail(UQL5_21_fix$WC_15cm[!is.na(UQL5_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_15cm[idx] <- (UQL5_21_fix$WC_15cm[r$starts[i]] + UQL5_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-30 16:50:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-08-06 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Remove glitches
#================================================================================
UQL5_21$WC_15cm[UQL5_21$WC_15cm > 0.2931 & UQL5_21$WC_15cm < 0.2933] <- NA

#30 cm 
################################################################
UQL5_21$WC_30cm[UQL5_21$WC_30cm < 0.235] <- NA
missing <- which(is.na(UQL5_21$WC_30cm))

if(1 %in% missing){
  UQL5_21$WC_30cm[1] <- head(UQL5_21$WC_30cm[!is.na(UQL5_21$WC_30cm)],1)
}
if(nrow(UQL5_21) %in% missing){
  UQL5_21$WC_30cm[nrow(data)] <- tail(UQL5_21$WC_30cm[!is.na(UQL5_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21$WC_30cm[idx] <- (UQL5_21$WC_30cm[r$starts[i]] + UQL5_21$WC_30cm[r$ends[i]])/2
}

#Fix drips 
#================================================================
#Get rid of increases
UQL5_21 <- UQL5_21 %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21 <- transform(UQL5_21, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21 <- transform(UQL5_21, WC_30cm=ifelse(incr < -0.01, 
                                             as.numeric(stats::filter(WC_30cm, rep(1/48, 48), sides=2)),
                                             WC_30cm))

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time < "2021-04-01 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.31] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-01 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-01 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-26 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2848] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-01 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-26 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-26 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-01 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.31] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-26 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-01 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-01 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-07 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.295] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-01 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-07 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-07 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-12 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.29] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-07 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-12 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-12 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-20 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.285] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-12 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-20 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-20 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-01 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.28] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-20 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-01 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-01 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-12 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.27] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-01 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-12 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-12 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-30 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.255] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-12 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-30 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-04 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-30 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.243] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-04 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-30 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-01-01 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-01-06 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.317] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-01-01 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-01-06 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-01-12 13:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-01-13 02:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.35] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-01-12 13:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-01-13 02:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-01-16 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-01-19 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.315] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-01-16 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-01-19 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-01-19 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-01-23 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.3147] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-01-19 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-01-23 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-02-01 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-02-28 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.314] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-02-01 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-02-28 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)
     
#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-03-01 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-03-03 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.315] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-03-01 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-03-03 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-03-04 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-03-10 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.01 | incr > 0.01, 
                                             as.numeric(stats::filter(WC_30cm, rep(1/12, 12), sides=2)),
                                             WC_30cm))

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-03-04 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-03-10 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-03-08 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-03-14 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.001 | incr > 0.001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/12, 12), sides=2)),
                                                     WC_30cm))

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-03-08 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-03-14 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-03-15 10:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-03-20 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-03-15 10:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-03-20 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-03-20 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-03-23 20:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/3, 3), sides=2)),
                                                     WC_30cm))

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-03-20 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-03-23 20:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-03-23 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-03-30 20:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/6, 6), sides=2)),
                                                     WC_30cm))

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-03-23 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-03-30 20:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-01 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-04 20:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.310] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-01 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-04 20:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-08 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-09 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.307] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-08 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-09 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-09 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-10 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.305] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-09 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-10 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-10 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-11 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.3035] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-10 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-11 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-11 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-11 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.303] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-11 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-11 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-12 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-13 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.3015] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-12 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-13 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-13 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-15 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.298] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-13 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-15 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-15 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-16 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2985] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-15 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-16 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-16 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-18 14:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.295] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-16 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-18 14:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-21 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-23 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2915] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-21 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-23 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-23 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-27 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.29] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-23 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-27 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-26 12:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-27 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.313] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-26 12:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-27 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-28 12:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-28 21:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.312] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-28 12:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-28 21:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-29 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-29 15:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.3116] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-29 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-29 15:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-01 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-02 05:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.304] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-01 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-02 05:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-02 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-03 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.3008] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-02 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-03 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-03 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-03 18:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2995] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-03 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-03 18:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-04 10:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-06 18:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-04 10:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-06 18:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-09 15:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-10 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.294] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-09 15:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-10 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-12 15:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-13 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2892] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-12 15:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-13 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-13 15:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-15 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.286] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-13 15:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-15 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-15 15:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-17 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.287] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-15 15:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-17 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-18 03:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-19 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2863] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-18 03:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-19 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-20 03:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-22 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2855] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-20 03:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-22 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-24 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-24 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.284] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-24 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-24 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-26 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-27 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2825] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-26 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-27 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-27 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-29 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2825] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-27 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-29 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-29 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-31 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.282] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-29 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-31 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-01 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-10 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.272] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-01 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-10 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-11 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-17 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.266] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-11 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-17 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)


#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-17 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-19 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-17 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-19 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-19 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-22 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-19 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-22 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-22 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-24 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2615] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-22 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-24 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-24 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-26 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2625] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-24 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-26 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-01 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-02 08:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.347] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-01 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-02 08:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-03 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-05 08:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.245] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-03 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-05 08:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-05 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-09 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.254] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-05 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-09 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-09 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-12 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.254] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-09 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-12 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-12 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-15 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.253] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-12 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-15 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-15 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-19 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.252] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-15 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-19 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-19 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-21 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.25] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-19 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-21 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-21 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-24 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2475] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-21 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-24 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-24 00:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-26 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2479] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-24 00:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-26 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-26 05:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-26 18:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.259] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-26 05:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-26 18:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-28 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-29 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.256] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-28 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-29 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-29 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-08-15 12:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.243] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-29 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-08-15 12:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-08-08 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-08-10 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2455] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-08-08 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-08-10 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-08-08 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-08-10 00:00:01")


UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.2455] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-08-08 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-08-10 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-01 20:00:01")

UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-15 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm > 0.325] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-01 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-15 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-01 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-05 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.001 | incr > 0.001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/9, 9), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-01 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-05 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-05 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-15 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.001 | incr > 0.001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/9, 9), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-05 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-15 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-15 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-25 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.001 | incr > 0.001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/9, 9), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-15 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-25 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-25 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-05 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/9, 9), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-04-25 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-05 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-05 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-15 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/9, 9), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-05 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-15 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-15 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-05-25 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/6, 6), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-15 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-05-25 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-05-25 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-05 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/12, 12), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-05-25 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-05 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-05 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-15 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-05 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-15 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-15 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-25 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-15 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-25 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-23 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-06-30 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-23 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-06-30 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-02 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-10 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-02 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-10 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-10 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-15 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-10 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-15 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-13 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-25 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-13 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-25 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-07-24 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-08-10 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-24 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-08-10 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-04-01 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-06 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm > 0.3225 | UQL5_21_fix$WC_30cm < 0.308] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}
#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-07-24 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-08-10 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-01-06 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-01-09 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.32] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-01-06 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-01-09 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-01-12 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-01-14 00:00:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm < 0.32] <- NA
missing <- which(is.na(UQL5_21_fix$WC_30cm))

if(1 %in% missing){
  UQL5_21_fix$WC_30cm[1] <- head(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}
if(nrow(UQL5_21_fix) %in% missing){
  UQL5_21_fix$WC_30cm[nrow(data)] <- tail(UQL5_21_fix$WC_30cm[!is.na(UQL5_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21_fix$WC_30cm[idx] <- (UQL5_21_fix$WC_30cm[r$starts[i]] + UQL5_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-01-12 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-01-14 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)


#Remove glitch
#========================================================================
UQL5_21$WC_30cm[UQL5_21$WC_30cm == 0.3245] <- NA

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-01-19 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-01-31 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-01-19 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-01-31 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-01-30 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-02-05 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)

#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-01-30 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-02-05 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-02-04 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-02-15 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-02-04 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-02-15 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-02-15 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-02-25 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-02-15 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-02-25 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-02-23 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-03-01 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-02-23 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-03-01 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-02-28 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-03-08 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-02-28 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-03-08 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-03-07 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-03-18 00:00:01")

UQL5_21_fix <- UQL5_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL5_21_fix <- transform(UQL5_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL5_21_fix <- transform(UQL5_21_fix, WC_30cm=ifelse(incr < -0.0001 | incr > 0.0001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-03-07 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-03-18 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-03-17 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-01 00:00:01")

Soil <- ggplot(data = subset(UQL5_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = smoothed_data$WC_30cm_smoothed, color = "blue")) 
Soil

smooth_and_cap <- function(data, x_axis, y_axis, y_min, y_max) {
  # cap the data at the specified y-axis limits
  data[[y_axis]] <- pmin(pmax(data[[y_axis]], y_min), y_max)
  
  # sort the data by the x-axis variable
  data <- data[order(data[[x_axis]]), ]
  
  # interpolate any missing data points
  data[[y_axis]] <- na.approx(data[[y_axis]])
  
  # calculate the rolling mean of the data
  data[[paste0(y_axis, "_smoothed")]] <- rollmean(data[[y_axis]], k = 5, na.pad = TRUE, align = "center")
  
  # return the smoothed and capped data
  return(data)
}

#Add in data frame, x-axis variable, y -axis variable, 
#min and max y-axis values, and number of data points to interpolate
smoothed_data <- smooth_and_cap(UQL5_21_fix, "Date_time", "WC_30cm", 0.319, 0.326)

UQL5_21_fix$WC_30cm <- smoothed_data$WC_30cm_smoothed

#Recombine
UQL5_21_early <- filter(UQL5_21, Date_time < "2021-03-17 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-04-01 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Subset to fix early year drips
#=============================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-03-30 20:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-04-15 00:00:01")

Soil <- ggplot(data = subset(UQL5_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "blue"))
Soil

smooth_and_cap <- function(data, x_axis, y_axis, y_min, y_max) {
  # cap the data at the specified y-axis limits
  data[[y_axis]] <- pmin(pmax(data[[y_axis]], y_min), y_max)
  
  # sort the data by the x-axis variable
  data <- data[order(data[[x_axis]]), ]
  
  # interpolate any missing data points
  data[[y_axis]] <- na.approx(data[[y_axis]])
  
  # calculate the rolling mean of the data
  data[[paste0(y_axis, "_smoothed")]] <- rollmean(data[[y_axis]], k = 5, na.pad = TRUE, align = "center")
  
  # return the smoothed and capped data
  return(data)
}

#Add in data frame, x-axis variable, y -axis variable, 
#min and max y-axis values, and number of data points to interpolate
smoothed_data <- smooth_and_cap(UQL5_21_fix, "Date_time", "WC_30cm", 0.29, 0.324)

UQL5_21_fix$WC_30cm <- 






UQL5_21_early <- filter(UQL5_21, Date_time < "2021-03-10 20:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-08-10 00:00:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#100 cm 
################################################################
UQL5_21$WC_100cm[UQL5_21$WC_100cm < 0.2] <- NA
missing <- which(is.na(UQL5_19$WC_100cm))

if(1 %in% missing){
  UQL5_21$WC_100cm[1] <- head(UQL5_21$WC_100cm[!is.na(UQL5_21$WC_100cm)],1)
}
if(nrow(UQL5_21) %in% missing){
  UQL5_21$WC_100cm[nrow(data)] <- tail(UQL5_21$WC_100cm[!is.na(UQL5_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL5_21$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL5_21$WC_100cm[idx] <- (UQL5_21$WC_100cm[r$starts[i]] + UQL5_21$WC_100cm[r$ends[i]])/2
}

#Get rid of glitch during missing dates and turn them into NAs
#=====================================================================
UQL5_21_fix <- filter(UQL5_21, Date_time > "2021-06-27 18:00:01")
UQL5_21_fix <- filter(UQL5_21_fix, Date_time < "2021-07-02 13:40:01")

UQL5_21_fix$WC_30cm[UQL5_21_fix$WC_30cm > 0] <- NA
UQL5_21_fix$WC_15cm[UQL5_21_fix$WC_15cm > 0] <- NA
UQL5_21_fix$WC_100cm[UQL5_21_fix$WC_100cm > 0] <- NA

UQL5_21_early <- filter(UQL5_21, Date_time < "2021-06-27 18:00:01")
UQL5_21_late <- filter(UQL5_21, Date_time > "2021-07-02 13:40:01")
UQL5_21 <- bind_rows(UQL5_21_late, UQL5_21_fix, UQL5_21_early)

#Missing dates at end of year
#=====================================================================================
#Replace missing dates with NAs from 07/25 to 08/10
#========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 15, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-07-26"), as.Date("2021-08-09"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

UQL5_21 <- insertRows(UQL5_21, c(1:16), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(UQL5_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
UQL5_clean <- merge(UQL5_18, UQL5_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
UQL5_clean <- merge(UQL5_clean, UQL5_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
UQL5_clean <- merge(UQL5_clean, UQL5_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
UQL5_clean <- merge(UQL5_clean, UQL5_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)
UQL5_clean <- select(UQL5_clean, Date_time, WC_15cm, WC_30cm, WC_100cm)
#Graph
#===================================================================================

Soil <- ggplot(data = subset(UQL5_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date")) 
Soil

png("UQL5_clean", width = 4500, height = 2500)

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
write.csv(UQL5_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL5_clean.csv" ) #this writes a csv file and sends it to the working folder
