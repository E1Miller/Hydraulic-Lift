#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/15/2022
#Description: QA/QC TRE 4

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

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new", 
                        pattern=glob2rx("Copy of T4M_H*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
TRE4_2019_2021 <- data19_21 %>%
  rename_all(funs(c("Date", "Time", "PAR", "WC_15cm", "WC_30cm","WC_100cm", "Date_time"))) %>% #this renames all of the columns
  select(Date_time, PAR, WC_15cm, WC_30cm, WC_100cm) #this keeps only the columns you will use

#TRE4_2019_2021 <- TRE4_2019_2021[c(1:82192), ]

#2017-2019 FILES
#========================================================================================================================
#If wanting to merge the files, needed to manually delete the extra column in W1M190111 and 
#extra row at the top, and needed to manually delete the extra columns in W1M171228, W1M180201, and W1M180302

#Set the data path 
data_path <- "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new" 
new_col_name <- c("Date_time", "PAR", "WC_15cm", "WC_30cm", "WC_100cm")

#Call in all the files in this folder with the W2M1 pattern, which excludes the datasheets from 2019-2020
files <- dir(data_path, pattern=glob2rx("Copy of T4M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
TRE4_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
TRE4 <- rbind(TRE4_2017_2019, TRE4_2019_2021)

#Write the csv
write.csv(TRE4,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE4.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(TRE4)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
TRE4$Date <- mdy_hms(TRE4$Date_time)

#Put year into a separate column 
TRE4 <- separate(TRE4, Date, c("Year"))

#TRE4 2017
##################################################################################################
TRE4_17 <- subset(TRE4, Year == '2017')

#Plotting 
TRE4_17$WC_15cm <- as.numeric(TRE4_17$WC_15cm)
TRE4_17$WC_30cm <- as.numeric(TRE4_17$WC_30cm)
TRE4_17$WC_100cm <- as.numeric(TRE4_17$WC_100cm)
TRE4_17$Date_time<- mdy_hms(TRE4_17$Date_time)

Soil <- ggplot(data = subset(TRE4_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE4 2018
##################################################################################################
TRE4_18 <- subset(TRE4, Year == '2018')

#Plotting 
TRE4_18$WC_15cm <- as.numeric(TRE4_18$WC_15cm)
TRE4_18$WC_30cm <- as.numeric(TRE4_18$WC_30cm)
TRE4_18$WC_100cm <- as.numeric(TRE4_18$WC_100cm)
TRE4_18$Date_time<- mdy_hms(TRE4_18$Date_time)

#15 cm
###########################################################################################

#Get rid of positive values in 15 cm 
#==========================================================================================
TRE4_18$WC_15cm[TRE4_18$WC_15cm == 0.7184] <- NA

#Subset and remove drips in June/July
#============================================================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-06-02 18:00:01")
TRE4_18_fix <- filter(TRE4_18_fix, Date_time < "2018-06-10 00:00:01")

TRE4_18_fix$WC_15cm[TRE4_18_fix$WC_15cm < 0.195] <- NA
missing <- which(is.na(TRE4_18_fix$WC_15cm))

if(1 %in% missing){
  TRE4_18_fix$WC_15cm[1] <- head(TRE4_18_fix$WC_15cm[!is.na(TRE4_18_fix$WC_15cm)],1)
}
if(nrow(TRE4_18_fix) %in% missing){
  TRE4_18_fix$WC_15cm[nrow(data)] <- tail(TRE4_18_fix$WC_15cm[!is.na(TRE4_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_18_fix$WC_15cm[idx] <- (TRE4_18_fix$WC_15cm[r$starts[i]] + TRE4_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-06-02 18:00:01")
TRE4_18_late <- filter(TRE4_18, Date_time > "2018-06-10 00:00:01")
TRE4_18 <- bind_rows(TRE4_18_early, TRE4_18_late, TRE4_18_fix)

#Subset and remove drips in June/July
#============================================================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-06-25 18:00:01")
TRE4_18_fix <- filter(TRE4_18_fix, Date_time < "2018-06-30 00:00:01")

TRE4_18_fix$WC_15cm[TRE4_18_fix$WC_15cm < 0.15] <- NA
missing <- which(is.na(TRE4_18_fix$WC_15cm))

if(1 %in% missing){
  TRE4_18_fix$WC_15cm[1] <- head(TRE4_18_fix$WC_15cm[!is.na(TRE4_18_fix$WC_15cm)],1)
}
if(nrow(TRE4_18_fix) %in% missing){
  TRE4_18_fix$WC_15cm[nrow(data)] <- tail(TRE4_18_fix$WC_15cm[!is.na(TRE4_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_18_fix$WC_15cm[idx] <- (TRE4_18_fix$WC_15cm[r$starts[i]] + TRE4_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-06-25 18:00:01")
TRE4_18_late <- filter(TRE4_18, Date_time > "2018-06-30 00:00:01")
TRE4_18 <- bind_rows(TRE4_18_early, TRE4_18_late, TRE4_18_fix)

#Subset and remove drips in June/July
#============================================================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-06-30 18:00:01")
TRE4_18_fix <- filter(TRE4_18_fix, Date_time < "2018-07-30 00:00:01")

TRE4_18_fix$WC_15cm[TRE4_18_fix$WC_15cm > 0.15] <- NA
missing <- which(is.na(TRE4_18_fix$WC_15cm))

if(1 %in% missing){
  TRE4_18_fix$WC_15cm[1] <- head(TRE4_18_fix$WC_15cm[!is.na(TRE4_18_fix$WC_15cm)],1)
}
if(nrow(TRE4_18_fix) %in% missing){
  TRE4_18_fix$WC_15cm[nrow(data)] <- tail(TRE4_18_fix$WC_15cm[!is.na(TRE4_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_18_fix$WC_15cm[idx] <- (TRE4_18_fix$WC_15cm[r$starts[i]] + TRE4_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-06-30 18:00:01")
TRE4_18_late <- filter(TRE4_18, Date_time > "2018-07-30 00:00:01")
TRE4_18 <- bind_rows(TRE4_18_early, TRE4_18_late, TRE4_18_fix)

#Subset and remove drips in June/July
#============================================================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-04-30 18:00:01")
TRE4_18_fix <- filter(TRE4_18_fix, Date_time < "2018-05-28 00:00:01")

TRE4_18_fix$WC_15cm[TRE4_18_fix$WC_15cm < 0.212] <- NA
missing <- which(is.na(TRE4_18_fix$WC_15cm))

if(1 %in% missing){
  TRE4_18_fix$WC_15cm[1] <- head(TRE4_18_fix$WC_15cm[!is.na(TRE4_18_fix$WC_15cm)],1)
}
if(nrow(TRE4_18_fix) %in% missing){
  TRE4_18_fix$WC_15cm[nrow(data)] <- tail(TRE4_18_fix$WC_15cm[!is.na(TRE4_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_18_fix$WC_15cm[idx] <- (TRE4_18_fix$WC_15cm[r$starts[i]] + TRE4_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-04-30 18:00:01")
TRE4_18_late <- filter(TRE4_18, Date_time > "2018-05-28 00:00:01")
TRE4_18 <- bind_rows(TRE4_18_early, TRE4_18_late, TRE4_18_fix)

#Subset and remove drips in June/July
#============================================================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-05-10 05:40:01")
TRE4_18_fix <- filter(TRE4_18_fix, Date_time < "2018-05-24 05:10:01")

#Drops from 0.2722 to 0.2553 
TRE4_18_fix$WC_15cm <- TRE4_18_fix$WC_15cm + 0.0169

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-05-10 05:40:01")
TRE4_18_late <- filter(TRE4_18, Date_time > "2018-05-24 05:10:01")
TRE4_18 <- bind_rows(TRE4_18_early, TRE4_18_late, TRE4_18_fix)

#Subset and remove drips in June/July
#============================================================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-05-24 04:50:01")
TRE4_18_fix <- filter(TRE4_18_fix, Date_time < "2018-05-31 10:00:01")

#Drops from 0.2383 to 0.2174
TRE4_18_fix$WC_15cm <- TRE4_18_fix$WC_15cm + 0.0209

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-05-24 04:50:01")
TRE4_18_late <- filter(TRE4_18, Date_time > "2018-05-31 10:00:01")
TRE4_18 <- bind_rows(TRE4_18_early, TRE4_18_late, TRE4_18_fix)

#Subset and remove drips in June/July
#============================================================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-05-10 04:10:01")

TRE4_18_fix$WC_15cm <- TRE4_18_fix$WC_15cm + 0.0169

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-05-10 04:10:01")
TRE4_18 <- bind_rows(TRE4_18_early, TRE4_18_fix)

#Subset and remove drips in June/July
#============================================================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-07-18 02:50:01")

TRE4_18_fix$WC_15cm <- TRE4_18_fix$WC_15cm + 0.0036

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-07-18 02:50:01")
TRE4_18 <- bind_rows(TRE4_18_early, TRE4_18_fix)



#100 cm 
################################################################

#Get rid of negative values 
#=========================================================
TRE4_18$WC_100cm[TRE4_18$WC_100cm < 0.19] <- NA
missing <- which(is.na(TRE4_18$WC_100cm))

if(1 %in% missing){
  TRE4_18$WC_100cm[1] <- head(TRE4_18$WC_100cm[!is.na(TRE4_18$WC_100cm)],1)
}
if(nrow(TRE4_18) %in% missing){
  TRE4_18$WC_100cm[nrow(data)] <- tail(TRE4_18$WC_100cm[!is.na(TRE4_18$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_18$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_18$WC_100cm[idx] <- (TRE4_18$WC_100cm[r$starts[i]] + TRE4_18$WC_100cm[r$ends[i]])/2
}

#Subset 100 cm to get rid of drips in April 
#========================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-03-25 1:00:01")
TRE4_18_fix <- filter(TRE4_18_fix, Date_time < "2018-04-10 1:00:01")


TRE4_18_fix$WC_100cm[TRE4_18_fix$WC_100cm < 0.27] <- NA
missing <- which(is.na(TRE4_18_fix$WC_100cm))

if(1 %in% missing){
  TRE4_18_fix$WC_100cm[1] <- head(TRE4_18_fix$WC_100cm[!is.na(TRE4_18_fix$WC_100cm)],1)
}
if(nrow(TRE4_18_fix) %in% missing){
  TRE4_18_fix$WC_100cm[nrow(data)] <- tail(TRE4_18_fix$WC_100cm[!is.na(TRE4_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_18_fix$WC_100cm[idx] <- (TRE4_18_fix$WC_100cm[r$starts[i]] + TRE4_18_fix$WC_100cm[r$ends[i]])/2
}

#Remove excessive positive values 
TRE4_18_fix$WC_100cm[TRE4_18_fix$WC_100cm > 0.274] <- NA
missing <- which(is.na(TRE4_18_fix$WC_100cm))

if(1 %in% missing){
  TRE4_18_fix$WC_100cm[1] <- head(TRE4_18_fix$WC_100cm[!is.na(TRE4_18_fix$WC_100cm)],1)
}
if(nrow(TRE4_18_fix) %in% missing){
  TRE4_18_fix$WC_100cm[nrow(data)] <- tail(TRE4_18_fix$WC_100cm[!is.na(TRE4_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_18_fix$WC_100cm[idx] <- (TRE4_18_fix$WC_100cm[r$starts[i]] + TRE4_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and try again 
#======================================================================
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-03-25 1:00:01")
TRE4_18_late <- filter(TRE4_18, Date_time > "2018-04-10 1:00:01")

TRE4_18 <- bind_rows(TRE4_18_fix, TRE4_18_early, TRE4_18_late)

#Remove 100 cm that is the same level 
#===================================================
TRE4_18$WC_100cm[TRE4_18$WC_100cm > 0.32419 & TRE4_18$WC_100cm < 0.32421] <- NA

#Subset 100 cm to get rid of that one drip in early March 
#========================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-03-05 1:00:01")
TRE4_18_fix <- filter(TRE4_18_fix, Date_time < "2018-03-10 1:00:01")


TRE4_18_fix$WC_100cm[TRE4_18_fix$WC_100cm < 0.268] <- NA
missing <- which(is.na(TRE4_18_fix$WC_100cm))

if(1 %in% missing){
  TRE4_18_fix$WC_100cm[1] <- head(TRE4_18_fix$WC_100cm[!is.na(TRE4_18_fix$WC_100cm)],1)
}
if(nrow(TRE4_18_fix) %in% missing){
  TRE4_18_fix$WC_100cm[nrow(data)] <- tail(TRE4_18_fix$WC_100cm[!is.na(TRE4_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_18_fix$WC_100cm[idx] <- (TRE4_18_fix$WC_100cm[r$starts[i]] + TRE4_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-03-05 1:00:01")
TRE4_18_late <- filter(TRE4_18, Date_time > "2018-03-10 1:00:01")
TRE4_18 <- bind_rows(TRE4_18_fix, TRE4_18_late, TRE4_18_early)

#Subset 100 cm to get rid of that one drip in early March 
#========================================================
TRE4_18_fix <- filter(TRE4_18, Date_time > "2018-04-04 1:00:01")
TRE4_18_fix <- filter(TRE4_18_fix, Date_time < "2018-05-10 06:00:01")

TRE4_18_fix$WC_100cm[TRE4_18_fix$WC_100cm < 0.39] <- NA

#Recombine
TRE4_18_early <- filter(TRE4_18, Date_time < "2018-04-04 1:00:01")
TRE4_18_late <- filter(TRE4_18, Date_time > "2018-05-10 06:00:01")
TRE4_18 <- bind_rows(TRE4_18_fix, TRE4_18_late, TRE4_18_early)

#Remove first part of January with the glitch in 100 cm
#=======================================================
TRE4_18$WC_100cm[TRE4_18$WC_100cm < 0.255] <- NA

#Plot again 
Soil <- ggplot(data = subset(TRE4_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE4 2019
##################################################################################################
TRE4_19 <- subset(TRE4, Year == '2019')

#Plotting 
TRE4_19$WC_15cm <- as.numeric(TRE4_19$WC_15cm)
TRE4_19$WC_30cm <- as.numeric(TRE4_19$WC_30cm)
TRE4_19$WC_100cm <- as.numeric(TRE4_19$WC_100cm)
TRE4_19$Date_time<- mdy_hms(TRE4_19$Date_time)

#15 cm
#############################################################################################
TRE4_19$WC_15cm[TRE4_19$WC_15cm == 0.7184] <- NA

#Subset and remove drips in July 
#=============================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-07-19 1:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-09-01 1:00:01")

TRE4_19_fix$WC_15cm[TRE4_19_fix$WC_15cm >0.18] <- NA
missing <- which(is.na(TRE4_19_fix$WC_15cm))

if(1 %in% missing){
  TRE4_19_fix$WC_15cm[1] <- head(TRE4_19_fix$WC_15cm[!is.na(TRE4_19_fix$WC_15cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_15cm[nrow(data)] <- tail(TRE4_19_fix$WC_15cm[!is.na(TRE4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_15cm[idx] <- (TRE4_19_fix$WC_15cm[r$starts[i]] + TRE4_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-07-19 1:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-09-01 1:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset and remove drips in July 
#=============================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-08-19 1:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-08-26 1:00:01")

TRE4_19_fix$WC_15cm[TRE4_19_fix$WC_15cm > 0.152] <- NA
missing <- which(is.na(TRE4_19_fix$WC_15cm))

if(1 %in% missing){
  TRE4_19_fix$WC_15cm[1] <- head(TRE4_19_fix$WC_15cm[!is.na(TRE4_19_fix$WC_15cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_15cm[nrow(data)] <- tail(TRE4_19_fix$WC_15cm[!is.na(TRE4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_15cm[idx] <- (TRE4_19_fix$WC_15cm[r$starts[i]] + TRE4_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-08-19 1:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-08-26 1:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#30 cm 
##############################################################################################
TRE4_19$WC_30cm[TRE4_19$WC_30cm < 0.248] <- NA
missing <- which(is.na(TRE4_19$WC_30cm))

if(1 %in% missing){
  TRE4_19$WC_30cm[1] <- head(TRE4_19$WC_30cm[!is.na(TRE4_19$WC_30cm)],1)
}
if(nrow(TRE4_19) %in% missing){
  TRE4_19$WC_30cm[nrow(data)] <- tail(TRE4_19$WC_30cm[!is.na(TRE4_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19$WC_30cm[idx] <- (TRE4_19$WC_30cm[r$starts[i]] + TRE4_19$WC_30cm[r$ends[i]])/2
}

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-07-09 1:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-07-20 1:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm < 0.28] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-07-09 1:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-07-20 1:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-02-14 1:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-02-15 1:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm < 0.347] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-02-14 1:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-02-15 1:00:01")

TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-01-14 1:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-01-16 1:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm < 0.328] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-01-14 1:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-01-16 1:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-01-17 1:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-01-17 12:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm < 0.341] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-01-17 1:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-01-17 12:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-02-11 1:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-02-13 12:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm < 0.335] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-01-17 1:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-01-17 12:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-02-11 1:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-02-13 12:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm < 0.335] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-01-17 1:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-01-17 12:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-02-20 1:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-02-24 12:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.336] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-02-20 1:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-02-24 12:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-03-04 09:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-03-05 12:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.34] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-03-04 09:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-03-05 12:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-03-08 09:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-03-10 12:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm < 0.3365] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-03-08 09:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-03-10 12:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-03-17 09:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-03-19 12:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.3345] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-03-17 09:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-03-19 12:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-04-01 09:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-04-05 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.339] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-04-01 09:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-04-05 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-04-07 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-04-08 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.3445] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-04-07 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-04-08 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-04-11 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-04-18 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.34] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-04-11 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-04-18 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-04-12 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-04-18 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.339] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-04-12 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-04-18 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-04-14 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-04-18 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.3375 | TRE4_19_fix$WC_30cm < 0.335] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-04-14 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-04-18 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-04-18 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-04-20 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.3365] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-04-18 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-04-20 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-04-23 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-04-30 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.335] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-04-23 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-04-30 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-04-30 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-05-06 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.33] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-04-30 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-05-06 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-05-06 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-05-15 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.3250] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-05-06 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-05-15 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-05-11 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-05-15 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.322] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-05-11 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-05-15 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-05-12 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-05-15 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.3215] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-05-12 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-05-15 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-05-25 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-06-03 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.338] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-05-25 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-06-03 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-06-02 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-06-13 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.333] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-06-02 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-06-13 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-06-13 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-06-23 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.323] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-06-13 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-06-23 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-06-18 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-06-23 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.3158] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-06-18 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-06-23 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-06-20 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-06-23 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.3139] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-06-20 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-06-23 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-07-01 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-07-08 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.301] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-07-01 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-07-08 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Subset end of July 
#=================================================================================
TRE4_19_fix <- filter(TRE4_19, Date_time > "2019-07-04 11:00:01")
TRE4_19_fix <- filter(TRE4_19_fix, Date_time < "2019-07-08 00:00:01")

TRE4_19_fix$WC_30cm[TRE4_19_fix$WC_30cm > 0.299] <- NA
missing <- which(is.na(TRE4_19_fix$WC_30cm))

if(1 %in% missing){
  TRE4_19_fix$WC_30cm[1] <- head(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}
if(nrow(TRE4_19_fix) %in% missing){
  TRE4_19_fix$WC_30cm[nrow(data)] <- tail(TRE4_19_fix$WC_30cm[!is.na(TRE4_19_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_19_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_19_fix$WC_30cm[idx] <- (TRE4_19_fix$WC_30cm[r$starts[i]] + TRE4_19_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE4_19_early <- filter(TRE4_19, Date_time < "2019-07-04 11:00:01")
TRE4_19_late <- filter(TRE4_19, Date_time > "2019-07-08 00:00:01")
TRE4_19 <- bind_rows(TRE4_19_early, TRE4_19_late, TRE4_19_fix)

#Remove the missing dates
#=========================================================================
#Replace missing dates with NAs - 09/26 to 12/31
insertDF <- as.data.frame(matrix(data = NA, nrow = 96, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-09-27"), as.Date("2019-12-31"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE4_19 <- insertRows(TRE4_19, c(39608:39704), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE4_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE4 2020
##################################################################################################
TRE4_20 <- subset(TRE4, Year == '2020')

#Plotting 
TRE4_20$WC_15cm <- as.numeric(TRE4_20$WC_15cm)
TRE4_20$WC_30cm <- as.numeric(TRE4_20$WC_30cm)
TRE4_20$WC_100cm <- as.numeric(TRE4_20$WC_100cm)
TRE4_20$Date_time<- mdy_hms(TRE4_20$Date_time)

#15 cm 
#############################################################################################

#Fix 15 cm glitch upwards
#===============================================================================================
TRE4_20_fix <- filter(TRE4_20, Date_time < "2020-09-01 1:00:01")

TRE4_20_fix$WC_15cm[TRE4_20_fix$WC_15cm > 0.135] <- NA

#Recombine
TRE4_20_late <- filter(TRE4_20, Date_time > "2020-09-01 1:00:01")

TRE4_20 <- bind_rows(TRE4_20_late, TRE4_20_fix)

#Subset and remove weird drip in November
#============================================================================================
TRE4_20_fix <- filter(TRE4_20, Date_time > "2020-11-04 11:00:01")
TRE4_20_fix <- filter(TRE4_20_fix, Date_time < "2020-11-09 00:00:01")

TRE4_20_fix$WC_15cm[TRE4_20_fix$WC_15cm > 0.125 | TRE4_20_fix$WC_15cm < 0.12] <- NA
missing <- which(is.na(TRE4_20_fix$WC_15cm))

if(1 %in% missing){
  TRE4_20_fix$WC_15cm[1] <- head(TRE4_20_fix$WC_15cm[!is.na(TRE4_20_fix$WC_15cm)],1)
}
if(nrow(TRE4_20_fix) %in% missing){
  TRE4_20_fix$WC_15cm[nrow(data)] <- tail(TRE4_20_fix$WC_15cm[!is.na(TRE4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_20_fix$WC_15cm[idx] <- (TRE4_20_fix$WC_15cm[r$starts[i]] + TRE4_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_20_early <- filter(TRE4_20, Date_time < "2020-11-04 11:00:01")
TRE4_20_late <- filter(TRE4_20, Date_time > "2020-11-09 00:00:01")
TRE4_20<- bind_rows(TRE4_20_early, TRE4_20_late, TRE4_20_fix)

#Remove the missing dates
#=========================================================================
#Replace missing dates with NAs - 01/01 to 08/21
insertDF <- as.data.frame(matrix(data = NA, nrow = 233, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-01-01"), as.Date("2020-08-20"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE4_20 <- insertRows(TRE4_20, c(1:233), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE4_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE4 2021
##################################################################################################
TRE4_21 <- subset(TRE4, Year == '2021')

#Plotting 
TRE4_21$WC_15cm <- as.numeric(TRE4_21$WC_15cm)
TRE4_21$WC_30cm <- as.numeric(TRE4_21$WC_30cm)
TRE4_21$WC_100cm <- as.numeric(TRE4_21$WC_100cm)
TRE4_21$Date_time<- mdy_hms(TRE4_21$Date_time)

#15 cm
###########################################################################################

#Fix negative ones
#=============================================================================================
TRE4_21$WC_15cm[TRE4_21$WC_15cm < 0.12] <- NA
missing <- which(is.na(TRE4_21$WC_15cm))

if(1 %in% missing){
  TRE4_21$WC_15cm[1] <- head(TRE4_21$WC_15cm[!is.na(TRE4_21$WC_15cm)],1)
}
if(nrow(TRE4_21) %in% missing){
  TRE4_21$WC_15cm[nrow(data)] <- tail(TRE4_21$WC_15cm[!is.na(TRE4_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21$WC_15cm[idx] <- (TRE4_21$WC_15cm[r$starts[i]] + TRE4_21$WC_15cm[r$ends[i]])/2
}

#Remove early dips
#==================================================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-01-29 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-15 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.27] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Fix increases 
#========================================================
#Create a new column that is the percent difference between the row below and the row above 
TRE4_21_fix <- TRE4_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================

#Make increase column not a percent 
TRE4_21_fix <- transform(TRE4_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

TRE4_21_fix <- transform(TRE4_21_fix, WC_15cm=ifelse(incr < -0.00001, 
                                                     as.numeric(stats::filter(WC_15cm, rep(1/144, 144), sides=2)), 
                                                     WC_15cm))

#Recombine
#===============================================
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-01-29 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-15 1:00:01")

TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-15 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-01 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.24] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Fix increases 
#========================================================
#Create a new column that is the percent difference between the row below and the row above 
TRE4_21_fix <- TRE4_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================

#Make increase column not a percent 
TRE4_21_fix <- transform(TRE4_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

TRE4_21_fix <- transform(TRE4_21_fix, WC_15cm=ifelse(incr < -0.00001, 
                                                     as.numeric(stats::filter(WC_15cm, rep(1/300, 300), sides=2)), 
                                                     WC_15cm))

#Recombine
#===============================================
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-01-29 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-15 1:00:01")

TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================

TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-15 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-22 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.268] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Create a new column that is the percent difference between the row below and the row above 
TRE4_21_fix <- TRE4_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================

#Make increase column not a percent 
TRE4_21_fix <- transform(TRE4_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

TRE4_21_fix <- transform(TRE4_21_fix, WC_15cm=ifelse(incr < -0.00001, 
                                                     as.numeric(stats::filter(WC_15cm, rep(1/48, 48), sides=2)), 
                                                     WC_15cm))

TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-15 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-22 1:00:01")

TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================

TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-22 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-28 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.25] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-22 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-28 1:00:01")

TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================

TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-28 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-31 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.24] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-28 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-31 1:00:01")

TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================

TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-31 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-05 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.235] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-31 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-05 1:00:01")

TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================

TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-05 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-10 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.22] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-05 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-10 1:00:01")

TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================

TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-10 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-18 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.208] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-10 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-18 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================

TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-17 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-24 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-17 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-24 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-25 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-05 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-25 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-05 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-05-05 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-15 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.18] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-05-05 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-15 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-05-15 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-24 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.176] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-05-15 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-24 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-05-25 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-31 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.167] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-05-25 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-31 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-06-01 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-06-10 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.15] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-06-01 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-06-10 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-06-11 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-06-20 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.15] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-06-11 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-06-20 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-06-21 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-06-30 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.13] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-06-11 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-06-20 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-07-01 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-07-10 1:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.125] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-07-01 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-07-10 1:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-15 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-16 01:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2795] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-15 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-16 01:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-16 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-20 01:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.275] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-16 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-20 01:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-20 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-21 01:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2725] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-20 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-21 01:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-22 1:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-22 12:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2675] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-22 1:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-22 12:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-22 11:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-22 20:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2665] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-22 11:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-22 20:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-22 22:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-23 04:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2655] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-22 22:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-23 04:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-24 04:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-25 04:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2614] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-24 04:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-25 04:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-25 04:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-26 04:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2585] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-25 04:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-26 04:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-26 04:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-28 04:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2534] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-26 04:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-28 04:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-28 04:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-03-29 06:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.250] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-28 04:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-03-29 06:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-03-29 04:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-01 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.241] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-03-29 04:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-01 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-06 04:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-08 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.227] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-06 04:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-08 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-10 04:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-12 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.220] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-10 04:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-12 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-12 04:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-14 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.215] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-12 04:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-14 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-19 04:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-19 10:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.207] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-19 04:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-19 10:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-26 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-27 01:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.235] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-26 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-27 01:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-27 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-29 01:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.221] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-27 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-29 01:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-29 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-02 01:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.213] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-29 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-02 01:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-05-02 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-03 14:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.208] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-05-02 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-03 14:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-05-05 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-07 14:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.1991] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-05-05 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-07 14:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-05-07 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-11 14:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.192] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-05-07 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-11 14:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-05-11 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-21 14:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.18] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-05-11 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-21 14:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-05-25 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-28 14:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.1737] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-05-25 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-28 14:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-05-28 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-06-01 14:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.168] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-05-28 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-06-01 14:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-06-01 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-06-03 14:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.168] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-06-01 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-06-03 14:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-06-03 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-06-07 20:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.16] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-06-03 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-06-07 20:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)


#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-03 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-13 20:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.215] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-03 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-13 20:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-13 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-20 20:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.2049] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-13 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-20 20:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-25 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-27 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.235] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-25 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-27 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-27 00:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-27 20:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.228] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-27 00:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-27 20:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-27 20:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-04-28 10:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.224] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-27 20:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-04-28 10:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-04-28 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-05-03 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.21] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-04-28 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-05-03 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-06-23 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-07-04 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.132] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-06-23 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-07-04 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-07-01 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-07-12 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.1275] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-07-01 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-07-12 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-07-12 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-07-17 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.126 | TRE4_21_fix$WC_15cm > 0.1301] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-07-12 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-07-17 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-07-18 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-07-22 06:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm > 0.1262] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-07-18 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-07-22 06:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-06-18 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-06-24 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.15] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-06-18 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-06-24 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-07-26 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-08-04 00:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.1405] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-07-26 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-08-04 00:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Calibrate 
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-06-24 11:10:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-07-22 10:00:01")

TRE4_21_fix$WC_15cm <- TRE4_21_fix$WC_15cm + 0.014

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-06-24 11:10:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-07-22 10:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-07-20 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-07-23 10:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm > 0.14 | TRE4_21_fix$WC_15cm < 0.132] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-07-20 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-07-23 10:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Subset to fix drips again
#==============================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-08-02 10:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-08-23 10:00:01")

TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm < 0.138] <- NA
missing <- which(is.na(TRE4_21_fix$WC_15cm))

if(1 %in% missing){
  TRE4_21_fix$WC_15cm[1] <- head(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}
if(nrow(TRE4_21_fix) %in% missing){
  TRE4_21_fix$WC_15cm[nrow(data)] <- tail(TRE4_21_fix$WC_15cm[!is.na(TRE4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE4_21_fix$WC_15cm[idx] <- (TRE4_21_fix$WC_15cm[r$starts[i]] + TRE4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-08-02 10:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-08-23 10:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Remove glitches and add Missing Dates
#########################################################################

#Remove extra columns 
#=======================================================================
TRE4_21 <- TRE4_21[c(1:6)]

#Remove glitch before 06/08 missing dates
#=======================================================================
TRE4_21_fix <- filter(TRE4_21, Date_time > "2021-06-07 12:00:01")
TRE4_21_fix <- filter(TRE4_21_fix, Date_time < "2021-06-08 15:00:01")

TRE4_21_fix$WC_100cm[TRE4_21_fix$WC_100cm > 0.355] <- NA
TRE4_21_fix$WC_30cm[TRE4_21_fix$WC_30cm > 0.3110] <- NA
TRE4_21_fix$WC_15cm[TRE4_21_fix$WC_15cm > 0.1675] <- NA

#Recombine
TRE4_21_early <- filter(TRE4_21, Date_time < "2021-06-07 12:00:01")
TRE4_21_late <- filter(TRE4_21, Date_time > "2021-06-08 15:00:01")
TRE4_21 <- bind_rows(TRE4_21_early, TRE4_21_late, TRE4_21_fix)

#Replace missing dates with NAs - 01-29 to 03-15
#========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 44, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-01-30"), as.Date("2021-03-14"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE4_21 <- insertRows(TRE4_21, c(4075:4118), new = insertDF)

#Replace missing dates with NAs - 06-08 to 06-20
#========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 13, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-06-08"), as.Date("2021-06-20"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE4_21 <- insertRows(TRE4_21, c(18839:18851), new = insertDF)

#Plot again
Soil <- ggplot(data = subset(TRE4_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 


#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
TRE4_clean <- merge(TRE4_18, TRE4_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
TRE4_clean <- merge(TRE4_clean, TRE4_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
TRE4_clean <- merge(TRE4_clean, TRE4_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
TRE4_clean <- merge(TRE4_clean, TRE4_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(TRE4_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("TRE4", width = 4500, height = 2500)

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
write.csv(TRE4_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE4_clean.csv" ) #this writes a csv file and sends it to the working folder



