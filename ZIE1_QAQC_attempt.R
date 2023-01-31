#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 01/31/2023
#Description: QA/QC ZIE1

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
                        pattern=glob2rx("Copy of Z1M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
ZIE1_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("Copy of Z1M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
ZIE1_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
ZIE1 <- rbind(ZIE1_2017_2019, ZIE1_2019_2021)

#Write the csv
write.csv(ZIE1,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE1.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(ZIE1)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
ZIE1$Date <- mdy_hms(ZIE1$Date_time)

#Put year into a separate column 
ZIE1 <- separate(ZIE1, Date, c("Year"))

#ZIE1 2017
##################################################################################################
ZIE1_17 <- subset(ZIE1, Year == '2017')

#Plotting 
ZIE1_17$WC_15cm <- as.numeric(ZIE1_17$WC_15cm)
ZIE1_17$WC_30cm <- as.numeric(ZIE1_17$WC_30cm)
ZIE1_17$WC_100cm <- as.numeric(ZIE1_17$WC_100cm)
ZIE1_17$Date_time<- mdy_hms(ZIE1_17$Date_time)

Soil <- ggplot(data = subset(ZIE1_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE1 2018
##################################################################################################
ZIE1_18 <- subset(ZIE1, Year == '2018')

#Plotting 
ZIE1_18$WC_15cm <- as.numeric(ZIE1_18$WC_15cm)
ZIE1_18$WC_30cm <- as.numeric(ZIE1_18$WC_30cm)
ZIE1_18$WC_100cm <- as.numeric(ZIE1_18$WC_100cm)
ZIE1_18$Date_time<- mdy_hms(ZIE1_18$Date_time)

# #Fix 30 cm drip in November
# #==============================================================================================
# ZIE1_18fix <- filter(ZIE1_18, Date_time > "2018-10-01 1:00:01")
# ZIE1_18fix <- filter(ZIE1_18fix, Date_time < "2018-12-24 1:00:01")
# 
# Soil <- ggplot(data = subset(ZIE1_18fix, !is.na(Date_time)), aes(x = Date_time)) + 
#   geom_line(aes(y = WC_100cm, color = "navyblue")) + 
#   geom_line(aes(y = WC_30cm, color = "blue")) + 
#   geom_line(aes(y = WC_15cm, color = "lightblue"))
# Soil 
# 
# ZIE1_18fix$WC_30cm[ZIE1_18fix$WC_30cm < 0] <- NA
# missing <- which(is.na(ZIE1_18fix$WC_30cm))
# 
# if(1 %in% missing){
#   ZIE1_18fix$WC_30cm[1] <- head(ZIE1_18fix$WC_30cm[!is.na(ZIE1_18fix$WC_30cm)],1)
# }
# if(nrow(ZIE1_18fix) %in% missing){
#   ZIE1_18fix$WC_30cm[nrow(data)] <- tail(ZIE1_18fix$WC_30cm[!is.na(ZIE1_18fix$WC_30cm)],1)
# }
# 
# #Find start and ends of each run of NAs
# get_runs <- function(x){
#   starts <- which(diff(x) == 1)
#   y <- rle(x)
#   len <- y$lengths[y$values==TRUE]
#   ends <- starts + len+1
#   return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
# }
# 
# r <- get_runs(is.na(ZIE1_18fix$WC_30cm))
# 
# 
# for(i in r$i){
#   idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
#   ZIE1_18fix$WC_30cm[idx] <- (ZIE1_18fix$WC_30cm[r$starts[i]] + ZIE1_18fix$WC_30cm[r$ends[i]])/2
# }
# 
# #Recombine 
# ZIE1_18early <- filter(ZIE1_18, Date_time < "2020-05-25 1:00:01")
# ZIE1_18late <- filter(ZIE1_18, Date_time > "2020-09-04 1:00:01")
# 
# ZIE1_18fix <- bind_rows(ZIE1_18early, ZIE1_18late, ZIE1_18fix)

#Plot again 
Soil <- ggplot(data = subset(ZIE1_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE1 2019
##################################################################################################
ZIE1_19 <- subset(ZIE1, Year == '2019')

#Plotting 
ZIE1_19$WC_15cm <- as.numeric(ZIE1_19$WC_15cm)
ZIE1_19$WC_30cm <- as.numeric(ZIE1_19$WC_30cm)
ZIE1_19$WC_100cm <- as.numeric(ZIE1_19$WC_100cm)
ZIE1_19$Date_time<- mdy_hms(ZIE1_19$Date_time)

# #Fix 100 cm drip in May 
# #==============================================================================================
ZIE1_19fix <- filter(ZIE1_19, Date_time > "2019-05-20 1:00:01")
ZIE1_19fix<- filter(ZIE1_19fix, Date_time < "2019-05-22 1:00:01")

ZIE1_19fix$WC_100cm[ZIE1_19fix$WC_100cm < 0.49] <- NA
missing <- which(is.na(ZIE1_19fix$WC_100cm))

if(1 %in% missing){
  ZIE1_19fix$WC_100cm[1] <- head(ZIE1_19fix$WC_100cm[!is.na(ZIE1_19fix$WC_100cm)],1)
}
if(nrow(ZIE1_19fix) %in% missing){
  ZIE1_19fix$WC_100cm[nrow(data)] <- tail(ZIE1_19fix$WC_100cm[!is.na(ZIE1_19fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_19fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_19fix$WC_100cm[idx] <- (ZIE1_19fix$WC_100cm[r$starts[i]] + ZIE1_19fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE1_19early <- filter(ZIE1_19, Date_time < "2019-05-20 1:00:01")
ZIE1_19late <- filter(ZIE1_19, Date_time > "2019-05-22 1:00:01")
ZIE1_19 <- bind_rows(ZIE1_19early, ZIE1_19late, ZIE1_19fix)

#Plot again 
Soil <- ggplot(data = subset(ZIE1_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE1 2020
##################################################################################################
ZIE1_20 <- subset(ZIE1, Year == '2020')

#Plotting 
ZIE1_20$WC_15cm <- as.numeric(ZIE1_20$WC_15cm)
ZIE1_20$WC_30cm <- as.numeric(ZIE1_20$WC_30cm)
ZIE1_20$WC_100cm <- as.numeric(ZIE1_20$WC_100cm)
ZIE1_20$Date_time<- mdy_hms(ZIE1_20$Date_time)

#15 cm
###########################################
ZIE1_20$WC_15cm[ZIE1_20$WC_15cm < 0] <- NA
missing <- which(is.na(ZIE1_20$WC_15cm))

if(1 %in% missing){
  ZIE1_20$WC_15cm[1] <- head(ZIE1_20$WC_15cm[!is.na(ZIE1_20$WC_15cm)],1)
}
if(nrow(ZIE1_20) %in% missing){
  ZIE1_20$WC_15cm[nrow(data)] <- tail(ZIE1_20$WC_15cm[!is.na(ZIE1_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20$WC_15cm[idx] <- (ZIE1_20$WC_15cm[r$starts[i]] + ZIE1_20$WC_15cm[r$ends[i]])/2
}

#Subset 
#===========================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-05-25 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-09-04 1:00:01")
ZIE1_20fix$WC_15cm[ZIE1_20fix$WC_15cm == 0.3019] <- NA

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-05-25 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-09-04 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#30 cm 
################################################################
ZIE1_20$WC_30cm[ZIE1_20$WC_30cm < 0] <- NA
missing <- which(is.na(ZIE1_20$WC_30cm))

if(1 %in% missing){
  ZIE1_20$WC_30cm[1] <- head(ZIE1_20$WC_30cm[!is.na(ZIE1_20$WC_30cm)],1)
}
if(nrow(ZIE1_20) %in% missing){
  ZIE1_20$WC_30cm[nrow(data)] <- tail(ZIE1_20$WC_30cm[!is.na(ZIE1_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20$WC_30cm[idx] <- (ZIE1_20$WC_30cm[r$starts[i]] + ZIE1_20$WC_30cm[r$ends[i]])/2
}

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-03-09 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-03-16 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm < 0.2797] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-03-09 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-03-16 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-03-16 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-03-21 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm > 0.2925] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-03-16 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-03-21 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-03-21 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-03-30 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm > 0.2935] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-03-21 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-03-30 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-04-13 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-04-30 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm > 0.296] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-04-13 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-04-30 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-04-18 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-04-30 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm > 0.294] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-04-18 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-04-30 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-04-25 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-04-30 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm > 0.2915] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-04-25 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-04-30 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-05-11 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-05-15 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm < 0.282] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-05-11 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-05-15 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-06-25 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-06-29 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm > 0.2785] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-06-25 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-06-29 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-09-01 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-09-15 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm > 0.2187] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-09-01 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-09-15 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-09-08 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-09-20 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm > 0.2175] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-09-08 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time >"2020-09-20 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-09-20 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-09-30 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm < 0.212 | ZIE1_20fix$WC_30cm > 0.217] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-09-20 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time >"2020-09-30 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Subset and removed glitches
#==================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-09-30 1:00:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-10-08 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm < 0.211 | ZIE1_20fix$WC_30cm > 0.215] <- NA
missing <- which(is.na(ZIE1_20fix$WC_30cm))

if(1 %in% missing){
  ZIE1_20fix$WC_30cm[1] <- head(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}
if(nrow(ZIE1_20fix) %in% missing){
  ZIE1_20fix$WC_30cm[nrow(data)] <- tail(ZIE1_20fix$WC_30cm[!is.na(ZIE1_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_20fix$WC_30cm[idx] <- (ZIE1_20fix$WC_30cm[r$starts[i]] + ZIE1_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-09-30 1:00:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-10-08 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Remove glitch 
#==================================================================
ZIE1_20$WC_30cm[ZIE1_20$WC_30cm == 0.25445] <- NA

#Missing dates and glitches
##########################################################################

#Remove glitch before missing date
#==========================================================================
ZIE1_20fix <- filter(ZIE1_20, Date_time > "2020-08-15 12:30:01")
ZIE1_20fix <- filter(ZIE1_20fix, Date_time < "2020-08-18 1:00:01")

ZIE1_20fix$WC_30cm[ZIE1_20fix$WC_30cm > 0] <- NA
ZIE1_20fix$WC_100cm[ZIE1_20fix$WC_100cm > 0] <- NA

#Recombine 
ZIE1_20early <- filter(ZIE1_20, Date_time < "2020-08-15 12:30:01")
ZIE1_20late <- filter(ZIE1_20, Date_time > "2020-08-18 1:00:01")
ZIE1_20 <- bind_rows(ZIE1_20early, ZIE1_20late, ZIE1_20fix)

#Replace missing dates
#================================================================================================
#Replace missing dates with NAs - 08/16 to 08/31
insertDF <- as.data.frame(matrix(data = NA, nrow = 13, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-08-17"), as.Date("2020-08-29"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE1_20 <- insertRows(ZIE1_20, c(32849:32862), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(ZIE1_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE1 2021
##################################################################################################
ZIE1_21 <- subset(ZIE1, Year == '2021')

#Plotting 
ZIE1_21$WC_15cm <- as.numeric(ZIE1_21$WC_15cm)
ZIE1_21$WC_30cm <- as.numeric(ZIE1_21$WC_30cm)
ZIE1_21$WC_100cm <- as.numeric(ZIE1_21$WC_100cm)
ZIE1_21$Date_time<- mdy_hms(ZIE1_21$Date_time)

#15 cm
###################################################################################
ZIE1_21$WC_15cm[ZIE1_21$WC_15cm < 0.2327] <- NA
missing <- which(is.na(ZIE1_21$WC_15cm))

if(1 %in% missing){
  ZIE1_21$WC_15cm[1] <- head(ZIE1_21$WC_15cm[!is.na(ZIE1_21$WC_15cm)],1)
}
if(nrow(ZIE1_21) %in% missing){
  ZIE1_21$WC_15cm[nrow(data)] <- tail(ZIE1_21$WC_15cm[!is.na(ZIE1_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21$WC_15cm[idx] <- (ZIE1_21$WC_15cm[r$starts[i]] + ZIE1_21$WC_15cm[r$ends[i]])/2
}

#Subset early in year
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-02-01 1:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-03-16 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.294] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-02-01 1:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-03-16 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Subset early in year
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-03-17 1:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-03-31 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.292] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-03-17 1:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-03-31 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Subset early in year
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-01 1:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-04-15 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.277] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-01 1:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-04-15 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Subset early in year
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-15 1:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-04-25 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.28] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-15 1:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-04-25 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Subset early in year
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-26 1:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-05-10 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.289] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-26 1:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-05-10 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Subset early in year
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-05-11 1:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-05-24 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.279] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-05-11 1:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-05-24 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Subset early in year
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-05-25 1:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-06-01 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.272] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-05-25 1:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-06-01 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-05-03 1:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-05-17 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.2755] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-05-03 1:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-05-17 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-05-17 1:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-05-30 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.272] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-05-17 1:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-05-30 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-25 3:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-04-27 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.30] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-25 3:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-04-27 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-02-14 3:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-02-16 1:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.312] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-02-14 3:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-02-16 1:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-02-15 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-02-16 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.315] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-02-15 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-02-16 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-02-16 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-02-22 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.306] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-02-16 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-02-22 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-02-22 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-02-24 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.305] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-02-22 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-02-24 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-02-24 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-02-28 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.301] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-02-24 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-02-28 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-02-24 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-02-27 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.3027] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-02-24 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-02-27 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-02-24 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-02-25 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.3051] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-02-24 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-02-25 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-02-28 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-03-01 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.3005] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-02-28 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-03-01 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-03-06 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-03-07 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.3125] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-03-06 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-03-07 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-09 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-04-12 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.288] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-09 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-04-12 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-12 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-04-18 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.285] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-12 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-04-18 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-21 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-04-23 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.282] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-21 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-04-23 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-21 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-04-22 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.2829] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-21 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-04-22 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-27 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-04-30 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.2975] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-27 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-04-30 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-05-10 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-05-30 00:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm > 0.29] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-05-10 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-05-30 00:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-05-11 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-05-13 12:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.2863] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-05-11 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-05-13 12:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix drips in May/June
#============================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-05-16 00:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-05-18 12:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm < 0.283] <- NA
missing <- which(is.na(ZIE1_21fix$WC_15cm))

if(1 %in% missing){
  ZIE1_21fix$WC_15cm[1] <- head(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}
if(nrow(ZIE1_21fix) %in% missing){
  ZIE1_21fix$WC_15cm[nrow(data)] <- tail(ZIE1_21fix$WC_15cm[!is.na(ZIE1_21fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21fix$WC_15cm[idx] <- (ZIE1_21fix$WC_15cm[r$starts[i]] + ZIE1_21fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-05-16 00:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-05-18 12:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#30 cm 
################################################################
ZIE1_21$WC_30cm[ZIE1_21$WC_30cm < 0.235] <- NA
missing <- which(is.na(ZIE1_21$WC_30cm))

if(1 %in% missing){
  ZIE1_21$WC_30cm[1] <- head(ZIE1_21$WC_30cm[!is.na(ZIE1_21$WC_30cm)],1)
}
if(nrow(ZIE1_21) %in% missing){
  ZIE1_21$WC_30cm[nrow(data)] <- tail(ZIE1_21$WC_30cm[!is.na(ZIE1_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21$WC_30cm[idx] <- (ZIE1_21$WC_30cm[r$starts[i]] + ZIE1_21$WC_30cm[r$ends[i]])/2
}

#100 cm 
################################################################
ZIE1_21$WC_100cm[ZIE1_21$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE1_19$WC_100cm))

if(1 %in% missing){
  ZIE1_21$WC_100cm[1] <- head(ZIE1_21$WC_100cm[!is.na(ZIE1_21$WC_100cm)],1)
}
if(nrow(ZIE1_21) %in% missing){
  ZIE1_21$WC_100cm[nrow(data)] <- tail(ZIE1_21$WC_100cm[!is.na(ZIE1_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE1_21$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE1_21$WC_100cm[idx] <- (ZIE1_21$WC_100cm[r$starts[i]] + ZIE1_21$WC_100cm[r$ends[i]])/2
}

#Glitches and missing dates
############################################################################

#Fix glitches
#================================================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-06-07 23:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-06-10 12:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm >0 ] <- NA
ZIE1_21fix$WC_30cm[ZIE1_21fix$WC_30cm >0 ] <- NA
ZIE1_21fix$WC_100cm[ZIE1_21fix$WC_100cm >0 ] <- NA

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-06-07 23:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-06-10 12:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix glitches
#================================================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-04-03 10:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-04-04 12:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm >0 ] <- NA
ZIE1_21fix$WC_30cm[ZIE1_21fix$WC_30cm >0 ] <- NA
ZIE1_21fix$WC_100cm[ZIE1_21fix$WC_100cm >0 ] <- NA

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-04-03 10:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-04-04 12:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix glitches
#================================================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-07-04 19:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-07-07 12:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm >0 ] <- NA
ZIE1_21fix$WC_30cm[ZIE1_21fix$WC_30cm >0 ] <- NA
ZIE1_21fix$WC_100cm[ZIE1_21fix$WC_100cm >0 ] <- NA

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-07-04 19:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-07-07 12:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Fix glitches
#================================================================================================
ZIE1_21fix <- filter(ZIE1_21, Date_time > "2021-07-24 12:00:01")
ZIE1_21fix <- filter(ZIE1_21fix, Date_time < "2021-07-26 12:00:01")

ZIE1_21fix$WC_15cm[ZIE1_21fix$WC_15cm >0 ] <- NA
ZIE1_21fix$WC_30cm[ZIE1_21fix$WC_30cm >0 ] <- NA
ZIE1_21fix$WC_100cm[ZIE1_21fix$WC_100cm >0 ] <- NA

#Recombine 
ZIE1_21early <- filter(ZIE1_21, Date_time < "2021-07-04 19:00:01")
ZIE1_21late <- filter(ZIE1_21, Date_time > "2021-07-07 12:00:01")
ZIE1_21 <- bind_rows(ZIE1_21early, ZIE1_21late, ZIE1_21fix)

#Replace missing dates
#================================================================================================

#Replace missing dates with NAs - 06/09 to 06/22
insertDF <- as.data.frame(matrix(data = NA, nrow = 12, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-06-10"), as.Date("2021-06-21"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE1_21<- insertRows(ZIE1_21, c(22393:22404), new = insertDF)

#Replace missing dates with NAs - 07/05 to 07/15
insertDF <- as.data.frame(matrix(data = NA, nrow = 9, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-07-06"), as.Date("2021-07-14"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE1_21<- insertRows(ZIE1_21, c(24339:24347), new = insertDF)

#Replace missing dates with NAs - 07/25 to 07/27
insertDF <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-07-26"), as.Date("2021-07-26"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE1_21<- insertRows(ZIE1_21, c(25772), new = insertDF)

#Replace missing dates with NAs - 04/03 to 04/07
insertDF <- as.data.frame(matrix(data = NA, nrow = 3, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2021-04-04"), as.Date("2021-04-06"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE1_21<- insertRows(ZIE1_21, c(13313:13315), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(ZIE1_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
ZIE1_clean <- merge(ZIE1_18, ZIE1_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
ZIE1_clean <- merge(ZIE1_clean, ZIE1_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE1_clean <- merge(ZIE1_clean, ZIE1_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE1_clean <- merge(ZIE1_clean, ZIE1_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(ZIE1_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("ZIE1_Salli", width = 4500, height = 2500)

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
write.csv(ZIE1_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE1_clean.csv" ) #this writes a csv file and sends it to the working folder

