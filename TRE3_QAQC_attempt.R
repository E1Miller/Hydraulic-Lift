#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/23/2023
#Description: QA/QC TRE 3

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

#CREATING ONE FULL DATASET FOR 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new", 
                        pattern=glob2rx("Copy of T3M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console


#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
TRE3_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("Copy of T3M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
TRE3_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
TRE3 <- rbind(TRE3_2017_2019, TRE3_2019_2021)

#Write the csv
write.csv(TRE3,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE3.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(TRE3)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
TRE3$Date <- mdy_hms(TRE3$Date_time)

#Put year into a separate column 
TRE3 <- separate(TRE3, Date, c("Year"))

#TRE3 2017
##################################################################################################
TRE3_17 <- subset(TRE3, Year == '2017')

#Plotting 
TRE3_17$WC_15cm <- as.numeric(TRE3_17$WC_15cm)
TRE3_17$WC_30cm <- as.numeric(TRE3_17$WC_30cm)
TRE3_17$WC_100cm <- as.numeric(TRE3_17$WC_100cm)
TRE3_17$Date_time<- mdy_hms(TRE3_17$Date_time)

Soil <- ggplot(data = subset(TRE3_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE3 2018
##################################################################################################
TRE3_18 <- subset(TRE3, Year == '2018')

#Plotting 
TRE3_18$WC_15cm <- as.numeric(TRE3_18$WC_15cm)
TRE3_18$WC_30cm <- as.numeric(TRE3_18$WC_30cm)
TRE3_18$WC_100cm <- as.numeric(TRE3_18$WC_100cm)
TRE3_18$Date_time<- mdy_hms(TRE3_18$Date_time)

#Calibrate 15 cm July section
#================================================================================
TRE3_18_fix <- filter(TRE3_18, Date_time > "2018-07-06 05:10:01")
TRE3_18_fix <- filter(TRE3_18_fix, Date_time < "2018-07-10 03:40:01")

TRE3_18_fix$WC_15cm <- TRE3_18_fix$WC_15cm + 0.0032

#Recombine
TRE3_18_early <- filter(TRE3_18, Date_time < "2018-07-06 05:10:01")
TRE3_18_late <- filter(TRE3_18, Date_time > "2018-07-10 03:40:01")
TRE3_18 <- bind_rows(TRE3_18_early, TRE3_18_late, TRE3_18_fix)

#Calibrate 30 cm September section
#================================================================================
TRE3_18_fix <- filter(TRE3_18, Date_time > "2018-08-24 06:30:01")
TRE3_18_fix <- filter(TRE3_18_fix, Date_time < "2018-09-07 10:30:01")

TRE3_18_fix$WC_30cm <- TRE3_18_fix$WC_30cm - 0.0027

#Recombine
TRE3_18_early <- filter(TRE3_18, Date_time < "2018-08-24 06:30:01")
TRE3_18_late <- filter(TRE3_18, Date_time > "2018-09-07 10:30:01")
TRE3_18 <- bind_rows(TRE3_18_early, TRE3_18_late, TRE3_18_fix)

#Calibrate 30 cm September section
#================================================================================
TRE3_18_fix <- filter(TRE3_18, Date_time > "2018-11-20 06:30:01")
TRE3_18_fix <- filter(TRE3_18_fix, Date_time < "2018-11-29 05:30:01")

TRE3_18_fix$WC_30cm[TRE3_18_fix$WC_30cm < 0.235] <- NA

#Recombine
TRE3_18_early <- filter(TRE3_18, Date_time < "2018-11-20 06:30:01")
TRE3_18_late <- filter(TRE3_18, Date_time > "2018-11-29 05:30:01")
TRE3_18 <- bind_rows(TRE3_18_early, TRE3_18_late, TRE3_18_fix)

#Plot again 
Soil <- ggplot(data = subset(TRE3_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE3 2019
##################################################################################################
TRE3_19 <- subset(TRE3, Year == '2019')

#Plotting 
TRE3_19$WC_15cm <- as.numeric(TRE3_19$WC_15cm)
TRE3_19$WC_30cm <- as.numeric(TRE3_19$WC_30cm)
TRE3_19$WC_100cm <- as.numeric(TRE3_19$WC_100cm)
TRE3_19$Date_time<- mdy_hms(TRE3_19$Date_time)

#30 cm 
################################################################
TRE3_19$WC_30cm[TRE3_19$WC_30cm < 0] <- NA
missing <- which(is.na(TRE3_19$WC_30cm))

if(1 %in% missing){
  TRE3_19$WC_30cm[1] <- head(TRE3_19$WC_30cm[!is.na(TRE3_19$WC_30cm)],1)
}
if(nrow(TRE3_19) %in% missing){
  TRE3_19$WC_30cm[nrow(data)] <- tail(TRE3_19$WC_30cm[!is.na(TRE3_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_19$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_19$WC_30cm[idx] <- (TRE3_19$WC_30cm[r$starts[i]] + TRE3_19$WC_30cm[r$ends[i]])/2
}

#Remove the missing dates
#=========================================================================
#Replace missing dates with NAs - 09/26 to 12/31
insertDF <- as.data.frame(matrix(data = NA, nrow = 96, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-09-27"), as.Date("2019-12-31"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE3_19 <- insertRows(TRE3_19, c(38655:38751), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE3_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 


#TRE3 2020
##################################################################################################
TRE3_20 <- subset(TRE3, Year == '2020')

#Plotting 
TRE3_20$WC_15cm <- as.numeric(TRE3_20$WC_15cm)
TRE3_20$WC_30cm <- as.numeric(TRE3_20$WC_30cm)
TRE3_20$WC_100cm <- as.numeric(TRE3_20$WC_100cm)
TRE3_20$Date_time<- mdy_hms(TRE3_20$Date_time)

#15 cm
###################################################################################

TRE3_20$WC_15cm[TRE3_20$WC_15cm < 0] <- NA
missing <- which(is.na(TRE3_20$WC_15cm))

if(1 %in% missing){
  TRE3_20$WC_15cm[1] <- head(TRE3_20$WC_15cm[!is.na(TRE3_20$WC_15cm)],1)
}
if(nrow(TRE3_20) %in% missing){
  TRE3_20$WC_15cm[nrow(data)] <- tail(TRE3_20$WC_15cm[!is.na(TRE3_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20$WC_15cm[idx] <- (TRE3_20$WC_15cm[r$starts[i]] + TRE3_20$WC_15cm[r$ends[i]])/2
}

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-09-29 18:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-10-03 00:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.1575] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-09-29 18:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-10-03 00:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-04 1:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-09 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.342] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-04 1:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-09 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-05 1:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-09 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.3465] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-05 1:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-09 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-06 11:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-09 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.346] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-06 11:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-09 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-08 11:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-14 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.338] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-08 11:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-14 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-08 11:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-10 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.345 | TRE3_20_fix$WC_15cm < 0.341] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-08 11:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-10 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-15 11:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-19 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.367] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-15 11:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-19 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-16 05:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-19 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.372] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-16 05:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-19 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-19 05:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-23 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.36] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-19 05:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-23 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-20 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-23 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.369] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-20 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-23 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-25 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-27 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.37] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-25 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-27 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-25 15:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-27 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.3765] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-25 15:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-27 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-25 15:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-27 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.3765] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-25 15:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-27 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-25 15:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-26 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.3735] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-25 15:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-26 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-26 15:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-30 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.367 | TRE3_20_fix$WC_15cm > 0.375] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-26 15:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-30 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-29 15:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-04-02 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.367 | TRE3_20_fix$WC_15cm > 0.375] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-29 15:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-04-02 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-04-01 15:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-04-06 12:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.369] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-04-01 15:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-04-06 12:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-04-04 15:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-04-05 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.3789] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-04-04 15:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-04-05 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-04-04 15:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-04-08 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.3695] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-04-04 15:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-04-08 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-04-06 20:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-04-15 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.372] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-04-06 20:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-04-15 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-04-06 20:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-04-09 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.369] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-04-06 20:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-04-09 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-04-11 20:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-04-18 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.368] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-04-11 20:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-04-18 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-02-24 20:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-01 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.348] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-02-24 20:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-01 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-02-26 20:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-01 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm > 0.3525] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-02-26 20:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-01 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-02 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-04 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.3525] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-02 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-04 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-04 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-09 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.346] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-04 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-09 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-14 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-19 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.348] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-14 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-19 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-16 17:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-19 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.373] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-16 17:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-19 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-17 07:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-19 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.376] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-17 07:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-19 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-18 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-18 21:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.3785] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-18 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-18 21:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-18 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-20 01:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.374] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-18 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-20 01:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-19 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-24 01:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.365] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-19 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-24 01:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-22 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-27 01:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.362] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-22 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-27 01:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-03-17 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-03-21 11:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.366] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-03-17 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-03-21 11:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove drip at end of September
#====================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-05-17 10:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-05-19 01:00:01")

TRE3_20_fix$WC_15cm[TRE3_20_fix$WC_15cm < 0.3775] <- NA
missing <- which(is.na(TRE3_20_fix$WC_15cm))

if(1 %in% missing){
  TRE3_20_fix$WC_15cm[1] <- head(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_15cm[nrow(data)] <- tail(TRE3_20_fix$WC_15cm[!is.na(TRE3_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_15cm[idx] <- (TRE3_20_fix$WC_15cm[r$starts[i]] + TRE3_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-05-17 10:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-05-19 01:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#30 cm 
########################################################################################

#Remove glitch in September
#==========================================================================================
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-09-21 18:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-10-03 00:00:01")

TRE3_20_fix$WC_30cm[TRE3_20_fix$WC_30cm > 0.2346] <- NA
missing <- which(is.na(TRE3_20_fix$WC_30cm))

if(1 %in% missing){
  TRE3_20_fix$WC_30cm[1] <- head(TRE3_20_fix$WC_30cm[!is.na(TRE3_20_fix$WC_30cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_30cm[nrow(data)] <- tail(TRE3_20_fix$WC_30cm[!is.na(TRE3_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_30cm[idx] <- (TRE3_20_fix$WC_30cm[r$starts[i]] + TRE3_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-09-21 18:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-10-03 00:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#100 cm 
################################################################
TRE3_20_fix <- filter(TRE3_20, Date_time > "2020-09-07 18:00:01")
TRE3_20_fix <- filter(TRE3_20_fix, Date_time < "2020-09-15 00:00:01")

TRE3_20_fix$WC_100cm[TRE3_20_fix$WC_100cm > 0.295] <- NA
missing <- which(is.na(TRE3_20_fix$WC_100cm))

if(1 %in% missing){
  TRE3_20_fix$WC_100cm[1] <- head(TRE3_20_fix$WC_100cm[!is.na(TRE3_20_fix$WC_100cm)],1)
}
if(nrow(TRE3_20_fix) %in% missing){
  TRE3_20_fix$WC_100cm[nrow(data)] <- tail(TRE3_20_fix$WC_100cm[!is.na(TRE3_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_20_fix$WC_100cm[idx] <- (TRE3_20_fix$WC_100cm[r$starts[i]] + TRE3_20_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_20_early <- filter(TRE3_20, Date_time < "2020-09-07 18:00:01")
TRE3_20_late <- filter(TRE3_20, Date_time > "2020-09-15 00:00:01")
TRE3_20 <- bind_rows(TRE3_20_early, TRE3_20_late, TRE3_20_fix)

#Remove the missing dates
#=========================================================================
#Replace missing dates with NAs - 01/01 to 09/02
insertDF <- as.data.frame(matrix(data = NA, nrow = 245, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-01-01"), as.Date("2020-09-01"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

TRE3_20 <- insertRows(TRE3_20, c(1:244), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(TRE3_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#TRE3 2021
##################################################################################################
TRE3_21 <- subset(TRE3, Year == '2021')

#Plotting 
TRE3_21$WC_15cm <- as.numeric(TRE3_21$WC_15cm)
TRE3_21$WC_30cm <- as.numeric(TRE3_21$WC_30cm)
TRE3_21$WC_100cm <- as.numeric(TRE3_21$WC_100cm)
TRE3_21$Date_time<- mdy_hms(TRE3_21$Date_time)

#100 cm 
###############################################################

TRE3_21$WC_100cm[TRE3_21$WC_100cm < 0.25] <- NA
missing <- which(is.na(TRE3_19$WC_100cm))

if(1 %in% missing){
  TRE3_21$WC_100cm[1] <- head(TRE3_21$WC_100cm[!is.na(TRE3_21$WC_100cm)],1)
}
if(nrow(TRE3_21) %in% missing){
  TRE3_21$WC_100cm[nrow(data)] <- tail(TRE3_21$WC_100cm[!is.na(TRE3_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21$WC_100cm[idx] <- (TRE3_21$WC_100cm[r$starts[i]] + TRE3_21$WC_100cm[r$ends[i]])/2
}


#Subset to remove drips
#===========================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-01-17 18:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-01-23 00:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm > 0.4047] <- NA
missing <- which(is.na(TRE3_21_fix$WC_100cm))

if(1 %in% missing){
  TRE3_21_fix$WC_100cm[1] <- head(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}
if(nrow(TRE3_21_fix) %in% missing){
  TRE3_21_fix$WC_100cm[nrow(data)] <- tail(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21_fix$WC_100cm[idx] <- (TRE3_21_fix$WC_100cm[r$starts[i]] + TRE3_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-01-17 18:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-01-23 00:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset and remove drips
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-01-23 18:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-01-26 00:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm > 0.3955] <- NA
missing <- which(is.na(TRE3_21_fix$WC_100cm))

if(1 %in% missing){
  TRE3_21_fix$WC_100cm[1] <- head(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}
if(nrow(TRE3_21_fix) %in% missing){
  TRE3_21_fix$WC_100cm[nrow(data)] <- tail(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21_fix$WC_100cm[idx] <- (TRE3_21_fix$WC_100cm[r$starts[i]] + TRE3_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-01-23 18:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-01-26 00:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset and remove drips
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-02-22 18:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-02-25 00:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm > 0.414] <- NA
missing <- which(is.na(TRE3_21_fix$WC_100cm))

if(1 %in% missing){
  TRE3_21_fix$WC_100cm[1] <- head(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}
if(nrow(TRE3_21_fix) %in% missing){
  TRE3_21_fix$WC_100cm[nrow(data)] <- tail(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21_fix$WC_100cm[idx] <- (TRE3_21_fix$WC_100cm[r$starts[i]] + TRE3_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-02-22 18:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-02-25 00:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset and remove drips
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-02-23 10:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-02-24 20:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm > 0.412] <- NA
missing <- which(is.na(TRE3_21_fix$WC_100cm))

if(1 %in% missing){
  TRE3_21_fix$WC_100cm[1] <- head(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}
if(nrow(TRE3_21_fix) %in% missing){
  TRE3_21_fix$WC_100cm[nrow(data)] <- tail(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21_fix$WC_100cm[idx] <- (TRE3_21_fix$WC_100cm[r$starts[i]] + TRE3_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-02-23 10:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-02-24 20:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset and remove drips
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-02-25 10:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-02-28 20:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm > 0.41] <- NA
missing <- which(is.na(TRE3_21_fix$WC_100cm))

if(1 %in% missing){
  TRE3_21_fix$WC_100cm[1] <- head(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}
if(nrow(TRE3_21_fix) %in% missing){
  TRE3_21_fix$WC_100cm[nrow(data)] <- tail(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21_fix$WC_100cm[idx] <- (TRE3_21_fix$WC_100cm[r$starts[i]] + TRE3_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-02-25 10:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-02-28 20:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset and remove drips
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-02-28 10:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-03-08 20:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm > 0.46] <- NA
missing <- which(is.na(TRE3_21_fix$WC_100cm))

if(1 %in% missing){
  TRE3_21_fix$WC_100cm[1] <- head(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}
if(nrow(TRE3_21_fix) %in% missing){
  TRE3_21_fix$WC_100cm[nrow(data)] <- tail(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21_fix$WC_100cm[idx] <- (TRE3_21_fix$WC_100cm[r$starts[i]] + TRE3_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-02-28 10:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-03-08 20:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset and remove drips
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-03-03 22:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-03-04 10:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm < 0.405 | TRE3_21_fix$WC_100cm > 0.425] <- NA
missing <- which(is.na(TRE3_21_fix$WC_100cm))

if(1 %in% missing){
  TRE3_21_fix$WC_100cm[1] <- head(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}
if(nrow(TRE3_21_fix) %in% missing){
  TRE3_21_fix$WC_100cm[nrow(data)] <- tail(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21_fix$WC_100cm[idx] <- (TRE3_21_fix$WC_100cm[r$starts[i]] + TRE3_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-03-03 22:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-03-04 10:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset and remove drips
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-03-03 24:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-03-05 10:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm < 0.41 | TRE3_21_fix$WC_100cm > 0.445] <- NA
missing <- which(is.na(TRE3_21_fix$WC_100cm))

if(1 %in% missing){
  TRE3_21_fix$WC_100cm[1] <- head(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}
if(nrow(TRE3_21_fix) %in% missing){
  TRE3_21_fix$WC_100cm[nrow(data)] <- tail(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21_fix$WC_100cm[idx] <- (TRE3_21_fix$WC_100cm[r$starts[i]] + TRE3_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-03-03 24:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-03-05 10:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset and remove drips
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-02-24 00:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-03-01 10:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm > 0.412] <- NA
missing <- which(is.na(TRE3_21_fix$WC_100cm))

if(1 %in% missing){
  TRE3_21_fix$WC_100cm[1] <- head(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}
if(nrow(TRE3_21_fix) %in% missing){
  TRE3_21_fix$WC_100cm[nrow(data)] <- tail(TRE3_21_fix$WC_100cm[!is.na(TRE3_21_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(TRE3_21_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  TRE3_21_fix$WC_100cm[idx] <- (TRE3_21_fix$WC_100cm[r$starts[i]] + TRE3_21_fix$WC_100cm[r$ends[i]])/2
}

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-02-24 00:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-03-01 10:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset and remove drips
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-03-16 00:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-04-01 10:00:01")

TRE3_21_fix$WC_100cm[TRE3_21_fix$WC_100cm < 0.45] <- NA

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-03-16 00:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-04-01 10:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Calibrate
#======================================================================================
TRE3_21_fix <- filter(TRE3_21, Date_time > "2021-04-03 00:00:01")
TRE3_21_fix <- filter(TRE3_21_fix, Date_time < "2021-12-01 10:00:01")

Soil <- ggplot(data = subset(TRE3_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Above 0.4 and then goes below 0.3204, so maybe drop it a little more too 

TRE3_21_fix$WC_100cm <- TRE3_21_fix$WC_100cm + 0.0796

#Recombine
TRE3_21_early <- filter(TRE3_21, Date_time < "2021-04-03 00:00:01")
TRE3_21_late <- filter(TRE3_21, Date_time > "2021-12-01 10:00:01")
TRE3_21 <- bind_rows(TRE3_21_early, TRE3_21_late, TRE3_21_fix)

#Subset 100 cm to get weird of the plateau 
#================================================================
subset_21 <- TRE3_21[c(10760:12598), ]
subset_21$WC_100cm <- NA

#Remerge 
TRE_21_early <- TRE3_21[c(1:10759), ]
TRE_21_late <- TRE3_21[c(12599:31181), ]
TRE3_21 <- bind_rows(TRE_21_early, subset_21, TRE_21_late)

#Plot again 
Soil <- ggplot(data = subset(TRE3_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge together data frames and plot 
#Combine all of the WIL1 dataframes/years into 1 dataframe 

#Merge 2018 and 2019 
TRE3_clean <- merge(TRE3_17, TRE3_18, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
TRE3_clean <- merge(TRE3_clean, TRE3_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
TRE3_clean <- merge(TRE3_clean, TRE3_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge with 2021
TRE3_clean <- merge(TRE3_clean, TRE3_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

TRE3_clean <- select(TRE3_clean, Date_time, WC_15cm, WC_30cm, WC_100cm)
#Graph
#===================================================================================

Soil <- ggplot(data = subset(TRE3_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date")) 
Soil

png("TRE3_clean", width = 4500, height = 2500)

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
write.csv(TRE3_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/TRE_new/TRE3_clean.csv" ) #this writes a csv file and sends it to the working folder



