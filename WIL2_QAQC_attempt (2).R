#Created by: Elise Miller
#Date started: 10/25/2022
#Date last edited: 02/06/2023
#Description: QA/QC WIL 2

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
library(padr)
library(berryFunctions)

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL", 
                        pattern=glob2rx("W2M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

data19_21  <- data19_21[c(1:6)]
#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
WIL2_2019_2021 <- data19_21 %>%
  rename_all(funs(c("Date", "Time", "PAR", "WC_15cm", "WC_30cm","WC_100cm", "Date_time"))) %>% #this renames all of the columns
  select(Date_time, PAR, WC_15cm, WC_30cm, WC_100cm) #this keeps only the columns you will use


#2017-2019 FILES
#========================================================================================================================
#If wanting to merge the files, needed to manually delete the extra column in W1M190111 and 
#extra row at the top, and needed to manually delete the extra columns in W1M171228, W1M180201, and W1M180302

#Set the data path 
data_path <- "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL"
new_col_name <- c("Date_time", "PAR", "WC_15cm", "WC_30cm", "WC_100cm")

#Call in all the files in this folder with the W2M1 pattern, which excludes the datasheets from 2019-2020
files <- dir(data_path, pattern=glob2rx("W2M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
WIL2_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
WIL2 <- rbind(WIL2_2017_2019, WIL2_2019_2021)

#Write the csv
write.csv(WIL2,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL2.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(WIL2)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
WIL2$Date <- mdy_hms(WIL2$Date_time)

#Put year into a separate column 
WIL2 <- separate(WIL2, Date, c("Year"))

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
WIL2$Date <- mdy_hms(WIL2$Date_time)

#Put year into a separate column 
WIL2 <- separate(WIL2, Date, c("Year"))

#WIL2 2017
##################################################################################################
WIL2_17 <- subset(WIL2, Year == '2017')

#Plotting 
WIL2_17$WC_15cm <- as.numeric(WIL2_17$WC_15cm)
WIL2_17$WC_30cm <- as.numeric(WIL2_17$WC_30cm)
WIL2_17$WC_100cm <- as.numeric(WIL2_17$WC_100cm)
WIL2_17$Date_time<- mdy_hms(WIL2_17$Date_time)

Soil <- ggplot(data = subset(WIL2_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL2 2018
##################################################################################################
WIL2_18 <- subset(WIL2, Year == '2018')

#Plotting 
WIL2_18$WC_15cm <- as.numeric(WIL2_18$WC_15cm)
WIL2_18$WC_30cm <- as.numeric(WIL2_18$WC_30cm)
WIL2_18$WC_100cm <- as.numeric(WIL2_18$WC_100cm)
WIL2_18$Date_time<- mdy_hms(WIL2_18$Date_time)

#Remove negative values that are less than a day 
#===============================================================
#15 cm
WIL2_18$WC_15cm[WIL2_18$WC_15cm < 0.27] <- NA
missing <- which(is.na(WIL2_18$WC_15cm))

if(1 %in% missing){
  WIL2_18$WC_15cm[1] <- head(WIL2_18$WC_15cm[!is.na(WIL2_18$WC_15cm)],1)
}
if(nrow(WIL2_18) %in% missing){
  WIL2_18$WC_15cm[nrow(data)] <- tail(WIL2_18$WC_15cm[!is.na(WIL2_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18$WC_15cm[idx] <- (WIL2_18$WC_15cm[r$starts[i]] + WIL2_18$WC_15cm[r$ends[i]])/2
}

#Filtering specific dates for 15 cm to deal with the drips 

#June to July section
WIL2_18_fix <- filter(WIL2_18, Date_time < "2018-06-26 00:00:01")

#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.308] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time > "2018-06-26 00:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix)

#Do mid July section 
WIL2_18_fix <- filter(WIL2_18, Date_time < "2018-07-15 00:00:01")


#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.297] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time > "2018-07-15 00:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix)

#Do later July section
WIL2_18_fix <- filter(WIL2_18, Date_time < "2018-07-25 00:00:01")


#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.292] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time > "2018-07-25 00:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix)


#Do later August secton 
#===========================================================
WIL2_18_fix <- filter(WIL2_18, Date_time < "2018-08-15 00:00:01")


#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.288] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time > "2018-08-15 00:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix)

#Do later August secton 
#===========================================================
WIL2_18_fix <- filter(WIL2_18, Date_time < "2018-08-31 00:00:01")

#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.287] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time > "2018-08-31 00:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix)


#Do later August secton 
#===========================================================
WIL2_18_fix <- filter(WIL2_18, Date_time < "2018-11-01 00:00:01")


#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.274] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time > "2018-11-01 00:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix)

#Do later August secton 
#===========================================================
WIL2_18_fix <- filter(WIL2_18, Date_time < "2018-11-15 00:00:01")


#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.277] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time > "2018-11-15 00:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix)



#Do later August secton 
#===========================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-12-01 00:00:01")


#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.33] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-12-01 00:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix)


#Do later August secton 
#===========================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-12-01 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-12-02 12:00:01")

#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.348] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-12-01 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-12-02 12:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Do later August secton 
#===========================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-12-10 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-12-11 12:00:01")

#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.346] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-12-10 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-12-02 12:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)



#Do later August secton 
#===========================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-12-18 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-12-25 12:00:01")

#15 cm
WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.352] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}


#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-12-18 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-12-25 12:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)


#Do later December section
#===========================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-11-25 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-11-28 12:00:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.33] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-11-25 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-11-28 12:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)



#Try to fix WIL 18 in December
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-11-25 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-11-30 12:00:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.33] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-11-25 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-11-30 12:00:01")

WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)


#Fix end of December
#=============================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-12-25 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-12-30 12:00:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.343] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-12-25 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-12-30 12:00:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-05-25 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-06-03 20:00:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.3237] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-05-25 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-06-03 20:00:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-06-13 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-06-19 20:00:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.3114] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-06-13 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-06-19 20:00:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-06-19 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-06-27 03:00:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.3075] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-06-19 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-06-27 03:00:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Get rid of glitch 
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-08-20 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-08-31 15:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm > 0] <- NA

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-08-20 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-08-31 15:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-08-31 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-09-03 15:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.2835] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-08-31 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-09-03 15:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-09-03 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-09-09 15:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.2826] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-09-03 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-09-09 15:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-09-09 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-09-13 15:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.282] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-09-09 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-09-13 15:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-09-13 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-09-22 15:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.28] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-09-13 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-09-22 15:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-09-22 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-09-27 15:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.2785] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-09-22 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-09-27 15:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-09-27 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-10-05 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.278] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-09-27 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-10-05 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Get rid of sustained glitch
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-09-29 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-10-04 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm == 0.278] <- NA

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-09-29 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-10-04 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-10-04 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-10-05 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.2785] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-10-04 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-10-05 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-10-05 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-10-07 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.27725] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-10-05 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-10-07 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-10-09 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-10-14 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.2775] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-10-09 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-10-14 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-10-14 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-10-24 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.2785] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-10-14 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-10-24 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-11-08 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-11-15 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.28] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-11-08 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-11-15 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-11-15 00:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-11-22 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.276] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-11-15 00:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-11-22 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-11-23 04:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-11-30 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.33] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-11-23 04:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-11-30 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-11-23 04:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-11-24 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.345] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-11-23 04:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-11-24 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-11-24 04:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-11-26 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.34] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-11-24 04:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-11-26 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-11-29 04:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-11-30 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.349] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-11-29 04:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-11-30 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-12-01 04:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-12-01 22:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.3516] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-12-01 04:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-12-01 22:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-12-03 04:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-12-05 23:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.341] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-12-03 04:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-12-05 23:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-12-16 04:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-12-17 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.358] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-12-16 04:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-12-17 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Subset and remove drips
#====================================================================
WIL2_18_fix <- filter(WIL2_18, Date_time > "2018-12-17 04:00:01")
WIL2_18_fix <- filter(WIL2_18_fix, Date_time < "2018-12-20 00:50:01")

WIL2_18_fix$WC_15cm[WIL2_18_fix$WC_15cm < 0.355] <- NA
missing <- which(is.na(WIL2_18_fix$WC_15cm))

if(1 %in% missing){
  WIL2_18_fix$WC_15cm[1] <- head(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}
if(nrow(WIL2_18_fix) %in% missing){
  WIL2_18_fix$WC_15cm[nrow(data)] <- tail(WIL2_18_fix$WC_15cm[!is.na(WIL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_18_fix$WC_15cm[idx] <- (WIL2_18_fix$WC_15cm[r$starts[i]] + WIL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_18_later <- filter(WIL2_18, Date_time < "2018-12-17 04:00:01")
WIL2_18_end <- filter(WIL2_18, Date_time > "2018-12-20 00:50:01")
WIL2_18 <- bind_rows(WIL2_18_later, WIL2_18_fix, WIL2_18_end)

#Plot again 
Soil <- ggplot(data = subset(WIL2_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm), color = "deepskyblue3") +
  geom_line(aes(y = WC_100cm), color = "navyblue") +
  geom_line(aes(y = WC_15cm), color = "lightblue") +
  ylab(expression(paste("30 cm Water Content"))) + 
  xlab(expression("Date"))  
Soil 

#WIL 2 2019
##################################################
WIL2_19 <- subset(WIL2, Year == '2019')

#Plotting 
WIL2_19$WC_15cm <- as.numeric(WIL2_19$WC_15cm)
WIL2_19$WC_30cm <- as.numeric(WIL2_19$WC_30cm)
WIL2_19$WC_100cm <- as.numeric(WIL2_19$WC_100cm)
WIL2_19$Date_time<- mdy_hms(WIL2_19$Date_time)

#15 cm
####################################################################################

#Remove anything below 0.25 
#================================================================================
WIL2_19$WC_15cm[WIL2_19$WC_15cm < 0.25] <- NA
missing <- which(is.na(WIL2_19$WC_15cm))

if(1 %in% missing){
  WIL2_19$WC_15cm[1] <- head(WIL2_19$WC_15cm[!is.na(WIL2_19$WC_15cm)],1)
}
if(nrow(WIL2_19) %in% missing){
  WIL2_19$WC_15cm[nrow(data)] <- tail(WIL2_19$WC_15cm[!is.na(WIL2_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_19$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_19$WC_15cm[idx] <- (WIL2_19$WC_15cm[r$starts[i]] + WIL2_19$WC_15cm[r$ends[i]])/2
}

#Subset early January to fix 15 cm drops
#===========================================================
WIL2_19_fix <- filter(WIL2_19, Date_time < "2019-01-05 01:00:01")

WIL2_19_fix$WC_15cm[WIL2_19_fix$WC_15cm < 0.34] <- NA
missing <- which(is.na(WIL2_19_fix$WC_15cm))

if(1 %in% missing){
  WIL2_19_fix$WC_15cm[1] <- head(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}
if(nrow(WIL2_19_fix) %in% missing){
  WIL2_19_fix$WC_15cm[nrow(data)] <- tail(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_19_fix$WC_15cm[idx] <- (WIL2_19_fix$WC_15cm[r$starts[i]] + WIL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_19_later <- filter(WIL2_19, Date_time > "2019-01-05 01:00:01")
WIL2_19 <- bind_rows(WIL2_19_later, WIL2_19_fix)

#Subset mid January again 
#===========================================================
WIL2_19_fix <- filter(WIL2_19, Date_time > "2019-01-05 7:00:01")
WIL2_19_fix <- filter(WIL2_19_fix, Date_time < "2019-01-05 17:00:01")

WIL2_19_fix$WC_15cm[WIL2_19_fix$WC_15cm < 0.35] <- NA
missing <- which(is.na(WIL2_19_fix$WC_15cm))

if(1 %in% missing){
  WIL2_19_fix$WC_15cm[1] <- head(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}
if(nrow(WIL2_19_fix) %in% missing){
  WIL2_19_fix$WC_15cm[nrow(data)] <- tail(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_19_fix$WC_15cm[idx] <- (WIL2_19_fix$WC_15cm[r$starts[i]] + WIL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_19_later <- filter(WIL2_19, Date_time > "2019-01-05 7:00:01")
WIL2_19_end <- filter(WIL2_19, Date_time > "2019-01-05 17:00:01")
WIL2_19 <- bind_rows(WIL2_19_later, WIL2_19_fix, WIL2_19_end)

#Subset mid January again 
#===========================================================
WIL2_19_fix <- filter(WIL2_19, Date_time > "2019-01-07 1:00:01")
WIL2_19_fix <- filter(WIL2_19_fix, Date_time < "2019-01-08 12:00:01")

WIL2_19_fix$WC_15cm[WIL2_19_fix$WC_15cm < 0.35] <- NA
missing <- which(is.na(WIL2_19_fix$WC_15cm))

if(1 %in% missing){
  WIL2_19_fix$WC_15cm[1] <- head(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}
if(nrow(WIL2_19_fix) %in% missing){
  WIL2_19_fix$WC_15cm[nrow(data)] <- tail(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_19_fix$WC_15cm[idx] <- (WIL2_19_fix$WC_15cm[r$starts[i]] + WIL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_19_later <- filter(WIL2_19, Date_time > "2019-01-07 1:00:01")
WIL2_19_end <- filter(WIL2_19, Date_time > "2019-01-08 12:00:01")
WIL2_19 <- bind_rows(WIL2_19_later, WIL2_19_fix, WIL2_19_end)

#Subset mid January again 
#===========================================================
WIL2_19_fix <- filter(WIL2_19, Date_time > "2019-01-07 05:00:01")
WIL2_19_fix <- filter(WIL2_19_fix, Date_time < "2019-01-08 12:00:01")

WIL2_19_fix$WC_15cm[WIL2_19_fix$WC_15cm < 0.352] <- NA
missing <- which(is.na(WIL2_19_fix$WC_15cm))

if(1 %in% missing){
  WIL2_19_fix$WC_15cm[1] <- head(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}
if(nrow(WIL2_19_fix) %in% missing){
  WIL2_19_fix$WC_15cm[nrow(data)] <- tail(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_19_fix$WC_15cm[idx] <- (WIL2_19_fix$WC_15cm[r$starts[i]] + WIL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_19_later <- filter(WIL2_19, Date_time > "2019-01-07 05:00:01")
WIL2_19_end <- filter(WIL2_19, Date_time > "2019-01-08 12:00:01")
WIL2_19 <- bind_rows(WIL2_19_later, WIL2_19_fix, WIL2_19_end)


#Subset mid January again 
#===========================================================
WIL2_19_fix <- filter(WIL2_19, Date_time > "2019-01-08 05:00:01")
WIL2_19_fix <- filter(WIL2_19_fix, Date_time < "2019-01-11 11:00:01")


WIL2_19_fix$WC_15cm[WIL2_19_fix$WC_15cm < 0.325] <- NA
missing <- which(is.na(WIL2_19_fix$WC_15cm))

if(1 %in% missing){
  WIL2_19_fix$WC_15cm[1] <- head(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}
if(nrow(WIL2_19_fix) %in% missing){
  WIL2_19_fix$WC_15cm[nrow(data)] <- tail(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_19_fix$WC_15cm[idx] <- (WIL2_19_fix$WC_15cm[r$starts[i]] + WIL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_19_later <- filter(WIL2_19, Date_time > "2019-01-08 05:00:01")
WIL2_19_end <- filter(WIL2_19, Date_time > "2019-01-11 01:00:01")
WIL2_19 <- bind_rows(WIL2_19_later, WIL2_19_fix, WIL2_19_end)

#Subset mid January again 
#===========================================================
WIL2_19_fix <- filter(WIL2_19, Date_time > "2019-01-07 05:00:01")
WIL2_19_fix <- filter(WIL2_19_fix, Date_time < "2019-01-11 06:00:01")

WIL2_19_fix$WC_15cm[WIL2_19_fix$WC_15cm < 0.352] <- NA
missing <- which(is.na(WIL2_19_fix$WC_15cm))

if(1 %in% missing){
  WIL2_19_fix$WC_15cm[1] <- head(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}
if(nrow(WIL2_19_fix) %in% missing){
  WIL2_19_fix$WC_15cm[nrow(data)] <- tail(WIL2_19_fix$WC_15cm[!is.na(WIL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_19_fix$WC_15cm[idx] <- (WIL2_19_fix$WC_15cm[r$starts[i]] + WIL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_19_later <- filter(WIL2_19, Date_time < "2019-01-08 05:00:01")
WIL2_19_end <- filter(WIL2_19, Date_time > "2019-01-11 11:00:01")
WIL2_19 <- bind_rows(WIL2_19_later, WIL2_19_fix, WIL2_19_end)

#Plot again 
Soil <- ggplot(data = subset(WIL2_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm), color = "deepskyblue3") +
  geom_line(aes(y = WC_100cm), color = "navyblue") +
  geom_line(aes(y = WC_15cm), color = "lightblue") +
  ylab(expression(paste("30 cm Water Content"))) + 
  xlab(expression("Date"))  
Soil 

#WIL2 2020
###############################################################################
WIL2_20 <- subset(WIL2, Year == '2020')

#Plotting 
WIL2_20$WC_15cm <- as.numeric(WIL2_20$WC_15cm)
WIL2_20$WC_30cm <- as.numeric(WIL2_20$WC_30cm)
WIL2_20$WC_100cm <- as.numeric(WIL2_20$WC_100cm)
WIL2_20$Date_time<- mdy_hms(WIL2_20$Date_time)

#15 cm 
######################################################################################

#Remove anything below 0.23
#====================================================================================
WIL2_20$WC_15cm[WIL2_20$WC_15cm < 0.23] <- NA
missing <- which(is.na(WIL2_20$WC_15cm))

if(1 %in% missing){
  WIL2_20$WC_15cm[1] <- head(WIL2_20$WC_15cm[!is.na(WIL2_20)],1)
}
if(nrow(WIL2_20) %in% missing){
  WIL2_20$WC_15cm[nrow(data)] <- tail(WIL2_20$WC_15cm[!is.na(WIL2_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20$WC_15cm[idx] <- (WIL2_20$WC_15cm[r$starts[i]] + WIL2_20$WC_15cm[r$ends[i]])/2
}

#Fix 15 cm drips in mid-March through mid-May 
#========================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-03-07 05:00:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-05-01 06:00:01")

WIL2_20_fix$WC_15cm[WIL2_20_fix$WC_15cm < 0.314] <- NA
missing <- which(is.na(WIL2_20_fix$WC_15cm))

if(1 %in% missing){
  WIL2_20_fix$WC_15cm[1] <- head(WIL2_20_fix$WC_15cm[!is.na(WIL2_20_fix$WC_15cm)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_15cm[nrow(data)] <- tail(WIL2_20_fix$WC_15cm[!is.na(WIL2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_15cm[idx] <- (WIL2_20_fix$WC_15cm[r$starts[i]] + WIL2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-03-07 05:00:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-05-01 06:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Fix the drips some more using the increase
WIL2_20_fix <- WIL2_20_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================


#Make increase column not a percent 
WIL2_20_fix <- transform(WIL2_20_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

WIL2_20_fix <- transform(WIL2_20_fix, WC_15cm=ifelse(incr < -0.01, 
                                                     as.numeric(stats::filter(WC_15cm, rep(1/144, 144), sides=2)), 
                                                     WC_15cm))
#Add them back into the full dataset 
WIL2_20_fix <- select(WIL2_20_fix, "Date_time", "PAR", "WC_100cm", "WC_15cm", "WC_30cm")

WIL2_20_later <- filter(WIL2_20, Date_time < "2020-03-07 05:00:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-05-01 06:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips again
#======================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-03-15 05:00:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-04-15 06:00:01")

WIL2_20_fix$WC_15cm[WIL2_20_fix$WC_15cm < 0.329] <- NA
missing <- which(is.na(WIL2_20_fix$WC_15cm))

if(1 %in% missing){
  WIL2_20_fix$WC_15cm[1] <- head(WIL2_20_fix$WC_15cm[!is.na(WIL2_20_fix$WC_15cm)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_15cm[nrow(data)] <- tail(WIL2_20_fix$WC_15cm[!is.na(WIL2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_15cm[idx] <- (WIL2_20_fix$WC_15cm[r$starts[i]] + WIL2_20_fix$WC_15cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time > "2020-04-15 06:00:01")
WIL2_20_end <- filter(WIL2_20, Date_time < "2020-03-15 05:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Remove glitch 
#===============================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-18 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-30 06:00:01")

WIL2_20_fix$WC_15cm[WIL2_20_fix$WC_15cm > 0] <- NA

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-18 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-30 06:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#30 cm 
################################################################################

#Remove anything below 0.25
#==============================================================================
WIL2_20$WC_30cm[WIL2_20$WC_30cm < 0.25] <- NA
missing <- which(is.na(WIL2_20$WC_30cm))

if(1 %in% missing){
  WIL2_20$WC_30cm[1] <- head(WIL2_20$WC_30cm[!is.na(WIL2_20)],1)
}
if(nrow(WIL2_20) %in% missing){
  WIL2_20$WC_30cm[nrow(data)] <- tail(WIL2_20$WC_30cm[!is.na(WIL2_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20$WC_30cm[idx] <- (WIL2_20$WC_30cm[r$starts[i]] + WIL2_20$WC_30cm[r$ends[i]])/2
}

#Remove glitch starting in September
#=================================================================================
WIL2_20$WC_30cm[WIL2_20$WC_30cm == 0.29135] <- NA

#Remove glitch in late June
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-06-24 13:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-06-30 06:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm > 0] <- NA

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-06-24 13:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-06-30 06:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Remove glitch in late June
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-05-24 13:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-06-11 00:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.3064] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-05-24 13:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-06-11 00:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Remove glitch in late June
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-05-24 13:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-06-07 00:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.309] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-05-24 13:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-06-07 00:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Remove glitch in late June
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-06-07 13:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-06-17 00:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.302] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-06-07 13:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-06-17 00:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Remove glitch in late June
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-06-17 13:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-06-27 00:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.295] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-06-17 13:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-06-27 00:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)


#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-16 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-27 00:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.2915] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-16 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-27 00:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-16 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-23 00:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.294] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-16 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-23 00:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-16 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-20 00:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.296] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-16 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-20 00:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-16 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-19 00:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.297] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-16 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-19 00:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Remove glitch in late June
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-06-24 13:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-06-30 06:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm > 0] <- NA

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-06-24 13:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-06-30 06:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-16 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-18 10:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.2975] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-16 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-18 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-16 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-17 03:00:01")


WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.2985] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-16 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-17 03:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-01 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-03 03:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.289 | WIL2_20_fix$WC_30cm > 0.2915] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-01 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-03 03:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-03 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-08 03:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.2865] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-03 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-08 03:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-10 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-12 13:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.2838] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-10 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-12 13:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-12 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-18 13:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.282] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-12 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-18 13:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-12 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-14 13:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.2835] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-12 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-14 13:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-14 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-24 13:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.28] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-14 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-24 13:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-27 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-29 13:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.278] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-27 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-29 13:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-09-01 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-09-02 13:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.278] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-09-01 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-09-02 13:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove drips
#=================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-31 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-03 13:00:01")

WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm < 0.285] <- NA
missing <- which(is.na(WIL2_20_fix$WC_30cm))

if(1 %in% missing){
  WIL2_20_fix$WC_30cm[1] <- head(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_30cm[nrow(data)] <- tail(WIL2_20_fix$WC_30cm[!is.na(WIL2_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_30cm[idx] <- (WIL2_20_fix$WC_30cm[r$starts[i]] + WIL2_20_fix$WC_30cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-31 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-03 13:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#100 cm
#####################################################################################

#Remove anything below 0
#=====================================================================================
WIL2_20$WC_100cm[WIL2_20$WC_100cm < 0] <- NA
missing <- which(is.na(WIL2_20$WC_100cm))

if(1 %in% missing){
  WIL2_20$WC_100cm[1] <- head(WIL2_20$WC_100cm[!is.na(WIL2_20)],1)
}
if(nrow(WIL2_20) %in% missing){
  WIL2_20$WC_100cm[nrow(data)] <- tail(WIL2_20$WC_100cm[!is.na(WIL2_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20$WC_100cm[idx] <- (WIL2_20$WC_100cm[r$starts[i]] + WIL2_20$WC_100cm[r$ends[i]])/2
}

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-07 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-08 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm < 0.283] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-07 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-08 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)


#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-10 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-11 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm < 0.282] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-10 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-11 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-11 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-13 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm < 0.279] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-11 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-13 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-12 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-13 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.282] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-12 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-13 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-13 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-16 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.2807 | WIL2_20_fix$WC_100cm < 0.278] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-13 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-16 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-15 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-16 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.2795] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-15 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-16 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-16 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-21 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.281] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-16 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-21 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)


#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-21 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-27 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.2775 | WIL2_20_fix$WC_100cm < 0.2715] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-21 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-27 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-23 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-26 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.2745] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-23 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-26 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-02 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-04 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.271 | WIL2_20_fix$WC_100cm < 0.2679] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-02 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-04 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-02 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-03 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm < 0.269] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-02 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-03 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-05 00:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-10 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.268] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-05 00:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-10 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-06 10:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-10 10:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.26625 | WIL2_20_fix$WC_100cm < 0.264] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-06 10:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-10 10:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-10 10:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-13 07:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.265 | WIL2_20_fix$WC_100cm < 0.2613] <- NA

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-10 10:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-13 07:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-13 10:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-18 07:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.27 | WIL2_20_fix$WC_100cm < 0.261] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-13 10:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-18 07:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-20 10:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-22 07:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.258] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-20 10:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-22 07:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-22 10:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-26 07:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm < 0.253] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-22 10:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-26 07:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Subset and remove glitches
#================================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-08-26 10:20:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-08-30 07:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm < 0.2523 | WIL2_20_fix$WC_100cm > 0.255] <- NA
missing <- which(is.na(WIL2_20_fix$WC_100cm))

if(1 %in% missing){
  WIL2_20_fix$WC_100cm[1] <- head(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix)],1)
}
if(nrow(WIL2_20_fix) %in% missing){
  WIL2_20_fix$WC_100cm[nrow(data)] <- tail(WIL2_20_fix$WC_100cm[!is.na(WIL2_20_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL2_20_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL2_20_fix$WC_100cm[idx] <- (WIL2_20_fix$WC_100cm[r$starts[i]] + WIL2_20_fix$WC_100cm[r$ends[i]])/2
}

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-08-26 10:20:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-08-30 07:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)


#Remove glitch starting in September
#=================================================================================
WIL2_20$WC_100cm[WIL2_20$WC_100cm == 0.24900] <- NA

#Missing dates and glitches
################################################################################

#Remove glitch they all experience at one point
WIL2_20$WC_100cm[WIL2_20$WC_100cm == 0.7186] <- NA
WIL2_20$WC_30cm[WIL2_20$WC_30cm == 0.7186] <- NA
WIL2_20$WC_15cm[WIL2_20$WC_15cm == 0.7186] <- NA

#Remove glitch before missing date in May 
#========================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-05-12 00:00:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-05-16 06:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0.31] <- NA
WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm > 0.305] <- NA
WIL2_20_fix$WC_15cm[WIL2_20_fix$WC_15cm > 0.322] <- NA

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-05-12 00:00:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-05-16 06:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Remove glitch before missing date in July
#========================================================================
WIL2_20_fix <- filter(WIL2_20, Date_time > "2020-07-27 18:00:01")
WIL2_20_fix <- filter(WIL2_20_fix, Date_time < "2020-07-29 06:00:01")

WIL2_20_fix$WC_100cm[WIL2_20_fix$WC_100cm > 0] <- NA
WIL2_20_fix$WC_30cm[WIL2_20_fix$WC_30cm > 0] <- NA

#Add them back into the full dataset 
WIL2_20_later <- filter(WIL2_20, Date_time < "2020-07-27 18:00:01")
WIL2_20_end <- filter(WIL2_20, Date_time > "2020-07-28 06:00:01")
WIL2_20 <- bind_rows(WIL2_20_later, WIL2_20_fix, WIL2_20_end)

#Remove the missing dates
#=========================================================================
#Replace missing dates with NAs - 05/15 to 06/05
insertDF <- as.data.frame(matrix(data = NA, nrow = 20, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-05-16"), as.Date("2020-06-04"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL2_20 <- insertRows(WIL2_20, c(19460:19479), new = insertDF)

#Replace missing dates with NAs - 07/01 to 07/08
insertDF <- as.data.frame(matrix(data = NA, nrow = 6, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-07-02"), as.Date("2020-07-07"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

WIL2_20 <- insertRows(WIL2_20, c(23212:23217), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(WIL2_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm), color = "deepskyblue3") +
  geom_line(aes(y = WC_100cm), color = "navyblue") +
  geom_line(aes(y = WC_15cm), color = "lightblue") +
  ylab(expression(paste("30 cm Water Content"))) + 
  xlab(expression("Date"))  
Soil 

#WIL2 2021
######################################################
WIL2_21 <- subset(WIL2, Year == '2021')

#Plotting 
WIL2_21$WC_15cm <- as.numeric(WIL2_21$WC_15cm)
WIL2_21$WC_30cm <- as.numeric(WIL2_21$WC_30cm)
WIL2_21$WC_100cm <- as.numeric(WIL2_21$WC_100cm)
WIL2_21$Date_time<- mdy_hms(WIL2_21$Date_time)

Soil <- ggplot(data = subset(WIL2_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
WIL2_clean <- merge(WIL2_18, WIL2_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
WIL2_clean <- merge(WIL2_clean, WIL2_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
WIL2_clean <- merge(WIL2_clean, WIL2_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
#WIL2_clean <- merge(WIL2_clean, WIL2_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
#                  all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(WIL2_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("WIL2", width = 4500, height = 2500)

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
write.csv(WIL2_clean, "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL2_clean.csv") #this writes a csv file and sends it to the working folder
