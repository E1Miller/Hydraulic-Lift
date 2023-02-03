#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/02/2023
#Description: QA/QC ZIE 4

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
                        pattern=glob2rx("Copy of Z4M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
ZIE4_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("Copy of Z4M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
ZIE4_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
ZIE4 <- rbind(ZIE4_2017_2019, ZIE4_2019_2021)

#Write the csv
write.csv(ZIE4,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE4.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(ZIE4)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
ZIE4$Date <- mdy_hms(ZIE4$Date_time)

#Put year into a separate column 
ZIE4 <- separate(ZIE4, Date, c("Year"))

#ZIE4 2017
##################################################################################################
ZIE4_17 <- subset(ZIE4, Year == '2017')

#Plotting 
ZIE4_17$WC_15cm <- as.numeric(ZIE4_17$WC_15cm)
ZIE4_17$WC_30cm <- as.numeric(ZIE4_17$WC_30cm)
ZIE4_17$WC_100cm <- as.numeric(ZIE4_17$WC_100cm)
ZIE4_17$Date_time<- mdy_hms(ZIE4_17$Date_time)

Soil <- ggplot(data = subset(ZIE4_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE4 2018
##################################################################################################
ZIE4_18 <- subset(ZIE4, Year == '2018')

#Plotting 
ZIE4_18$WC_15cm <- as.numeric(ZIE4_18$WC_15cm)
ZIE4_18$WC_30cm <- as.numeric(ZIE4_18$WC_30cm)
ZIE4_18$WC_100cm <- as.numeric(ZIE4_18$WC_100cm)
ZIE4_18$Date_time<- mdy_hms(ZIE4_18$Date_time)

#15 cm
###########################################
ZIE4_18$WC_15cm[ZIE4_18$WC_15cm < 0] <- NA
missing <- which(is.na(ZIE4_18$WC_15cm))

if(1 %in% missing){
  ZIE4_18$WC_15cm[1] <- head(ZIE4_18$WC_15cm[!is.na(ZIE4_18$WC_15cm)],1)
}
if(nrow(ZIE4_18) %in% missing){
  ZIE4_18$WC_15cm[nrow(data)] <- tail(ZIE4_18$WC_15cm[!is.na(ZIE4_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18$WC_15cm[idx] <- (ZIE4_18$WC_15cm[r$starts[i]] + ZIE4_18$WC_15cm[r$ends[i]])/2
}

#30 cm 
################################################################
ZIE4_18$WC_30cm[ZIE4_18$WC_30cm < 0] <- NA
missing <- which(is.na(ZIE4_18$WC_30cm))

if(1 %in% missing){
  ZIE4_18$WC_30cm[1] <- head(ZIE4_18$WC_30cm[!is.na(ZIE4_18$WC_30cm)],1)
}
if(nrow(ZIE4_18) %in% missing){
  ZIE4_18$WC_30cm[nrow(data)] <- tail(ZIE4_18$WC_30cm[!is.na(ZIE4_18$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18$WC_30cm[idx] <- (ZIE4_18$WC_30cm[r$starts[i]] + ZIE4_18$WC_30cm[r$ends[i]])/2
}

#100 cm 
################################################################\
ZIE4_18$WC_100cm[ZIE4_18$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE4_18$WC_100cm))

if(1 %in% missing){
  ZIE4_18$WC_100cm[1] <- head(ZIE4_18$WC_100cm[!is.na(ZIE4_18$WC_100cm)],1)
}
if(nrow(ZIE4_18) %in% missing){
  ZIE4_18$WC_100cm[nrow(data)] <- tail(ZIE4_18$WC_100cm[!is.na(ZIE4_18$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18$WC_100cm[idx] <- (ZIE4_18$WC_100cm[r$starts[i]] + ZIE4_18$WC_100cm[r$ends[i]])/2
}

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-01-31 1:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-02-05 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.46] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}
#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-01-31 1:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-02-05 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-02-11 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-02-17 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.393] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}
#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-02-11 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-02-17 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-04-17 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-05-17 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.48] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-04-17 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-05-17 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-05-07 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-05-17 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.339] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-05-07 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-05-17 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-05-10 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-05-17 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.333] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-05-10 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-05-17 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-05-17 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-05-20 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.3250] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-05-17 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-05-20 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-05-21 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-05-31 1:00:01")

Soil <- ggplot(data = subset(ZIE4_18fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil 

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.320] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-05-21 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-05-31 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-05-31 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-06-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.31] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-05-31 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-06-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-06-04 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-06-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.305] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-06-04 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-06-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-06-07 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-06-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.30] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-06-07 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-06-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-06-07 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-06-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.30] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-06-07 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-06-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-06-09 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-06-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.2975] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-06-09 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-06-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-06-12 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-06-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.295] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-06-12 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-06-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-06-16 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-06-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.29] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-06-16 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-06-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-07-18 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-07-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.2657] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-07-18 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-07-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-07-30 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-08-06 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.265] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-07-30 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-08-06 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-07-30 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-08-06 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.265] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-07-30 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-08-06 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-08-06 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-08-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.26] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-08-06 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-08-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-08-20 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-08-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.255] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-08-20 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-08-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-08-30 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-09-30 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.26] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-08-30 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-09-30 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-09-30 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-11-05 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.25] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-09-30 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-11-05 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Fix upwards drips 
#============================================================================
ZIE4_18fix <- filter(ZIE4_18, Date_time > "2018-11-05 00:00:01")
ZIE4_18fix <- filter(ZIE4_18fix, Date_time < "2018-11-22 1:00:01")

ZIE4_18fix$WC_100cm[ZIE4_18fix$WC_100cm > 0.25] <- NA
missing <- which(is.na(ZIE4_18fix$WC_100cm))

if(1 %in% missing){
  ZIE4_18fix$WC_100cm[1] <- head(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}
if(nrow(ZIE4_18fix) %in% missing){
  ZIE4_18fix$WC_100cm[nrow(data)] <- tail(ZIE4_18fix$WC_100cm[!is.na(ZIE4_18fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_18fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_18fix$WC_100cm[idx] <- (ZIE4_18fix$WC_100cm[r$starts[i]] + ZIE4_18fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_18early <- filter(ZIE4_18, Date_time < "2018-11-05 00:00:01")
ZIE4_18late <- filter(ZIE4_18, Date_time > "2018-11-22 1:00:01")

ZIE4_18 <- bind_rows(ZIE4_18late, ZIE4_18early, ZIE4_18fix)

#Replace missing dates with NAs - 06/30 t0 07/19
#=========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 20, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2018-06-30"), as.Date("2018-07-19"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE4_18 <- insertRows(ZIE4_18, c(25700:25719), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(ZIE4_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE4 2019
##################################################################################################
ZIE4_19 <- subset(ZIE4, Year == '2019')

#Plotting 
ZIE4_19$WC_15cm <- as.numeric(ZIE4_19$WC_15cm)
ZIE4_19$WC_30cm <- as.numeric(ZIE4_19$WC_30cm)
ZIE4_19$WC_100cm <- as.numeric(ZIE4_19$WC_100cm)
ZIE4_19$Date_time<- mdy_hms(ZIE4_19$Date_time)

#15 cm
###########################################
ZIE4_19$WC_15cm[ZIE4_19$WC_15cm < 0.088] <- NA
missing <- which(is.na(ZIE4_19$WC_15cm))

if(1 %in% missing){
  ZIE4_19$WC_15cm[1] <- head(ZIE4_19$WC_15cm[!is.na(ZIE4_19$WC_15cm)],1)
}
if(nrow(ZIE4_19) %in% missing){
  ZIE4_19$WC_15cm[nrow(data)] <- tail(ZIE4_19$WC_15cm[!is.na(ZIE4_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19$WC_15cm[idx] <- (ZIE4_19$WC_15cm[r$starts[i]] + ZIE4_19$WC_15cm[r$ends[i]])/2
}

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-03-19 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-04-01 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.285] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-03-19 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-04-01 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-03-25 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-04-01 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.292] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-03-25 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-04-01 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-03-28 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-03-29 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.295] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-03-28 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-03-29 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-04-01 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-04-08 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.289] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-04-01 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-04-08 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-04-15 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-04-20 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.28] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-04-15 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-04-20 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-05-13 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-05-20 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.242] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-05-13 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-05-20 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-06-17 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-06-19 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.225] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-06-17 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-06-19 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-09-14 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-10-19 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.104] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-09-14 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-10-19 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-09-16 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-09-20 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.15] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-09-16 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-09-20 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-11-20 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-12-31 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.11] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-11-20 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-12-31 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in March
#=============================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-04-15 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-04-17 1:00:01")

ZIE4_19fix$WC_15cm[ZIE4_19fix$WC_15cm < 0.2875] <- NA
missing <- which(is.na(ZIE4_19fix$WC_15cm))

if(1 %in% missing){
  ZIE4_19fix$WC_15cm[1] <- head(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_15cm[nrow(data)] <- tail(ZIE4_19fix$WC_15cm[!is.na(ZIE4_19fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_15cm[idx] <- (ZIE4_19fix$WC_15cm[r$starts[i]] + ZIE4_19fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-04-15 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-04-17 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#30 cm 
################################################################
ZIE4_19$WC_30cm[ZIE4_19$WC_30cm < 0.15] <- NA
missing <- which(is.na(ZIE4_19$WC_30cm))

if(1 %in% missing){
  ZIE4_19$WC_30cm[1] <- head(ZIE4_19$WC_30cm[!is.na(ZIE4_19$WC_30cm)],1)
}
if(nrow(ZIE4_19) %in% missing){
  ZIE4_19$WC_30cm[nrow(data)] <- tail(ZIE4_19$WC_30cm[!is.na(ZIE4_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19$WC_30cm[idx] <- (ZIE4_19$WC_30cm[r$starts[i]] + ZIE4_19$WC_30cm[r$ends[i]])/2
}

#Remove glitch from 2019-01-23 22:30:01 until 2019-05-10
#===============================================================
ZIE4_19$WC_30cm[ZIE4_19$WC_30cm == 0.30845] <- NA

#Subset and remove drips in August/September
#================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-08-09 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-08-11 1:00:01")

ZIE4_19fix$WC_30cm[ZIE4_19fix$WC_30cm < 0.2023] <- NA
missing <- which(is.na(ZIE4_19fix$WC_30cm))

if(1 %in% missing){
  ZIE4_19fix$WC_30cm[1] <- head(ZIE4_19fix$WC_30cm[!is.na(ZIE4_19fix$WC_30cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_30cm[nrow(data)] <- tail(ZIE4_19fix$WC_30cm[!is.na(ZIE4_19fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_30cm[idx] <- (ZIE4_19fix$WC_30cm[r$starts[i]] + ZIE4_19fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-08-09 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-08-11 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in August/September
#================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-10-09 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-11-11 1:00:01")

Soil <- ggplot(data = subset(ZIE4_19fix, !is.na(Date_time)), aes(x = Date_time)) +
  geom_line(aes(y = WC_30cm, color = "lightblue"))
Soil

ZIE4_19fix$WC_30cm[ZIE4_19fix$WC_30cm < 0.172] <- NA
missing <- which(is.na(ZIE4_19fix$WC_30cm))

if(1 %in% missing){
  ZIE4_19fix$WC_30cm[1] <- head(ZIE4_19fix$WC_30cm[!is.na(ZIE4_19fix$WC_30cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_30cm[nrow(data)] <- tail(ZIE4_19fix$WC_30cm[!is.na(ZIE4_19fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_30cm[idx] <- (ZIE4_19fix$WC_30cm[r$starts[i]] + ZIE4_19fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-10-09 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-11-11 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#Subset and remove drips in August/September
#================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-10-09 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-10-21 1:00:01")

Soil <- ggplot(data = subset(ZIE4_19fix, !is.na(Date_time)), aes(x = Date_time)) +
  geom_line(aes(y = WC_30cm, color = "lightblue"))
Soil

ZIE4_19fix$WC_30cm[ZIE4_19fix$WC_30cm < 0.175] <- NA
missing <- which(is.na(ZIE4_19fix$WC_30cm))

if(1 %in% missing){
  ZIE4_19fix$WC_30cm[1] <- head(ZIE4_19fix$WC_30cm[!is.na(ZIE4_19fix$WC_30cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_30cm[nrow(data)] <- tail(ZIE4_19fix$WC_30cm[!is.na(ZIE4_19fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_30cm[idx] <- (ZIE4_19fix$WC_30cm[r$starts[i]] + ZIE4_19fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-10-09 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-10-21 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)

#100 cm 
################################################################
ZIE4_19$WC_100cm[ZIE4_19$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE4_19$WC_100cm))

if(1 %in% missing){
  ZIE4_19$WC_100cm[1] <- head(ZIE4_19$WC_100cm[!is.na(ZIE4_19$WC_100cm)],1)
}
if(nrow(ZIE4_19) %in% missing){
  ZIE4_19$WC_100cm[nrow(data)] <- tail(ZIE4_19$WC_100cm[!is.na(ZIE4_19$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19$WC_100cm[idx] <- (ZIE4_19$WC_100cm[r$starts[i]] + ZIE4_19$WC_100cm[r$ends[i]])/2
}

# #Subset and remove drips in August/September
# #================================================================
ZIE4_19fix <- filter(ZIE4_19, Date_time > "2019-11-29 1:00:01")
ZIE4_19fix<- filter(ZIE4_19fix, Date_time < "2019-12-31 1:00:01")

ZIE4_19fix$WC_100cm[ZIE4_19fix$WC_100cm < 0.4395] <- NA
missing <- which(is.na(ZIE4_19fix$WC_100cm))

if(1 %in% missing){
  ZIE4_19fix$WC_100cm[1] <- head(ZIE4_19fix$WC_100cm[!is.na(ZIE4_19fix$WC_100cm)],1)
}
if(nrow(ZIE4_19fix) %in% missing){
  ZIE4_19fix$WC_100cm[nrow(data)] <- tail(ZIE4_19fix$WC_100cm[!is.na(ZIE4_19fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_19fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_19fix$WC_100cm[idx] <- (ZIE4_19fix$WC_100cm[r$starts[i]] + ZIE4_19fix$WC_100cm[r$ends[i]])/2
}

#Recombine
ZIE4_19early <- filter(ZIE4_19, Date_time < "2019-11-29 1:00:01")
ZIE4_19late <- filter(ZIE4_19, Date_time > "2019-12-31 1:00:01")

ZIE4_19 <- bind_rows(ZIE4_19late, ZIE4_19early, ZIE4_19fix)


#Replace missing dates with NAs 
# Dates from 05/11 to 05/12 
#=========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 2, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-05-11"), as.Date("2019-05-12"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE4_19 <- insertRows(ZIE4_19, c(18631:18632), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(ZIE4_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE4 2020
##################################################################################################
ZIE4_20 <- subset(ZIE4, Year == '2020')

#Plotting 
ZIE4_20$WC_15cm <- as.numeric(ZIE4_20$WC_15cm)
ZIE4_20$WC_30cm <- as.numeric(ZIE4_20$WC_30cm)
ZIE4_20$WC_100cm <- as.numeric(ZIE4_20$WC_100cm)
ZIE4_20$Date_time<- mdy_hms(ZIE4_20$Date_time)

#15 cm
###########################################
ZIE4_20$WC_15cm[ZIE4_20$WC_15cm < 0.15] <- NA
missing <- which(is.na(ZIE4_20$WC_15cm))

if(1 %in% missing){
  ZIE4_20$WC_15cm[1] <- head(ZIE4_20$WC_15cm[!is.na(ZIE4_20$WC_15cm)],1)
}
if(nrow(ZIE4_20) %in% missing){
  ZIE4_20$WC_15cm[nrow(data)] <- tail(ZIE4_20$WC_15cm[!is.na(ZIE4_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20$WC_15cm[idx] <- (ZIE4_20$WC_15cm[r$starts[i]] + ZIE4_20$WC_15cm[r$ends[i]])/2
}

ZIE4_20 <- ZIE4_20 %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
ZIE4_20 <- transform(ZIE4_20, incr=as.numeric(gsub('\\%', '', increase))/100)

ZIE4_20 <- transform(ZIE4_20, WC_15cm=ifelse(incr < -0.01, 
                                             as.numeric(stats::filter(WC_15cm, rep(1/4, 4), sides=2)), 
                                             WC_15cm))

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-01-31 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-05-24 1:00:01")

ZIE4_20fix$WC_15cm[ZIE4_20fix$WC_15cm < 0.23] <- NA
missing <- which(is.na(ZIE4_20fix$WC_15cm))

if(1 %in% missing){
  ZIE4_20fix$WC_15cm[1] <- head(ZIE4_20fix$WC_15cm[!is.na(ZIE4_20fix$WC_15cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_15cm[nrow(data)] <- tail(ZIE4_20fix$WC_15cm[!is.na(ZIE4_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_15cm[idx] <- (ZIE4_20fix$WC_15cm[r$starts[i]] + ZIE4_20fix$WC_15cm[r$ends[i]])/2
}

#Plot again 
Soil <- ggplot(data = subset(ZIE4_20fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-01-31 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-05-24 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-06-29 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-07-14 1:00:01")

ZIE4_20fix$WC_15cm[ZIE4_20fix$WC_15cm < 0.35] <- NA
missing <- which(is.na(ZIE4_20fix$WC_15cm))

if(1 %in% missing){
  ZIE4_20fix$WC_15cm[1] <- head(ZIE4_20fix$WC_15cm[!is.na(ZIE4_20fix$WC_15cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_15cm[nrow(data)] <- tail(ZIE4_20fix$WC_15cm[!is.na(ZIE4_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_15cm[idx] <- (ZIE4_20fix$WC_15cm[r$starts[i]] + ZIE4_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-06-29 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-07-14 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-03-15 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-23 1:00:01")

ZIE4_20fix$WC_15cm[ZIE4_20fix$WC_15cm < 0.27] <- NA
missing <- which(is.na(ZIE4_20fix$WC_15cm))

if(1 %in% missing){
  ZIE4_20fix$WC_15cm[1] <- head(ZIE4_20fix$WC_15cm[!is.na(ZIE4_20fix$WC_15cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_15cm[nrow(data)] <- tail(ZIE4_20fix$WC_15cm[!is.na(ZIE4_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_15cm[idx] <- (ZIE4_20fix$WC_15cm[r$starts[i]] + ZIE4_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-03-15 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-23 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-20 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-05-01 1:00:01")

ZIE4_20fix$WC_15cm[ZIE4_20fix$WC_15cm < 0.243] <- NA
missing <- which(is.na(ZIE4_20fix$WC_15cm))

if(1 %in% missing){
  ZIE4_20fix$WC_15cm[1] <- head(ZIE4_20fix$WC_15cm[!is.na(ZIE4_20fix$WC_15cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_15cm[nrow(data)] <- tail(ZIE4_20fix$WC_15cm[!is.na(ZIE4_20fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_15cm[idx] <- (ZIE4_20fix$WC_15cm[r$starts[i]] + ZIE4_20fix$WC_15cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-20 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-05-01 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#30 cm 
################################################################
ZIE4_20$WC_30cm[ZIE4_20$WC_30cm < 0] <- NA
missing <- which(is.na(ZIE4_20$WC_30cm))

if(1 %in% missing){
  ZIE4_20$WC_30cm[1] <- head(ZIE4_20$WC_30cm[!is.na(ZIE4_20$WC_30cm)],1)
}
if(nrow(ZIE4_20) %in% missing){
  ZIE4_20$WC_30cm[nrow(data)] <- tail(ZIE4_20$WC_30cm[!is.na(ZIE4_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20$WC_30cm[idx] <- (ZIE4_20$WC_30cm[r$starts[i]] + ZIE4_20$WC_30cm[r$ends[i]])/2
}

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-01-31 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-05-24 1:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.28] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-01-31 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-05-24 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-02-14 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-02-20 1:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-02-14 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-02-20 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-02-14 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-02-20 1:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-02-14 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-02-20 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-02-20 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-02-24 1:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.297] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-02-20 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-02-24 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-02-24 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-02-28 1:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2925] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-02-24 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-02-28 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-02-27 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-03 1:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.289] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-02-27 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-03 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-02-28 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-01 1:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2915] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-02-28 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-01 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-03-01 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-04 1:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2876] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-03-01 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-04 1:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-03-04 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-06 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.284] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-03-04 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-06 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-03-08 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-10 15:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2835] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-03-08 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-10 15:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-03-14 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-20 15:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.285] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-03-14 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-20 15:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-03-15 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-15 15:00:01")

#Plot again 
Soil <- ggplot(data = subset(ZIE4_20fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.3142] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-03-15 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-15 15:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-03-15 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-20 15:00:01")

#Plot again 
Soil <- ggplot(data = subset(ZIE4_20fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.309] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-03-15 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-20 15:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-03-20 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-03-30 15:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.301] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-03-20 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-03-30 15:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-13 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-17 15:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.3029] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-13 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-17 15:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-13 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-17 15:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.3029] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-13 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-17 15:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-17 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-19 15:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.3015] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-17 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-19 15:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-19 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-24 15:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2949] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-19 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-24 15:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-24 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-26 20:00:01")

#Plot again 
Soil <- ggplot(data = subset(ZIE4_20fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.292] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-24 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-26 20:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-24 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-26 20:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.292] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-24 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-26 20:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-27 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-29 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2905] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-27 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-29 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-28 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-29 08:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2905] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-28 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-29 08:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-05-01 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-05-02 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.288] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-05-01 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-05-02 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-05-02 1:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-05-03 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.285] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-05-02 1:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-05-03 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-05-02 16:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-05-05 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.291] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-05-02 16:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-05-05 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-05-05 00:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-05-07 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.288] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-05-05 00:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-05-07 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-05-07 00:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-05-08 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.287] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-05-07 00:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-05-08 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-05-08 00:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-05-10 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2845] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-05-08 00:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-05-10 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-02-05 00:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-02-14 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.305] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-02-05 00:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-02-14 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-05-30 00:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-06-09 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.275] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-05-30 00:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-06-09 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-06-04 00:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-06-06 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.282] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-06-04 00:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-06-06 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-20 00:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-27 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2925] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-20 00:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-27 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-04-20 00:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-04-23 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.2949] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-04-20 00:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-04-23 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#Subset and then remove drips
#==========================================================================
ZIE4_20fix <- filter(ZIE4_20, Date_time > "2020-06-08 00:00:01")
ZIE4_20fix <- filter(ZIE4_20fix, Date_time < "2020-06-10 00:00:01")

ZIE4_20fix$WC_30cm[ZIE4_20fix$WC_30cm < 0.275] <- NA
missing <- which(is.na(ZIE4_20fix$WC_30cm))

if(1 %in% missing){
  ZIE4_20fix$WC_30cm[1] <- head(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}
if(nrow(ZIE4_20fix) %in% missing){
  ZIE4_20fix$WC_30cm[nrow(data)] <- tail(ZIE4_20fix$WC_30cm[!is.na(ZIE4_20fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20fix$WC_30cm[idx] <- (ZIE4_20fix$WC_30cm[r$starts[i]] + ZIE4_20fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_20early <- filter(ZIE4_20, Date_time < "2020-06-08 00:00:01")
ZIE4_20late <- filter(ZIE4_20, Date_time > "2020-06-10 00:00:01")

ZIE4_20 <- bind_rows(ZIE4_20late, ZIE4_20early, ZIE4_20fix)

#100 cm 
################################################################
ZIE4_20$WC_100cm[ZIE4_20$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE4_19$WC_100cm))

if(1 %in% missing){
  ZIE4_20$WC_100cm[1] <- head(ZIE4_20$WC_100cm[!is.na(ZIE4_20$WC_100cm)],1)
}
if(nrow(ZIE4_20) %in% missing){
  ZIE4_20$WC_100cm[nrow(data)] <- tail(ZIE4_20$WC_100cm[!is.na(ZIE4_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_20$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_20$WC_100cm[idx] <- (ZIE4_20$WC_100cm[r$starts[i]] + ZIE4_20$WC_100cm[r$ends[i]])/2
}

#Replace missing dates with NAs 
# Dates from 04/11 to 04/15 
#=========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 3, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-04-12"), as.Date("2020-04-14"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE4_20 <- insertRows(ZIE4_20, c(36734:36736), new = insertDF)

# Dates from 05/29 to 06/02 
#=========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 3, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-05-30"), as.Date("2020-06-01"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE4_20 <- insertRows(ZIE4_20, c(33051:33053), new = insertDF)

# Dates from 07/11 to 07/17
#=========================================================================
insertDF <- as.data.frame(matrix(data = NA, nrow = 5, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-07-12"), as.Date("2020-07-16"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

ZIE4_20 <- insertRows(ZIE4_20, c(1:5), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(ZIE4_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#ZIE4 2021
##################################################################################################
ZIE4_21 <- subset(ZIE4, Year == '2021')

#Plotting 
ZIE4_21$WC_15cm <- as.numeric(ZIE4_21$WC_15cm)
ZIE4_21$WC_30cm <- as.numeric(ZIE4_21$WC_30cm)
ZIE4_21$WC_100cm <- as.numeric(ZIE4_21$WC_100cm)
ZIE4_21$Date_time<- mdy_hms(ZIE4_21$Date_time)

#15 cm
###########################################
ZIE4_21$WC_15cm[ZIE4_21$WC_15cm < 0] <- NA
missing <- which(is.na(ZIE4_21$WC_15cm))

if(1 %in% missing){
  ZIE4_21$WC_15cm[1] <- head(ZIE4_21$WC_15cm[!is.na(ZIE4_21$WC_15cm)],1)
}
if(nrow(ZIE4_21) %in% missing){
  ZIE4_21$WC_15cm[nrow(data)] <- tail(ZIE4_21$WC_15cm[!is.na(ZIE4_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_21$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_21$WC_15cm[idx] <- (ZIE4_21$WC_15cm[r$starts[i]] + ZIE4_21$WC_15cm[r$ends[i]])/2
}

#30 cm 
################################################################
ZIE4_21$WC_30cm[ZIE4_21$WC_30cm < 0] <- NA
missing <- which(is.na(ZIE4_21$WC_30cm))

if(1 %in% missing){
  ZIE4_21$WC_30cm[1] <- head(ZIE4_21$WC_30cm[!is.na(ZIE4_21$WC_30cm)],1)
}
if(nrow(ZIE4_21) %in% missing){
  ZIE4_21$WC_30cm[nrow(data)] <- tail(ZIE4_21$WC_30cm[!is.na(ZIE4_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_21$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_21$WC_30cm[idx] <- (ZIE4_21$WC_30cm[r$starts[i]] + ZIE4_21$WC_30cm[r$ends[i]])/2
}

#Subset and remove drips in April/May 
#================================================================================
ZIE4_21fix <- filter(ZIE4_21, Date_time > "2021-4-15 00:00:01")
ZIE4_21fix <- filter(ZIE4_21fix, Date_time < "2021-05-01 00:00:01")

Soil <- ggplot(data = subset(ZIE4_21fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE4_21fix$WC_30cm[ZIE4_21fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(ZIE4_21fix$WC_30cm))

if(1 %in% missing){
  ZIE4_21fix$WC_30cm[1] <- head(ZIE4_21fix$WC_30cm[!is.na(ZIE4_21fix$WC_30cm)],1)
}
if(nrow(ZIE4_21fix) %in% missing){
  ZIE4_21fix$WC_30cm[nrow(data)] <- tail(ZIE4_21fix$WC_30cm[!is.na(ZIE4_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_21fix$WC_30cm[idx] <- (ZIE4_21fix$WC_30cm[r$starts[i]] + ZIE4_21fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_21early <- filter(ZIE4_21, Date_time < "2021-4-15 00:00:01")
ZIE4_21late <- filter(ZIE4_21, Date_time > "2021-05-01 00:00:01")

ZIE4_21 <- bind_rows(ZIE4_21late, ZIE4_21early, ZIE4_21fix)

#Subset and remove drips in April/May 
#================================================================================
ZIE4_21fix <- filter(ZIE4_21, Date_time > "2021-05-24 00:00:01")
ZIE4_21fix <- filter(ZIE4_21fix, Date_time < "2021-05-27 00:00:01")

Soil <- ggplot(data = subset(ZIE4_21fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE4_21fix$WC_30cm[ZIE4_21fix$WC_30cm < 0.218] <- NA
missing <- which(is.na(ZIE4_21fix$WC_30cm))

if(1 %in% missing){
  ZIE4_21fix$WC_30cm[1] <- head(ZIE4_21fix$WC_30cm[!is.na(ZIE4_21fix$WC_30cm)],1)
}
if(nrow(ZIE4_21fix) %in% missing){
  ZIE4_21fix$WC_30cm[nrow(data)] <- tail(ZIE4_21fix$WC_30cm[!is.na(ZIE4_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_21fix$WC_30cm[idx] <- (ZIE4_21fix$WC_30cm[r$starts[i]] + ZIE4_21fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_21early <- filter(ZIE4_21, Date_time < "2021-05-24 00:00:01")
ZIE4_21late <- filter(ZIE4_21, Date_time > "2021-05-27 00:00:01")

ZIE4_21 <- bind_rows(ZIE4_21late, ZIE4_21early, ZIE4_21fix)

#Subset and remove drips in April/May 
  #This one caused a few losses in the dataset, but it seemed better than the dip
#================================================================================
ZIE4_21fix <- filter(ZIE4_21, Date_time > "2021-04-29 00:00:01")
ZIE4_21fix <- filter(ZIE4_21fix, Date_time < "2021-05-02 00:00:01")

Soil <- ggplot(data = subset(ZIE4_21fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE4_21fix$WC_30cm[ZIE4_21fix$WC_30cm < 0.2675] <- NA
missing <- which(is.na(ZIE4_21fix$WC_30cm))

if(1 %in% missing){
  ZIE4_21fix$WC_30cm[1] <- head(ZIE4_21fix$WC_30cm[!is.na(ZIE4_21fix$WC_30cm)],1)
}
if(nrow(ZIE4_21fix) %in% missing){
  ZIE4_21fix$WC_30cm[nrow(data)] <- tail(ZIE4_21fix$WC_30cm[!is.na(ZIE4_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_21fix$WC_30cm[idx] <- (ZIE4_21fix$WC_30cm[r$starts[i]] + ZIE4_21fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_21early <- filter(ZIE4_21, Date_time < "2021-04-29 00:00:01")
ZIE4_21late <- filter(ZIE4_21, Date_time > "2021-05-02 00:00:01")

ZIE4_21 <- bind_rows(ZIE4_21late, ZIE4_21early, ZIE4_21fix)


#Subset and remove drips in April/May 
#================================================================================
ZIE4_21fix <- filter(ZIE4_21, Date_time > "2021-06-07 00:00:01")
ZIE4_21fix <- filter(ZIE4_21fix, Date_time < "2021-06-16 00:00:01")

Soil <- ggplot(data = subset(ZIE4_21fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

ZIE4_21fix$WC_30cm[ZIE4_21fix$WC_30cm < 0.175] <- NA
missing <- which(is.na(ZIE4_21fix$WC_30cm))

if(1 %in% missing){
  ZIE4_21fix$WC_30cm[1] <- head(ZIE4_21fix$WC_30cm[!is.na(ZIE4_21fix$WC_30cm)],1)
}
if(nrow(ZIE4_21fix) %in% missing){
  ZIE4_21fix$WC_30cm[nrow(data)] <- tail(ZIE4_21fix$WC_30cm[!is.na(ZIE4_21fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_21fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_21fix$WC_30cm[idx] <- (ZIE4_21fix$WC_30cm[r$starts[i]] + ZIE4_21fix$WC_30cm[r$ends[i]])/2
}

#Recombine
ZIE4_21early <- filter(ZIE4_21, Date_time < "2021-06-07 00:00:01")
ZIE4_21late <- filter(ZIE4_21, Date_time > "2021-06-16 00:00:01")

ZIE4_21 <- bind_rows(ZIE4_21late, ZIE4_21early, ZIE4_21fix)

#100 cm 
################################################################
ZIE4_21$WC_100cm[ZIE4_21$WC_100cm < 0] <- NA
missing <- which(is.na(ZIE4_19$WC_100cm))

if(1 %in% missing){
  ZIE4_21$WC_100cm[1] <- head(ZIE4_21$WC_100cm[!is.na(ZIE4_21$WC_100cm)],1)
}
if(nrow(ZIE4_21) %in% missing){
  ZIE4_21$WC_100cm[nrow(data)] <- tail(ZIE4_21$WC_100cm[!is.na(ZIE4_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(ZIE4_21$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  ZIE4_21$WC_100cm[idx] <- (ZIE4_21$WC_100cm[r$starts[i]] + ZIE4_21$WC_100cm[r$ends[i]])/2
}


#Plot again 
Soil <- ggplot(data = subset(ZIE4_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
ZIE4_clean <- merge(ZIE4_18, ZIE4_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
ZIE4_clean <- merge(ZIE4_clean, ZIE4_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE4_clean <- merge(ZIE4_clean, ZIE4_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
ZIE4_clean <- merge(ZIE4_clean, ZIE4_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(ZIE4_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("ZIE4_Salli", width = 4500, height = 2500)

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
write.csv(ZIE4_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/ZIE/ZIE4_clean.csv" ) #this writes a csv file and sends it to the working folder


