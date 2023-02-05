#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/05/2023
#Description: QA/QC UQL 2

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
#Had to copy and paste the header column of the first csv over the other csvs, so that R knew they were the same column 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL", 
                        pattern=glob2rx("Copy of U2M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console


#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
UQL2_2019_2021 <- data19_21 %>%
  rename_all(funs(c("Date", "Time", "PAR", "WC_15cm", "WC_30cm","WC_100cm", "Date_time"))) %>% #this renames all of the columns
  select(Date_time, PAR, WC_15cm, WC_30cm, WC_100cm) #this keeps only the columns you will use


#2017-2018 files
#========================================================================================================================
#To merge the files, needed to manually delete the extra column in W1M190111 and 
#extra row at the top, and needed to manually delete the extra columns in W1M171228, W1M180201, and W1M180302

#Set the data path 
data_path <- "~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL"
new_col_name <- c("Date_time", "PAR", "WC_15cm", "WC_30cm", "WC_100cm")

#Call in all the files in this folder with the W2M1 pattern, which excludes the datasheets from 2019-2020
files <- dir(data_path, pattern=glob2rx("Copy of U2M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
UQL2_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
UQL2 <- rbind(UQL2_2017_2019, UQL2_2019_2021)

#Write the csv
write.csv(UQL2,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL2.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(UQL2)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
UQL2$Date <- mdy_hms(UQL2$Date_time)

#Put year into a separate column 
UQL2 <- separate(UQL2, Date, c("Year"))

#UQL2 2017
##################################################################################################
UQL2_17 <- subset(UQL2, Year == '2017')

#Plotting 
UQL2_17$WC_15cm <- as.numeric(UQL2_17$WC_15cm)
UQL2_17$WC_30cm <- as.numeric(UQL2_17$WC_30cm)
UQL2_17$WC_100cm <- as.numeric(UQL2_17$WC_100cm)
UQL2_17$Date_time<- mdy_hms(UQL2_17$Date_time)

Soil <- ggplot(data = subset(UQL2_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 
##################################################################################################

#UQL2 2018
##################################################################################################
UQL2_18 <- subset(UQL2, Year == '2018')

#Plotting 
UQL2_18$WC_15cm <- as.numeric(UQL2_18$WC_15cm)
UQL2_18$WC_30cm <- as.numeric(UQL2_18$WC_30cm)
UQL2_18$WC_100cm <- as.numeric(UQL2_18$WC_100cm)
UQL2_18$Date_time<- mdy_hms(UQL2_18$Date_time)

#15 cm
###########################################
UQL2_18$WC_15cm[UQL2_18$WC_15cm < 0] <- NA
missing <- which(is.na(UQL2_18$WC_15cm))

if(1 %in% missing){
  UQL2_18$WC_15cm[1] <- head(UQL2_18$WC_15cm[!is.na(UQL2_18$WC_15cm)],1)
}
if(nrow(UQL2_18) %in% missing){
  UQL2_18$WC_15cm[nrow(data)] <- tail(UQL2_18$WC_15cm[!is.na(UQL2_18$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_18$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_18$WC_15cm[idx] <- (UQL2_18$WC_15cm[r$starts[i]] + UQL2_18$WC_15cm[r$ends[i]])/2
}

#Subset and remove drip at end of September/early October
#========================================================================
UQL2_18_fix <- filter(UQL2_18, Date_time > "2018-09-29 1:00:01")
UQL2_18_fix <- filter(UQL2_18_fix, Date_time < "2018-10-01 12:00:01")

Soil <- ggplot(data = subset(UQL2_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

UQL2_18_fix$WC_15cm[UQL2_18_fix$WC_15cm < 0.198] <- NA

#Recombine 
UQL2_18_later <- filter(UQL2_18, Date_time < "2018-09-29 1:00:01")
UQL2_18_end <- filter(UQL2_18, Date_time > "2018-10-01 12:00:01")

UQL2_18 <- bind_rows(UQL2_18_fix, UQL2_18_later, UQL2_18_end)

#Subset and remove drip at end of September/early October
#========================================================================
UQL2_18_fix <- filter(UQL2_18, Date_time > "2018-11-20 1:00:01")
UQL2_18_fix <- filter(UQL2_18_fix, Date_time < "2018-12-31 12:00:01")

UQL2_18_fix$WC_15cm[UQL2_18_fix$WC_15cm < 0.209] <- NA
missing <- which(is.na(UQL2_18_fix$WC_15cm))

if(1 %in% missing){
  UQL2_18_fix$WC_15cm[1] <- head(UQL2_18_fix$WC_15cm[!is.na(UQL2_18_fix$WC_15cm)],1)
}
if(nrow(UQL2_18_fix) %in% missing){
  UQL2_18_fix$WC_15cm[nrow(data)] <- tail(UQL2_18_fix$WC_15cm[!is.na(UQL2_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_18_fix$WC_15cm[idx] <- (UQL2_18_fix$WC_15cm[r$starts[i]] + UQL2_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
UQL2_18_later <- filter(UQL2_18, Date_time < "2018-11-20 1:00:01")
UQL2_18_end <- filter(UQL2_18, Date_time > "2018-12-31 12:00:01")
UQL2_18 <- bind_rows(UQL2_18_fix, UQL2_18_later, UQL2_18_end)

#30 cm 
############################################################################

#Subset and remove drip at end of September/early October
#========================================================================
UQL2_18_fix <- filter(UQL2_18, Date_time > "2018-08-29 1:00:01")
UQL2_18_fix <- filter(UQL2_18_fix, Date_time < "2018-10-20 12:00:01")

UQL2_18_fix$WC_30cm[UQL2_18_fix$WC_30cm < 0.178] <- NA

#Recombine 
UQL2_18_later <- filter(UQL2_18, Date_time < "2018-08-29 1:00:01")
UQL2_18_end <- filter(UQL2_18, Date_time > "2018-10-20 12:00:01")
UQL2_18 <- bind_rows(UQL2_18_fix, UQL2_18_later, UQL2_18_end)

#Subset and remove drip at end of September/early October
#========================================================================
UQL2_18_fix <- filter(UQL2_18, Date_time > "2018-10-19 1:00:01")
UQL2_18_fix <- filter(UQL2_18_fix, Date_time < "2018-11-02 12:00:01")

UQL2_18_fix$WC_30cm[UQL2_18_fix$WC_30cm < 0.1785] <- NA

#Recombine 
UQL2_18_later <- filter(UQL2_18, Date_time < "2018-10-19 1:00:01")
UQL2_18_end <- filter(UQL2_18, Date_time > "2018-11-02 12:00:01")
UQL2_18 <- bind_rows(UQL2_18_fix, UQL2_18_later, UQL2_18_end)

#Subset and remove drip at end of September/early October
#========================================================================
UQL2_18_fix <- filter(UQL2_18, Date_time > "2018-11-01 1:00:01")
UQL2_18_fix <- filter(UQL2_18_fix, Date_time < "2018-11-30 12:00:01")

Soil <- ggplot(data = subset(UQL2_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "lightblue"))
Soil 

UQL2_18_fix$WC_30cm[UQL2_18_fix$WC_30cm < 0.176] <- NA

#Recombine 
UQL2_18_later <- filter(UQL2_18, Date_time < "2018-11-01 1:00:01")
UQL2_18_end <- filter(UQL2_18, Date_time > "2018-11-30 12:00:01")
UQL2_18 <- bind_rows(UQL2_18_fix, UQL2_18_later, UQL2_18_end)

#Plot again 
Soil <- ggplot(data = subset(UQL2_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL2 2019
##################################################################################################
UQL2_19 <- subset(UQL2, Year == '2019')

#Plotting 
UQL2_19$WC_15cm <- as.numeric(UQL2_19$WC_15cm)
UQL2_19$WC_30cm <- as.numeric(UQL2_19$WC_30cm)
UQL2_19$WC_100cm <- as.numeric(UQL2_19$WC_100cm)
UQL2_19$Date_time<- mdy_hms(UQL2_19$Date_time)

#15 cm
###########################################
UQL2_19$WC_15cm[UQL2_19$WC_15cm < 0.21] <- NA
missing <- which(is.na(UQL2_19$WC_15cm))

if(1 %in% missing){
  UQL2_19$WC_15cm[1] <- head(UQL2_19$WC_15cm[!is.na(UQL2_19$WC_15cm)],1)
}
if(nrow(UQL2_19) %in% missing){
  UQL2_19$WC_15cm[nrow(data)] <- tail(UQL2_19$WC_15cm[!is.na(UQL2_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19$WC_15cm[idx] <- (UQL2_19$WC_15cm[r$starts[i]] + UQL2_19$WC_15cm[r$ends[i]])/2
}

#Get rid of drips in 15 cm in late September through October
#======================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-09-23 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-09-29 1:00:01")

UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.252] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time > "2019-09-29 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time < "2019-09-23 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)


#Get rid of drips in 15 cm in late September through October
#======================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-03 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-10-04 1:00:01")


UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.248] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time > "2019-10-04 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time < "2019-10-03 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove more dips later in October
#===================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-01 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-10-08 1:00:01")


UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.24] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time > "2019-10-08 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  < "2019-10-01 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove more dips later in October
#===================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-09 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-10-12 1:00:01")


UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.235] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time > "2019-10-12 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  < "2019-10-09 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove more dips later in October
#===================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-22 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-10-24 1:00:01")


UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.245] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time > "2019-10-24 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  < "2019-10-22 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)


#Remove more dip at start of September 
#===================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-09-19 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-09-22 1:00:01")

UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.26] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-09-19 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-09-22 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove more dip at start of September 
#===================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-09-25 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-09-27 1:00:01")

UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.2557] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-09-25 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-09-27 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove lowest 15 cm value that is the drip
UQL2_19$WC_15cm[UQL2_19$WC_15cm < 0.22] <- NA
missing <- which(is.na(UQL2_19$WC_15cm))

if(1 %in% missing){
  UQL2_19$WC_15cm[1] <- head(UQL2_19$WC_15cm[!is.na(UQL2_19$WC_15cm)],1)
}
if(nrow(UQL2_19) %in% missing){
  UQL2_19$WC_15cm[nrow(data)] <- tail(UQL2_19$WC_15cm[!is.na(UQL2_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19$WC_15cm[idx] <- (UQL2_19$WC_15cm[r$starts[i]] + UQL2_19$WC_15cm[r$ends[i]])/2
}

#Remove December drip 
#==============================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-12-16 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-12-31 1:00:01")

UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.323] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-12-16 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-12-31 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove drips 
#==============================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-01 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-10-04 1:00:01")

UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.25] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-10-01 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-10-04 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove drips 
#==============================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-09 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-10-11 1:00:01")

UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.237] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-10-09 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-10-11 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove drips 
#==============================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-26 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-10-27 1:00:01")

UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm < 0.2375] <- NA
missing <- which(is.na(UQL2_19_fix$WC_15cm))

if(1 %in% missing){
  UQL2_19_fix$WC_15cm[1] <- head(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}
if(nrow(UQL2_19_fix) %in% missing){
  UQL2_19_fix$WC_15cm[nrow(data)] <- tail(UQL2_19_fix$WC_15cm[!is.na(UQL2_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_19_fix$WC_15cm[idx] <- (UQL2_19_fix$WC_15cm[r$starts[i]] + UQL2_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-10-26 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-10-27 1:00:01")
UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove drips 
#==============================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-31 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-11-14 20:00:01")

#Get rid of increases
UQL2_19_fix <- UQL2_19_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL2_19_fix <- transform(UQL2_19_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL2_19_fix <- transform(UQL2_19_fix, WC_15cm=ifelse(incr < -0.001 | incr > 0.001 , 
                                                     as.numeric(stats::filter(WC_15cm, rep(1/24, 24), sides=2)),
                                                     WC_15cm))

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-10-31 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-11-14 20:00:01")
UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove glitch in 15 cm 
#========================================================================
UQL2_19$WC_15cm[UQL2_19$WC_15cm == 0.3643] <- NA

#30 cm
###################################################################################

#Remove drips 
#==============================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-28 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-11-14 20:00:01")

#Get rid of increases
UQL2_19_fix <- UQL2_19_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL2_19_fix <- transform(UQL2_19_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL2_19_fix <- transform(UQL2_19_fix, WC_30cm=ifelse(incr < -0.001 | incr > 0.001 , 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)),
                                                     WC_30cm))

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-10-28 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-11-14 20:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Calibrate
#===========================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-09-15 19:50:01 ")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-12-31 23:50:01")

UQL2_19_fix$WC_30cm <- UQL2_19_fix$WC_30cm + 0.0079 

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-09-15 19:50:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-12-31 23:50:01")
UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove glitches
#===========================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-09-01 19:50:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-12-31 23:50:01")

UQL2_19_fix$WC_30cm[UQL2_19_fix$WC_30cm < 0.178] <- NA

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-09-01 19:50:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-12-31 23:50:01")
UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove glitches
#===========================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-09-01 19:50:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-10-01 23:50:01")

UQL2_19_fix$WC_30cm[UQL2_19_fix$WC_30cm < 0.181] <- NA

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-09-01 19:50:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-10-01 23:50:01")
UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove glitches
#===========================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-10-13 19:50:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-10-21 23:50:01")

UQL2_19_fix$WC_30cm[UQL2_19_fix$WC_30cm < 0.184] <- NA

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-10-13 19:50:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-10-21 23:50:01")
UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove glitches
#===========================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-11-11 19:50:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-11-21 23:50:01")


UQL2_19_fix$WC_30cm[UQL2_19_fix$WC_30cm > 0.182] <- NA

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-11-11 19:50:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-11-21 23:50:01")
UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Remove glitches
#===========================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-08-08 19:50:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-08-10 10:00:01")

UQL2_19_fix$WC_30cm[UQL2_19_fix$WC_30cm < 0.209] <- NA

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-08-08 19:50:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-08-10 10:00:01")
UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Missing dates and glitches
###########################################################################

#Remove glitches before the missing dates began
#========================================================================
UQL2_19_fix <- filter(UQL2_19, Date_time > "2019-12-04 1:00:01")
UQL2_19_fix <- filter(UQL2_19_fix, Date_time < "2019-12-15 1:00:01")

UQL2_19_fix$WC_100cm[UQL2_19_fix$WC_100cm > 0.229] <- NA
UQL2_19_fix$WC_30cm[UQL2_19_fix$WC_30cm > 0.191] <- NA
UQL2_19_fix$WC_15cm[UQL2_19_fix$WC_15cm > 0.33] <- NA

#Recombine July with other dataset 
UQL2_19_later <- filter(UQL2_19, Date_time < "2019-12-04 1:00:01")
UQL2_19_end <- filter(UQL2_19, Date_time  > "2019-12-15 1:00:01")

UQL2_19 <- bind_rows(UQL2_19_fix, UQL2_19_later, UQL2_19_end)

#Replace missing dates
#========================================================================
#12/06 to 12/17
insertDF <- as.data.frame(matrix(data = NA, nrow = 10, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2019-12-07"), as.Date("2019-12-16"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

UQL2_19 <- insertRows(UQL2_19, c(50301:50310), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(UQL2_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL2 2020
##################################################################################################
UQL2_20 <- subset(UQL2, Year == '2020')

#Plotting 
UQL2_20$WC_15cm <- as.numeric(UQL2_20$WC_15cm)
UQL2_20$WC_30cm <- as.numeric(UQL2_20$WC_30cm)
UQL2_20$WC_100cm <- as.numeric(UQL2_20$WC_100cm)
UQL2_20$Date_time<- mdy_hms(UQL2_20$Date_time)

#15 cm
###########################################
UQL2_20$WC_15cm[UQL2_20$WC_15cm < 0.2] <- NA
missing <- which(is.na(UQL2_20$WC_15cm))

if(1 %in% missing){
  UQL2_20$WC_15cm[1] <- head(UQL2_20$WC_15cm[!is.na(UQL2_20$WC_15cm)],1)
}
if(nrow(UQL2_20) %in% missing){
  UQL2_20$WC_15cm[nrow(data)] <- tail(UQL2_20$WC_15cm[!is.na(UQL2_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_20$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_20$WC_15cm[idx] <- (UQL2_20$WC_15cm[r$starts[i]] + UQL2_20$WC_15cm[r$ends[i]])/2
}

#Remove 15 cm dips in April 
#==============================================================
UQL2_20_fix <- filter(UQL2_20, Date_time > "2020-04-01 1:00:01")
UQL2_20_fix <- filter(UQL2_20_fix, Date_time < "2020-04-24 1:00:01")

UQL2_20_fix$WC_15cm[UQL2_20_fix$WC_15cm < 0.32] <- NA
missing <- which(is.na(UQL2_20_fix$WC_15cm))

if(1 %in% missing){
  UQL2_20_fix$WC_15cm[1] <- head(UQL2_20_fix$WC_15cm[!is.na(UQL2_20_fix$WC_15cm)],1)
}
if(nrow(UQL2_20_fix) %in% missing){
  UQL2_20_fix$WC_15cm[nrow(data)] <- tail(UQL2_20_fix$WC_15cm[!is.na(UQL2_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_20_fix$WC_15cm[idx] <- (UQL2_20_fix$WC_15cm[r$starts[i]] + UQL2_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL2_20_later <- filter(UQL2_20, Date_time > "2020-04-24 1:00:01")
UQL2_20_end <- filter(UQL2_20, Date_time  < "2020-04-01 1:00:01")
UQL2_20 <- bind_rows(UQL2_20_fix, UQL2_20_later, UQL2_20_end)

#Fix 15 cm glitch in October
#========================================================================
UQL2_20$WC_15cm[UQL2_20$WC_15cm == 0.27425] <- NA

#Get rid of glitches after missing dates 
#======================================================================
UQL2_20_fix <- filter(UQL2_20, Date_time > "2020-11-02 00:00:01")
UQL2_20_fix <- filter(UQL2_20_fix, Date_time < "2020-11-13 01:00:01")

UQL2_20_fix$WC_15cm[UQL2_20_fix$WC_15cm > 0.2] <- NA

#Recombine July with other dataset 
UQL2_20_early <- filter(UQL2_20, Date_time < "2020-11-02 00:00:01")
UQL2_20_later <- filter(UQL2_20, Date_time  > "2020-11-13 01:00:01")
UQL2_20 <- bind_rows(UQL2_20_fix, UQL2_20_later, UQL2_20_early)

#30 cm 
################################################################
UQL2_20$WC_30cm[UQL2_20$WC_30cm < 0.15] <- NA
missing <- which(is.na(UQL2_20$WC_30cm))

if(1 %in% missing){
  UQL2_20$WC_30cm[1] <- head(UQL2_20$WC_30cm[!is.na(UQL2_20$WC_30cm)],1)
}
if(nrow(UQL2_20) %in% missing){
  UQL2_20$WC_30cm[nrow(data)] <- tail(UQL2_20$WC_30cm[!is.na(UQL2_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_20$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_20$WC_30cm[idx] <- (UQL2_20$WC_30cm[r$starts[i]] + UQL2_20$WC_30cm[r$ends[i]])/2
}

#Remove glitch
#===========================================================================
UQL2_20_fix <- filter(UQL2_20, Date_time > "2020-11-05 08:00:01")
UQL2_20_fix <- filter(UQL2_20_fix, Date_time < "2020-11-14 1:00:01")

Soil <- ggplot(data = subset(UQL2_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "lightblue"))
Soil 

UQL2_20_fix$WC_30cm[UQL2_20_fix$WC_30cm > 0] <- NA

#Recombine July with other dataset 
UQL2_20_early <- filter(UQL2_20, Date_time < "2020-11-05 08:00:01")
UQL2_20_later <- filter(UQL2_20, Date_time  > "2020-11-14 1:00:01")
UQL2_20 <- bind_rows(UQL2_20_fix, UQL2_20_later, UQL2_20_early)

#Calibrate
#===========================================================================
UQL2_20_fix <- filter(UQL2_20, Date_time > "2020-09-10 03:30:01")
UQL2_20_fix <- filter(UQL2_20_fix, Date_time < "2020-12-31 23:50:01")

UQL2_20_fix$WC_30cm <- UQL2_20_fix$WC_30cm + 0.0051

#Recombine July with other dataset 
UQL2_20_early <- filter(UQL2_20, Date_time < "2020-09-10 03:30:01")
UQL2_20_later <- filter(UQL2_20, Date_time  > "2020-12-31 23:50:01")
UQL2_20 <- bind_rows(UQL2_20_fix, UQL2_20_later, UQL2_20_early)

#100 cm 
################################################################
UQL2_20$WC_100cm[UQL2_20$WC_100cm < 0] <- NA
missing <- which(is.na(UQL2_19$WC_100cm))

if(1 %in% missing){
  UQL2_20$WC_100cm[1] <- head(UQL2_20$WC_100cm[!is.na(UQL2_20$WC_100cm)],1)
}
if(nrow(UQL2_20) %in% missing){
  UQL2_20$WC_100cm[nrow(data)] <- tail(UQL2_20$WC_100cm[!is.na(UQL2_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_20$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_20$WC_100cm[idx] <- (UQL2_20$WC_100cm[r$starts[i]] + UQL2_20$WC_100cm[r$ends[i]])/2
}

#Missing dates and glitches
##########################################################################

#Get rid of glitches before April missing dates 
#======================================================================
UQL2_20_fix <- filter(UQL2_20, Date_time > "2020-03-23 1:00:01")
UQL2_20_fix <- filter(UQL2_20_fix, Date_time < "2020-03-25 23:00:01")

UQL2_20_fix$WC_100cm[UQL2_20_fix$WC_100cm > 0.295] <- NA
UQL2_20_fix$WC_30cm[UQL2_20_fix$WC_30cm > 0.29] <- NA
UQL2_20_fix$WC_15cm[UQL2_20_fix$WC_15cm > 0.332] <- NA

#Recombine July with other dataset 
UQL2_20_later <- filter(UQL2_20, Date_time < "2020-03-23 1:00:01")
UQL2_20_end <- filter(UQL2_20, Date_time  > "2020-03-25 23:00:01")

UQL2_20 <- bind_rows(UQL2_20_fix, UQL2_20_later, UQL2_20_end)

#Get rid of glitches before April missing dates 
#======================================================================
UQL2_20_fix <- filter(UQL2_20, Date_time > "2020-10-24 12:00:01")
UQL2_20_fix <- filter(UQL2_20_fix, Date_time < "2020-10-25 23:00:01")

UQL2_20_fix$WC_100cm[UQL2_20_fix$WC_100cm > 0.222] <- NA
UQL2_20_fix$WC_30cm[UQL2_20_fix$WC_30cm > 0.155] <- NA

#Recombine July with other dataset 
UQL2_20_later <- filter(UQL2_20, Date_time < "2020-10-24 12:00:01")
UQL2_20_end <- filter(UQL2_20, Date_time  > "2020-10-25 23:00:01")
UQL2_20 <- bind_rows(UQL2_20_fix, UQL2_20_later, UQL2_20_end)

#Replace missing dates
#========================================================================
#03/25 to 04/16
insertDF <- as.data.frame(matrix(data = NA, nrow = 21, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-03-26"), as.Date("2020-04-15"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

UQL2_20 <- insertRows(UQL2_20, c(12121:12141), new = insertDF)

#10/25 to 11/02
insertDF <- as.data.frame(matrix(data = NA, nrow = 7, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-10-26"), as.Date("2020-11-01"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

UQL2_20 <- insertRows(UQL2_20, c(39839:39845), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(UQL2_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL2 2021
##################################################################################################
UQL2_21 <- subset(UQL2, Year == '2021')

#Plotting 
UQL2_21$WC_15cm <- as.numeric(UQL2_21$WC_15cm)
UQL2_21$WC_30cm <- as.numeric(UQL2_21$WC_30cm)
UQL2_21$WC_100cm <- as.numeric(UQL2_21$WC_100cm)
UQL2_21$Date_time<- mdy_hms(UQL2_21$Date_time)

#100 cm 
################################################################
UQL2_21$WC_100cm[UQL2_21$WC_100cm < 0] <- NA
missing <- which(is.na(UQL2_21$WC_100cm))

if(1 %in% missing){
  UQL2_21$WC_100cm[1] <- head(UQL2_21$WC_100cm[!is.na(UQL2_21$WC_100cm)],1)
}
if(nrow(UQL2_21) %in% missing){
  UQL2_21$WC_100cm[nrow(data)] <- tail(UQL2_21$WC_100cm[!is.na(UQL2_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL2_21$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL2_21$WC_100cm[idx] <- (UQL2_21$WC_100cm[r$starts[i]] + UQL2_21$WC_100cm[r$ends[i]])/2
}

#Remove glitch 
#=======================================================================
UQL2_21$WC_100cm[UQL2_21$WC_100cm == 0.2216] <- NA

#Plot again 
Soil <- ggplot(data = subset(UQL2_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
UQL2_clean <- merge(UQL2_18, UQL2_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
UQL2_clean <- merge(UQL2_clean, UQL2_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
UQL2_clean <- merge(UQL2_clean, UQL2_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
UQL2_clean <- merge(UQL2_clean, UQL2_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(UQL2_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 5) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 5) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 5 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date")) 
Soil

png("UQL2", width = 4500, height = 2500)

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
write.csv(UQL2_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL2_clean.csv" ) #this writes a csv file and sends it to the working folder

