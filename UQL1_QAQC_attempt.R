#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/05/2023
#Description: QA/QC UQL 1

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

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL", 
                        pattern=glob2rx("Copy of U1M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
UQL1_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("Copy of U1M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
UQL1_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
UQL1 <- rbind(UQL1_2017_2019, UQL1_2019_2021)

#Write the csv
write.csv(UQL1,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL1.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(UQL1)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
UQL1$Date <- mdy_hms(UQL1$Date_time)

#Put year into a separate column 
UQL1 <- separate(UQL1, Date, c("Year"))

#UQL1 2017
##################################################################################################
UQL1_17 <- subset(UQL1, Year == '2017')

#Plotting 
UQL1_17$WC_15cm <- as.numeric(UQL1_17$WC_15cm)
UQL1_17$WC_30cm <- as.numeric(UQL1_17$WC_30cm)
UQL1_17$WC_100cm <- as.numeric(UQL1_17$WC_100cm)
UQL1_17$Date_time<- mdy_hms(UQL1_17$Date_time)

Soil <- ggplot(data = subset(UQL1_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 
##########################################################################

#UQL1 2018
##################################################################################################
UQL1_18 <- subset(UQL1, Year == '2018')

#Plotting 
UQL1_18$WC_15cm <- as.numeric(UQL1_18$WC_15cm)
UQL1_18$WC_30cm <- as.numeric(UQL1_18$WC_30cm)
UQL1_18$WC_100cm <- as.numeric(UQL1_18$WC_100cm)
UQL1_18$Date_time<- mdy_hms(UQL1_18$Date_time)

#15 cm 
############################################################################

#Calibrate 
UQL1_18_fix <- filter(UQL1_18, Date_time > "2018-08-21 1:00:01")
UQL1_18_fix <- filter(UQL1_18_fix, Date_time < "2018-09-16 03:50:01")

Soil <- ggplot(data = subset(UQL1_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

UQL1_18_fix$WC_100cm <- UQL1_18_fix$WC_100cm + 0.4982

#Recombine and then subset again
UQL1_18_early <- filter(UQL1_18, Date_time < "2018-07-01 1:00:01")
UQL1_18_late <- filter(UQL1_18, Date_time > "2018-12-31 23:50:01")
UQL1_18 <- bind_rows(UQL1_18_early,UQL1_18_late, UQL1_18_fix )



#30 cm 
################################################################
UQL1_18$WC_30cm[UQL1_18$WC_30cm == 0.7184] <- NA
missing <- which(is.na(UQL1_18$WC_30cm))

if(1 %in% missing){
  UQL1_18$WC_30cm[1] <- head(UQL1_18$WC_30cm[!is.na(UQL1_18$WC_30cm)],1)
}
if(nrow(UQL1_18) %in% missing){
  UQL1_18$WC_30cm[nrow(data)] <- tail(UQL1_18$WC_30cm[!is.na(UQL1_18$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_18$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_18$WC_30cm[idx] <- (UQL1_18$WC_30cm[r$starts[i]] + UQL1_18$WC_30cm[r$ends[i]])/2
}

#100 cm 
################################################################
UQL1_18$WC_100cm[UQL1_18$WC_100cm == -0.4737] <- NA


#Calibrate 
#==============================================================
UQL1_18_fix <- filter(UQL1_18, Date_time > "2018-07-01 1:00:01")
UQL1_18_fix <- filter(UQL1_18_fix, Date_time < "2018-12-31 23:50:01")

UQL1_18_fix$WC_100cm <- UQL1_18_fix$WC_100cm + 0.4982

#Recombine and then subset again
UQL1_18_early <- filter(UQL1_18, Date_time < "2018-07-01 1:00:01")
UQL1_18_late <- filter(UQL1_18, Date_time > "2018-12-31 23:50:01")
UQL1_18 <- bind_rows(UQL1_18_early,UQL1_18_late, UQL1_18_fix )

#Fix drips
#===========================================================
UQL1_18_fix <- filter(UQL1_18, Date_time > "2018-07-01 1:00:01")
UQL1_18_fix <- filter(UQL1_18_fix, Date_time < "2018-07-21 1:00:01")

UQL1_18_fix$WC_100cm[UQL1_18_fix$WC_100cm > 0.235] <- NA

#Recombine and then subset again
UQL1_18_early <- filter(UQL1_18, Date_time < "2018-07-01 1:00:01")
UQL1_18_late <- filter(UQL1_18, Date_time > "2018-07-21 1:00:01")
UQL1_18 <- bind_rows(UQL1_18_early,UQL1_18_late, UQL1_18_fix )

#Fix drips 
#========================================================================
UQL1_18_fix <- filter(UQL1_18, Date_time > "2018-09-23 1:00:01")
UQL1_18_fix <- filter(UQL1_18_fix, Date_time < "2018-11-10 1:00:01")

UQL1_18_fix$WC_100cm[UQL1_18_fix$WC_100cm < 0.2475] <- NA
missing <- which(is.na(UQL1_18_fix$WC_100cm))

if(1 %in% missing){
  UQL1_18_fix$WC_100cm[1] <- head(UQL1_18_fix$WC_100cm[!is.na(UQL1_18_fix$WC_100cm)],1)
}
if(nrow(UQL1_18_fix) %in% missing){
  UQL1_18_fix$WC_100cm[nrow(data)] <- tail(UQL1_18_fix$WC_100cm[!is.na(UQL1_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_18_fix$WC_100cm[idx] <- (UQL1_18_fix$WC_100cm[r$starts[i]] + UQL1_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and then subset again
UQL1_18_early <- filter(UQL1_18, Date_time < "2018-09-23 1:00:01")
UQL1_18_late <- filter(UQL1_18, Date_time > "2018-11-10 1:00:01")
UQL1_18 <- bind_rows(UQL1_18_early,UQL1_18_late, UQL1_18_fix )

#Fix drips 
#===========================================================
UQL1_18_fix <- filter(UQL1_18, Date_time > "2018-11-21 1:00:01")
UQL1_18_fix <- filter(UQL1_18_fix, Date_time < "2018-12-31 23:50:01")

UQL1_18_fix$WC_100cm <- UQL1_18_fix$WC_100cm + 0.01

#Recombine and then subset again
UQL1_18_early <- filter(UQL1_18, Date_time < "2018-11-21 1:00:01")
UQL1_18_late <- filter(UQL1_18, Date_time > "2018-12-31 23:50:01")
UQL1_18 <- bind_rows(UQL1_18_early,UQL1_18_late, UQL1_18_fix )

#Remove drips 
#======================================================================
UQL1_18_fix <- filter(UQL1_18, Date_time > "2018-11-13 1:00:01")
UQL1_18_fix <- filter(UQL1_18_fix, Date_time < "2018-11-30 23:50:01")

Soil <- ggplot(data = subset(UQL1_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) 
Soil 

UQL1_18_fix$WC_100cm[UQL1_18_fix$WC_100cm < 0.245] <- NA
missing <- which(is.na(UQL1_18_fix$WC_100cm))

if(1 %in% missing){
  UQL1_18_fix$WC_100cm[1] <- head(UQL1_18_fix$WC_100cm[!is.na(UQL1_18_fix$WC_100cm)],1)
}
if(nrow(UQL1_18_fix) %in% missing){
  UQL1_18_fix$WC_100cm[nrow(data)] <- tail(UQL1_18_fix$WC_100cm[!is.na(UQL1_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_18_fix$WC_100cm[idx] <- (UQL1_18_fix$WC_100cm[r$starts[i]] + UQL1_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine and then subset again
UQL1_18_early <- filter(UQL1_18, Date_time < "2018-11-13 1:00:01")
UQL1_18_late <- filter(UQL1_18, Date_time > "2018-11-30 23:50:01")
UQL1_18 <- bind_rows(UQL1_18_early,UQL1_18_late, UQL1_18_fix )

#Plot again 
Soil <- ggplot(data = subset(UQL1_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL1 2019
##################################################################################################
UQL1_19 <- subset(UQL1, Year == '2019')

#Plotting 
UQL1_19$WC_15cm <- as.numeric(UQL1_19$WC_15cm)
UQL1_19$WC_30cm <- as.numeric(UQL1_19$WC_30cm)
UQL1_19$WC_100cm <- as.numeric(UQL1_19$WC_100cm)
UQL1_19$Date_time<- mdy_hms(UQL1_19$Date_time)

#15 cm
###########################################
UQL1_19$WC_15cm[UQL1_19$WC_15cm < 0] <- NA
missing <- which(is.na(UQL1_19$WC_15cm))

if(1 %in% missing){
  UQL1_19$WC_15cm[1] <- head(UQL1_19$WC_15cm[!is.na(UQL1_19$WC_15cm)],1)
}
if(nrow(UQL1_19) %in% missing){
  UQL1_19$WC_15cm[nrow(data)] <- tail(UQL1_19$WC_15cm[!is.na(UQL1_19$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_19$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_19$WC_15cm[idx] <- (UQL1_19$WC_15cm[r$starts[i]] + UQL1_19$WC_15cm[r$ends[i]])/2
}

#30 cm 
################################################################
UQL1_19$WC_30cm[UQL1_19$WC_30cm > 0.35] <- NA
missing <- which(is.na(UQL1_19$WC_30cm))

if(1 %in% missing){
  UQL1_19$WC_30cm[1] <- head(UQL1_19$WC_30cm[!is.na(UQL1_19$WC_30cm)],1)
}
if(nrow(UQL1_19) %in% missing){
  UQL1_19$WC_30cm[nrow(data)] <- tail(UQL1_19$WC_30cm[!is.na(UQL1_19$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_19$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_19$WC_30cm[idx] <- (UQL1_19$WC_30cm[r$starts[i]] + UQL1_19$WC_30cm[r$ends[i]])/2
}

#100 cm 
################################################################
UQL1_19_fix <- UQL1_19[c(1:19093), ]



#Recombine
#=============================
#UQL1_19_late <- UQL1_19[c(19094:42338), ]

#UQL1_19 <- bind_rows(UQL1_19_fix, UQL1_19_late)

#Plot again 
Soil <- ggplot(data = subset(UQL1_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 


#UQL1 2020
##################################################################################################
UQL1_20 <- subset(UQL1, Year == '2020')

#Plotting 
UQL1_20$WC_15cm <- as.numeric(UQL1_20$WC_15cm)
UQL1_20$WC_30cm <- as.numeric(UQL1_20$WC_30cm)
UQL1_20$WC_100cm <- as.numeric(UQL1_20$WC_100cm)
UQL1_20$Date_time<- mdy_hms(UQL1_20$Date_time)

#15 cm
###########################################
UQL1_20$WC_15cm[UQL1_20$WC_15cm < 0] <- NA
missing <- which(is.na(UQL1_20$WC_15cm))

if(1 %in% missing){
  UQL1_20$WC_15cm[1] <- head(UQL1_20$WC_15cm[!is.na(UQL1_20$WC_15cm)],1)
}
if(nrow(UQL1_20) %in% missing){
  UQL1_20$WC_15cm[nrow(data)] <- tail(UQL1_20$WC_15cm[!is.na(UQL1_20$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20$WC_15cm[idx] <- (UQL1_20$WC_15cm[r$starts[i]] + UQL1_20$WC_15cm[r$ends[i]])/2
}
#Fix drips 
#===========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-09-10 1:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-10-01 1:00:01")

UQL1_20_fix$WC_15cm[UQL1_20_fix$WC_15cm < 0.207] <- NA
missing <- which(is.na(UQL1_20_fix$WC_15cm))

if(1 %in% missing){
  UQL1_20_fix$WC_15cm[1] <- head(UQL1_20_fix$WC_15cm[!is.na(UQL1_20_fix$WC_15cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_15cm[nrow(data)] <- tail(UQL1_20_fix$WC_15cm[!is.na(UQL1_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_15cm[idx] <- (UQL1_20_fix$WC_15cm[r$starts[i]] + UQL1_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-09-10 1:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-10-01 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#30 cm 
################################################################
UQL1_20$WC_30cm[UQL1_20$WC_30cm < 0.15] <- NA
missing <- which(is.na(UQL1_20$WC_30cm))

if(1 %in% missing){
  UQL1_20$WC_30cm[1] <- head(UQL1_20$WC_30cm[!is.na(UQL1_20$WC_30cm)],1)
}
if(nrow(UQL1_20) %in% missing){
  UQL1_20$WC_30cm[nrow(data)] <- tail(UQL1_20$WC_30cm[!is.na(UQL1_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20$WC_30cm[idx] <- (UQL1_20$WC_30cm[r$starts[i]] + UQL1_20$WC_30cm[r$ends[i]])/2
}

#Fix drips 
#===========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-05-17 1:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-06-04 1:00:01")

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.26] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-05-17 1:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-06-04 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Fix drips 
#===========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-05-18 12:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-06-01 1:00:01")

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.278] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-05-18 12:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-06-01 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Fix drips 
#===========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-05-18 12:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-05-22 1:00:01")

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.293] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-05-18 12:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-05-22 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Subset to fix September/October drips
#=============================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-09-10 12:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-10-07 1:00:01")

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.18] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-09-10 12:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-10-07 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Subset to fix September/October drips
#=============================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-09-10 12:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-09-18 1:00:01")

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.194] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-09-10 12:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-09-18 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Subset to fix earlier September drips 
#==========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-08-10 12:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-09-10 1:00:01")

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.192] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-08-10 12:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-09-10 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Subset to fix earlier May/June drips
#==========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-05-17 16:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-05-20 1:00:01")

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-05-17 16:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-05-20 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Subset to fix earlier May/June drips
#==========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-05-26 16:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-05-28 1:00:01")

Soil <- ggplot(data = subset(UQL1_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.284] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-05-26 16:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-05-28 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Subset to fix earlier May/June drips
#==========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-05-28 16:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-06-01 1:00:01")

Soil <- ggplot(data = subset(UQL1_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.28] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-05-28 16:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-06-01 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Subset to fix earlier May/June drips
#==========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-06-01 16:00:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-06-03 1:00:01")

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm < 0.274] <- NA
missing <- which(is.na(UQL1_20_fix$WC_30cm))

if(1 %in% missing){
  UQL1_20_fix$WC_30cm[1] <- head(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}
if(nrow(UQL1_20_fix) %in% missing){
  UQL1_20_fix$WC_30cm[nrow(data)] <- tail(UQL1_20_fix$WC_30cm[!is.na(UQL1_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_20_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_20_fix$WC_30cm[idx] <- (UQL1_20_fix$WC_30cm[r$starts[i]] + UQL1_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-06-01 16:00:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-06-03 1:00:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#Get rid of glitch (swoop) after missing dates in November
#==========================================================
UQL1_20_fix <- filter(UQL1_20, Date_time > "2020-11-02 00:30:01")
UQL1_20_fix <- filter(UQL1_20_fix, Date_time < "2020-11-13 03:30:01")

Soil <- ggplot(data = subset(UQL1_20_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

UQL1_20_fix$WC_30cm[UQL1_20_fix$WC_30cm > 0.15] <- NA

#Recombine and then subset again 
UQL1_20_early <- filter(UQL1_20, Date_time < "2020-11-02 00:30:01")
UQL1_20_late <- filter(UQL1_20, Date_time > "2020-11-13 03:30:01")
UQL1_20 <- bind_rows(UQL1_20_early,UQL1_20_late, UQL1_20_fix )

#100 cm 
################################################################
UQL1_20$WC_100cm[UQL1_20$WC_100cm == 0.7184] <- NA

#Replace missing dates with NAs
#==================================================================

#07/06 to 09/04
insertDF <- as.data.frame(matrix(data = NA, nrow = 61, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-07-06"), as.Date("2020-09-04"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

UQL1_20 <- insertRows(UQL1_20, c(26966:27026), new = insertDF)

#10/21 to 11/01
insertDF <- as.data.frame(matrix(data = NA, nrow = 11, ncol = 5))
colnames(insertDF) <- c("PAR", "WC_15cm","WC_30cm", "WC_100cm", "Year")
Date_time <- seq(as.Date("2020-10-22"), as.Date("2020-11-01"),"days")
Date <- as.data.frame(Date_time) 
insertDF <- cbind(Date, insertDF)

UQL1_20 <- insertRows(UQL1_20, c(33799:33809), new = insertDF)

#Plot again 
Soil <- ggplot(data = subset(UQL1_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL1 2021
##################################################################################################
UQL1_21 <- subset(UQL1, Year == '2021')

#Plotting 
UQL1_21$WC_15cm <- as.numeric(UQL1_21$WC_15cm)
UQL1_21$WC_30cm <- as.numeric(UQL1_21$WC_30cm)
UQL1_21$WC_100cm <- as.numeric(UQL1_21$WC_100cm)
UQL1_21$Date_time<- mdy_hms(UQL1_21$Date_time)

#15 cm
###########################################
UQL1_21$WC_15cm[UQL1_21$WC_15cm < 0] <- NA
missing <- which(is.na(UQL1_21$WC_15cm))

if(1 %in% missing){
  UQL1_21$WC_15cm[1] <- head(UQL1_21$WC_15cm[!is.na(UQL1_21$WC_15cm)],1)
}
if(nrow(UQL1_21) %in% missing){
  UQL1_21$WC_15cm[nrow(data)] <- tail(UQL1_21$WC_15cm[!is.na(UQL1_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21$WC_15cm[idx] <- (UQL1_21$WC_15cm[r$starts[i]] + UQL1_21$WC_15cm[r$ends[i]])/2
}

#30 cm 
################################################################
UQL1_21$WC_30cm[UQL1_21$WC_30cm < 0.2] <- NA
missing <- which(is.na(UQL1_21$WC_30cm))

if(1 %in% missing){
  UQL1_21$WC_30cm[1] <- head(UQL1_21$WC_30cm[!is.na(UQL1_21$WC_30cm)],1)
}
if(nrow(UQL1_21) %in% missing){
  UQL1_21$WC_30cm[nrow(data)] <- tail(UQL1_21$WC_30cm[!is.na(UQL1_21$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21$WC_30cm[idx] <- (UQL1_21$WC_30cm[r$starts[i]] + UQL1_21$WC_30cm[r$ends[i]])/2
}

#Try to fix extreme drips
#=======================================================
UQL1_21 <- UQL1_21 %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL1_21 <- transform(UQL1_21, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL1_21 <- transform(UQL1_21, WC_30cm=ifelse(incr < -0.01, 
                                             as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)), 
                                             WC_30cm))

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-20 12:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-20 1:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.31] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}


#Create a new column that is the percent difference between the row below and the row above 
UQL1_21_fix <- UQL1_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================


#Make increase column not a percent 
UQL1_21_fix <- transform(UQL1_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL1_21_fix <- transform(UQL1_21_fix, WC_30cm=ifelse(incr < -0.01, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)), 
                                                     WC_30cm))

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-20 12:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-20 1:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )


#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-13 12:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-02-16 03:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.34449] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}


Soil <- ggplot(data = subset(UQL1_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-13 12:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-02-16 03:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-06 01:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-12 01:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.37] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-06 01:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-12 01:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-06 01:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-07 01:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.38] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-06 01:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-07 01:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-09 01:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-12 01:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.389] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-09 01:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-12 01:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-09 18:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-10 20:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.399] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-09 18:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-10 20:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-15 09:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-16 10:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.374] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-15 09:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-16 10:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix)

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-21 00:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-26 10:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.36] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-15 09:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-16 10:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-21 13:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-22 00:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.37] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-21 13:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-22 00:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-22 13:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-23 00:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.367] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-22 13:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-23 00:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-23 00:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-03-25 00:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.373] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-23 00:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-03-25 00:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-13 00:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-02-15 00:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.372] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-13 00:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-02-15 00:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips (this one is the iffiest change)
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-16 00:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-02-18 00:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.37] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-16 00:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-02-18 00:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-18 00:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-02-18 12:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.372] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-18 00:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-02-18 12:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-18 12:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-02-19 00:00:01")

Soil <- ggplot(data = subset(UQL1_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.365] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-18 12:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-02-19 00:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-19 00:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-02-19 12:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.388] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-19 00:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-02-19 12:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-19 12:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-02-20 12:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.383] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-19 12:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-02-20 12:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-20 12:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-02-23 14:00:01")

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm < 0.378] <- NA
missing <- which(is.na(UQL1_21_fix$WC_30cm))

if(1 %in% missing){
  UQL1_21_fix$WC_30cm[1] <- head(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}
if(nrow(UQL1_21_fix) %in% missing){
  UQL1_21_fix$WC_30cm[nrow(data)] <- tail(UQL1_21_fix$WC_30cm[!is.na(UQL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21_fix$WC_30cm[idx] <- (UQL1_21_fix$WC_30cm[r$starts[i]] + UQL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-20 12:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-02-23 14:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Removed sustained dip 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-02-15 00:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-02-16 20:00:01")

Soil <- ggplot(data = subset(UQL1_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue"))
Soil 

UQL1_21_fix$WC_30cm[UQL1_21_fix$WC_30cm > 0.3] <- NA

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-02-15 00:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-02-16 20:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )

#Subset and fix drips 
#========================================================
UQL1_21_fix <- filter(UQL1_21, Date_time > "2021-03-10 12:00:01")
UQL1_21_fix <- filter(UQL1_21_fix, Date_time < "2021-04-20 1:00:01")

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================

UQL1_21_fix <- UQL1_21_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL1_21_fix <- transform(UQL1_21_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL1_21_fix <- transform(UQL1_21_fix, WC_30cm=ifelse(incr < -0.001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/24, 24), sides=2)), 
                                                     WC_30cm))

#Recombine and then subset again 
UQL1_21_early <- filter(UQL1_21, Date_time < "2021-03-10 12:00:01")
UQL1_21_late <- filter(UQL1_21, Date_time > "2021-04-20 1:00:01")
UQL1_21 <- bind_rows(UQL1_21_early,UQL1_21_late, UQL1_21_fix )


#100 cm 
################################################################
UQL1_21$WC_100cm[UQL1_21$WC_100cm < 0] <- NA
missing <- which(is.na(UQL1_19$WC_100cm))

if(1 %in% missing){
  UQL1_21$WC_100cm[1] <- head(UQL1_21$WC_100cm[!is.na(UQL1_21$WC_100cm)],1)
}
if(nrow(UQL1_21) %in% missing){
  UQL1_21$WC_100cm[nrow(data)] <- tail(UQL1_21$WC_100cm[!is.na(UQL1_21$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL1_21$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL1_21$WC_100cm[idx] <- (UQL1_21$WC_100cm[r$starts[i]] + UQL1_21$WC_100cm[r$ends[i]])/2
}

#Remove glitch at beginning of year 
#=========================================================
UQL1_21$WC_100cm[UQL1_21$WC_100cm == 0.7184] <- NA
UQL1_21$WC_100cm[UQL1_21$WC_100cm > 0.35] <- NA


#Plot again 
Soil <- ggplot(data = subset(UQL1_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Combine all of the WIL1 dataframes/years into 1 dataframe 

#Merge 2018 and 2019 
UQL1_clean <- merge(UQL1_18, UQL1_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
UQL1_clean <- merge(UQL1_clean, UQL1_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021

UQL1_clean <- merge(UQL1_clean, UQL1_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

UQL1_clean <- merge(UQL1_clean, UQL1_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph


Soil <- ggplot(data = subset(UQL1_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 6) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 6) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 6) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("UQL1", width = 4500, height = 2500)

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
write.csv(UQL1_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL1_clean.csv" ) #this writes a csv file and sends it to the working folder


