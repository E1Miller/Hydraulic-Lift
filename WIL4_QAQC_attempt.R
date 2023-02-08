#Created by: Elise Miller
#Date started: 10/25/2022
#Date last edited: 02/08/2023
#Description: QA/QC WIL 4

#Attach dependencies 
library(tidyverse)
library(dplyr)
library(plyr)
library(readr)
library(ggpubr)
library(tidyr)
library(purrr)
library(lubridate, warn.conflicts = FALSE)
library(googledrive)

setwd("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL")

#CREATING ONE FULL DATASET FOR WIL1 
#################################################################################################################################################
#2019-2021 FILES
#============================================================================================================
#Note that the 2019-2020 files had two separate columns for date and time, so they had to be worked with separately 

data19_21 <- list.files("~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL", 
                        pattern=glob2rx("W4M_*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Remove additional columns 
data19_21 <- data19_21[c(1:6)]

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
WIL4_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("W4M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
WIL4_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
WIL4 <- rbind(WIL4_2017_2019, WIL4_2019_2021)

#Write the csv
write.csv(WIL4,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL4.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(WIL4)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
WIL4$Date <- mdy_hms(WIL4$Date_time)

#Put year into a separate column 
WIL4 <- separate(WIL4, Date, c("Year"))

#WIL4 2017
##################################################################################################
WIL4_17 <- subset(WIL4, Year == '2017')

#Plotting 
WIL4_17$WC_15cm <- as.numeric(WIL4_17$WC_15cm)
WIL4_17$WC_30cm <- as.numeric(WIL4_17$WC_30cm)
WIL4_17$WC_100cm <- as.numeric(WIL4_17$WC_100cm)
WIL4_17$Date_time<- mdy_hms(WIL4_17$Date_time)

Soil <- ggplot(data = subset(WIL4_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL4 2018
##################################################################################################
WIL4_18 <- subset(WIL4, Year == '2018')

#Plotting 
WIL4_18$WC_15cm <- as.numeric(WIL4_18$WC_15cm)
WIL4_18$WC_30cm <- as.numeric(WIL4_18$WC_30cm)
WIL4_18$WC_100cm <- as.numeric(WIL4_18$WC_100cm)
WIL4_18$Date_time<- mdy_hms(WIL4_18$Date_time)

#15 cm 
########################################################################

#Calibrate 
#=======================================================================
WIL4_18_fix <- filter(WIL4_18, Date_time > "2018-08-22 10:00:01")
WIL4_18_fix <- filter(WIL4_18_fix, Date_time < "2018-09-25 09:20:01")

WIL4_18_fix$WC_15cm <- WIL4_18_fix$WC_15cm + 0.013

#Recombine 
WIL4_18_early <- filter(WIL4_18, Date_time < "2018-08-22 10:00:01")
WIL4_18_late <- filter(WIL4_18, Date_time > "2018-09-25 09:20:01")
WIL4_18 <- bind_rows(WIL4_18_early, WIL4_18_fix, WIL4_18_late)

#Calibrate 
#=======================================================================
WIL4_18_fix <- filter(WIL4_18, Date_time > "2018-11-13 11:10:01")

WIL4_18_fix$WC_15cm <- WIL4_18_fix$WC_15cm + 0.0064

#Recombine 
WIL4_18_early <- filter(WIL4_18, Date_time < "2018-11-13 11:10:01")
WIL4_18 <- bind_rows(WIL4_18_early, WIL4_18_fix)

#Fix drips
#==================================================================
WIL4_18_fix <- filter(WIL4_18, Date_time > "2018-08-22 10:00:01")
WIL4_18_fix <- filter(WIL4_18_fix, Date_time < "2018-09-25 09:20:01")

#Fix the drips some more using the increase
WIL4_18_fix <- WIL4_18_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================

#Make increase column not a percent 
WIL4_18_fix <- transform(WIL4_18_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

WIL4_18_fix <- transform(WIL4_18_fix, WC_15cm=ifelse(incr < -0.00001 | incr > 0.00001, 
                                                     as.numeric(stats::filter(WC_15cm, rep(1/2, 2), sides=2)), 
                                                     WC_15cm))
#Recombine 
WIL4_18_early <- filter(WIL4_18, Date_time < "2018-08-22 10:00:01")
WIL4_18_late <- filter(WIL4_18, Date_time > "2018-09-25 09:20:01")
WIL4_18 <- bind_rows(WIL4_18_early, WIL4_18_fix, WIL4_18_late)

#Fix drip
#======================================================================
WIL4_18_fix <- filter(WIL4_18, Date_time > "2018-08-20 1:00:01")
WIL4_18_fix <- filter(WIL4_18_fix, Date_time < "2018-09-02 01:00:01")

WIL4_18_fix$WC_15cm[WIL4_18_fix$WC_15cm > 0.195] <- NA
missing <- which(is.na(WIL4_18_fix$WC_15cm))

if(1 %in% missing){
  WIL4_18_fix$WC_15cm[1] <- head(WIL4_18_fix$WC_15cm[!is.na(WIL4_18_fix$WC_15cm)],1)
}
if(nrow(WIL4_18_fix) %in% missing){
  WIL4_18_fix$WC_15cm[nrow(data)] <- tail(WIL4_18_fix$WC_15cm[!is.na(WIL4_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_18_fix$WC_15cm[idx] <- (WIL4_18_fix$WC_15cm[r$starts[i]] + WIL4_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
WIL4_18_early <- filter(WIL4_18, Date_time < "2018-08-20 1:00:01")
WIL4_18_late <- filter(WIL4_18, Date_time > "2018-09-02 01:00:01")
WIL4_18 <- bind_rows(WIL4_18_early, WIL4_18_fix, WIL4_18_late)

#Fix drip
#======================================================================
WIL4_18_fix <- filter(WIL4_18, Date_time > "2018-11-10 1:00:01")
WIL4_18_fix <- filter(WIL4_18_fix, Date_time < "2018-11-20 01:00:01")

WIL4_18_fix$WC_15cm[WIL4_18_fix$WC_15cm < 0.151] <- NA
missing <- which(is.na(WIL4_18_fix$WC_15cm))

if(1 %in% missing){
  WIL4_18_fix$WC_15cm[1] <- head(WIL4_18_fix$WC_15cm[!is.na(WIL4_18_fix$WC_15cm)],1)
}
if(nrow(WIL4_18_fix) %in% missing){
  WIL4_18_fix$WC_15cm[nrow(data)] <- tail(WIL4_18_fix$WC_15cm[!is.na(WIL4_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_18_fix$WC_15cm[idx] <- (WIL4_18_fix$WC_15cm[r$starts[i]] + WIL4_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
WIL4_18_early <- filter(WIL4_18, Date_time < "2018-11-10 1:00:01")
WIL4_18_late <- filter(WIL4_18, Date_time > "2018-11-20 01:00:01")
WIL4_18 <- bind_rows(WIL4_18_early, WIL4_18_fix, WIL4_18_late)

#Plot again 
Soil <- ggplot(data = subset(WIL4_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm), color = "blue") +
  geom_line(aes(y = WC_15cm), color = "lightblue") +
  geom_line(aes(y = WC_100cm), color = "navyblue") +
  ylab(expression(paste("30 cm Water Content"))) + 
  xlab(expression("Date"))  
Soil 

#WIL4 2019
##################################################################################################
WIL4_19 <- subset(WIL4, Year == '2019')

#Plotting 
WIL4_19$WC_15cm <- as.numeric(WIL4_19$WC_15cm)
WIL4_19$WC_30cm <- as.numeric(WIL4_19$WC_30cm)
WIL4_19$WC_100cm <- as.numeric(WIL4_19$WC_100cm)
WIL4_19$Date_time<- mdy_hms(WIL4_19$Date_time)

#15 cm
############################################################################################
#Try to fix the 15 cm drips
#Fix the drips some more using the increase
WIL4_19_fix <- WIL4_19 %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_15cm-lag(WC_15cm),
    increase=scales::percent(diff / lag(WC_15cm))
  ) %>%
  filter(row_number()!=1)

#If the percent difference is greater than 20%, replace the value with the mean of the value above and below 
#=======================================================================

#Make increase column not a percent 
WIL4_19_fix <- transform(WIL4_19_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

WIL4_19_fix <- transform(WIL4_19_fix, WC_15cm=ifelse(incr < -0.0009, 
                                                     as.numeric(stats::filter(WC_15cm, rep(1/2, 2), sides=2)), 
                                                     WC_15cm))
WIL4_19 <- WIL4_19_fix

#Plot again
Soil <- ggplot(data = subset(WIL4_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL4 2020
##################################################################################################
WIL4_20 <- subset(WIL4, Year == '2020')

#Plotting 
WIL4_20$WC_15cm <- as.numeric(WIL4_20$WC_15cm)
WIL4_20$WC_30cm <- as.numeric(WIL4_20$WC_30cm)
WIL4_20$WC_100cm <- as.numeric(WIL4_20$WC_100cm)
WIL4_20$Date_time<- mdy_hms(WIL4_20$Date_time)

#30 cm 
################################################################
WIL4_20$WC_30cm[WIL4_20$WC_30cm < 0.28] <- NA
missing <- which(is.na(WIL4_20$WC_30cm))

if(1 %in% missing){
  WIL4_20$WC_30cm[1] <- head(WIL4_20$WC_30cm[!is.na(WIL4_20$WC_30cm)],1)
}
if(nrow(WIL4_20) %in% missing){
  WIL4_20$WC_30cm[nrow(data)] <- tail(WIL4_20$WC_30cm[!is.na(WIL4_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20$WC_30cm[idx] <- (WIL4_20$WC_30cm[r$starts[i]] + WIL4_20$WC_30cm[r$ends[i]])/2
}

#Fix 30 cm dips 
#=======================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-09-14 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-12-02 01:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.294] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

WIL4_20_early <- filter(WIL4_20, Date_time < "2020-09-14 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-12-02 01:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-09-02 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-09-15 01:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.305] <- NA

#Recombine 
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-09-02 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-09-15 01:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-09-17 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-09-25 01:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.3045 | WIL4_20_fix$WC_30cm > 0.3065] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-09-17 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-09-25 01:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-09-25 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-09-28 01:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.3035 | WIL4_20_fix$WC_30cm > 0.305] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-09-25 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-09-28 01:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-09-28 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-01 01:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.3025 | WIL4_20_fix$WC_30cm > 0.3045] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-09-28 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-01 01:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-01 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-06 19:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.301 | WIL4_20_fix$WC_30cm > 0.3037] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}
#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-01 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-03 19:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-01 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-03 19:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.30 | WIL4_20_fix$WC_30cm > 0.3037] <- NA

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-01 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-03 19:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-06 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-16 19:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.30075 | WIL4_20_fix$WC_30cm > 0.30135] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-06 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-16 19:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-16 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-18 19:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.3 | WIL4_20_fix$WC_30cm > 0.30148] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-16 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-18 19:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-18 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-21 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.298 | WIL4_20_fix$WC_30cm > 0.30148] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-18 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-21 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-21 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-23 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.2975] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-21 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-23 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-23 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-25 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.297 | WIL4_20_fix$WC_30cm > 0.3] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-23 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-25 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-25 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-28 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.2975 | WIL4_20_fix$WC_30cm > 0.299] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-25 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-28 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-28 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-31 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.2975 | WIL4_20_fix$WC_30cm > 0.299] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-28 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-31 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-31 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-03 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.296 | WIL4_20_fix$WC_30cm > 0.299] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-31 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-03 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-03 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-06 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.2955 | WIL4_20_fix$WC_30cm > 0.3] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-03 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-06 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-06 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-10 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.296 | WIL4_20_fix$WC_30cm > 0.298] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-06 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-10 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-10 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-14 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.2955 | WIL4_20_fix$WC_30cm > 0.2985] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-10 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-14 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-14 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-17 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.2955 | WIL4_20_fix$WC_30cm > 0.2985] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-14 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-17 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-17 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-20 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.2972 | WIL4_20_fix$WC_30cm > 0.3013] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-17 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-20 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-20 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-24 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.2995 | WIL4_20_fix$WC_30cm > 0.3045] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-20 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-24 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-20 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-22 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.3] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-20 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-22 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-22 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-23 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.30175 | WIL4_20_fix$WC_30cm > 0.3038] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-22 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-23 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-23 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-25 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.302 | WIL4_20_fix$WC_30cm > 0.306] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-23 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-25 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-25 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-28 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.302 | WIL4_20_fix$WC_30cm > 0.308] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-25 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-28 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-28 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-11-30 00:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.3025] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-28 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-11-30 00:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-11-30 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-12-01 10:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.301 | WIL4_20_fix$WC_30cm > 0.31] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-11-30 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-12-01 10:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-12-01 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-12-02 12:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.297] <- NA

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-12-01 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-12-02 12:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-12-02 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-12-05 12:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.292] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-12-02 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-12-05 12:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-09-14 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-09-21 12:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.3045 | WIL4_20_fix$WC_30cm > 0.307] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-09-14 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-09-21 12:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-02 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-06 12:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm > 0.30179 & WIL4_20_fix$WC_30cm < 0.30181] <- NA

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-02 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-06 12:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-04 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-06 12:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.3007 | WIL4_20_fix$WC_30cm > 0.303] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-04 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-06 12:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#Subset and remove drips starting in September
#=============================================================================================
WIL4_20_fix <- filter(WIL4_20, Date_time > "2020-10-19 1:00:01")
WIL4_20_fix <- filter(WIL4_20_fix, Date_time < "2020-10-31 12:00:01")

WIL4_20_fix$WC_30cm[WIL4_20_fix$WC_30cm < 0.2974 | WIL4_20_fix$WC_30cm > 0.3003] <- NA
missing <- which(is.na(WIL4_20_fix$WC_30cm))

if(1 %in% missing){
  WIL4_20_fix$WC_30cm[1] <- head(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}
if(nrow(WIL4_20_fix) %in% missing){
  WIL4_20_fix$WC_30cm[nrow(data)] <- tail(WIL4_20_fix$WC_30cm[!is.na(WIL4_20_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20_fix$WC_30cm[idx] <- (WIL4_20_fix$WC_30cm[r$starts[i]] + WIL4_20_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_20_early <- filter(WIL4_20, Date_time < "2020-10-19 1:00:01")
WIL4_20_late <- filter(WIL4_20, Date_time > "2020-10-31 12:00:01")
WIL4_20 <- bind_rows(WIL4_20_early, WIL4_20_fix, WIL4_20_late)

#100 cm 
########################################################################################
WIL4_20$WC_100cm[WIL4_20$WC_100cm < 0] <- NA
missing <- which(is.na(WIL4_20$WC_100cm))

if(1 %in% missing){
  WIL4_20$WC_100cm[1] <- head(WIL4_20$WC_100cm[!is.na(WIL4_20$WC_100cm)],1)
}
if(nrow(WIL4_20) %in% missing){
  WIL4_20$WC_100cm[nrow(data)] <- tail(WIL4_20$WC_100cm[!is.na(WIL4_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20$WC_100cm[idx] <- (WIL4_20$WC_100cm[r$starts[i]] + WIL4_20$WC_100cm[r$ends[i]])/2
}

#Get rid of weird increases 
#==========================================================================================
WIL4_20$WC_100cm[WIL4_20$WC_100cm > 0.4] <- NA
missing <- which(is.na(WIL4_20$WC_100cm))

if(1 %in% missing){
  WIL4_20$WC_100cm[1] <- head(WIL4_20$WC_100cm[!is.na(WIL4_20$WC_100cm)],1)
}
if(nrow(WIL4_20) %in% missing){
  WIL4_20$WC_100cm[nrow(data)] <- tail(WIL4_20$WC_100cm[!is.na(WIL4_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_20$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_20$WC_100cm[idx] <- (WIL4_20$WC_100cm[r$starts[i]] + WIL4_20$WC_100cm[r$ends[i]])/2
}

#Fix 100 cm 
#====================================================================
WIL4_20$WC_100cm[WIL4_20$WC_100cm == 0.7184] <- NA

#Fix 100 cm 
#====================================================================
WIL4_20$WC_100cm[WIL4_20$WC_100cm > 0.295549 & WIL4_20$WC_100cm < 0.295551] <- NA

#Fix 100 cm 
#====================================================================
WIL4_20$WC_100cm[WIL4_20$WC_100cm > 0.34839 & WIL4_20$WC_100cm < 0.34841] <- NA

#Fix 100 cm 
#====================================================================
WIL4_20$WC_100cm[WIL4_20$WC_100cm > 0.367349 & WIL4_20$WC_100cm < 0.367351] <- NA

#Plot again 
Soil <- ggplot(data = subset(WIL4_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL4 2021
##################################################################################################
WIL4_21 <- subset(WIL4, Year == '2021')

#Plotting 
WIL4_21$WC_15cm <- as.numeric(WIL4_21$WC_15cm)
WIL4_21$WC_30cm <- as.numeric(WIL4_21$WC_30cm)
WIL4_21$WC_100cm <- as.numeric(WIL4_21$WC_100cm)
WIL4_21$Date_time<- mdy_hms(WIL4_21$Date_time)

#15 cm 
##############################################################################

#Subset and remove drips
#=============================================================================================
WIL4_21_fix <- filter(WIL4_21, Date_time > "2021-04-17 1:00:01")
WIL4_21_fix <- filter(WIL4_21_fix, Date_time < "2021-04-30 12:00:01")

WIL4_21_fix$WC_15cm[WIL4_21_fix$WC_15cm < 0.284] <- NA
missing <- which(is.na(WIL4_21_fix$WC_15cm))

if(1 %in% missing){
  WIL4_21_fix$WC_15cm[1] <- head(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}
if(nrow(WIL4_21_fix) %in% missing){
  WIL4_21_fix$WC_15cm[nrow(data)] <- tail(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_21_fix$WC_15cm[idx] <- (WIL4_21_fix$WC_15cm[r$starts[i]] + WIL4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_21_early <- filter(WIL4_21, Date_time < "2021-04-17 1:00:01")
WIL4_21_late <- filter(WIL4_21, Date_time > "2021-04-30 12:00:01")
WIL4_21 <- bind_rows(WIL4_21_early, WIL4_21_fix, WIL4_21_late)

#Subset and remove drips
#=============================================================================================
WIL4_21_fix <- filter(WIL4_21, Date_time > "2021-04-30 1:00:01")
WIL4_21_fix <- filter(WIL4_21_fix, Date_time < "2021-05-02 12:00:01")

WIL4_21_fix$WC_15cm[WIL4_21_fix$WC_15cm < 0.286] <- NA
missing <- which(is.na(WIL4_21_fix$WC_15cm))

if(1 %in% missing){
  WIL4_21_fix$WC_15cm[1] <- head(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}
if(nrow(WIL4_21_fix) %in% missing){
  WIL4_21_fix$WC_15cm[nrow(data)] <- tail(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_21_fix$WC_15cm[idx] <- (WIL4_21_fix$WC_15cm[r$starts[i]] + WIL4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_21_early <- filter(WIL4_21, Date_time < "2021-04-30 1:00:01")
WIL4_21_late <- filter(WIL4_21, Date_time > "2021-05-02 12:00:01")
WIL4_21 <- bind_rows(WIL4_21_early, WIL4_21_fix, WIL4_21_late)

#Subset and remove drips
#=============================================================================================
WIL4_21_fix <- filter(WIL4_21, Date_time > "2021-05-24 1:00:01")
WIL4_21_fix <- filter(WIL4_21_fix, Date_time < "2021-05-26 12:00:01")

WIL4_21_fix$WC_15cm[WIL4_21_fix$WC_15cm < 0.263] <- NA
missing <- which(is.na(WIL4_21_fix$WC_15cm))

if(1 %in% missing){
  WIL4_21_fix$WC_15cm[1] <- head(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}
if(nrow(WIL4_21_fix) %in% missing){
  WIL4_21_fix$WC_15cm[nrow(data)] <- tail(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_21_fix$WC_15cm[idx] <- (WIL4_21_fix$WC_15cm[r$starts[i]] + WIL4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_21_early <- filter(WIL4_21, Date_time < "2021-05-24 1:00:01")
WIL4_21_late <- filter(WIL4_21, Date_time > "2021-05-26 12:00:01")
WIL4_21 <- bind_rows(WIL4_21_early, WIL4_21_fix, WIL4_21_late)

#Subset and remove drips
#=============================================================================================
WIL4_21_fix <- filter(WIL4_21, Date_time > "2021-06-10 1:00:01")
WIL4_21_fix <- filter(WIL4_21_fix, Date_time < "2021-06-15 12:00:01")

WIL4_21_fix$WC_15cm[WIL4_21_fix$WC_15cm < 0.24] <- NA
missing <- which(is.na(WIL4_21_fix$WC_15cm))

if(1 %in% missing){
  WIL4_21_fix$WC_15cm[1] <- head(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}
if(nrow(WIL4_21_fix) %in% missing){
  WIL4_21_fix$WC_15cm[nrow(data)] <- tail(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_21_fix$WC_15cm[idx] <- (WIL4_21_fix$WC_15cm[r$starts[i]] + WIL4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_21_early <- filter(WIL4_21, Date_time < "2021-06-10 1:00:01")
WIL4_21_late <- filter(WIL4_21, Date_time > "2021-06-15 12:00:01")
WIL4_21 <- bind_rows(WIL4_21_early, WIL4_21_fix, WIL4_21_late)

#Subset and remove drips
#=============================================================================================
WIL4_21_fix <- filter(WIL4_21, Date_time > "2021-06-28 1:00:01")
WIL4_21_fix <- filter(WIL4_21_fix, Date_time < "2021-07-01 12:00:01")

WIL4_21_fix$WC_15cm[WIL4_21_fix$WC_15cm < 0.2287] <- NA
missing <- which(is.na(WIL4_21_fix$WC_15cm))

if(1 %in% missing){
  WIL4_21_fix$WC_15cm[1] <- head(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}
if(nrow(WIL4_21_fix) %in% missing){
  WIL4_21_fix$WC_15cm[nrow(data)] <- tail(WIL4_21_fix$WC_15cm[!is.na(WIL4_21_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL4_21_fix$WC_15cm[idx] <- (WIL4_21_fix$WC_15cm[r$starts[i]] + WIL4_21_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
#============================================================================
WIL4_21_early <- filter(WIL4_21, Date_time < "2021-06-28 1:00:01")
WIL4_21_late <- filter(WIL4_21, Date_time > "2021-07-01 12:00:01")
WIL4_21 <- bind_rows(WIL4_21_early, WIL4_21_fix, WIL4_21_late)

#Calibrate 
#==============================================================================
WIL4_21_fix <- filter(WIL4_21, Date_time > "2021-07-08 05:10:01")
WIL4_21_fix <- filter(WIL4_21_fix, Date_time < "2021-07-27 10:30:01")

WIL4_21_fix$WC_15cm <- WIL4_21_fix$WC_15cm + 0.0044

#Recombine 
#============================================================================
WIL4_21_early <- filter(WIL4_21, Date_time < "2021-07-08 05:10:01")
WIL4_21_late <- filter(WIL4_21, Date_time > "2021-07-27 10:30:01")
WIL4_21 <- bind_rows(WIL4_21_early, WIL4_21_fix, WIL4_21_late)

#30 cm 
######################################################################

#Remove drip in beginning of year 
#=========================================================================
WIL4_21_fix <- filter(WIL4_21, Date_time > "2021-01-08 1:00:01")
WIL4_21_fix <- filter(WIL4_21_fix, Date_time < "2021-01-18 12:00:01")

WIL4_21_fix$WC_30cm[WIL4_21_fix$WC_30cm < 0.29] <- NA

#Recombine 
#============================================================================
WIL4_21_early <- filter(WIL4_21, Date_time < "2021-01-08 1:00:01")
WIL4_21_late <- filter(WIL4_21, Date_time > "2021-01-11 12:00:01")
WIL4_21 <- bind_rows(WIL4_21_early, WIL4_21_fix, WIL4_21_late)

#Plot again 
Soil <- ggplot(data = subset(WIL4_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
WIL4_clean <- merge(WIL4_18, WIL4_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
WIL4_clean <- merge(WIL4_clean, WIL4_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
WIL4_clean <- merge(WIL4_clean, WIL4_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
WIL4_clean <- merge(WIL4_clean, WIL4_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Graph

Soil <- ggplot(data = subset(WIL4_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4 ) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date"))  
Soil

png("WIL4_Salli", width = 4500, height = 2500)

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
write.csv(WIL4_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL4_clean.csv")


