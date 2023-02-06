#Created by: Elise Miller
#Date started: 10/26/2022
#Date last edited: 02/05/2022
#Description: QA/QC UQL 4

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
                        pattern=glob2rx("Copy of U4M_HY*.csv")) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data19_21                                            # Print data to RStudio console

#Merge date and time columns into a date_time column 
data19_21$Date_time <- paste(data19_21$Date, data19_21$`Time, GMT-08:00`)

#Rename the columns 
UQL4_2019_2021 <- data19_21 %>%
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
files <- dir(data_path, pattern=glob2rx("Copy of U4M1*.csv")) 
data17_19 <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) 

#Rename the columns so that they can be rbinded together
data17_19 <- lapply(data17_19, setNames, nm = new_col_name)

#Bind the rows together 
UQL4_2017_2019 <- data17_19 %>% bind_rows()

#MERGING AND SAVING THE TWO SEPARATE DATASETS FOR WIL1
#===============================================================================================================
UQL4 <- rbind(UQL4_2017_2019, UQL4_2019_2021)

#Write the csv
write.csv(UQL4,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL4.csv" ) #this writes a csv file and sends it to the working folder

#QA/QC FOR WIL1
###############################################################################################################
attach(UQL4)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Turn the Date_time column into a date-time column (POSICX)
UQL4$Date <- mdy_hms(UQL4$Date_time)

#Put year into a separate column 
UQL4 <- separate(UQL4, Date, c("Year"))

#UQL4 2017
##################################################################################################
UQL4_17 <- subset(UQL4, Year == '2017')

#Plotting 
UQL4_17$WC_15cm <- as.numeric(UQL4_17$WC_15cm)
UQL4_17$WC_30cm <- as.numeric(UQL4_17$WC_30cm)
UQL4_17$WC_100cm <- as.numeric(UQL4_17$WC_100cm)
UQL4_17$Date_time<- mdy_hms(UQL4_17$Date_time)

Soil <- ggplot(data = subset(UQL4_17, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 
##################################################################################################

#UQL4 2018
##################################################################################################
UQL4_18 <- subset(UQL4, Year == '2018')

#Plotting 
UQL4_18$WC_15cm <- as.numeric(UQL4_18$WC_15cm)
UQL4_18$WC_30cm <- as.numeric(UQL4_18$WC_30cm)
UQL4_18$WC_100cm <- as.numeric(UQL4_18$WC_100cm)
UQL4_18$Date_time<- mdy_hms(UQL4_18$Date_time)

#15 cm 
####################################################################################

#Calibrate
#========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-08-02 08:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-08-29 02:00:01")

UQL4_18_fix$WC_15cm <- UQL4_18_fix$WC_15cm + 0.032

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-08-02 08:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-08-29 02:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Calibrate
#========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-08-29 01:50:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-12-31 23:50:01")

UQL4_18_fix$WC_15cm <- UQL4_18_fix$WC_15cm + 0.0192

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-08-29 01:50:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-12-31 23:50:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Calibrate
#========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-08-07 10:30:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-12-31 23:50:01")

UQL4_18_fix$WC_15cm <- UQL4_18_fix$WC_15cm + 0.0074

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-08-07 10:30:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-12-31 23:50:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Remove drips
#========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-07-27 10:30:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-08-10 23:50:01")

UQL4_18_fix$WC_15cm[UQL4_18_fix$WC_15cm < 0.166] <- NA
missing <- which(is.na(UQL4_18_fix$WC_15cm))

if(1 %in% missing){
  UQL4_18_fix$WC_15cm[1] <- head(UQL4_18_fix$WC_15cm[!is.na(UQL4_18_fix$WC_15cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_15cm[nrow(data)] <- tail(UQL4_18_fix$WC_15cm[!is.na(UQL4_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_15cm[idx] <- (UQL4_18_fix$WC_15cm[r$starts[i]] + UQL4_18_fix$WC_15cm[r$ends[i]])/2
}


#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-07-27 10:30:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-08-10 23:50:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#30 cm 
#################################################################################
UQL4_18$WC_30cm[UQL4_18$WC_30cm < 0.1] <- NA
missing <- which(is.na(UQL4_18$WC_30cm))

if(1 %in% missing){
  UQL4_18$WC_30cm[1] <- head(UQL4_18$WC_30cm[!is.na(UQL4_18$WC_30cm)],1)
}
if(nrow(UQL4_18) %in% missing){
  UQL4_18$WC_30cm[nrow(data)] <- tail(UQL4_18$WC_30cm[!is.na(UQL4_18$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18$WC_30cm[idx] <- (UQL4_18$WC_30cm[r$starts[i]] + UQL4_18$WC_30cm[r$ends[i]])/2
}

#Get rid of 30 cm dips
#=========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-01 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-11 1:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.21] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL4_18_later <- filter(UQL4_18, Date_time < "2018-06-01 1:00:01")
UQL4_18_end <- filter(UQL4_18, Date_time  > "2018-06-11 1:00:01")
UQL4_18 <- bind_rows(UQL4_18_fix, UQL4_18_later, UQL4_18_end)

#Subset again and try to get rid of dips 
#========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-01 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-13 03:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.154] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Get rid of increases
UQL4_18_fix <- UQL4_18_fix %>% 
  arrange(Date_time) %>% 
  mutate(
    diff=WC_30cm-lag(WC_30cm),
    increase=scales::percent(diff / lag(WC_30cm))
  ) %>%
  filter(row_number()!=1)
#Make increase column not a percent 
UQL4_18_fix <- transform(UQL4_18_fix, incr=as.numeric(gsub('\\%', '', increase))/100)

UQL4_18_fix <- transform(UQL4_18_fix, WC_30cm=ifelse(incr < -0.001, 
                                                     as.numeric(stats::filter(WC_30cm, rep(1/15, 15), sides=2)),
                                                     WC_30cm))

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-01 1:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-07-13 03:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again and try to get rid of dips 
#========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-01 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-24 03:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.177] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}


#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-01 1:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-24 03:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again and try to get rid of dips 
#========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-05 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-20 03:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.193] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-05 1:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-20 03:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-07 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-12 03:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.207] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-07 1:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-12 03:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-07 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-09 20:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.212] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-07 1:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-09 20:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-09 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-10 20:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.21 | UQL4_18_fix$WC_30cm > 0.2162] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-09 1:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-10 20:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-12 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-14 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.20245 | UQL4_18_fix$WC_30cm > 0.212] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-12 1:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-14 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-14 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-16 12:00:01")


UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.198 | UQL4_18_fix$WC_30cm > 0.2055] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-14 1:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-16 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-15 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-17 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.1978 | UQL4_18_fix$WC_30cm > 0.2025] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-15 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-17 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-16 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-18 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.1958] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-16 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-18 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-17 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-19 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.1948 | UQL4_18_fix$WC_30cm > 0.2015] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-17 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-19 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-18 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-20 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.193 | UQL4_18_fix$WC_30cm > 0.1986] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-18 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-20 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-19 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-21 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.1935 | UQL4_18_fix$WC_30cm > 0.1975] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-19 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-21 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-21 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-23 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm > 0.195] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-21 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-23 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-23 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-26 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.18 | UQL4_18_fix$WC_30cm > 0.19] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-23 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-26 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-26 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-28 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.18 | UQL4_18_fix$WC_30cm > 0.184] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-26 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-28 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-27 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-06-29 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.177] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-27 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-06-29 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-29 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-01 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.1725 | UQL4_18_fix$WC_30cm > 0.18] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-29 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-07-01 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-06-30 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-02 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.171 | UQL4_18_fix$WC_30cm > 0.176] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-06-30 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-07-02 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-07-01 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-03 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm > 0.175] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-07-01 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-07-03 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-07-02 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-04 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.167 | UQL4_18_fix$WC_30cm > 0.1715] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-07-02 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-07-04 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-07-03 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-05 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.166] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-07-03 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-07-05 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-07-04 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-06 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.165] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-07-04 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-07-06 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-07-05 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-07 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.165 | UQL4_18_fix$WC_30cm > 0.169] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-07-05 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-07-07 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset again 
#===========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-07-09 15:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-10 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm < 0.16 | UQL4_18_fix$WC_30cm > 0.165] <- NA
missing <- which(is.na(UQL4_18_fix$WC_30cm))

if(1 %in% missing){
  UQL4_18_fix$WC_30cm[1] <- head(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_30cm[nrow(data)] <- tail(UQL4_18_fix$WC_30cm[!is.na(UQL4_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_30cm[idx] <- (UQL4_18_fix$WC_30cm[r$starts[i]] + UQL4_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-07-09 15:00:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-07-10 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)

#Subset and remove glitch in July through August
#===========================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-07-12 23:50:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-08-08 12:00:01")

UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm > 0.1] <- NA

#Recombine and subset again 
UQL4_18_early <- filter(UQL4_18, Date_time < "2018-07-12 23:50:01")
UQL4_18_late <- filter(UQL4_18, Date_time > "2018-08-08 12:00:01")
UQL4_18 <- bind_rows(UQL4_18_early, UQL4_18_late, UQL4_18_fix)


#100 cm 
################################################################
UQL4_18$WC_100cm[UQL4_18$WC_100cm < 0.2] <- NA
missing <- which(is.na(UQL4_18$WC_100cm))

if(1 %in% missing){
  UQL4_18$WC_100cm[1] <- head(UQL4_18$WC_100cm[!is.na(UQL4_18$WC_100cm)],1)
}
if(nrow(UQL4_18) %in% missing){
  UQL4_18$WC_100cm[nrow(data)] <- tail(UQL4_18$WC_100cm[!is.na(UQL4_18$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18$WC_100cm[idx] <- (UQL4_18$WC_100cm[r$starts[i]] + UQL4_18$WC_100cm[r$ends[i]])/2
}


#Fix 100 cm drips 
#===============================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-08-31 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-09-26 1:00:01")

UQL4_18_fix$WC_100cm[UQL4_18_fix$WC_100cm < 0.215] <- NA
missing <- which(is.na(UQL4_18_fix$WC_100cm))

if(1 %in% missing){
  UQL4_18_fix$WC_100cm[1] <- head(UQL4_18_fix$WC_100cm[!is.na(UQL4_18_fix$WC_100cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_100cm[nrow(data)] <- tail(UQL4_18_fix$WC_100cm[!is.na(UQL4_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_100cm[idx] <- (UQL4_18_fix$WC_100cm[r$starts[i]] + UQL4_18_fix$WC_100cm[r$ends[i]])/2
}


#Recombine July with other dataset 
UQL4_18_later <- filter(UQL4_18, Date_time < "2018-08-31 1:00:01")
UQL4_18_end <- filter(UQL4_18, Date_time  > "2018-09-26 1:00:01")
UQL4_18_new <- bind_rows(UQL4_18_fix, UQL4_18_later, UQL4_18_end)

#Get rid of drips again 
#=============================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-09-10 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-09-22 1:00:01")

UQL4_18_fix$WC_100cm[UQL4_18_fix$WC_100cm < 0.216] <- NA
missing <- which(is.na(UQL4_18_fix$WC_100cm))

if(1 %in% missing){
  UQL4_18_fix$WC_100cm[1] <- head(UQL4_18_fix$WC_100cm[!is.na(UQL4_18_fix$WC_100cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_100cm[nrow(data)] <- tail(UQL4_18_fix$WC_100cm[!is.na(UQL4_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_100cm[idx] <- (UQL4_18_fix$WC_100cm[r$starts[i]] + UQL4_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL4_18_later <- filter(UQL4_18, Date_time < "2018-09-10 1:00:01")
UQL4_18_end <- filter(UQL4_18, Date_time  > "2018-09-22 1:00:01")
UQL4_18 <- bind_rows(UQL4_18_fix, UQL4_18_later, UQL4_18_end)

#Get rid of drips again 
#=============================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-07-10 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-07-18 11:00:01")

UQL4_18_fix$WC_100cm[UQL4_18_fix$WC_100cm < 0.2550] <- NA
missing <- which(is.na(UQL4_18_fix$WC_100cm))

if(1 %in% missing){
  UQL4_18_fix$WC_100cm[1] <- head(UQL4_18_fix$WC_100cm[!is.na(UQL4_18_fix$WC_100cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_100cm[nrow(data)] <- tail(UQL4_18_fix$WC_100cm[!is.na(UQL4_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_100cm[idx] <- (UQL4_18_fix$WC_100cm[r$starts[i]] + UQL4_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL4_18_later <- filter(UQL4_18, Date_time < "2018-07-10 1:00:01")
UQL4_18_end <- filter(UQL4_18, Date_time  > "2018-07-18 11:00:01")
UQL4_18 <- bind_rows(UQL4_18_fix, UQL4_18_later, UQL4_18_end)

#Get rid of drips again 
#=============================================================================
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-09-12 1:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-09-20 11:00:01")

Soil <- ggplot(data = subset(UQL4_18_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue"))
Soil 

UQL4_18_fix$WC_100cm[UQL4_18_fix$WC_100cm < 0.218] <- NA
missing <- which(is.na(UQL4_18_fix$WC_100cm))

if(1 %in% missing){
  UQL4_18_fix$WC_100cm[1] <- head(UQL4_18_fix$WC_100cm[!is.na(UQL4_18_fix$WC_100cm)],1)
}
if(nrow(UQL4_18_fix) %in% missing){
  UQL4_18_fix$WC_100cm[nrow(data)] <- tail(UQL4_18_fix$WC_100cm[!is.na(UQL4_18_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_18_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_18_fix$WC_100cm[idx] <- (UQL4_18_fix$WC_100cm[r$starts[i]] + UQL4_18_fix$WC_100cm[r$ends[i]])/2
}

#Recombine July with other dataset 
UQL4_18_later <- filter(UQL4_18, Date_time < "2018-09-12 1:00:01")
UQL4_18_end <- filter(UQL4_18, Date_time  > "2018-09-20 11:00:01")
UQL4_18 <- bind_rows(UQL4_18_fix, UQL4_18_later, UQL4_18_end)

#Remove glitches
#################################################################################
UQL4_18_fix <- filter(UQL4_18, Date_time > "2018-09-26 22:00:01")
UQL4_18_fix <- filter(UQL4_18_fix, Date_time < "2018-10-23 14:00:01")

UQL4_18_fix$WC_100cm[UQL4_18_fix$WC_100cm > 0] <- NA
UQL4_18_fix$WC_30cm[UQL4_18_fix$WC_30cm > 0] <- NA
UQL4_18_fix$WC_15cm[UQL4_18_fix$WC_15cm > 0] <- NA

#Recombine July with other dataset 
UQL4_18_later <- filter(UQL4_18, Date_time < "2018-09-26 22:00:01")
UQL4_18_end <- filter(UQL4_18, Date_time  > "2018-10-23 14:00:01")
UQL4_18 <- bind_rows(UQL4_18_fix, UQL4_18_later, UQL4_18_end)

#Plot again 
Soil <- ggplot(data = subset(UQL4_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL4 2019
##################################################################################################
UQL4_19 <- subset(UQL4, Year == '2019')

#Plotting 
UQL4_19$WC_15cm <- as.numeric(UQL4_19$WC_15cm)
UQL4_19$WC_30cm <- as.numeric(UQL4_19$WC_30cm)
UQL4_19$WC_100cm <- as.numeric(UQL4_19$WC_100cm)
UQL4_19$Date_time<- mdy_hms(UQL4_19$Date_time)

#15 cm 
#################################################################################

#Remove drips
#=============================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-05-10 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-05-20 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.23] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time <  "2019-05-10 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-05-20 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove drips (this one led to some gaps)
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-20 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-24 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.127] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time <  "2019-08-20 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-24 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove drips
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-20 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-09-24 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.117] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time <  "2019-08-20 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-09-24 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove drips
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-09-17 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-09-24 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.1197] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time <  "2019-09-17 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-09-24 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove drips
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-09-24 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-09-29 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.127] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time <  "2019-09-24 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-09-29 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-10-14 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-10-17 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.110] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-10-14 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-10-17 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-10-16 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-10-20 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm > 0.120] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-10-16 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-10-20 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-10-22 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-10-24 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.116] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-10-22 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-10-24 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-10-24 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-11-24 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.105 | UQL4_19_fix$WC_15cm > 0.117] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-10-24 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-11-24 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-11-18 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-11-24 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm > 0.111 | UQL4_19_fix$WC_15cm < 0.107] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL4_19_early <- filter(UQL4_19, Date_time < "2019-11-18 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-11-24 00:00:01")
UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-09-28 11:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-10-03 00:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.125 | UQL4_19_fix$WC_15cm > 0.13] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL4_19_early <- filter(UQL4_19, Date_time < "2019-09-28 11:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-10-03 00:00:01")
UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-10-14 11:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-10-21 08:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.1158 | UQL4_19_fix$WC_15cm > 0.1170] <- NA

#Recombine
UQL4_19_early <- filter(UQL4_19, Date_time < "2019-10-14 11:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-10-21 08:00:01")
UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-05 11:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-12 08:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.1375] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}
#Recombine
UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-05 11:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-12 08:00:01")
UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-05-14 11:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-05-16 08:00:01")

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.232] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL4_19_early <- filter(UQL4_19, Date_time < "2019-05-14 11:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-05-16 08:00:01")
UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and remove increases
#=========================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-11-25 11:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-12-16 08:00:01")

Soil <- ggplot(data = subset(UQL4_19_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

UQL4_19_fix$WC_15cm[UQL4_19_fix$WC_15cm < 0.104] <- NA
missing <- which(is.na(UQL4_19_fix$WC_15cm))

if(1 %in% missing){
  UQL4_19_fix$WC_15cm[1] <- head(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_15cm[nrow(data)] <- tail(UQL4_19_fix$WC_15cm[!is.na(UQL4_19_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_15cm[idx] <- (UQL4_19_fix$WC_15cm[r$starts[i]] + UQL4_19_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL4_19_early <- filter(UQL4_19, Date_time < "2019-11-25 11:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-12-16 08:00:01")
UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#100 cm 
#################################################################################
UQL4_19$WC_100cm[UQL4_19$WC_100cm < 0.22] <- NA
missing <- which(is.na(UQL4_19$WC_100cm))

if(1 %in% missing){
  UQL4_19$WC_100cm[1] <- head(UQL4_19$WC_100cm[!is.na(UQL4_19$WC_100cm)],1)
}
if(nrow(UQL4_19) %in% missing){
  UQL4_19$WC_100cm[nrow(data)] <- tail(UQL4_19$WC_100cm[!is.na(UQL4_19$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19$WC_100cm[idx] <- (UQL4_19$WC_100cm[r$starts[i]] + UQL4_19$WC_100cm[r$ends[i]])/2
}

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-03-29 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-03-30 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.385] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-03-29 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-03-30 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-03-31 1:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-04-06 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.331] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-03-31 1:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-04-06 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-04-06 12:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-04-07 22:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.3855] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-04-06 12:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-04-07 22:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-04-11 12:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-04-28 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.327] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-04-11 12:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-04-28 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-04-28 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-05-10 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.312] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-04-28 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-05-10 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-05-27 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-10 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.326] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-05-27 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-10 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-10 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-25 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.30] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-10 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-25 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-25 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-15 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.28] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-25 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-15 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-01 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-15 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.315] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-01 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-15 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-25 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-15 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.28] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-25 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-15 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-01 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-15 00:00:01")


UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.315] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-01 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-15 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-15 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-30 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.267] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-15 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-30 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-30 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-12 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.255] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-30 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-12 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-12 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-30 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.245] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-12 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-30 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-30 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-09-30 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.225] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-30 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-09-30 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-01-21 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-03-30 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.325] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-01-21 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-03-30 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-03-01 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-03-07 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.375] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-03-01 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-03-07 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-03-30 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-03-31 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.356] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-03-30 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-03-31 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-03-30 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-03-30 14:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.369] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-03-30 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-03-30 14:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-03-31 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-03-31 12:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.3425] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-03-31 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-03-31 12:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-04-05 18:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-04-07 12:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.39] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-04-05 18:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-04-07 12:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-04-12 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-04-15 12:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.333] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-04-12 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-04-15 12:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-04-18 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-04-23 12:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.330] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-04-18 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-04-23 12:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-04-18 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-04-20 12:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.332] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-04-18 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-04-20 12:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-04-27 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-04-30 12:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.326] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-04-27 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-04-30 12:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-05-29 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-05-31 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.334] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-05-29 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-05-31 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-05-31 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-03 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.332] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-05-31 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-03 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-05 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-07 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.329] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-05 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-07 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-10 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-11 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.325] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-10 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-11 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-11 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-13 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.321] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-11 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-13 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-13 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-15 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.32] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-13 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-15 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-16 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-18 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.316] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-16 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-18 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-18 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-20 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.3125] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-18 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-20 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-23 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-25 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.3050] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-23 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-25 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-25 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-06-27 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.302] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-25 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-06-27 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-06-29 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-02 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.295] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-06-29 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-02 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-05 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-07 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.2892] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-05 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-07 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-08 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-10 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.287] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-08 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-10 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-10 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-13 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.2849] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-10 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-13 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-16 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-22 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.275] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-16 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-22 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-16 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-19 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.278] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-16 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-19 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-16 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-17 20:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.279] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-16 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-17 20:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-18 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-19 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.278] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-18 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-19 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-19 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-21 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.276] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-19 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-21 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-21 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-24 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.2725] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-21 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-24 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-01 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-05 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.261] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-01 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-05 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-04 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-07 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.26] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-04 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-07 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-05 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-06 12:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.2605] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-05 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-06 12:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-08 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-09 12:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.2584] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-08 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-09 12:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-09 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-11 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.2574] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-09 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-11 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-13 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-14 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.254] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-13 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-14 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-14 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-19 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.2508] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-14 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-19 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-08-19 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-08-22 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.249] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-08-19 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-08-22 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-09-01 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-09-07 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.24] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-09-01 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-09-07 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-09-12 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-09-14 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.2365] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-09-12 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-09-14 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-09-14 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-09-24 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.234] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-09-14 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-09-24 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-10 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-15 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.2825] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-10 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-15 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Subset and get rid of dips
#==================================================================================
UQL4_19_fix <- filter(UQL4_19, Date_time > "2019-07-15 00:00:01")
UQL4_19_fix <- filter(UQL4_19_fix, Date_time < "2019-07-17 00:00:01")

UQL4_19_fix$WC_100cm[UQL4_19_fix$WC_100cm < 0.279] <- NA
missing <- which(is.na(UQL4_19_fix$WC_100cm))

if(1 %in% missing){
  UQL4_19_fix$WC_100cm[1] <- head(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}
if(nrow(UQL4_19_fix) %in% missing){
  UQL4_19_fix$WC_100cm[nrow(data)] <- tail(UQL4_19_fix$WC_100cm[!is.na(UQL4_19_fix$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_19_fix$WC_100cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_19_fix$WC_100cm[idx] <- (UQL4_19_fix$WC_100cm[r$starts[i]] + UQL4_19_fix$WC_100cm[r$ends[i]])/2
}

UQL4_19_early <- filter(UQL4_19, Date_time < "2019-07-15 00:00:01")
UQL4_19_late <- filter(UQL4_19, Date_time > "2019-07-17 00:00:01")

UQL4_19 <- bind_rows(UQL4_19_late, UQL4_19_early, UQL4_19_fix)

#Remove glitch in 100 cm 
#====================================================================================
UQL4_19$WC_100cm[UQL4_19$WC_100cm > 0.232049 & UQL4_19$WC_100cm < 0.232051] <- NA

#Remove glitch in 100 cm at end of the year
#====================================================================================
UQL4_19$WC_100cm[UQL4_19$WC_100cm == 0.2291] <- NA

#Remove glitch in 100 cm at end of the year
#====================================================================================
UQL4_19$WC_100cm[UQL4_19$WC_100cm == 0.2319] <- NA

#Plot again 
Soil <- ggplot(data = subset(UQL4_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL4 2020
##################################################################################################
UQL4_20 <- subset(UQL4, Year == '2020')

#Plotting 
UQL4_20$WC_15cm <- as.numeric(UQL4_20$WC_15cm)
UQL4_20$WC_30cm <- as.numeric(UQL4_20$WC_30cm)
UQL4_20$WC_100cm <- as.numeric(UQL4_20$WC_100cm)
UQL4_20$Date_time<- mdy_hms(UQL4_20$Date_time)

#15 cm
###########################################

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-06-08 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-06-12 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.205] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-06-08 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-06-12 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-04-30 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-05-12 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.25 | UQL4_20_fix$WC_15cm < 0.217] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-04-30 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-05-12 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-06-15 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-06-22 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.1825] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-06-15 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-06-22 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-05-29 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-06-08 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.238 | UQL4_20_fix$WC_15cm < 0.213] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-05-29 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-06-08 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-06-25 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-06-27 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.1724] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-06-25 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-06-27 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-07-06 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-07-12 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.1613] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-07-06 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-07-12 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-07-12 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-07-22 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.1325] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-07-12 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-07-22 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-07-22 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-07-28 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.126] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-07-22 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-07-28 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-07-30 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-08-03 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.128 | UQL4_20_fix$WC_15cm < 0.12] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-07-30 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-08-03 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-08-03 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-08-23 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.128 | UQL4_20_fix$WC_15cm < 0.1125] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-08-03 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-08-23 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-08-03 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-08-10 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.1175] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-08-03 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-08-10 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-08-24 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-08-30 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.120] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-08-24 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-08-30 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-08-31 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-09-08 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.111 | UQL4_20_fix$WC_15cm > 0.118] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-08-31 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-09-08 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-08 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-09-18 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.105] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-08 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-09-18 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-23 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-09-28 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.093] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-23 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-09-28 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-24 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-09-25 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.094] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-24 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-09-25 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-25 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-09-27 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.095] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-25 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-09-27 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-27 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-10-07 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.089 | UQL4_20_fix$WC_15cm > 0.1] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-27 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-10-07 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-10-06 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-10-19 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.092 | UQL4_20_fix$WC_15cm > 0.1] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-10-06 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-10-19 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-10-19 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-11-10 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.97] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

UQL4_20_early <- filter(UQL4_20, Date_time < "2020-10-19 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-11-10 00:00:01")

UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-10-26 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-11-10 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.095 | UQL4_20_fix$WC_15cm < 0.0815] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-10-26 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-11-10 00:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-04-22 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-05-10 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.251] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-04-22 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-05-10 00:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-04-29 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-05-10 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.238] <- NA

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-04-29 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-05-10 00:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-05-02 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-05-04 00:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.233 | UQL4_20_fix$WC_15cm < 0.2301] <- NA

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-05-02 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-05-04 00:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-05-11 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-05-14 12:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0] <- NA

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-05-11 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-05-14 12:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-05-29 22:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-06-06 12:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.2325] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-05-29 22:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-06-06 12:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-06-02 22:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-06-06 12:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm > 0.225] <- NA

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-06-02 22:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-06-06 12:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Fix dips in 15 cm in early April/May 
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-06-13 22:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-06-22 12:00:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.2025] <- NA

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-06-13 22:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-06-22 12:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Calibrate
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-21 03:10:001")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-12-31 23:50:01")

UQL4_20_fix$WC_15cm <- UQL4_20_fix$WC_15cm + 0.0066

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-21 03:10:001")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-12-31 23:50:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Calibrate
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-15 03:10:001")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-09-18 23:50:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.1064] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-15 03:10:001")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-09-18 23:50:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Calibrate
#================================================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-18 03:10:001")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-09-23 23:50:01")

UQL4_20_fix$WC_15cm[UQL4_20_fix$WC_15cm < 0.1] <- NA
missing <- which(is.na(UQL4_20_fix$WC_15cm))

if(1 %in% missing){
  UQL4_20_fix$WC_15cm[1] <- head(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}
if(nrow(UQL4_20_fix) %in% missing){
  UQL4_20_fix$WC_15cm[nrow(data)] <- tail(UQL4_20_fix$WC_15cm[!is.na(UQL4_20_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20_fix$WC_15cm[idx] <- (UQL4_20_fix$WC_15cm[r$starts[i]] + UQL4_20_fix$WC_15cm[r$ends[i]])/2
}

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-18 03:10:001")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-09-23 23:50:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#30 cm 
################################################################
UQL4_20$WC_30cm[UQL4_20$WC_30cm < 0.15] <- NA
missing <- which(is.na(UQL4_20$WC_30cm))

if(1 %in% missing){
  UQL4_20$WC_30cm[1] <- head(UQL4_20$WC_30cm[!is.na(UQL4_20$WC_30cm)],1)
}
if(nrow(UQL4_20) %in% missing){
  UQL4_20$WC_30cm[nrow(data)] <- tail(UQL4_20$WC_30cm[!is.na(UQL4_20$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20$WC_30cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20$WC_30cm[idx] <- (UQL4_20$WC_30cm[r$starts[i]] + UQL4_20$WC_30cm[r$ends[i]])/2
}

#Get rid of glitch before missing dates
#=========================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-08-22 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-09-25 00:00:01")

UQL4_20_fix$WC_30cm[UQL4_20_fix$WC_30cm > 0.09] <- NA

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-08-23 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-09-25 00:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Get rid of glitch after missing dates
#=========================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-22 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-11-25 00:00:01")

UQL4_20_fix$WC_30cm[UQL4_20_fix$WC_30cm == 0.22005] <- NA

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-22 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-11-25 00:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#Get rid of glitch after missing dates
#=========================================================================
UQL4_20_fix <- filter(UQL4_20, Date_time > "2020-09-22 00:00:01")
UQL4_20_fix <- filter(UQL4_20_fix, Date_time < "2020-11-25 00:00:01")

UQL4_20_fix$WC_30cm[UQL4_20_fix$WC_30cm == 0.1503] <- NA

#Recombine
UQL4_20_early <- filter(UQL4_20, Date_time < "2020-09-22 00:00:01")
UQL4_20_late <- filter(UQL4_20, Date_time > "2020-11-25 00:00:01")
UQL4_20 <- bind_rows(UQL4_20_late, UQL4_20_early, UQL4_20_fix)

#100 cm 
################################################################
UQL4_20$WC_100cm[UQL4_20$WC_100cm < 0] <- NA
missing <- which(is.na(UQL4_19$WC_100cm))

if(1 %in% missing){
  UQL4_20$WC_100cm[1] <- head(UQL4_20$WC_100cm[!is.na(UQL4_20$WC_100cm)],1)
}
if(nrow(UQL4_20) %in% missing){
  UQL4_20$WC_100cm[nrow(data)] <- tail(UQL4_20$WC_100cm[!is.na(UQL4_20$WC_100cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_20$WC_100cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_20$WC_100cm[idx] <- (UQL4_20$WC_100cm[r$starts[i]] + UQL4_20$WC_100cm[r$ends[i]])/2
}

#Plot again 
Soil <- ggplot(data = subset(UQL4_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#UQL4 2021
##################################################################################################
UQL4_21 <- subset(UQL4, Year == '2021')

#Plotting 
UQL4_21$WC_15cm <- as.numeric(UQL4_21$WC_15cm)
UQL4_21$WC_30cm <- as.numeric(UQL4_21$WC_30cm)
UQL4_21$WC_100cm <- as.numeric(UQL4_21$WC_100cm)
UQL4_21$Date_time<- mdy_hms(UQL4_21$Date_time)

#15 cm
###########################################
UQL4_21$WC_15cm[UQL4_21$WC_15cm < 0.12] <- NA
missing <- which(is.na(UQL4_21$WC_15cm))

if(1 %in% missing){
  UQL4_21$WC_15cm[1] <- head(UQL4_21$WC_15cm[!is.na(UQL4_21$WC_15cm)],1)
}
if(nrow(UQL4_21) %in% missing){
  UQL4_21$WC_15cm[nrow(data)] <- tail(UQL4_21$WC_15cm[!is.na(UQL4_21$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_21$WC_15cm))


for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_21$WC_15cm[idx] <- (UQL4_21$WC_15cm[r$starts[i]] + UQL4_21$WC_15cm[r$ends[i]])/2
}

#Subset
#==================================================================
UQL4_21_fix <- filter(UQL4_21, Date_time > "2021-02-01 00:00:01")
UQL4_21_fix <- filter(UQL4_21_fix, Date_time < "2021-04-10 00:00:01")

UQL4_21_fix$WC_15cm[UQL4_21_fix$WC_15cm > 0.300149 & UQL4_21_fix$WC_15cm < 0.300151 ] <- NA

UQL4_21_early <- filter(UQL4_21, Date_time < "2021-02-01 00:00:01")
UQL4_21_late <- filter(UQL4_21, Date_time > "2021-04-10 00:00:01")

UQL4_21 <- bind_rows(UQL4_21_late, UQL4_21_early, UQL4_21_fix)

#Subset
#==================================================================
UQL4_21_fix <- filter(UQL4_21, Date_time > "2021-02-01 00:00:01")
UQL4_21_fix <- filter(UQL4_21_fix, Date_time < "2021-02-10 00:00:01")

UQL4_21_fix$WC_15cm[UQL4_21_fix$WC_15cm < 0.305 ] <- NA
missing <- which(is.na(UQL4_21_fix$WC_15cm))

if(1 %in% missing){
  UQL4_21_fix$WC_15cm[1] <- head(UQL4_21_fix$WC_15cm[!is.na(UQL4_21_fix$WC_15cm)],1)
}
if(nrow(UQL4_21_fix) %in% missing){
  UQL4_21_fix$WC_15cm[nrow(data)] <- tail(UQL4_21_fix$WC_15cm[!is.na(UQL4_21_fix)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(UQL4_21_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  UQL4_21_fix$WC_15cm[idx] <- (UQL4_21_fix$WC_15cm[r$starts[i]] + UQL4_21_fix$WC_15cm[r$ends[i]])/2
}


UQL4_21_early <- filter(UQL4_21, Date_time < "2021-02-01 00:00:01")
UQL4_21_late <- filter(UQL4_21, Date_time > "2021-02-10 00:00:01")

UQL4_21 <- bind_rows(UQL4_21_late, UQL4_21_early, UQL4_21_fix)

#Plot again
Soil <- ggplot(data = subset(UQL4_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "navyblue")) + 
  geom_line(aes(y = WC_30cm, color = "blue")) + 
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018
UQL4_clean <- merge(UQL4_18, UQL4_17, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2020 
UQL4_clean <- merge(UQL4_clean, UQL4_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
UQL4_clean <- merge(UQL4_clean, UQL4_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
UQL4_clean <- merge(UQL4_clean, UQL4_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)
UQL4_clean <- select(UQL4_clean, Date_time, WC_15cm, WC_30cm, WC_100cm)
#Graph
#===================================================================================

Soil <- ggplot(data = subset(UQL4_clean, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_100cm, color = "100 cm"), color = "navy blue", linetype = "dotted", size = 4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(aes(y = WC_30cm, color = "30 cm "), color = "blue", linetype = 2, size = 4) + 
  geom_line(aes(y = WC_15cm, color = "15 cm"), color = "lightblue", linetype = "solid", size = 4) + 
  ylab(expression(paste("Water Content (m3/m3)"))) + 
  xlab(expression("Date")) 
Soil

png("UQL4_clean", width = 4500, height = 2500)

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
write.csv(UQL4_clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/UQL/UQL4_clean.csv" ) #this writes a csv file and sends it to the working folder

