#Created by: Elise Miller
#Date started: 02/09/2023
#Date last edited: 02/10/2023
#Description: QA/QC WIL 1, this is the second cleaning because the first file was hard to work with 
#because of poor coding 

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

#FILES
#========================================================================================================================
attach(WIL1_QAQC2)

#BREAK APART WIL1.csv INTO DIFFERENT YEARS
#==================================================================================================

#Rename
WIL1 <- WIL1_QAQC2

#Put year into a separate column 
WIL1 <- WIL1 %>% dplyr::mutate(year = lubridate::year(Date_time))

#WIL1 2018
##################################################################################################
WIL1_18 <- subset(WIL1, year == '2018')

#Convert Date_time column from character to date
WIL1_18$Date_time<- ymd_hms(WIL1_18$Date_time)

#15 cm 
##############################################################################################

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-01-07 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-01-17 12:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.31] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-01-07 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-01-17 12:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-03-01 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-03-03 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.332] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-03-01 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-03-03 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-03-03 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-03-10 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.3135] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-03-03 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-03-10 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-01-03 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-01-10 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.30] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-01-03 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-01-10 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-03-12 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-03-19 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.311] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-03-12 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-03-19 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-03-17 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-03-26 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.314] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-03-17 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-03-26 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-03-30 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-04-07 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.305] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-03-30 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-04-07 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-04-07 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-04-19 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.315] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-04-07 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-04-19 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-04-14 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-04-19 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.3165] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-04-14 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-04-19 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-04-25 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-05-01 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.3067] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-04-25 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-05-01 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-05-21 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-05-28 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.2945] <- NA

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time <"2018-05-21 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-05-28 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-05-21 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-05-26 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.2955] <- NA

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time <"2018-05-21 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-05-26 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-06-04 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-06-12 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.2875] <- NA

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-06-04 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-06-12 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-06-5 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-06-07 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm < 0.2905] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-06-05 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-06-07 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drips in June
#===============================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-07-05 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-07-11 02:00:01")

WIL1_18_fix$WC_15cm[WIL1_18_fix$WC_15cm > 0.268] <- NA
missing <- which(is.na(WIL1_18_fix$WC_15cm))

if(1 %in% missing){
  WIL1_18_fix$WC_15cm[1] <- head(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_15cm[nrow(data)] <- tail(WIL1_18_fix$WC_15cm[!is.na(WIL1_18_fix$WC_15cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_15cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_15cm[idx] <- (WIL1_18_fix$WC_15cm[r$starts[i]] + WIL1_18_fix$WC_15cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-07-05 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-07-11 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#30 cm 
##########################################################################
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-09-30 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-10-11 02:00:01")


WIL1_18_fix$WC_30cm[WIL1_18_fix$WC_30cm < 0.20275] <- NA

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-09-30 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-10-11 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Subset and remove drip
#====================================================================
WIL1_18_fix <- filter(WIL1_18, Date_time > "2018-10-11 1:00:01")
WIL1_18_fix <- filter(WIL1_18_fix, Date_time < "2018-11-11 02:00:01")

WIL1_18_fix$WC_30cm[WIL1_18_fix$WC_30cm < 0.201] <- NA
missing <- which(is.na(WIL1_18_fix$WC_30cm))

if(1 %in% missing){
  WIL1_18_fix$WC_30cm[1] <- head(WIL1_18_fix$WC_30cm[!is.na(WIL1_18_fix$WC_30cm)],1)
}
if(nrow(WIL1_18_fix) %in% missing){
  WIL1_18_fix$WC_30cm[nrow(data)] <- tail(WIL1_18_fix$WC_30cm[!is.na(WIL1_18_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_18_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_18_fix$WC_30cm[idx] <- (WIL1_18_fix$WC_30cm[r$starts[i]] + WIL1_18_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_18_later <- filter(WIL1_18, Date_time < "2018-10-11 1:00:01")
WIL1_18_end <- filter(WIL1_18, Date_time > "2018-11-11 02:00:01")
WIL1_18 <- bind_rows(WIL1_18_later, WIL1_18_fix, WIL1_18_end)

#Plot again 
Soil <- ggplot(data = subset(WIL1_18, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL 1 2019
##################################################################################################
WIL1_19 <- subset(WIL1, year == '2019')

#Convert Date_time column from character to date
WIL1_19$Date_time<- ymd_hms(WIL1_19$Date_time)

#30 cm 
##########################################################################

#Remove drips
#==========================================================================
WIL1_19$WC_30cm[WIL1_19$WC_30cm == 0.3228] <- NA

#Plot again 
Soil <- ggplot(data = subset(WIL1_19, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL 1 2020
##################################################################################################
WIL1_20 <- subset(WIL1, year == '2020')

#Convert Date_time column from character to date
WIL1_20$Date_time<- ymd_hms(WIL1_20$Date_time)

#Plot again 
Soil <- ggplot(data = subset(WIL1_20, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#WIL 1 2021
##################################################################################################
WIL1_21 <- subset(WIL1, year == '2021')

#Convert Date_time column from character to date
WIL1_21$Date_time<- ymd_hms(WIL1_21$Date_time)

#30 cm 
###############################################################################

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-01-04 1:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-01-07 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.274] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-01-04 1:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-01-07 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-01-07 1:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-01-09 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.276] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-01-07 1:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-01-09 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-01-09 1:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-01-14 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.274] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-01-09 1:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-01-14 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-01-01 1:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-01-05 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.27] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-01-01 1:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-01-05 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-01-31 11:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-02-03 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.2775] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-01-31 11:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-02-03 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-02-09 11:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-02-23 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.274] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-02-09 11:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-02-23 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-02-12 14:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-02-16 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.2805] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-02-12 14:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-02-16 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-02-14 04:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-02-16 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.2815] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-02-14 04:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-02-16 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-02-18 04:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-02-22 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.2795] <- NA

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-02-18 04:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-02-22 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-01-18 04:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-02-22 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.268] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-01-18 04:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-02-22 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-02-22 04:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-03-22 02:00:01")

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.2695] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-02-22 04:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-03-22 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Subset and remove drips
#===========================================================================
WIL1_21_fix <- filter(WIL1_21, Date_time > "2021-01-29 01:00:01")
WIL1_21_fix <- filter(WIL1_21_fix, Date_time < "2021-02-02 02:00:01")

Soil <- ggplot(data = subset(WIL1_21_fix, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) 
Soil 

WIL1_21_fix$WC_30cm[WIL1_21_fix$WC_30cm < 0.278] <- NA
missing <- which(is.na(WIL1_21_fix$WC_30cm))

if(1 %in% missing){
  WIL1_21_fix$WC_30cm[1] <- head(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}
if(nrow(WIL1_21_fix) %in% missing){
  WIL1_21_fix$WC_30cm[nrow(data)] <- tail(WIL1_21_fix$WC_30cm[!is.na(WIL1_21_fix$WC_30cm)],1)
}

#Find start and ends of each run of NAs
get_runs <- function(x){
  starts <- which(diff(x) == 1)
  y <- rle(x)
  len <- y$lengths[y$values==TRUE]
  ends <- starts + len+1
  return(list(starts=starts,len=len,ends=ends, i=1:length(starts)))
}

r <- get_runs(is.na(WIL1_21_fix$WC_30cm))

for(i in r$i){
  idx <- seq(r$starts[i]+1,r$ends[i]-1,1)
  WIL1_21_fix$WC_30cm[idx] <- (WIL1_21_fix$WC_30cm[r$starts[i]] + WIL1_21_fix$WC_30cm[r$ends[i]])/2
}

#Recombine 
WIL1_21_later <- filter(WIL1_21, Date_time < "2021-01-29 01:00:01")
WIL1_21_end <- filter(WIL1_21, Date_time > "2021-02-02 02:00:01")
WIL1_21 <- bind_rows(WIL1_21_later, WIL1_21_fix, WIL1_21_end)

#Plot again 
Soil <- ggplot(data = subset(WIL1_21, !is.na(Date_time)), aes(x = Date_time)) + 
  geom_line(aes(y = WC_30cm, color = "blue")) +
  geom_line(aes(y = WC_100cm, color = "navyblue")) +
  geom_line(aes(y = WC_15cm, color = "lightblue"))
Soil 

#Merge and plot 
##################################################################################
#Combine all of the UQL2 dataframes/years into 1 dataframe 

#Merge 2017 and 2018

#Merge the merged with 2020 
WIL1_QAQC2clean <- merge(WIL1_18, WIL1_19, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
WIL1_QAQC2clean <- merge(WIL1_QAQC2clean, WIL1_20, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)

#Merge the merged with 2021
WIL1_QAQC2clean <- merge(WIL1_QAQC2clean, WIL1_21, by = c("Date_time", "WC_15cm", "WC_30cm", "WC_100cm"), 
                    all.x = TRUE, all.y = TRUE)
WIL1_QAQC2clean <- select(WIL1_QAQC2clean, Date_time, WC_15cm, WC_30cm, WC_100cm)

write.csv(WIL1_QAQC2clean,"~/Library/CloudStorage/GoogleDrive-mill9104@d.umn.edu/Shared drives/Caspar Data/Soil Moisture/Working_data/WIL/WIL1_QAQC2clean.csv" ) #this writes a csv file and sends it to the working folder
