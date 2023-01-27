#Created by: Elise Miller
#Date started: 09/26/22
#Date last edited: 
#Description: WIL soil moisture graphs hydraulic redistribution 

#Attach files
attach(WIL1)
attach(WIL2)
attach(WIL3)
attach(WIL4)
attach(WIL5)

#Attach dependencies 
library(tidyverse) #I got rid of a lot of libraries, if something doesn't work, add 1 by 1 
library(dplyr)
library(ggpubr)
library(tidyr)
library(lubridate, warn.conflicts = FALSE)

#Converting to a date column 
WIL1$Date <- mdy_hms(WIL1$Date_time) #got warned 8807 failed to parse
WIL2$Date <- mdy_hms(WIL2$Date_time)
WIL3$Date <- mdy_hms(WIL3$Date_time)
WIL4$Date <- mdy_hms(WIL4$Date_time)
WIL5$Date <- mdy_hms(WIL5$Date_time)

#Converting soil moisture character to a numeric
WIL1$WC_100cm <- as.numeric(WIL1$WC_100cm)
WIL1$WC_30cm <- as.numeric(WIL1$WC_30cm)
WIL1$WC_15cm <- as.numeric(WIL1$WC_15cm)

WIL2$WC_100cm <- as.numeric(WIL2$WC_100cm)
WIL2$WC_30cm <- as.numeric(WIL2$WC_30cm)
WIL2$WC_15cm <- as.numeric(WIL2$WC_15cm)

WIL3$WC_100cm <- as.numeric(WIL3$WC_100cm)
WIL3$WC_30cm <- as.numeric(WIL3$WC_30cm)
WIL3$WC_15cm <- as.numeric(WIL3$WC_15cm)

WIL4$WC_100cm <- as.numeric(WIL4$WC_100cm)
WIL4$WC_30cm <- as.numeric(WIL4$WC_30cm)
WIL4$WC_15cm <- as.numeric(WIL4$WC_15cm)

WIL5$WC_100cm <- as.numeric(WIL5$WC_100cm)
WIL5$WC_30cm <- as.numeric(WIL5$WC_30cm)
WIL5$WC_15cm <- as.numeric(WIL5$WC_15cm)

#Plot
#WIL1
Soil_WIL <- ggplot(data = subset(WIL1, !is.na(Date)), aes(x = Date)) + 
  geom_line(aes(y= WIL1$WC_15cm, color = "15 cm", linetype = "dotted")) + 
  geom_line(aes(y= WIL1$WC_30cm, color = "30 cm", linetype = "dashed")) + 
  geom_line(aes(y= WIL1$WC_100cm, color = "100 cm")) +
  labs(x = "Date", y = "Water Content", color = "Legend")+
  scale_color_manual(values = c("darkblue", "lightblue", "blue")) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
Soil_WIL


#WIL2
Soil_WIL2 <- ggplot(data = subset(WIL2, !is.na(Date)), aes(x = Date)) + 
  geom_line(aes(y= WIL2$WC_15cm, color = "15 cm", linetype = "dotted")) + 
  geom_line(aes(y= WIL2$WC_30cm, color = "30 cm", linetype = "dashed")) + 
  geom_line(aes(y= WIL2$WC_100cm, color = "100 cm")) +
  labs(x = "Date", y = "Water Content", color = "Legend")+
  scale_color_manual(values = c("darkblue", "lightblue", "blue")) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
Soil_WIL2


#WIL3
Soil_WIL3 <- ggplot(data = subset(WIL3, !is.na(Date)), aes(x = Date)) + 
  geom_line(aes(y= WIL3$WC_15cm), color = "lightblue", linetype = "dashed") + 
  geom_line(aes(y= WIL3$WC_30cm), color = "blue", linetype = "dotted") + 
  geom_line(aes(y= WIL3$WC_100cm), color = "darkblue") + 
  labs(x = "Date", y = "Water Content") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
Soil_WIL3

#WIL4
Soil_WIL4 <- ggplot(data = subset(WIL4, !is.na(Date)), aes(x = Date)) + 
  geom_line(aes(y= WIL4$WC_15cm), color = "lightblue", linetype = "dashed") + 
  geom_line(aes(y= WIL4$WC_30cm), color = "blue", linetype = "dotted") + 
  geom_line(aes(y= WIL4$WC_100cm), color = "darkblue") + 
  labs(x = "Date", y = "Water Content") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
Soil_WIL4

#WIL5
Soil_WIL5 <- ggplot(data = subset(WIL5, !is.na(Date)), aes(x = Date)) + 
  geom_line(aes(y= WIL5$WC_15cm), color = "lightblue", linetype = "dashed") + 
  geom_line(aes(y= WIL5$WC_30cm), color = "blue", linetype = "dotted") + 
  geom_line(aes(y= WIL5$WC_100cm), color = "darkblue") + 
  labs(x = "Date", y = "Water Content") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
Soil_WIL5
