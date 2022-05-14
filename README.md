##### The Global Burden of Cancer 2010 to 2019: How do Developed and           #
##### Developing Countries Compare in Cancer Rates                             #
# Code developed by Max Winter and Andre Faid                                  #
################################################################################

##### SET WORKING DIRECTORY ####################################################
# User should set the relevant working directory.                              #
# Either Session --> Set Working Directory --> To Source File Location         #
# To Source File Location means that the data will be imported and saved       #
# IN THE SAME FOLDER where you saved the R Script you're working with!         #
# OR                                                                           #
## setwd("")                                                                   #
# This code used setwd("~/Desktop/Data project") as the working directory that #
# contained the data set.                                                      # 
################################################################################

##### RELEVANT PACKAGES ########################################################
##### Users should install the relevant packages below                         #
#                                                                              #
# Use install.packages('') if unable install the required packages from        #
# library ()                                                                   #
################################################################################
library(corrplot) # For correlation matrix graph visualization.
library(ggplot2) # For graph visualizations. 
library(ggthemes) # For graph customisation.
library(Amelia) # For miss map of missing data.
library(naniar) # Shows missing values. 
library(janitor) # Shows rows of a data frame with identical values for the specified variables.
library(GGally)# For scatter plot matrix graph visualization. 
library(corrplot) # For correlation matrix graph visualization.
library(ggpubr) # To have multiple plots on one same page. 
library(scales) # stops ggplot using a scientific notation scale.
library(forecast) # package provides various functions for computing and visualizing basic time series components
library(tseries) # package provides various functions for computing and visualizing basic time series components

##### DATA INPUT ###############################################################

# Deaths Data 
Deaths <- read.csv("Diss data/Deaths.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Deaths) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Deaths) # Shows the name of variables in the data set.
summary(Deaths) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Deaths) # Shows the observations and variables of the data.

# DALYs Data 
DALYs <- read.csv("Diss data/DALYs.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(DALYs) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(DALYs) # Shows the name of variables in the data set.
summary(DALYs) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(DALYs) # Shows the observations and variables of the data.

# YLLs Data 
YLLs <- read.csv("Diss data/YLLs.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(YLLs) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(YLLs) # Shows the name of variables in the data set.
summary(YLLs) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(YLLs) # Shows the observations and variables of the data.

# Prevalence Data 
Prev <- read.csv("Diss data/Prevalence.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Prev) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Prev) # Shows the name of variables in the data set.
summary(Prev) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Prev) # Shows the observations and variables of the data.

##### DATA CODING/ CLEANING ####################################################

# Deaths Data 
Deaths[Deaths == "" | Deaths == " "] <- NA # Makes any missing data a NA. 
Deaths[Deaths == "N/A" | Deaths == "n/a" | Deaths == "N/a"| Deaths == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Deaths$location_name <- factor(Deaths$location_name)   
Deaths <- subset(Deaths, select =-c(1,3,5,6,7,9,11,15,16)) # upper and lower values 

# DALYs Data 
DALYs[DALYs == "" | DALYs == " "] <- NA # Makes any missing data a NA. 
DALYs[DALYs == "N/A" | DALYs == "n/a" | DALYs == "N/a"| DALYs == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
DALYs$location_name <- factor(DALYs$location_name)  
DALYs <- subset(DALYs, select =-c(1,3,5,6,7,9,11,15,16)) # upper and lower values 

# YLLS Data 
YLLs[YLLs == "" | YLLs == " "] <- NA # Makes any missing data a NA. 
YLLs[YLLs == "N/A" | YLLs == "n/a" | YLLs == "N/a"| YLLs == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
YLLs$location_name <- factor(YLLs$location_name)  
YLLs <- subset(YLLs, select =-c(1,3,5,6,7,9,11,15,16)) # upper and lower values 

# Prevalence Data 
Prev[Prev == "" | Prev == " "] <- NA # Makes any missing data a NA. 
Prev[Prev == "N/A" | Prev == "n/a" | Prev == "N/a"| Prev == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Prev$location_name <- factor(Prev$location_name)  
Prev <- subset(Prev, select =-c(1,3,5,6,7,9,11,15,16)) # upper and lower values 

# Check for missing data
par(bg = "#d5e4eb") # Changes colour of plot background. 
missmap(Deaths, col=c("red", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(DALYs, col=c("red", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(YLLs, col=c("red", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Prev, col=c("red", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 

##### EXPLORATORY DATA ANALYSIS ################################################

# To reagrance locations in bar graphs  
Deaths$location_name <- factor(Deaths$location_name, levels = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"))
DALYs$location_name <- factor(DALYs$location_name, levels = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"))
Prev$location_name <- factor(Prev$location_name, levels = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"))
YLLs$location_name <- factor(YLLs$location_name, levels = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"))

#### Bar grpahs : Cancer Deaths, DALYs, YLLs, Prevalence rates ################# 
Cdeaths <- ggplot(Deaths, aes(x =location_name, y = val, fill = location_name)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Death Rate", x= "", y= "Deaths") +
  scale_colour_economist() +
  scale_y_continuous(labels = comma)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = "#d5e4eb")) # Background color of the plot

CDALY <- ggplot(DALYs, aes(x =location_name, y = val, fill = location_name)) + 
  geom_bar(stat = "identity") + 
  labs(title = "DALYs Rate", x= "", y= "DALYs") +
  scale_colour_economist() +
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = "#d5e4eb"))

Cprev <- ggplot(Prev, aes(x =location_name, y = val, fill = location_name)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Prevalence Rate", x= "", y= "Prevalence") +
  scale_colour_economist() +
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = "#d5e4eb"))

CYLL <- ggplot(YLLs, aes(x =location_name, y = val, fill = location_name)) + 
  geom_bar(stat = "identity") + 
  labs(title = "YLLs Rate", x= "", y= "YLLs") +
  scale_colour_economist() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = "#d5e4eb")) 

ggarrange(Cdeaths, CDALY, Cprev,CYLL + rremove("x.text"), 
          ncol = 2, nrow = 2) # This code arranges the above figures into one plot in order to see all figures better and easier.

#### Box plots: Cancer Deaths, DALYs, YLLs, Prevalence rates ################### 

box1 <- ggplot(Deaths, aes(y = val)) +
  stat_boxplot(geom = "errorbar", width = 0.15) + 
  labs(title = "Death Rate", x= "", y= "Number of Deaths") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) + 
  geom_boxplot() 

box2 <- ggplot(DALYs, aes(y = val)) +
  stat_boxplot(geom = "errorbar", width = 0.15) + 
  labs(title = "DALYs Rate", x= "", y= "Number of DALYs") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) + 
  geom_boxplot() 

box3 <- ggplot(Prev, aes(y = val)) +
  stat_boxplot(geom = "errorbar", width = 0.15) + 
  labs(title = "Prevalence Rate", x= "", y= "Prevalence") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) + 
  geom_boxplot() 

box4 <- ggplot(YLLs, aes(y = val)) +
  stat_boxplot(geom = "errorbar", width = 0.15) + 
  labs(title = "YLLs Rate", x= "", y= "Number of YLLs") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) + 
  geom_boxplot() 

ggarrange(box1, box2, box3,box4 + rremove("x.text"), 
          ncol = 2, nrow = 2) # This code arranges the above figures into one plot in order to see all figures better and easier.

##### TIME SERIES ANALYSIS #####################################################

library(forecast) # package provides various functions for computing and visualizing basic time series components
library(tseries) # package provides various functions for computing and visualizing basic time series components
library(fpp2) # package provides various functions for computing and visualizing basic time series components

##### TIME SERIES DATA CODING/ CLEANING ########################################

# Deaths 
Deaths_high <- Deaths[!(Deaths$location_name=="High-middle SDI" | Deaths$location_name=="Middle SDI" | 
                     Deaths$location_name=="Low-middle SDI" | Deaths$location_name=="Low SDI"),]
Deaths_low <- Deaths[!(Deaths$location_name=="High-middle SDI" | Deaths$location_name=="Middle SDI" | 
                          Deaths$location_name=="Low-middle SDI" | Deaths$location_name=="High SDI"),]
# DALYs
DALYs_high <- DALYs[!(DALYs$location_name=="High-middle SDI" | DALYs$location_name=="Middle SDI" | 
                     DALYs$location_name=="Low-middle SDI"| DALYs$location_name=="Low SDI"),]
DALYs_low <- DALYs[!(DALYs$location_name=="High-middle SDI" | DALYs$location_name=="Middle SDI" | 
                        DALYs$location_name=="Low-middle SDI"| DALYs$location_name=="High SDI"),]
# YLLs
YLLs_high <- YLLs[!(YLLs$location_name=="High-middle SDI" | YLLs$location_name=="Middle SDI" | 
                   YLLs$location_name=="Low-middle SDI"| YLLs$location_name=="Low SDI"),]
YLLs_low <- YLLs[!(YLLs$location_name=="High-middle SDI" | YLLs$location_name=="Middle SDI" | 
                      YLLs$location_name=="Low-middle SDI"| YLLs$location_name=="High SDI"),]
# Prevalence 
Prev_high <- Prev[!(Prev$location_name=="High-middle SDI" | Prev$location_name=="Middle SDI" | 
                 Prev$location_name=="Low-middle SDI"| Prev$location_name=="Low SDI"),]
Prev_low <- Prev[!(Prev$location_name=="High-middle SDI" | Prev$location_name=="Middle SDI" | 
                      Prev$location_name=="Low-middle SDI"| Prev$location_name=="High SDI"),]






















