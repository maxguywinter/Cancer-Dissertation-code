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
library(gridExtra) # To have multiple plots on one same page. 
library(scales) # stops ggplot using a scientific notation scale.
library(forecast) # package provides various functions for computing and visualizing basic time series components
library(tseries) # package provides various functions for computing and visualizing basic time series components
library(tidyverse) # for EDA and rearranging data 
library(rpart) # for building classification and regression trees
library(rpart.plot) # to plot "rpart" function
library(pROC) # to plot ROC curve

##### DATA INPUT ###############################################################
##### Measure data #############################################################
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

##### Age data #################################################################
# Age Deaths 
Age_death <- read.csv("Diss data/Age_deaths.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Age_death) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Age_death) # Shows the name of variables in the data set.
summary(Age_death) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Age_death) # Shows the observations and variables of the data.

# Age Deaths High SDI
Death_Age_highSDI <- read.csv("Diss data/Death_Age_HighSDI.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Death_Age_highSDI) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Death_Age_highSDI) # Shows the name of variables in the data set.
summary(Death_Age_highSDI) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Death_Age_highSDI) # Shows the observations and variables of the data.

# Age Deaths Middle SDI
Death_Age_middleSDI <- read.csv("Diss data/Death_Age_MiddleSDI.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Death_Age_middleSDI) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Death_Age_middleSDI) # Shows the name of variables in the data set.
summary(Death_Age_middleSDI) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Death_Age_middleSDI) # Shows the observations and variables of the data.

# Age Deaths Low SDI
Death_Age_lowSDI <- read.csv("Diss data/Deaths_Age_LowSDI.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Death_Age_lowSDI) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Death_Age_lowSDI) # Shows the name of variables in the data set.
summary(Death_Age_lowSDI) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Death_Age_lowSDI) # Shows the observations and variables of the data.

# Age DALYs 
Age_DALY <- read.csv("Diss data/Age_DALY.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Age_DALY) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Age_DALY) # Shows the name of variables in the data set.
summary(Age_DALY) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Age_DALY) # Shows the observations and variables of the data.

# Age YLLs 
Age_YLL <- read.csv("Diss data/Age_YLL.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Age_YLL) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Age_YLL) # Shows the name of variables in the data set.
summary(Age_YLL) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Age_YLL) # Shows the observations and variables of the data.

# Age Prevalence 
Age_Prev <- read.csv("Diss data/Age_Prev.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Age_Prev) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Age_Prev) # Shows the name of variables in the data set.
summary(Age_Prev) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Age_Prev) # Shows the observations and variables of the data.

# Age Prevalence High SDI
Prev_Age_highSDI <- read.csv("Diss data/Age_highSDI.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Prev_Age_highSDI) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Prev_Age_highSDI) # Shows the name of variables in the data set.
summary(Prev_Age_highSDI) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Prev_Age_highSDI) # Shows the observations and variables of the data.

# Age Prevalence Middle SDI
Prev_Age_middleSDI <- read.csv("Diss data/Age_middleSDI.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Prev_Age_middleSDI) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Prev_Age_middleSDI) # Shows the name of variables in the data set.
summary(Prev_Age_middleSDI) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Prev_Age_middleSDI) # Shows the observations and variables of the data.

# Age Prevalence Low SDI
Prev_Age_lowSDI <- read.csv("Diss data/Age_lowSDI.csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(Prev_Age_lowSDI) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(Prev_Age_lowSDI) # Shows the name of variables in the data set.
summary(Prev_Age_lowSDI) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(Prev_Age_lowSDI) # Shows the observations and variables of the data.

##### DATA CODING/ CLEANING ####################################################
##### Measure data #############################################################
# Deaths Data 
Deaths[Deaths == "" | Deaths == " "] <- NA # Makes any missing data a NA. 
Deaths[Deaths == "N/A" | Deaths == "n/a" | Deaths == "N/a"| Deaths == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Deaths$location_name <- factor(Deaths$location_name)   
Deaths <- subset(Deaths, select =-c(1,3,5,6,7,9,11,15,16)) # Removes upper and lower values 

# DALYs Data 
DALYs[DALYs == "" | DALYs == " "] <- NA # Makes any missing data a NA. 
DALYs[DALYs == "N/A" | DALYs == "n/a" | DALYs == "N/a"| DALYs == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
DALYs$location_name <- factor(DALYs$location_name)  
DALYs <- subset(DALYs, select =-c(1,3,5,6,7,9,11,15,16)) 

# YLLS Data 
YLLs[YLLs == "" | YLLs == " "] <- NA # Makes any missing data a NA. 
YLLs[YLLs == "N/A" | YLLs == "n/a" | YLLs == "N/a"| YLLs == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
YLLs$location_name <- factor(YLLs$location_name)  
YLLs <- subset(YLLs, select =-c(1,3,5,6,7,9,11,15,16))  

# Prevalence Data 
Prev[Prev == "" | Prev == " "] <- NA # Makes any missing data a NA. 
Prev[Prev == "N/A" | Prev == "n/a" | Prev == "N/a"| Prev == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Prev$location_name <- factor(Prev$location_name)  
Prev <- subset(Prev, select =-c(1,3,5,6,7,9,11,15,16)) 

##### Age data #################################################################
# Age Deaths 
Age_death[Age_death == "" | Age_death == " "] <- NA # Makes any missing data a NA. 
Age_death[Age_death == "N/A" | Age_death == "n/a" | Age_death == "N/a"| Age_death == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Age_death$location_name <- factor(Age_death$location_name)  
Age_death$age_name <- factor(Age_death$age_name)  
Age_death <- subset(Age_death, select =-c(1,3,5,6,7,9,11,15,16)) 

# Age Deaths High SDI
Death_Age_highSDI[Death_Age_highSDI == "" | Death_Age_highSDI == " "] <- NA # Makes any missing data a NA. 
Death_Age_highSDI[Death_Age_highSDI == "N/A" | Death_Age_highSDI == "n/a" | Death_Age_highSDI == "N/a"| Death_Age_highSDI == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Death_Age_highSDI$location<- factor(Death_Age_highSDI$location)  
Death_Age_highSDI$age <- factor(Death_Age_highSDI$age)  
Death_Age_highSDI <- subset(Death_Age_highSDI, select =-c(1,3,5,6,9,10)) 

# Age Deaths Middle SDI
Death_Age_middleSDI[Death_Age_middleSDI == "" | Death_Age_middleSDI == " "] <- NA # Makes any missing data a NA. 
Death_Age_middleSDI[Death_Age_middleSDI == "N/A" | Death_Age_middleSDI == "n/a" | Death_Age_middleSDI == "N/a"| Death_Age_middleSDI == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Death_Age_middleSDI$location<- factor(Death_Age_middleSDI$location)  
Death_Age_middleSDI$age <- factor(Death_Age_middleSDI$age)  
Death_Age_middleSDI <- subset(Death_Age_middleSDI, select =-c(1,3,5,6,9,10)) 

# Age Deaths Low SDI
Death_Age_lowSDI[Death_Age_lowSDI == "" | Death_Age_lowSDI == " "] <- NA # Makes any missing data a NA. 
Death_Age_lowSDI[Death_Age_lowSDI == "N/A" | Death_Age_lowSDI == "n/a" | Death_Age_lowSDI == "N/a"| Death_Age_lowSDI == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Death_Age_lowSDI$location<- factor(Death_Age_lowSDI$location)  
Death_Age_lowSDI$age <- factor(Death_Age_lowSDI$age)  
Death_Age_lowSDI <- subset(Death_Age_lowSDI, select =-c(1,3,5,6,9,10)) 

# Age DALYs 
Age_DALY[Age_DALY == "" | Age_DALY == " "] <- NA # Makes any missing data a NA. 
Age_DALY[Age_DALY == "N/A" | Age_DALY == "n/a" | Age_DALY == "N/a"| Age_DALY == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Age_DALY$location_name <- factor(Age_DALY$location_name)  
Age_DALY$age_name <- factor(Age_DALY$age_name)   
Age_DALY <- subset(Age_DALY, select =-c(1,3,5,6,7,9,11,15,16)) 

# Age YLLs 
Age_YLL[Age_YLL == "" | Age_YLL == " "] <- NA # Makes any missing data a NA. 
Age_YLL[Age_YLL == "N/A" | Age_YLL == "n/a" | Age_YLL == "N/a"| Age_YLL == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Age_YLL$location_name <- factor(Age_YLL$location_name)  
Age_YLL$age_name <- factor(Age_YLL$age_name)   
Age_YLL <- subset(Age_YLL, select =-c(1,3,5,6,7,9,11,15,16)) 

# Age Prevalence 
Age_Prev[Age_Prev == "" | Age_Prev == " "] <- NA # Makes any missing data a NA. 
Age_Prev[Age_Prev == "N/A" | Age_Prev == "n/a" | Age_Prev == "N/a"| Age_Prev == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Age_Prev$location_name <- factor(Age_Prev$location_name)
Age_Prev$age_name <- factor(Age_Prev$age_name) 
Age_Prev <- subset(Age_Prev, select =-c(1,3,5,6,7,9,11,15,16))

# Age Prevalence High SDI
Prev_Age_highSDI[Prev_Age_highSDI == "" | Prev_Age_highSDI == " "] <- NA # Makes any missing data a NA. 
Prev_Age_highSDI[Prev_Age_highSDI == "N/A" | Prev_Age_highSDI == "n/a" | Prev_Age_highSDI == "N/a"| Prev_Age_highSDI == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Prev_Age_highSDI$location<- factor(Prev_Age_highSDI$location)  
Prev_Age_highSDI$age <- factor(Prev_Age_highSDI$high_SDI_age)  
Prev_Age_highSDI <- subset(Prev_Age_highSDI, select =-c(1,3,5,6,9,10)) 

# Age Prevalence Middle SDI
Prev_Age_middleSDI[Prev_Age_middleSDI == "" | Prev_Age_middleSDI == " "] <- NA # Makes any missing data a NA. 
Prev_Age_middleSDI[Prev_Age_middleSDI == "N/A" | Prev_Age_middleSDI == "n/a" | Prev_Age_middleSDI == "N/a"| Prev_Age_middleSDI == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Prev_Age_middleSDI$location<- factor(Prev_Age_middleSDI$location)  
Prev_Age_middleSDI$age <- factor(Prev_Age_middleSDI$middle_SDI_age)  
Prev_Age_middleSDI <- subset(Prev_Age_highSDI, select =-c(1,3,5,6,9,10)) 

# Age Prevalence Low SDI
Prev_Age_lowSDI[Prev_Age_lowSDI == "" | Prev_Age_lowSDI == " "] <- NA # Makes any missing data a NA. 
Prev_Age_lowSDI[Prev_Age_lowSDI == "N/A" | Prev_Age_lowSDI == "n/a" | Prev_Age_lowSDI == "N/a"| Prev_Age_lowSDI == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
Prev_Age_lowSDI$location<- factor(Prev_Age_lowSDI$location)  
Prev_Age_lowSDI$age <- factor(Prev_Age_lowSDI$low_SDI_age)  
Prev_Age_lowSDI <- subset(Prev_Age_lowSDI, select =-c(1,3,5,6,9,10)) 

# Check for missing data
##### Measure data #############################################################
par(bg = "#d5e4eb") # Changes colour of plot background. 
missmap(Deaths, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(DALYs, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(YLLs, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Prev, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 

##### Age data #################################################################
missmap(Age_death, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Death_Age_highSDI, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data).
missmap(Death_Age_middleSDI, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Death_Age_lowSDI, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Age_DALY, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Age_YLL, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Age_Prev, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Prev_Age_highSDI, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Prev_Age_middleSDI, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
missmap(Prev_Age_lowSDI, col=c("red3", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 

##### EXPLORATORY DATA ANALYSIS ################################################
# To rearrange locations in bar graphs  
Deaths$location_name <- factor(Deaths$location_name, levels = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"))
DALYs$location_name <- factor(DALYs$location_name, levels = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"))
Prev$location_name <- factor(Prev$location_name, levels = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"))
YLLs$location_name <- factor(YLLs$location_name, levels = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"))

#### Bar graphs : Cancer Deaths, DALYs, YLLs, Prevalence rates ################# 
Cdeaths <- ggplot(Deaths, aes(x =location_name, y = val, fill = location_name)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Death Rate", x= "", y= "Deaths") +
  scale_fill_manual(values = c("High SDI" = "#1380A1", "High-middle SDI" = "mediumslateblue",
                               "Middle SDI" = "forestgreen", "Low-middle SDI" = "orange",
                               "Low SDI" = "red3")) +
  scale_y_continuous(labels = comma) +
  guides(fill = guide_legend(title = "Level of Development")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top",
        legend.background = element_rect(colour = 1),
        plot.background = element_rect(fill = "#d5e4eb")) # Background color of the plot

CDALY <- ggplot(DALYs, aes(x =location_name, y = val, fill = location_name)) + 
  geom_bar(stat = "identity") + 
  labs(title = "DALYs Rate", x= "", y= "DALYs") +
  scale_fill_manual(values = c("High SDI" = "#1380A1", "High-middle SDI" = "mediumslateblue",
                               "Middle SDI" = "forestgreen", "Low-middle SDI" = "orange",
                               "Low SDI" = "red3")) +
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#d5e4eb"))

Cprev <- ggplot(Prev, aes(x =location_name, y = val, fill = location_name)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Prevalence Rate", x= "", y= "Prevalence") +
  scale_fill_manual(values = c("High SDI" = "#1380A1", "High-middle SDI" = "mediumslateblue",
                               "Middle SDI" = "forestgreen", "Low-middle SDI" = "orange",
                               "Low SDI" = "red3")) +
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#d5e4eb"))

CYLL <- ggplot(YLLs, aes(x =location_name, y = val, fill = location_name)) + 
  geom_bar(stat = "identity") + 
  labs(title = "YLLs Rate", x= "", y= "YLLs") +
  scale_fill_manual(values = c("High SDI" = "#1380A1", "High-middle SDI" = "mediumslateblue",
                               "Middle SDI" = "forestgreen", "Low-middle SDI" = "orange",
                               "Low SDI" = "red3")) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#d5e4eb")) 

ggarrange(Cdeaths, CDALY, Cprev,CYLL + rremove("x.text"), 
          ncol = 2, nrow = 2, 
          common.legend = TRUE, legend="top") # This code arranges the above figures into one plot in order to see all figures better and easier.

#### Box plots: Cancer Deaths, DALYs, YLLs, Prevalence rates ################### 
box1 <- ggplot(Deaths, aes(y = val)) +
  stat_boxplot(geom = "errorbar", width = 0.15) + 
  labs(title = "Death Rate", x= "", y= "Number of Deaths") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) + 
  geom_boxplot(fill = '#1380A1') 

box2 <- ggplot(DALYs, aes(y = val)) +
  stat_boxplot(geom = "errorbar", width = 0.15) + 
  labs(title = "DALYs Rate", x= "", y= "Number of DALYs") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) + 
  geom_boxplot(fill = '#1380A1') 

box3 <- ggplot(Prev, aes(y = val)) +
  stat_boxplot(geom = "errorbar", width = 0.15) + 
  labs(title = "Prevalence Rate", x= "", y= "Prevalence") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) + 
  geom_boxplot(fill = '#1380A1') 

box4 <- ggplot(YLLs, aes(y = val)) +
  stat_boxplot(geom = "errorbar", width = 0.15) + 
  labs(title = "YLLs Rate", x= "", y= "Number of YLLs") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) + 
  geom_boxplot(fill = '#1380A1') 

ggarrange(box1, box2, box3,box4 + rremove("x.text"), 
          ncol = 2, nrow = 2) # This code arranges the above figures into one plot in order to see all figures better and easier.

#### Bar plots: high SDI and low SDI 2010-2019 ################### 

p<-ggplot(data=sdi_2010_2019, aes(x=sdi_2010_2019$year, y=sdi_2010_2019$`low-SDI`)) +
  geom_bar(stat="identity", fill='#1380A1')+
  theme(plot.background = element_rect(fill = "#d5e4eb"))

p + labs(title="Plot of low SDI for the years 2010 to 2019", 
         x="year", y = "total number of countries with a SDI score of <0.5")

r<-ggplot(data=sdi_2010_2019, aes(x=sdi_2010_2019$year, y=sdi_2010_2019$`high-SDI`)) +
  geom_bar(stat="identity", fill="red3") +
  theme(plot.background = element_rect(fill = "#d5e4eb"))

r + labs(title="Plot of high SDI for the years 2010 to 2019", 
         x="year", y = "total number of countries with a SDI score of >0.5")

grid.arrange(p + labs(title="Plot of low SDI for the years 2010 to 2019", 
                      x="year", y = "total number of countries with a SDI score of <0.5"), r + labs(title="Plot of high SDI for the years 2010 to 2019", 
                                              x="year", y = "total number of countries with a SDI score of >0.5"),  ncol = 1, nrow = 2)

sum(sdi_2010_2019$`low-SDI`)
sum(sdi_2010_2019$`high-SDI`)

##### AGE DATA ANALYSIS ########################################################
# Age Deaths 
Death_Age_highSDI_plot <- ggplot(Death_Age_highSDI, aes(x = year, y = val, color = age)) +
  labs(title = "High SDI Age Death Rate",y = "Death Rate", x = "Years") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line()

Death_Age_middleSDI_plot <- ggplot(Death_Age_middleSDI, aes(x = year, y = val, color = age)) +
  labs(title = "Middle SDI Age Death Rate",y = "Death Rate", x = "Years") +
  scale_fill_discrete(name = "Age Group")+
  theme(legend.position = "top", 
        legend.background = element_rect(colour = 1),
        plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line() 

Death_Age_lowSDI_plot <- ggplot(Death_Age_lowSDI, aes(x = year, y = val, color = age)) +
  labs(title = "Low SDI Age Death Rate",y = "Death Rate", x = "Years") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line() 

# Age Prevalence 
Prev_Age_highSDI_plot <- ggplot(Prev_Age_highSDI, aes(x = year, y = val, color = high_SDI_age)) +
  labs(title = "High SDI Age Prevalence Rate",y = "Prevalence Rate", x = "Years") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line() 

Prev_Age_middleSDI_plot <- ggplot(Prev_Age_middleSDI, aes(x = year, y = val, color = middle_SDI_age)) +
  labs(title = "Middle SDI Age Prevalence Rate",y = "Prevalence Rate", x = "Years") +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line() 

Prev_Age_lowSDI_plot <- ggplot(Prev_Age_lowSDI, aes(x = year, y = val, color = low_SDI_age)) +
  labs(title = "Low SDI Age Prevalence Rate",y = "Prevalence Rate", x = "Years") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line() 

ggarrange(Death_Age_highSDI_plot, Death_Age_middleSDI_plot, Death_Age_lowSDI_plot,
          Prev_Age_highSDI_plot,Prev_Age_middleSDI_plot, Prev_Age_lowSDI_plot + rremove("x.text"), 
          ncol = 3, nrow = 2,
          common.legend = TRUE, legend="top") # This code arranges the above figures into one plot in order to see all figures better and easier.

##### TIME SERIES ANALYSIS #####################################################
##### TIME SERIES DATA CODING/ CLEANING ########################################
# Deaths 
Deaths_h<- Deaths[!(Deaths$location_name=="High-middle SDI" | Deaths$location_name=="Middle SDI" | 
                     Deaths$location_name=="Low-middle SDI" | Deaths$location_name=="Low SDI"),]
Deaths_high<- Deaths_h %>% arrange(year) # rearranges year data my year from 2010 to 2019 


Deaths_m<- Deaths[!(Deaths$location_name=="High-middle SDI" | Deaths$location_name=="High SDI" | 
                      Deaths$location_name=="Low-middle SDI" | Deaths$location_name=="Low SDI"),]
Deaths_middle<- Deaths_m %>% arrange(year) # rearranges year data my year from 2010 to 2019 

Deaths_l <- Deaths[!(Deaths$location_name=="High-middle SDI" | Deaths$location_name=="Middle SDI" | 
                          Deaths$location_name=="Low-middle SDI" | Deaths$location_name=="High SDI"),]
Deaths_low<- Deaths_l %>% arrange(year) # rearranges year data my year from 2010 to 2019 

# DALYs
DALYs_h <- DALYs[!(DALYs$location_name=="High-middle SDI" | DALYs$location_name=="Middle SDI" | 
                     DALYs$location_name=="Low-middle SDI"| DALYs$location_name=="Low SDI"),]
DALYs_high<- DALYs_h %>% arrange(year) # rearranges year data my year from 2010 to 2019 

DALYs_m <- DALYs[!(DALYs$location_name=="High-middle SDI" | DALYs$location_name=="High SDI" | 
                     DALYs$location_name=="Low-middle SDI"| DALYs$location_name=="Low SDI"),]
DALYs_middle<- DALYs_m %>% arrange(year) # rearranges year data my year from 2010 to 2019 

DALYs_l<- DALYs[!(DALYs$location_name=="High-middle SDI" | DALYs$location_name=="Middle SDI" | 
                        DALYs$location_name=="Low-middle SDI"| DALYs$location_name=="High SDI"),]
DALYs_low<- DALYs_l %>% arrange(year) # rearranges year data my year from 2010 to 2019 

# YLLs
YLLs_h <- YLLs[!(YLLs$location_name=="High-middle SDI" | YLLs$location_name=="Middle SDI" | 
                   YLLs$location_name=="Low-middle SDI"| YLLs$location_name=="Low SDI"),]
YLLs_high<- YLLs_h %>% arrange(year) # rearranges year data my year from 2010 to 2019 

YLLs_m <- YLLs[!(YLLs$location_name=="High-middle SDI" | YLLs$location_name=="High SDI" | 
                   YLLs$location_name=="Low-middle SDI"| YLLs$location_name=="Low SDI"),]
YLLs_middle<- YLLs_m %>% arrange(year) # rearranges year data my year from 2010 to 2019 

YLLs_l<- YLLs[!(YLLs$location_name=="High-middle SDI" | YLLs$location_name=="Middle SDI" | 
                      YLLs$location_name=="Low-middle SDI"| YLLs$location_name=="High SDI"),]
YLLs_low<- YLLs_l %>% arrange(year)

# Prevalence 
Prev_h <- Prev[!(Prev$location_name=="High-middle SDI" | Prev$location_name=="Middle SDI" | 
                 Prev$location_name=="Low-middle SDI"| Prev$location_name=="Low SDI"),]
Prev_high<- Prev_h %>% arrange(year) # rearranges year data my year from 2010 to 2019 

Prev_m <- Prev[!(Prev$location_name=="High-middle SDI" | Prev$location_name=="High SDI" | 
                   Prev$location_name=="Low-middle SDI"| Prev$location_name=="Low SDI"),]
Prev_middle<- Prev_m %>% arrange(year) # rearranges year data my year from 2010 to 2019 

Prev_l <- Prev[!(Prev$location_name=="High-middle SDI" | Prev$location_name=="Middle SDI" | 
                      Prev$location_name=="Low-middle SDI"| Prev$location_name=="High SDI"),]
Prev_low<- Prev_l %>% arrange(year) # rearranges year data my year from 2010 to 2019 

##### TIME SERIES DEATHS #######################################################
# High SDI 
Death_high.ts <- ts(Deaths_high["val"], start = c(2010),end=c(2019), frequency = 1)
str(Death_high.ts)
Death_high.ts
deaths_high_plot<- autoplot(Death_high.ts) + ylab('High SDI Death Rate') + 
  labs(title = "High SDI Death Rate",y = "Death Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

# Middle SDI 
Death_middle.ts <- ts(Deaths_middle["val"], start = c(2010),end=c(2019), frequency = 1)
str(Death_middle.ts)
Death_middle.ts
deaths_middle_plot<- autoplot(Death_middle.ts) + ylab('Middle SDI Death Rate') + 
  labs(title = "Middle SDI Death Rate",y = "Death Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

# Low SDI
Death_low.ts <- ts(Deaths_low["val"], start = c(2010),end=c(2019), frequency = 1)
str(Death_low.ts)
Death_low.ts
deaths_low_plot<- autoplot(Death_low.ts) + ylab('Low SDI Death Rate') + 
  labs(title = "Low SDI Death Rate",y = "Death Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

##### TIME SERIES DALYs ########################################################
# High SDI 
DALYs_high.ts <- ts(DALYs_high["val"], start = c(2010),end=c(2019), frequency = 1)
str(DALYs_high.ts)
DALYs_high.ts
DALYs_high_plot<- autoplot(DALYs_high.ts) + ylab('High SDI DALYs Rate') + 
  labs(title = "High SDI DALYs Rate",y = "DALYs Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

# Middle SDI 
DALYs_middle.ts <- ts(DALYs_middle["val"], start = c(2010),end=c(2019), frequency = 1)
str(DALYs_middle.ts)
DALYs_middle.ts
DALYs_middle_plot<- autoplot(DALYs_middle.ts) + ylab('Middle SDI DALYs Rate') + 
  labs(title = "Middle SDI DALYs Rate",y = "DALYs Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

# Low SDI
DALYs_low.ts <- ts(DALYs_low["val"], start = c(2010),end=c(2019), frequency = 1)
str(DALYs_low.ts)
DALYs_low.ts
DALYs_low_plot<- autoplot(DALYs_low.ts) + ylab('Low SDI DALYs Rate') + 
  labs(title = "Low SDI DALYs Rate",y = "DALYs Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

##### TIME SERIES YLLs #########################################################
# High SDI 
YLLs_high.ts <- ts(YLLs_high["val"], start = c(2010),end=c(2019), frequency = 1)
str(YLLs_high.ts)
YLLs_high.ts
YLLs_high_plot<- autoplot(YLLs_high.ts) + ylab('High SDI YLLs Rate') + 
  labs(title = "High SDI YLLs Rate",y = "YLLs Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

# Middle SDI 
YLLs_middle.ts <- ts(YLLs_middle["val"], start = c(2010),end=c(2019), frequency = 1)
str(YLLs_middle.ts)
YLLs_middle.ts
YLLs_middle_plot<- autoplot(YLLs_middle.ts) + ylab('Middle SDI YLLs Rate') + 
  labs(title = "Middle SDI YLLs Rate",y = "YLLs Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

# Low SDI
YLLs_low.ts <- ts(YLLs_low["val"], start = c(2010),end=c(2019), frequency = 1)
str(YLLs_low.ts)
YLLs_low.ts
YLLs_low_plot<- autoplot(YLLs_low.ts) + ylab('Low SDI YLLs Rate') + 
  labs(title = "Low SDI YLLs Rate",y = "YLLs Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

##### TIME SERIES Prevalence ###################################################
# High SDI 
Prev_high.ts <- ts(Prev_high["val"], start = c(2010),end=c(2019), frequency = 1)
str(Prev_high.ts)
Prev_high.ts
Prev_high_plot<- autoplot(Prev_high.ts) + ylab('High Prevalence YLLs Rate') + 
  labs(title = "High SDI Prevalence Rate",y = "Prevalence Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

# Middle SDI 
Prev_middle.ts <- ts(Prev_middle["val"], start = c(2010),end=c(2019), frequency = 1)
str(Prev_middle.ts)
Prev_middle.ts
Prev_middle_plot<- autoplot(Prev_middle.ts) + ylab('Middle Prevalence YLLs Rate') + 
  labs(title = "Middle SDI Prevalence Rate",y = "Prevalence Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")
  
# Low SDI
Prev_low.ts <- ts(Prev_low["val"], start = c(2010),end=c(2019), frequency = 1)
str(Prev_low.ts)
Prev_low.ts
Prev_low_plot<- autoplot(Prev_low.ts) + ylab('Low Prevalence YLLs Rate') + 
  labs(title = "Low SDI Prevalence Rate",y = "Prevalence Rate", x = "Years") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")
 
#tobacco time series analysis

tobacco.daly.high.sdi<- IHME.GBD_2019_DATA.4ba7dc1e.1
tobacco.daly.high<- tobacco.daly.high.sdi %>% arrange(year)
tobacco.daly.high.ts <- ts(tobacco.daly.high["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.daly.high.plot<- autoplot(tobacco.daly.high.ts)+ 
  ylab('DALY Rate of tobacco risk') + 
  labs(title="DALY in high countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

tobacco.daly.med.sdi<- IHME.GBD_2019_DATA.1130d03f.1
tobacco.daly.med<- tobacco.daly.med.sdi %>% arrange(year)
tobacco.daly.med.ts <- ts(tobacco.daly.med["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.daly.med.plot<- autoplot(tobacco.daly.med.ts)+ 
  ylab('DALY Rate of tobacco risk') + 
  labs(title="DALY in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

tobacco.daly.low<- tobacco.daly.low.sdi %>% arrange(year)

tobacco.daly.low.ts <- ts(tobacco.daly.low["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.daly.low.plot<- autoplot(tobacco.daly.low.ts)+ 
  ylab('DALY Rate of tobacco risk') + 
  labs(title="DALY in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

tobacco.deaths.high.sdi<- IHME.GBD_2019_DATA.be5ea5c6.1

tobacco.deaths.high<- tobacco.deaths.high.sdi %>% arrange(year)

tobacco.deaths.high.ts <- ts(tobacco.deaths.high["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.deaths.high.plot<- autoplot(tobacco.deaths.high.ts)+ 
  ylab('Death Rate of tobacco risk') + 
  labs(title="Deaths in High SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1") 

tobacco.deaths.med.sdi<- IHME.GBD_2019_DATA.3ad13977.1

tobacco.deaths.med<- tobacco.deaths.med.sdi %>% arrange(year)

tobacco.deaths.med.ts <- ts(tobacco.deaths.med["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.deaths.med.plot<- autoplot(tobacco.deaths.med.ts)+ 
  ylab('Death Rate of tobacco risk') + 
  labs(title="Deaths in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

tobacco.deaths.low.sdi<- IHME.GBD_2019_DATA.75556e39.1

tobacco.deaths.low<- tobacco.deaths.low.sdi %>% arrange(year)

tobacco.deaths.low.ts <- ts(tobacco.deaths.low["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.deaths.low.plot<- autoplot(tobacco.deaths.low.ts)+ 
  ylab('Death Rate of tobacco risk') + 
  labs(title="Deaths in Low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

tobacco.yld.low.sdi<- IHME.GBD_2019_DATA.8dbbab14.1

tobacco.yld.low<- tobacco.yld.low.sdi %>% arrange(year)

tobacco.yld.low.ts <- ts(tobacco.yld.low["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.yld.low.plot<- autoplot(tobacco.yld.low.ts)+ 
  ylab('YLD Rate of tobacco risk') + 
  labs(title="YLD in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

tobacco.yld.med.sdi<- IHME.GBD_2019_DATA.4ffeb157.1

tobacco.yld.med<- tobacco.yld.med.sdi %>% arrange(year)

tobacco.yld.med.ts <- ts(tobacco.yld.med["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.yld.med.plot<- autoplot(tobacco.yld.med.ts)+ 
  ylab('YLD Rate of tobacco') + 
  labs(title="YLD in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

tobacco.yld.high.sdi<- IHME.GBD_2019_DATA.23e75cd4.1

tobacco.yld.high<- tobacco.yld.high.sdi %>% arrange(year)

tobacco.yld.high.ts <- ts(tobacco.yld.high["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.yld.high.plot<- autoplot(tobacco.yld.high.ts)+ 
  ylab('YLD Rate of tobacco risk') + 
  labs(title="YLD in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

tobacco.yll.high.sdi<- IHME.GBD_2019_DATA.87e34f9b.1

tobacco.yll.high<- tobacco.yll.high.sdi %>% arrange(year)

tobacco.yll.high.ts <- ts(tobacco.yll.high["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.yll.high.plot<- autoplot(tobacco.yll.high.ts)+ 
  ylab('YLL Rate of tobacco risk') + 
  labs(title="YLL in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

tobacco.yll.med.sdi<- IHME.GBD_2019_DATA.ffccd1d8.1

tobacco.yll.med<- tobacco.yll.med.sdi %>% arrange(year)

tobacco.yll.med.ts <- ts(tobacco.yll.med["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.yll.med.plot<- autoplot(tobacco.yll.med.ts)+ 
  ylab('YLL Rate of tobacco risk') + 
  labs(title="YLL in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

tobacco.yll.low.sdi<- IHME.GBD_2019_DATA.62b70de9.1

tobacco.yll.low<- tobacco.yll.low.sdi %>% arrange(year)

tobacco.yll.low.ts <- ts(tobacco.yll.low["val"], start = c(2010),end=c(2019), frequency = 1)

tobacco.yll.low.plot<-autoplot(tobacco.yll.low.ts)+ 
  ylab('YLL Rate of tobacco risk') + 
  labs(title="YLL in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

# air pollution time series analysis 

air.daly.high.sdi<- IHME.GBD_2019_DATA.c30c7f1b.1

air.daly.high<- air.daly.high.sdi %>% arrange(year)

air.daly.high.ts <- ts(air.daly.high["val"], start = c(2010),end=c(2019), frequency = 1)

air.daly.high.plot<- autoplot(air.daly.high.ts)+ 
  ylab('DALY Rate of air pollution risk') + 
  labs(title="DALY in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

air.daly.med.sdi<- IHME.GBD_2019_DATA.d23e2034.1

air.daly.med<- air.daly.med.sdi %>% arrange(year)

air.daly.med.ts <- ts(air.daly.med["val"], start = c(2010),end=c(2019), frequency = 1)

air.daly.med.plot<- autoplot(air.daly.med.ts)+ 
  ylab('DALY Rate of air pollution risk') + 
  labs(title="DALY medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

air.daly.low.sdi<- IHME.GBD_2019_DATA.fdf39169.1

air.daly.low<- air.daly.low.sdi %>% arrange(year)

air.daly.low.ts <- ts(air.daly.low["val"], start = c(2010),end=c(2019), frequency = 1)

air.daly.low.plot<-autoplot(air.daly.low.ts)+ 
  ylab('DALY Rate of air pollution risk') + 
  labs(title="DALY in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

air.deaths.low.sdi<- IHME.GBD_2019_DATA.605db46d.1

air.deaths.low<- air.deaths.low.sdi %>% arrange(year)

air.deaths.low.ts <- ts(air.deaths.low["val"], start = c(2010),end=c(2019), frequency = 1)

air.deaths.low.plot<-autoplot(air.deaths.low.ts)+ 
  ylab('Death Rate of air pollution risk') + 
  labs(title="Deaths in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

air.deaths.med.sdi<- IHME.GBD_2019_DATA.3ea25e10.1

air.deaths.med<- air.deaths.med.sdi %>% arrange(year)

air.deaths.med.ts <- ts(air.deaths.med["val"], start = c(2010),end=c(2019), frequency = 1)

air.deaths.med.plot<-autoplot(air.deaths.med.ts)+ 
  ylab('Death Rate of air pollution risk') + 
  labs(title="Deaths in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

air.deaths.high.sdi<- IHME.GBD_2019_DATA.e010848f.1

air.deaths.high<- air.deaths.high.sdi %>% arrange(year)

air.deaths.high.ts <- ts(air.deaths.high["val"], start = c(2010),end=c(2019), frequency = 1)

air.deaths.high.plot<-autoplot(air.deaths.high.ts)+ 
  ylab('Death Rate of air pollution risk') + 
  labs(title="Deaths in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

air.yld.high.sdi<- IHME.GBD_2019_DATA.aa70a842.1

air.yld.high<- air.yld.high.sdi %>% arrange(year)

air.yld.high.ts <- ts(air.yld.high["val"], start = c(2010),end=c(2019), frequency = 1)

air.yld.high.plot<-autoplot(air.yld.high.ts)+ 
  ylab('YLD Rate of air pollution risk') + 
  labs(title="YLD in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

air.yld.med.sdi<- IHME.GBD_2019_DATA.1f74192a.1

air.yld.med<- air.yld.med.sdi %>% arrange(year)

air.yld.med.ts <- ts(air.yld.med["val"], start = c(2010),end=c(2019), frequency = 1)

air.yld.med.plot<-autoplot(air.yld.med.ts)+ 
  ylab('YLD Rate of air pollution risk') + 
  labs(title="YLD in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

air.yld.low.sdi<- IHME.GBD_2019_DATA.5eac6de4.1

air.yld.low<- air.yld.low.sdi %>% arrange(year)

air.yld.low.ts <- ts(air.yld.low["val"], start = c(2010),end=c(2019), frequency = 1)

air.yld.low.plot<-autoplot(air.yld.low.ts)+ 
  ylab('YLD Rate of air pollution risk') + 
  labs(title="YLD in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

air.yll.low.sdi<- IHME.GBD_2019_DATA.dcb541e4.1

air.yll.low<- air.yll.low.sdi %>% arrange(year)

air.yll.low.ts <- ts(air.yll.low["val"], start = c(2010),end=c(2019), frequency = 1)

air.yll.low.plot<-autoplot(air.yll.low.ts)+ 
  ylab('YLL Rate of air pollution risk') + 
  labs(title="YLL in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

air.yll.med.sdi<- IHME.GBD_2019_DATA.4b8528af.1

air.yll.med<- air.yll.med.sdi %>% arrange(year)

air.yll.med.ts <- ts(air.yll.med["val"], start = c(2010),end=c(2019), frequency = 1)

air.yll.med.plot<-autoplot(air.yll.med.ts)+ 
  ylab('YLL Rate of air pollution risk') + 
  labs(title="YLL in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

air.yll.high.sdi<- IHME.GBD_2019_DATA.755892cd.1

air.yll.high<- air.yll.high.sdi %>% arrange(year)

air.yll.high.ts <- ts(air.yll.high["val"], start = c(2010),end=c(2019), frequency = 1)

air.yll.high.plot<-autoplot(air.yll.high.ts)+ 
  ylab('YLL Rate of air pollution risk') + 
  labs(title="YLL in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

#occupational risk time series analysis 

occupational.daly.high.sdi<- IHME.GBD_2019_DATA.26f57601.1

occupational.daly.high<- occupational.daly.high.sdi %>% arrange(year)

occupational.daly.high.ts <- ts(occupational.daly.high["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.daly.high.plot<- autoplot(occupational.daly.high.ts)+ 
  ylab('DALY Rate of occupational risk') + 
  labs(title="DALY in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

occupational.daly.med.sdi<- IHME.GBD_2019_DATA.ada38869.1

occupational.daly.med<- occupational.daly.med.sdi %>% arrange(year)

occupational.daly.med.ts <- ts(occupational.daly.med["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.daly.med.plot<- autoplot(occupational.daly.med.ts)+ 
  ylab('DALY Rate of occupational risk') + 
  labs(title="DALY in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

occupational.daly.low.sdi<- IHME.GBD_2019_DATA.d94da778.1

occupational.daly.low<- occupational.daly.low.sdi %>% arrange(year)

occupational.daly.low.ts <- ts(occupational.daly.low["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.daly.low.plot<-autoplot(occupational.daly.low.ts)+ 
  ylab('DALY Rate of occupational risk') + 
  labs(title="DALY in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

occupational.deaths.low.sdi<- IHME.GBD_2019_DATA.35a05d54.1

occupational.deaths.low<- occupational.deaths.low.sdi %>% arrange(year)

occupational.deaths.low.ts <- ts(occupational.deaths.low["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.deaths.low.plot<-autoplot(occupational.deaths.low.ts)+ 
  ylab('Death Rate of occupational risk') + 
  labs(title="Deaths in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

occupational.deaths.med.sdi<- IHME.GBD_2019_DATA.aae670ab.1

occupational.deaths.med<- occupational.deaths.med.sdi %>% arrange(year)

occupational.deaths.med.ts <- ts(occupational.deaths.med["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.deaths.med.plot<-autoplot(occupational.deaths.med.ts)+ 
  ylab('Death Rate of occupational risk') + 
  labs(title="Deaths in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

occupational.deaths.high.sdi<- IHME.GBD_2019_DATA.d1343b93.1

occupational.deaths.high<- occupational.deaths.high.sdi %>% arrange(year)

occupational.deaths.high.ts <- ts(occupational.deaths.high["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.deaths.high.plot<-autoplot(occupational.deaths.high.ts)+ 
  ylab('Death Rate of occupational risk') + 
  labs(title="Deaths in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

occupational.yld.high.sdi<- IHME.GBD_2019_DATA.7dcaf738.1

occupational.yld.high<- occupational.yld.high.sdi %>% arrange(year)

occupational.yld.high.ts <- ts(occupational.yld.high["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.yld.high.plot<-autoplot(occupational.yld.high.ts)+ 
  ylab('YLD Rate of occupational risk') + 
  labs(title="YLD in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

occupational.yld.med.sdi<- IHME.GBD_2019_DATA.3c3d7321.1

occupational.yld.med<- occupational.yld.med.sdi %>% arrange(year)

occupational.yld.med.ts <- ts(occupational.yld.med["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.yld.med.plot<-autoplot(occupational.yld.med.ts)+ 
  ylab('YLD Rate of occupational risk') + 
  labs(title="YLD in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

occupational.yld.low.sdi<- IHME.GBD_2019_DATA.cafd2fdc.1
occupational.yld.low<- occupational.yld.low.sdi %>% arrange(year)
occupational.yld.low.ts <- ts(occupational.yld.low["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.yld.low.plot<-autoplot(occupational.yld.low.ts)+ 
  ylab('YLD Rate of occupational risk') + 
  labs(title="YLD in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

occupational.yll.low.sdi<- IHME.GBD_2019_DATA.18ecdeb8.1

occupational.yll.low<- occupational.yll.low.sdi %>% arrange(year)

occupational.yll.low.ts <- ts(occupational.yll.low["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.yll.low.plot<-autoplot(occupational.yll.low.ts)+ 
  ylab('YLL Rate of occupational risk') + 
  labs(title="YLL in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

occupational.yll.med.sdi<- IHME.GBD_2019_DATA.3c8d4c05.1

occupational.yll.med<- occupational.yll.med.sdi %>% arrange(year)

occupational.yll.med.ts <- ts(occupational.yll.med["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.yll.med.plot<-autoplot(occupational.yll.med.ts)+ 
  ylab('YLL Rate of occupational risk') + 
  labs(title="YLL in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

occupational.yll.high.sdi<- IHME.GBD_2019_DATA.ce2e10d3.1

occupational.yll.high<- occupational.yll.high.sdi %>% arrange(year)

occupational.yll.high.ts <- ts(occupational.yll.high["val"], start = c(2010),end=c(2019), frequency = 1)

occupational.yll.high.plot<-autoplot(occupational.yll.high.ts)+ 
  ylab('YLL Rate of occupational risk') + 
  labs(title="YLL in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

#dietary risks time series analysis

dietary.daly.high.sdi<- IHME.GBD_2019_DATA.5db958bf.1

dietary.daly.high<- dietary.daly.high.sdi %>% arrange(year)

dietary.daly.high.ts <- ts(dietary.daly.high["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.daly.high.plot<-autoplot(dietary.daly.high.ts)+ 
  ylab('DALY Rate of dietary risk') + 
  labs(title="DALY in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

dietary.daly.med.sdi<- IHME.GBD_2019_DATA.b2888617.1

dietary.daly.med<- dietary.daly.med.sdi %>% arrange(year)

dietary.daly.med.ts <- ts(dietary.daly.med["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.daly.med.plot<-autoplot(dietary.daly.med.ts)+ 
  ylab('DALY Rate of dietary risk') + 
  labs(title="DALY in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

dietary.daly.low.sdi<- IHME.GBD_2019_DATA.b671674c.1

dietary.daly.low<- dietary.daly.low.sdi %>% arrange(year)

dietary.daly.low.ts <- ts(dietary.daly.low["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.daly.low.plot<-autoplot(dietary.daly.low.ts)+ 
  ylab('DALY Rate of dietary risk') + 
  labs(title="DALY in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

dietary.deaths.low.sdi<- IHME.GBD_2019_DATA.b9afdf0f.1

dietary.deaths.low<- dietary.deaths.low.sdi %>% arrange(year)

dietary.deaths.low.ts <- ts(dietary.deaths.low["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.deaths.low.plot<-autoplot(dietary.deaths.low.ts)+ 
  ylab('Death Rate of dietary risk') + 
  labs(title="Deaths in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

dietary.deaths.med.sdi<- IHME.GBD_2019_DATA.f10d62d7.1

dietary.deaths.med<- dietary.deaths.med.sdi %>% arrange(year)

dietary.deaths.med.ts <- ts(dietary.deaths.med["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.deaths.med.plot<-autoplot(dietary.deaths.med.ts)+ 
  ylab('Death Rate of dietary risk') + 
  labs(title="Deaths in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

dietary.deaths.high.sdi<- IHME.GBD_2019_DATA.09853661.1

dietary.deaths.high<- dietary.deaths.high.sdi %>% arrange(year)

dietary.deaths.high.ts <- ts(dietary.deaths.high["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.deaths.high.plot<-autoplot(dietary.deaths.high.ts)+ 
  ylab('Death Rate of dietary risk') + 
  labs(title="Deaths in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

dietary.yld.high.sdi<- IHME.GBD_2019_DATA.ac3b7537.1

dietary.yld.high<- dietary.yld.high.sdi %>% arrange(year)

dietary.yld.high.ts <- ts(dietary.yld.high["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.yld.high.plot<-autoplot(dietary.yld.high.ts)+ 
  ylab('YLD Rate of dietary risk') + 
  labs(title="YLD in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

dietary.yld.med.sdi<- IHME.GBD_2019_DATA.245a90ac.1

dietary.yld.med<- dietary.yld.med.sdi %>% arrange(year)

dietary.yld.med.ts <- ts(dietary.yld.med["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.yld.med.plot<-autoplot(dietary.yld.med.ts)+ 
  ylab('YLD Rate of dietary risk') + 
  labs(title="YLD in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

dietary.yld.low.sdi<- IHME.GBD_2019_DATA.95df5efc.1

dietary.yld.low<- dietary.yld.low.sdi %>% arrange(year)

dietary.yld.low.ts <- ts(dietary.yld.low["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.yld.low.plot<-autoplot(dietary.yld.low.ts)+ 
  ylab('YLD Rate of dietary risk') + 
  labs(title="YLD in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

dietary.yll.low.sdi<- IHME.GBD_2019_DATA.7afdd63c.1

dietary.yll.low<- dietary.yll.low.sdi %>% arrange(year)

dietary.yll.low.ts <- ts(dietary.yll.low["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.yll.low.plot<-autoplot(dietary.yll.low.ts)+ 
  ylab('YLL Rate of dietary risk') + 
  labs(title="YLL in low SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "red3")

dietary.yll.med.sdi<- IHME.GBD_2019_DATA.fdd41fdf.1

dietary.yll.med<- dietary.yll.med.sdi %>% arrange(year)

dietary.yll.med.ts <- ts(dietary.yll.med["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.yll.med.plot<-autoplot(dietary.yll.med.ts)+ 
  ylab('YLL Rate of dietary risk') + 
  labs(title="YLL in medium SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "forestgreen")

dietary.yll.high.sdi<- IHME.GBD_2019_DATA.03013f17.1

dietary.yll.high<- dietary.yll.high.sdi %>% arrange(year)

dietary.yll.high.ts <- ts(dietary.yll.high["val"], start = c(2010),end=c(2019), frequency = 1)

dietary.yll.high.plot<-autoplot(dietary.yll.high.ts)+ 
  ylab('YLL Rate of dietary risk') + 
  labs(title="YLL in high SDI countries") +
  theme(plot.background = element_rect(fill = "#d5e4eb")) +
  geom_line(color = "#1380A1")

##### TIME SERIES Plots ########################################################
grid.arrange(deaths_high_plot, deaths_middle_plot, deaths_low_plot,
             DALYs_high_plot,DALYs_middle_plot, DALYs_low_plot,
             YLLs_high_plot,YLLs_middle_plot, YLLs_low_plot,
             Prev_high_plot,Prev_middle_plot, Prev_low_plot, 
             ncol = 3, nrow = 4)
             
grid.arrange(tobacco.daly.high.plot, tobacco.daly.med.plot, tobacco.daly.low.plot,
             tobacco.deaths.high.plot, tobacco.deaths.med.plot, tobacco.deaths.low.plot,
             tobacco.yld.high.plot, tobacco.yld.med.plot, tobacco.yld.low.plot,
             tobacco.yll.high.plot, tobacco.yll.med.plot, tobacco.yll.low.plot, ncol = 3, nrow = 4)

grid.arrange(air.daly.high.plot, air.daly.med.plot, air.daly.low.plot,
             air.deaths.high.plot, air.deaths.med.plot, air.deaths.low.plot,
             air.yld.high.plot, air.yld.med.plot, air.yld.low.plot,
             air.yll.high.plot, air.yll.med.plot, air.yll.low.plot, ncol = 3, nrow = 4)

grid.arrange(occupational.daly.high.plot, occupational.daly.med.plot, occupational.daly.low.plot,
             occupational.deaths.high.plot, occupational.deaths.med.plot, occupational.deaths.low.plot,
             occupational.yld.high.plot, occupational.yld.med.plot, occupational.yld.low.plot,
             occupational.yll.high.plot, occupational.yll.med.plot, occupational.yll.low.plot, ncol = 3, nrow = 4)

grid.arrange(dietary.daly.high.plot, dietary.daly.med.plot, occupational.daly.low.plot,
             dietary.deaths.high.plot, dietary.deaths.med.plot, occupational.deaths.low.plot,
             dietary.yld.high.plot, dietary.yld.med.plot, occupational.yld.low.plot,
             dietary.yll.high.plot, dietary.yll.med.plot, occupational.yll.low.plot, ncol = 3, nrow = 4)   
             
             
##### classification ########################################################
decision_tree<- IHME.GBD_2019_DATA.f17ca5d2.1

view(decision_tree)

decision_tree$location <- factor(decision_tree$location)
decision_tree$rei <- factor(decision_tree$rei)

decision_tree <- na.omit(decision_tree) # removing any missing data.

tree_full2 <- rpart(decision_tree$location ~., data= decision_tree, control=rpart.control(minsplit=2, cp=0))

rpart.plot(tree_full2,extra=2,under=TRUE,varlen=0,faclen=0,cex=.7)

#creating a confusion table to display the tree based on Sex.
confusion_table<-table(decision_tree$location, 	predict(tree_full2,decision_tree,type="class"))
#viewing the confusion_table.
confusion_table

correct<- sum(diag(confusion_table))
error<- sum(confusion_table)-correct
accuracy<-correct / (correct+error);accuracy

library(caret);confusionMatrix(data = predict(tree_full2, decision_tree, type="class"), reference = decision_tree$location)

p1 <- predict(tree_full2, decision_tree, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(decision_tree$location, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')
