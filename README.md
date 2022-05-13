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

##### DATA INPUT ###############################################################
data <- read_csv("Diss data/Cancer pres  .csv") # Imports the data set. The header=True command tells RStudio to use the first row of the data file as the names of each variable/column. 
attach(data) # Attaches the data to your environment so that you can directly refer to the variable by name.
names(data) # Shows the name of variables in the data set.
summary(data) # Produces summary data (Min, Median, Mean and Max) for the individual variables. 
str(data) # Shows the observations and variables of the data.

##### DATA CODING/ CLEANING ####################################################
data[data == "" | data == " "] <- NA # Makes any missing data a NA. 
data[data == "N/A" | data == "n/a" | data == "N/a"| data == "-"] <- NA # Standardizes NA values (N/A, n/a, N/a) to NA. 
data$location <- factor(data$location)                                               

par(bg = "#d5e4eb") # Changes colour of plot background. 
missmap(data, col=c("red", "#1380A1"),legend = TRUE) # Checks for missing data by producing a map (red points indicts missing data). 
pct_miss(data) # Percent of ALL data frame values that are missing
pct_miss_case(data) # Percent of rows with any value missing
pct_complete_case(data) # Percent of rows that are complete (no values missing) 

data <- subset(data, select =-c(9,10)) # upper and lower values  

##### Create new data frames for each measure ##################################
YLLs <- data [c(1:110), c(1,2,3,4,5,6,7,8)] # Creates new data frame with just YLLs 
YLLs <- YLLs[!(YLLs$location=="Global" | YLLs$location=="World Bank Income Levels"),] # Removes global and world bank income levels 

Deaths <- data [c(111:220), c(1,2,3,4,5,6,7,8)] # Creates new data frame with just Deaths 
Deaths <- Deaths[!(Deaths$location=="Global" | Deaths$location=="World Bank Income Levels"),]

Prevalence <- data [c(331:440), c(1,2,3,4,5,6,7,8)] # Creates new data frame with just Prevalence  
Prevalence <- Prevalence[!(Prevalence$location=="Global" | Prevalence$location=="World Bank Income Levels"),]

DALYs <- data [c(441:550), c(1,2,3,4,5,6,7,8)] # Creates new data frame with just DALYs 
DALYs <- DALYs[!(DALYs$location=="Global" | DALYs$location=="World Bank Income Levels"),]



















