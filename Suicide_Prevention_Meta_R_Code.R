#Install the required packages for meta-analysis
install.packages("tidyverse")
install.packages("meta")
install.packages("metafor")
install.packages("devtools")
install.packages("janitor")

#Load the packages
library(tidyverse)
library(meta)
library(metafor)

#Install {dmetar}
devtools::install_github("MathiasHarrer/dmetar")

#Load the tidyverse package
library(tidyverse)

#Install janitor package to help with cleaning data names 
#install.packages("janitor")
library(janitor)

#Setting the folder as working directory
setwd("~/Documents/PhD/R/Doing_Meta_Analysis_in_R_Practice")

#Import dataset 
library(readxl)
SuicidePrevention <- read_excel("SuicidePrevention.xlsx")
View(SuicidePrevention) 

#Checking the data summary
glimpse(SuicidePrevention)

#Change class from character to numeric
SuicidePrevention$n.e <- as.numeric(SuicidePrevention$n.e)
SuicidePrevention$mean.e <- as.numeric(SuicidePrevention$mean.e)
SuicidePrevention$sd.e <- as.numeric(SuicidePrevention$sd.e)
SuicidePrevention$n.c <- as.numeric(SuicidePrevention$n.c)
SuicidePrevention$mean.c <- as.numeric(SuicidePrevention$mean.c)
SuicidePrevention$sd.c <- as.numeric(SuicidePrevention$sd.c)
SuicidePrevention$n.c <- as.numeric(SuicidePrevention$n.c)

#Change age_group and control to factor
SuicidePrevention$age_group <- as.factor(SuicidePrevention$age_group)
SuicidePrevention$control <- as.factor(SuicidePrevention$control)

#Check levels in age group
levels(SuicidePrevention$age_group)

#Shortening factor labels
new.factor.levels <- c("gen", "older")
new.factor.levels


