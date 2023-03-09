#Install the required packages for meta-analysis
install.packages("tidyverse")
install.packages("meta")
install.packages("metafor")
install.packages("devtools")
install.packages("janitor")
install.packages("esc")

#Load the packages
library(tidyverse)
library(meta)
library(metafor)
library(janitor)
library(esc)

#Install {dmetar}
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("MathiasHarrer/dmetar")
devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)

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

## Fixed-effect model
# Calculate Hedges' g and the Standard Error
# - We save the study names in "study".
# - After that, we use the pipe operator to directly transform
#   the results to a data frame.
SP_calc <- esc_mean_sd(grp1m = SuicidePrevention$mean.e,
                       grp1sd = SuicidePrevention$sd.e,
                       grp1n = SuicidePrevention$n.e,
                       grp2m = SuicidePrevention$mean.c,
                       grp2sd = SuicidePrevention$sd.c,
                       grp2n = SuicidePrevention$n.c,
                       study = SuicidePrevention$author,
                       es.type = "g") %>% 
  as.data.frame()

# Let us catch a glimpse of the data
# The data set contains Hedges' g ("es") and standard error ("se")
glimpse(SP_calc)

# We now calculate the inverse variance-weights for each study
SP_calc$w <- 1/SP_calc$se^2

# Then, we use the weights to calculate the pooled effect
pooled_effect <- sum(SP_calc$w*SP_calc$es)/sum(SP_calc$w)
pooled_effect

# Use metcont to pool results.
m.cont <- metacont(n.e = n.e,
                   mean.e = mean.e,
                   sd.e = sd.e,
                   n.c = n.c,
                   mean.c = mean.c,
                   sd.c = sd.c,
                   studlab = author,
                   data = SuicidePrevention,
                   sm = "SMD",
                   method.smd = "Hedges",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "Suicide Prevention")

summary(m.cont)
