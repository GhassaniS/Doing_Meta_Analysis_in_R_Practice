#Load ThirdWave dataset
load("~/Documents/PhD/R/Doing_Meta_Analysis_in_R_Practice/thirdwave.rda")

library(tidyverse) # needed for 'glimpse'
library(meta)
library(metafor)
library(dmetar) #needed to find outliers

glimpse(ThirdWave) # take a peek on the dataset

#Meta-Analysis using random-effects model
m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")

summary(m.gen)

#Check if results differ substantially using Paule-Mandel
#instead of the restricted maximum likelihood estimator
m.gen_update <- update.meta(m.gen, 
                            method.tau = "PM")

# Get pooled effect
m.gen_update$TE.random

# Get tau^2 estimate
m.gen_update$tau2

save(m.gen, 
     file = "~/Documents/PhD/R/Doing_Meta_Analysis_in_R_Practice/meta-analysis.rda")
# example path

#updating to get the prediction intervals to see how the interventions 
#are predicted to be like 
#if it has negative value then someone doing this intervention might get
#negative outcomes
#so that's not good 
m.gen <- update.meta(m.gen, prediction = TRUE)
summary(m.gen)

#why was the test of heterogeneity significant 
#who are the culprits 
#let's find the culprits to our heterogeneity between studies
find.outliers(m.gen)

#even if the effect sizes aren't very high of low
#some studies still have high influence on our results 
#so we need to calculate the influence of each study 
m.gen.inf <- InfluenceAnalysis(m.gen, random = TRUE)

#to increase max overlaps so data points are visible
options(ggrepel.max.overlaps = Inf) 

#making Baujat plot  
plot(m.gen.inf, "baujat")

#plotting influence diagnostics
plot(m.gen.inf, "influence")

#print forest plots to see the changes if we omit a study from the meta-analysis
plot(m.gen.inf, "es") #based on effect sizes
plot(m.gen.inf, "i2") #based on i2 

#last thing is to do GOSH plots
#we fit the same meta-analysis model to all possible subsets 
#why do they call it GOSH it doesn't match Graphic Display of Heterogeneity 
#but let's generate the GOSH plot anyway
m.rma <- rma(yi = m.gen$TE,
             sei = m.gen$seTE,
             method = m.gen$method.tau,
             test = "knha")
res.gosh <- gosh(m.rma)
