#Load ThirdWave dataset
load("~/Documents/PhD/R/Doing_Meta_Analysis_in_R_Practice/thirdwave.rda")

library(tidyverse) # needed for 'glimpse'
library(meta)

data(ThirdWave)
glimpse(ThirdWave)

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
     file = "~/Documents/PhD/R/Doing_Meta_Analysis_in_R_Practice/meta-analysis.rda") # example path
