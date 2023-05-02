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
#this makes me mad
#but let's see what this does
m.rma <- rma(yi = m.gen$TE,
             sei = m.gen$seTE,
             method = m.gen$method.tau,
             test = "knha")
res.gosh <- gosh(m.rma)

#generating GOSH plot
plot(res.gosh, alpha = 0.01)

#okay the output is kinda pretty
#what is that thing on the bottom right 
#so what causes that shape 
#using gosh diagnostics to detect clusters and determine which studies 
#contribute the most to each cluster 
#if a study is over-represented in a cluster, that's the culprit 
res.gosh.diag <- gosh.diagnostics(res.gosh, 
                                  km.params = list(centers = 2),
                                  db.params = list(eps = 0.08, 
                                                   MinPts = 50))
res.gosh.diag

#looks like study 3 4 and 16 are the problem 
#lets plot to inspect the results 
plot(res.gosh.diag)

#now let's see what happens if we remove these culprits 
update.meta(m.gen, exclude = c(3, 4, 16)) %>% 
  summary()

#the main components of a forest plot - name of each study, graph of the effect
#size, a line representing CI
#The point estimate is surrounded by a square - the bigger it is, the larger
#the weight of the study 
#the diamond on the bottom of the plot is the pooled effect 
#the length of the diamond symbolises the CI 
#let's generate a forest plot
forest.meta(m.gen, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))

#that looks great but let's add the risk of bias column
forest.meta(m.gen, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftcols = c("studlab", "TE", "seTE", "RiskOfBias"),
            leftlabs = c("Author", "g", "SE", "Risk of Bias"))

#trying out the JAMA pre-packaged layout 
forest.meta(m.gen, layout = "JAMA")

#whoa that's so cool
#let's try the RevMan one 
forest.meta(m.gen, layout = "RevMan5")

#okay now I want to play around with the forest plot
forest.meta(m.gen, 
            sortvar = TE,
            prediction = TRUE,
            leftcols = c("studlab", "TE", "seTE", "RiskOfBias"),
            leftlabs = c("Author", "g", "SE", "Risk of Bias"),
            label.right = "Favours treatment",
            col.square = "pink",
            col.diamond = "purple")

#printing the drapery plot 
#this plot is based on p-value functions to prevent us 
#from solely relying on p<0.05 significance threshold
#this function plots a confidence curve for each study and the average effect
#the x-axis shows the effect size metric, y-axis the assumed p-value

drapery(m.gen, 
        labels = "studlab",
        type = "pval", 
        legend = FALSE,
        col.predict = "pink",
        col.random = "purple")

#the peak of the p-value functions represents the exact value of the effect size
#then we go down the y-axis and p-value becomes smaller, while CI becomes wider 
#until we reach conventional significance thresholds marked by the dash lines
#in here, the pooled effect size is greater than 0 because the purple line
#reaches 0 on the x-axis when p < 0.01
