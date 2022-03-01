#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2021

library(ggplot2)
library(dplyr)
library(pscl)
source("./read_data.R")

## Does moisture content affect heat release?

ggplot(alldata,aes(moisture_content,ignition))+
  geom_point()+
  geom_smooth(method = "glm",method.args=list(family=binomial(link = "cloglog")), 
              fullrange=TRUE, se=FALSE,color="red")+
  scale_x_continuous(limits = c(2.406877,131.754543),
                     breaks = c(0,25,50,75,100,125))+
  xlab("Moisture content (%)")+
  ylab("Probability of getting ignited")+
  theme_bw()

## Null model for comparison.

log.null.moisture <- glm(ignition~1,
                         family = binomial(link = "cloglog"), 
                         data = alldata) 

log.mod.moisture <- glm(ignition~moisture_content,
                        family = binomial(link = "cloglog"),
                        data = alldata)

summary(log.mod.moisture) # p value 0.0573 , not statistically significant!!


pscl::pR2(log.mod.moisture)["McFadden"] #McFadden 0.04627074




