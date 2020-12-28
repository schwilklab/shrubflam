# 2020-12-28

# Initial analyses

library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)


ftrials <- read_csv("../data/flam_trials.csv") %>%
  mutate(date=mdy(date),
         tempdiff = (temp.d1.post-temp.d1.pre+temp.d2.post-temp.d2.pre)/2,
         massconsum=(mass.pre - mass.post)/mass.pre)
wx <- read_csv("../data/flam_trial_weather.csv") %>% mutate(date=mdy(date))
ftrials <- left_join(ftrials, wx, by=c("date", "trial"))


genus_sp <- str_match(ftrials$species, "([a-zA-z]+) ([a-zA-z]+)?")
ftrials$genus <- genus_sp[,2]
ftrials$se <- genus_sp[,3]

ggplot(ftrials, aes(genus, tempdiff)) + geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))


ggplot(ftrials, aes(genus, massconsum)) + geom_jitter(width=0.2, size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))

ggplot(ftrials, aes(x=reorder(genus, vol.burned), vol.burned)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))


ggplot(ftrials, aes(x=reorder(genus, vol.burned), vol.burned)) + geom_jitter(width=0.2, size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))
