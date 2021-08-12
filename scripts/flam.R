# 2020-12-28

# Initial analyses

library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)

# convert "m:s" format to seconds only
#time_seconds <- function(x)
  

ftrials <- read_csv("../data/flam_trials.csv") %>%
  mutate(date=mdy(date),
         tempdiff = (temp.d1.post-temp.d1.pre+temp.d2.post-temp.d2.pre)/2,
         heat = (tempdiff + 1.5)*53*0.921, # in joules
         massconsum=(mass.pre - mass.post)/mass.pre,
         flame.dur = as.numeric(as.duration(ms(flame.dur)), "seconds"))

wx <- read_csv("../data/flam_trial_weather.csv") %>% mutate(date=mdy(date))
ftrials <- left_join(ftrials, wx, by=c("date", "trial"))


genus_sp <- str_match(ftrials$species, "([a-zA-z]+) ([a-zA-z]+)?")
ftrials$genus <- genus_sp[,2]
ftrials$se <- genus_sp[,3]
## ftrials$taxon <- genus
## ftrials$taxon[ftrials$taxon %in% c("Condalia", "Ziziphus")] <- "Rhamnaceae"


ggplot(ftrials, aes(genus, flame.dur)) + geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))


ggplot(ftrials, aes(genus, massconsum)) + geom_jitter(width=0.2, size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))

ggplot(ftrials, aes(x=reorder(genus, vol.burned), vol.burned)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))


ggplot(ftrials, aes(x=reorder(genus, vol.burned), vol.burned)) + geom_jitter(width=0.2, size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))



# pca
pca <- prcomp(select(ftrials, massconsum, tempdiff, vol.burned, flame.ht, flame.dur))
plot(pca)

ftrials$PC1 <- pca$x[,1]

ggplot(ftrials, aes(x=reorder(genus, PC1), -PC1)) +
  geom_boxplot() +
  xlab("Taxon") +
  ylab("Flammability score (PC1)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))
ggsave("../results/fig1.pdf")

ggplot(ftrials, aes(x=reorder(genus, -heat), heat)) +
  geom_boxplot() +
  xlab("Taxon") +
  ylab("Heat release (J)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))
ggsave("../results/fig2.pdf")
