## Figures and tables for presentations and publications

source("./analysis.R")
source("./ggplot_theme.R")


## Ignition
fig_ignition_moisture <- ggplot(alldata, aes(moisture_content, ignition)) +
  geom_point() +
  geom_smooth(method = "glm",method.args=list(family=binomial(link = "cloglog")), 
              fullrange=TRUE, se=FALSE,color="red") +
  scale_x_continuous(limits = c(2.406877,131.754543),
                     breaks = c(0,25,50,75,100,125)) +
  xlab("Moisture content (%)") +
  ylab("Probability of ignition") +
  prestheme.nogridlines +
  theme(axis.title = element_text(size=12,face = "bold"))
ggsave("../results/ignition_moisture_logistic_figure.pdf", plot = fig_ignition_moisture,
       height=beamer_height, width=beamer_height, units="cm")

fig_ignition_wind <- ggplot(alldata,aes(windspeed, ignition)) +
  geom_point() +
  geom_smooth(method = "glm",method.args=list(family=binomial(link = "cloglog")), 
              fullrange=TRUE, se=FALSE,color="red") +
  scale_x_continuous(limits = c(0,8.7), breaks = c(0,2,4,6,8)) +
  xlab("Windspeed") +
  ylab("Probability of getting ignited") +
  prestheme.nogridlines
ggsave("../results/ignition_wind_logistic_figure.pdf", plot = fig_ignition_wind,
       height=beamer_height, width=beamer_height, units="cm")


#### Figure: Flammability vs mass
model_data_sum <- model_data %>% group_by(group) %>%
  summarize(across(c(canopy_density, total_mass_g, PC1), list(mean = mean, sum  = sum)), na.rm=TRUE)

fig1 <- ggplot(model_data, aes(total_mass_g, PC1)) +
  geom_point(alpha=0.4, size=3) +
  geom_point(data=model_data_sum, aes(total_mass_g_mean, PC1_mean), size=5) +
  #scale_colour_manual(schwilkcolors) +
  bestfit +
  #geom_smooth(method="lm",se=FALSE, color="black") +
  xlab("Mass per 70 cm (g)") +
  ylab("Flammability (PC1 score)") +
  prestheme.nogridlines +
  theme(legend.position="none")

#fig1
ggsave("../results/shrubflam_fig1.pdf", fig1, width=10, height=beamer_height, units="cm")

