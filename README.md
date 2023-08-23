# Silwood-Mammal-s-Master-s-Thesis
# Install packges (run once)
install.packages(c("devtools",
                   "tidyverse",
                   "jpeg",
                   "activity",
                   "Distance",
                   "gtools"))
devtools::install_github("inbo/camtraptor")

################################################
# Load packages (run before each session)
library(tidyverse)
library(camtraptor)
library(activity)
library(Distance)
library(gtools)
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/camtraptor_density_example/main/rem_functions.R")

################################################
# Load data
package <- read_camtrap_dp("datapackage.json")

################################################
# One step analysis

r<- check_deployment_models(r)
plot_deployment_schedule(r)
result <- rem_estimate(r, check_deployments = FALSE)
library(readxl)
Last <- read_excel("Last.xlsx") na.rm = TRUE)
View(Last)

#Poisson Model for Badger RA
# Fit a full model with poisson errors
modPois <- glm(Badger ~ habitat + Road + Building + Humans + offset(log(Effort)),family = poisson, data = Last)
# Check model for overdispersion
performance::check_overdispersion(modPois)
# If OD not detected you can proceed to model
# exploration / simplifaction with the Poisson model;
# if OD is detected, switch to negative binomial errors:
BB <- MASS::glm.nb (Badger ~ habitat + Road + Building + Humans + offset(log(Effort)), data = Last)
# explore / simplified from here...
BB <- MASS::glm.nb (Badger ~ Building + offset(log(Effort)), data = Last)

##Poisson Model for Fox RA
# Fit a full model with poisson errors
modPois <- glm(Fox ~ habitat + Road + Building + Humans + offset(log(Effort)),family = poisson, data = Last)
# Check model for overdispersion
performance::check_overdispersion(modPois)
# If OD not detected you can proceed to model
# exploration / simplifaction with the Poisson model;
# if OD is detected, switch to negative binomial errors:
BB <- MASS::glm.nb (Fox ~ habitat + Road + Building + Humans + offset(log(Effort)), data = Last)
# explore / simplified from here...
BB <- MASS::glm.nb (Fox ~ Humans + offset(log(Effort)), data = Last)

##Poisson Model for Fox RA
# Fit a full model with poisson errors
modPois <- glm(Fox ~ habitat + Road + Building + Humans + offset(log(Effort)),family = poisson, data = Last)
# Check model for overdispersion
performance::check_overdispersion(modPois)
# If OD not detected you can proceed to model
# exploration / simplifaction with the Poisson model;
# if OD is detected, switch to negative binomial errors:
BB <- MASS::glm.nb (Fox ~ habitat + Road + Building + Humans + offset(log(Effort)), data = Last)
# explore / simplified from here...
BB <- MASS::glm.nb (Fox ~ Humans + offset(log(Effort)), data = Last)

##Poisson Model for Interspecies Interactions for fox
# Fit a full model with poisson errors
modPois <- glm(Fox ~ Badger + Squirrel + Hare + Roe + Munjact + offset(log(Effort)),family = poisson, data = Last)
# Check model for overdispersion
performance::check_overdispersion(modPois)
# If OD not detected you can proceed to model
# exploration / simplifaction with the Poisson model;
# if OD is detected, switch to negative binomial errors:
BB <- MASS::glm.nb (Fox ~ Badger + Squirrel + Hare + Roe + Munjact + offset(log(Effort)),family = poisson, data = Last)
# explore / simplified from here...
BB <- MASS::glm.nb (Fox ~ Squirrel + Hare + Roe + offset(log(Effort)), data = Last)

##Poisson Model for Interspecies Interactions for badger
# Fit a full model with poisson errors
modPois <- glm(Badger ~ Fox + Squirrel + Hare + Roe + Munjact + offset(log(Effort)),family = poisson, data = Last)
# Check model for overdispersion
performance::check_overdispersion(modPois)
# If OD not detected you can proceed to model
# exploration / simplifaction with the Poisson model;
# if OD is detected, switch to negative binomial errors:
BB <- MASS::glm.nb (Badger ~ Fox + Squirrel + Hare + Roe + Munjact + offset(log(Effort)),family = poisson, data = Last)
# explore / simplified from here...
BB <- MASS::glm.nb (Badger ~ Squirrel + Hare + Roe + offset(log(Effort)), data = Last)

##Binomial for Fox Nocturnality
# Fit a full model with poisson errors
modPois <- glm(FN ~ Habitat + Building + Road + Human, family = binomial, data = Last)
# Check model for overdispersion
performance::check_overdispersion(modPois)
# model is overdispersed
modPois <- glm(FN ~ Habitat + Building + Road + Human, family = quasibinomial, data = Last)

##Binomial for  Interspecies Temporal Overlals for  Fox 
# Fit a full model with poisson errors
modPois <- glm(FN ~ Habitat + SN + HN + RN + MN, family = binomial, data = Last)
# Check model for overdispersion
performance::check_overdispersion(modPois)
# model is overdispersed
modPois <- glm(FN ~ Habitat + Building + Road + Human, family = quasibinomial, data = Last)

#Habitat
FoxAbundanceHabitat <- lm(Fox ~ Habitat + offset(log(Time), data = Last)
FoxNocturnality <- lm(FN ~ Habitat, data = Last)
BadgerAbundanceHabitat <- lm(Badger ~ Habitat + offset(log(Time), data = Last)

#Diagrams for Relative Abundance for badgers
mod <- MASS::glm.nb(Badger ~ Building + offset(log(Time)), data = Last) # an example model
splot <- sjPlot::plot_model(modPois, type="pred") # use sjPlot::plot_model to get plot data but not plot it
sdat <- splot$Building$data # output rates are per deployment with average effort
# change variable in the middle to get other responses
cols <- c("predicted", "std.error", "conf.low", "conf.high") # names of trap rate data columns to transform
sdat[, cols] <- sdat[, cols] / mean(Last$time) # transform to trap rate per unit time
# make plot 
ggplot(data=sdat, aes(x=x, y=predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.1) +
  geom_point(data=dat, aes(x=Badger, y=Building)) +
  xlab("Distance to Building (m)") +
  ylab(" Badger trap rate (per day)") +
  theme_classic()

#Fox Abundance
mod <- MASS::glm.nb(Fox ~ Humans + offset(log(effort)), data = Last) # an example model
splot <- sjPlot::plot_model(modPois, type="pred") # use sjPlot::plot_model to get plot data but not plot it
sdat <- splot$Humans$data # output rates are per deployment with average effort
# change variable in the middle to get other responses
cols <- c("predicted", "std.error", "conf.low", "conf.high") # names of trap rate data columns to transform
sdat[, cols] <- sdat[, cols] / mean(Last$Time) # transform to trap rate per unit time
# make plot (I prefer classic theme but up to you; I’ve assumed effort unit is days for purpose of axis labels)
ggplot(data=sdat, aes(x=x, y=predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.1) +
  geom_point(data=dat, aes(x=Fox, y=Humans)) +
  xlab("Human trap rate (per hour)”) +
  ylab(" Badger trap rate (per hour)") +
  theme_classic()

#Fox Habitat
ggplot(data=Last, aes(x= FN, y=Habitat)) +
  ggplot()
  xlab("Habitat”) +
  ylab(" Fox Nocturnality") +
  theme_classic()

#Fox Nocturnality

ggplot(data= Last, aes(x= Building , y= FN)) +
  geom_jitter() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.1) +
    xlab("Distance to Building”) +
  ylab(" Fox Nocturnality)") +
  theme_classic() +
stat_smooth(method=glm, family = binomial , color="black", fill="grey", se= TRUE) 

ggplot(data= Last, aes(x= Road , y= FN)) +
  geom_jitter() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.1) +
    xlab("Distance to Road (m)”) +
  ylab(" Fox Nocturnality)") +
  theme_classic() +
stat_smooth(method=glm, family = binomial , color="black", fill="grey", se= TRUE) 


