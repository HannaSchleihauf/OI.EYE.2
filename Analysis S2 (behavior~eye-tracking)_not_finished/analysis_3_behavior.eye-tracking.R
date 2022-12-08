## Project Name: OI_EYE_2
## Analysis 3 - Analysis of behavioral data and eye-tracking data combined -

# Here we only look at the experimental condition.
# The response variable is the over-imitation (yes/no)
# Our predictors are the covariate relative looking time into the models face, the factor
# action type (no-contact/pseudo-instrumental; within-subjects) and 
# age group(child/adult; between-subjects), and the interactions.

# load necessary fuPItions and packages
library("lme4")
library("effects")
library("emmeans")
library("lme4")
library("ggplot2")
library("ggthemes")
library("tidyr")
library("dplyr")
library("gridExtra")
library("cowplot")
library("grid")
library("ggbeeswarm")
library("zoo")
library("optimx")

source("./functions/diagnostic_fcns.r")
source("./functions/glmm_stability.r")
source("./functions//drop1_para.r")
source("./functions/boot_glmm.r")

# make function to delete every nth row of the data.frame
Nth.delete <- function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]

# load data
xdata=read.table(file="./data_processed/processed_eye_tracking_data.txt", header=T, sep="\t")
str(xdata)
tapply(xdata$trial, list(xdata$single.id, xdata$box), sum)

# subset the data frame, so that we have only the relevant AOIs and actions
xdata = subset(xdata, aoi == "face" & (action == "PI" |  action == "NC"))

# calculate response variable: proportional score for AOI looking time relative to screen looking time
xdata$prop.time = xdata$aoi.time/xdata$time.fixations.screen  #relative to fixation length
range(xdata$prop.time, na.rm = T)

# making sure factors are factors etc.
xdata$aoi = as.factor(xdata$aoi)
xdata$child.adult = as.factor(xdata$child.adult)
xdata$box.id = as.factor(xdata$box)
xdata$trial.overall = as.numeric(as.factor(paste(xdata$box, xdata$trial, sep = ".")))

# calculate means for prop.times and screen fixation times from a data frame in 
# which we delete all trials in which children didn't look at the screen
no.zero.sreen.trials = subset(xdata, xdata$time.fixations.screen != 0) 
xx = aggregate(no.zero.sreen.trials$prop.time, 
               list(no.zero.sreen.trials$box, no.zero.sreen.trials$single.id), mean)
colnames(xx) <- c("box", "single.id", "prop.time.mean")
range(xx$prop.time.mean)

yy = aggregate(no.zero.sreen.trials$time.fixations.screen, 
               list(no.zero.sreen.trials$box, no.zero.sreen.trials$single.id), mean)
colnames(yy) <- c("box", "single.id", "time.fixations.screen.mean")
range(yy$time.fixations.screen.mean)


# creating a new data frame with the mean of looking time 
# (otherwise we would have 3 predictor measurements for 1 response measurement)
xxdata = Nth.delete(xdata, 3)
xxdata = Nth.delete(xxdata, 2)
xxdata = subset(xxdata, select=-c(prop.time, prop.time.demo, time.fixations.screen, time.demo, aoi.time, trial))
#merge with xx and yy which entail the mean data
yydata = merge(x = xxdata, y = xx, by = c("single.id", "box"), all.x=TRUE)
yydata = merge(x = yydata, y = yy, by = c("single.id", "box"), all.x=TRUE)


# how many adults and how many children do we have in the final data set
nlevels(as.factor(yydata$single.id[yydata$child.adult == "child"]))
nlevels(as.factor(yydata$single.id[yydata$child.adult == "parent"]))
# of how many participants do we also have the behavioral data
nlevels(as.factor(yydata$single.id[yydata$child.adult == "child" & yydata$behavior != "NA"]))
nlevels(as.factor(yydata$single.id[yydata$child.adult == "parent"& yydata$behavior != "NA"]))

# merge behavioral data with eye-tracking data (and test whether it is the same as the data I merged by hand)
bdata=read.table(file="./data_processed/processed_behavioral_data.txt", header=T, sep="\t")
bdata.exp = subset(bdata, condition == "experimental")
bdata.exp = subset(bdata.exp, action.type == "contact" | action.type == "no.contact")
bdata.exp$child.adult[bdata.exp$child.adult == "adult"] = "parent"
bdata.exp$single.id= paste(bdata.exp$id.dyad, bdata.exp$child.adult, sep = ".")
bdata.exp$box = bdata.exp$trial
colnames(bdata.exp)[10] <- "box"
new.data = merge(x = yydata, y = bdata.exp[ , c("single.id", "box", "oi.score")], by = c("single.id", "box"), all.x=TRUE)
sum((new.data$behavior - new.data$oi.score) != 0, na.rm = TRUE)
# everything is correct

#################################################################################
#################################################################################

# Checking whether we have an equal distribution of relative face-looking times across 
# action types and age groups, so that we can fit a model with a three-way-interaction

# plot for three-way interaction mean face-looking time * action type * age group
ggplot(yydata, aes(x = prop.time.mean, y = behavior, color = action)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) +
  facet_grid(action ~ child.adult, labeller = label_both)
  # the data is not evenly distributed

# Make the plot look nicer for the manuscript
action.labs <- c("NC", "PI")
names(action.labs) <- c("NC", "PI")
age.labs <- c("children", "adults")
names(age.labs) <- c("child", "parent") 

theme_set(theme_bw())
ggplot(yydata, aes(x = prop.time.mean, y = behavior, color = interaction(action, child.adult))) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) +
  facet_grid(action ~ child.adult, 
             labeller = labeller(child.adult = age.labs, action = action.labs)) +
  xlab("") +
  scale_x_continuous(name = c("mean face-looking time"), labels = scales::percent) + 
  scale_y_continuous(breaks = 0:1, limits = c(-0.25, 1.25),
                     labels = paste0(c("No OI", "OI"))) +
  theme(legend.position = "None", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        axis.title.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0))) + 
  scale_color_manual(values = c(
    (adjustcolor("#EE2C2C", alpha.f = 0.15)),
    (adjustcolor("#43CD80", alpha.f = 0.15)),
    (adjustcolor("#be2323", alpha.f = 0.15)),
    (adjustcolor("#199657", alpha.f = 0.15))
  ))

# plot for effect of mean face-looking for both action type separately and for both age groups combined
ggplot(yydata, aes(x = prop.time.mean, y = behavior, color = action)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) +
  facet_wrap(~interaction(action)) 

ggplot(yydata, aes(x = prop.time.mean, y = behavior, color = action)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) +
  facet_wrap(~interaction(child.adult)) 

# face-looking times for PI actions are much shorter, so we cannot fit a three-way interaction model
# this would lead to extrapolation interpretations
# Thus, we analyze NC actions and PI actions separably


#################################################################################
#################################################################################
# NC actions
NCdata <-  subset(yydata, yydata$action == "NC")

xx.fe.re=fe.re.tab(fe.model="behavior ~ prop.time.mean*child.adult",
                   re="(1|dyad.id) + (1|single.id)", 
                   other.vars= c("time.fixations.screen.mean"), data=NCdata)

xx.fe.re$summary
t.data=xx.fe.re$data 
str(t.data)
table(NCdata$behavior)
table(t.data$behavior)

# inspecting predictor
hist(t.data$prop.time.mean) #slighly left skewed
hist(sqrt(t.data$prop.time.mean)) # would be an option in case I have problems with the non-transformed version
# centering variables included in random slope
t.data$z.prop.time.mean = scale(t.data$prop.time.mean)
t.data$child.adult.code = t.data$child.adult.parent - mean(t.data$child.adult.parent)


# Starting with the model ---------------------------------------------------

full.NC = glmer(behavior ~ (z.prop.time.mean) + 
                   (1 + z.prop.time.mean || single.id) +
                   (1 + child.adult.code || dyad.id),
                 data = t.data,family = binomial)
# fitting a mixed model resulted in several issues: overestimation of random effects, 
# extreme estimates, model instability
# This did not change even after trying many differnet control functions.

# Thus, we fitted a model without random slope structure, results need be interpreted carefully

full.NC = glm(behavior ~ (as.vector(z.prop.time.mean)*child.adult),
                data = t.data, family = binomial)
red.NC = glm(behavior ~ (as.vector(z.prop.time.mean)),
                data = t.data, family = binomial)
null.NC = glm(behavior ~ (1),
             data = t.data, family = binomial)
plot.NC = glm(behavior ~ (as.vector(z.prop.time.mean)),
              data = t.data, family = binomial)

# assumption tests
library("car")
vif(glm(behavior ~ (as.vector(z.prop.time.mean)+child.adult),
        data = t.data, family = binomial))
overdisp.test(full.NC) 
# model stability
cbind(coefficients(full.NC), coefficients(full.NC)+
        t(apply(X=dfbeta(full.NC), MARGIN=2, FUN=range)))

round(summary(full.NC)$coefficients, 3)
# confidence intervals
round(cbind(coefficients(full.NC), confint(object=full.NC)), 3)


# full-null model comparison
anova(full.NC, null.NC, test = "Chisq")
# full-reduced model comparison
drop1(full.NC, test = "Chisq")
# main effects
drop1(red.NC, test = "Chisq")

plot(effect("z.prop.time.mean*child.adult", full.NC))
round(summary(full.NC)$coefficients, 3)

# prediction data for plot
pred.data=data.frame(
  child.adult=rep(x=levels(t.data$child.adult), each=100),
  z.prop.time.mean=seq(from=min(t.data$z.prop.time.mean),
              to=max(t.data$z.prop.time.mean), length.out=100)
)
ci.plot=predict.glm(object=full.NC, newdata=pred.data,
                    type="link", se.fit=T)
ci.plot=data.frame(fitted = ci.plot$fit,
  lwr=ci.plot$fit-ci.plot$se.fit*
    abs(qt(p=0.025, df=full.NC$df.residual)),
  upr=ci.plot$fit+ci.plot$se.fit*
    abs(qt(p=0.025, df=full.NC$df.residual)))
ci.plot=exp(ci.plot)/(1+exp(ci.plot))
ci.plot=data.frame(ci.plot, pred.data)

ci.plot.children <- subset(ci.plot, ci.plot$child.adult == "child")
ci.plot.adult <- subset(ci.plot, ci.plot$child.adult == "parent")

# prediction data for plot
pred.data.general=data.frame(
  z.prop.time.mean=seq(from=min(t.data$z.prop.time.mean),
                       to=max(t.data$z.prop.time.mean), length.out=100)
)
ci.plot.general=predict.glm(object=plot.NC, newdata=pred.data.general,
                            type="link", se.fit=T)
ci.plot.general=data.frame(fitted = ci.plot.general$fit,
                           lwr=ci.plot.general$fit-ci.plot.general$se.fit*
                             abs(qt(p=0.025, df=plot.NC$df.residual)),
                           upr=ci.plot.general$fit+ci.plot.general$se.fit*
                             abs(qt(p=0.025, df=plot.NC$df.residual)))
ci.plot.general=exp(ci.plot.general)/(1+exp(ci.plot.general))
ci.plot.general=data.frame(ci.plot.general, pred.data.general)

# Plot for NC actions
# y axis labels
range(t.data$prop.time.mean)
percentage.labels= c("0%", "20%", "40%", "60%", "80%")
percentage.comma = seq(from = 0, to = 0.8, length.out = 5)
##and a vector in z-space at which they need to be displayed
percentage.ticks=
  (percentage.comma - mean(t.data$prop.time.mean))/
  sd(t.data$prop.time.mean)

set.seed(1)
theme_set(theme_bw())
NC.plot = ggplot(t.data, aes(x = z.prop.time.mean, y = behavior, color = child.adult)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) +
  geom_ribbon(data=ci.plot.general,
              aes(x = z.prop.time.mean,
                  ymin = lwr,
                  ymax = upr),
              alpha = 0.3,
              fill="#696969", inherit.aes=FALSE) +
  geom_line(data=ci.plot.general, 
            aes(x=z.prop.time.mean, y=fitted), colour="#696969", lwd=1) + 
  
  geom_ribbon(data=ci.plot.children,
              aes(x = z.prop.time.mean,
                  ymin = lwr,
                  ymax = upr),
              alpha = 0.1,
              fill="#EE2C2C", inherit.aes=FALSE) +
  geom_line(data=ci.plot.children, 
              aes(x=z.prop.time.mean, y=fitted), colour=(adjustcolor("#EE2C2C", alpha.f = 0.25)), lwd=1, lty = 4) + 
  geom_ribbon(data=ci.plot.adult,
                aes(x = z.prop.time.mean,
                    ymin = lwr,
                    ymax = upr),
                alpha = 0.1,
                fill="#be2323", inherit.aes=FALSE) +
    geom_line(data=ci.plot.adult, 
              aes(x=z.prop.time.mean, y=fitted), colour=(adjustcolor("#be2323", alpha.f = 0.25)), lwd=1, lty = 2) + 
  xlab("") +
  scale_x_continuous(breaks = percentage.ticks, 
                     labels =  percentage.labels) +
  scale_y_continuous(breaks = 0:1, limits = c(-0.25, 1.25),
                     labels = paste0(c("No OI", "OI"))) +
  theme(legend.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        axis.title.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0))) + 
  scale_color_manual(values = c(
    (adjustcolor("#EE2C2C", alpha.f = 0.15)),
    (adjustcolor("#be2323", alpha.f = 0.15))
  ))
NC.plot





#################################################################################
#################################################################################
# PI actions
PIdata <-  subset(yydata, yydata$action == "PI")

xx.fe.re=fe.re.tab(fe.model="behavior ~ prop.time.mean*child.adult",
                   re="(1|dyad.id) + (1|single.id)", 
                   other.vars= c("time.fixations.screen.mean"), data=PIdata)

xx.fe.re$summary
t.data=xx.fe.re$data 
str(t.data)
table(PIdata$behavior)
table(t.data$behavior)

# inspecting predictor
hist(t.data$prop.time.mean) #slightly left skewed
hist(sqrt(t.data$prop.time.mean)) # would be an option in case I have problems with the non-transformed version
# centering variables included in random slope
t.data$z.prop.time.mean = scale(t.data$prop.time.mean)
t.data$child.adult.code = t.data$child.adult.parent - mean(t.data$child.adult.parent)


# Starting with the model ---------------------------------------------------

full.PI = glmer(behavior ~ (z.prop.time.mean) + 
                  (1 + z.prop.time.mean || single.id) +
                  (1 + child.adult.code || dyad.id),
                data = t.data,family = binomial)
# fitting a mixed model resulted in several issues: overestimation of random effects, 
# extreme estimates, model instability

# Thus, we fitted a model without random slope structure, results need be interpreted carefully

full.PI = glm(behavior ~ (as.vector(z.prop.time.mean)*child.adult),
              data = t.data, family = binomial)
red.PI = glm(behavior ~ (as.vector(z.prop.time.mean)),
             data = t.data, family = binomial)
null.PI = glm(behavior ~ (1),
              data = t.data, family = binomial)
plot.PI = glm(behavior ~ (as.vector(z.prop.time.mean)),
             data = t.data, family = binomial)


# assumption tests
library("car")
vif(glm(behavior ~ (as.vector(z.prop.time.mean)+child.adult),
        data = t.data, family = binomial))
overdisp.test(full.PI) 
# model stability
round(cbind(coefficients(full.PI), coefficients(full.PI)+
        t(apply(X=dfbeta(full.PI), MARGIN=2, FUN=range))), 3)

round(summary(full.PI)$coefficients, 3)
# confidence intervals
round(cbind(coefficients(full.PI), confint(object=full.PI)), 3)


# full-null model comparison
anova(full.PI, null.PI, test = "Chisq")
# full-reduced model comparison
drop1(full.PI, test = "Chisq")
# main effects
drop1(red.PI, test = "Chisq")

plot(effect("z.prop.time.mean*child.adult", full.PI))
round(summary(full.PI)$coefficients, 3)

# prediction data for plot
pred.data=data.frame(
  child.adult=rep(x=levels(t.data$child.adult), each=100),
  z.prop.time.mean=seq(from=min(t.data$z.prop.time.mean),
                       to=max(t.data$z.prop.time.mean), length.out=100)
)
ci.plot=predict.glm(object=full.PI, newdata=pred.data,
                    type="link", se.fit=T)
ci.plot=data.frame(fitted = ci.plot$fit,
                   lwr=ci.plot$fit-ci.plot$se.fit*
                     abs(qt(p=0.025, df=full.PI$df.residual)),
                   upr=ci.plot$fit+ci.plot$se.fit*
                     abs(qt(p=0.025, df=full.PI$df.residual)))
ci.plot=exp(ci.plot)/(1+exp(ci.plot))
ci.plot=data.frame(ci.plot, pred.data)
ci.plot.children <- subset(ci.plot, ci.plot$child.adult == "child")
ci.plot.adult <- subset(ci.plot, ci.plot$child.adult == "parent")

# prediction data for plot
pred.data.general=data.frame(
  z.prop.time.mean=seq(from=min(t.data$z.prop.time.mean),
                       to=max(t.data$z.prop.time.mean), length.out=100)
)
ci.plot.general=predict.glm(object=plot.PI, newdata=pred.data.general,
                    type="link", se.fit=T)
ci.plot.general=data.frame(fitted = ci.plot.general$fit,
                   lwr=ci.plot.general$fit-ci.plot.general$se.fit*
                     abs(qt(p=0.025, df=plot.PI$df.residual)),
                   upr=ci.plot.general$fit+ci.plot.general$se.fit*
                     abs(qt(p=0.025, df=plot.PI$df.residual)))
ci.plot.general=exp(ci.plot.general)/(1+exp(ci.plot.general))
ci.plot.general=data.frame(ci.plot.general, pred.data.general)



# Plot for PI actions
# y axis labels
range(t.data$prop.time.mean)
percentage.labels= c("0%", "20%", "40%", "60%", "80%")
percentage.comma = seq(from = 0, to = 0.8, length.out = 5)
##and a vector in z-space at which they need to be displayed
percentage.ticks=
  (percentage.comma - mean(t.data$prop.time.mean))/
  sd(t.data$prop.time.mean)

theme_set(theme_bw())
PI.plot = ggplot(t.data, aes(x = z.prop.time.mean, y = behavior, color = child.adult)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) +
  geom_ribbon(data=ci.plot.general,
             aes(x = z.prop.time.mean,
                 ymin = lwr,
                 ymax = upr),
             alpha = 0.3,
             fill="#696969", inherit.aes=FALSE) +
  geom_line(data=ci.plot.general, 
            aes(x=z.prop.time.mean, y=fitted), colour="#696969", lwd=1) + 
  
  geom_ribbon(data=ci.plot.children,
              aes(x = z.prop.time.mean,
                  ymin = lwr,
                  ymax = upr),
              alpha = 0.1,
              fill="#43CD80", inherit.aes=FALSE) +
  geom_line(data=ci.plot.children, 
            aes(x=z.prop.time.mean, y=fitted), colour=(adjustcolor("#43CD80", alpha.f = 0.25)), lwd=1, lty = 4) + 
  geom_ribbon(data=ci.plot.adult,
              aes(x = z.prop.time.mean,
                  ymin = lwr,
                  ymax = upr),
              alpha = 0.1,
              fill="#199657", inherit.aes=FALSE) +
  geom_line(data=ci.plot.adult, 
            aes(x=z.prop.time.mean, y=fitted), colour=(adjustcolor("#199657", alpha.f = 0.25)), lwd=1, lty = 2) +
  xlab("") +
  scale_x_continuous(breaks = percentage.ticks, 
                     labels =  percentage.labels, limits = c(-1.1438354, 6.6720002)) +
  scale_y_continuous(breaks = 0:1, limits = c(-0.25, 1.25),
                     labels = paste0(c("No OI", "OI"))) +
  theme(legend.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        axis.title.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0))) + 
  scale_color_manual(values = c(
    (adjustcolor("#43CD80", alpha.f = 0.15)),
    (adjustcolor("#199657", alpha.f = 0.15))
  ))
NC.plot
PI.plot


save.image("analysis_3_behavior.eye-tracking.RData")
