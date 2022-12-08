## Project Name: OI_EYE_2
## Analysis 1 - Analysis of face-looking times

## Do children look longer into the model's face when no-contact actions
## are demonstrated compared to when pseudo-instrumental actions or relevant actions
## are demonstrated? Our predictors are the factors action type
## (pseudo-instrumental/no-contact/relevant; within-subjects)
## and age group (child/adult; between-subjects).

## Written by: Hanna Schleihauf
## Date: 17 January 2022

# load necessary functions and packages
library("lme4")
library("effects")
library("glmmTMB")
library("emmeans")
library("dplyr")
library("tidyr")
library("tidyverse")

source("./functions/diagnostic_fcns.r")
source("./functions/boot_glmmTMB.r")
source("./functions/drop1_para_glmmtmb.r")
source("./functions/glmmTMB_stability.r")

# load data
xdata <- read.table(file = "./data_processed/processed_eye_tracking_data.txt", header = T, sep = "\t")

# inspect data
str(xdata)
# check whether fixation screen are always longer than fixation AOIs
sum(xdata$time.fixations.screen < xdata$aoi.time)

# calculate response variable: proportional score for AOI looking time relative to screen looking time
xdata$prop.time <- xdata$aoi.time / xdata$time.fixations.screen
range(xdata$prop.time, na.rm = T)
# check for how many trials we had no looking time at the screen
sum(is.na(xdata$prop.time))
# making sure factors are factors etc.
xdata$aoi <- as.factor(xdata$aoi)
xdata$child.adult <- as.factor(xdata$child.adult)
xdata$box <- as.factor(xdata$box)
xdata$trial.overall <- as.numeric(as.factor(paste(xdata$box, xdata$trial, sep = ".")))

# subset the data frame, so that we have only the the looking time for the relevant AOI "face"
# and during the demonstration of the relevant actions (Intro and marble showing is excluded)
xxdata <- subset(xdata, aoi == "face" & (action == "PI" | action == "R" | action == "NC"))
# delete cases in which we have absolute no fixations on the screen
table(xxdata$time.fixations.screen == 0)
table(xxdata$trial, xxdata$time.fixations.screen == 0)
xxdata <- subset(xxdata, xxdata$time.fixations.screen != 0) # 5 cases
# how many adults and how many children do we have in the final data set
nlevels(as.factor(xxdata$single.id[xxdata$child.adult == "child"]))
nlevels(as.factor(xxdata$single.id[xxdata$child.adult == "parent"]))
# how many trials per block did each participant contribute to the data on average
mean(table(xxdata$single.id, xxdata$box) / 2) # with at least 2.9 valid trials per block

# prepare model fitting (dummy coding and centering)
xx.fe.re <- fe.re.tab(
  fe.model = "prop.time ~ action*child.adult",
  re = "(1|dyad.id) + (1|single.id) + (1|box)", data = xxdata,
  other.vars = c("time.fixations.screen", "trial", "box")
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

# inspect response
hist(t.data$prop.time, breaks = 100)
range(t.data$prop.time)
# we have values 0 and 1 in the response variable, but for a beta model the values
# need to be a little bit below 1 and a little above 0, therefore, we move the values
# a little bit away from 0 and 1
t.data$t.prop.time <- t.data$prop.time # create a new variable that can be transformed
t.data$t.prop.time <- (t.data$t.prop.time * (length(t.data$t.prop.time) - 1) + 0.5) / length(t.data$t.prop.time)
hist(t.data$t.prop.time, breaks = 100)
range(t.data$t.prop.time) # checking whether it worked

# center all  variables included in random slope
t.data$child.adult.code <- t.data$child.adult.parent - mean(t.data$child.adult.parent)
t.data$action.PI.code <- t.data$action.PI - mean(t.data$action.PI)
t.data$action.R.code <- t.data$action.R - mean(t.data$action.R)
t.data$z.time.fixations.screen <- scale(t.data$time.fixations.screen)
t.data$z.trial <- scale(t.data$trial)

# Preliminary analysis ----------------------------------------------------
# Preliminary analysis to see whether trial number per block has an effect on the
# looking time into the model's face

contr <-
  glmmTMBControl(
    optCtrl = list(iter.max = 200000, eval.max = 200000),
    profile = FALSE, collect = FALSE
  )

pre.analysis <- glmmTMB(t.prop.time ~ z.trial +
  (1 + (action.PI.code + action.R.code) + z.trial || single.id) +
  (1 + child.adult.code || dyad.id) +
  (1 + (action.PI.code + action.R.code) + z.trial || box),
data = t.data, family = beta_family(link = "logit"), control = contr,
weights = time.fixations.screen,
REML = FALSE
)

# checking assumptions
warnings()
summary(pre.analysis)$varcor # random effects
ranef.diagn.plot(pre.analysis) # distribution of random effects
overdisp.test(pre.analysis) # overdispersion
# model stability (exclude data of one individual at the time and fit model again)
pre.stab2 <- glmmTMB.stab(
  model.res = pre.analysis, contr = contr, ind.cases = F, para = F,
  data = t.data, use = "single.id", n.cores = c("all-1"), save.path = NULL, load.lib = T, lib.loc = .libPaths()
)
pre.stab2$detailed$warnings
xx <- round(pre.stab2$summary[, -1], 3)
m.stab.plot(pre.stab2$summary[, -1])
xx ## model is stable

# significance tests. Since we only have one predictor is is equivalent to the full-null model test.
test.pre <- drop1p(
  model.res = pre.analysis, para = F, data = t.data, contr = contr,
  n.cores = c("all-1"), to.del = NULL, return.model.results = F
)
test.pre$drop1.res

# look at estimates
round(summary(pre.analysis)$coefficients$cond, 3)

# calculate bootstraps
boot.plot.pre.analysis <- boot.glmmTMB(
  model.res = pre.analysis, data = t.data, excl.non.conv = T,
  nboots = 1000, para = T, resol = 100, level = 0.95, use = c("z.trial"),
  contr = contr, circ.var.name = NULL, circ.var = NULL, n.cores = c("all-1"),
  save.path = NULL, load.lib = T, lib.loc = .libPaths(), set.all.effects.2.zero = F
)
boot.plot.pre.analysis$ci.estimates
boot.plot.pre.analysis$ci.fitted

# Main analysis ----------------------------------------------------
# In the main analysis we investigate whether there is an effect of action type and age group
# on children's looking time into the model's face
full <- glmmTMB(t.prop.time ~ (action + child.adult)^2 +
  (1 + (action.PI.code + action.R.code) || single.id) +
  (1 + child.adult.code || dyad.id) +
  (1 + z.trial + (action.PI.code + action.R.code) || box),
data = t.data, family = beta_family(link = "logit"), control = contr,
weights = time.fixations.screen,
REML = FALSE
)

# checking assumptions
warnings()
summary(full)$varcor # random effects
ranef.diagn.plot(full) # distribution of random effects
overdisp.test(full) # overdispersion
# colliniarity
xx <- lm(t.prop.time ~ (action + child.adult),
  weights = time.fixations.screen,
  data = t.data
)
library(car)
vif(xx)
# model stability (exclude data of one individual at the time and fit model again)
full.stab <- glmmTMB.stab(
  model.res = full, contr = contr, ind.cases = F, para = T,
  data = t.data, use = NULL, n.cores = c("all-1"),
  save.path = NULL, load.lib = T, lib.loc = .libPaths()
)
full.stab$detailed$warnings
xx <- round(full.stab$summary[, -1], 3)
m.stab.plot(full.stab$summary[, -1])
xx # model is stable

# full-null-model comparison to see whether the all predictors taken together explain more of the variance than the intercept alone
null <- glmmTMB(t.prop.time ~ 1 +
  (1 + (action.PI.code + action.R.code) || single.id) +
  (1 + child.adult.code || dyad.id) +
  (1 + z.trial + (action.PI.code + action.R.code) || box),
data = t.data, family = beta_family(link = "logit"), control = contr,
weights = time.fixations.screen,
REML = FALSE
)
anova(full, null, test = "Chisq")
# look at estimates
round(summary(full)$coefficients$cond, 3)

# significance tests of interaction effect
test.full <- drop1p(
  model.res = full, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test.full$drop1.res

# dropping interaction form the model to get to main effects
full.1 <- glmmTMB(t.prop.time ~ (action + child.adult) +
  (1 + (action.PI.code + action.R.code) || single.id) +
  (1 + child.adult.code || dyad.id) +
  (1 + z.trial + (action.PI.code + action.R.code) || box),
data = t.data, family = beta_family(link = "logit"), control = contr,
weights = time.fixations.screen,
REML = FALSE
)
# significance tests of main effect
tests.1 <- drop1p(
  model.res = full.1, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
tests.1$drop1.res
round(tests.1$drop1.res$Pr..Chisq., 4) # action type is significant

# Bootstrap for full model
boot.full <- boot.glmmTMB(
  model.res = full, data = t.data, excl.non.conv = T, nboots = 1000, para = F, resol = 100, level = 0.95,
  use = c("action"), contr = contr, circ.var.name = NULL, circ.var = NULL, n.cores = c("all-1", "all"),
  save.path = NULL, load.lib = T, lib.loc = .libPaths(), set.all.effects.2.zero = F
)
round(boot.full$ci.estimates$fe, 3)

# fitting a plotmodel (with predictors that are not shows centered)
plotmodel <- glmmTMB(t.prop.time ~ (action + child.adult.code) +
  (1 + (action.PI.code + action.R.code) || single.id) +
  (1 + child.adult.code || dyad.id) +
  (1 + z.trial + (action.PI.code + action.R.code) || box),
data = t.data, family = beta_family(link = "logit"), control = contr,
weights = time.fixations.screen,
REML = FALSE
)
# bootstraps for plotmodel
boot.plot <- boot.glmmTMB(
  model.res = plotmodel, data = t.data, excl.non.conv = T,
  nboots = 1000, para = T, level = 0.95, use = c("action"), # resol=100,
  contr = contr, circ.var.name = NULL, circ.var = NULL, n.cores = c("all-1", "all"),
  save.path = NULL, load.lib = T, lib.loc = .libPaths(), set.all.effects.2.zero = F
)
table(boot.plot$ci.fitted$action)

# post-hoc pairwise comparison
# for action type
emm <- emmeans(full.1, ~action)
summary(emm, type = "response")
plot(emm, by = "action", intervals = TRUE, type = "response")
emmeans(full.1, pairwise ~ action, adjust = "tukey")
summary(pairs(emm), type = "response")
summary(pairs(regrid(emm)), type = "response")

emmip(emm, ~"action", type = "link")
emmip(emm, ~"action", type = "response")
plot(effect("action", full.1))

# descriptive statistics
tapply(t.data$prop.time, t.data$trial, mean)
tapply(t.data$prop.time, list(t.data$action, t.data$child.adult), mean)
aggregate(t.data$prop.time, list(t.data$child.adult, t.data$action, t.data$trial), mean)
tapply(t.data$prop.time, list(t.data$child.adult, t.data$action, t.data$trial), mean)

save.image("./Analysis_1_Face_Looking_Time/Analysis_1_Face_Looking_Time.RData")
