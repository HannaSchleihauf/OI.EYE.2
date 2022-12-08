## Project Name: OI_EYE_2
## Analysis 2 - Analysis of behavioral data -
## comparison of experimental groups and repective baseline groups
## We analyzed whether children and adults over-imitated above baseline level
## for either action type category.

# load necessary functions and packages
library("lme4")
library("effects")
library("emmeans")
library("lme4")
library("ggplot2")
library("tidyr")
library("dplyr")
library("car")

source("./functions/diagnostic_fcns.r")
source("./functions/glmm_stability.r")
source("./functions/drop1_para.r")
source("./functions/boot_glmm.r")

# load data
xdata <- read.table(file = "./data_processed/processed_behavioral_data.txt", header = T, sep = "\t", stringsAsFactors = T)
?read.table

# Preparing the analysis --------------------------------------------------
# inspecting the response
ftable(oi.score ~ condition + child.adult + action.type, data = xxdata)
aggregate(xxdata$oi.score, list(xxdata$child.adult, xxdata$action.type, xxdata$condition), mean, na.rm = T)
# we have a complete separation problem: In the baseline condition neither children
# nor adults performed NC action

# making sure factors are factors etc.
xdata$box.id <- as.factor(xdata$box.id)
xdata$action.type <- as.factor(xdata$action.type)
xdata$condition <- as.factor(xdata$condition)
xdata$counterbalancing <- as.factor(xdata$condition)
# making NC actions the reference category of action.type
xdata$action.type <- relevel(xdata$action, ref = "NC")
# making children the reference category of child.adult
xdata$child.adult <- relevel(as.factor(xdata$child.adult), ref = "child")

# since we are not interested in the performance of R action,
# but only in the irrelevant actions, we create a subset of the data with only the irrelevant actions
xxdata <- subset(xdata, xdata$action.type != "R")
xxdata$action.type <- droplevels(xxdata$action.type)

# prepare model fitting (dummy coding and centering)
xx.fe.re <- fe.re.tab(
  fe.model = "oi.score  ~ condition*child.adult*action.type",
  re = "(1|dyad.id) + (1|single.id) + (1|box.id)", data = xxdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

levels(t.data$child.adult)

# center all  variables included in random slope
t.data$child.adult.code <- as.numeric(t.data$child.adult) - mean(as.numeric(t.data$child.adult))
t.data$action.type.PI.code <- as.numeric(t.data$action.type.PI) - mean(as.numeric(t.data$action.type.PI))
t.data$condition.experimental.code <- as.numeric(t.data$condition.experimental) - mean(as.numeric(t.data$condition.experimental))
t.data.z.trial.per.action.type <- scale(as.numeric(t.data$trial.per.group))

# fitting the full model to see whether complete separation leads to a problem
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))

full1 <- glmer(oi.score ~ condition * action.type * child.adult +
  (1 + (action.type.PI.code) || single.id) +
  (1 + child.adult.code || dyad.id) +
  (1 + (action.type.PI.code) * condition.experimental.code || box.id),
data = t.data, control = contr, family = binomial
)

summary(full1)$coefficients # standard errors are extremely big, this results from a complete seperation problem

# Changing response to solve the complete seperation problem --------------
# function to keep warnings
keepWarnings <- function(expr) {
  localWarnings <- list()
  value <- withCallingHandlers(expr,
    warning = function(w) {
      localWarnings[[length(localWarnings) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = localWarnings)
}

# make a new variable for the response
resp <- as.numeric(t.data$oi.score)
# check again which cells have a complete separation issue (have no 1s in it)
ftable(resp ~ condition + child.adult + action.type, t.data)
tapply(resp, list(t.data$condition, t.data$child.adult, t.data$action.type), mean)
# determining in which cells we will be changing one data point at a time
to.change.1 <- (1:nrow(t.data))[(t.data$condition == "baseline" & t.data$child.adult == "adult" & t.data$action.type == "NC")]
to.change.2 <- (1:nrow(t.data))[(t.data$condition == "baseline" & t.data$child.adult == "child" & t.data$action.type == "NC")]
# to.change.others = (1:nrow(t.data))[t.data$oi.score == 1] #this way the number of 0 and 1 s stays the same
# creating empty variables to store the results
test.full.null <- c()
all.full.coeffs <- c()
test.3.way.int <- c()

overdispersion.test <- c()
colliniarity.test <- c()

emm.post.hoc.full <- c()

boot.full.values <- c()
boot.full.estimates <- c()

to.drop <- c("condition", "action.type", "child.adult")
for (i in 1:(length(to.change.1))) { # length 1 and 2 have the same length i = 1
  set.seed(i)
  new.resp <- resp
  new.resp[to.change.1[i]] <- ifelse(new.resp[to.change.1[i]] == 0, 1, 0)
  new.resp[to.change.2[i]] <- ifelse(new.resp[to.change.2[i]] == 0, 1, 0)
  # new.resp[sample(to.change.others, size=2, replace=F)] = 0

  full <- keepWarnings(glmer(new.resp ~ condition * action.type * child.adult +
    (1 + (action.type.PI.code) || single.id) +
    (1 + child.adult.code || dyad.id) +
    (1 + (action.type.PI.code) * condition.experimental.code || box.id), #+
  data = t.data, control = contr, family = binomial
  ))
  null <- keepWarnings(glmer(new.resp ~ 1 +
    (1 + (action.type.PI.code) || single.id) +
    (1 + child.adult.code || dyad.id) +
    (1 + (action.type.PI.code) * condition.experimental.code || box.id), #+
  data = t.data, control = contr, family = binomial
  ))

  # Full
  if (length(full$warnings) == 0 & length(null$warnings) == 0) {
    full.new <- full$value
    null.new <- null$value


    # full.new null.new model comparisons
    test.full.null <- rbind(test.full.null, as.data.frame(anova(null.new, full.new, test = "Chisq"))["full.new", c("Chisq", "Df", "Pr(>Chisq)")])
    # full.new coefficients
    xx <- summary(full.new)$coefficients
    all.full.coeffs <- rbind(all.full.coeffs, data.frame(term = rownames(xx), xx))
    # full.new-red.new model comparisons
    tests <- drop1p(model.res = full.new, para = F, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
    test.3.way.int <- rbind(test.3.way.int, as.data.frame(tests$drop1.res[2, c("Chisq", "Chi.Df", "Pr..Chisq.")])) # same as drop1

    # assumptions
    xx <- overdisp.test(full.new)
    overdispersion.test <-
      rbind(overdispersion.test, as.data.frame(xx[1, c("dispersion.parameter")]))
    # colliniarity
    yy <- lm(new.resp ~ condition + action.type + child.adult,
      data = t.data
    )
    xx <- as.data.frame(vif(yy))
    colliniarity.test <-
      rbind(colliniarity.test, data.frame(
        term = rownames(xx),
        vif.value = xx[, 1]
      ))

    # post hoc pairwise comparisons model with both two-way interactions
    emm <- emmeans(full.new, ~ action.type * condition * child.adult)
    emm.pairs <- summary(contrast(emm, "pairwise")[c(9, 24, 27, 2)], type = "response", adjust = "none")
    emm.post.hoc.full <- rbind(emm.post.hoc.full, data.frame(
      term = emm.pairs$contrast,
      odds.ratio = emm.pairs$odds.ratio,
      se = emm.pairs$SE,
      p.value = emm.pairs$p.value
    ))
    ### boot.full.new
    boot.full <- boot.glmm.pred(
      model.res = full.new, excl.warnings = T, nboots = 25,
      para = F, resol = 100, level = 0.95, use = c("condition", "action.type", "child.adult")
    )

    boot.full$ci.predicted$name <- paste(boot.full$ci.predicted$condition,
      boot.full$ci.predicted$action.type,
      boot.full$ci.predicted$child.adult,
      sep = "."
    )
    boot.full.values <-
      rbind(boot.full.values, data.frame(
        term = boot.full$ci.predicted$name,
        fitted = boot.full$ci.predicted$fitted,
        lower.cl = boot.full$ci.predicted$lower.cl,
        upper.cl = boot.full$ci.predicted$upper.cl
      ))
    boot.full.estimates <-
      rbind(boot.full.estimates, data.frame(
        term = rownames(boot.full$ci.estimates),
        orig = boot.full$ci.estimates$orig,
        X2.5. = boot.full$ci.estimates$X2.5.,
        X97.5. = boot.full$ci.estimates$X97.5.
      ))
  }

  print(i)
}

# save.image("Analysis_2b_Behavioral_Baseline_Comparison.RData")
load("Analysis_2b_Behavioral_Baseline_Comparison.RData")



# Evaluation of the results -----------------------------------------------
#### FULL
# full null model comparison:
# test.full.null
xx <- round(apply(test.full.null, 2, mean), 3)
xx

# coefficients of the full model
# all.full.coeffs
xx <- mapply(FUN = tapply, X = all.full.coeffs[, -1], MoreArgs = list(INDEX = all.full.coeffs$term, FUN = mean))
round(xx, 3)
write.table(round(xx, 3), "analysis.2a_test.full.null.txt", quote = FALSE, sep = "\t")

# overdispersion test
xx <- round(apply(overdispersion.test, 2, mean), 3)
xx

## colliniarity test
xx <- mapply(FUN = tapply, X = colliniarity.test, MoreArgs = list(INDEX = colliniarity.test$term, FUN = mean))
round(xx, 3)

# test of the three-way interaction:
# test.3.way.int
xx <- round(apply(test.3.way.int, 2, mean), 3)
xx


# post-hoc full
xx <- mapply(FUN = tapply, X = as.data.frame(emm.post.hoc.full$p.value), MoreArgs = list(INDEX = emm.post.hoc.full$term, FUN = mean))
round(xx, 3)

xx <- mapply(FUN = tapply, X = as.data.frame(emm.post.hoc.full$odds.ratio), MoreArgs = list(INDEX = emm.post.hoc.full$term, FUN = mean))
round(xx, 3)

xx <- mapply(FUN = tapply, X = as.data.frame(emm.post.hoc.full$se), MoreArgs = list(INDEX = emm.post.hoc.full$term, FUN = mean))
round(xx, 3)


## boot.full values
boot.plot.1 <- mapply(FUN = tapply, X = boot.full.values[, -1], MoreArgs = list(INDEX = boot.full.values$term, FUN = mean)) # median
round(boot.plot.1, 3)

## boot.full estimates
xx <- mapply(FUN = tapply, X = boot.full.estimates[, -1], MoreArgs = list(INDEX = boot.full.estimates$term, FUN = mean)) # median
round(xx, 3)


# interrater reli
reli.data <- subset(xdata, xdata$oi.score.blind != "NA")
ratings <- reli.data %>% select(oi.score, oi.score.blind)
library(irr)
agree(ratings)

# Cohans Kappa:
(99 - 50) / (100 - 50)


save.image("Analysis_2b_Behavioral_Baseline_Comparison.RData")
