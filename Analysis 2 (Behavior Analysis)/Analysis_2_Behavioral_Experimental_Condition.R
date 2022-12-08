## Project Name: OI_EYE_2
## Analysis 2 - Analysis of behavioral data -
## comparison of experimental groups

## We analyzed whether children and adults over-imitated certain action types more than others.

## Written by: Hanna Schleihauf
## Date: 4 December 2022

# load necessary functions and packages
library("lme4")
library("effects")
library("emmeans")
library("lme4")
library("ggplot2")
library("tidyr")
library("dplyr")
library("car")
library("tidyverse")

source("./functions/diagnostic_fcns.r")
source("./functions/glmm_stability.r")
source("./functions/drop1_para.r")
source("./functions/boot_glmm.r")

options(scipen = 999)

# load data
xxdata <- read.table(file = "./data_processed/processed_behavioral_data.txt", header = T, sep = "\t", stringsAsFactors = T)
# subset data
xdata <- subset(xxdata, xxdata$action.type == "experimental")

# inspecting the response
ftable(oi.score ~ child.adult + action.type, data = xdata) # All of the adults performed all relevant actions, thus we likley have a complete seperation problem

# Changing response to solve the complete separation problem --------------
# package to keep warnings
library("kyotil")

# making sure factors are factors etc.
xdata$box.id <- as.factor(xdata$box.id)
xdata$action.type <- as.factor(xdata$action.type)
xdata$counterbalancing <- as.factor(xdata$action.type)
# making NC actions the reference category of action.type
xdata$action.type <- relevel(xdata$action, ref = "NC")
# making children the reference category of child.adult
xdata$child.adult <- relevel(as.factor(xdata$child.adult), ref = "child")

# prepare model fitting (dummy coding and centering)
xx.fe.re <- fe.re.tab(
  fe.model = "oi.score  ~ child.adult*action.type",
  re = "(1|dyad.id) + (1|single.id) + (1|box.id)", data = xdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

# center all  variables included in random slope
t.data$child.adult.code <- as.numeric(t.data$child.adult) - mean(as.numeric(t.data$child.adult))
t.data$action.type.PI.code <- as.numeric(t.data$action.type.PI) - mean(as.numeric(t.data$action.type.PI))
t.data$action.type.R.code <- as.numeric(t.data$action.type.R) - mean(as.numeric(t.data$action.type.R))
t.data.z.trial.per.action.type <- scale(as.numeric(t.data$trial.per.group))

# determining in which cells we will be changing one data point at a time
to.change.1 <- (1:nrow(t.data))[(t.data$action.type == "R" & t.data$child.adult == "adult")]
# to.change.others = (1:nrow(t.data))[t.data$resp == 1] #this way the number of 0 and 1 s stays the same

# creating empty data frame to store the results
all.res <- data.frame(
  n.sim = c(1:length(to.change.1)),
  to.change.1 = NA,
  # full warning
  full.warnings = NA,
  # all.full.coeffs
  all.full.coeffs.intercept = NA,
  all.full.coeffs.action.typePI = NA,
  all.full.coeffs.action.typeR = NA,
  all.full.coeffs.child.adultadult = NA,
  all.full.coeffs.action.typePI.child.adultadult = NA,
  all.full.coeffs.action.typeR.child.adultadult = NA,
  # null warnings
  null.warnings = NA,
  # test.full.null
  test.full.null.Chisq = NA,
  test.full.null.Df = NA,
  test.full.null.Pr..Chisq = NA,
  # red warnings
  red.warnings = NA,
  # red coefficients
  all.red.coeffs.intercept = NA,
  all.red.coeffs.action.typePI = NA,
  all.red.coeffs.action.typeR = NA,
  all.red.coeffs.child.adultadult = NA,
  # red reduced model comparisons
  test.2.way.action.type.child.adult.Chisq = NA,
  test.2.way.action.type.child.adult.Chi.Df = NA,
  test.2.way.action.type.child.adult.Pr..Chisq = NA,
  test.2.way.action.type.child.adult.n.opt.warnings = NA,
  test.2.way.action.type.child.adult.n.fun.warnings = NA,
  #
  test.main.child.adult.Chisq = NA,
  test.main.child.adult.Chi.Df = NA,
  test.main.child.adult.Pr..Chisq = NA,
  test.main.child.adult.n.opt.warnings = NA,
  test.main.child.adult.n.fun.warnings = NA,
  #
  test.main.action.type.Chisq = NA,
  test.main.action.type.Chi.Df = NA,
  test.main.action.type.Pr..Chisq = NA,
  test.main.action.type.n.opt.warnings = NA,
  test.main.action.type.n.fun.warnings = NA,
  # assumptions
  overdispersion.test.full = NA,
  colliniarity.test.child.adult = NA,
  colliniarity.test.action.type = NA
)

emm.post.hoc.full <- c()
emm.post.hoc.full.cis <- c()
emm.post.hoc.action.type <- c()
emm.post.hoc.action.type.cis <- c()
emm.post.hoc.child.adult <- c()
emm.post.hoc.child.adult.cis <- c()

boot.full.values <- c()
boot.plot.action.type.values <- c()
boot.plot.child.adult.values <- c()

boot.full.estimates <- c()
boot.plot.action.type.estimates <- c()
boot.plot.child.adult.estimates <- c()

# Running the analysis --------------------------------------------------
# fitting the full model to see whether complete separation leads to a problem
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))

for (i in 1:(length(to.change.1))) { # i = 1
  set.seed(i)
  # make a new variable for the response
  t.data$new.resp <- as.numeric(t.data$oi.score)
  xx <- to.change.1[i]
  t.data$new.resp[xx] <- ifelse(t.data$new.resp[xx] == 0, 1, 0)
  ftable(new.resp ~ action.type + child.adult, t.data) # check whether it worked
  all.res$to.change.1[i] <- xx

  # Full model
  full1 <- keepWarnings(glmer(new.resp ~ action.type * child.adult +
    (1 + (action.type.PI.code + action.type.R.code) || single.id) +
    (1 + child.adult.code || dyad.id) +
    (1 + (action.type.PI.code + action.type.R.code) || box.id),
  data = t.data, control = contr, family = binomial
  ))
  red1 <- keepWarnings(glmer(new.resp ~ action.type + child.adult +
    (1 + (action.type.PI.code + action.type.R.code) || single.id) +
    (1 + child.adult.code || dyad.id) +
    (1 + (action.type.PI.code + action.type.R.code) || box.id),
  data = t.data, control = contr, family = binomial
  ))


  if (length(full1$warnings) == 0 & length(red1$warnings) == 0) {
    full <- full1$value
    red <- red1$value

    all.res$full.warnings[i] <- "no"
    all.res$all.full.coeffs.intercept[i] <- summary(full)$coefficients["(Intercept)", 1]
    all.res$all.full.coeffs.action.typePI[i] <- summary(full)$coefficients["action.typePI", 1]
    all.res$all.full.coeffs.action.typeR[i] <- summary(full)$coefficients["action.typeR", 1]
    all.res$all.full.coeffs.child.adultadult[i] <- summary(full)$coefficients["child.adultadult", 1]
    all.res$all.full.coeffs.action.typePI.child.adultadult[i] <- summary(full)$coefficients["action.typePI:child.adultadult", 1]
    all.res$all.full.coeffs.action.typeR.child.adultadult[i] <- summary(full)$coefficients["action.typeR:child.adultadult", 1]

    # full-red model comparisons
    tests.full.red <- drop1p(model.res = full, para = F, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
    test.2.way.int <- as.data.frame(tests.full.red$drop1.res[2, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])
    all.res$test.2.way.action.type.child.adult.Chisq[i] <- test.2.way.int$Chisq
    all.res$test.2.way.action.type.child.adult.Chi.Df[i] <- test.2.way.int$Chi.Df
    all.res$test.2.way.action.type.child.adult.Pr..Chisq[i] <- test.2.way.int$Pr..Chisq.
    all.res$test.2.way.action.type.child.adult.n.opt.warnings[i] <- test.2.way.int$n.opt.warnings
    all.res$test.2.way.action.type.child.adult.n.fun.warnings[i] <- test.2.way.int$n.fun.warnings

    # assumptions
    all.res$overdispersion.test.full[i] <- overdisp.test(full)[1, c("dispersion.parameter")]
    # colliniarity
    all.res$colliniarity.test.action.type[i] <- vif(red)[1, 3]
    all.res$colliniarity.test.child.adult[i] <- vif(red)[2, 3]

    # post hoc pairwise comparisons model with three-way interaction
    emm <- emmeans(full, ~ action.type * child.adult)
    emm.post.hoc.full.cis <- rbind(emm.post.hoc.full.cis, as.data.frame(emm))

    emm.pairs.full <- summary(contrast(emm, "pairwise")[c(1, 2, 3, 6, 8, 12, 13, 14, 15)],
      type = "response", adjust = "fdr"
    )
    emm.post.hoc.full <- rbind(emm.post.hoc.full, data.frame(
      term = emm.pairs.full$contrast,
      odds.ratio = emm.pairs.full$odds.ratio,
      se = emm.pairs.full$SE,
      p.value = emm.pairs.full$p.value
    ))

    # boot full
    boot.full <- boot.glmm.pred(
      model.res = full, excl.warnings = T, nboots = 10,
      para = F, resol = 100, level = 0.95, use = c("action.type", "child.adult")
    )

    boot.full$ci.predicted$name <- paste(boot.full$ci.predicted$action.type,
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
  } else {
    all.res$full.warnings[i] <- "yes"
  }

  # Null model
  null1 <- keepWarnings(glmer(new.resp ~ 1 +
    (1 + (action.type.PI.code + action.type.R.code) || single.id) +
    (1 + child.adult.code || dyad.id) +
    (1 + (action.type.PI.code + action.type.R.code) || box.id),
  data = t.data, control = contr, family = binomial
  ))

  if (length(full1$warnings) == 0 & length(null1$warnings) == 0) {
    null <- null1$value
    all.res$null.warnings[i] <- "no"

    # full null model comparisons
    test.full.null <- as.data.frame(anova(null, full, test = "Chisq"))["full", c("Chisq", "Df", "Pr(>Chisq)")]
    all.res$test.full.null.Chisq[i] <- test.full.null$Chisq
    all.res$test.full.null.Df[i] <- test.full.null$Df
    all.res$test.full.null.Pr..Chisq[i] <- test.full.null$`Pr(>Chisq)`
  } else {
    all.res$null.warnings[i] <- "yes"
  }

  # Red model
  if (length(red1$warnings) == 0) {
    red <- red1$value
    all.res$red.warnings[i] <- "no"

    all.res$all.red.coeffs.intercept[i] <- summary(red)$coefficients["(Intercept)", 1]
    all.res$all.red.coeffs.action.typePI[i] <- summary(red)$coefficients["action.typePI", 1]
    all.res$all.red.coeffs.action.typeR[i] <- summary(red)$coefficients["action.typeR", 1]
    all.res$all.red.coeffs.child.adultadult[i] <- summary(red)$coefficients["child.adultadult", 1]

    # red-main model comparisons
    tests.red.main <- drop1p(model.res = red, para = F, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
    test.main.action.type <-
      as.data.frame(tests.red.main$drop1.res[2, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])
    test.main.child.adult <-
      as.data.frame(tests.red.main$drop1.res[3, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])


    all.res$test.main.child.adult.Chisq[i] <- test.main.child.adult$Chisq
    all.res$test.main.child.adult.Chi.Df[i] <- test.main.child.adult$Chi.Df
    all.res$test.main.child.adult.Pr..Chisq[i] <- test.main.child.adult$Pr..Chisq.
    all.res$test.main.child.adult.n.opt.warnings[i] <- test.main.child.adult$n.opt.warnings
    all.res$test.main.child.adult.n.fun.warnings[i] <- test.main.child.adult$n.fun.warnings
    #
    all.res$test.main.action.type.Chisq[i] <- test.main.action.type$Chisq
    all.res$test.main.action.type.Chi.Df[i] <- test.main.action.type$Chi.Df
    all.res$test.main.action.type.Pr..Chisq[i] <- test.main.action.type$Pr..Chisq.
    all.res$test.main.action.type.n.opt.warnings[i] <- test.main.action.type$n.opt.warnings
    all.res$test.main.action.type.n.fun.warnings[i] <- test.main.action.type$n.fun.warnings


    # post hoc pairwise comparisons model with only one two-way interactions
    emm <- emmeans(red, ~action.type)
    emm.pairs.red <- summary(contrast(emm, "pairwise"),
      type = "response", adjust = "tukey"
    )
    emm.post.hoc.action.type <- rbind(emm.post.hoc.action.type, data.frame(
      term = emm.pairs.red$contrast,
      odds.ratio = emm.pairs.red$odds.ratio,
      se = emm.pairs.red$SE,
      p.value = emm.pairs.red$p.value
    ))

    emm <- emmeans(red, ~child.adult)
    emm.pairs.red <- summary(contrast(emm, "pairwise"),
      type = "response", adjust = "tukey"
    )
    emm.post.hoc.child.adult <- rbind(emm.post.hoc.child.adult, data.frame(
      term = emm.pairs.red$contrast,
      odds.ratio = emm.pairs.red$odds.ratio,
      se = emm.pairs.red$SE,
      p.value = emm.pairs.red$p.value
    ))

    # boot action.type
    plot.model.action.type <- keepWarnings(glmer(new.resp ~ action.type + child.adult.code +
      (1 + (action.type.PI.code + action.type.R.code) || single.id) +
      (1 + child.adult.code || dyad.id) +
      (1 + (action.type.PI.code + action.type.R.code) || box.id),
    data = t.data, control = contr, family = binomial
    ))

    if (length(plot.model.action.type$warnings) == 0) {
      boot.plot.action.type <- boot.glmm.pred(
        model.res = plot.model.action.type$value, excl.warnings = T, nboots = 10,
        para = F, resol = 100, level = 0.95, use = c("action.type")
      )

      boot.plot.action.type.values <-
        rbind(boot.plot.action.type.values, data.frame(
          term = boot.plot.action.type$ci.predicted$action.type,
          fitted = boot.plot.action.type$ci.predicted$fitted,
          lower.cl = boot.plot.action.type$ci.predicted$lower.cl,
          upper.cl = boot.plot.action.type$ci.predicted$upper.cl
        ))
      boot.plot.action.type.estimates <-
        rbind(boot.plot.action.type.estimates, data.frame(
          term = rownames(boot.plot.action.type$ci.estimates),
          orig = boot.plot.action.type$ci.estimates$orig,
          X2.5. = boot.plot.action.type$ci.estimates$X2.5.,
          X97.5. = boot.plot.action.type$ci.estimates$X97.5.
        ))
    }

    # boot child.adult
    plot.model.child.adult <- keepWarnings(glmer(new.resp ~ (action.type.PI.code + action.type.R.code) + child.adult +
      (1 + (action.type.PI.code + action.type.R.code) || single.id) +
      (1 + child.adult.code || dyad.id) +
      (1 + (action.type.PI.code + action.type.R.code) || box.id),
    data = t.data, control = contr, family = binomial
    ))

    if (length(plot.model.child.adult$warnings) == 0) {
      boot.plot.child.adult <- boot.glmm.pred(
        model.res = plot.model.child.adult$value, excl.warnings = T, nboots = 10,
        para = F, resol = 100, level = 0.95, use = c("child.adult")
      )

      boot.plot.child.adult.values <-
        rbind(boot.plot.child.adult.values, data.frame(
          term = boot.plot.child.adult$ci.predicted$child.adult,
          fitted = boot.plot.child.adult$ci.predicted$fitted,
          lower.cl = boot.plot.child.adult$ci.predicted$lower.cl,
          upper.cl = boot.plot.child.adult$ci.predicted$upper.cl
        ))
      boot.plot.child.adult.estimates <-
        rbind(boot.plot.child.adult.estimates, data.frame(
          term = rownames(boot.plot.child.adult$ci.estimates),
          orig = boot.plot.child.adult$ci.estimates$orig,
          X2.5. = boot.plot.child.adult$ci.estimates$X2.5.,
          X97.5. = boot.plot.child.adult$ci.estimates$X97.5.
        ))
    }
  } else {
    all.res$red.warnings[i] <- "yes"
  }


  print(i)
}


# Evaluation of the results -----------------------------------------------
str(all.res)
# how many models did converge
sum(all.res$full.warnings == "no")
all.res.2 <- subset(all.res, full.warnings == "no")

# means of full-null-comparisons
# Chisq
round(mean(all.res.2$test.full.null.Chisq, na.rm = T), 10)
round(range(all.res.2$test.full.null.Chisq, na.rm = T), 10)
# DF
range(all.res.2$test.full.null.Df, na.rm = T)
# p-value
round(mean(all.res.2$test.full.null.Pr..Chisq, na.rm = T), 10)
round(range(all.res.2$test.full.null.Pr..Chisq, na.rm = T), 10)

# assumption tests
# overdispersion parameter
round(mean(all.res.2$overdispersion.test.full, na.rm = T), 10)
round(range(all.res.2$overdispersion.test.full, na.rm = T), 10)
# colliniarity
round(mean(all.res.2$colliniarity.test.action.type, na.rm = T), 10)
round(range(all.res.2$colliniarity.test.action.type, na.rm = T), 10)
round(mean(all.res.2$colliniarity.test.child.adult, na.rm = T), 10)
round(range(all.res.2$colliniarity.test.child.adult, na.rm = T), 10)

# means of two-way interaction
# Chisq
round(mean(all.res.2$test.2.way.action.type.child.adult.Chisq, na.rm = T), 3)
round(range(all.res.2$test.2.way.action.type.child.adult.Chisq, na.rm = T), 3)
# DF
range(all.res.2$test.2.way.action.type.child.adult.Chi.Df, na.rm = T)
# p-value
round(mean(all.res.2$test.2.way.action.type.child.adult.Pr..Chisq, na.rm = T), 3)
round(range(all.res.2$test.2.way.action.type.child.adult.Pr..Chisq, na.rm = T), 3)
hist(all.res.2$test.2.way.action.type.child.adult.Pr..Chisq)

# Main Effect Age
# Chisq
round(mean(all.res.2$test.main.child.adult.Chisq, na.rm = T), 3)
round(range(all.res.2$test.main.child.adult.Chisq, na.rm = T), 3)
# DF
range(all.res.2$test.main.child.adult.Chi.Df, na.rm = T)
# p-value
round(mean(all.res.2$test.main.child.adult.Pr..Chisq, na.rm = T), 3)
round(range(all.res.2$test.main.child.adult.Pr..Chisq, na.rm = T), 3)
hist(all.res.2$test.main.child.adult.Pr..Chisq, breaks = seq(from = 0, to = 1, by = 0.05))

# Main Effect action.type
# Chisq
round(mean(all.res.2$test.main.action.type.Chisq, na.rm = T), 3)
round(range(all.res.2$test.main.action.type.Chisq, na.rm = T), 3)
# DF
range(all.res.2$test.main.action.type.Chi.Df, na.rm = T)
# p-value
round(mean(all.res.2$test.main.action.type.Pr..Chisq, na.rm = T), 10)
round(range(all.res.2$test.main.action.type.Pr..Chisq, na.rm = T), 3)
hist(all.res.2$test.main.action.type.Pr..Chisq, breaks = seq(from = 0, to = 1, by = 0.05))

library("tidyverse")
all.coefs <-
  all.res.2 %>%
  select(vars_select(names(all.res.2), starts_with("all.full.coeffs", ignore.case = TRUE)))

round(colMeans(all.coefs, na.rm = T), 3)
data.frame(min = sapply(all.coefs, min, na.rm = T), max = sapply(all.coefs, max, na.rm = T))

# post-hoc full
xx <- mapply(FUN = tapply, X = as.data.frame(emm.post.hoc.full$p.value), MoreArgs = list(INDEX = emm.post.hoc.full$term, FUN = median))
round(xx, 3)

# post-hoc red
xx <- mapply(FUN = tapply, X = as.data.frame(emm.post.hoc.action.type$p.value), MoreArgs = list(INDEX = emm.post.hoc.action.type$term, FUN = mean))
round(xx, 3)

xx <- mapply(FUN = tapply, X = as.data.frame(emm.post.hoc.child.adult$p.value), MoreArgs = list(INDEX = emm.post.hoc.child.adult$term, FUN = mean))
round(xx, 3)

# bootstrap fitted values
xx <- mapply(FUN = tapply, X = as.data.frame(boot.full.values$fitted), MoreArgs = list(INDEX = boot.full.values$term, FUN = mean))
round(xx, 3)
# bootstrap lower cl
xx <- mapply(FUN = tapply, X = as.data.frame(boot.full.values$lower.cl), MoreArgs = list(INDEX = boot.full.values$term, FUN = mean))
round(xx, 3)
# bootstrap upper cl
xx <- mapply(FUN = tapply, X = as.data.frame(boot.full.values$upper.cl), MoreArgs = list(INDEX = boot.full.values$term, FUN = mean))
round(xx, 3)

# bootstrap fitted values
xx <- mapply(FUN = tapply, X = as.data.frame(boot.full.estimates$orig), MoreArgs = list(INDEX = boot.full.estimates$term, FUN = mean))
round(xx, 3)
# bootstrap lower cl
xx <- mapply(FUN = tapply, X = as.data.frame(boot.full.estimates$X2.5.), MoreArgs = list(INDEX = boot.full.estimates$term, FUN = mean))
round(xx, 3)
# bootstrap upper cl
xx <- mapply(FUN = tapply, X = as.data.frame(boot.full.estimates$X97.5.), MoreArgs = list(INDEX = boot.full.estimates$term, FUN = mean))
round(xx, 3)

# save.image("Analysis_2_Behavioral_Experimental_Comparison.RData")

load("Analysis_2_Behavioral_Experimental_Comparison.RData")
