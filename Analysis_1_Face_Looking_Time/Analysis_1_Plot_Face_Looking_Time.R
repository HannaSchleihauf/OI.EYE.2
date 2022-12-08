## Project Name: OI_EYE_2
## Analysis 1 - Plot for the analysis of face-looking times

## Do children look longer into the model's face when no-contact actions
## are demonstrated compared to when pseudo-instrumental actions or relevant actions
## are demonstrated? Our predictors are the factors  action type
## (pseudo-instrumental/no-contact/relevant; within-subjects)
## and age group (child/adult; between-subjects).

## Written by: Hanna Schleihauf
## Date: 17 January 2022



## model values in link space
summary(plotmodel)$coefficients$cond[, 1]
b0 <- summary(plotmodel)$coefficients$cond[1, 1] # intercept
XactionPI <- summary(plotmodel)$coefficients$cond[2, 1]
XactionR <- summary(plotmodel)$coefficients$cond[3, 1]
Xchild.adult.code <- summary(plotmodel)$coefficients$cond[4, 1]

NC_logs <- b0
PI_logs <- b0 + XactionPI # the reference group
R_logs <- b0 + XactionR

# Compute the probabilities (this is what will actually get plotted):
NC_probs <- plogis(NC_logs)
PI_probs <- plogis(PI_logs)
R_probs <- plogis(R_logs)

# I could also simply use boot.plot$ci.fitted
round(boot.plot$ci.fitted$fitted, 3)

plot.data <- data.frame(NC = NC_probs, PI = PI_probs, R = R_probs)
plot.data <- gather(plot.data, key = action, value = prob, c("NC", "PI", "R"))
str(plot.data)

str(xxdata)

library(ggplot2)
#### plot action type
xxdata$prob <- xxdata$prop.time
theme_set(theme_gray()) # pre-set the bw theme.
g <- ggplot(xxdata, aes(x = action, y = prob, colour = action))
g <- g + geom_jitter(width = 0.3, height = 0.025, alpha = 0.3)
# confidence intervals
g <- g + geom_errorbar(aes(
  x = c("NC"),
  ymin = c(boot.plot$ci.fitted$lower.cl[boot.plot$ci.fitted$action == "NC"]),
  ymax = c(boot.plot$ci.fitted$upper.cl[boot.plot$ci.fitted$action == "NC"])
),
width = .125, colour = "red4",
position = position_dodge(0.9)
)
g <- g + geom_errorbar(aes(
  x = c("PI"),
  ymin = c(boot.plot$ci.fitted$lower.cl[boot.plot$ci.fitted$action == "PI"]),
  ymax = c(boot.plot$ci.fitted$upper.cl[boot.plot$ci.fitted$action == "PI"])
),
width = .125, colour = "springgreen4",
position = position_dodge(0.9)
)
g <- g + geom_errorbar(aes(
  x = c("R"),
  ymin = c(boot.plot$ci.fitted$lower.cl[boot.plot$ci.fitted$action == "R"]),
  ymax = c(boot.plot$ci.fitted$upper.cl[boot.plot$ci.fitted$action == "R"])
),
width = .125, colour = "blue",
position = position_dodge(0.9)
)
g <- g + geom_crossbar(
  data = plot.data, aes(ymin = prob, ymax = prob),
  size = 0.5, col = c("red4", "springgreen4", "blue"), width = .5
)
g <- g +
  labs( # subtitle="main effect - action type",
    y = "proportional fixation time on the model's face",
    x = "action type",
    title = "Analysis 1: Eye-Tracking Analysis"
  )

g <- g + theme(legend.position = "none")
g <- g + theme(
  axis.text.x = element_text(
    size = 10, face = NULL,
    margin = margin(
      t = 1, r = 0, b = 1, l = 0,
      unit = "mm"
    )
  ),
  axis.text.y = element_text(
    size = 9, face = NULL,
    margin = margin(
      t = 0, r = 1, b = 0, l = 1,
      unit = "mm"
    )
  ),
  axis.title.x = element_text(
    size = 10, face = NULL,
    margin = margin(
      t = 2, r = 1, b = 0, l = 1,
      unit = "mm"
    )
  ),
  axis.title.y = element_text(
    size = 10, face = NULL,
    margin = margin(
      t = 1, r = 1, b = 0, l = 0,
      unit = "mm"
    )
  )
)
g <- g + theme(panel.background = element_rect(fill = "white", colour = "grey50"))

palette()
g <- g + scale_color_manual(values = c("#EE2C2C", "#43CD80", "#2297E6"))
g <- g + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
g


#### plot action type*age group
boot.full$ci.fitted

# Compute the probibilities (this is what will actually get plotted):
NC_probs_child <- boot.full$ci.fitted$fitted[1]
NC_probs_adult <- boot.full$ci.fitted$fitted[4]
PI_probs_child <- boot.full$ci.fitted$fitted[2]
PI_probs_adult <- boot.full$ci.fitted$fitted[5]
R_probs_child <- boot.full$ci.fitted$fitted[3]
R_probs_adult <- boot.full$ci.fitted$fitted[6]

plot.data <- data.frame(
  NC_probs_child = NC_probs_child, NC_probs_adult = NC_probs_adult,
  PI_probs_child = PI_probs_child, PI_probs_adult = PI_probs_adult,
  R_probs_child = R_probs_child, R_probs_adult = R_probs_adult
)
plot.data <- gather(plot.data,
  key = action, value = prob,
  c("NC_probs_child", "NC_probs_adult", "PI_probs_child", "PI_probs_adult", "R_probs_child", "R_probs_adult")
)
plot.data$child.adult <- c("child", "parent", "child", "parent", "child", "parent")
plot.data$action <- c("NC", "NC", "PI", "PI", "R", "R")

df <- data.frame(x = rnorm(10), y = rnorm(10))

xxdata$prob <- xxdata$prop.time
theme_set(theme_gray()) # pre-set the bw theme.
g <- ggplot(xxdata, aes(x = interaction(child.adult, action), y = prob, colour = interaction(child.adult, action)))
g <- g +
  geom_rect(xmin = 0.6, xmax = 2.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5) +
  geom_rect(xmin = 2.6, xmax = 4.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5) +
  geom_rect(xmin = 4.6, xmax = 6.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5)
g <- g + geom_jitter(width = 0.3, height = 0.025, alpha = 0.3)
# confidence intervals
g <- g + geom_errorbar(aes(
  x = c("child.NC"),
  ymin = c(boot.full$ci.fitted$lower.cl[boot.full$ci.fitted$action == "NC" & boot.full$ci.fitted$child.adult == "child"]),
  ymax = c(boot.full$ci.fitted$upper.cl[boot.full$ci.fitted$action == "NC" & boot.full$ci.fitted$child.adult == "child"])
),
width = .125, colour = "#EE2C2C",
position = position_dodge(0.9)
)
g <- g + geom_errorbar(aes(
  x = c("parent.NC"),
  ymin = c(boot.full$ci.fitted$lower.cl[boot.full$ci.fitted$action == "NC" & boot.full$ci.fitted$child.adult == "parent"]),
  ymax = c(boot.full$ci.fitted$upper.cl[boot.full$ci.fitted$action == "NC" & boot.full$ci.fitted$child.adult == "parent"])
),
width = .125, colour = "red4",
position = position_dodge(0.9)
)
g <- g + geom_errorbar(aes(
  x = c("child.PI"),
  ymin = c(boot.full$ci.fitted$lower.cl[boot.full$ci.fitted$action == "PI" & boot.full$ci.fitted$child.adult == "child"]),
  ymax = c(boot.full$ci.fitted$upper.cl[boot.full$ci.fitted$action == "PI" & boot.full$ci.fitted$child.adult == "child"])
),
width = .125, colour = "springgreen4",
position = position_dodge(0.9)
)
g <- g + geom_errorbar(aes(
  x = c("parent.PI"),
  ymin = c(boot.full$ci.fitted$lower.cl[boot.full$ci.fitted$action == "PI" & boot.full$ci.fitted$child.adult == "parent"]),
  ymax = c(boot.full$ci.fitted$upper.cl[boot.full$ci.fitted$action == "PI" & boot.full$ci.fitted$child.adult == "parent"])
),
width = .125, colour = "#006f37",
position = position_dodge(0.9)
)
g <- g + geom_errorbar(aes(
  x = c("child.R"),
  ymin = c(boot.full$ci.fitted$lower.cl[boot.full$ci.fitted$action == "R" & boot.full$ci.fitted$child.adult == "child"]),
  ymax = c(boot.full$ci.fitted$upper.cl[boot.full$ci.fitted$action == "R" & boot.full$ci.fitted$child.adult == "child"])
),
width = .125, colour = "blue",
position = position_dodge(0.9)
)
g <- g + geom_errorbar(aes(
  x = c("parent.R"),
  ymin = c(boot.full$ci.fitted$lower.cl[boot.full$ci.fitted$action == "R" & boot.full$ci.fitted$child.adult == "parent"]),
  ymax = c(boot.full$ci.fitted$upper.cl[boot.full$ci.fitted$action == "R" & boot.full$ci.fitted$child.adult == "parent"])
),
width = .125, colour = "blue4",
position = position_dodge(0.9)
)
g <- g + geom_crossbar(
  data = plot.data, aes(ymin = prob, ymax = prob),
  size = 0.5, col = c("#EE2C2C", "red4", "springgreen4", "#006f37", "blue", "blue4"), width = .5
)

g <- g + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
palette()
g <- g + scale_color_manual(values = c("#EE2C2C", "#be2323", "#43CD80", "#199657", "#2297E6", "#1b78b8"))
g <- g + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
g <- g + scale_x_discrete(labels = c("NC\nchild", "NC\nadult", "PI\nchild", "PI\nadult", "R\nchild", "R\nadult"))


g <- g +
  labs( # subtitle="main effect - action type",
    y = "proportional fixation time on the model's face",
    x = "action type * age group",
    title = "Analysis 1: Eye-Tracking Analysis"
  )

g <- g + theme(legend.position = "none")
g <- g + theme(
  axis.text.x = element_text(
    size = 10, face = NULL,
    margin = margin(
      t = 1, r = 0, b = 1, l = 0,
      unit = "mm"
    )
  ),
  axis.text.y = element_text(
    size = 9, face = NULL,
    margin = margin(
      t = 0, r = 1, b = 0, l = 1,
      unit = "mm"
    )
  ),
  axis.title.x = element_text(
    size = 10, face = NULL,
    margin = margin(
      t = 2, r = 1, b = 0, l = 1,
      unit = "mm"
    )
  ),
  axis.title.y = element_text(
    size = 10, face = NULL,
    margin = margin(
      t = 1, r = 1, b = 0, l = 0,
      unit = "mm"
    )
  )
)

g
