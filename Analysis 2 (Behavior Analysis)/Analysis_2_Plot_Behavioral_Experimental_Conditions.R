## Project Name: OI_EYE_2
## Analysis 2 - Plot -  Analysis of behavioral data -
## comparison of experimental groups

## We analyzed whether children and adults over-imitated certain action types more than others.

## Written by: Hanna Schleihauf
## Date: 2 December 2022


# load necessary functions and packages
library("ggplot2")
library("tidyverse")

# load workspace
load("Analysis_2_Behavioral_Experimental_Comparison.RData")

### plot action*age.group
###
boot.full <- NA
boot.full$fitted <- as.data.frame(mapply(
  FUN = tapply, X = as.data.frame(boot.full.values$fitted),
  MoreArgs = list(INDEX = boot.full.values$term, FUN = mean)
))
boot.full$lower.cl <- mapply(
  FUN = tapply, X = as.data.frame(boot.full.values$lower.cl),
  MoreArgs = list(INDEX = boot.full.values$term, FUN = mean)
)
boot.full$upper.cl <- mapply(
  FUN = tapply, X = as.data.frame(boot.full.values$upper.cl),
  MoreArgs = list(INDEX = boot.full.values$term, FUN = mean)
)
t.data$plot.group <- paste(t.data$action.type, t.data$child.adult, sep = ".")
t.data$plot.group <- as.factor(t.data$plot.group)
t.data$plot.group <- ordered(t.data$plot.group,
  levels = c(
    "NC.child", "NC.adult",
    "PI.child", "PI.adult",
    "R.child", "R.adult"
  )
)
levels(t.data$plot.group)
boot.full$group <- rownames(boot.full$fitted)

# plot child.adult
t.data$fitted <- as.numeric((t.data$oi.score))
str(t.data)
theme_set(theme_gray()) # pre-set the bw theme.
g4 <- ggplot(t.data, aes(x = plot.group, y = fitted, colour = plot.group))
g4 <- g4 +
  geom_rect(xmin = 0.6, xmax = 2.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5) +
  geom_rect(xmin = 2.6, xmax = 4.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5) +
  geom_rect(xmin = 4.6, xmax = 6.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5)

g4 <- g4 + geom_jitter(width = 0.3, height = 0.025, alpha = 0.3)

g4 <- g4 + geom_errorbar(aes(
  x = c("PI.child"),
  ymin = c(boot.full$lower.cl[boot.full$group == "PI.child"]),
  ymax = c(boot.full$upper.cl[boot.full$group == "PI.child"])
),
width = .125, colour = "#43CD80",
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("PI.adult"),
  ymin = c(boot.full$lower.cl[boot.full$group == "PI.adult"]),
  ymax = c(boot.full$upper.cl[boot.full$group == "PI.adult"])
),
width = .125, colour = "#199657",
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("NC.child"),
  ymin = c(boot.full$lower.cl[boot.full$group == "NC.child"]),
  ymax = c(boot.full$upper.cl[boot.full$group == "NC.child"])
),
width = .125, colour = (adjustcolor("#EE2C2C", alpha.f = 0.15)),
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("NC.adult"),
  ymin = c(boot.full$lower.cl[boot.full$group == "NC.adult"]),
  ymax = c(boot.full$upper.cl[boot.full$group == "NC.adult"])
),
width = .125, colour = (adjustcolor("#be2323", alpha.f = 0.15)),
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("R.child"),
  ymin = c(boot.full$lower.cl[boot.full$group == "R.child"]),
  ymax = c(boot.full$upper.cl[boot.full$group == "R.child"])
),
width = .125, colour = (adjustcolor("blue", alpha.f = 0.15)),
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("R.adult"),
  ymin = c(boot.full$lower.cl[boot.full$group == "R.adult"]),
  ymax = c(boot.full$upper.cl[boot.full$group == "R.adult"])
),
width = .125, colour = (adjustcolor("blue4", alpha.f = 0.15)),
position = position_dodge(0.9)
)

fittedvalues <- as.data.frame(cbind(
  group = boot.full$group,
  fitted = boot.full$fitted$`boot.full.values$fitted`
))

g4 <- g4 + geom_crossbar(
  data = fittedvalues, aes(
    x = c(
      "NC.adult", "NC.child",
      "PI.adult", "PI.child",
      "R.adult", "R.child"
    ),
    y = as.numeric(fitted),
    ymin = as.numeric(fitted),
    ymax = as.numeric(fitted)
  ),
  width = 0.5, colour = c("#be2323", "#EE2C2C", "#199657", "#43CD80", "blue4", "blue")
)


g4 <- g4 + labs(
  subtitle = "Results",
  y = "probability to imitate",
  x = element_blank()
)
g4 <- g4 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
g4 <- g4 + theme(
  legend.title = element_blank(),
  legend.position = "none",
  axis.text.x = element_text(
    size = 10, face = NULL,
    margin = margin(
      t = 1, r = 0, b = 1, l = 0,
      unit = "mm"
    )
  ),
  axis.text.y = element_text(
    size = 11, face = NULL,
    margin = margin(
      t = 0, r = 1, b = 0, l = 1,
      unit = "mm"
    )
  ),
  axis.title.x = element_text(
    size = 10, face = NULL,
    margin = margin(
      t = 1.5, r = 1, b = 0, l = 0,
      unit = "mm"
    )
  ),
  axis.title.y = element_text(
    size = 12, face = NULL,
    margin = margin(
      t = 1.5, r = 0, b = 0, l = 0,
      unit = "mm"
    )
  )
)
g4 <- g4 + scale_x_discrete(labels = c("NC\nchild", "NC\nadult", "PI\nchild", "PI\nadult", "R\nchild", "R\nadult"))
g4 <- g4 + scale_color_manual(values = c("#EE2C2C", "#be2323", "#43CD80", "#199657", "#2297E6", "#1b78b8"))

g4 <- g4 + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
g4
