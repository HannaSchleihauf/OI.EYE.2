## Project Name: OI_EYE_2
## Analysis 2 - Plot -  Analysis of behavioral data -
## comparison of experimental groups and control group

## We analyzed whether children and adults over-imitated above baseline level
## for either action type category. Our predictors are the factors action type
## (pseudo-instrumental/NC; within-subjects),
## age group (child/adult; between-subjects),
## and condition (experimental/baseline; between-subjects)

# load necessary functions and packages
library("ggplot2")
library("tidyverse")

# load workspace
load("Analysis_2b_Behavioral_Baseline_Comparison.RData")

### plot condition*action*age.group
###
dev.off()
boot.plot.1 <- as.data.frame(boot.plot.1)
t.data$plot.group <- paste(t.data$action.type, t.data$condition, t.data$child.adult, sep = ".")
t.data$plot.group <- as.factor(t.data$plot.group)
t.data$plot.group <- ordered(t.data$plot.group,
  levels = c(
    "NC.baseline.child", "NC.baseline.adult",
    "PI.baseline.child", "PI.baseline.adult",
    "NC.experimental.child", "NC.experimental.adult",
    "PI.experimental.child", "PI.experimental.adult"
  )
)
levels(t.data$plot.group)
boot.plot.1$group <- rownames(boot.plot.1)



t.data$fitted <- as.numeric((t.data$oi.score))
str(t.data)
theme_set(theme_gray()) # pre-set the bw theme.
g4 <- ggplot(t.data, aes(x = plot.group, y = fitted, colour = plot.group))
g4 <- g4 +
  geom_rect(xmin = 0.6, xmax = 2.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5) +
  geom_rect(xmin = 2.6, xmax = 4.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5) +
  geom_rect(xmin = 4.6, xmax = 6.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5) +
  geom_rect(xmin = 6.6, xmax = 8.4, ymin = -0.04, ymax = 1.03, fill = "#f2f2f2", color = "#f2f2f2", alpha = 0.5)
g4 <- g4 + geom_jitter(width = 0.3, height = 0.025, alpha = 0.3)
g4 <- g4 + geom_errorbar(aes(
  x = c("NC.baseline.child"),
  ymin = c(boot.plot.1$lower.cl[boot.plot.1$group == "baseline.no.contact.child"]),
  ymax = c(boot.plot.1$upper.cl[boot.plot.1$group == "baseline.no.contact.child"])
),
width = .125, colour = (adjustcolor("#EE2C2C", alpha.f = 0.65)),
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("NC.baseline.adult"),
  ymin = c(boot.plot.1$lower.cl[boot.plot.1$group == "baseline.no.contact.adult"]),
  ymax = c(boot.plot.1$upper.cl[boot.plot.1$group == "baseline.no.contact.adult"])
),
width = .125, colour = (adjustcolor("#be2323", alpha.f = 0.65)),
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("PI.baseline.child"),
  ymin = c(boot.plot.1$lower.cl[boot.plot.1$group == "baseline.contact.child"]),
  ymax = c(boot.plot.1$upper.cl[boot.plot.1$group == "baseline.contact.child"])
),
width = .125, colour = (adjustcolor("#43CD80", alpha.f = 0.65)),
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("PI.baseline.adult"),
  ymin = c(boot.plot.1$lower.cl[boot.plot.1$group == "baseline.contact.adult"]),
  ymax = c(boot.plot.1$upper.cl[boot.plot.1$group == "baseline.contact.adult"])
),
width = .125, colour = (adjustcolor("#199657", alpha.f = 0.65)),
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("NC.experimental.child"),
  ymin = c(boot.plot.1$lower.cl[boot.plot.1$group == "experimental.no.contact.child"]),
  ymax = c(boot.plot.1$upper.cl[boot.plot.1$group == "experimental.no.contact.child"])
),
width = .125, colour = "#EE2C2C",
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("NC.experimental.adult"),
  ymin = c(boot.plot.1$lower.cl[boot.plot.1$group == "experimental.no.contact.adult"]),
  ymax = c(boot.plot.1$upper.cl[boot.plot.1$group == "experimental.no.contact.adult"])
),
width = .125, colour = "#be2323",
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("PI.experimental.child"),
  ymin = c(boot.plot.1$lower.cl[boot.plot.1$group == "experimental.contact.child"]),
  ymax = c(boot.plot.1$upper.cl[boot.plot.1$group == "experimental.contact.child"])
),
width = .125, colour = "#43CD80",
position = position_dodge(0.9)
)
g4 <- g4 + geom_errorbar(aes(
  x = c("PI.experimental.adult"),
  ymin = c(boot.plot.1$lower.cl[boot.plot.1$group == "experimental.contact.adult"]),
  ymax = c(boot.plot.1$upper.cl[boot.plot.1$group == "experimental.contact.adult"])
),
width = .125, colour = "#199657",
position = position_dodge(0.9)
)


g4 <- g4 + geom_crossbar(
  data = boot.plot.1, aes(
    x = c(
      "PI.baseline.adult", "PI.baseline.child",
      "NC.baseline.adult", "NC.baseline.child",
      "PI.experimental.adult", "PI.experimental.child",
      "NC.experimental.adult", "NC.experimental.child"
    ),
    y = fitted,
    ymin = fitted,
    ymax = fitted
  ),
  width = 0.5, colour = c(
    (adjustcolor("#199657", alpha.f = 0.75)), (adjustcolor("#43CD80", alpha.f = 0.75)),
    (adjustcolor("#be2323", alpha.f = 0.75)), (adjustcolor("#EE2C2C", alpha.f = 0.75)),
    "#199657", "#43CD80",
    "#be2323", "#EE2C2C"
  )
)
g4 <- g4 + labs(
  subtitle = "Post-hoc pairwise comparisons of experimental condition with respective baseline levels",
  y = "probability to perform irrelevant actions",
  x = element_blank(),
  title = "Analysis 2b: Behavioral Analysis - Baseline Comparison"
)
g4 <- g4 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
g4 <- g4 + theme(
  legend.title = element_blank(),
  axis.text.x = element_text(
    size = 8, face = NULL,
    margin = margin(
      t = 1, r = 0, b = 1, l = 0,
      unit = "mm"
    )
  ),
  axis.text.y = element_text(
    size = 8, face = NULL,
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
    size = 10, face = NULL,
    margin = margin(
      t = 1.5, r = 0, b = 0, l = 0,
      unit = "mm"
    )
  )
)
g4 <- g4 + scale_x_discrete(labels = c(
  "NC\nchild", "NC\nadult",
  "PI\nchild", "PI\nadult",
  "NC\nchild", "NC\nadult",
  "PI\nchild", "PI\nadult"
))
g4 <- g4 + scale_color_manual(values = c(
  (adjustcolor("#EE2C2C", alpha.f = 0.15)), adjustcolor("#be2323", alpha.f = 0.15),
  (adjustcolor("#43CD80", alpha.f = 0.15)), adjustcolor("#199657", alpha.f = 0.15),
  "#EE2C2C", "#be2323",
  "#43CD80", "#199657"
))
g4 <- g4 + theme(legend.position = "none")
g4 <- g4 + theme(panel.background = element_rect(fill = "white", colour = "grey50"))

library(grid)
text_baseline <- textGrob("baseline conditions", gp = gpar(fontsize = 10))
text_experimental <- textGrob("experimental conditions", gp = gpar(fontsize = 10))

g4 <- g4 +
  theme(plot.margin = unit(c(1, 2, 1.5, 1), "lines")) +
  annotation_custom(text_baseline, xmin = 2.5, xmax = 2.5, ymin = -0.20, ymax = -0.20) +
  annotation_custom(text_experimental, xmin = 6.5, xmax = 6.5, ymin = -0.20, ymax = -0.20) +
  coord_cartesian(clip = "off")

g4











# # Plot for action*condition interaction -----------------------------------
# dev.off()
# boot.plot.2 = as.data.frame(boot.plot.2)
# t.data$plot.group = paste(t.data$action.type, t.data$condition, sep = ".")
# t.data$plot.group = as.factor(t.data$plot.group)
# t.data$plot.group = ordered(t.data$plot.group,
#                             levels = c("NC.baseline", "NC.experimental",
#                                        "PI.baseline", "PI.experimental" ))
# levels(t.data$plot.group)
# rownames(boot.plot.2) = c("baseline.PI", "baseline.NC", "experimental.PI", "experimental.NC")
# boot.plot.2$group = rownames(boot.plot.2)
# #boot.plot.2$group[order(c("baseline.NC", "experimental.NC", "baseline.PI", "experimental.PI"))]
#
# t.data$fitted = as.numeric((t.data$oi.score))
# str(t.data)
# theme_set( theme_gray())  # pre-set the bw theme.
# g3 <- ggplot(t.data, aes(x = plot.group , y = fitted, colour = action.type))
# g3 <- g3 +
#   geom_rect(xmin = 0.6, xmax = 2.4,   ymin = -0.04, ymax = 1.03,   fill = "#f2f2f2", color="#f2f2f2", alpha=0.5) +
#   geom_rect(xmin = 2.6,    xmax = 4.4, ymin = -0.04, ymax = 1.03,   fill = "#f2f2f2", color="#f2f2f2", alpha=0.5)
# g3 <- g3 +  geom_jitter(width = 0.3, height = 0.025, alpha = 0.3)
# g3 <- g3 + geom_errorbar(aes(x=c("PI.baseline"),
#                              ymin=c(boot.plot.2$lower.cl[boot.plot.2$group == "baseline.PI"]),
#                              ymax=c(boot.plot.2$upper.cl[boot.plot.2$group == "baseline.PI"])),
#                          width=.125, colour="springgreen4",
#                          position=position_dodge(0.9))
# g3 <- g3 + geom_errorbar(aes(x=c("PI.experimental"),
#                              ymin=c(boot.plot.2$lower.cl[boot.plot.2$group == "experimental.PI"]),
#                              ymax=c(boot.plot.2$upper.cl[boot.plot.2$group == "experimental.PI"])),
#                          width=.125, colour="springgreen4",
#                          position=position_dodge(0.9))
# g3 <- g3 + geom_errorbar(aes(x=c("NC.baseline"),
#                              ymin=c(boot.plot.2$lower.cl[boot.plot.2$group == "baseline.NC"]),
#                              ymax=c(boot.plot.2$upper.cl[boot.plot.2$group == "baseline.NC"])),
#                          width=.125, colour="red4",
#                          position=position_dodge(0.9))
# g3 <- g3 + geom_errorbar(aes(x=c("NC.experimental"),
#                              ymin=c(boot.plot.2$lower.cl[boot.plot.2$group == "experimental.NC"]),
#                              ymax=c(boot.plot.2$upper.cl[boot.plot.2$group == "experimental.NC"])),
#                          width=.125, colour="red4",
#                          position=position_dodge(0.9))
# g3 <- g3 + geom_crossbar(data = boot.plot.2, aes(x=c("PI.baseline", "NC.baseline", "PI.experimental",
#                                                      "NC.experimental"),
#                                                  y = fitted,
#                                                  ymin= fitted,
#                                                  ymax= fitted),
#                          width=0.5, colour= c("springgreen4", "red4",  "springgreen4", "red4"))
# g3 = g3 +  labs(subtitle="Non-significant Interaction Effect of Condition * Action Type",
#                 y="probability to over-imitate",
#                 x=element_blank(),
#                 title="Analysis 2a1: Behavioral Analysis -\nBaseline Comparison")
# g3 = g3 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
# g3 = g3 + theme(legend.title = element_blank(),
#                 axis.text.x = element_text(size = 8, face=NULL,
#                                            margin = margin(t = 1, r = 0, b = 1, l = 0,
#                                                            unit = "mm")),
#                 axis.text.y = element_text(size = 8, face=NULL,
#                                            margin = margin(t = 0, r = 1, b = 0, l = 1,
#                                                            unit = "mm")),
#                 axis.title.x = element_text(size = 10, face=NULL,
#                                             margin = margin(t = 1.5, r = 1, b = 0, l = 0,
#                                                             unit = "mm")),
#                 axis.title.y = element_text(size = 10, face=NULL,
#                                             margin = margin(t = 1.5, r = 0, b = 0, l = 0,
#                                                             unit = "mm")))
# g3 = g3 + scale_x_discrete(labels=c("NC\nbaseline",
#                                     "NC\nexperimental",
#                                     "PI\nbaseline",
#                                     "PI\nexperimental"))
# g3 = g3 + scale_color_manual(values = c("#EE2C2C", "#43CD80"), labels = c("NC", "PI"))
# g3 = g3 + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
# g3
