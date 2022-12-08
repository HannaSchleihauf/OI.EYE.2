#################################################################################
#################################################################################
###Plot three way interaction
#load("/Users/hanna.schleihauf/Dropbox/Research/my_projects/OI/OI_EYE_2/analysis/OI_EYE_2/with.maximum.RData")
load("/Users/hanna.schleihauf/Dropbox/Research/my_projects/OI/OI_EYE_2/shared/analysis/OI_EYE_2/Analysis 2c (behavior~eye-tracking)/boot.plot.new.RData")
round(boot.plot$ci.estimates, 3)
boot.predicted=as.data.frame(boot.plot.new$ci.predicted)
#boot.fitted=as.data.frame(boot.plot$ci.fitted)

boot.plot$xvals <- 
  seq(from=min(t.data$z.prop.time.with.max), to=max(t.data$z.prop.time.with.max), length.out = 100)

##y axis labels
range(t.data$prop.time.mean)
range(t.data$prop.time.with.max)
percentage.labels= c("0%", "10%", "20%", "30%", "40%")
percentage.comma = seq(from = 0, to = 0.4, length.out = 5)
##and a vector in z-space at which they need to be displayed
percentage.ticks=
  (percentage.comma - mean(t.data$prop.time.with.max))/
  sd(t.data$prop.time.with.max)


dev.off()
library(ggplot2)
#time plot
theme_set( theme_gray())  # pre-set the bw theme.
g <- ggplot()
g <- g + geom_jitter(data = t.data , aes(x = z.prop.time.with.max, y = behavior, colour = child.adult), width = 0.025, height = 0.025, alpha = 0.4)
g <- g + facet_wrap(~action)

NCdata = subset(t.data, t.data$action == "NC")
NCdata.child = subset(t.data, t.data$action == "NC" & t.data$child.adult == "child")
NCdata.adult = subset(t.data, t.data$action == "NC" & t.data$child.adult == "parent")
PIdata = subset(t.data, t.data$action == "PI")
PIdata.child = subset(t.data, t.data$action == "PI" & t.data$child.adult == "child")
PIdata.adult = subset(t.data, t.data$action == "PI" & t.data$child.adult == "parent")

boot.predictedNC = subset(boot.predicted, boot.predicted$action == "NC")
boot.predictedNC.child <- subset(boot.predicted, boot.predicted$action == "NC" & boot.predicted$child.adult == "child")
boot.predictedNC.adult <- subset(boot.predicted, boot.predicted$action == "NC" & boot.predicted$child.adult == "parent")
boot.predictedPI = subset(boot.predicted, boot.predicted$action == "PI")
boot.predictedPI.child <- subset(boot.predicted, boot.predicted$action == "PI" & boot.predicted$child.adult == "child")
boot.predictedPI.adult <- subset(boot.predicted, boot.predicted$action == "PI" & boot.predicted$child.adult == "parent")




##NC PLOT children
gNC.C <- ggplot()
gNC.C <- gNC.C + geom_jitter(data = NCdata.child , 
                             aes(x = z.prop.time.with.max, y = behavior), #, colour = child.adult), 
                             width = 0.025, height = 0.025, colour="#EE2C2C", alpha = 0.3)
#add child CIs
gNC.C = gNC.C  + geom_ribbon(data=boot.predictedNC.child,
                             aes(x = z.prop.time.with.max,
                                 ymin = lower.cl,
                                 ymax = upper.cl),
                             alpha = 0.1,
                             fill="#EE2C2C")
gNC.C = gNC.C  + geom_line(data=boot.predictedNC.child, 
                           aes(x=z.prop.time.with.max, y=fitted), colour="#EE2C2C", lwd=1)
gNC.C = gNC.C  +  scale_x_continuous(breaks = percentage.ticks, 
                                     labels =  percentage.labels)
gNC.C = gNC.C  + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
gNC.C = gNC.C  + labs(subtitle="Children", 
                      y="", 
                      x="", 
                      title="No-contact Actions")
gNC.C = gNC.C  + theme(legend.title = element_blank(), 
                       axis.text.x = element_text(size = 8, face=NULL, 
                                                  margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                                  unit = "mm")), 
                       axis.text.y = element_text(size = 8, face=NULL,
                                                  margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                                  unit = "mm")),
                       axis.title.x = element_text(size = 10, face=NULL,
                                                   margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                                   unit = "mm")),
                       axis.title.y = element_text(size = 10, face=NULL,
                                                   margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                                   unit = "mm")))
#gNC.C = gNC.C  + scale_color_manual(values = c("#EE2C2C"))
gNC.C  <-  gNC.C + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
gNC.C

##NC PLOT adults
gNC.A <- ggplot()
gNC.A <- gNC.A + geom_jitter(data = NCdata.adult , 
                             aes(x = z.prop.time.with.max, y = behavior), #, colour = child.adult), 
                             width = 0.025, height = 0.025, alpha = 0.3, colour="red4")
#add adults CIs
gNC.A <- gNC.A  + geom_ribbon(data=boot.predictedNC.adult,
                              aes(x = z.prop.time.with.max,
                                  ymin = lower.cl,
                                  ymax = upper.cl),
                              alpha = 0.1,
                              fill="red4")
gNC.A <- gNC.A  + geom_line(data=boot.predictedNC.adult, 
                            aes(x=z.prop.time.with.max, y=fitted), colour="red4", lwd=1)

gNC.A <- gNC.A  +  scale_x_continuous(breaks = percentage.ticks, 
                                      labels =  percentage.labels)
gNC.A <- gNC.A  + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
gNC.A <- gNC.A  + labs(subtitle="Adults", 
                       y="", 
                       x="", 
                       title="")
gNC.A <- gNC.A  + theme(legend.title = element_blank(), 
                        axis.text.x = element_text(size = 8, face=NULL, 
                                                   margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                                   unit = "mm")), 
                        axis.text.y = element_text(size = 8, face=NULL,
                                                   margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                                   unit = "mm")),
                        axis.title.x = element_text(size = 10, face=NULL,
                                                    margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                                    unit = "mm")),
                        axis.title.y = element_text(size = 10, face=NULL,
                                                    margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                                    unit = "mm")))
#gNC.A <- gNC.A   + scale_color_manual(values = c("red 4"))
gNC.A  <-  gNC.A + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
gNC.A


##PI PLOT children
gPI.C <- ggplot()
gPI.C <- gPI.C + geom_jitter(data = PIdata.child , 
                             aes(x = z.prop.time.with.max, y = behavior), #, colour = child.adult), 
                             width = 0.025, height = 0.025, colour="#25c117", alpha = 0.3)
#add child CIs
gPI.C = gPI.C  + geom_ribbon(data=boot.predictedPI.child,
                             aes(x = z.prop.time.with.max,
                                 ymin = lower.cl,
                                 ymax = upper.cl),
                             alpha = 0.1,
                             fill="#25c117")
gPI.C = gPI.C  + geom_line(data=boot.predictedPI.child, 
                           aes(x=z.prop.time.with.max, y=fitted), colour="#25c117", lwd=1)
gPI.C = gPI.C  +  scale_x_continuous(breaks = percentage.ticks, 
                                     labels =  percentage.labels)
gPI.C = gPI.C  + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
gPI.C = gPI.C  + labs(subtitle="Children", 
                      y="Probability to Over-imitate", 
                      x="Percentage of Face-Fixation Time", 
                      title="Pseudo-instrumental Actions")
gPI.C = gPI.C  + theme(legend.title = element_blank(), 
                       axis.text.x = element_text(size = 8, face=NULL, 
                                                  margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                                  unit = "mm")), 
                       axis.text.y = element_text(size = 8, face=NULL,
                                                  margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                                  unit = "mm")),
                       axis.title.x = element_text(size = 10, face=NULL,
                                                   margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                                   unit = "mm")),
                       axis.title.y = element_text(size = 10, face=NULL,
                                                   margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                                   unit = "mm")))
#gPI.C = gPI.C  + scale_color_manual(values = c("#25c117"))
gPI.C  <-  gPI.C + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
gPI.C

##PI PLOT adults
gPI.A <- ggplot()
gPI.A <- gPI.A + geom_jitter(data = PIdata.adult , 
                             aes(x = z.prop.time.with.max, y = behavior), #, colour = child.adult), 
                             width = 0.025, height = 0.025, alpha = 0.3, colour="#006f37")
#add adults CIs
gPI.A <- gPI.A  + geom_ribbon(data=boot.predictedPI.adult,
                              aes(x = z.prop.time.with.max,
                                  ymin = lower.cl,
                                  ymax = upper.cl),
                              alpha = 0.1,
                              fill="#006f37")
gPI.A <- gPI.A  + geom_line(data=boot.predictedPI.adult, 
                            aes(x=z.prop.time.with.max, y=fitted), colour="#006f37", lwd=1)
gPI.A <- gPI.A  +  scale_x_continuous(breaks = percentage.ticks, 
                                      labels =  percentage.labels)
gPI.A <- gPI.A  + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
gPI.A <- gPI.A  + labs(subtitle="Adults", 
                       y="", 
                       x="", 
                       title="")
gPI.A <- gPI.A  + theme(legend.title = element_blank(), 
                        axis.text.x = element_text(size = 8, face=NULL, 
                                                   margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                                   unit = "mm")), 
                        axis.text.y = element_text(size = 8, face=NULL,
                                                   margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                                   unit = "mm")),
                        axis.title.x = element_text(size = 10, face=NULL,
                                                    margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                                    unit = "mm")),
                        axis.title.y = element_text(size = 10, face=NULL,
                                                    margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                                    unit = "mm")))
#gPI.A <- gPI.A   + scale_color_manual(values = c("#006f37"))
gPI.A  <-  gPI.A + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
gPI.A

#grid.arrange(p1, p2, nrow = 1)
plot_grid(gNC.C, gNC.A, gPI.C, gPI.A, align = "hv", nrow = 2, ncol = 2) # rel_widths = c(10.00/24, 14.00/24))




#################################################################################
#################################################################################

####Plots
####
plotmodel1 = glmer(behavior ~ (z.prop.time.mean + action.PI.code + child.adult.code) + 
                     (1 + action.PI.code + z.prop.time.mean  || single.id) +
                     (1 + child.adult.code || dyad.id) + 
                     (1 + action.PI.code || box.id),
                   data = t.data,  control = contr, family = binomial) 

plotmodel2 = glmer(behavior ~ (z.prop.time.mean + action + child.adult.code) + 
                     (1 + action.PI.code + z.prop.time.mean  || single.id) +
                     (1 + child.adult.code || dyad.id) + 
                     (1 + action.PI.code || box.id),
                   data = t.data,  control = contr, family = binomial) 

plotmodel3 = glmer(behavior ~ (z.prop.time.mean + action.PI.code + child.adult) + 
                     (1 + action.PI.code + z.prop.time.mean  || single.id) +
                     (1 + child.adult.code || dyad.id) + 
                     (1 + action.PI.code || box.id),
                   data = t.data,  control = contr, family = binomial) 
##bootstraps for plotmodel
#boot.plot1=boot.glmm.pred(model.res=plotmodel1, excl.warnings=T, nboots=1000, para=T, n.cores=c("all-1"), resol=100, level=0.95, use=c("z.prop.time.mean"))
#boot.plot2=boot.glmm.pred(model.res=plotmodel2, excl.warnings=T, nboots=1000, para=T, n.cores=c("all-1"), resol=100, level=0.95, use=c("action"))
#boot.plot3=boot.glmm.pred(model.res=plotmodel3, excl.warnings=T, nboots=1000, para=T, n.cores=c("all-1"), resol=100, level=0.95, use=c("child.adult"))
#save.image("plots_behavior.new.RData")

#PLOT 1: interaction time*action
summary(plotmodel1)$coefficients[,1]
#save coefficent values, so we can use them in an equation 
# save the coefficient values so we can use them in the equations
b0 = summary(plotmodel1)$coefficients[1,1]# intercept
Xztime = summary(plotmodel1)$coefficients[2,1]
XactionPI = summary(plotmodel1)$coefficients[3,1]
Xadult  = summary(plotmodel1)$coefficients[4,1]

Xtime_range <- seq(from=min(t.data$z.prop.time.mean), to=max(t.data$z.prop.time.mean), length.out = 100)
range(Xtime_range)

response_logits <- b0 + 
  Xztime*Xtime_range + 
  XactionPI*0 + 
  Xadult*0

# PI_logits_adult <- b0 + 
#   Xztime*Xtime_range + 
#   XactionPI*1 + 
#   Xadult*1 +
#   XztimeXactionPI*Xtime_range + 
#   XztimeXadult*Xtime_range + # the reference group
#   XactionPIXadult*1 + 
#   XztimeXactionPIXadult*Xtime_range

# Compute the probibilities (this is what will actually get plotted):
response_probs <- plogis(response_logits)

plot.data <- data.frame(looking.time=time_probs, time.range=Xtime_range)
#plot.data <- gather(plot.data, key=group, value=prob)
str(plot.data)

# plot.data.NC.child <- subset(plot.data, plot.data$group == "NC_child")
# plot.data.PI.child <- subset(plot.data, plot.data$group == "PI_child")
# plot.data.NC.adult <- subset(plot.data, plot.data$group == "NC_adult")
# plot.data.PI.adult <- subset(plot.data, plot.data$group == "PI_adult")

# NC.data.child = subset(t.data, t.data$action == "NC" & t.data$child.adult == "child")
# PI.data.child = subset(t.data, t.data$action == "PI" & t.data$child.adult == "child")
# NC.data.adult = subset(t.data, t.data$action == "NC" & t.data$child.adult == "parent")
# PI.data.adult = subset(t.data, t.data$action == "PI" & t.data$child.adult == "parent")

##to get a feeling how it looks like
plot(t.data$prop.time.mean, t.data$behavior)

boot.res.plot1=as.data.frame(boot.plot1$ci.predicted)
boot.res.plot2=as.data.frame(boot.plot2$ci.predicted)
boot.res.plot3=as.data.frame(boot.plot3$ci.predicted)

boot.res.plot1$xvals <- 
  seq(from=min(t.data$z.prop.time.mean), to=max(t.data$z.prop.time.mean), length.out = 100)

##y axis labels
range(t.data$prop.time.mean)
percentage.labels= c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")
percentage.comma = seq(from = 0, to = 0.8, length.out = 9)
##and a vector in z-space at which they need to be displayed
percentage.ticks=
  (percentage.comma - mean(t.data$prop.time.mean))/
  sd(t.data$prop.time.mean)


dev.off()
library(ggplot2)
#time plot
theme_set( theme_gray())  # pre-set the bw theme.
g1 <- ggplot()
g1 <- g1 + geom_jitter(data = t.data , aes(x = z.prop.time.mean, y = behavior, colour = action), width = 0.025, height = 0.025, alpha = 0.4)
g1 = g1 + geom_ribbon(data=boot.res.plot1,
                      aes(x = z.prop.time.mean,
                          ymin = lower.cl,
                          ymax = upper.cl),
                      alpha = 0.5,
                      fill="grey50")
g1 = g1 + geom_line(data=boot.res.plot1, 
                    aes(x=z.prop.time.mean, y=fitted), colour="grey40", lwd=1)
g1 = g1 +  scale_x_continuous(breaks = percentage.ticks, 
                              labels =  percentage.labels)
g1 = g1 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
g1 = g1 + labs(subtitle="Main Effect of Face-Looking Time", 
               y="Probability to Over-imitate", 
               x="Percentage of Face-Fixation Time", 
               title=element_blank())
g1 = g1 + theme(legend.title = element_blank(), 
                axis.text.x = element_text(size = 8, face=NULL, 
                                           margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                           unit = "mm")), 
                axis.text.y = element_text(size = 8, face=NULL,
                                           margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                           unit = "mm")),
                axis.title.x = element_text(size = 10, face=NULL,
                                            margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                            unit = "mm")),
                axis.title.y = element_text(size = 10, face=NULL,
                                            margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                            unit = "mm")))
g1 = g1 + scale_color_manual(values = c("#EE2C2C", "#43CD80", "#2297E6"))
g1

#action plot
t.data$fitted = as.numeric((t.data$behavior))
str(t.data)
theme_set( theme_gray())  # pre-set the bw theme.
g2 <- ggplot(data = t.data , aes(x = action, y = fitted, colour = action))
g2 <- g2 + geom_jitter(width = 0.45, height = 0.025, alpha = 0.3)
g2 <- g2 + geom_errorbar(aes(x=c("NC"), 
                             ymin=c(boot.res.plot2$lower.cl[boot.res.plot2$action == "NC"]),
                             ymax=c(boot.res.plot2$upper.cl[boot.res.plot2$action == "NC"])),
                         width=.125, colour="red",
                         position=position_dodge(0.9))
g2 <- g2 + geom_errorbar(aes(x=c("PI"), 
                             ymin=c(boot.res.plot2$lower.cl[boot.res.plot2$action == "PI"]),
                             ymax=c(boot.res.plot2$upper.cl[boot.res.plot2$action == "PI"])),
                         width=.125, colour="#25c117",
                         position=position_dodge(0.9))
g2 <- g2 + geom_crossbar(data=boot.res.plot2,
                         aes(ymin= c(fitted),
                             ymax= c(fitted)), 
                         width=0.5, colour= c("red", "#25c117"))
g2 = g2 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
g2 = g2 + theme(legend.position = "none")
g2 = g2 +  labs(subtitle="Main Effect of Action Type", 
                y="Probability to Over-imitate",
                x="Action Type", 
                title=element_blank())
g2 = g2 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
g2 = g2 + theme(legend.title = element_blank(),
                axis.text.x = element_text(size = 8, face=NULL, 
                                           margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                           unit = "mm")), 
                axis.text.y = element_text(size = 8, face=NULL,
                                           margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                           unit = "mm")),
                axis.title.x = element_text(size = 10, face=NULL,
                                            margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                            unit = "mm")),
                axis.title.y = element_text(size = 10, face=NULL,
                                            margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                            unit = "mm")))
g2 = g2 + scale_color_manual(values = c("#EE2C2C", "#43CD80", "#2297E6"))
g2

#plot child.adult 
t.data$fitted = as.numeric((t.data$behavior))
str(t.data)
theme_set( theme_gray())  # pre-set the bw theme.
g3 <- ggplot(data = t.data , aes(x = child.adult, y = fitted, colour = action))
g3 <- g3 + geom_jitter(width = 0.40, height = 0.025, alpha = 0.3)
g3 <- g3 + geom_errorbar(aes(x=c("child"), 
                             ymin=c(boot.res.plot3$lower.cl[boot.res.plot3$child.adult == "child"]),
                             ymax=c(boot.res.plot3$upper.cl[boot.res.plot3$child.adult == "child"])),
                         width=.125, colour="grey50",
                         position=position_dodge(0.9))
g3 <- g3 + geom_errorbar(aes(x=c("parent"), 
                             ymin=c(boot.res.plot3$lower.cl[boot.res.plot3$child.adult == "parent"]),
                             ymax=c(boot.res.plot3$upper.cl[boot.res.plot3$child.adult == "parent"])),
                         width=.125, colour="grey50",
                         position=position_dodge(0.9))
g3 <- g3 + geom_crossbar(data=boot.res.plot3,
                         aes(ymin= c(fitted[boot.res.plot3$child.adult == "child"], 
                                     fitted[boot.res.plot3$child.adult == "parent"]),
                             ymax= c(fitted[boot.res.plot3$child.adult == "child"], 
                                     fitted[boot.res.plot3$child.adult == "parent"])), 
                         width=0.5, colour= c("grey50", "grey50"))
g3 = g3 +  labs(subtitle="Main Effect of Age Group", 
                y="Probability to Over-imitate",
                x="Age Group", 
                title=element_blank())
g3 = g3 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
g3 = g3 + theme(legend.title = element_blank(),
                axis.text.x = element_text(size = 8, face=NULL, 
                                           margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                           unit = "mm")), 
                axis.text.y = element_text(size = 8, face=NULL,
                                           margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                           unit = "mm")),
                axis.title.x = element_text(size = 10, face=NULL,
                                            margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                            unit = "mm")),
                axis.title.y = element_text(size = 10, face=NULL,
                                            margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                            unit = "mm")), 
                plot.title = element_text(hjust = 0.5, size = 10, face="bold", 
                                          margin = margin(t = 0, r = 0, b = 1.5, l = 0, 
                                                          unit = "mm")))
g3 = g3 + scale_color_manual(values = c("#EE2C2C", "#43CD80", "#2297E6"))
g3


plot_row1 <- plot_grid(g1, labels = c("A"), axis = "b", rel_widths = c(1))
plot_row2 <- plot_grid(g2, g3, align = "h", nrow = 1, ncol = 2, 
                       labels = c("B", "C"), axis = "b", rel_widths = c(1, 1.3))


# now add the title
title <- ggdraw() +
  draw_label(
    "Analysis 2b: Behavioral Analysis - Experimental Conditions",
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(
  title, plot_row1, plot_row2, align = "v",
  nrow = 3,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1, 1))

plogis(0.8)






# -------------------------------------------------------------------------




#boot.plotfull.NC=boot.glmm.pred(model.res=full.NC, excl.warnings=T, nboots=1000, 
#             para=T, n.cores=c("all-1"), resol=100, level=0.95, 
#             use=c("z.prop.time.mean", "child.adult"))
boot.plotfull.NC.child = subset(boot.plotfull.NC$ci.predicted, boot.plotfull.NC$ci.predicted$child.adult == "child")
boot.plotfull.NC.adult = subset(boot.plotfull.NC$ci.predicted, boot.plotfull.NC$ci.predicted$child.adult == "parent")

###Plot the non-significant interaction 
boot.plotfull.NC$xvals <- 
  seq(from=min(t.data.NC$z.prop.time.mean), to=max(t.data.NC$z.prop.time.mean), length.out = 100)

##y axis labels
range(t.data.NC$prop.time.mean)
percentage.labels= c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")
percentage.comma = seq(from = 0, to = 0.8, length.out = 9)
##and a vector in z-space at which they need to be displayed
percentage.ticks=
  (percentage.comma - mean(t.data$prop.time.mean))/
  sd(t.data$prop.time.mean)

##NC PLOT children
gNC.C <- ggplot()
gNC.C <- gNC.C + geom_jitter(data = NCdata.child , 
                             aes(x = z.prop.time.mean, y = behavior), #, colour = child.adult), 
                             width = 0.025, height = 0.025, colour="#EE2C2C", alpha = 0.3)
#add child CIs
gNC.C = gNC.C  + geom_ribbon(data=boot.plotfull.NC.child,
                             aes(x = z.prop.time.mean,
                                 ymin = lower.cl,
                                 ymax = upper.cl),
                             alpha = 0.1,
                             fill="#EE2C2C")
gNC.C = gNC.C  + geom_line(data=boot.plotfull.NC.child, 
                           aes(x=z.prop.time.mean, y=fitted), colour="#EE2C2C", lwd=1)
gNC.C = gNC.C  +  scale_x_continuous(breaks = percentage.ticks, 
                                     labels =  percentage.labels)
gNC.C = gNC.C  + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
gNC.C = gNC.C  + labs(title="No-contact actions", 
                      subtitle="Children", 
                      y="Probability to Over-Imitate", 
                      x="Percentage of Face-Fixation Time")
gNC.C = gNC.C  + theme(legend.title = element_blank(), 
                       axis.text.x = element_text(size = 8, face=NULL, 
                                                  margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                                  unit = "mm")), 
                       axis.text.y = element_text(size = 8, face=NULL,
                                                  margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                                  unit = "mm")),
                       axis.title.x = element_text(size = 10, face=NULL,
                                                   margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                                   unit = "mm")),
                       axis.title.y = element_text(size = 10, face=NULL,
                                                   margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                                   unit = "mm")))
#gNC.C = gNC.C  + scale_color_manual(values = c("#EE2C2C"))
gNC.C  <-  gNC.C + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
gNC.C

##NC PLOT adults
gNC.A <- ggplot()
gNC.A <- gNC.A + geom_jitter(data = NCdata.adult , 
                             aes(x = z.prop.time.mean, y = behavior), #, colour = child.adult), 
                             width = 0.025, height = 0.025, alpha = 0.3, colour="red4")
#add adults CIs
gNC.A <- gNC.A  + geom_ribbon(data= boot.plotfull.NC.adult,
                              aes(x = z.prop.time.mean,
                                  ymin = lower.cl,
                                  ymax = upper.cl),
                              alpha = 0.1,
                              fill="red4")
gNC.A <- gNC.A  + geom_line(data= boot.plotfull.NC.adult, 
                            aes(x=z.prop.time.mean, y=fitted), colour="red4", lwd=1)

gNC.A <- gNC.A  +  scale_x_continuous(breaks = percentage.ticks, 
                                      labels =  percentage.labels)
gNC.A <- gNC.A  + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
gNC.A <- gNC.A  + labs(subtitle="Adults", 
                       y="", 
                       x="", 
                       title="")
gNC.A <- gNC.A  + theme(legend.title = element_blank(), 
                        axis.text.x = element_text(size = 8, face=NULL, 
                                                   margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                                   unit = "mm")), 
                        axis.text.y = element_text(size = 8, face=NULL,
                                                   margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                                   unit = "mm")),
                        axis.title.x = element_text(size = 10, face=NULL,
                                                    margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                                    unit = "mm")),
                        axis.title.y = element_text(size = 10, face=NULL,
                                                    margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                                    unit = "mm")))
#gNC.A <- gNC.A   + scale_color_manual(values = c("red 4"))
gNC.A  <-  gNC.A + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
gNC.A

#plot_grid(gNC.C, gNC.A, gPI.C, gPI.A, align = "hv", nrow = 2, ncol = 2) # rel_widths = c(10.00/24, 14.00/24))
plot_grid(gNC.C, gNC.A, align = "h", nrow = NULL, ncol = ) # rel_widths = c(10.00/24, 14.00/24))

#NC bootstraps for child adult
#boot.plot1.red.NC = boot.glmm.pred(model.res=red.NC, excl.warnings=T, nboots=1000, 
# para=T, n.cores=c("all-1"), resol=100, level=0.95, 
# use=c("child.adult")) 

round(boot.plot1.red.NC$ci.predicted$lower.cl, 3)
NC.boot.predicted=as.data.frame(boot.plot1.red.NC$ci.predicted)

# #plot child.adult 
t.data.NC$fitted = as.numeric((t.data.NC$behavior))
str(t.data)
theme_set(theme(panel.background = element_rect(fill = "white", colour = "grey50")))  # pre-set the bw theme
Plot.age.NC <- ggplot(data = t.data.NC , aes(x = child.adult, y = fitted, colour = child.adult))
Plot.age.NC <- Plot.age.NC + geom_jitter(width = 0.35, height = 0.025, alpha = 0.3)
Plot.age.NC <- Plot.age.NC + geom_errorbar(aes(x=c("child"),
                                               ymin=c(NC.boot.predicted$lower.cl[NC.boot.predicted$child.adult == "child"]),
                                               ymax=c(NC.boot.predicted$upper.cl[NC.boot.predicted$child.adult == "child"])),
                                           width=.125, position=position_dodge(0.9))
Plot.age.NC <- Plot.age.NC + geom_errorbar(aes(x=c("parent"),
                                               ymin=c(NC.boot.predicted$lower.cl[NC.boot.predicted$child.adult == "parent"]),
                                               ymax=c(NC.boot.predicted$upper.cl[NC.boot.predicted$child.adult == "parent"])),
                                           width=.125, position=position_dodge(0.9))
Plot.age.NC <- Plot.age.NC + geom_crossbar(data=NC.boot.predicted,
                                           aes(ymin= c(fitted[child.adult == "child"],
                                                       fitted[child.adult == "parent"]),
                                               ymax= c(fitted[child.adult == "child"],
                                                       fitted[child.adult == "parent"])),
                                           width=0.5)
Plot.age.NC <- Plot.age.NC +  labs(subtitle="Main Effect of Age Group",
                                   y="Probability to Over-imitate",
                                   x="Age Group",
                                   title="No-Contact Actions")
Plot.age.NC <- Plot.age.NC + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
Plot.age.NC <- Plot.age.NC + theme(
  legend.position = "none", 
  legend.title = element_blank(),
  axis.text.x = element_text(size = 10, face="plain",
                             margin = margin(t = 1, r = 0, b = 1, l = 0,
                                             unit = "mm")),
  axis.text.y = element_text(size = 10, face="plain",
                             margin = margin(t = 0, r = 1, b = 0, l = 1,
                                             unit = "mm")),
  axis.title.x = element_text(size = 10, face="plain",
                              margin = margin(t = 1.5, r = 1, b = 0, l = 0,
                                              unit = "mm")),
  axis.title.y = element_text(size = 10, face= "plain",  angle = 90, 
                              margin = margin(t = 1.5, r = 0, b = 0, l = 0,
                                              unit = "mm")),
  #axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
  #axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"))
  plot.title = element_text(hjust = 0, size = 10, face="bold", 
                            margin = margin(t = 0, r = 0, b = 1.5, l = 0,
                                            unit = "mm")), 
  plot.subtitle = element_text(hjust = 0, size = 10, face="plain", 
                               margin = margin(t = 0, r = 0, b = 1.5, l = 0,
                                               unit = "mm")))
Plot.age.NC <- Plot.age.NC + scale_color_manual(values = c("#EE2C2C", "red4"))
Plot.age.NC


# -------------------------------------------------------------------------

#NC bootstraps for looking time
red.NC.plot2 = glmer(behavior ~ (z.prop.time.mean) + child.adult.code +
                       (1 + z.prop.time.mean || single.id) + 
                       (1 + child.adult.code || dyad.id),
                     data = t.data.NC, control = contr, family = binomial)

#boot.plot2.red.NC = boot.glmm.pred(model.res=red.NC.plot2, excl.warnings=T, nboots=1000, 
#para=T, n.cores=c("all-1"), resol=100, level=0.95, 
#use=c("z.prop.time.mean"))

NC.boot.predicted2=as.data.frame(boot.plot2.red.NC$ci.predicted)
tail(NC.boot.predicted2)
##time plot
theme_set(theme(panel.background = element_rect(fill = "white", colour = "grey50"))) 
# theme_set( theme_gray())  # pre-set the bw theme.
Plot.time.NC <- ggplot()
Plot.time.NC <- Plot.time.NC + geom_jitter(data = t.data.NC , 
                                           aes(x = z.prop.time.mean, y = behavior, colour = child.adult), width = 0.025, height = 0.025, alpha = 0.4)
Plot.time.NC <- Plot.time.NC + geom_ribbon(data=NC.boot.predicted2,
                                           aes(x = z.prop.time.mean,
                                               ymin = lower.cl,
                                               ymax = upper.cl),
                                           alpha = 0.5,
                                           fill="grey50")
Plot.time.NC <- Plot.time.NC + geom_line(data=NC.boot.predicted2, 
                                         aes(x=z.prop.time.mean, y=fitted), colour="grey40", lwd=1)
Plot.time.NC <- Plot.time.NC +  scale_x_continuous(breaks = percentage.ticks, 
                                                   labels =  percentage.labels)
Plot.time.NC <- Plot.time.NC + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
Plot.time.NC <- Plot.time.NC + labs(subtitle="Main Effect of Face-Fixation Time",
                                    y="Probability to Over-imitate",
                                    x="Percentage of Face-Fixation Time",
                                    title="No-Contact Actions")
Plot.time.NC <- Plot.time.NC + theme(legend.title = element_blank(),
                                     #legend.position = "none",
                                     axis.text.x = element_text(size = 10, face=NULL,
                                                                margin = margin(t = 1, r = 0, b = 1, l = 0,
                                                                                unit = "mm")),
                                     axis.text.y = element_text(size = 10, face=NULL,
                                                                margin = margin(t = 0, r = 1, b = 0, l = 1,
                                                                                unit = "mm")),
                                     axis.title.x = element_text(size = 10, face=NULL,
                                                                 margin = margin(t = 1.5, r = 1, b = 0, l = 0,
                                                                                 unit = "mm")),
                                     axis.title.y = element_text(size = 10, face=NULL, angle = 90, 
                                                                 margin = margin(t = 1.5, r = 0, b = 0, l = 0,
                                                                                 unit = "mm")), 
                                     plot.title = element_text(hjust = 0, size = 10, face="bold", 
                                                               margin = margin(t = 0, r = 0, b = 1.5, l = 0,
                                                                               unit = "mm")), 
                                     plot.subtitle = element_text(hjust = 0, size = 10, face="plain", 
                                                                  margin = margin(t = 0, r = 0, b = 1.5, l = 0,
                                                                                  unit = "mm")))
Plot.time.NC <- Plot.time.NC + scale_color_manual(values = c("#EE2C2C", "red4"))
Plot.time.NC 

plot_grid(Plot.age.NC, Plot.time.NC, align = "h", nrow = NULL, ncol = 2, rel_widths = c(10.00/24, 14.00/24))



###PI 
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))
full.PI = glmer(behavior ~ (z.prop.time.mean)*child.adult +
                  (1 + z.prop.time.mean  || single.id) + 
                  (1 + child.adult.code || dyad.id),
                data = t.data.PI, control = contr, family = binomial)
ranef.diagn.plot(full.PI)
tests=drop1p(model.res=full.PI, para=F, data=t.data.PI, contr=contr, n.cores=c("all-1", "all"), to.del=NULL)
round(tests$drop1.res, 3)
plot(effect("z.prop.time.mean*child.adult", full.PI))

#boot.plotfull.PI = boot.glmm.pred(model.res=full.PI, excl.warnings=T, nboots=1000, 
#                  para=T, n.cores=c("all-1"), resol=100, level=0.95, 
#                  use=c("z.prop.time.mean", "child.adult"))

boot.plotfull.PI.child = subset(boot.plotfull.PI$ci.predicted, boot.plotfull.PI$ci.predicted$child.adult == "child")
boot.plotfull.PI.adult = subset(boot.plotfull.PI$ci.predicted, boot.plotfull.PI$ci.predicted$child.adult == "parent")

###Plot the non-significant interaction 
boot.plotfull.PI$xvals <- 
  seq(from=min(t.data.PI$z.prop.time.mean), to=max(t.data.PI$z.prop.time.mean), length.out = 100)

##y axis labels
range(t.data.PI$prop.time.mean)
percentage.labels= c("0%", "10%", "20%", "30%", "40%")
percentage.comma = seq(from = 0, to = 0.4, length.out = 5)
##and a vector in z-space at which they need to be displayed
percentage.ticks=
  (percentage.comma - mean(t.data.PI$prop.time.mean))/
  sd(t.data.PI$prop.time.mean)

##PI PLOT children
gPI.C <- ggplot()
gPI.C <- gPI.C + geom_jitter(data = PIdata.child , 
                             aes(x = z.prop.time.mean, y = behavior), #, colour = child.adult), 
                             width = 0.025, height = 0.025, colour="#25c117", alpha = 0.3)
#add child CIs
gPI.C = gPI.C  + geom_ribbon(data=boot.plotfull.PI.child,
                             aes(x = z.prop.time.mean,
                                 ymin = lower.cl,
                                 ymax = upper.cl),
                             alpha = 0.1,
                             fill="#25c117")
gPI.C = gPI.C  + geom_line(data=boot.plotfull.PI.child, 
                           aes(x=z.prop.time.mean, y=fitted), colour="#25c117", lwd=1)
gPI.C = gPI.C  +  scale_x_continuous(breaks = percentage.ticks, 
                                     labels =  percentage.labels)
gPI.C = gPI.C  + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
gPI.C = gPI.C  + labs(title="Pseudo-instrumental Actions", subtitle="Children", 
                      y="Probability to Over-Imitate", x="Percentage of Face-Fixation Time")
gPI.C = gPI.C  + theme(legend.title = element_blank(), 
                       axis.text.x = element_text(size = 8, face=NULL, 
                                                  margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                                  unit = "mm")), 
                       axis.text.y = element_text(size = 8, face=NULL,
                                                  margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                                  unit = "mm")),
                       axis.title.x = element_text(size = 10, face=NULL,
                                                   margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                                   unit = "mm")),
                       axis.title.y = element_text(size = 10, face=NULL,
                                                   margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                                   unit = "mm")))
#gPI.C = gPI.C  + scale_color_manual(values = c("#EE2C2C"))
gPI.C  <-  gPI.C + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
gPI.C

##PI PLOT adults
gPI.A <- ggplot()
gPI.A <- gPI.A + geom_jitter(data = PIdata.adult , 
                             aes(x = z.prop.time.mean, y = behavior), #, colour = child.adult), 
                             width = 0.025, height = 0.025, alpha = 0.3, colour="#006f37")
#add adults CIs
gPI.A <- gPI.A  + geom_ribbon(data= boot.plotfull.PI.adult,
                              aes(x = z.prop.time.mean,
                                  ymin = lower.cl,
                                  ymax = upper.cl),
                              alpha = 0.1,
                              fill="#006f37")
gPI.A <- gPI.A  + geom_line(data= boot.plotfull.PI.adult, 
                            aes(x=z.prop.time.mean, y=fitted), colour="#006f37", lwd=1)

gPI.A <- gPI.A  +  scale_x_continuous(breaks = percentage.ticks, 
                                      labels =  percentage.labels)
gPI.A <- gPI.A  + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
gPI.A <- gPI.A  + labs(subtitle="Adults", 
                       y="", 
                       x="", 
                       title="")
gPI.A <- gPI.A  + theme(legend.title = element_blank(), 
                        axis.text.x = element_text(size = 8, face=NULL, 
                                                   margin = margin(t = 1, r = 0, b = 1, l = 0, 
                                                                   unit = "mm")), 
                        axis.text.y = element_text(size = 8, face=NULL,
                                                   margin = margin(t = 0, r = 1, b = 0, l = 1, 
                                                                   unit = "mm")),
                        axis.title.x = element_text(size = 10, face=NULL,
                                                    margin = margin(t = 1.5, r = 1, b = 0, l = 0, 
                                                                    unit = "mm")),
                        axis.title.y = element_text(size = 10, face=NULL,
                                                    margin = margin(t = 1.5, r = 0, b = 0, l = 0, 
                                                                    unit = "mm")))
#gPI.A <- gPI.A   + scale_color_manual(values = c("red 4"))
gPI.A  <-  gPI.A + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
gPI.A

#plot_grid(gPI.C, gPI.A, gPI.C, gPI.A, align = "hv", nrow = 2, ncol = 2) # rel_widths = c(10.00/24, 14.00/24))
plot_grid(gPI.C, gPI.A, align = "h", nrow = NULL, ncol = 2) # rel_widths = c(10.00/24, 14.00/24))


red.PI = glmer(behavior ~ (z.prop.time.mean) + child.adult +
                 (1 + z.prop.time.mean  || single.id) + 
                 (1 + child.adult.code || dyad.id),
               data = t.data.PI, control = contr, family = binomial)
ranef.diagn.plot(red.PI)
tests=drop1p(model.res=red.PI, para=F, data=t.data.PI, contr=contr, n.cores=c("all-1", "all"), to.del=NULL)
round(tests$drop1.res, 3)
plot(effect("z.prop.time.mean", red.PI))
plot(effect("child.adult", red.PI))

#PI bootstraps for child adult
#boot.plot1.red.PI = boot.glmm.pred(model.res=red.PI, excl.warnings=T, nboots=1000,
#para=T, n.cores=c("all-1"), resol=100, level=0.95,
#use=c("child.adult"))

PI.boot.predicted=as.data.frame(boot.plot1.red.PI$ci.predicted)

# #plot child.adult 
t.data.PI$fitted = as.numeric((t.data.PI$behavior))
str(t.data)
theme_set(theme(panel.background = element_rect(fill = "white", colour = "grey50")))  # pre-set the bw theme
Plot.age.PI <- ggplot(data = t.data.PI , aes(x = child.adult, y = fitted))
Plot.age.PI <- Plot.age.PI + geom_jitter(width = 0.35, height = 0.025, alpha = 0.3)
Plot.age.PI <- Plot.age.PI + geom_errorbar(aes(x="child", 
                                               ymin=PI.boot.predicted$lower.cl[PI.boot.predicted$child.adult == "child"],
                                               ymax=PI.boot.predicted$upper.cl[PI.boot.predicted$child.adult == "child"]),
                                           width=.125, colour="#25c117", position=position_dodge(0.9))
Plot.age.PI <- Plot.age.PI + geom_errorbar(aes(x=c("parent"),
                                               ymin=c(PI.boot.predicted$lower.cl[PI.boot.predicted$child.adult == "parent"]),
                                               ymax=c(PI.boot.predicted$upper.cl[PI.boot.predicted$child.adult == "parent"])),
                                           width=.125, colour="#006f37",  position=position_dodge(0.9))
Plot.age.PI <- Plot.age.PI + geom_crossbar(data=PI.boot.predicted,
                                           aes(ymin= c(fitted[child.adult == "child"],
                                                       fitted[child.adult == "parent"]),
                                               ymax= c(fitted[child.adult == "child"],
                                                       fitted[child.adult == "parent"])),
                                           width=0.5)
Plot.age.PI <- Plot.age.PI +  labs(subtitle="Main Effect of Age Group",
                                   y="Probability to Over-imitate",
                                   x="Age Group",
                                   title="No-Contact Actions")
Plot.age.PI <- Plot.age.PI + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
Plot.age.PI <- Plot.age.PI + theme(
  legend.position = "none", 
  legend.title = element_blank(),
  axis.text.x = element_text(size = 10, face="plain",
                             margin = margin(t = 1, r = 0, b = 1, l = 0,
                                             unit = "mm")),
  axis.text.y = element_text(size = 10, face="plain",
                             margin = margin(t = 0, r = 1, b = 0, l = 1,
                                             unit = "mm")),
  axis.title.x = element_text(size = 10, face="plain",
                              margin = margin(t = 1.5, r = 1, b = 0, l = 0,
                                              unit = "mm")),
  axis.title.y = element_text(size = 10, face= "plain",  angle = 90, 
                              margin = margin(t = 1.5, r = 0, b = 0, l = 0,
                                              unit = "mm")),
  #axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
  #axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"))
  plot.title = element_text(hjust = 0, size = 10, face="bold", 
                            margin = margin(t = 0, r = 0, b = 1.5, l = 0,
                                            unit = "mm")), 
  plot.subtitle = element_text(hjust = 0, size = 10, face="plain", 
                               margin = margin(t = 0, r = 0, b = 1.5, l = 0,
                                               unit = "mm")))
Plot.age.PI <- Plot.age.PI + scale_color_manual(values = c("#EE2C2C", "red4"))
Plot.age.PI

#bootstraps for looking time
red.PI.plot2 = glmer(behavior ~ (z.prop.time.mean) + child.adult.code +
                       (1 + z.prop.time.mean || single.id) + 
                       (1 + child.adult.code || dyad.id),
                     data = t.data.PI, control = contr, family = binomial)

boot.plot2.red.PI = boot.glmm.pred(model.res=red.PI.plot2, excl.warnings=T, nboots=1000,
                                   para=T, n.cores=c("all-1"), resol=100, level=0.95,
                                   use=c("z.prop.time.mean"))



