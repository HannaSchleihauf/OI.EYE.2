## Project Name: OI_EYE_2
## Analysis 1 - Prediction Plot for the analysis of face-looking times - Hypotheses Set 1

## Do children look longer into the model's face when no-contact actions
## are demonstrated compared to when pseudo-instrumental actions or relevant actions
## are demonstrated? Our predictors are the factors  action type
## (pseudo-instrumental/no-contact/relevant; within-subjects)
## and age group (child/adult; between-subjects).

## Written by: Hanna Schleihauf
## Date: 17 January 2022

# empty plot
dev.off()
plot(
  x = 1, y = 1, type = "n", xlim = c(0.5, 3.5), ylim = c(-0.1, 1.1),
  tcl = -0.25,
  xaxs = "i", xaxt = "n",
  las = 1,
  xlab = "", ylab = ""
)

## add model line
# NC
rect(
  xleft = 0.8 - 0.10, ybottom = 0, xright = 1.2 + 0.10, ytop = 1,
  density = NA, angle = 45,
  col = adjustcolor("#EE2C2C", alpha.f = 0.25), border = NULL, lty = par("lty"), lwd = par("lwd")
)
pred.yvals <- 0.30
segments(
  x0 = 0.8, x1 = 1.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red4"
)

# PI
rect(
  xleft = 1.8 - 0.10, ybottom = 0, xright = 2.2 + 0.10, ytop = 1,
  density = NA, angle = 45,
  col = adjustcolor("#43CD80", alpha.f = 0.25), border = NULL, lty = par("lty"), lwd = par("lwd")
)
pred.yvals <- 0.10
segments(
  x0 = 1.8, x1 = 2.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "springgreen4"
)

# R
rect(
  xleft = 2.8 - 0.10, ybottom = 0, xright = 3.2 + 0.10, ytop = 1,
  density = NA, angle = 45,
  col = adjustcolor("#2297E6", alpha.f = 0.25), border = NULL, lty = par("lty"), lwd = par("lwd")
)
pred.yvals <- 0.10
segments(
  x0 = 2.8, x1 = 3.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue"
)


# add labels at x-axis
mtext(text = c("action type"), side = 1, line = 0.5, at = 2.5)

# add lables at y-axis
mtext(text = "proportional fixation time on the model's face", side = 2, line = 2.7, at = 0.5)
