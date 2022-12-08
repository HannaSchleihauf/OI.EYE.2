## Project Name: OI_EYE_2
## Analysis 2 - Prediction Plot -  Analysis of behavioral data -
## comparison of experimental groups

## We analyzed whether children and adults over-imitated certain action types more than others.

## Written by: Hanna Schleihauf
## Date: 2 December 2022




# empty plot
plot(
  x = 1, y = 1, type = "n", xlim = c(0, 7), ylim = c(-0.02, 1.02),
  tcl = -0.25, yaxt = "n",
  xaxs = "i", xaxt = "n",
  xlab = "", ylab = ""
)
axis(2, at = seq(from = 0, to = 1, length.out = 6), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"), las = 1, tick = TRUE, tck = -0.01)


rect(
  xleft = 0.60, ybottom = -0.02, xright = 2.4, ytop = 1.02,
  density = NA, angle = 45,
  col = grey(level = 0.25, alpha = 0.07), border = NULL, lty = par("lty"), lwd = par("lwd")
)
rect(
  xleft = 2.60, ybottom = -0.02, xright = 4.4, ytop = 1.02,
  density = NA, angle = 45,
  col = grey(level = 0.25, alpha = 0.07), border = NULL, lty = par("lty"), lwd = par("lwd")
)
rect(
  xleft = 4.60, ybottom = -0.02, xright = 6.4, ytop = 1.02,
  density = NA, angle = 45,
  col = grey(level = 0.25, alpha = 0.07), border = NULL, lty = par("lty"), lwd = par("lwd")
)


## add model line
# NC
rect(
  xleft = 0.8 - 0.12, ybottom = 0, xright = 1.2 + 0.12, ytop = 1,
  density = NA, angle = 45,
  col = adjustcolor("#EE2C2C", alpha.f = 0.25), border = NULL, lty = par("lty"), lwd = par("lwd")
)

pred.yvals <- 0.1
segments(
  x0 = 0.8, x1 = 1.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "#EE2C2C"
)

# NC
rect(
  xleft = 1.8 - 0.12, ybottom = 0, xright = 2.2 + 0.12, ytop = 1,
  density = NA, angle = 45,
  col = adjustcolor("#be2323", alpha.f = 0.25), border = NULL, lty = par("lty"), lwd = par("lwd")
)
pred.yvals <- 0.1
segments(
  x0 = 1.8, x1 = 2.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red4"
)

# PI
rect(
  xleft = 2.8 - 0.12, ybottom = 0, xright = 3.2 + 0.12, ytop = 1,
  density = NA, angle = 45,
  col = adjustcolor("#43CD80", alpha.f = 0.25), border = NULL, lty = par("lty"), lwd = par("lwd")
)
pred.yvals <- 0.8
segments(
  x0 = 2.8, x1 = 3.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "springgreen4"
)

# PI
rect(
  xleft = 3.8 - 0.12, ybottom = 0, xright = 4.2 + 0.12, ytop = 1,
  density = NA, angle = 45,
  col = adjustcolor("#199657", alpha.f = 0.25), border = NULL, lty = par("lty"), lwd = par("lwd")
)
pred.yvals <- 0.3
segments(
  x0 = 3.8, x1 = 4.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "#006f37"
)

# R
rect(
  xleft = 4.8 - 0.12, ybottom = 0, xright = 5.2 + 0.12, ytop = 1,
  density = NA, angle = 45,
  col = adjustcolor("#2297E6", alpha.f = 0.25), border = NULL, lty = par("lty"), lwd = par("lwd")
)
pred.yvals <- 0.9
segments(
  x0 = 4.8, x1 = 5.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue"
)

# R
rect(
  xleft = 5.8 - 0.12, ybottom = 0, xright = 6.2 + 0.12, ytop = 1,
  density = NA, angle = 45,
  col = adjustcolor("#1b78b8", alpha.f = 0.25), border = NULL, lty = par("lty"), lwd = par("lwd")
)
pred.yvals <- 0.9
segments(
  x0 = 5.8, x1 = 6.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue4"
)


# add labels at x-axis
mtext(text = c("action combinations"), side = 1, line = 0.5, at = 4.5)

# add lables at y-axis
mtext(text = "looking preference", side = 2, line = 2.7, at = 0.5)
