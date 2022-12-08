## Project Name: OI_EYE_2
## Pre-processing raw Tobii ProLab output
## In the original output file each AOI for each stimuli has its own column.
## We will summarize the same AOIs ober all stimuli.

## Written by: Hanna Schleihauf
## Date: 29 March 2021

# load necessary packages
library(dplyr)
# make a function that can search names
search.name <- function(x, y) names(y[grep(x, names(y))])

# load files which contain the name of the video files for which we want to extract looking times
file.names <- read.table("./data_raw/file.names.txt", sep = "\t", header = T)

for (k in 1:nrow(file.names)) {
  file.name <- paste("./data_raw/Tobii_Output_with_filter/", file.names$file[k], ".txt", sep = "")
  # location for the output
  output.file.name <- paste("./data_preprocessed/Tobii_Output_preprosessed/", file.names$file[k], ".txt", sep = "")

  tracking <- read.table(file.name, sep = "\t", header = T, dec = ".")

  xx <- search.name("face", tracking)
  xx
  tracking$face <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$face[is.na(tracking$face)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$face)]
  }
  sum(tracking$face, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("hand.left", tracking)
  xx
  tracking$hand.left <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$hand.left[is.na(tracking$hand.left)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$hand.left)]
  }
  sum(tracking$hand.left, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("hand.right", tracking)
  xx
  tracking$hand.right <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$hand.right[is.na(tracking$hand.right)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$hand.right)]
  }
  sum(tracking$hand.right, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("box", tracking)
  xx
  tracking$box <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$box[is.na(tracking$box)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$box)]
  }
  sum(tracking$box, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("golden.marbel", tracking)
  xx
  tracking$golden.marbel <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$golden.marbel[is.na(tracking$golden.marbel)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$golden.marbel)]
  }
  sum(tracking$golden.marbel, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("tool", tracking)
  xx
  tracking$tool <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$tool[is.na(tracking$tool)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$tool)]
  }
  sum(tracking$tool, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("barrier", tracking)
  xx
  tracking$barrier <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$barrier[is.na(tracking$barrier)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$barrier)]
  }
  sum(tracking$barrier, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("\\<NC\\>", tracking)
  xx
  tracking$NC <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$NC[is.na(tracking$NC)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$NC)]
  }
  sum(tracking$NC, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("\\<PI\\>", tracking)
  xx
  tracking$PI <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$PI[is.na(tracking$PI)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$PI)]
  }
  sum(tracking$PI, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("\\<R\\>", tracking)
  xx
  tracking$R <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$R[is.na(tracking$R)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$R)]
  }
  sum(tracking$R, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  xx <- search.name("\\<full\\>", tracking)
  xx
  tracking$full <- eval(parse(text = paste("tracking$", xx[1])))
  for (j in 2:NROW(xx)) {
    tracking$full[is.na(tracking$full)] <- eval(parse(text = paste("tracking$", xx[j])))[is.na(tracking$full)]
  }
  sum(tracking$full, na.rm = TRUE)
  tracking <- select(tracking, -xx)

  print(file.name)
  write.table(tracking, file = output.file.name, sep = "\t", col.names = T, row.names = F, quote = F, dec = ".", na = "")
}
