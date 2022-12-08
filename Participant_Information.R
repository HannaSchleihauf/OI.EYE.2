library("readxl")
xdata <- read_excel("/Users/HannaSchleihauf/Dropbox/Research/Projects/OI/OI_EYE_2/Kopie von Masterlog_OI_EYE_2.xlsx", na= c("NA"))


xdata = xdata[,-(9:14)]

xxdata = subset(xdata, condition == "experimantal")
yydata = subset(xdata, condition == "baseline")

aggregate(xxdata$age, list(xxdata$child.adult), mean)
aggregate(xxdata$age, list(xxdata$child.adult), sd)

ftable(xxdata$child.adult ~ xxdata$sex)

aggregate(yydata$age, list(yydata$child.adult), mean)
aggregate(yydata$age, list(yydata$child.adult), sd)
ftable(yydata$child.adult ~ yydata$sex)
