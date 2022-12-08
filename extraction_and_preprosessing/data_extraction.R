## Project Name: OI_EYE_2
## AOI extraction from pre-processed Tobii ProLab output

## Written by: Hanna Schleihauf
## Date: 29 March 2021

# load files which contain the name of the video files for which we want to extract looking times
# load files which contain the order in which the video files were presented for each counterbalancing order
file.names <- read.table("./data_raw/file.names.txt", sep = "\t", header = T)
file.orders <- read.table("./data_raw/file.orders.txt", sep = "\t", header = T)

# create a data frame and storage location for the result of the extraction
n.simus <- 17281
output <- data.frame(stimulus.name = rep(x = NA, times = n.simus), aoi.time = rep(x = NA, times = n.simus), time.demo = rep(x = NA, times = n.simus), time.fixations.screen = rep(x = NA, times = n.simus))
output.file.name <- "./data_processed/data_processed.txt"

# outer loop: prepare extracting the AOI looking time data
for (k in 1:nrow(file.names)) {
  file.name <- paste("./data_preprocessed/Tobii_Output_preprosessed/", file.names$file[k], ".txt", sep = "")
  box.name <- file.names$box[k]
  tracking <- read.table(file.name, sep = "\t", header = T, dec = ",")

  # delete rows in which accidentally a mouse or keyboard event was recorded
  tracking <- droplevels(tracking[!tracking$Event == "MouseEvent", ])
  tracking <- droplevels(tracking[!tracking$Event == "KeyboardEvent", ])

  # filling up empty rows so that loop isn't interrupted
  tracking$face[is.na(tracking$face)] <- 0
  tracking$hand.left[is.na(tracking$hand.left)] <- 0
  tracking$hand.right[is.na(tracking$hand.right)] <- 0
  tracking$box[is.na(tracking$box)] <- 0
  tracking$golden.marbel[is.na(tracking$golden.marbel)] <- 0
  tracking$tool[is.na(tracking$tool)] <- 0
  tracking$barrier[is.na(tracking$barrier)] <- 0
  tracking$box[is.na(tracking$box)] <- 0
  tracking$NC[is.na(tracking$NC)] <- 0
  tracking$PI[is.na(tracking$PI)] <- 0
  tracking$R[is.na(tracking$R)] <- 0
  tracking$full[is.na(tracking$full)] <- 0


  # inner loop: start extracting the looking time data for all AOIs
  for (j in 1:nrow(file.orders)) {
    for (i in 1:nrow(tracking)) {
      if ((tracking$Event.value[i] == eval(parse(text = paste("file.orders$", box.name, sep = "")))[j]) &
        (tracking$Event[i] == "VideoStimulusStart")) {
        a <- i
      }
    }

    for (i in 1:nrow(tracking)) {
      if ((tracking$Event.value[i] == eval(parse(text = paste("file.orders$", box.name, sep = "")))[j]) &
        (tracking$Event[i] == "VideoStimulusEnd")) {
        b <- i
      }
    }

    time.demo <- b - a

    time.fixations.screen <- 0
    for (i in a:b) {
      if (tracking$Eye.movement.type[i] == "Fixation") {
        time.fixations.screen <- time.fixations.screen + 1
      }
    }

    # face
    face.hit <- 0
    for (l in a:b) {
      if (tracking$face[l] == 1) {
        face.hit <- face.hit + 1
      }
    }
    output$aoi.time[min(which(is.na(output$aoi.time)))] <- face.hit
    output$time.demo[min(which(is.na(output$time.demo)))] <- time.demo
    output$time.fixations.screen[min(which(is.na(output$time.fixations.screen)))] <- time.fixations.screen
    output$stimulus.name[min(which(is.na(output$stimulus.name)))] <- box.name

    # hands
    hands.hit <- 0
    for (l in a:b) {
      if (tracking$hand.left[l] == 1 | tracking$hand.right[l] == 1) {
        hands.hit <- hands.hit + 1
      }
    }
    output$aoi.time[min(which(is.na(output$aoi.time)))] <- hands.hit
    output$time.demo[min(which(is.na(output$time.demo)))] <- time.demo
    output$time.fixations.screen[min(which(is.na(output$time.fixations.screen)))] <- time.fixations.screen
    output$stimulus.name[min(which(is.na(output$stimulus.name)))] <- box.name

    # marble
    marble.hit <- 0
    for (l in a:b) {
      if (tracking$golden.marbel[l] == 1) {
        marble.hit <- marble.hit + 1
      }
    }
    output$aoi.time[min(which(is.na(output$aoi.time)))] <- marble.hit
    output$time.demo[min(which(is.na(output$time.demo)))] <- time.demo
    output$time.fixations.screen[min(which(is.na(output$time.fixations.screen)))] <- time.fixations.screen
    output$stimulus.name[min(which(is.na(output$stimulus.name)))] <- box.name

    # box
    box.hit <- 0
    for (l in a:b) {
      if (tracking$box[l] == 1) {
        box.hit <- box.hit + 1
      }
    }
    output$aoi.time[min(which(is.na(output$aoi.time)))] <- box.hit
    output$time.demo[min(which(is.na(output$time.demo)))] <- time.demo
    output$time.fixations.screen[min(which(is.na(output$time.fixations.screen)))] <- time.fixations.screen
    output$stimulus.name[min(which(is.na(output$stimulus.name)))] <- box.name

    # tool
    tool.hit <- 0
    for (l in a:b) {
      if (tracking$tool[l] == 1) {
        tool.hit <- tool.hit + 1
      }
    }
    output$aoi.time[min(which(is.na(output$aoi.time)))] <- tool.hit
    output$time.demo[min(which(is.na(output$time.demo)))] <- time.demo
    output$time.fixations.screen[min(which(is.na(output$time.fixations.screen)))] <- time.fixations.screen
    output$stimulus.name[min(which(is.na(output$stimulus.name)))] <- box.name


    # box+barrier
    box.barrier.hit <- 0
    for (l in a:b) {
      if (tracking$box[l] == 1 | tracking$barrier[l] == 1) {
        box.barrier.hit <- box.barrier.hit + 1
      }
    }
    output$aoi.time[min(which(is.na(output$aoi.time)))] <- box.barrier.hit
    output$time.demo[min(which(is.na(output$time.demo)))] <- time.demo
    output$time.fixations.screen[min(which(is.na(output$time.fixations.screen)))] <- time.fixations.screen
    output$stimulus.name[min(which(is.na(output$stimulus.name)))] <- box.name

    # hands+tool
    hands.tool.hit <- 0
    for (l in a:b) {
      if (tracking$hand.left[l] == 1 | tracking$hand.right[l] == 1 | tracking$tool[l] == 1) {
        hands.tool.hit <- hands.tool.hit + 1
      }
    }
    output$aoi.time[min(which(is.na(output$aoi.time)))] <- hands.tool.hit
    output$time.demo[min(which(is.na(output$time.demo)))] <- time.demo
    output$time.fixations.screen[min(which(is.na(output$time.fixations.screen)))] <- time.fixations.screen
    output$stimulus.name[min(which(is.na(output$stimulus.name)))] <- box.name

    # hand+barrier+tool
    hands.barrier.tool.hit <- 0
    for (l in a:b) {
      if (tracking$hand.left[l] == 1 | tracking$hand.right[l] == 1 | tracking$tool[l] == 1 | tracking$barrier[l] == 1) {
        hands.barrier.tool.hit <- hands.barrier.tool.hit + 1
      }
    }
    output$aoi.time[min(which(is.na(output$aoi.time)))] <- hands.barrier.tool.hit
    output$time.demo[min(which(is.na(output$time.demo)))] <- time.demo
    output$time.fixations.screen[min(which(is.na(output$time.fixations.screen)))] <- time.fixations.screen
    output$stimulus.name[min(which(is.na(output$stimulus.name)))] <- box.name

    ## hand+box+barrier+tool
    hands.box.barrier.tool.hit <- 0
    for (l in a:b) {
      if (tracking$hand.left[l] == 1 | tracking$hand.right[l] == 1 | tracking$tool[l] == 1 | tracking$box[l] == 1 | tracking$barrier[l] == 1) {
        hands.box.barrier.tool.hit <- hands.box.barrier.tool.hit + 1
      }
    }
    output$aoi.time[min(which(is.na(output$aoi.time)))] <- hands.box.barrier.tool.hit
    output$time.demo[min(which(is.na(output$time.demo)))] <- time.demo
    output$time.fixations.screen[min(which(is.na(output$time.fixations.screen)))] <- time.fixations.screen
    output$stimulus.name[min(which(is.na(output$stimulus.name)))] <- box.name
  }

  print(file.name)
}

write.table(output, file = output.file.name, sep = "\t", col.names = T, row.names = F, quote = F, dec = ".", na = "")
