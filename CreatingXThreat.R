## Estimate XThreat on each of 96 regions
## Add the xT added of each event in the statsbomb data

library(dplyr)
library(ggplot2)
library(ggsoccer)
library(akima)
library(RColorBrewer)




statsbomb_events <- read.csv("Statsbombevents.csv", row.names = 1)


# Calculate shot probability, avg xG, movement probability, and matrix of transition probabilities
# for each location


# Shot probability, movement probability, turnover probability and avg Xg


ShotMoveTurnover_probs_xG <- statsbomb_events %>% group_by(location.bin)%>%
                        summarise(move_prob = length(which(!is.na(location.received.bin)))/n(),
                                  shot_prob = length(which(type.id==16))/n(),
                                  turnover_prob = length(which(is.na(location.received.bin) &
                                                                 type.id!=16))/n(),
                                  avg_XG = mean(shot.statsbomb_xg, na.rm = TRUE))
ShotMoveTurnover_probs_xG$avg_XG[is.nan(ShotMoveTurnover_probs_xG$avg_XG)] <- 0
#View(ShotMoveTurnover_probs_xG)


# Matrix of transition probabilities
# 96x96 matrix with element i,j being the probability of moving from bin i to bin j
# Note probability of moving into the same bin is nonzero if pass or dribble results
# in next event occurring within same 10x10 bin


statsbomb_events_moved <- statsbomb_events[!is.na(statsbomb_events$location.received.bin),]
move_transition_matrix <- with(statsbomb_events_moved, table(location.bin, location.received.bin))
move_transition_matrix <- move_transition_matrix/rowSums(move_transition_matrix)
#View(move_transition_matrix)



# Iterate to calculate xT starting with xT = 0

xT <- as.data.frame(cbind(ShotMoveTurnover_probs_xG$location.bin, rep(0,96)))
names(xT) <- c("location.bin","xT")

iterations = 100

for (i in 1:iterations){
  xT$xT <- ShotMoveTurnover_probs_xG$shot_prob*ShotMoveTurnover_probs_xG$avg_XG +
                    ShotMoveTurnover_probs_xG$move_prob*(move_transition_matrix%*%as.matrix(xT$xT))
}




## Heatmap of xThreat

source("soccerPitch.R")

# first get locations back to x and y

xT$location.y <- ifelse(xT$location.bin%%8==0, 75, xT$location.bin%%8*10 - 5)
xT$location.x <- ceiling(xT$location.bin/8)*10 - 5


fld <- with(xT, interp(x = location.x, y = location.y, z = xT))
gdat <- interp2xyz(fld, data.frame=TRUE)

names(gdat)[3] <- "xT"



color.gradient <- function(x, colors, colsteps=4) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

cols<-rev(c(brewer.pal(11, "Spectral")[1:7], "#67b27e", "#7ad095", "#C7E8C2",  "#FFFFFF"))

p <- create_Pitch(line_colour = "black", goal_colour = "black", BasicFeatures = TRUE)

xT_heatmap <- p + geom_tile(data=gdat, aes(x=x, y=y, fill=xT), alpha=0.7) +   
              scale_fill_gradientn(colours=cols, na.value="white")
#xT_heatmap
#ggsave(xT_heatmap, filename = "xTHeatMap.png")


## Add xTAdded to each event

calc_xT_added <- function(event) {
  
  if (!is.na(event$location.received.bin)){
    xT_added <- xT[xT$location.bin==event$location.received.bin,]$xT - 
      xT[xT$location.bin==event$location.bin,]$xT
  } else if (event$type.id == 16){
    xT_added <- xT[xT$location.bin==event$location.bin,]$xT
  } else if (event$type.id %in% c(3,38) | !is.na(event$pass.outcome.id) | event$dribble.outcome.name=="Incomplete"){
    xT_added <- -xT[xT$location.bin==event$location.bin,]$xT
  } else {
    xT_added <- 0
  }
  
  return(xT_added)
  
}



xT_added <- rep(NA, nrow(statsbomb_events))

pb <- txtProgressBar(min = 0, max = nrow(statsbomb_events), style = 3)

for (i in 1:nrow(statsbomb_events)){
  
  # update progress bar
  setTxtProgressBar(pb, i)
  
  event <- statsbomb_events[i,]
  
  xT_added[i] <- calc_xT_added(event)
}

statsbomb_events["xT_added"] <- xT_added
summary(statsbomb_events$xT_added)
write.csv(statsbomb_events, file="Statsbombevents_added.csv")



  




