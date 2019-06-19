## set some colours for our plot
grass_green<-"#538032"
white<-"#ffffff" 
black<-"#000000"

create_Pitch <- function(grass_colour=white, line_colour=black, background_colour=white, 
                         goal_colour=black, BasicFeatures){
  
  theme_blankPitch = function(size=12) { 
    theme(
      #axis.line=element_blank(), 
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      #axis.ticks.margin=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      #       panel.border=element_blank(), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ymin <- 4 # minimum width
  ymax <- 76 # maximum width
  xmin <- 3.5 # minimum length
  xmax <- 116.5 # maximum length
  
  # Defining features along the length
  boxEdgeDef <- 18
  boxEdgeOff <- 102
  halfwayline <- 60
  sixYardDef <- 6
  sixYardOff <- 114
  penSpotDef <- 12
  penSpotOff <- 108
  
  # Defining features along the width
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40   
  
  # other dimensions
  centreCirle_d <- 20    
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### create center circle ####
  center_circle <- circleFun(c(halfwayline,CentreSpot),centreCirle_d,npoints = 100)
  
  
  
  if(BasicFeatures == FALSE){
    ## initiate the plot, set some boundries to the plot
    p <- ggplot() + xlim(c(xmin-5,xmax+5)) + ylim(c(ymin-5,ymax+5)) +
      # add the theme 
      theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) +
      # add the 18 yard box defensive
      geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) + 
      # add the 18 yard box offensive
      geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
      # add halway line
      geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour) + 
      # add the goal Defensive
      geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_colour, size = 1) +
      # add the goal offensive
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1)
    
  }else{
    ## initiate the plot, set some boundries to the plot
    p <- ggplot() + xlim(c(xmin-5,xmax+5)) + ylim(c(ymin-5,ymax+5)) +
      # add the theme 
      theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) +
      # add the 18 yard box defensive
      geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) + 
      # add the 18 yard box offensive
      geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
      # add halway line
      geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour) +
      # add the six yard box Defensive
      geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour)  +
      # add the six yard box offensive
      geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour) +
      # add centre circle 
      geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour) +
      # add penalty spot left 
      geom_point(aes(x = penSpotDef , y = CentreSpot), colour = line_colour) + 
      # add penalty spot right
      geom_point(aes(x = penSpotOff , y = CentreSpot), colour = line_colour) + 
      # add centre spot 
      geom_point(aes(x = halfwayline , y = CentreSpot), colour = line_colour) + 
      # add the goal Defensive
      geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_colour, size = 1) +
      # add the goal offensive
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1) 
  }
  
  return(p)
  
}



