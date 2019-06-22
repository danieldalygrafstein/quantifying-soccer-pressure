## Player eval stats ##

## Pass percentage and turnover percentages for each player, when under pressure and when not under pressure ##
## Calculate these as totals and as per event. Again do when under pressure and when not under pressure
## Maybe split the field in thirds, can get these turnover and pass percentages when facing pressure and when not facing pressure.

library(tidyverse)
library(reshape2)
source("soccerPitch.R")


statsbomb_events <- read.csv("Statsbombevents_xTadded.csv", row.names = 1)


## Where are pressures happening? Where passturnovers happening?

p <- create_Pitch(line_colour = "black", goal_colour = "black", BasicFeatures = TRUE)




pressure_events <- statsbomb_events %>% group_by(location.bin) %>% 
  summarise(percent_pressure=length(which(!is.na(under_pressure)))/n())

pressure_events$location.y <- ifelse(pressure_events$location.bin%%8==0, 75, pressure_events$location.bin%%8*10 - 5)
pressure_events$location.x <- ceiling(pressure_events$location.bin/8)*10 - 5



pressure_plot <- p+geom_count(data=pressure_events, aes(x=location.x, y=location.y, size=percent_pressure
                                                        , colour=percent_pressure)) +
  scale_size_area(max_size = 20, guide='none') + 
  scale_color_gradient(low="darkgreen", high="lightgreen")+
  labs(title = "Pressure Event Locations") +
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5))

#ggsave(pressure_plot, filename = "PressurePlot.png", width = 10, height = 6.5)


turnover_events <- statsbomb_events %>% group_by(location.bin) %>% 
  summarise(percent_turnover=length(which(type.id %in% c(3,38) | !is.na(pass.outcome.id) | 
                                            dribble.outcome.name=="Incomplete"))/n())

turnover_events$location.y <- ifelse(turnover_events$location.bin%%8==0, 75, turnover_events$location.bin%%8*10 - 5)
turnover_events$location.x <- ceiling(turnover_events$location.bin/8)*10 - 5

turnover_plot <- p+geom_count(data=turnover_events, aes(x=location.x, y=location.y, size=percent_turnover
                                                        , colour=percent_turnover)) +
  scale_size_area(max_size = 20, guide='none') + 
  scale_color_gradient(low="indianred4", high="indianred1")+
  labs(title = "Turnover Event Locations") +
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5))

#ggsave(turnover_plot, filename = "TurnoverPlot.png", width = 10, height = 6.5)



## Player preliminary summary statistics ##

## only keep players with > 300 events

statsbomb_events_trimmed <- statsbomb_events %>% group_by(player.id) %>% filter(n() > 300)


## percent of actions pressured

check_majority <- function(vector_positions){
  return(names(sort(table(vector_positions),decreasing=TRUE)[1]))
}

check_position <- function(position){
  if(str_detect(toString(position), "Back")) {return("Back")}
  else if (str_detect(toString(position), "Midfield")) {return("Midfield")}
  else {return("Forward")}
}



pressure_actions_pct <- statsbomb_events_trimmed %>% group_by(player.name) %>%
  summarise(pct_pressure = length(which(!is.na(under_pressure)))/n(),
            position_group = check_position(check_majority(position.name)))

pressure_actions_pct$pct_not_pressured <- 1-pressure_actions_pct$pct_pressure
pressure_actions_pct <- pressure_actions_pct[c(sample(which(pressure_actions_pct$position_group=="Back"),3),
                                               sample(which(pressure_actions_pct$position_group=="Midfield"),3),
                                               sample(which(pressure_actions_pct$position_group=="Forward"),3)),]


pressure_actions.m <- melt(pressure_actions_pct, id.vars = c("player.name", "position_group"))
pressure_actions.m %>% mutate_if(is.factor, as.character) -> pressure_actions.m
pressure_actions.m$player.name <- sapply(strsplit(pressure_actions.m$player.name, ' '), 
                                         function(x) x[2])
pressure_actions.m$player.name <- as.factor(pressure_actions.m$player.name)
pressure_actions.m$variable <- factor(pressure_actions.m$variable, levels = c("pct_pressure",
                                                                              "pct_not_pressured"),
                                      labels = c("pressured", "not pressured"))


pct_pressured <- ggplot(pressure_actions.m, aes(x = player.name, y = value, fill = variable, 
                                                label = paste0(round(value*100,2),"%"))) +
  geom_bar(stat = "identity") + facet_grid(~position_group, scales = "free") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "Percentage of Actions Pressured", x = "Player Name", y = "Percentage Pressured") +
  theme_bw() +
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        text=element_text(size=16)) +
  guides(fill=guide_legend(title=NULL)) 



#ggsave(pct_pressured, filename = "Plots/PctActionsPressured.png", width = 10, height = 6.5)


## Percent of pressure passes completed


pressure_passes_cmplt <- statsbomb_events_trimmed %>% filter(type.id==30 & !is.na(under_pressure)) %>%
  group_by(player.name) %>%
  summarise(pct_completed = length(which(is.na(pass.outcome.id)))/n(),
            position_group = check_position(check_majority(position.name)))

pressure_passes_cmplt$pct_not_completed <- 1-pressure_passes_cmplt$pct_completed


pressure_passes_cmplt <- pressure_passes_cmplt[c(sample(which(pressure_passes_cmplt$position_group=="Back"),3),
                                                 sample(which(pressure_passes_cmplt$position_group=="Midfield"),3),
                                                 sample(which(pressure_passes_cmplt$position_group=="Forward"),3)),]


pressure_passes_cmplt.m <- melt(pressure_passes_cmplt, id.vars = c("player.name", "position_group"))
pressure_passes_cmplt.m %>% mutate_if(is.factor, as.character) -> pressure_passes_cmplt.m
pressure_passes_cmplt.m$player.name <- sapply(strsplit(pressure_passes_cmplt.m$player.name, ' '), 
                                              function(x) x[length(x)])
pressure_passes_cmplt.m$player.name <- as.factor(pressure_passes_cmplt.m$player.name)

pressure_passes_cmplt.m$variable <- factor(pressure_passes_cmplt.m$variable, 
                                           levels=c("pct_not_completed","pct_completed"),
                                           labels = c("not completed","completed"))

passes_completed <- ggplot(pressure_passes_cmplt.m, 
                           aes(x = player.name, y = value, fill = variable, 
                               label = paste0(round(value*100,2),"%"))) +
  geom_bar(stat = "identity") + facet_grid(~position_group, scales = "free") +
  scale_fill_manual("legend", values = c("not completed" = "orange", "completed" = "green4")) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "Percentage of Pressured Passes Completed", x = "Player Name", y = "Percentage Completed") +
  theme_bw() +
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        text=element_text(size=16)) +
  guides(fill=guide_legend(title=NULL)) 
passes_completed

#ggsave(passes_completed, filename = "Plots/PctPressurePasses.png", width = 10, height = 6.5)



## Pct of turnovers when under pressure

turnovers_pct <- statsbomb_events_trimmed %>% filter(!is.na(under_pressure)) %>%
  group_by(player.name) %>%
  summarise(turnover_pct = length(which(type.id %in% c(3,38) 
                                        | !is.na(pass.outcome.id) 
                                        | dribble.outcome.name=="Incomplete"))/n(),
            position_group = check_position(check_majority(position.name)))




turnovers_pct <- turnovers_pct[c(sample(which(turnovers_pct$position_group=="Back"),3),
                                 sample(which(turnovers_pct$position_group=="Midfield"),3),
                                 sample(which(turnovers_pct$position_group=="Forward"),3)),]


turnovers_pct.m <- melt(turnovers_pct, id.vars = c("player.name", "position_group"))
turnovers_pct.m %>% mutate_if(is.factor, as.character) -> turnovers_pct.m
turnovers_pct.m$player.name <- sapply(strsplit(turnovers_pct.m$player.name, ' '), 
                                      function(x) x[length(x)])
turnovers_pct.m$player.name <- as.factor(turnovers_pct.m$player.name)

turnover_plot <- ggplot(turnovers_pct.m, 
                        aes(x = player.name, y = value,
                            label = paste0(round(value*100,2),"%"))) +
  geom_bar(stat = "identity", fill = "paleturquoise3") + facet_grid(~position_group, scales = "free") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "Percentage of Pressures Causing Turnovers", x = "Player Name", y = "Percentage Turnovers") +
  theme_bw() +
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        text=element_text(size=16)) +
  guides(fill=guide_legend(title=NULL)) 
turnover_plot


#ggsave(turnover_plot, filename = "Plots/TurnoverPressurePct.png", width = 9, height = 6.5)






## % dif in turnovers when pressure vs not pressure

turnovers_pct_no_pressure <- statsbomb_events_trimmed %>% filter(is.na(under_pressure)) %>%
  group_by(player.name) %>%
  summarise(no_pressure = length(which(type.id %in% c(3,38) 
                                       | !is.na(pass.outcome.id) 
                                       | dribble.outcome.name=="Incomplete"))/n(),
            position_group = check_position(check_majority(position.name)))

turnovers_pct <- statsbomb_events_trimmed %>% filter(!is.na(under_pressure)) %>%
  group_by(player.name) %>%
  summarise(pressure = length(which(type.id %in% c(3,38) 
                                    | !is.na(pass.outcome.id) 
                                    | dribble.outcome.name=="Incomplete"))/n(),
            position_group = check_position(check_majority(position.name)))

turnovers_pct_no_pressure$pressure <- turnovers_pct$pressure




turnovers_pct_no_pressure <- turnovers_pct_no_pressure[c(sample(which(turnovers_pct_no_pressure$position_group==
                                                                        "Back"),3),
                                                         sample(which(turnovers_pct_no_pressure$position_group=="Midfield"),3),
                                                         sample(which(turnovers_pct_no_pressure$position_group=="Forward"),3)),]


turnovers_pct_no_pressure.m <- melt(turnovers_pct_no_pressure, id.vars = c("player.name", "position_group"))
turnovers_pct_no_pressure.m %>% mutate_if(is.factor, as.character) -> turnovers_pct_no_pressure.m
turnovers_pct_no_pressure.m$player.name <- sapply(strsplit(turnovers_pct_no_pressure.m$player.name, ' '), 
                                                  function(x) x[length(x)])
turnovers_pct_no_pressure.m$player.name <- as.factor(turnovers_pct_no_pressure.m$player.name)



turnover_with_without_pressure <- ggplot(turnovers_pct_no_pressure.m, 
                                         aes(x = player.name, y = value, fill = variable, 
                                             label = paste0(round(value*100,2),"%"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + 
  facet_grid(~position_group, scales = "free") +
  scale_fill_manual("legend", values = c("pressure" = "#3182bd", "no_pressure" = "#9ecae1")) +
  geom_text(size = 3, position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "Turnover Percentage With and Without Pressure", x = "Player Name", y = "Turnover Percentage") +
  theme_bw() +
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        text=element_text(size=16)) +
  guides(fill=guide_legend(title=NULL)) 
turnover_with_without_pressure

#ggsave(turnover_with_without_pressure, filename = "Plots/TurnoversWithWithoutPressure.png", width = 10, height = 6.5)




## Turnovers Under pressure split on each third of the field

location_column<-which(names(statsbomb_events_trimmed)=="location.bin")

statsbomb_events_trimmed$location.third <- apply(statsbomb_events_trimmed, 1, function(x)  if (x[location_column]<25){"defending"}
                                                 else if (x[location_column]<73){"midfield"}
                                                 else {"attacking"} )

turnovers_pct_location <- statsbomb_events_trimmed %>% filter(!is.na(under_pressure)) %>%
  group_by(player.name, location.third) %>%
  summarise(turnover_pct = length(which(type.id %in% c(3,38) 
                                        | !is.na(pass.outcome.id) 
                                        | dribble.outcome.name=="Incomplete"))/n(),
            position_group = check_position(check_majority(position.name)))


turnovers_pct_location <- turnovers_pct_location[turnovers_pct_location$player.name %in% c("Abbey-Leigh Stringer",
                                                                                           "Abbie McManus",
                                                                                           "Alisha Lehmann"),]


turnovers_pct_location.m <- melt(turnovers_pct_location, id.vars = c("player.name", 
                                                                     "position_group","location.third"))

turnovers_pct_location.m$location.third <- as.factor(turnovers_pct_location.m$location.third)
turnovers_pct_location.m$location.third <- ordered(turnovers_pct_location.m$location.third, 
                                                   levels=c("defending","midfield","attacking"))

turnover_by_location <- ggplot(turnovers_pct_location.m, 
                               aes(x = player.name, y = value, fill = location.third, 
                                   label = paste0(round(value*100,2),"%"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6),  width = 0.6) + 
  scale_fill_manual("legend", values = c("defending" = "#fde0dd", "midfield" = "#fa9fb5", "attacking" = "#c51b8a")) +
  geom_text(size = 3, position = position_dodge(0.6), vjust = -0.5) +
  labs(title = "Turnover Percentage Under Pressure By Field Location", 
       x = "Player Name", y = "Turnover Percentage") +
  theme_bw() +
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        text=element_text(size=16)) +
  guides(fill=guide_legend(title="Field Location")) 
turnover_by_location


#ggsave(turnover_by_location, filename = "Plots/TurnoversByLocation.png", width = 10, height = 6.5)



