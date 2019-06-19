
## Player eval stats ##

## xTadded for each player on passes and cumulative xT added for each
## player. 
## Calculate these as totals and as per event. Again do when under pressure and when not under pressure
## Maybe split the field in thirds, xT added in each third when facing pressure and when not facing pressure.
## Rank players based on xT added under pressure, 1-10 based on percentiles


## Want cumulative xT added including turnovers+shots, just passes/dribbles, just pass/dribbles with no turnovers
## repeat with under pressure
## Then get all these statistics per event 
## 12 total metrics

check_majority <- function(vector_positions){
  return(names(sort(table(vector_positions),decreasing=TRUE)[1]))
}

check_position <- function(position){
  if(str_detect(toString(position), "Back")) {return("Back")}
  else if (str_detect(toString(position), "Midfield")) {return("Midfield")}
  else {return("Forward")}
}


# Cumulative xT added

library(tidyverse)


statsbomb_events <- read.csv("Statsbombevents_xTadded.csv", row.names = 1)


statsbomb_events <- statsbomb_events %>% group_by(player.id) %>% filter(n() > 300)

xT_cumulative <- statsbomb_events %>% group_by(player.id) %>%
                  summarise(
                            name = unique(player.name),
                            position = check_position(check_majority(position.name)),
                            team = check_majority(team.name),
                            total_xT = sum(xT_added)) %>% 
                  arrange(desc(total_xT))

statsbomb_passes_dribbles <- statsbomb_events %>% filter(type.id %in% c(30, 14))


xT_passes_dribbles <- statsbomb_passes_dribbles %>% group_by(player.id) %>%
                        summarise(
                        name = unique(player.name),
                        position = check_position(check_majority(position.name)),
                        team = check_majority(team.name),
                        total_xT = sum(xT_added)) %>% 
                      arrange(desc(total_xT))


xT_passes_dribbles_noturnovers <- statsbomb_passes_dribbles %>% filter(is.na(pass.outcome.id) | dribble.outcome.id==8) %>%
                                    group_by(player.id) %>%
                                    summarise(
                                    name = unique(player.name),
                                    position = check_position(check_majority(position.name)),
                                    team = check_majority(team.name),
                                    total_xT = sum(xT_added)) %>% 
                                    arrange(desc(total_xT))


total_xT_pressure <- statsbomb_events %>% group_by(player.id) %>% filter(!is.na(under_pressure)) %>%
                            summarise(
                              total_xT_pressure = sum(xT_added)) %>% 
                            arrange(desc(total_xT_pressure)) 


xT_cumulative <- xT_cumulative %>% inner_join(total_xT_pressure)
rm(total_xT_pressure)


xT_passes_dribbles_pressure <- statsbomb_passes_dribbles %>% group_by(player.id) %>% 
  filter(!is.na(under_pressure)) %>%
  summarise(
    total_xT_pressure = sum(xT_added)) %>% 
  arrange(desc(total_xT_pressure))

xT_passes_dribbles <- xT_passes_dribbles %>% inner_join(xT_passes_dribbles_pressure)
rm(xT_passes_dribbles_pressure)


xT_passes_dribbles_noturnovers_pressure <- statsbomb_passes_dribbles %>% filter(is.na(pass.outcome.id) | dribble.outcome.id==8) %>%
  filter(!is.na(under_pressure)) %>%
  group_by(player.id) %>%
  summarise(
    total_xT_pressure = sum(xT_added)) %>% 
  arrange(desc(total_xT_pressure))

xT_passes_dribbles_noturnovers <- xT_passes_dribbles_noturnovers %>%
                                    inner_join(xT_passes_dribbles_noturnovers_pressure)
rm(xT_passes_dribbles_noturnovers_pressure)


## Now repeat per event ##

xT_perevent <- statsbomb_events %>% group_by(player.id) %>%
  summarise(
    name = unique(player.name),
    position = check_position(check_majority(position.name)),
    team = check_majority(team.name),
    xT_per_event = sum(xT_added)/n()) %>% 
  arrange(desc(xT_per_event))


xT_passes_dribbles_perevent <- statsbomb_passes_dribbles %>% group_by(player.id) %>%
  summarise(
    name = unique(player.name),
    position = check_position(check_majority(position.name)),
    team = check_majority(team.name),
    xT_per_event = sum(xT_added)/n()) %>% 
  arrange(desc(xT_per_event))


xT_passes_dribbles_noturnovers_perevent <- statsbomb_passes_dribbles %>% filter(is.na(pass.outcome.id) | dribble.outcome.id==8)  %>%
  group_by(player.id) %>%
  summarise(
    name = unique(player.name),
    position = check_position(check_majority(position.name)),
    team = check_majority(team.name),
    xT_per_event = sum(xT_added)/n()) %>% 
  arrange(desc(xT_per_event))


xT_pressure_perevent <- statsbomb_events %>% group_by(player.id) %>% filter(!is.na(under_pressure)) %>%
  summarise(
    xT_pressure_perevent = sum(xT_added)/n()) %>% 
  arrange(desc(xT_pressure_perevent)) 


xT_perevent <- xT_perevent %>% inner_join(xT_pressure_perevent)
rm(xT_pressure_perevent)


xT_passes_dribbles_pressure_perevent <- statsbomb_passes_dribbles %>% group_by(player.id) %>% 
  filter(!is.na(under_pressure)) %>%
  summarise(
    xT_pressure_perevent = sum(xT_added)/n()) %>% 
  arrange(desc(xT_pressure_perevent))

xT_passes_dribbles_perevent <- xT_passes_dribbles_perevent %>% inner_join(xT_passes_dribbles_pressure_perevent)
rm(xT_passes_dribbles_pressure_perevent)


xT_passes_dribbles_noturnovers_pressure_perevent <- statsbomb_passes_dribbles %>% filter(is.na(pass.outcome.id) | dribble.outcome.id==8)  %>%
  filter(!is.na(under_pressure)) %>%
  group_by(player.id) %>%
  summarise(
    xT_pressure_perevent = sum(xT_added)/n()) %>% 
  arrange(desc(xT_pressure_perevent))

xT_passes_dribbles_noturnovers_perevent <- xT_passes_dribbles_noturnovers_perevent %>%
  inner_join(xT_passes_dribbles_noturnovers_pressure_perevent)
rm(xT_passes_dribbles_noturnovers_pressure_perevent)


quantile(xT_passes_dribbles_noturnovers_perevent$xT_pressure_perevent, seq(0,1,0.1))

xT_passes_dribbles_noturnovers_perevent[xT_passes_dribbles_noturnovers_perevent$team=="Arsenal WFC",]

quantile(rev(xT_perevent$xT_per_event - xT_perevent$xT_pressure_perevent), seq(0,1,0.1))

xT_perevent$resiliency <- xT_perevent$xT_per_event-xT_perevent$xT_pressure_perevent
xT_perevent[xT_perevent$team=="Arsenal WFC",]







