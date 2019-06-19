## Get all public event data from Statsbomb Github ##

## Create one dataframe for each competition:
## FA Women's Super League (matches 37.json)
## FIFA World Cup (matches 43.json)
## National Women's Soccer League (matches 49.json)

## For each competition combine event data from each game into a list
## Then combine list into one dataset

## For each game:
## Strip to just shots, passes, dribbles, dispossessed, miscontrols 
## This corresponds to type IDs 16 (shots), 30 (passes, and incomplete passes), 
## 14 (dribbles and incomplete dribbles), 3 (dispossessed), and 38 (miscontrol)
## Add start and end locations as individual columns for x and y, i.e. 4 columns total
## for dribbles, get location of the next event to see where this action moved to
## Also, add 12x8 location bins corresponding to 10x10 boxes to dataframe for each location start (and end)

library(jsonlite)

x.cut.points <- seq(0, 120, by=10)
y.cut.points <- seq(0, 80, by=10)

# Select set of columns:
# We want id, index, period, timestamp, possession, location, under_pressure, type.id, type.name,
# possession_team.id, possession_team.name, play_pattern.id, play_pattern.name, tead.id,
# team.name, player.id, player.name, position.id, position.name, pass.end_location, pass.recipient.id, 
# pass.recipient.name, pass.type.id, pass.type.name, pass.outcome.id, pass.outcome.name,
# ball_receipt.outcome.id, ball_receipt.outcome.name, shot.statsbomb_xg, dribble.outcome.id,
# dribble.outcome.name

columns_select <- c("index","period","timestamp","possession","location","under_pressure", "type.id",
                    "type.name", "possession_team.id", "possession_team.name", "play_pattern.id",
                    "play_pattern.name", "team.id", "team.name", "player.id", "player.name", "position.id",
                    "position.name", "pass.end_location", "pass.recipient.id", "pass.recipient.name",
                    "pass.type.id", "pass.type.name", "pass.outcome.id", "pass.outcome.name", 
                    "ball_receipt.outcome", "ball_receipt.outcome.name", "shot.statsbomb_xg",
                    "dribble.outcome.id", "dribble.outcome.name")


# Take dataframe of matches and create list
# Each element of returned list corresponds to dataframe of events corresponding to that match
create_list_events <- function(matches){
  data <- list()
  pb <- txtProgressBar(min = 0, max = nrow(matches), style = 3)
  
  for (j in 1:nrow(matches)){
    
    # update progress bar
    setTxtProgressBar(pb, j)
    
    match <- matches$match_id[j]
    
    # Get event data for each match from public Statsbomb repo
    json_file <- paste0("https://raw.githubusercontent.com/statsbomb/open-data/master/data/events/",
                        toString(match), ".json")
    json_data <- fromJSON(json_file, flatten = TRUE)
    
    # Strip to only shots, passes, dribbles, miscontrols, dispossessed
    json_events <- json_data[json_data$type.id %in% c(16, 30, 14, 3, 38),]
    
    
    # Select relevant columns and order alphabetically so all df's in list have same column
    # orders and can be row binded together
    json_events <- json_events[,(names(json_events) %in% columns_select)]
    json_events <- json_events[, order(names(json_events))]
    
    
    
    # Get location into two columns, x and y coordinates, then remove list of locations column
    xy_location <- do.call(rbind, json_events$location)
    json_events["location.X"] <- xy_location[,1]
    json_events["location.Y"] <- xy_location[,2]
    json_events$location <- NULL
    
    
    # Get location of where event ended. Need to take location of next event for dribbles.
    # For events that resulted in turnover (incomplete pass, incomplete dribble, dispossess, miscontrol)
    # Put NA for where the event ended up
    # Dribble outcome ID 8 = complete, outcome ID = 9 = incomplete
    # Pass outcome ID 9 = incomplete, 75 = out, 77 = unknown, 76 = offside
    
    json_events["location.received.X"] <- rep(NA, nrow(json_events))
    json_events["location.received.Y"] <- rep(NA, nrow(json_events))
    
    
    for (i in 1:nrow(json_events)){
      event <- json_events[i,]
      if (event$type.id == 30 & is.na(event$pass.outcome.id)){
        json_events[i,]$location.received.X <- event$pass.end_location[[1]][1]
        json_events[i,]$location.received.Y <- event$pass.end_location[[1]][2]
        next()
      } 
      
      if (event$type.id == 14 & event$dribble.outcome.id==8 & i<nrow(json_events)){
        json_events[i,]$location.received.X <- json_events[i+1,]$location.X
        json_events[i,]$location.received.Y <- json_events[i+1,]$location.Y
      }
    }
    
    json_events$pass.end_location <- NULL
    
    
    # Create 12x8 grid of location bins for each action, 2 columns, one for where action
    # started, and if applicable one for where action ended
    
    json_events['location.bin'] <- 8*as.numeric(cut(json_events$location.X, x.cut.points)) + 
      as.numeric(cut(json_events$location.Y, y.cut.points)) - 8
    
    json_events['location.received.bin'] <- 8*as.numeric(cut(json_events$location.received.X, x.cut.points)) + 
      as.numeric(cut(json_events$location.received.Y, y.cut.points)) - 8
    
    
    data[[j]] <- json_events
    
    
  }
  
  return(data)
  
}

# FA Womens Superleague
FAWSuperLeague_data <- create_list_events(fromJSON("Matches/37.json"))
df_events_FAWSuperleague <-as.data.frame(do.call("rbind",FAWSuperLeague_data))
#write.csv(df_events_FAWSuperleague, file = "events/FAWSuperLeagueEvents.csv")


# 2018 FIFA WorldCup

FIFAWorldCup_data <- create_list_events(fromJSON("Matches/43.json"))
df_events_FIFAWorldCup <-as.data.frame(do.call("rbind",FIFAWorldCup_data))
#write.csv(df_events_FIFAWorldCup, file = "events/FIFAWorldCupEvents.csv")


# National Womens Soccer League
NWSL_data <- create_list_events(fromJSON("Matches/49.json"))
df_events_NSWL <-as.data.frame(do.call("rbind",NWSL_data))
#write.csv(df_events_NSWL, file = "events/NWSLEvents.csv")


statsbomb_events <- rbind(df_events_FAWSuperleague, df_events_FIFAWorldCup, df_events_NSWL)
write.csv(statsbomb_events, file="Statsbombevents.csv")










