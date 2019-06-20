# Quantifying_Pressure
Project evaluating player skill under pressure using Statsbomb public event level data.

The main idea of this project is to use Expected Threat (xT), as introduced by Karun Singh [here](https://karun.in/blog/expected-threat.html), to quantify how soccer players react to defensive pressure. 

The **GatheringEventDataFromStatsbomb.R** script takes JSON match event files from the StatsBomb public repository and produces a csv with the events of interest (shots, passes, dribbles, turnovers). **CreatingXThreat.R** takes these events and calculates Expected Threat values for field locations and the xT-added for each event, found in **xT_locations.csv** and **Statsbombevents_xTadded.csv**, respectively. The **Descriptive_Plots.R** and **PlayerEval.R** scripts take the event and xT-added data and produce high level metrics and rankings for players.

See the **QuantifyingPressure.pdf** report for more detail. 
