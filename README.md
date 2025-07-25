# pitch-movement-dashboard

This shiny app uses YakkerTech pitch movement data from multiple games. 
The dashboard helps teams evaluate pitcher profiles, data by pitch 
type, and movement trends. 

## Features
**Pitch Movement Plot**
Plots horizontal and vertical break on an axis to visualize
how much movement a given pitcher has. This can be filtered by pitcher, pitch type, 
pitcher team, game, and opposing hitter. The points on the graph are colored by 
pitch type to analyze how each different pitch moves. 

**Movement Summary**
Shows a table of pitch metrics that are each color coded by 
percentile. The small data set causes some skewness here, but the more Yakkertech 
data we can get, the more accurate these colors will be. The metrics are 
organized by pitch type and give averages for each of the metrics over these
games. 

**Strike Zone Heatmap** 
Interactive heatmap of pitch locations (from pitcher POV).
This can be filtered by pitcher, opposing batter handedness, opposing batter, 
opposing team, etc. 

## Installation

1. Clone this repo: https://github.com/bellaholtz/pitch-movement-dashboard

2. Open project in R and download the necessary packages using the following: 
install.packages(c(
"shiny", "dplyr", "readr", "tidyr", "ggplot2",
"stringr", "reactable", "bslib", "shinythemes"))

3. run the following in the console 
shiny::runApp()

## Data

This app uses YakkerTech pitch data across multiple games, saved in .csv format
- Place your raw game files in a desktop folder named 'tennesseeYtdata' 
- The app then processes this folder and creates a cleaned summarized file, 
movement_sum.csv
- This will get data moved into the app and able to be processed in the app 

## Usage 

After launching the app: 
- Use the Movement Plot tab to see the pitch movements. Interact with the custom
filters to get relevant data
- Check the movement_summary tab to see color coded performance metrics by 
pitch type 
- The Pitch Location Heatmap tab identifies a pitch lovation pattern, filtered
by batter hand, team, and more!

## Credits 

This was built by Isabella Holtz as a part of the 2025 Summer Internship. Future
improvements include more filters or a way to compare pitchers side by side. 
