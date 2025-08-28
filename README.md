
# Baseball Trackman Shiny App

## Overview 
This is a R Shiny app designed to help pitchers from high school to the professional level (and their coaches) analyzise their Trackman Data. 

A Demo version can be found at this link: https://nrch28.shinyapps.io/rd_pitching_app_public/ (Huge Thanks to Tucker Whitacre for letting me use his data).

## How to Use
First, select the pitcher using the "Select Player" dropdown. After selecting the pitcher, the dates when the pitcher has thrown and pitch types he throws will autopopulate. After selecting desired dates and pitch types, click "Update" to generate visualizations. 

*Note* The download buttons may cause the demo web app to crash, so see "Bullpen Example Report" and "Live AB Example Report"

The "Download Raw Data" button doownloads the raw trackman csv file that is currently being used to generate the visualizations. (Only downloads data with selected player, date, and pitch types). 
The "Download Bullpen Report" button downloads a pdf report with the most charts and tables focusing on pitch metrics. (No live AB data is required for this report).
The "Download Live Report" button downloads a pdf report that shows results data from a live ab session. (Live AB data is required for this report). 

## Features 
### Tab 1) Movement Plot and Metics Table

  Simple IVB and HB plot that can help get a base understanding of how a pitch moves
  
  You can compare two groups of pitches by lasso selecting a group, clicking the "toggle slection group" button, lasso selecting another group, and then clicking the "show comparison" button. This will show a pop up with averages from the two selected groups. 
  
  From this plot you can also select/deslect pitches from averages calculations in table 
  
  Below the plot is the metrics table that shows pitch type averages for velo, spin, IVB, HB, Extension, Release Height, Release Side, and Stuff+. 
  The stuff+ model I created is also available on my github
  
### Tab 2) Distributions

  Shows distribution of Stuff+, Velo, Spin, IVB, and HB by pitch type. This is helpful for getting a sense of a pitcher's consestency for each pitch type.  
    
### Tab 3) Location Heat Maps 

  This tab plots the pitch locations of each pitch type. While ther is no information on "indended zone", this is helpful to visualize pitcher's command of each  pitch, and where he tends to miss. 
  
### Tab 4) 3d Release Point Plot (w/ centroids)

  This plot shows the relase point of each pitch as well as centroids, which is the "average" release point for each pitch. This can be used to look at release point consistency (more on that with the tables).
  

  *Note* I wanted to include arm angle calculations here, but decided against it because of inacuracy. (See "Arm Angle Calculation Validation" on my github).
  
### Tab 5) Spin Clock

  Shows how much spin a pitcher generates at each 30 min spin axis bin. The length of the bar corresponds with the average spin rate in the 30 min bin. 

### Tab 6) Velo Endurance

  Shows how a pitcher's velo fluctuates over the course of an outing/bullpen. This tab is only designed to use data from one day, so the user must only select one date.

### Tab 7) Trends 

  Shows how a pitcher has progressed between sessions. This Tab includes charts for Velo, Spin, and Break. For each session there is a boxplot, actual daily average, and an exponentially weighted moving average.

### Tab 8) Live Data 

  If a live AB session is selected, this tab will show three different location charts, one for all pitches, one for swings, and one for balls in play. It will also show a spray chart for the balls in play as well as a table of basic stats from the outing. 

  
  
