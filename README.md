# Olympic Medals Shiny app

## Introduction and purpose
This app is design to interactively visualize Olympic Medal winners. The development of the app is part of the BAN400 course at the Norwegian School of Economics. The app consists of three tabs outlining a map with the most medal winning countries, a time line graph over the medal development for each country and a markdown report with graphs and detailed information about the dataset.

With this app we want the user to get an interactive experience of the Olympic medal count.

BILDE AV APPEN

## Installation 
*Olympic Medals* depends on different packages to be able to run. The packages transform the data and makes the app.


`install.packages("shiny")
 install.packages("ggplot2")
 install.packages("tidyverse")
 install.packages("dplyr")
 install.packages("leaflet")
 install.packages("markdown")
 install.packages("knitr")`


## Methode
The app is made in *Shiny*, and the specifications are mainly from this library. Additionally we are including a Markdown report as one of the tabs in the app by using the Markdown library in R. 

## Input
*Olympic Medals* is made interactive, to account for different intputs. The user is free to click around in the map, or choose the countries and medal type to look at the time line for the different countries. This makes the input functions to an extent dynamic.

## Output 
The app is made flexible so that the choose variables are shown on the screen. The graph time line of the medal development in the different countries is one of the main features. As in the graph, the user is free to click around in the map, and the map will reveal different information about the country when clicked on. In the Markdown report we have collected some of the infomration from the dataset, like which country and athlet that have won the most medals. 

BILDE AV APPEN?

