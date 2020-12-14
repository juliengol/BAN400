# Olympic Medals Shiny app
### Exam BAN400, cand: 10, 30, 75

## Introduction and purpose
This app is designed to visualize Olympic Medal winners interactively. The development of the app is part of the BAN400 course at the Norwegian School of Economics. The app consists of three tabs outlining a map with the most medal-winning countries, a timeline graph over the medal development for each country and a markdown report with graphs and detailed information about the dataset.

*Olympic Medals* is made interactive, to account for different inputs. The user is free to click around in the map or choose the countries and medal type to look at the timeline for the different countries. In the Markdown report, we have collected some of the information from the dataset, like which country and athletes that have won the most medals.  

<img src="https://raw.githubusercontent.com/juliengol/BAN400/main/medal_map1.JPG" width="300"> <img src="https://raw.githubusercontent.com/juliengol/BAN400/main/medal_timeline.JPG" width="300"> <img src="https://raw.githubusercontent.com/juliengol/BAN400/main/markdown_report.JPG" width="300">


## Method and installations 
*Olympic Medals* depends on different packages to be able to run. The app is made in *Shiny*, and the specifications are mainly from this library. Additionally, a Markdown report is included as one of the tabs in the app by using the Markdown library in R. The packages transform the data and makes the app.


`install.packages("shiny")
 install.packages("ggplot2")
 install.packages("tidyverse")
 install.packages("dplyr")
 install.packages("leaflet")
 install.packages("markdown")
 install.packages("knitr")`

## How to start and use the app
The dataset used in the app is a combination of medals from the Winter and Summer Olympics, in addition to information about countries and the geographical location of the countries. To make sure there are no duplicates, we clean the dataset before we start the necessary calculations and adjustments. 

One of the main frames used in the app is the data frame called Medal, consisting of year, country and number of medals. This data frame is further used to specify different aspects. 

The app consists of a user interface, *ui*, deciding how we want the app to look like, and a server, *server*, being the function of how to get the desired output. The server function consists of the three parameters input, output, and session. The app is opened by running `shiny::shinyApp(ui = ui, server = server)`


In order to make the app and markdown tab work, you need to download the Markdown.Rmd file found in the zip-folder, and push the "knit" button in Rstudio *before* running the app. 




