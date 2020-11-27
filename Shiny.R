# load packages
library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(leaflet)
library(markdown)
library(knitr)
library(hablar)

# Loading data sets 
winter<-read_csv("winter.csv") %>%              # Winter olympics
  rename(code   = "Country") %>%                # renaming the column "Country" to code, to merge with "info"
  mutate(season = "winter")

summer<-read_csv("summer.csv") %>%              # Summer olympics
  rename(code   = "Country") %>%                # renaming the column "Country" to code, to merge with "info"
  mutate(season = "summer")

info<- read_csv("dictionary.csv") %>%           # General information about countries. 
  na.omit() %>%                                 # using the column "code" to be able to merge the data frames.
  rename(code = "Code")                          

# Using bind_rows to combine the df summer and winter, 
# and inner_join to bind them by the column containing country code 
df.ol<- winter %>% 
  bind_rows(summer) %>% 
  inner_join(., info, by = "code") 


# Identifying duplicates in df.ol 
duplicates <- df.ol %>% 
  find_duplicates(year, athlete, event, medal, discipline)

#Removing duplicates
df.ol <- df.ol %>% distinct()


# to avoid confusion, we apply lower case to all column names:
colnames(df.ol) <- tolower(colnames(df.ol))

#Medal overview per year and country, cumsum of medal count
medal <-
  df.ol %>%
  mutate(Gold = medal == "Gold",    
         Silver = medal == "Silver",
         Bronze = medal == "Bronze") %>%
  group_by(year, country) %>% 
  dplyr::summarise(        
    Nr_Medals = sum(!is.na(medal)),    
    Gold = sum(Gold, na.rm = T),
    Silver = sum(Silver   , na.rm = T),
    Bronze = sum(Bronze    , na.rm = T),
    contestants = n()) %>% 
  na.omit() %>% 
  select(year, country, Gold, Silver, Bronze) %>%            # sort by countries over time
  group_by(country) %>%                      
  arrange(year) %>%                         
  mutate(gold = cumsum(Gold), silver = cumsum(Silver), bronze = cumsum(Bronze)) %>%           
  ungroup() 

#Adding total number of medals
medal$combined <-rowSums(medal[, c(6,7,8)])

#-------------------------------------------------------------------------------- !!!Denne kan kanksje endres? Vi trenger ikke funksjonen

# making a function to find medals per country
medals<- function(data, by_year = FALSE) {
  # preparing the data
  ol <- data %>%                               
    mutate(Gold   = medal == "Gold",           # splitting the medal column
           Silver = medal == "Silver",         # into separate columns for 
           Bronze = medal == "Bronze")         # gold, silver and bronze medals
  # Then filter                                
  if(isTRUE(by_year)) {                       
    ol<- ol %>%                                # grouping by year and country if  
      group_by(year, country)                    # TRUE is set in the argument,
  } else {                                     # group only by country if not
    ol<- ol %>% 
      group_by(country)
  }
  # dividing by medal type
  ol <- ol %>% 
    dplyr::summarise(                         
      Nr_Medals   = sum(!is.na(medal)),       # counting total amount of medals
      Gold        = sum(Gold,   na.rm = T),   # sorting by country and number of 
      Silver      = sum(Silver, na.rm = T),   # each type of medals, counting 
      Bronze      = sum(Bronze, na.rm = T),   # winning contestants.
      contestants = n()) %>%                  
    arrange(-Nr_Medals)                       # most winning countries at the top 
}                                             # !is.na --> sum all values that is not equal to na.

#-------------------------------------------------------------------------------

#Total number of medals per country                 
only.medals<-medals(df.ol, by_year = FALSE)   # sorting only by country

#Adjusting df with coordinates of capitals to be used in the map
coord<- read_csv("concap.csv") %>% 
  select(CountryName, CapitalLatitude, CapitalLongitude) %>% 
  dplyr::rename(country = "CountryName",             # choosing country 
                lat     = "CapitalLatitude",         # and coordinates
                lng     = "CapitalLongitude")


# Preparing the data:
# joining the two df by "country", using the df based on medals.
df.map<- only.medals %>%                        
  right_join(., coord, by = "country") %>%  # using right_join to exclude the 
  na.omit(df.map)                           # not-medal-winning nations.


#SHINY APP

#User interface
ui <- 
  # making a navbar page to get the tabs
  navbarPage("Olympic Medals", id="medals",
                 tabPanel("Olympic Medals Map",    # designing the tab for the map
                          div(class="outer",
                              fluidPage(
                                leafletOutput("mymap",height = 1000)
                              )
                              )),
                 tabPanel("Medal Count Timeline",  # designing the tab for the graph timeline
                          fluidPage(
                   titlePanel(h2("Olympic medals timeline by country",align = "center")),
                   sidebarLayout(    
                     sidebarPanel(
                       selectInput(inputId = "dataset",             # making the drop down menu with country
                                   label = "Choose a country:",
                                   choices = sort(medal$country)),
                       selectInput(inputId = "medaltype",           # making the drop down menu with medal type
                                   label = "Chooce type of medal:",
                                   choices = list("gold", "silver", "bronze", "combined"))), 
                     mainPanel(
                       plotOutput("ts_plot")
                     )))
                 ),
                 tabPanel("Markdown report",     # designing the tab for the markdown report
                   h2("Markdown report"),
                   (htmlOutput("inc"))),
                   hr()
                   )
                 

# server function
server <- function(input,output, session){
  # the map tab 
  output$mymap <- renderLeaflet({      
    # plotting the map
     m <- leaflet() %>%
      addTiles() %>%
      addCircles(data=df.map,
                 lat=~lat,
                 lng=~lng,
                 radius=~Nr_Medals*120,  # deciding that total medal count will be the radius of the sircles at the map
                 weight = 1, 
                 popup=paste("Country:", df.map$country, "<br>", "Total medals:", df.map$Nr_Medals, "<br>", "Gold Medals:", df.map$Gold, "<br>", "Silver Medals:", df.map$Silver, "<br>", "Bronze Medals:", df.map$Bronze)) %>% 
      setView(lng = 53, lat = 9, zoom = 2)   # setting the world as the view of the map 
  })
  
 # the graph tab
    datasetInput <- reactive({      # filtering the dataset to consist of the data for the chosen country
      medal %>% filter(country == input$dataset)
    })
    
    # plot time series
    output$ts_plot <- renderPlot({  
      
      dataset <- datasetInput()
      ggplot(dataset, aes(x = year, y=get(input$medaltype))) + xlab("Year") + ylab("Number of Medals") + geom_line()
      
    })
    
 # the markdown tab
    getPage <- function() {
      return(includeHTML("Markdown.html"))
    }
    output$inc <- renderUI({getPage()})
   
  }
  
shiny::shinyApp(ui = ui, server = server)    # starting the app


