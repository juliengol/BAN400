# load packages
library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(leaflet)
library(markdown)
library(knitr)

# Loading data sets 
winter<-read_csv("winter.csv") %>%              # Winter olympics
  rename(code   = "Country") %>%                # renaming the column "Country" to code, to merge with "info"
  mutate(season = "winter")

summer<-read_csv("summer.csv") %>%              # Summer olympics
  rename(code   = "Country") %>%                # renaming the column "Country" to code, to merge with "info"
  mutate(season = "summer")

info<- read_csv("dictionary.csv") %>%           # General information about countries. 
  na.omit() %>%                                 # using the column "code" to be
  rename(code = "Code")                         # able to merge the data frames.

# we want one data frame containing both summer and winter olympics
# Using bind_rows to combine the df summer and winter, 
# and inner_join to bind them by the column containing country code 
df.ol<- winter %>% 
  bind_rows(summer) %>% 
  inner_join(., info, by = "code") 

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
medal$total <-rowSums(medal[, c(6,7,8)])

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


#Husk å laste ned nyeste versjon av HTML og kall den Markdown.html for å slippe å endre koden

ui <- navbarPage("Olympic Medals", id="medals",
                 tabPanel("Home",
                          h1("Olympic Medals from 1796-2012"),
                          uiOutput("img", src="src", style="width: 2px", align="right")
                          ),
                 tabPanel("Olympic Medals Map", 
                          div(class="outer",
                              fluidPage(
                                leafletOutput("mymap",height = 1000)
                              )
                              )),
                 tabPanel("Medal Count Timeline",
                          fluidPage(
                   titlePanel(h2("Olympic medals timeline by country",align = "center")),
                   sidebarLayout(    
                     sidebarPanel(
                       selectInput(inputId = "dataset",
                                   label = "Choose a country:",
                                   choices = sort(medal$country)),
                       selectInput(inputId = "medaltype",
                                   label = "Chooce type of medal:",
                                   choices = list("gold", "silver", "bronze", "total"))), 
                     mainPanel(
                       plotOutput("ts_plot")
                     )))
                 ),
                 tabPanel("Markdown report",
                   h2("Markdown report"),
                   (htmlOutput("inc"))),
                   hr()
                   )
                 


server <- function(input,output, session){
  output$img <- renderUI({
    tags$img(src = "https://i.insider.com/5a7b34d346a28825008b46c6?width=1100&format=jpeg&auto=webp")
  })
  
  
  output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      addCircles(data=df.map,
                 lat=~lat,
                 lng=~lng,
                 radius=~Nr_Medals*120, 
                 weight = 1, 
                 popup=paste("Country:", df.map$country, "<br>", "Total medals:", df.map$Nr_Medals, "<br>", "Gold Medals:", df.map$Gold, "<br>", "Silver Medals:", df.map$Silver, "<br>", "Bronze Medals:", df.map$Bronze)) %>% 
      setView(lng = 53, lat = 9, zoom = 2) 
  })
  
    datasetInput <- reactive({
      medal %>% filter(country == input$dataset)
    })
    
    # plot time series
    output$ts_plot <- renderPlot({
      
      dataset <- datasetInput()
      ggplot(dataset, aes(x = year, y=get(input$medaltype))) + xlab("Year") + ylab("Number of Medals") + geom_line()
      
    })
    
      getPage <- function() {
        return(includeHTML("Markdown.html"))
      }
      output$inc <- renderUI({getPage()})
   
  }
  
shiny::shinyApp(ui = ui, server = server)  




