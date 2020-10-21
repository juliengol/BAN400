####------ Exam BAN 420 ------####
# Date: 16.09.20
# Candidate numbers: 
# ------------------------------ #




# for this assignment, we want to explore datasets containing the olympic 
# medal winners throughout the years. To describe the data, we apply functions
# and put together different types of plots, animations and maps.  

# We have separated the assignment into seven tasks:

# 0: clearing the list and loading relevant packages
# 1: load and sort data sets
# 2: Data Exploration
# 3: Word cloud by country
# 4: Animation for medals over the the years for the most-winning country: US
# 5: Using maps to explore the magnitude of medals from the different countries
# 6: Multiple graphs
# 7: Bar animation for amount of medals per country over the years. GIF



#### 0: Clear list and loading relevant packages -------------------------------

# Clear list
rm(list=ls())

# load packages
library(tidyverse)
library(lubridate)
library(stargazer)
library(magrittr)
library(sp)
library(ggplot2)
library(rgdal)
library(sf)
library(lwgeom)
library(mapview)
library(gganimate)
library(stringr)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)



#### Task 1 - Load data sets ---------------------------------------------------



# Loading data sets 
winter<-read_csv("winter.csv") %>%              # Winter olympics
  rename(code   = "Country") %>%                
  mutate(season = "winter")
 


summer<-read_csv("summer.csv") %>%              # Summer olympics
  rename(code   = "Country") %>% 
  mutate(season = "summer")



info<- read_csv("dictionary.csv") %>%           # General information 
  na.omit() %>%                                 # using the column "code" to be
  rename(code = "Code")                         # able to merge the data frames.



# we want one data frame containing both summer and winter olympics:
df.ol<- winter %>% 
  bind_rows(summer) %>% 
  inner_join(., info, by = "code") 

# Using bind_rows to combine the df summer and winter, 
# and inner_join to bind them by the column containing country code  



# to avoid confusion, we apply lower case to all column names:
colnames(df.ol) <- tolower(colnames(df.ol))



#### Task 2 - Data exploration -------------------------------------------------



# Looking at some basic statistics of the data set:
summary(df.ol)
head(df.ol)
str(df.ol)



# Finding the country which participates in the most events. Using arrange()  
df.ol %>%
  group_by(country, sport) %>%
  summarise(events = n_distinct(event)) %>%      # counting the number of events
  arrange(desc(events)) %>%                      # arranging in descending order
  head()

# The US participates in the most events



# Total number of medals by type (gold, silver, bronze)
df.ol %>% count(medal)

# Bronze: 10055, Gold: 9929, Silver: 9973



# Which country has won the most medals for one single sport? 
df.ol %>%
  count(event, country) %>%
  group_by(event) %>%
  mutate(totals = sum(n),                       # Finding the number of medals 
         percent_Medals = n/sum(n)) %>%         # per country sorted by events
  arrange(desc(percent_Medals)) %>%             # counting the total number of 
  filter(n> 200)                                # medals and calculating the
                                                # percentage  

# The US has won the most medals for one single sport, with 42.6% of the medals
# in basketball



# Data frame separating winning medals by type and country:
# (creating a function to be able to separate by other factors as well) 
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
}                                             # !is.na --> sum all values that is
                                              # not equal to na.


# Then, using the function to create df                  
only.medals<-medals(df.ol, by_year = FALSE)   # sorting only by country
head(only.medals)           
                         

year.medals<-medals(df.ol, by_year = TRUE)    # sorting by year and country
head(year.medals)         
                          


# Plotting amount of sport events arranged each year:
df.ol%>%
  group_by(season, year) %>%
  summarise(Events = n_distinct(event)) %>%
  ggplot(mapping = aes(x = year, y = Events, color = season)) + 
  theme_bw() +
  geom_point() +                             # using both points and lines
  geom_line()                                # in the plot

### NOTE: 
# The graph displays that the summer olympics has a higher number of sport 
# events than the winter olympic. Furthermore, the number of events is 
# increasing for both olympic types over the years. 



# Olympic medals by gender for winter and summer combined
df.ol %>% 
  count(year, gender) %>%
  group_by(year) %>%
  mutate(Percent = n/sum(n)*100) %>%                 # sorting by percentage
  arrange(desc(Percent)) %>%
  ggplot(mapping = aes(x = year, y = Percent, color = gender)) + 
  geom_point() + 
  theme_minimal() +
  scale_x_continuous(minor_breaks = seq(min(df.ol$year) , max(df.ol$year), 4), 
                     breaks = seq(min(df.ol$year), max(df.ol$year), 4)) +
  labs(title = 'Olympic Medals by Gender', color = 'Legend', 
       subtitle = 'Percentage of Medals Won by Each Gender') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Olympic medals by gender and season
df.ol %>% 
  count(year, season, gender) %>%
  group_by(year, season) %>%
  mutate(Percent = n/sum(n)*100) %>%                # sorting by percentage
  arrange(desc(Percent)) %>%
  ggplot(mapping = aes(x = year, y = Percent, color = gender)) + 
  geom_point() + 
  facet_wrap(~season, ncol = 1) +
  theme_minimal() +
  scale_x_continuous(minor_breaks = seq(min(df.ol$year) , max(df.ol$year), 4), 
                     breaks = seq(min(df.ol$year), max(df.ol$year), 4)) +
  labs(title = 'Olympic Medals by Gender', color = 'Legend', 
       subtitle = 'Percentage of Medals Won by Each Gender') +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# for both plots we see that women are increasingly winning more medals over the
# years, which reflects the fact that the number of olympic events in which they
# can participate increases over time. In the end both genders win about the same
# percentage of the medals, which reflects that both genders participates in the
# same amount of sports. 



# Sports vs medals
df.ol[, c(1,3)] %>% 
  ggplot(aes(fct_infreq(sport), fill=sport)) + 
  geom_bar() + 
  xlab("sports") + 
  ylab("total Gold medal number") +
  coord_flip()

# Aquatics has the most medals



# Medals per contestant
indiv_medals <- df.ol %>%                   # creating a data frame for medals
  group_by(athlete,sport) %>%               # per contestants, and another where
  summarise(medals = n())                   # medals is sorted by descending order

indiv_medals <- data.frame(indiv_medals)
sort_indiv_medals <- indiv_medals[order(-indiv_medals$medals),]
head(sort_indiv_medals,20)



# Top individual atheltes (Gold medal winners)
athletes <- as.data.frame(df.ol$athlete)                    # Creating data frame
athletes_freq <-data.frame(ftable(athletes)) 
athletes_freq <- athletes_freq[order(-athletes_freq$Freq),] # Descending order
top_athletes <- athletes_freq[(1:15),]                      # Top 15 athletes

# data.frame(ftable(athletes)) counts the number of times the different names is mentioned



# creating the plot for the top athelets
top_athletes_plot<- 
  ggplot(data=top_athletes, aes(x = Freq, y = reorder(x, Freq), fill=Freq)) + 
  geom_bar(stat="identity") +                                                 
  geom_text(aes(label=Freq), hjust=-0.5, size=3.5) +
  xlab("Number of Gold Medal per Athlete ") + 
  ylab("Name of Athletes") +
  ggtitle("Top Individual Athletes Gold Medals Count")
top_athletes_plot

# Using reorder() to place the most winning athlete at the top 



#### Task 3 - Word cloud -------------------------------------------------------



df.cloud<- df.ol %>% 
  group_by(country) %>% 
  summarise(medals = n())

wordcloud(words          = df.cloud$country,        # choosing country as words
          freq           = df.cloud$medals,         # size based on number of medals
          colors         = brewer.pal(8,"Dark2"),
          random.order   = FALSE)                   # false = plotted in decreasing
                                                    # frequency 

#### NOTE
# The word cloud also confirms that the US has won the most medals



#### Task 4 - Looking at US: animate a graph based on years --------------------



# first preparing the data, creating a df containing only US observations
df.us <- df.ol %>% 
  filter(country == "United States") %>% 
  select(c(medal, year, season)) %>%
  group_by(year, medal) %>% 
  summarise(n_medal = n()) %>%                      
  complete(medal,year)



# creating a sorted list of unique years in the df (df.us)
#medal<- 
 # df.us %>%
  #pull(year) %>%                      # (X hvorfor "uniqe years", hva betyr det?X)
  #unique(.) %>%                       # var berre sånn i formelen eg fant,
  #sort(.)                             # men sjekka, og denne kan kjørest uten 



# finding the year with the most medals won to set a limit on the y axis.
most_medals <- max(df.us$n_medal, na.rm = TRUE)



# plotting the animation
df.us %>% 
  ggplot(aes(year,n_medal,color=medal,group=medal)) + 
  geom_line() +
  scale_y_continuous(limits = c(0, most_medals)) +
  theme_minimal() +
  ylab("Number of edals") + 
  xlab("Year") + 
  ggtitle("OL Medals", subtitle = "United States, US") + 
  transition_reveal(year)

# ggsave("US_gif.png", plot = last_plot()) ## If we want to save the plot. 



#### Task 5 - Creating a map with animation ------------------------------------



# Need to load the coordinates to cities/capitals in order to create the map. 
coord<- read_csv("concap.csv") %>% 
  select(CountryName, CapitalLatitude, CapitalLongitude) %>% 
  dplyr::rename(country = "CountryName",             # choosing country 
                lat     = "CapitalLatitude",         # and coordinates
                lng     = "CapitalLongitude")



# Preparing the data:
# Creating a df with only one observation for each each competing country. 
ol.map<- df.ol %>% 
  select(country) %>% 
  group_by(country) %>% 
  summarise(n_country = n()) %>% 
  select(country)



# joining the two df by "country", using the df based on medals.
df.map<- only.medals %>%                        
  right_join(., coord, by = "country") %>%  # using right_join to exclude the 
  na.omit(df.map)                           # not-medal-winning nations.



# Plotting the world map - using mapview
sf <- st_as_sf(df.map, coords=c("lng", "lat"))
plot(st_geometry(sf), axes=TRUE)
st_crs(sf) <- 4326
plot(st_geometry(sf), axes=TRUE)

mapview(sf, zcol = "Nr_Medals")                 # Plotting with scale

mapview(sf, cex = "Nr_Medals")                  # Plotting with circle size 

#### NOTE
# we can yet again see that the US is the most-winning nation.



#### Task 6 - Plotting multiple graphs -----------------------------------------



# in this section, we aim to create a function where we can plot number 
# of medals won based on country, as well as giving the opportunity to select 
# whether the sex of the winner, and the of medal the contestant won 
# (Gold, Silver, Bronze)
# we also want to save the plot as a pdf. 


plotsport<- function(type_sport,
                     name_country,
                     data, 
                     sex           = NA,        #(X hva vil dette si?  = NA X) -
                     type_medal    = NA, 
                     file_name     = NA, 
                     title         = "")
{
  
# Preparing the data, selecting and grouping by year, medal, sport, gender, country
  ol <- data %>%                                           # Creating a temporary df. 
    select(c(medal, year, sport, gender, country)) %>%
    group_by(year, medal, sport, gender, country) %>%
    summarise(n_medal = n()) %>% 
    ungroup() %>%
    complete(year) %>% 
    filter(sport %in% type_sport) %>% 
    filter(country %in% name_country)
  
  
  # if gender, filter by gender
  if(!is.na(sex)) {                # Using an if- function to seperate two outcomes, 
    ol<- ol %>%                    # If sex is defined, the plot will filter by gender. 
      filter(gender %in% sex)     
  } else {                         # if not, it will return to the temporary df "ol" 
    ol<- data
  }                                      
  
  
  # if gender, filter by medal
  if(!is.na(type_medal)) {           # Using an if- function to seperate two outcomes,
    ol<- ol %>%                      # If type medal is defined, the plot will filter by medal
      filter(medal %in% type_medal) 
  } else {                           # if not, it will return to the temporary df "ol"
    ol<- data
  }  
  
  ol <- ol %>% 
    ggplot(aes(x = year, y = n_medal)) +      # Using GGplot2
    geom_point() + 
    geom_line() + 
    xlab("Year") + 
    ylab("Number of medals") + 
    theme_minimal() + 
    ggtitle(title, subtitle = paste(sex, "from", name_country, "Winning", type_medal)) # Using paste() to connect 
                                                                                       # the characteristics of 
                                                                                       # the plot to the spesific plot. 
                                                                            # (X haha, klarte ikkje å finne
  # If file namen, save the file                                            # ein betre forklaring X)
  if(!is.na(file_name)) {
    ggsave(file_name)
  } else {
    ol
  }
  
  return(ol)
  
}



# creating a plot containing Norwegian women silver-medalists in skiing:
plotsport(type_sport   = "Skiing",
          name_country = "Norway",
          data         = df.ol, 
          sex          = "Women", 
          type_medal   = "Silver",
          file_name    = "skiing.pdf", 
          title        = "Skiing")


# Note: 
# Problem with the filtering of gender, and medal_type: 
# At first, the filtering was above the "preperation of data" section, however, when 
# the filtering was moved down between "preperation" and plotting, the problem was solved. 
# (X skal disse kommentarene med? X)



#### Task 7 - Creating a bar animation -----------------------------------------



# medal <- year.medals%>%             (X: kan dette slettes? X)
# select(year, country, Gold)         (X dette ogs?? X)


# first, creating and sorting a df for medals
medal <- year.medals %>%                     # choose to count gold medals and
  select(year, country, Gold) %>%            # sort by countries over time
  filter(year > 1954)%>%                     # choosing to count from 1954 and forwards
  group_by(country) %>%                      # (X: men betyr dette at den ikke teller medaljene
  arrange(year) %>%                          # f?r 1954? eller er de ogs? med? X)
  mutate(value = cumsum(Gold)) %>%           # sorting by ascending order (year)
  ungroup()                                  


medals_formatted <- medal %>%
  group_by(year) %>%
  mutate(rank = rank(-value)) %>%            # Descending value
  group_by(country) %>% 
  filter(rank <=10) %>%                      # only choosing top 10 countries
  ungroup()

# Animation
anim <- ggplot(medals_formatted, aes(rank, 
                                     group = country, 
                                     fill  = as.factor(country), 
                                     color = as.factor(country))) +
  geom_tile(aes(y      = value/2,
                height = value,
                width  = 0.9), 
                alpha  = 0.8, 
                color  = NA) +
  geom_text(aes(y = 0,    label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = value,label = value, hjust = 0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  labs(title    = 'Gold medals per year : {closest_state}',  
       subtitle = "Top 10 Countries",
       caption  = "Olympics medals") 

#GIF
animate(anim, 500, fps = 15,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"), end_pause = 5, start_pause =  5) 



