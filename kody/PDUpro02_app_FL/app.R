#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# to aplikacja, ktorej finalnie nie uzylem,
# ale przydala sie do nauki pakietu


library("dplyr")
library("ggplot2")
library("stringi")
library("shiny")

fillCancelledRatio <- function(df) {
  ifelse(df$Origin == "ATL", "yellow3", 
         ifelse(df$Origin == "DEN", "red3",
                ifelse(df$Origin == "IAH", "green3",
                       ifelse(df$Origin == "DFW", "blue3",
                              ifelse(df$Origin == "JFK", "pink3",
                                     ifelse(df$Origin == "LAS", "grey50",
                                            ifelse(df$Origin == "LAX", "purple3",
                                                   ifelse(df$Origin == "PHX", "violetred3",
                                                          ifelse(df$Origin == "SFO", "antiquewhite3",
                                                                 ifelse(df$Origin == "ORD", "orange3", 
                                                                        "white"))))))))))
}

fillMeanTime <- function(df) {
  ifelse(df$Origin == "ATL", "yellow1", 
         ifelse(df$Origin == "DEN", "red1",
                ifelse(df$Origin == "IAH", "green1",
                       ifelse(df$Origin == "DFW", "blue1",
                              ifelse(df$Origin == "JFK", "pink1",
                                     ifelse(df$Origin == "LAS", "grey70",
                                            ifelse(df$Origin == "LAX", "purple1",
                                                   ifelse(df$Origin == "PHX", "violetred2",
                                                          ifelse(df$Origin == "SFO", "antiquewhite2",
                                                                 ifelse(df$Origin == "ORD", "orange1", 
                                                                        "white"))))))))))
}





obrobDane <- function(data) {
  df <- data %>%
    select(Year, UniqueCarrier, FlightNum, TailNum, DepDelay, Origin, Cancelled, Diverted) %>%
    filter(Origin %in% biggestAirports) %>% 
    filter(DepDelay >= 0) %>% 
    group_by(Origin) %>%
    summarize(meanTime = mean(DepDelay, na.rm = TRUE), 
              flightsNumber = n(), 
              medianTime = median(DepDelay, na.rm = TRUE), 
              maxTime = max(DepDelay, na.rm = TRUE), 
              minTime = min(DepDelay, na.rm = TRUE)) %>%
    left_join(data %>% 
                select(Origin, Cancelled) %>%
                filter(Origin %in% biggestAirports) %>% 
                group_by(Origin) %>% 
                summarize(cancelledRatio = mean(Cancelled) * 100),
              by = join_by(Origin)) %>% 
    mutate(Score = meanTime * delayWeight + cancelledRatio * cancelledWeight) %>% 
    left_join(airports %>% 
                select(iata, airport, city, state, country),
              by = join_by(Origin == iata)) %>% 
    arrange(desc(Score)) %>% 
    mutate(position = letters[1:10])
  df
}

stworzWykres <- function(df, year) {
  ggplot(df, aes(x = position)) +
    geom_bar(aes(y = -meanTime),
             stat = "identity",
             position = "stack",
             width = 0.8,
             fill = fillMeanTime(df),
             color = "black") +
    geom_bar(aes(y = 3 * cancelledRatio),
             stat = "identity",
             position = "stack",
             width = 0.8, 
             fill = fillCancelledRatio(df),
             color = "black") +
    geom_text(aes(y = -28 , label = Origin),
              hjust = 2,
              vjust = 0.3,
              size = 6,
              fontface = "bold",
              family = "mono") +
    geom_text(aes(x = 11.3, y = 0, label = "")) +
    coord_flip() +
    theme_minimal() +
    labs(x = "", y = paste("Średni czas opóźnienia (min)", "% opóźnionych lotów", sep = "                         ")) +
    scale_y_continuous(
      breaks =  c(-seq(27, 0, -3), seq(0, 21, 3)),
      labels = c(seq(27, 0, -3), 0:7),
      limits = c(-33, 22)) +
    ggtitle(paste("Rok", year)) +
    theme(axis.text.y = element_blank(),
          plot.margin = margin(20, 20, 20, 20),
          panel.spacing = margin(t = 20),
          axis.title.x = element_text(size = 10,
                                      vjust = 0.1,
                                      hjust = 0.6,
                                      family = "mono"),
          plot.title = element_text(size = 16,
                                    hjust = 0.44,
                                    vjust = 0.5, 
                                    face = "bold",
                                    family = "")) +
    geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0),
                 arrow = arrow(length = unit(0.4, "cm")),
                 linewidth = 1.3,
                 lineend = "square")
}



# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Które lotnisko w danym roku cechowało się najlepszym odlotem"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            selectInput("rok",
                        "Wybierz rok",choices=c("1987", "1988","1989","1990","1991","1992","1993","1994",
                                                      "1995","1996","1997","1998","1999","2000","2001","2002","2003"
                                                      ,"2004","2005","2006","2007", "2008"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myPlot"),
           textOutput("bestAirport")
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$myPlot <- renderPlot({
      # wczytanie danych i obrobka w poczatkowych stadiach projektu
      # data <- read.csv(file.path("..", "..", "dane", "lata", paste(input$rok, ".csv", sep="")))
      # data$DepDelay <- ifelse(data$DepDelay >= 0, data$DepDelay, ifelse(data$DepDelay >= -7, 0, data$DepDelay))
      
      # obrobka danych
      # df <- obrobDane(data)
      
      #wczytanie danych od razu obrobionych
      df <- read.csv(file.path("..", "..", "dane", "najwazniejszeDane.csv")) %>% 
        filter(Year == input$rok)
      
      
      #tworzenie wykresu
      year <- input$rok
      stworzWykres(df, year)
    })
    
  output$bestAirport <- renderText({"tekst"})
}

# Run the application 
shinyApp(ui = ui, server = server)



