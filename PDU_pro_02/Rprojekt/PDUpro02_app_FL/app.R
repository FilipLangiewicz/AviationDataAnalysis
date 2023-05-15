#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
  plot <- ggplot(df, aes(x = position)) +
    geom_bar(aes(y = -meanTime),
             stat = "identity",
             position = "stack",
             width = 0.8,
             fill = fillMeanTime(),
             color = "black") +
    geom_bar(aes(y = 3 * cancelledRatio),
             stat = "identity",
             position = "stack",
             width = 0.8, 
             fill = fillCancelledRatio(),
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
  plot
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Które lotnisko w danym roku cechowało się najlepszym odlotem"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("rok",
                        "Wybierz rok",choices=c("1987", "1988","1989","1990","1991","1992","1993","1994",
                                                      "1995","1996","1997","1998","1999","2000","2001","2002","2003"
                                                      ,"2004","2005","2006","2007", "2008"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$myPlot <- renderPlot({
      # wczytanie danych
      data <- read.csv(file.path("..", "..", "dane", "lata", file))
      data$DepDelay <- ifelse(data$DepDelay >= 0, data$DepDelay, ifelse(data$DepDelay >= -7, 0, data$DepDelay))
      
      # obrobka danych
      df <- obrobDane(data)
      
      #tworzenie wykresu
      year <- input$rok
      stworzWykres(df, year)
      
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
