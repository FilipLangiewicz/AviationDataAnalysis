#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# aplikacja prezentujaca zestaw danych
# dla wybranego z listy lotniska (po IATA)



library("shiny")
library("dplyr")
library("stringi")
library("ggplot2")
library("scales")


biggestAirports <- c("ATL", "DEN", "DFW", "IAH", "JFK", "LAS", "LAX", "ORD", "PHX", "SFO")


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Zestaw danych na temat funkcjonowania danego lotniska w latach 1988 - 2007"),
    
    
    
    # input    
    sidebarLayout(
        sidebarPanel(
            selectInput("lotnisko",
                          "Wybierz lotnisko",
                          choices = biggestAirports),
            textOutput("nazwa"),
            tags$head(tags$style("#nazwa{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 margin-top: 20px;
                                 }"
            )
            ),
            textOutput("miasto"),
            tags$head(tags$style("#miasto{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 margin-top: 10px;
                                 }"
            )
            ),
            textOutput("stan"),
            tags$head(tags$style("#stan{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 margin-top: 10px;
                                 }"
            )
            ),
            textOutput("kraj"),
            tags$head(tags$style("#kraj{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 margin-top: 10px;
                                 }"
            )
            ),
            textOutput("dlugosc"),
            tags$head(tags$style("#dlugosc{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 margin-top: 10px;
                                 }"
            )
            ),
            textOutput("szerokosc"),
            tags$head(tags$style("#szerokosc{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 margin-top: 10px;
                                 }"
            )
            ),
            imageOutput("obraz", height = "100%"),
            tags$head(tags$style("#obraz img {width: 100%;
                                margin-top: 20px;
                                border-radius: 6px;
                               }"
                              )
            ),
      ),
        
        
        mainPanel(
                 plotOutput("flightsNumberPlot", height = "230px"),
                 plotOutput("meanTime", height = "230px"),
                 plotOutput("cancelledRatio", height = "230px")
        )
    )
)



# Define server logic  
server <- function(input, output) {
    najwazniejszeDane <- read.csv(file.path("..", "..", "dane", "najwazniejszeDane.csv"))
    najwazniejszeDane$cancelledRatio <- najwazniejszeDane$cancelledRatio / 100 
    airports <- read.csv(file.path("..", "..", "dane", "airports.csv"))
    states <- read.csv((file.path("..", "..", "dane", "states.csv")), sep = ";")
    
     
    # generowanie wykresu z liczba lotow po latach
    output$flightsNumberPlot <- renderPlot({
      lotnisko <- input$lotnisko
      
      df <- najwazniejszeDane %>% 
        filter(Origin == lotnisko, Year != 1987, Year != 2008) %>% 
        select(Origin, flightsNumber, Year)
      
      ggplot(df, aes(x = Year, y = flightsNumber)) +
        geom_line(linewidth = 1.5,
                  color = "red1") +
        geom_point(size = 1.5,
                   color = "red3") +  
        labs(x = "", y = "") +
        scale_y_continuous(limits = c(0, NA),
                           labels = number_format(big.mark = "_")) +
        scale_x_continuous(breaks = seq(1987, 
                                        2008, 
                                        by = 2)) +
        ggtitle("Całkowita liczba lotów w danym roku") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14,
                                         family = "mono",
                                         face = "bold",
                                         colour = "black"),
              axis.text.y = element_text(size = 14,
                                        family = "mono",
                                        face = "bold",
                                        colour = "black"),
              plot.title = element_text(size = 20,
                                        colour = "black",
                                        hjust = 0.4,
                                        face = "bold.italic"),
              plot.background = element_rect(color = "red1",
                                             fill = NA),
              plot.margin = margin(t = 20))
    })
    
    #generowanie wykresu ze srednim opoznieniem w tych latach
    output$meanTime <- renderPlot({
      lotnisko <- input$lotnisko
      
      df <- najwazniejszeDane %>% 
        filter(Origin == lotnisko, Year != 1987, Year != 2008) %>% 
        select(Origin, meanTime, Year)
      
      ggplot(df, aes(x = Year, y = meanTime)) +
        geom_line(linewidth = 1.5,
                  color = "green1") +
        geom_point(size = 1.5,
                   color = "green3") +  
        labs(x = "", y = "") +
        scale_y_continuous(limits = c(0, NA),
                           labels = number_format(big.mark = "_")) +
        scale_x_continuous(breaks = seq(1987, 
                                        2008, 
                                        by = 2)) +
        ggtitle("Średni czas opóźnienia lotu w danym roku (w minutach)") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14,
                                         family = "mono",
                                         face = "bold",
                                         colour = "black"),
              axis.text.y = element_text(size = 14,
                                         family = "mono",
                                         face = "bold",
                                         colour = "black"),
              plot.title = element_text(size = 20,
                                        colour = "black",
                                        hjust = 0.4,
                                        face = "bold.italic"),
              plot.background = element_rect(color = "green1",
                                             fill = NA),
              plot.margin = margin(t = 20))
    })
    
    #generowanie wykresu ze wspolczynnikiem opoznien w tych latach
    output$cancelledRatio <- renderPlot({
      lotnisko <- input$lotnisko
      
      df <- najwazniejszeDane %>% 
        filter(Origin == lotnisko, Year != 1987, Year != 2008) %>% 
        select(Origin, cancelledRatio, Year)
      
      ggplot(df, aes(x = Year, y = cancelledRatio)) +
        geom_line(linewidth = 1.5,
                  color = "blue1") +
        geom_point(size = 1.5,
                   color = "blue3") +  
        labs(x = "", y = "") +
        scale_y_continuous(limits = c(0, NA),
                           labels = number_format(big.mark = "_")) +
        scale_x_continuous(breaks = seq(1987, 
                                        2008, 
                                        by = 2)) +
        ggtitle("Współczynnik odwołań lotów w danym roku (odwołane / wszystkie) ") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14,
                                         family = "mono",
                                         face = "bold",
                                         colour = "black"),
              axis.text.y = element_text(size = 14,
                                         family = "mono",
                                         face = "bold",
                                         colour = "black"),
              plot.title = element_text(size = 20,
                                        colour = "black",
                                        hjust = 0.4,
                                        face = "bold.italic"),
              plot.background = element_rect(color = "blue1",
                                          fill = NA),
              plot.margin = margin(t = 20))
    })
    
    
    output$nazwa <- renderText({
        lotnisko <- input$lotnisko
        nazwa <- znajdzDana("airport", lotnisko)
        paste("Wybrane lotnisko: ", nazwa, sep = "")
      })
    
    output$miasto <- renderText({
      lotnisko <- input$lotnisko
      miasto <- znajdzDana("city", lotnisko)
      paste("Miasto: ", miasto, sep = "")
    })
    
    output$stan <- renderText({
      lotnisko <- input$lotnisko
      nazwa <- as.character(znajdzDana("state", lotnisko))
      stan <- as.character(states %>% 
        filter(Code == nazwa) %>% 
        select(State))
      paste("Stan: ", stan," (", nazwa, ")", sep = "")
    })
    
    output$kraj <- renderText({
      lotnisko <- input$lotnisko
      nazwa <- znajdzDana("country", lotnisko)
      paste("Państwo: ", nazwa, sep = "")
    })
    
    output$dlugosc <- renderText({
      lotnisko <- input$lotnisko
      long <- znajdzDana("long", lotnisko)
      paste("Dlugość geograficzna: ", long, sep = "")
    })    
    
    output$szerokosc <- renderText({
      lotnisko <- input$lotnisko
      lat <- znajdzDana("lat", lotnisko)
      paste("Szerokość geograficzna: ", lat, sep = "")
    })
    
    znajdzDana <- function(kolumna, lotnisko) {
      airports %>% 
        filter(iata == lotnisko) %>% 
        select(all_of(kolumna))
    }
    
    output$obraz <- renderImage({
      list(src = file.path("..", "..", "zdjecia", paste(input$lotnisko,".jpg", sep = "")),  
           alt = "Lotnisko"
      )
    }, deleteFile = FALSE)
}


# Run the application 
shinyApp(ui = ui, server = server)


