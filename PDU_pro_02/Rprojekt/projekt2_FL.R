# ktore lotnisko mozna wybrac by bylo jak najmniejsze opoznienie, 
# taka aplikacja ze wybieramy lotnisko, 
# pokazuje jakies dane typu srednia itp
# a potem robi jakis ranking typu ze to lotnisko jest najlepsze

library("dplyr")
library("ggplot2")
library("stringi")
library("gganimate")
library("gifski")


data <- read.csv(file.path("..", "dane", "lata", "2006.csv"))
#filtruje danych - wylot do 7 minut przed czasem uznaje za punktualny (0 minut spoznienia)
data$DepDelay <- ifelse(data$DepDelay >= 0, data$DepDelay, ifelse(data$DepDelay >= -7, 0, data$DepDelay))


airports <- read.csv(file.path("..", "dane", "airports.csv"))


files <- list.files(path = file.path("..", "dane", "lata"))



# data %>%
#   select(Year, UniqueCarrier, FlightNum, TailNum, DepDelay, Origin, Cancelled, Diverted) %>%
#   group_by(Origin) %>%
#   summarize(meanTime = mean(DepDelay, na.rm = TRUE), flightsNumber = n(), medianTime = median(DepDelay, na.rm = TRUE), maxTime = max(DepDelay, na.rm = TRUE), minTime = min(DepDelay, na.rm = TRUE)) %>%
#   arrange((meanTime)) %>%
#   left_join(airports, by = join_by(Origin == iata)) %>% 
#   filter(Origin %in% c("ATL", "DEN", "DFW", "IAH", "JFK", "LAS", "LAX", "ORD", "PHX", "SFO")) %>% 
#   slice_head(n = 20) %>% 
#   arrange(Origin)
  


biggestAirports <- c("ATL", "DEN", "DFW", "IAH", "JFK", "LAS", "LAX", "ORD", "PHX", "SFO")
delayWeight <- 1
cancelledWeight <- 3

file_tmp <- files[19:20]





fillCancelledRatio <- function(df) {
  ifelse(df$Origin == "ATL", "yellow3", 
         ifelse(df$Origin == "DEN", "red3",
                ifelse(df$Origin == "DFW", "green3",
                       ifelse(df$Origin == "IAH", "blue3",
                              ifelse(df$Origin == "JFK", "pink3",
                                     ifelse(df$Origin == "LAS", "grey50",
                                            ifelse(df$Origin == "LAX", "purple3",
                                                   ifelse(df$Origin == "ORD", "violetred3",
                                                          ifelse(df$Origin == "PHX", "antiquewhite3",
                                                                 ifelse(df$Origin == "SFO", "orange3", 
                                                                        "white"))))))))))
}

fillMeanTime <- function(df) {
  ifelse(df$Origin == "ATL", "yellow1", 
         ifelse(df$Origin == "DEN", "red1",
                ifelse(df$Origin == "DFW", "green1",
                       ifelse(df$Origin == "IAH", "blue1",
                              ifelse(df$Origin == "JFK", "pink1",
                                     ifelse(df$Origin == "LAS", "grey70",
                                            ifelse(df$Origin == "LAX", "purple1",
                                                   ifelse(df$Origin == "ORD", "violetred2",
                                                          ifelse(df$Origin == "PHX", "antiquewhite2",
                                                                 ifelse(df$Origin == "SFO", "orange1", 
                                                                        "white"))))))))))
}





stworzDf <- function(data){
  data %>%
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
}



stworzWykres <- function(df, tytul) {
  ggplot(df, aes(x = position)) +
    geom_bar(aes(y = -meanTime),
             stat = "identity",
             position = "stack",
             width = 0.8,
             fill = fillMeanTime(df),
             color = "black") +
    geom_bar(aes(y = cancelledWeight * cancelledRatio),
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
    labs(x = "", y = paste("Średni czas opóźnienia (min)", "% odwołanych lotów", sep = "                         ")) +
    scale_y_continuous(
      breaks =  c(-seq(27, 0, -3), seq(0, 21, 3)),
      labels = c(seq(27, 0, -3), 0:7),
      limits = c(-33, 22)) +
    ggtitle(tytul) +
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


Scores <- data.frame(biggestAirports, matrix(0, nrow = 10, ncol = 4))
colnames(Scores) <- c("Origin", "score", "TotalDelayTime", "TotalCancelledNumber", "TotalFlightsNumber")

for (file in files) {  #file_tmp) { 
  # wczytanie danych i mala obrobka
  data <- read.csv(file.path("..", "dane", "lata", file))
  data$DepDelay <- ifelse(data$DepDelay >= 0, data$DepDelay, ifelse(data$DepDelay >= -7, 0, data$DepDelay))
  
  # wybranie tego co chcemy 
  print(
    df <- stworzDf(data)
  )
  
  #do podsumowania dane wybieramy
  
  year_data <- df %>% 
    arrange(Origin) %>% 
    select(Origin, Score, meanTime, cancelledRatio, flightsNumber)
  
  Scores$score <- Scores$score + year_data$Score  
  Scores$TotalDelayTime <- Scores$TotalDelayTime + year_data$meanTime * year_data$flightsNumber
  Scores$TotalCancelledNumber <- Scores$TotalCancelledNumber + year_data$cancelledRatio * year_data$flightsNumber / 100
  Scores$TotalFlightsNumber <- Scores$TotalFlightsNumber + year_data$flightsNumber
  
  
  #tworzenie wykresu
  rok <- stri_replace_all_regex(file, "\\.[^.]*$", "")
  tytul <- paste("Rok", rok)
  stworzWykres(df, tytul)

  #zapisywanie wykresu
  nazwa <- stri_replace_all_regex(file, "\\.[^.]*$", ".jpg")
  
  ggsave(file.path("..", "MozeKiedysBedzieTuWykres", nazwa), plot = last_plot(), dpi = 300, width = 10, height = 6)  
  
  
}

#tworzymy podsumowanie

score_data <- Scores %>% 
  mutate(meanTime = TotalDelayTime / TotalFlightsNumber) %>% 
  mutate(cancelledRatio = TotalCancelledNumber / TotalFlightsNumber * 100) %>% 
  mutate(Score = delayWeight * meanTime + cancelledWeight * cancelledRatio) %>% 
  arrange(desc(Score)) %>% 
  mutate(position = letters[1:10])

#towrzymy wykres podsumowania
stworzWykres(score_data, "Ogółem 1987 - 2008")

#zapisujemy
ggsave(file.path("..", "MozeKiedysBedzieTuWykres", "podsumowanie.jpg"), plot = last_plot(), dpi = 300, width = 10, height = 6) 

#####koniec




# probujemy zrobic animacje

data_2006 <- read.csv(file.path("..", "dane", "lata", "2006.csv"))
#filtruje danych - wylot do 7 minut przed czasem uznaje za punktualny (0 minut spoznienia)
data_2006$DepDelay <- ifelse(data_2006$DepDelay >= 0, data_2006$DepDelay, ifelse(data_2006$DepDelay >= -7, 0, data_2006$DepDelay))

data_2005 <- read.csv(file.path("..", "dane", "lata", "2005.csv"))
#filtruje danych - wylot do 7 minut przed czasem uznaje za punktualny (0 minut spoznienia)
data_2005$DepDelay <- ifelse(data_2005$DepDelay >= 0, data_2005$DepDelay, ifelse(data_2005$DepDelay >= -7, 0, data_2005$DepDelay))



df_2006 <- data_2006 %>%
  select(Year, UniqueCarrier, FlightNum, TailNum, DepDelay, Origin, Cancelled, Diverted) %>%
  filter(Origin %in% biggestAirports) %>% 
  filter(DepDelay >= 0) %>% 
  group_by(Origin) %>%
  summarize(meanTime = mean(DepDelay, na.rm = TRUE), 
            flightsNumber = n(), 
            medianTime = median(DepDelay, na.rm = TRUE), 
            maxTime = max(DepDelay, na.rm = TRUE), 
            minTime = min(DepDelay, na.rm = TRUE)) %>%
  left_join(data_2006 %>% 
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


df_2005 <- df_2005 %>% mutate(Year = 2005)
df_2006 <- df_2006 %>% mutate(Year = 2006)



binded0 <- rbind(df_2005, df_2006)


anim <- 
  ggplot(binded0, aes(x = position, group = Origin, fill = Origin)) +
  geom_bar(aes(y = -meanTime),
           stat = "identity",
           position = "stack",
           width = 0.8,
           color = "black",
           alpha = 0.6) +
  geom_bar(aes(y = cancelledWeight * cancelledRatio),
           stat = "identity",
           position = "stack",
           width = 0.8, 
           color = "black",
           alpha = 0.8) +
  geom_text(aes(y = -28 , label = Origin),
            hjust = 2,
            vjust = 0.3,
            size = 13,
            fontface = "bold",
            family = "mono") +
  geom_text(aes(x = 11.3, y = 0, label = "")) +
  scale_fill_manual(values=c("yellow1", "red1", "blue1", "green1", "pink1", "grey70", "purple1", "orange1", "violetred2", "antiquewhite2")) +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = paste("Średni czas opóźnienia (min)", "% odwołanych lotów", sep = "             ")) +
  scale_y_continuous(
    breaks =  c(-seq(27, 0, -3), seq(0, 21, 3)),
    labels = c(seq(27, 0, -3), 0:7),
    limits = c(-33, 22)) +
  guides(color = FALSE, fill = FALSE) +
  ggtitle("Rok {closest_state}") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20),
        plot.margin = margin(20, 20, 30, 20),
        panel.spacing = margin(t = 20),
        axis.title.x = element_text(size = 30,
                                    vjust = 0.2,
                                    hjust = 0.6,
                                    family = "mono"),
        plot.title = element_text(size = 50,
                                  hjust = 0.44,
                                  vjust = 1.5, 
                                  face = "bold",
                                  family = "")) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0),
               arrow = arrow(length = unit(0.4, "cm")),
               linewidth = 1.3,
               lineend = "square") +
  transition_states(Year, transition_length = 2, state_length = 1, wrap = FALSE)

animate(anim, 40, fps = 20,  width = 1500, height = 1000, 
        renderer = gifski_renderer("gganim.gif"), end_pause = 15, start_pause =  15) 



binded0 %>% 
  group_by(Year)


  
  
  
  
  


ggplot(binded, aes(x = position, frame = Year)) +
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
  ggtitle(paste("Rok", tytul)) +
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
               lineend = "square") +
  transition_time(Year) +
  labs(title = "Rok: {frame_time}")







  
  
  

