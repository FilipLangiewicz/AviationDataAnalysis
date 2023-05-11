# ktore lotnisko mozna wybrac by bylo jak najmniejsze opoznienie, 
# taka aplikacja ze wybieramy lotnisko, 
# pokazuje jakies dane typu srednia itp
# a potem robi jakis ranking typu ze to lotnisko jest najlepsze

library("dplyr")
library("data.table")
library("ggplot2")
library("stringi")

data <- read.csv(file.path("..", "dane", "lata", "2006.csv"))
#filtruje danych - wylot do 7 minut przed czasem uznaje za punktualny (0 minut spoznienia)
data$DepDelay <- ifelse(data$DepDelay >= 0, data$DepDelay, ifelse(data$DepDelay >= -7, 0, data$DepDelay))


airports <- read.csv(file.path("..", "dane", "airports.csv"))

# data <- data %>% 
#   select(-c(Tailnum, AirTime, TaxiIn, TaxiOut,))

files <- list.files(path = file.path("..", "dane", "lata"))

for (file in files) {
  data <- read.csv(file.path("..", "dane", "lata", file))
  print(showData(data, airports))
}


showData <- function(data, airports) {
    data %>%
    select(Year, UniqueCarrier, FlightNum, TailNum, DepDelay, Origin, Cancelled, Diverted) %>%
    group_by(Origin) %>%
    summarize(meanTime = mean(DepDelay, na.rm = TRUE), flightsNumber = n(), medianTime = median(DepDelay, na.rm = TRUE), maxTime = max(DepDelay, na.rm = TRUE), minTime = min(DepDelay, na.rm = TRUE)) %>%
    arrange((meanTime)) %>%
    right_join(airports, by = join_by(Origin == iata)) %>% 
    slice_head(n = 20)
  
}



data %>%
  filter(Cancelled == 1) %>%
  select(DepDelay, Cancelled)

airports %>%
  filter(country != "USA")

data %>% 
  filter(Origin %in% c("ROP", "ROR", "SPN", "YAP") | Dest %in% c("ROP", "ROR", "SPN", "YAP"))

data %>% 
  filter(Origin == "BFI") %>% 
  # select(DepDelay, Origin,, Dest, DepTime, CRSDepTime) %>% 
  arrange(desc(DepDelay)) %>% 
  slice_head(n = 20)


data %>%
  select(Year, UniqueCarrier, FlightNum, TailNum, DepDelay, Origin, Cancelled, Diverted) %>%
  group_by(Origin) %>%
  summarize(meanTime = mean(DepDelay, na.rm = TRUE), flightsNumber = n(), medianTime = median(DepDelay, na.rm = TRUE), maxTime = max(DepDelay, na.rm = TRUE), minTime = min(DepDelay, na.rm = TRUE)) %>%
  arrange((meanTime)) %>%
  left_join(airports, by = join_by(Origin == iata)) %>% 
  filter(Origin %in% c("ATL", "DEN", "DFW", "IAH", "JFK", "LAS", "LAX", "ORD", "PHX", "SFO")) %>% 
  slice_head(n = 20) %>% 
  arrange(Origin)
  


biggestAirports <- c("ATL", "DEN", "DFW", "IAH", "JFK", "LAS", "LAX", "ORD", "PHX", "SFO")
delayWeight <- 1
cancelledWeight <- 3

file_tmp <- files[19:20]





fillCancelledRatio <- function() {
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

fillMeanTime <- function() {
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



for (file in files) {  #file_tmp) { 
  data <- read.csv(file.path("..", "dane", "lata", file))
  data$DepDelay <- if_else(data$DepDelay >= 0, data$DepDelay, ifelse(data$DepDelay >= -7, 0, data$DepDelay))
  
  print(
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
  )
  
  
  ggplot(df, aes(x = position)) +
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
              hjust = 1.8,
              vjust = 0.3,
              size = 6,
              fontface = "bold",
              family = "mono") +
    coord_flip() +
    theme_minimal() +
    labs(x = "", y = "") +
    scale_y_continuous(
      breaks =  c(-seq(27, 0, -3), seq(0, 18, 3)),
      labels = c(seq(27, 0, -3), 0:6),
      limits = c(-33, 18)) +
    theme(axis.text.y = element_blank(),
          plot.margin = margin(20, 20, 20, 20),
          panel.spacing = margin(t = 20)) +
    geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0),
                 arrow = arrow(length = unit(0.4, "cm")),
                 linewidth = 1.3,
                 lineend = "square") 
    
    

  nazwa <- stri_replace_all_regex(file, "\\.[^.]*$", ".jpg")
  

  ggsave(file.path("..", "MozeKiedysBedzieTuWykres", nazwa), plot = last_plot(), dpi = 300, width = 10, height = 6)  
  
  
}



ggplot(df) +
  geom_point(aes(x = 0, y = Score)) +
  xlim(-20, 40) 
  geom_col(aes(x = -cancelledRatio, y = Score), fill = "BLUE" ) 
  geom_col(aes(x = meanTime, y = Origin), fill = "BLUE3") +
  geom_text(label = rownames(df),
            check_overlap = TRUE) +
  labs(title = "year",
       x = "",
       y = "") +
  theme_minimal()


  
  
  ggplot(df) +
    geom_bar(aes(x = Origin, y = -meanTime, fill = "meanTime"),
             stat = "identity",
             width = 0.5,
             position = "stack") +
    geom_bar(aes(x = Origin, y = cancelledRatio, fill = "cancelledRatio"),
             stat = "identity",
             width = 0.5,
             position = "stack") +
    geom_text(aes(x = Origin, y = Score, label = Origin), vjust = -0.0, size = 3) +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = c(meanTime = "blue", cancelledRatio = "red"),
                      guide = guide_legend(title = NULL)) +
    labs(x = NULL, y = "Score")
  

