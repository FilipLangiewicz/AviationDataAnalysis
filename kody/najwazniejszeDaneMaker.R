# skrypt do stworzenia malej ramki danych
# ze wszystkimi waznymi danymi

library("dplyr")
library("stringi")


airports <- read.csv(file.path("..", "dane", "airports.csv"))
files <- list.files(path = file.path("..", "dane", "lata"))
biggestAirports <- c("ATL", "DEN", "DFW", "IAH", "JFK", "LAS", "LAX", "ORD", "PHX", "SFO")
delayWeight <- 1
cancelledWeight <- 3


dfAllYears <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(dfAllYears) <- c("Origin", "meanTime", "flightsNumber", "medianTime", "maxTime", "minTime", "cancelledRatio", "Score", "airport", "city", "state", "country", "position", "Year")



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



#tworzenie ramki danych ze wszystkich lat (z tylko potrzebnymi danymi)

for (file in files) {
  # wczytanie danych i mala obrobka
  data <- read.csv(file.path("..", "dane", "lata", file))
  data$DepDelay <- ifelse(data$DepDelay >= 0, data$DepDelay, ifelse(data$DepDelay >= -7, 0, data$DepDelay))
  
  
  rok <- stri_replace_all_regex(file, "\\.[^.]*$", "")
  
  # wybranie tego co chcemy 
  df <- stworzDf(data)
  dfWithYear <- df %>% 
    mutate(Year = rok)
  
  #laczenie w jedna duza
  dfAllYears <- rbind(dfAllYears, dfWithYear)
}

write.csv(dfAllYears,file.path("..", "dane", "najwazniejszeDane.csv"))


