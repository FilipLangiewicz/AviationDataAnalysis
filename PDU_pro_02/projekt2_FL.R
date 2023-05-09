# ktore lotnisko mozna wybrac by bylo jak najmniejsze opoznienie, 
# taka aplikacja ze wybieramy lotnisko, 
# pokazuje jakies dane typu srednia itp
# a potem robi jakis ranking typu ze to lotnisko jest najlepsze

library("dplyr")

data <- read.csv(file.path("dane","1988.csv"))
airports <- read.csv(file.path("dane", "airports.csv"))

# data <- data %>% 
#   select(-c(Tailnum, AirTime, TaxiIn, TaxiOut,))

files <- list.files(path = file.path("dane", "lata"))

for (file in files) {
  data <- read.csv(file.path("dane", "lata", file))
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

