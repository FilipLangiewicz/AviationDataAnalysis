# generowanie porownan danych dla dwoch lotnisk


library("ggplot2")
library("dplyr")
library("scales")



najwazniejszeDane <- read.csv(file.path("..", "dane", "najwazniejszeDane.csv"))
najwazniejszeDane$cancelledRatio <- najwazniejszeDane$cancelledRatio / 100 
airports <- read.csv(file.path("..", "dane", "airports.csv"))
lotniska1 <-  c("IAH", "ORD")
lotniska2 <- c("ATL", "JFK")

lotniska <- lotniska2

# generowanie wykresu z liczba lotow w tych latach
  df <- najwazniejszeDane %>% 
    filter(Origin %in% lotniska, Year != 1987, Year != 2008) %>% 
    select(Origin, flightsNumber, Year)
  
  
  ggplot(df, aes(x = Year, y = flightsNumber, group = Origin)) +
    geom_line(linewidth = 1.5,
              aes(color = Origin)) +
    geom_point(size = 1.5,
               aes(color = Origin)) +  
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
          plot.margin = margin(t = 20),
          legend.position = "right") +
    scale_color_manual(values = c("#00539C", "#EEA47F"), name = "Lotnisko")
    
  ggsave(file.path("..", "MozeKiedysBedzieTuWykres", "LiczbaLotow2.jpg"), plot = last_plot(), dpi = 300, width = 10, height = 6)
  
  
#generowanie wykresu ze srednim opoznieniem w tych latach
  df <- najwazniejszeDane %>% 
    filter(Origin %in% lotniska, Year != 1987, Year != 2008) %>% 
    select(Origin, meanTime, Year)
  
  ggplot(df, aes(x = Year, y = meanTime, group = Origin)) +
    geom_line(linewidth = 1.5,
              aes(color = Origin)) +
    geom_point(size = 1.5,
               aes(color = Origin)) +  
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
          plot.margin = margin(t = 20),
          legend.position = "right") +
  scale_color_manual(values = c("#00539C", "#EEA47F"), name = "Lotnisko")
  
  ggsave(file.path("..", "MozeKiedysBedzieTuWykres", "Opoznienie2.jpg"), plot = last_plot(), dpi = 300, width = 10, height = 6)
  

#generowanie wykresu ze wspolczynnikiem opoznien w tych latach
  df <- najwazniejszeDane %>% 
    filter(Origin %in% lotniska, Year != 1987, Year != 2008) %>% 
    select(Origin, cancelledRatio, Year)
  
  ggplot(df, aes(x = Year, y = cancelledRatio, group = Origin)) +
    geom_line(linewidth = 1.5,
              aes(color = Origin)) +
    geom_point(size = 1.5,
               aes(color = Origin)) +  
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
          plot.margin = margin(t = 20),
          legend.position = "right") +
    scale_color_manual(values = c("#00539C", "#EEA47F"), name = "Lotnisko")
  
  ggsave(file.path("..", "MozeKiedysBedzieTuWykres", "Odwolania2.jpg"), plot = last_plot(), dpi = 300, width = 10, height = 6)



