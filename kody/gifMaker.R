#skrypt do wygenerowania gifa


library("ggplot2")
library("gganimate")
library("gifski")


dfAllYears <- read.csv(file.path("..", "dane", "najwazniejszeDane.csv"))
delayWeight <- 1
cancelledWeight <- 3

anim <- 
  ggplot(dfAllYears, aes(x = position, group = Origin, fill = Origin)) +
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
  transition_states(Year, 
                    transition_length = 2, 
                    state_length = 1, 
                    wrap = FALSE)

animate(anim, 240, fps = 8,  width = 1500, height = 1000, 
        renderer = gifski_renderer(file.path("..","MozeKiedysBedzieTuWykres", "zmiany.gif")), end_pause = 15, start_pause =  15) 


