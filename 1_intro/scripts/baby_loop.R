library(tidyverse) ; library(babynames) ; library(hrbrthemes)

list_of_names <- c("Bob", "Bobbie", "Bobbette")

for(name_search in list_of_names){
  
  p <- babynames %>%
    filter(name == name_search)  %>%
    ggplot(data = ., aes(x = year, y = n)) +
    geom_line(aes(color = sex), size = 2) +
    scale_color_manual(values = c("#66c2a5", "#ff7f00"),
                       labels = c("Female", "Male")) +
    labs(title = paste0("Babies named ", name_search," between 1880 and 2015"),
         x = "Year", 
         y = "Frequency",
         caption = "Source: Social Security Administration",
         color = "") +
    theme_ipsum_rc() +
    theme(legend.position = "top")
  
  name_of_plot <- paste0("baby_plot_", name_search, ".png")
  ggsave(name_of_plot, scale=1, dpi=300, plot = p)
  
}
