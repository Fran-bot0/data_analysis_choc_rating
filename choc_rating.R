# Challenge for R Course
# Chocolate Bar Rating over years
# Written by Francisco Narciso 20/05/24 InPP

library(dplyr)
library(ggplot2)

raw_data <- read.csv('data/chocolate.csv')

head(raw_data)
names(raw_data)
summary(raw_data)
unique(is.na(raw_data))

grouped_data <- group_by(raw_data, company_location, review_date)

grouped_data <- grouped_data |>
  summarise(mean = mean(rating)) |>
  filter(company_location %in% c('Belgium', 
                                 'France', 
                                 'Germany',
                                 'Italy', 
                                 'Switzerland'))

unique(grouped_data$company_location)
sapply(grouped_data, class)


ggplot(grouped_data, aes(x = review_date, y = mean, color = company_location)) +
  geom_point(size = 3.5) +
  geom_line() +
  annotate('segment',
           linetype = 'dashed',
           size = 1,
           x = 2006, 
           xend = 2021, 
           y = mean(grouped_data$mean), 
           yend = mean(grouped_data$mean),
           colour = '#ffffff') + 
  labs(title='Chocolate Bar Rating over years',
       y = 'Mean Rating',
       x = '',
       color = 'Country') +
  
  scale_x_continuous(breaks = c(2006:2021), 
                     limits = c(2006, 2021)) +
  scale_y_continuous(breaks = seq(0, 4, by = 0.25), 
                     limits = c(2.5, 4)) +
  scale_color_manual(values = c("Belgium" = "#6f1d1b", 
                                "France" = "#bb9457",
                                'Germany' = '#432818',
                                'Italy' = '#585828',
                                'Switzerland' = '#99582a')) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        panel.border = element_blank(), 
        axis.line = element_line(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = c(0.1, 0.85),
        legend.key.size = unit(1, "lines"),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(color = NA, fill = NA),
        plot.title = element_text(size = 16, face = 'italic', hjust = 0.5),
        panel.background = element_rect(fill = '#f7ece1'),
        plot.background = element_rect(fill = "#f7ece1"))

ggsave('img/choc_graph.png', width = 12, height = 6, dpi = 300)
