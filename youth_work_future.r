##median populations ----
library(xlsx)
library(tidyverse)
library(ggthemes)
library(forcats)
library(directlabels)
##Import data ----
med_pop <- read.csv("median_pop_median_variant.csv")
urban_pop <- read.csv("urban_pop.csv")
pop_proj <- read.csv("pop_proj.csv")


##Convert data
med_pop <- pivot_longer(med_pop, cols = 10:26, names_to = "year")

##Remove the X in year ----
med_pop$year <- str_sub(med_pop$year, 2, 5)

##Do a graph on median ages by region and year----
med_pop_new <- med_pop %>% group_by(Region) %>% 
  filter(year %in% c("2020", "2050") & !Region %in% 
  c("Oceania", "Australia & NZ"))

ggplot(data = med_pop_new, aes(x = fct_reorder(Region, value), y = value, fill = year)) + 
  geom_boxplot() + theme_economist() + 
  labs(y = "Median age (Years)", x = "Region", 
       title = "Median Age by Region (2020 & 2050)", 
       caption = "Figure 1: Median Age by Region (2020, 2050) 
       Source: Constructed from UNPD data") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(color = "black", size = 12, face = "bold")) + 
  theme(axis.text.x = element_text(size = 7)) + 
  theme(axis.title.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 7)) + 
  theme(axis.title.y = element_text(size = 8)) + 
  theme(legend.text = element_text(size = 8)) + 
  theme(plot.caption = element_text(size = 7))

urban_pop %>% group_by(Region) %>% 
  filter(!Region %in% c("North America")) %>% 
  ggplot(aes(x = year, y = urban_pop, color = Region)) + 
  geom_line() + theme_economist() + 
  labs(y = "Urban Population (%)", x = "Year", 
       title = "Urban Population by Region", 
       caption = "Figure 1: Urban Population by Region (1950 - 2050)
       $LAC: Latin America and the Caribbean
       Source: Constructed from UNPD data") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(color = "black", size = 12, face = "bold")) + 
  theme(axis.text.x = element_text(size = 7)) + 
  theme(axis.title.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 7)) + 
  theme(axis.title.y = element_text(size = 8)) + 
  theme(legend.text = element_text(size = 8)) + 
  theme(plot.caption = element_text(size = 7)) + 
  geom_dl(aes(label = Region), method = list(dl.combine("last.points"), cex = 0.8)) 

##
pop_proj %>% filter(Entity %in% 
    c("Asia", "Africa", "Europe", "Latin America", 
      "North America", "Oceania") & as.numeric(Year %in% c(1950:2100)) & 
      !is.na(Population)) %>% group_by(Entity) %>% 
  ggplot(aes(x = Year, y = Population, fill = Entity)) + geom_col() + 
  theme_economist() + labs(y = "Population", x = "Year", 
       title = "Total World Population by Region (1950 - 2100)", 
       caption = "Figure 1: Total Population by Region (1950 - 2100)
       $Latin America: Latin America and the Caribbean
       Source: Constructed from UNPD data") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(color = "black", size = 12, face = "bold")) + 
  theme(axis.text.x = element_text(size = 7)) + 
  theme(axis.title.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 7)) + 
  theme(axis.title.y = element_text(size = 8)) + 
  theme(legend.text = element_text(size = 8)) + 
  theme(plot.caption = element_text(size = 7))

