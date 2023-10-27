pacman::p_load(
  tidyverse,
  skimr
)
library(tidyverse)
library(skimr)
library(dplyr)
str(table1)
Dat_1<-iris
str(Dat_1)


##Seminar##
rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  kableExtra,
  flextable, 
  skimr)
data<- read.csv("EMDAT.csv", header=TRUE)
glimpse(data)
str(data)

glimpse(data_2)
levels(data_2$Country)
data_2 <- data %>% select(Country.name, Year,Number.of.total.people.affected.by.disasters,Number.of.deaths.from.disasters,Homelessness.rate.from.disasters)%>%
  rename(death_by_disasters = Number.of.deaths.from.disasters, affected_by_disasters= Number.of.total.people.affected.by.disasters, 
         homelessness = Homelessness.rate.from.disasters,Country = Country.name )
averages <- data_2 %>%
  filter(!Country %in% c("World", "Soviet Union")) %>%  
  group_by(Country) %>%
  summarise(
    avg_deaths = mean(death_by_disasters, na.rm = TRUE),
    avg_affect = mean(affected_by_disasters, na.rm = TRUE),
    avg_homelessness = mean(homelessness, na.rm = TRUE)
  )
top_20_deaths<- averages %>%
  arrange(desc(avg_deaths)) %>%
  head(20) %>%
  kable(caption = "Top 20 countries by Average Deaths ")
top_20_deaths %>%
  kable_styling("striped") %>%
  kable_classic(full_width=FALSE)
  
top_20_homelessness <- averages %>%
  arrange(desc(avg_homelessness)) %>%
  head(20) %>%
  kable(caption = "Top 20 Countries by Average Homelessness")
top_20_homelessness %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)

data_2 <- data_2 %>% mutate(high_death = ifelse(death_by_disasters > 500, 1, 0))

data_2_wide <- data_2 %>%
  pivot_wider(
    names_from = Year,       
    values_from = c(death_by_disasters, affected_by_disasters, homelessness, high_death))
saveRDS(data_2_wide, "data_2_wide.rds")
