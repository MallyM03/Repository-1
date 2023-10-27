if(!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  glue,
  gapminder,
  ggplot2,
  gridExtra
  
)

average=68
if(average>69) {
  print("First-class honours")
} else {
  print("Second Class Honours")
}

##Complex if-else##

average_1=59
if (average_1>69) {
  print("First-class")
} else if (average_1<70 & average_1>58){
  print("Second Class Honours")
} else {
  print("Third-Class Honours")
}

##Functional Coding##
gap<-gapminder
head(gap)
gap_ifelse<- gap %>%
  mutate(life.35= if_else(lifeExp>=35,1,0))
gap_ifelse

gap_norm<-gap %>%
  mutate(pop_norm=(pop-min(pop)/max(pop)-min(pop)),
         gdp_norm=(gdpPercap-min(gdpPercap)/max(gdpPercap)),
         life_norm=(lifeExp-min(lifeExp)/max(lifeExp)))
summary(gap_norm$pop_norm)

##Writing Functions##

simple.function<-function(x,y){
  print(x-y+1)
}
simple.function(200,100)

multiple.item<-function(x,y){
  thing1<-x
  thing2<-y
  return(list(thing1,thing2))
}
multiple.item("Tomato","Lettuce")

##Functional Programming##
set.seed(1234)
df<-tibble(
  "a"= sample(c(-99,1:3),size= 5,replace = TRUE),
  "b"= sample(c(-99,1:3),size = 5, replace=TRUE),
  "c"= sample(c(-99,1:3),size = 5, replace=TRUE),
  "d"= sample(c(-99,1:3),size = 5, replace=TRUE),
)

fix_missing<- function(x){
  x[x== -99]<-NA
  x
}  

library(tidyverse)
df<-purrr::map_df(df,fix_missing)
df

data("airquality")
out1<-vector("double",ncol(airquality))

for(i in seq_along(airquality)) {
  out1[[i]]<-mean(airquality[[i]],na.rm=TRUE)
}
out1<-airquality %>% map_dbl(mean,na.rm= TRUE)
out1

##Exercise 2##
Dat_1<-list(
  data.frame(City=c("London","Birmingham","Manchester"),
             Population=c(645466,358459,159459)),
  data.frame(City=c("Leeds","Liverpool","Newcastle"),
             Population=c(6565678,346845,234562))
)
population_threshhold<-500000
filtered_data<-map(Dat_1,~filter(.x,Population>=population_threshhold))
filtered_data
combined_data<- reduce(filtered_data, bind_rows)
combined_data
