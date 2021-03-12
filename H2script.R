#install.packages("countrycode")

library(here)
library(lubridate)
library(tidyverse)
library(janitor)
library(rio)
library(ggplot2)

#data
transit_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

crime <- import(here("data","crime.csv")) %>% 
  clean_names() %>% 
  as_tibble()

country_codes <- countrycode::codelist %>% 
  select(country_name = country.name.en, country = ecb)

#part2: Uncertainty plot


#part3: Dotplot
#code provided by Daniel:
model_data <- crime %>% 
  mutate(neighborhood_id = relevel(factor(neighborhood_id), ref = "barnum"))

m <- glm(is_crime ~ neighborhood_id, 
         data = model_data,
         family = "binomial")

tidied <- broom::tidy(m)

#Barnum compared to Regis(example from homework):
regis <- tidied %>% 
  filter(term == "neighborhood_idregis")

qnorm(ppoints(20), 
      mean = regis$estimate,
      sd = regis$std.error)

#Barnum compared to Barnum-West:
bwest <- tidied %>% 
  filter(term == "neighborhood_idbarnum-west")

qnorm(ppoints(20), 
      mean = bwest$estimate,
      sd = bwest$std.error)

#part4: Table
