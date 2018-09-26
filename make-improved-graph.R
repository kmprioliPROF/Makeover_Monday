# Thinh Pham & Katherine M. Prioli
# Makeover Monday - create improved graph
# Wed Sep 26 14:25:37 2018 ------------------------------

#### Libraries ----

library(tidyverse)
library(fiftystater)


#### Read in data ----

incometbl <- read_csv("incometbl.csv")

incometbl <- incometbl %>% 
  mutate(state = tolower(state))


#### Creating separate tibbles for each income category ----

# Can get the function below working if we have time;
# otherwise, what's below will suffice

# incomecat_fn <- function(x, i){
#   incometbl %>%
#     filter(incomecat == x[i]) -> paste0("income_cat_",i)
# }
# 
# x <- c("<$25K", "$25K-$50K", "$51K-$75K", "$76K-$100K", "$101K-$150K", ">$150K")
# 
# for(i in 1:length(x)){
#   incomecat_fn(x, i)
# }

incomecat_1 <- incometbl %>%
  filter(incomecat == x[1])

incomecat_2 <- incometbl %>%
  filter(incomecat == x[2])

incomecat_3 <- incometbl %>%
  filter(incomecat == x[3])

incomecat_4 <- incometbl %>% 
  filter(incomecat == x[4])

incomecat_5 <- incometbl %>% 
  filter(incomecat == x[5])

incomecat_6 <- incometbl %>% 
  filter(incomecat == x[6])


#### Plot choropleths ----

map_cat_1 <- ggplot(data = incomecat_1, aes(map_id = state)) +
  geom_map(aes(fill = catpct), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  ggtitle("<$25K Income Distribution Across the US")
map_cat_1

map_cat_2 <- ggplot(data = incomecat_2, aes(map_id = state)) +
  geom_map(aes(fill = catpct), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  ggtitle("$25K-$50K Income Distribution Across the US")
map_cat_2

map_cat_3 <- ggplot(data = incomecat_3, aes(map_id = state)) +
  geom_map(aes(fill = catpct), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  ggtitle("$51K-$75K Income Distribution Across the US")
map_cat_3

map_cat_4 <- ggplot(data = incomecat_4, aes(map_id = state)) +
  geom_map(aes(fill = catpct), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  ggtitle("$76K-$100K Income Distribution Across the US")
map_cat_4

map_cat_5 <- ggplot(data = incomecat_5, aes(map_id = state)) +
  geom_map(aes(fill = catpct), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  ggtitle("$101K-$150K Income Distribution Across the US")
map_cat_5

map_cat_6 <- ggplot(data = incomecat_6, aes(map_id = state)) +
  geom_map(aes(fill = catpct), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  ggtitle(">$150K Income Distribution Across the US")
map_cat_6