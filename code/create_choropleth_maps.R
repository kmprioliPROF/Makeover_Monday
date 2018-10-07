## Project: Makeover monday
## Thinh Pham
## Create choropleth maps of income distributions

library(tidyverse)
library(maps)

#### Set up state map layer and income data ---
states <- map_data("state")
incometbl <- read_csv("incometbl_vote.csv")

### Prepare to merge incometbl and state data
incometbl <- incometbl %>% 
                  mutate(region = tolower(state)) %>% 
                  select(-state)

income_data <- states %>%                                  #Inner_join() used to eliminate D.C
                  inner_join(incometbl, by = "region")     #(not listed in the original graph)

# Set up factor levels for organization of graph
incomecat_levels <- factor(c("<$25K", "$25K-$50K", "$51K-$75K",
                             "$76K-$100K", "$101K-$150K", ">$150K"))
income_data[,"incomecat"] <- factor(income_data[,"incomecat"], 
                                    levels = incomecat_levels)

#### Create plotting function with diverging color schemes ---
plot_category_scheme1 <- function(cat, scheme = 1) {
    scheme1 <- c('#8c510a','#d8b365','#f6e8c3',
                 '#c7eae5','#5ab4ac','#01665e')
    scheme2 <- c('#b35806','#f1a340','#fee0b6',
                 '#d8daeb','#998ec3','#542788')
    color_scheme <- if(scheme == 1) {scheme1} else {scheme2}
  
    result <- income_data %>% filter(incomecat == cat) %>% 
    mutate(percentage = cut_interval(catpct, n = 6)) %>%  #Divide into 6 equal intervals
       ggplot() +
       geom_polygon(aes(x = long, y = lat,
                     fill = percentage, 
                     group = group,
                     color = winner)) +                   #Border color by political party
       scale_fill_manual(values = color_scheme) +
       scale_color_manual(values = c("black", "red")) +
       coord_fixed(1.3) +
       ggtitle(paste("Distribution of income group", cat))
    
    result
}

# Create and save plots ----
# cat_names <- c("below_25k", "25_to_50k", "51_to_75k",
#                "76_to_100k", "101_to_150k", "above_150k")
# for (i in 1:6) {
#     cat <- as.character(incomecat_levels)[i]
#     name <- cat_names[i]
#     plot_category_scheme1(cat,scheme = 1)
#     ggsave(filename = paste0("choropleth\\scheme1\\choropleth1_",name,".png"),
#            height = 4, width = 8)
#     plot_category_scheme1(cat,scheme = 2)
#     ggsave(filename = paste0("choropleth\\scheme2\\choropleth2_",name,".png"),
#            height = 4, width = 8)
# }