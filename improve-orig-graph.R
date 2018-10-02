# Thinh Pham & Katherine M. Prioli
# Makeover Monday - split reproduced graph by election results
# Tue Oct 02 15:31:24 2018 ------------------------------


#### Libraries ----

library(tidyverse)
library(readxl)
library(grid)
library(gridExtra)


#### Read in, wrangle, and join 2016 presidential election data ----

incometbl_raw <- read_csv("incometbl.csv")

incometbl <- incometbl_raw %>% 
  arrange(desc(incomecat), catpct) %>% 
  mutate(incomecat = factor(incomecat,
                            levels = c("<$25K", 
                                       "$25K-$50K", 
                                       "$51K-$75K", 
                                       "$76K-$100K", 
                                       "$101K-$150K", 
                                       ">$150K"))) %>%
  mutate(state = factor(state, levels = rev(state[1:50])))

popvote_raw <- read_xlsx("federalelections2016.xlsx", sheet = "Appendix A", range = "A7:C59")

popvote <- as.tibble(popvote_raw) %>% 
  filter(!is.na(STATE) & STATE != "D.C.") %>% 
  mutate(gop = as.numeric(TRUMP)) %>% 
  mutate(dem = as.numeric(CLINTON)) %>% 
  mutate(winner = case_when(
    dem < gop ~ "R",
    dem > gop ~ "D"
  )) %>% 
  mutate(winner = factor(winner, levels = c("R", "D"))) %>% 
  rename(state = STATE) %>% 
  mutate(state = factor(state, levels = rev(incometbl$state[1:50]))) %>% 
  select(state, winner)

popvote_gop <- popvote %>% filter(winner == "R") %>% count()
weight_gop = popvote_gop / 50
popvote_dem <- popvote %>% filter(winner == "D") %>% count()
weight_dem = popvote_dem / 50


incometbl <- full_join(incometbl, popvote, by = "state")

write_csv(incometbl, "incometbl_vote.csv")                                          # Export vote-augmented dataset
                                                                                    # in case Thinh wants it

#### Separate reproduced graph by election results ----

reprod_plot_gop <- ggplot(data = filter(incometbl, winner == "R"), 
                          aes(x = state, y = catpct / 100, fill = incomecat, label = catpct)) +
  coord_flip() +                                                                    # Put states on vertical axis
  geom_bar(stat = "identity", aes(y = catpct * 100), position = "fill") +
  scale_fill_manual(values = c(                                                     # Match color scheme of original graph
    "<$25K" = "#D62728",
    "$25K-$50K" = "#FF7F03",
    "$51K-$75K" = "#FFD500",
    "$76K-$100K" = "#9467BD",
    "$101K-$150K" = "#1F77B4",
    ">$150K" = "#6FAA12")) +
  geom_text(aes(label = paste0(catpct,"%")),                                        # Label stacked bars
            size = 2, color = "white", fontface = "bold", 
            hjust = 0.5, position = position_stack(vjust = 0.5)) +
  scale_y_reverse() +                                                               # Ensure <$25K category on left
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.5, "cm")) +
  guides(fill = guide_legend(nrow = 1))                                             # Force all legend elements into one row
#reprod_plot_gop

reprod_plot_dem <- ggplot(data = filter(incometbl, winner == "D"), 
                          aes(x = state, y = catpct / 100, fill = incomecat, label = catpct)) +
  coord_flip() +                                                                    # Put states on vertical axis
  geom_bar(stat = "identity", aes(y = catpct * 100), position = "fill") +
  scale_fill_manual(values = c(                                                     # Match color scheme of original graph
    "<$25K" = "#D62728",
    "$25K-$50K" = "#FF7F03",
    "$51K-$75K" = "#FFD500",
    "$76K-$100K" = "#9467BD",
    "$101K-$150K" = "#1F77B4",
    ">$150K" = "#6FAA12")) +
  geom_text(aes(label = paste0(catpct,"%")),                                        # Label stacked bars
            size = 2, color = "white", fontface = "bold", 
            hjust = 0.5, position = position_stack(vjust = 0.5)) +
  scale_y_reverse() +                                                               # Ensure <$25K category on left
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
#reprod_plot_dem

plot_gop <- arrangeGrob(reprod_plot_gop, left = textGrob("TRUMP", 
                                                         rot = 90,
                                                         gp = gpar(fontsize = 14, fontface = "bold")))
plot_dem <- arrangeGrob(reprod_plot_dem, left = textGrob("CLINTON", 
                                                         rot = 90,
                                                         gp = gpar(fontsize = 14, fontface = "bold")))

grid.arrange(plot_gop, plot_dem, nrow = 2,
             heights = c(2 * weight_gop, 2 * weight_dem),
             top = textGrob("HOUSEHOLD INCOME DISTRIBUTION IN USA \n BY STATE AND 2016 POPULAR VOTE",
                            gp = gpar(fontsize = 16, fontface = "bold")))


# Note - grid.arrange disables ggsave(), so I manually exported the graph using the Plots tab