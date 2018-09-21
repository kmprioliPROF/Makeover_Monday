# Thinh Pham & Katherine M. Prioli
# Makeover Monday - reproduce original graph
# Thu Sep 20 22:59:12 2018 ------------------------------


#### Libraries ----

library(tidyverse)


#### Read in and subset data ----

incomedata <- read_csv("ACS_16_1YR_S1901_with_ann.csv")
incomedata <- incomedata %>% select("GEO.display-label",     # Subset to only the variables of interest
                                    "HC01_EST_VC01",
                                    "HC01_EST_VC02",
                                    "HC01_EST_VC03",
                                    "HC01_EST_VC04",
                                    "HC01_EST_VC05",
                                    "HC01_EST_VC06",
                                    "HC01_EST_VC07",
                                    "HC01_EST_VC08",
                                    "HC01_EST_VC09",
                                    "HC01_EST_VC10",
                                    "HC01_EST_VC11")

colnames(incomedata) <- c("state",                           # GEO.display-label
                          "tot_households",                  # HC01_EST_VC01
                          "Less than $10,000",               # HC01_EST_VC02
                          "$10,000 to $14,999",              # HC01_EST_VC03
                          "$15,000 to $24,999",              # HC01_EST_VC04
                          "$25,000 to $34,999",              # HC01_EST_VC05
                          "$35,000 to $49,999",              # HC01_EST_VC06
                          "$50,000 to $74,999",              # HC01_EST_VC07
                          "$75,000 to $99,999",              # HC01_EST_VC08
                          "$100,000 to $149,999",            # HC01_EST_VC09
                          "$150,000 to $199,999",            # HC01_EST_VC10
                          "$200,000 or more")                # HC01_EST_VC11

incomedata <- incomedata %>% 
  slice(2:n()) %>% 
  filter(state != "Puerto Rico" & state != "District of Columbia")       # Omit PR and DC per original graph
#View(incomedata)


#### Gather and mutate data ----

incomegat <- incomedata %>% 
  gather(`Less than $10,000`,
         `$10,000 to $14,999`,
         `$15,000 to $24,999`,
         `$25,000 to $34,999`,
         `$35,000 to $49,999`,
         `$50,000 to $74,999`,
         `$75,000 to $99,999`,
         `$100,000 to $149,999`,
         `$150,000 to $199,999`,
         `$200,000 or more`,
         key = "incomelev", value = "pct")
#View(incomegat)

incometbl <- incomegat %>% 
  mutate(tot_households = as.numeric(tot_households)) %>% 
  mutate(pct = as.numeric(pct)) %>% 
  mutate(n_households = round((pct / 100) * tot_households, digits = 0)) %>% 
  mutate(incomecat = case_when(
    incomelev == "Less than $10,000" ~ "<$25K",
    incomelev == "$10,000 to $14,999" ~ "<$25K",
    incomelev == "$15,000 to $24,999" ~ "<$25K",
    incomelev == "$25,000 to $34,999" ~ "$25K-$50K",
    incomelev == "$35,000 to $49,999" ~ "$25K-$50K",
    incomelev == "$50,000 to $74,999" ~ "$51K-$75K",
    incomelev == "$75,000 to $99,999" ~ "$76K-$100K",
    incomelev == "$100,000 to $149,999" ~ "$101K-$150K",
    incomelev == "$150,000 to $199,999" ~ ">$150K",
    incomelev == "$200,000 or more" ~ ">$150K")) %>% 
  mutate(incomecat = factor(incomecat,
                        levels = c("<$25K", 
                                   "$25K-$50K", 
                                   "$51K-$75K", 
                                   "$76K-$100K", 
                                   "$101K-$150K", 
                                   ">$150K"))) %>%
  group_by(state, incomecat) %>% 
  mutate(catpct = sum(pct)) %>% 
  select(state, catpct, incomecat) %>% 
  distinct(state, catpct, incomecat) %>% 
  arrange(desc(incomecat), catpct)
#View(incometbl)

incometbl <- within(incometbl,                     # Assign levels for state column
                    state <- factor(state,
                                    levels = rev(incometbl$state[1:50])))

#### Recreate the original graph ----

reprod_plot <- ggplot(incometbl, aes(x = state, y = catpct / 100, 
                                     fill = incomecat, label = catpct)) +
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
  guides(fill = guide_legend(nrow = 1)) +                                           # Force all legend elements into one row
  ggtitle("HOUSEHOLD INCOME DISTRIBUTION IN USA BY STATE")
reprod_plot

#ggsave("reprod_plot.png", height = 15, width = 9.1, units = "in")


#### Export wrangled data to .csv for use in making new graph ----

write_csv(incometbl, "incometbl.csv")


#### NEXT STEPS FOR REPRODUCED GRAPH ----
    # Cosmetic improvements as needed (low priority)