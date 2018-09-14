# Katherine M. Prioli
# Makeover Monday - reproduce original graph
# Thu Sep 13 20:33:54 2018 ------------------------------


#### Libraries ----

library(tidyverse)


#### Read in and subset data ----

incomedata <- read_csv("ACS_16_1YR_S1901_with_ann.csv")
incomedata <- incomedata %>% select("GEO.display-label",
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

colnames(incomedata) <- c("state",                   # GEO.display-label
                          "tot_households",          # HC01_EST_VC01
                          "Less than $10,000",       # HC01_EST_VC02
                          "$10,000 to $14,999",      # HC01_EST_VC03
                          "$15,000 to $24,999",      # HC01_EST_VC04
                          "$25,000 to $34,999",      # HC01_EST_VC05
                          "$35,000 to $49,999",      # HC01_EST_VC06
                          "$50,000 to $74,999",      # HC01_EST_VC07
                          "$75,000 to $99,999",      # HC01_EST_VC08
                          "$100,000 to $149,999",    # HC01_EST_VC09
                          "$150,000 to $199,999",    # HC01_EST_VC10
                          "$200,000 or more")        # HC01_EST_VC11

incomedata <- incomedata %>% 
  slice(2:n()) %>% 
  filter(state != "Puerto Rico" & state != "District of Columbia")  # Omitting DC and PR per original graph
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
    incomelev == "Less than $10,000" ~ 1,
    incomelev == "$10,000 to $14,999" ~ 1,
    incomelev == "$15,000 to $24,999" ~ 1,
    incomelev == "$25,000 to $34,999" ~ 2,
    incomelev == "$35,000 to $49,999" ~ 2,
    incomelev == "$50,000 to $74,999" ~ 3,
    incomelev == "$75,000 to $99,999" ~ 4,
    incomelev == "$100,000 to $149,999" ~ 5,
    incomelev == "$150,000 to $199,999" ~ 6,
    incomelev == "$200,000 or more" ~ 6)) %>% 
  group_by(state, incomecat) %>% 
  mutate(catpct = sum(pct)) %>% 
  select(state, incomecat, catpct) %>% 
  distinct(state, incomecat, catpct)
#View(incometbl)

#incomesubset <- incometbl %>% filter(state %in% c("West Virginia", "Pennsylvania", "New Jersey"))


#### Recreating the original graph ----

orig_plot <- ggplot(incometbl, aes(x = state, y = catpct / 100, fill = incomecat, label = catpct)) +
  coord_flip() +
  #ylim(0, 100) +
  geom_bar(stat = "identity", aes(y = catpct * 100), position = "fill") +
  geom_text(aes(label = paste0(catpct,"%")), size = 2, color = "white", hjust = 0.5, position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = "top",
        aspect.ratio = 1.61) +
  ggtitle("HOUSEHOLD INCOME DISTRIBUTION IN USA BY STATE")
orig_plot

#ggsave("orig_plot.png", height = 15, width = 9.1, units = "in")


#### NEXT STEPS:
     # Discretize the income categories and apply a custom color scale
     # Properly format the labels (they're in the thousands right now)
     # Order the graph by increasing income category 6
     # Apply nice formatting to legend