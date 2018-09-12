# Katherine M. Prioli
# Makeover Monday potential datasets exploration
# Wed Sep 12 17:25:42 2018 ------------------------------


library(tidyverse)
library(readxl)


incomedf <- read_excel("household-income-distribution-by-state.xlsx")
#View(incomedf)
any(is.na(incomedf))

whiskeydf <- read_excel("whiskey-sales.xlsx")
#View(whiskeydf)
any(is.na(whiskeydf))

#### Investigating missing data in whiskeydf ----

num <- sum(is.na(whiskeydf$Cases))
denom <- num + sum(!is.na(whiskeydf$Cases))
pct_missing <- num / denom