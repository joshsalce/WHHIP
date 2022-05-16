library(tidyverse)
library(dplyr)

WHHIP = read.csv(file.choose())


## All pitchers at least 10 innings pitched
# No position players
WHHIP %>%
  filter(IP >= 10.00) -> WHHIP_filt


## Relievers
WHHIP_filt %>%
  filter(IP <= 90.00) -> WHHIP_RP
  
## Starters
WHHIP_filt %>%
  filter(IP >= 90.00) -> WHHIP_SP
