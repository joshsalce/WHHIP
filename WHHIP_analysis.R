library(tidyverse)
library(dplyr)

WHHIP = read.csv(file.choose())


## All pitchers at least 10 innings pitched
# No position players
WHHIP %>%
  filter(IP >= 30.00) -> WHHIP_filt


hist(WHHIP_filt$WHHIP,
     main = "WHHIP for all Pitchers (10 IP or more)", 
     xlab = "WHHIP",           
     ylab = "Frequency",
     col = 3,
     breaks = 22) +
  abline(v = 0.888, col = "black", lwd = 3)


### Avg. WHHIP (ALL)
avg_WHHIP = (sum(WHHIP_filt$BB)+ sum(WHHIP_filt$Hard.Hits) )/(sum(WHHIP_filt$IP))
round(avg_WHHIP, 3) 

# Average WHHIP: 0.888



### Avg. WHHIP (RP)
avg_WHHIP_RP = (sum(WHHIP_RP$BB)+ sum(WHHIP_RP$Hard.Hits) )/(sum(WHHIP_RP$IP))
round(avg_WHHIP_RP, 3)

# Average WHHIP RP: 0.922



### Avg. WHHIP (SP)
avg_WHHIP_SP = (sum(WHHIP_SP$BB)+ sum(WHHIP_SP$Hard.Hits) )/(sum(WHHIP_SP$IP))
round(avg_WHHIP_SP, 3)

# Average WHHIP SP: 0.858

  

### Relievers
WHHIP_filt %>%
  filter(IP <= 90.00) -> WHHIP_RP

hist(WHHIP_RP$WHHIP,
     main = "WHHIP for 'Relief' Pitchers (90 IP or less)", 
     xlab = "WHHIP",           
     ylab = "Frequency",
     col = 2,
     breaks = 22) +
  abline(v = 0.922, col = "black", lwd = 3)


### Starters
WHHIP_filt %>%
  filter(IP >= 90.00) -> WHHIP_SP

hist(WHHIP_SP$WHHIP,
     main = "WHHIP for 'Starting' Pitchers (90 IP or more)", 
     xlab = "WHHIP",           
     ylab = "Frequency",
     col = 4,
     breaks = 22) +
  abline(v = 0.858, col = "black", lwd = 3)

### 1d Scatterplots

library(ggplot2)
ggplot(WHHIP_SP, aes(x = WHHIP, y = 1)) +  
  geom_jitter(height = 2) + 
  ylim(-1, 3) +  
  theme(axis.title.y = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +  
  coord_fixed(ratio = 0.03)

WHHIP_SP %>% filter(WHHIP <= 0.670) -> top10_SP


ggplot(WHHIP_RP, aes(x = WHHIP, y = 1)) +  
  geom_jitter(height = 1.5) + 
  ylim(-2, 5) +  
  theme(axis.title.y = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +  
  coord_fixed(ratio = 0.03) 

WHHIP_RP %>% filter(WHHIP <= 0.597) -> top10_RP
head(WHHIP_RP$WHHIP, 10)

