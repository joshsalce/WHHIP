library(tidyverse)
library(dplyr)

WHHIP = read.csv(file.choose())
WHHIP.v2 = read.csv(file.choose())

## All pitchers at least 30 innings pitched
# No position players
WHHIP %>%
  filter(IP >= 30.00) -> WHHIP_filt

WHHIP.v2 %>%
  filter(IP >= 30.00) -> WHHIP.v2_filt

### Avg. WHHIP (ALL)
avg_WHHIP = round((sum(WHHIP_filt$BB)+ sum(WHHIP_filt$Hard.Hits) )/(sum(WHHIP_filt$IP)), 3)
avg_WHHIP
      
# Average WHHIP: 0.888

hist(WHHIP_filt$WHHIP,
     main = "WHHIP for All Pitchers (10 IP or more)", 
     xlab = "WHHIP (Average = 0.888)",           
     ylab = "Frequency",
     col = 3,
     breaks = 22) +
  abline(v = 0.888, col = "black", lwd = 3)



### Relievers
WHHIP_filt %>%
  filter(IP <= 90.00) -> WHHIP_RP

### Avg. WHHIP (RP)
avg_WHHIP_RP = (sum(WHHIP_RP$BB)+ sum(WHHIP_RP$Hard.Hits) )/(sum(WHHIP_RP$IP))
avg_WHHIP_RP = round(avg_WHHIP_RP, 3)

# Average WHHIP RP: 0.922

hist(WHHIP_RP$WHHIP,
     main = "WHHIP for 'Relief' Pitchers (90 IP or less)", 
     xlab = "WHHIP (Average = 0.922)",           
     ylab = "Frequency",
     col = 2,
     breaks = 22) +
  abline(v = 0.922, col = "black", lwd = 3)



### Starters
WHHIP_filt %>%
  filter(IP >= 90.00) -> WHHIP_SP


### Avg. WHHIP (SP)
avg_WHHIP_SP = (sum(WHHIP_SP$BB)+ sum(WHHIP_SP$Hard.Hits) )/(sum(WHHIP_SP$IP))
avg_WHHIP_SP = round(avg_WHHIP_SP, 3)

# Average WHHIP SP: 0.858


hist(WHHIP_SP$WHHIP,
     main = "WHHIP for 'Starting' Pitchers (90 IP or more)", 
     xlab = "WHHIP (Average = 0.858)",           
     ylab = "Frequency",
     col = 4,
     breaks = 22) +
  abline(v = 0.858, col = "black", lwd = 3)

### WHHIP Correlation to ERA
library(ggplot2)

WHHIP_ERA_cor = round(cor(WHHIP_filt$WHHIP, WHHIP.v2_filt$ERA), 2)

plot(WHHIP_filt$WHHIP, WHHIP.v2_filt$ERA, 
                  xlab="WHHIP", 
                  ylab = "ERA",
                  main = "Correlation Plot from WHHIP to ERA",
                  pch = 16)
Model <- lm(WHHIP.v2_filt$ERA ~ WHHIP_filt$WHHIP)
abline(Model, col = "red", lwd = 3)
legend("topleft",legend=paste("R^2 is", format(summary(Model)$r.squared,digits=3)))
legend(x='bottomright', legend=paste('Cor =',WHHIP_ERA_cor))



# WHHIP to FIP
WHHIP_FIP_cor = round(cor(WHHIP.v2_filt$FIP, WHHIP_filt$WHHIP), 2)

plot(WHHIP_filt$WHHIP, WHHIP.v2_filt$FIP, 
     xlab="WHHIP", ylab = "FIP", 
     main = "Correlation Plot from WHHIP to FIP",
     pch = 16)

Model5 <- lm(WHHIP.v2_filt$FIP ~ WHHIP_filt$WHHIP)
abline(Model5, col = "turquoise")
legend("topleft",legend=paste("R^2 is", format(summary(Model5)$r.squared,digits=3)))
legend(x='bottomright', legend=paste('Cor =',WHHIP_FIP_cor))