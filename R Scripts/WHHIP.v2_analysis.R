UpUplibrary(tidyverse)
library(dplyr)

WHHIP.v2 = read.csv(file.choose())

WHHIP.v2 %>%
  filter(IP >= 30.00) -> WHHIP.v2_filt

### Avg. WHHIP.v2 (ALL)
avg_WHHIP.v2 = (sum(WHHIP.v2_filt$BB)+ sum(WHHIP.v2_filt$Hard.Hits) )/ (sum(WHHIP.v2_filt$IP))
avg_WHHIP.v2 = round(avg_WHHIP.v2, 3) 

# Average WHHIP: 1.441


hist(WHHIP.v2_filt$WHHIP,
     main = "WHHIP.v2 for All Pitchers (10 IP or more)", 
     xlab = "WHHIP.v2 (Average = 1.441)",           
     ylab = "Frequency",
     col = 5,
     breaks = 22) +
  abline(v = 1.441, col = "black", lwd = 4)



### Relievers
WHHIP.v2_filt %>%
  filter(IP <= 90.00) -> WHHIP.v2_RP

### Avg. WHHIP (RP)
avg_WHHIP.v2_RP = (sum(WHHIP.v2_RP$BB)+ sum(WHHIP.v2_RP$Hard.Hits) )/(sum(WHHIP.v2_RP$IP))
avg_WHHIP.v2_RP = round(avg_WHHIP.v2_RP, 3)

# Average WHHIP RP: 1.470


hist(WHHIP.v2_RP$WHHIP,
     main = "WHHIP.v2 for 'Relief' Pitchers (90 IP or less)", 
     xlab = "WHHIP.v2 (Average = 1.474)",           
     ylab = "Frequency",
     col = 6,
     breaks = 22) +
  abline(v = avg_WHHIP.v2_RP, col = "black", lwd = 4)


### Starters
WHHIP.v2_filt %>%
  filter(IP >= 90.00) -> WHHIP.v2_SP


### Avg. WHHIP (SP)
avg_WHHIP.v2_SP = (sum(WHHIP.v2_SP$BB)+ sum(WHHIP.v2_SP$Hard.Hits) )/(sum(WHHIP.v2_SP$IP))
avg_WHHIP.v2_SP = round(avg_WHHIP.v2_SP, 3)

# Average WHHIP SP: 1.416


hist(WHHIP.v2_SP$WHHIP,
     main = "WHHIP.v2 for 'Starting' Pitchers (90 IP or more)", 
     xlab = "WHHIP.v2 (Average = 1.42)",           
     ylab = "Frequency",
     col = 7,
     breaks = 22) +
  abline(v = avg_WHHIP.v2_SP, col = "black", lwd = 4)


##### Correlation Graphs
library(ggplot2)

# Correlation of WHHIP.v2 to ERA
WHHIP.v2_ERA_cor = round(cor(WHHIP.v2_filt$WHHIP, WHHIP.v2_filt$ERA), 2)

plot(WHHIP.v2_filt$WHHIP, WHHIP.v2_filt$ERA, 
     xlab="WHHIP.v2", ylab = "ERA", 
     main = "Correlation Plot from WHHIP.v2 to ERA",
     pch = 16)

Model2 <- lm(WHHIP.v2_filt$ERA ~ WHHIP.v2_filt$WHHIP)
abline(Model2, col = "purple")
legend("topleft",legend=paste("R^2 is", format(summary(Model2)$r.squared,digits=3)))
legend(x='bottomright', legend=paste('Cor =',WHHIP.v2_ERA_cor))



# Correlation of WHHIP.v2 to FIP
WHHIP.v2_FIP_cor = round(cor(WHHIP.v2_filt$WHHIP, WHHIP.v2_filt$FIP), 2)

plot(WHHIP.v2_filt$WHHIP, WHHIP.v2_filt$FIP, 
     xlab="WHHIP.v2", ylab = "FIP", 
     main = "Correlation Plot from WHHIP.v2 to FIP",
     pch = 16)

Model4 <- lm(WHHIP.v2_filt$FIP ~ WHHIP.v2_filt$WHHIP)
abline(Model4, col = "brown")
legend("topleft",legend=paste("R^2 is", format(summary(Model4)$r.squared,digits=3)))
legend(x='bottomright', legend=paste('Cor =',WHHIP.v2_FIP_cor))


# WHHIP.v2 to SIERA
WHHIP.v2_SIERA_cor = round(cor(WHHIP.v2_filt$WHHIP, WHHIP.v2_filt$SIERA), 2)

plot(WHHIP.v2_filt$WHHIP, WHHIP.v2_filt$SIERA, 
     xlab="WHHIP.v2", ylab = "SIERA", 
     main = "Correlation Plot from WHHIP.v2 to SIERA",
     pch = 16)

Model8 <- lm(WHHIP.v2_filt$SIERA ~ WHHIP.v2_filt$WHHIP)
abline(Model8, col = "salmon")
legend("topleft",legend=paste("R^2 is", format(summary(Model8)$r.squared,digits=3)))
legend(x='bottomright', legend=paste('Cor =',WHHIP.v2_SIERA_cor))


################################################################################




# Correlation of WHIP to ERA
WHIP_ERA_cor = round(cor(WHHIP.v2_filt$WHIP, WHHIP.v2_filt$ERA), 2)

plot(WHHIP.v2_filt$WHIP, WHHIP.v2_filt$ERA, 
     xlab="WHIP", ylab = "ERA", 
     main = "Correlation Plot from WHIP to ERA",
     pch = 16)

Model3 <- lm(WHHIP.v2_filt$ERA ~ WHHIP.v2_filt$WHIP)
abline(Model3, col = "orange")
legend("topleft",legend=paste("R^2 is", format(summary(Model3)$r.squared,digits=3)))
legend(x='bottomright', legend=paste('Cor =',WHIP_ERA_cor))


#WHIP to FIP
WHIP_FIP_cor = round(cor(WHHIP.v2_filt$WHIP, WHHIP.v2_filt$FIP), 2)

plot(WHHIP.v2_filt$WHIP, WHHIP.v2_filt$FIP, 
     xlab="WHIP", ylab = "FIP", 
     main = "Correlation Plot from WHIP to FIP",
     pch = 16)

Model6 <- lm(WHHIP.v2_filt$FIP ~ WHHIP.v2_filt$WHIP)
abline(Model6, col = "forestgreen")
legend("topleft",legend=paste("R^2 is", format(summary(Model6)$r.squared,digits=3)))
legend(x='bottomright', legend=paste('Cor =',WHIP_FIP_cor))



#WHIP to SIERA
WHIP_SIERA_cor = round(cor(WHHIP.v2_filt$WHIP, WHHIP.v2_filt$SIERA), 2)

plot(WHHIP.v2_filt$WHIP, WHHIP.v2_filt$SIERA, 
     xlab="WHIP", ylab = "SIERA", 
     main = "Correlation Plot from WHIP to SIERA",
     pch = 16)

Model7 <- lm(WHHIP.v2_filt$SIERA ~ WHHIP.v2_filt$WHIP)
abline(Model7, col = "lightblue")
legend("topleft",legend=paste("R^2 is", format(summary(Model7)$r.squared,digits=3)))
legend(x='bottomright', legend=paste('Cor =',WHIP_SIERA_cor))


