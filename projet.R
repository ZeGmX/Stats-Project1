setwd("~/Cours/Stats/Projet1")

t2012 <- read.csv("2012.csv", header=T, sep=";", dec=",")
t2013 <- read.csv("2013.csv", header=T, sep=";", dec=,"")
t2014 <- read.csv("2014.csv", header=T, sep=";", dec=",")

nrow(t2012)
nrow(t2013)
nrow(t2014)

length(t2012)
length(t2013)
length(t2014)

summary(t2012)
summary(t2013)
summary(t2014)

names(t2012) == names(t2013)
names(t2013) == names(t2014)

### Q1 ###

library(dplyr)
t <- t2014 %>%
  filter(lib_mrq != "MERCEDES")
 #filter(is.na(conso_exurb) == F)  %>%
  #filter(hybride == "non")

cor.test(t$conso_exurb, t$masse_ordma_max)

plot(t$masse_ordma_max, t$conso_exurb)
abline(line(t$masse_ordma_max, t$conso_exurb))


library(ggplot2)
gg <- ggplot(t, aes(x=masse_ordma_max, y=conso_exurb)) + geom_point(aes(col=gamme)) +
  geom_smooth(method="lm") +
  labs(title="Corrélation entre la masse d'une voiture et sa consommation extra urbaine") +
  xlab("Masse (kg)") + ylab("Consommation extra urbaine (l/100km)") 
plot(gg)
abline(line(t$masse_ordma_max, t$conso_exurb))





### Q2 ###

#Inferieur et luxe

tinf <- t %>%
  filter(gamme == "INFERIEURE")
nrow(tinf)

tluxe <- t %>%
  filter(gamme == "LUXE")
nrow(tluxe)

t2 <- t %>% 
  filter(gamme == "LUXE" | gamme == "INFERIEURE")

shapiro.test(tinf$co2)
shapiro.test(tluxe$co2)

wilcox.test(tinf$co2, tluxe$co2)

gg <- ggplot(t2, aes(x=gamme, y=co2, fill=gamme, colour=gamme)) +
  geom_boxplot(alpha=0.5, outlier.alpha=0, colour="black") +
  geom_jitter(fill="black")  +
  labs(title="Rejet de CO2 pour les véhicules de gamme inférieure et de gamme luxueuse") +
  xlab("Gamme") + ylab("Rejet de C02 (g/km)")
  
plot(gg)

