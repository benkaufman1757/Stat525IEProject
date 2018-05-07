library ( ggplot2 )
library ( GGally )
library(plyr)
projpath <- "~/Documents/Academics/Stat525/"
setwd(projpath)

land <- read.csv ( "data/MinnLand.csv" )
head(land)

p <- ggplot( land, aes ( x = year , y = log(acrePrice)) )
p + geom_boxplot(aes(group=year))

land$yearFact <- as.factor(land$year)

mod <- lm(log(acrePrice) ~ yearFact, data = land)
summary(mod)

mod2 <- lm(log(acrePrice) ~ yearFact - 1, data = land)
summary(mod2)


dt <- ddply(land,~yearFact,summarise,mean=mean(log(acrePrice)),sd=sd(log(acrePrice)),n=length(log(acrePrice)))
dt$sd / sqrt(dt$n)


