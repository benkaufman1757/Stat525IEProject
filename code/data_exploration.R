library(ggplot2)
library(GGally)
library(RColorBrewer)
projpath <- "~/Documents/Academics/Stat525/Project/"
setwd(projpath)

pollution <- read.csv ( "data/NO2.csv" )
head(pollution)

pollution$Time_In_Hours <- pollution$Hour + (24*pollution$Day)

p <- ggplot( pollution, aes ( x = Time_In_Hours, y = log_NO2) )
pollution$R_WDir <- as.numeric(pollution$R_WDir)
pollution$G_WDir <- as.numeric(pollution$G_WDir)
pollution$B_WDir <- as.numeric(pollution$B_WDir)

pollution$WDir_Color <- rgb(pollution$R_WDir/255,pollution$G_WDir/255,pollution$B_WDir/255)

p+geom_point(colour=pollution$WDir_Color)

vars <- c ( 2, 3, 4, 5, 6, 7, 8, 1 )

ggpairs ( pollution, columns = vars )

p <- ggplot( pollution, aes ( x = log_carsperhour, y = log_NO2) )
p+geom_point(colour=pollution$WDir_Color)

mod <- lm( log_NO2 ~ log_carsperhour, data = pollution)
p <- ggplot( pollution, aes ( x = log_carsperhour, y = log_NO2) )
p + geom_point() +
  geom_abline( intercept = coef(mod)[1], slope = coef(mod)[2])

pollution$resid <- resid(mod)
pollution$fit <- fitted(mod)
p.r <- ggplot(pollution , aes ( x = fit, y = resid) )
p.r + geom_point()



p <- ggplot( pollution, aes ( x = Hour, y = log_NO2) )
p + geom_point() 


p <- ggplot( pollution, aes ( x = Hour, y = log_carsperhour) )
p + geom_point() 


mod2<- lm( log_NO2 ~ log_carsperhour + Hour, data = pollution)
summary(mod2)

