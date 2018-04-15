library(ggplot2)
library(car)
library(GGally)

setwd("~/GitHub/Stat525IEProject/data")
wages <- read.csv(file = "cpswages.dat.txt", header = FALSE, sep=" ")
names(wages) <- c("EDUCATION",	"SOUTH", "SEX", "EXPERIENCE", "UNION",  "WAGE", "AGE", "RACE", "OCCUPATION", "SECTOR", "MARR")
wages = wages[,c(1,2,3,4,5,7,8,9,10,11,6)]

# Set up factors
wages$SOUTH <- factor(wages$SOUTH, levels=c(0,1),
                      labels=c('North','South'))

wages$SEX <- factor(wages$SEX, levels=c(0,1),
                    labels=c('Male','Female'))

wages$UNION <- factor(wages$UNION, levels=c(0,1),
                      labels=c('Non-Unionized','Unionized'))

wages$RACE <- factor(wages$RACE, levels=c(1,2,3),
                     labels=c('Other','Hispanic', 'White'))

wages$OCCUPATION <- factor(wages$OCCUPATION, levels=c(1,2,3,4,5,6),
                           labels=c('Management','Sales','Clerical','Service','Professional','Other'))

wages$SECTOR <- factor(wages$SECTOR, levels=c(0,1,2),
                       labels=c('Other','Manufacturing','Construction'))

wages$MARR <- factor(wages$MARR, levels=c(0,1),
                     labels=c('Unmarried','Married'))

ggpairs(wages)

wages$EXPERIENCE = wages$EXPERIENCE + 1

nonfactor = wages[,c("EDUCATION","AGE","WAGE","EXPERIENCE")]
lapply(nonfactor[,-1], range)

summary(powerTransform(nonfactor))

wages$logWage = log(wages$WAGE)

ggpairs(wages[,c("EDUCATION","AGE","WAGE","EXPERIENCE")])

p1 = ggplot(aes(x=log(WAGE), y=EDUCATION), data=wages)
p1 + geom_point() + facet_wrap(~OCCUPATION)

mod = lm(log(WAGE)~EDUCATION, data=wages)
summary(mod)

wages$resid1 = resid(mod)
wages$fit1 = fitted(mod)

p2 = ggplot(aes(x=fit1, y=resid1), data=wages)
p2 + geom_point(aes(color=SEX), data=wages) + facet_wrap(~OCCUPATION)

max ( hatvalues (mod) )

wages = subset(wages, hatvalues(mod) < 0.02)


summary(powerTransform(log(WAGE)~EDUCATION + EXPERIENCE+AGE, data=wages))


mod2 = lm(log(WAGE)~SEX + AGE + SEX:AGE, data=wages)
summary(mod2)

mod3 = lm(log(WAGE)~SEX + EXPERIENCE + UNION + WAGE + RACE + OCCUPATION + SECTOR + MARR, data=wages)
summary(mod3)

