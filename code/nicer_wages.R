library(ggplot2)
library(GGally)
projpath <- "~/Documents/Academics/Stat525/Project/"
setwd(projpath)

# create dataframe
wages <- read.csv(file = "data/cpswages.dat.txt", header = FALSE, sep=" ")
names(wages) <- c("EDUCATION",	"SOUTH", "SEX", "EXPERIENCE", "UNION",  "WAGE", "AGE", "RACE", "OCCUPATION", "SECTOR", "MARR")
wages = wages[,c(1,2,3,4,5,7,8,9,10,11,6)]
head(wages)

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

wages$MANUFACTURING <- 1*(wages$SECTOR == 'Manufacturing')
wages$CONSTRUCTION <- 1*(wages$SECTOR == 'Construction')

wages$MANAGEMENT <- 1*(wages$OCCUPATION == 'Management')
wages$SALES <- 1*(wages$OCCUPATION == 'Sales')
wages$CLERICAL <- 1*(wages$OCCUPATION == 'Clerical')
wages$SERVICE <- 1*(wages$OCCUPATION == 'Service')
wages$PROFESSIONAL <- 1*(wages$OCCUPATION == 'Professional')

wages$WHITE <- 1*(wages$RACE == 'White')
wages$HISPANIC <- 1*(wages$RACE == 'Hispanic')

women <- subset(wages,SEX=='Female')
men <- subset(wages,SEX=='Male')
######################### IS THERE A WAGE GAP BETWEEN M/F? #############
p <- ggplot( wages, aes ( x = SEX, y = WAGE) )
p + geom_boxplot() + ggtitle("Wage vs Sex")

nrow(women)
nrow(men)
t.test(men$WAGE, women$WAGE, alternative = "greater")

######################### WHY IS THERE A WAGE GAP? #############


######################### HYPOTHESIS: MEN ARE BETTER EDUCATED, SHOULD EARN MORE #############
mod1 <- lm( WAGE ~ EDUCATION, data = men)
coef(mod1)

mod2 <- lm( WAGE ~ EDUCATION, data = women)
coef(mod2)

mod3 <- lm( WAGE ~EDUCATION + SEX, data = wages)
coef(mod3)

mod4 <- lm( WAGE ~EDUCATION, data = wages)
coef(mod4)

p_ed_wg <- ggplot( wages, aes ( x = EDUCATION, y = WAGE) ) 
p_ed_wg + geom_jitter(aes(color=SEX)) + 
  scale_color_manual(name = 'SEX', values = c("blue", "pink")) 

p_ed_wg_counts <- ggplot(wages, aes(EDUCATION, fill = SEX)) 
p_ed_wg_counts + geom_histogram(position = 'dodge') +
  scale_fill_manual(values=c("blue", "pink"))

t.test(subset(men,EDUCATION==12)$WAGE,subset(women,EDUCATION==12)$WAGE)

# Do the confidence intervals overlap? 
# Does the wage gap decrease as level of education increases?
p_ed_wg + geom_jitter(aes(color=SEX)) + 
  geom_smooth(data=women, method='lm',formula=y~x,color="pink") +
  geom_smooth(data=men, method='lm',formula=y~x,color="blue") + 
  scale_color_manual(name = 'SEX', values = c("blue", "pink"))

p_ed_wg + geom_jitter() + 
  geom_smooth(data=wages, method='lm',formula=y~x,color="black")





