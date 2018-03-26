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

# scatterplot matrix
#vars <- c ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 )
#ggpairs ( wages, columns = vars )


# SEX specific graphs

################################  EDUCATION WAGE SEX  ##########################################
p_ed_wg <- ggplot( wages, aes ( x = EDUCATION, y = WAGE) ) + geom_jitter(aes(color=SEX))
p_ed_wg + scale_color_manual(name = 'SEX', values = c("blue", "pink")) 

mod <- lm( WAGE ~ EDUCATION, data = wages)

p_edu_wg <- ggplot( wages, aes ( x = EDUCATION, y = WAGE) ) + geom_jitter(aes(color=SEX)) + 
  geom_smooth(data=subset(wages, SEX == 'Female'), method='lm',formula=y~x,color="pink") +
  geom_smooth(data=subset(wages, SEX == 'Male'), method='lm',formula=y~x,color="blue") + 
  geom_smooth(method='lm',color="black") + 
  labs(title = paste("Adj R2 = ",signif(summary(mod)$adj.r.squared, 5),
                     "Intercept =",signif(mod$coef[[1]],5 ),
                     " Slope =",signif(mod$coef[[2]], 5)))

p_edu_wg + scale_color_manual(name='SEX', values = c("blue", "pink"))
  
################################  EXPERIENCE WAGE SEX  ##########################################

p_exp_wg <- ggplot( wages, aes ( x = EXPERIENCE, y = WAGE) ) + geom_point(aes(color=SEX))
p_exp_wg + scale_color_manual(name = 'SEX', values = c("blue", "pink"))


mod <- lm( WAGE ~ EXPERIENCE, data = wages)

p_exp_wg <- ggplot( wages, aes ( x = EXPERIENCE, y = WAGE) ) + geom_point(aes(color=SEX)) + 
  geom_smooth(data=subset(wages, SEX == 'Female'),method="lm", formula=y~x,color="pink",fill="pink") +
  geom_smooth(data=subset(wages, SEX == 'Male'),method="lm",formula=y~x,color="blue",fill="blue") + 
  geom_smooth(method='lm',color="black") + 
  facet_grid ( SECTOR ~ OCCUPATION ) + 
  #facet_wrap( SECTOR ~ OCCUPATION) +
  labs(title = paste("Adj R2 = ",signif(summary(mod)$adj.r.squared, 5),
                     "Intercept =",signif(mod$coef[[1]],5 ),
                     " Slope =",signif(mod$coef[[2]], 5)))

p_exp_wg + scale_color_manual(name='SEX', values = c("blue", "pink"))


################################  SECTOR WAGE UNION  ##########################################

p <-ggplot(wages, aes(x=SECTOR, y=WAGE)) + 
  geom_jitter( aes(color=factor(UNION)),position=position_jitter(width=.2, height=0))
p + scale_color_manual(name = 'UNION STATUS',labels = c("Not-Unionized", "Unionized"), values = c("red", "blue")) +
  xlab("SECTOR")

################################  RACE  ##########################################

sum(wages$RACE == 'White') / nrow(wages)

sum(wages$RACE == 'Hispanic') / nrow(wages)

sum(wages$RACE == 'Other') / nrow(wages)


# boxplots
p <- ggplot( wages, aes ( x = SEX, y = WAGE) )
p + geom_boxplot() + ggtitle("Wage by Sex")

mod <- lm( WAGE ~ EDUCATION + EXPERIENCE + UNION + AGE, data = wages)
summary(mod)

p_exp_wg <- ggplot( wages, aes ( x = EXPERIENCE, y = EDUCATION, z = WAGE) ) + geom_point(aes(color=factor(SEX)))
p_exp_wg + scale_color_manual(labels = c("Female", "Male"), values = c("pink", "blue"))

# use this to cycle through all subsets of factors
lapply(0:4, function(x) combn(4,x))

library(ggm)

head(wages[,c(1,2,3,4,5,7,8,9,10,11,6)])

wages = wages[,c(1,2,3,4,5,7,8,9,10,11,6)]

ps <- powerset(names(wages)[c(1,2,3,4,5,6,11,12,13,14,15,16,17,18,19,20)]) 
max_r_sqrd <- -1
max_mod <- NULL
for (ss in ps) {
  regressors <- paste(ss, collapse ="+")
  dat <- paste("WAGE ~ ", regressors, sep="")
  #print(dat)
  tempmod <- lm(as.formula(dat) , data = wages)
  mod_r2 <- summary(tempmod)$r.squared 
  if (mod_r2 > max_r_sqrd) {
    max_r_sqrd <- mod_r2
    max_mod <- tempmod
  }
}



ps <- powerset(c("A","B","C")) 
for (ss in ps) {
  print(paste(ss, sep ="+"))
}

p <- ggplot( wages, aes ( x = as.factor(SOUTH), y = EDUCATION) )
p + geom_boxplot() 

p_sec_wg_un <- ggplot( wages, aes ( x = SECTOR, y = WAGE) ) + geom_jitter(aes(color=factor(UNION)))
p_sec_wg_un + scale_color_manual(labels = c("Not-Unionized", "Unionized"), values = c("red", "blue")) 

p_sec_wg_vi <-ggplot( wages, aes ( x = SECTOR, y = WAGE) )
p_sec_wg_vi + geom_violin(aes(fill = factor(UNION)))

p <-ggplot( wages, aes ( x = EDUCATION, y = WAGE) )
p + geom_violin(aes(name='blah',fill = factor(UNION)))


p_sec_wg_vi <-ggplot( wages, aes ( x = MARR, y = EXPERIENCE) )
p_sec_wg_vi + geom_violin(aes(fill = factor(MARR)))

p_sec_wg_vi <- ggplot( wages, aes ( x = as.factor(EDUCATION), y = WAGE) )
p_sec_wg_vi + geom_violin() + geom_point() 

p <-ggplot(wages, aes(x=EDUCATION, y=WAGE))
p  + geom_jitter(position=position_jitter(width=.2, height=0)) + xlab("EDUCATION")



mod <- lm ( WAGE ~ EDUCATION, data = wages )
summary(mod)

p <-ggplot(wages, aes(x=EDUCATION, y=WAGE))
p  + geom_jitter(position=position_jitter(width=.2, height=0)) + xlab("EDUCATION") +
  geom_abline ( intercept = coef(mod)[1], slope = coef(mod)[2] )

head(wages)
mod <- lm ( WAGE ~ EDUCATION + SOUTH + SEX + EXPERIENCE + UNION + AGE + WHITE + HISPANIC + , data = wages )