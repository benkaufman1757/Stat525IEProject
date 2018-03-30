library(ggplot2)
library(GGally)
library(car)
library(ggpmisc)
library(MASS)
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

mod3 <- lm( WAGE ~ EDUCATION + SEX, data = wages)
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

# Added Variable Plot, include sex
mod5 <- lm( WAGE ~ EDUCATION + SEX, data = wages)
mod6 <- lm( EXPERIENCE ~ EDUCATION + SEX, data = wages)
resid_wg_exp <- resid(mod5)
resid_exp_edu <- resid(mod6)
avp_ed_ex <- data.frame(resid_wg_exp, resid_exp_edu)
head(avp_ed_ex)

p_added_v_edu_exp <- ggplot( avp_ed_ex, aes ( x = resid_exp_edu, y = resid_wg_exp) ) 
p_added_v_edu_exp + geom_point() +
  geom_smooth(data=wages, method='lm',formula=y~x,color="pink")

ggpairs(wages, columns = c("EDUCATION", "WAGE"), aes(colour=SEX))

ggpairs(wages,columns = c("EDUCATION", "WAGE"), lower = list(continuous=wrap("points", position="jitter")),aes(colour=SEX))

mod7 <- lm( WAGE ~ EDUCATION + EXPERIENCE + SEX, data = wages)
summary(mod7)
summary(mod5)$r.squared
summary(mod4)$r.squared


mod8 <- lm( resid_wg_exp ~ resid_exp_edu, data = avp_ed_ex)
summary(mod8)

m1 <- lm( WAGE ~ EDUCATION + SEX, data = wages)
m2 <- lm( WAGE ~ EDUCATION + SEX + EXPERIENCE, data = wages)
coef(m1)
coef(m2)
summary(m1)
summary(m2)
summary(lm( WAGE ~ EDUCATION + SEX, data = wages))

confint(m2)

temp <- lm( WAGE ~ EDUCATION, data = men)
summary(temp)
coef(temp) 
coef(M1)
M1 <- lm( WAGE ~ EDUCATION + SEX, data = wages)
M2 <- lm( WAGE ~ EDUCATION + SEX + SEX:EDUCATION, data = wages)
M3 <- lm( EXPERIENCE ~ EDUCATION + SEX, data = wages)
M4 <- lm( WAGE ~ EDUCATION + SEX + EXPERIENCE, data = wages)
M5 <- lm( WAGE ~ EXPERIENCE + SEX, data = wages)

p_ed_wg <- ggplot( wages, aes ( x = EDUCATION, y = WAGE) ) 
p_ed_wg + geom_jitter(aes(color=SEX)) + 
  scale_color_manual(name = 'SEX', values = c("blue", "pink")) +
  geom_abline ( intercept = coef(M1)[1], slope = coef(M1)[2] ,color="red") + 
  geom_abline ( intercept = coef(M2)[1], slope = coef(M2)[2], color="blue" )

p_ex_wg <- ggplot( wages, aes ( x = EXPERIENCE, y = WAGE) ) 
p_ex_wg + geom_jitter(aes(color=SEX)) + 
  geom_abline ( intercept = coef(M5)[1], slope = coef(M5)[2] ,color="black")

summary(M1)
summary(M2)
summary(M3)
summary(M4)
summary(M5)

coef(M1)
coef(M2)
coef(M3)
coef(M4)

confint(M1)
confint(M2)
0.2532

p_added_v_edu_exp <- ggplot( ) 
p_added_v_edu_exp + geom_point(x = resid(M3), y = resid(M4)) 


resid_wg_edu <- resid(M1)
resid_exp_edu <- resid(M3)

avp_ed_ex <- data.frame(resid_wg_edu, resid_exp_edu)
temp <- lm( resid_wg_edu ~ resid_exp_edu, data = avp_ed_ex)
p_added_v_edu_exp <- ggplot( avp_ed_ex, aes ( x = resid_exp_edu, y = resid_wg_exp) ) 
p_added_v_edu_exp + geom_point() +
  #geom_smooth(data=wages, method='lm',formula=y~x,color="pink") +
  geom_abline ( intercept = coef(temp)[1], slope = coef(temp)[2], color="blue" ) +
  ggtitle("Added Variable Plot for Experience") +
  xlab("Residuals of E[Experience|Education,Sex]") +
  ylab("Residuals of E[Wage|Education,Sex]")
coef(temp)
coef(M4)

summary(M1)$r.squared
summary(M4)$r.squared
summary(temp)$r.squared


library(dplyr)
set.seed(1)
wages %>% 
  group_by(OCCUPATION) %>%
  summarise(no_rows = length(OCCUPATION))

professional <- subset(wages, OCCUPATION=="Professional")
M7 <- lm( WAGE ~ EDUCATION + EXPERIENCE + SEX, data = professional)
summary(M7)
confint(M7)

p <- ggplot( , aes ( x = EDUCATION, y = WAGE) )
p + geom_jitter(aes(color=SEX))

M8 <- lm( WAGE ~ EDUCATION + EXPERIENCE + OCCUPATION +EXPERIENCE:OCCUPATION  + SEX, data = wages)
summary(M8)
confint(M8)
confint(M1)
confint(M4)

ggpairs(wages,columns = c("EDUCATION", "WAGE"), 
        lower = list(continuous=wrap("points", position="jitter"), 
        diag = list(discrete = wrap("barDiag"))), 
        aes(colour=SEX)) + 
  ggtitle("Marginal Plot of Wage vs Education")
ggpairs(wages,columns = c("EDUCATION", "WAGE"), 
        lower = list(continuous=wrap("points", position="jitter")), 
        aes(colour=SEX)) + 
  ggtitle("Marginal Plot of Wage vs Education")

formula <- y ~ x
p_ed_wg <- ggplot( wages, aes ( x = EDUCATION, y = WAGE) ) 
p_ed_wg + geom_jitter(aes(color=SEX)) + 
  geom_smooth(data=women, method='lm',formula=y~x,color="pink") +
  geom_smooth(data=men, method='lm',formula=y~x,color="blue") + 
  scale_color_manual(name = 'SEX', values = c("blue", "pink")) +
  stat_poly_eq(data=women,parse=T, aes(label = ..eq.label..),eq.with.lhs = "hat(y)[F]~`=`~", formula=y~x) + 
  stat_poly_eq(data=men,parse=T, aes(label = ..eq.label..),eq.with.lhs = "hat(y)[M]~`=`~", formula=y~x,label.y.npc = 0.85)
        

m1 <- lm( WAGE ~ EDUCATION, data = men)
m2 <- lm( WAGE ~ EDUCATION, data = women)

summary(m2)

avp_df <- data.frame(resid(M1),resid(M3))
avp_ed_ex <- ggplot( avp_df, aes ( x = resid.M3., y = resid.M1.) ) 
avp_ed_ex + geom_point()

p_added_v_edu_exp + geom_point() +
  #geom_smooth(data=wages, method='lm',formula=y~x,color="pink") +
  geom_abline ( intercept = coef(temp)[1], slope = coef(temp)[2], color="blue" ) +
  ggtitle("Added Variable Plot for Experience") +
  xlab("hat(e)(E[Experience|Education,Sex])") +
  ylab("Residuals of E[Wage|Education,Sex]")


ggpairs(wages[c(1,2,3,4,5,6,7,11)])
head(wages)

temp <- lm( log(WAGE) ~ AGE, data = men)
p <- ggplot( men, aes ( x = EXPERIENCE + EDUCATION, y = WAGE) ) 
p <- ggplot( men, aes ( x = AGE, y = WAGE) ) 
p+geom_point() + geom_smooth()
temp2 <- lm( WAGE ~ AGE, data = men)
p <- ggplot( men, aes ( x = AGE, y = log(WAGE)) ) 
p+geom_point() + geom_abline ( intercept = coef(temp)[1], slope = coef(temp)[2], color="blue" )

summary(temp)
summary(temp2)


temp <- lm( log(WAGE) ~ AGE, data = professional)
service = subset(wages,OCCUPATION=="Service")
p <- ggplot( service , aes ( x = EDUCATION, y = WAGE) ) 
p+geom_point(aes(color=SEX)) + 
  geom_smooth(data=subset(service,SEX=="Female"),method="lm",color="red") + 
  geom_smooth(data=subset(service,SEX=="Male"),method="lm")

nrow(subset(wages,OCCUPATION=="Service"))
confint(M2)


avp_ed_ex <- data.frame(resid_wg_edu, resid_exp_edu)
ex_avp_mod <- lm(resid_wg_edu ~ log(resid_exp_edu), data = avp_ed_ex)
p_added_v_edu_exp <- ggplot( avp_ed_ex, aes ( x = resid_exp_edu, y = resid_wg_exp) ) 
p_added_v_edu_exp + geom_point() +
  geom_smooth(data=wages, method='lm',formula=y~x,color="pink") +
  #geom_abline ( intercept = coef(ex_avp_mod)[1], slope = coef(ex_avp_mod)[2], color="blue" ) +
  ggtitle("Added Variable Plot for Experience") +
  xlab(expression(paste(hat(e), "(E[Experience|Education,Sex])"))) +
  ylab(expression(paste(hat(e), "(E[Wage|Education,Sex])")))

temp <- data.frame(resid_wg_edu, fitted(M1))
p <- ggplot( temp, aes ( x = fitted.M1., y = resid_wg_edu) ) 
p + geom_point()

######## MINCER EARNINGS FUNCTION ######
wages$EXP2 <- wages$EXPERIENCE^2
mincer <- lm( log(WAGE) ~ EDUCATION + EXPERIENCE + EXP2 , data = wages)
summary(mincer)

sex_mincer <- lm( log(WAGE) ~ EDUCATION + EXPERIENCE + EXP2 + SEX , data = wages)
summary(sex_mincer)
confint(sex_mincer)

temp <- lm(log(WAGE) ~ EDUCATION + SEX, data = wages)
p <- ggplot( wages, aes ( x = EXPERIENCE, y = log(WAGE)) )
p + geom_point()
summary(temp)

temp2 <- lm(WAGE ~ EDUCATION + SEX, data = wages)
summary(temp2)


temp <- summary(M2)
temp$coefficients[4,4] 

sex_occ_mincer <- lm( log(WAGE) ~ EDUCATION + EXPERIENCE + EXP2 + SEX + OCCUPATION , data = wages)
summary(sex_occ_mincer)

## EXPERIENCE AVP FOR log(WAGE) w/ M1

Ml1 <- lm( log(WAGE) ~ EDUCATION + SEX, data = wages)
Ml2 <- lm( log(WAGE) ~ EDUCATION + SEX + EXPERIENCE, data = wages)
resid_wg_edu <- resid(Ml1)
resid_exp_edu <- resid(M3)
fitted_wg_edu <- fitted(Ml1)

avp_ed_ex <- data.frame(resid_wg_edu, resid_exp_edu, fitted_wg_edu)
ex_avp_mod <- lm(resid_wg_edu ~ resid_exp_edu, data = avp_ed_ex)

f_vs_rs <- ggplot( avp_ed_ex, aes ( x = fitted_wg_edu, y = resid_wg_edu) ) 
f_vs_rs + geom_point() +
  xlab(expression(hat(y))) + 
  ylab(expression(paste(hat(e), "(M1)")))

p_added_v_edu_exp <- ggplot( avp_ed_ex, aes ( x = resid_exp_edu, y = resid_wg_edu) ) 
p_added_v_edu_exp + geom_point() +
  geom_smooth(data=wages, method='lm',formula=y~x,color="pink") +
  geom_abline ( intercept = coef(ex_avp_mod)[1], slope = coef(ex_avp_mod)[2], color="blue" ) +
  ggtitle("Added Variable Plot for Experience") +
  xlab(expression(paste(hat(e), "(M3)"))) +
  ylab(expression(paste(hat(e), "(M1)")))

summary(ex_avp_mod)$r.squared
summary(Ml1)
summary(M1)
summary(Ml2)
summary(M2)

####

ex <- lm( WAGE ~ EXPERIENCE + SEX, data = wages)
exl <- lm( log(WAGE) ~ EXPERIENCE + SEX, data = wages)
summary(ex)
summary(exl)

#### Table
wages %>% 
  group_by(OCCUPATION) %>%
  summarise(total = length(OCCUPATION),
            men = length(subset(OCCUPATION,SEX=="Male")),
            women = length(subset(OCCUPATION,SEX=="Female")))




##################### ADDING OCCUPATION
wages <- within(wages, OCCUPATION <- relevel(OCCUPATION, ref = 6))
levels(wages$OCCUPATION)

relevel(wages$OCCUPATION, ref = 6)
M5 <- lm( WAGE ~ EDUCATION + SEX + EXPERIENCE + OCCUPATION, data = wages)
summary(M5)

Ml5 <- lm( log(WAGE) ~ EDUCATION + SEX + EXPERIENCE + OCCUPATION, data = wages)
summary(Ml5)

summary(M1)
summary(M4)


## do mincer with + sex + sex*education + sex*experience 

####### Fitted vs residuals

resid_M1 <- resid(M1)
fitted_M1 <- fitted(M1)

df_M1 <- data.frame(resid_M1, fitted_M1)

f_vs_rs_M1 <- ggplot( df_M1, aes ( x = fitted_M1, y = resid_M1) ) 
f_vs_rs_M1 + geom_point() +
  xlab(expression(hat(y))) + 
  ylab(expression(paste(hat(e), "(M1)")))

###### Transformations

fitted_M4 <- fitted(M4)
temp <- data.frame(fitted_M4,wages$WAGE)
inv_fitt_value_M1 <- ggplot( temp, aes ( x =wages.WAGE , y = fitted_M4) )
inv_fitt_value_M1 + geom_point()

# geometric mean
gmY <- exp(mean(log(wages$WAGE)))
boxcox(M4)





