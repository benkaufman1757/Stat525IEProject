library(ggplot2)
library(GGally)
library(car)
library(ggpmisc)
library(MASS)
library(dplyr)
library(gvlma)
library ( splines )
projpath <- "~/Documents/Academics/Stat525/Project/"
setwd(projpath)

# create dataframe
wages <- read.csv(file = "data/cpswages.dat.txt", header = FALSE, sep=" ")
names(wages) <- c("EDUCATION",	"SOUTH", "SEX", "EXPERIENCE", "UNION",  "WAGE", "AGE", "RACE", "OCCUPATION", "SECTOR", "MARR")
wages = wages[,c(1,2,3,4,5,7,8,9,10,11,6)]
head(wages)
#wages <- wages[wages$WAGE<30,] # remove outlier

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

ggpairs(wages, columns = c("EDUCATION", "EXPERIENCE", "AGE", "SEX" ,"WAGE"), aes(colour=SEX))

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
M3 <- lm( EXPERIENCE ~ EDUCATION + SEX, data = wages)   # ERRORS NOT REALLY NORMALLY DISTRIBUTED
M4 <- lm( WAGE ~ EDUCATION + SEX + EXPERIENCE, data = wages)
M5 <- lm( log(WAGE) ~ EDUCATION + SEX + EXPERIENCE, data = wages)
M6 <- lm( log(WAGE) ~ EDUCATION + SEX*OCCUPATION + EXPERIENCE, data = wages)

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
  group_by(OCCUPATION,UNION) %>%
  summarise(no_rows = length(OCCUPATION))

wages %>% 
  group_by(OCCUPATION,SEX) %>%
  summarise(no_rows = length(OCCUPATION))

wages %>% 
  group_by(RACE,OCCUPATION) %>%
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
resid_wg_edu <- resid(M1)
resid_exp_edu <- resid(M3)
fitted_wg_edu <- fitted(M1)

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
wages <- within(wages, OCCUPATION <- relevel(OCCUPATION, ref = "Management"))
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
lambda <- 1/10
Y <- wages$WAGE
transformed <- gmY^(1-lambda)*((Y^lambda) -1) / lambda
hist(transformed)
hist(Y)
wages$TRANSFORMED <- transformed

summary(lm(TRANSFORMED~EDUCATION + SEX,wages))

summary(lm(log(WAGE)~EDUCATION + SEX,wages))

summary(powerTransform(M4))


##### AVP FOR EXP^2

temp1 <- lm( log(WAGE) ~ EDUCATION + SEX + EXPERIENCE, data = wages)
temp2 <- lm( EXP2 ~ EDUCATION + SEX + EXPERIENCE, data = wages)

resid1 <- resid(temp1)
resid2 <- resid(temp2)

tempdf <- data.frame(resid1, resid2)
p <- ggplot( tempdf, aes ( x = resid2, y = resid1) ) 
p + geom_point()

tempmod <- lm(resid1 ~ resid2, data=tempdf)
summary(tempmod)

tmp <- powerTransform ( cbind ( EDUCATION ) ~ 1, data = wages )
summary(tmp)

temp3 <- lm( log(WAGE) ~ EDUCATION + SEX + EXPERIENCE + EXP2 + OCCUPATION, data = wages)
summary(temp3)
avPlots(temp3)
residualPlot(temp3)
residualPlots(temp3)

temp4 <- lm( log(WAGE) ~ EDUCATION*SEX , data = wages)
summary(temp4)

residualPlot()

####### Oaxaca Decomposition
male_ef <- lm(WAGE ~ EDUCATION, data=men)
female_ef <- lm(WAGE ~ EDUCATION, data=women)

coef(male_ef)
coef(female_ef)
mean(men$EDUCATION)
mean(women$EDUCATION)
# This is the same as M1 effectively


resid_M4 <- resid(M4)
fitted_M4 <- fitted(M4)

df_M4 <- data.frame(resid_M4, fitted_M4)

f_vs_rs_M4 <- ggplot( df_M4, aes ( x = fitted_M4, y = resid_M4) ) 
f_vs_rs_M4 + geom_point() + #aes(color=wages$RACE)
  #facet_wrap(wages$UNION~wages$OCCUPATION) + 
  ggtitle("Residuals vs Fitted Values for M4")
xlab(expression(hat(y))) + 
  ylab(expression(paste(hat(e), "(M4)")))





resid_M5 <- resid(M5)
fitted_M5 <- fitted(M5)

df_M5 <- data.frame(resid_M5, fitted_M5)

f_vs_rs_M5 <- ggplot( df_M5, aes ( x = fitted_M5, y = resid_M5) ) 
f_vs_rs_M5 + geom_point() + #aes(color=wages$RACE)
  facet_wrap(~wages$OCCUPATION) + 
  ggtitle("Residuals vs Fitted Values for M5") + 
  xlab(expression(hat(y))) + 
  ylab(expression(paste(hat(e), "(M5)")))

summary(M6)

resid_M6 <- resid(M6)
fitted_M6 <- fitted(M6)

df_M6 <- data.frame(resid_M6, fitted_M6)

f_vs_rs_M6 <- ggplot( df_M6, aes ( x = fitted_M6, y = resid_M6) ) 
f_vs_rs_M6 + geom_point() +
  xlab(expression(hat(y))) + 
  ylab(expression(paste(hat(e), "(M6)")))


temp <- lm( log(WAGE) ~ EDUCATION + SEX*OCCUPATION*UNION*RACE + EXPERIENCE, data = wages)

qplot(resid(M1))
qplot(resid(M2))
qplot(resid(M3))
qplot(resid(M4))
qplot(resid(M5))
qplot(resid(M6))

x <- seq(20)
y <- seq(20)^2
tempdf <- data.frame(x, y)

p <- ggplot( tempdf, aes ( x = x, y = y) ) 
p + geom_point()

mod <- lm(y~x,tempdf)
f <- fitted(mod)
r <- resid(mod)
p <- ggplot( tempdf, aes ( x = x, y = r) ) 
p + geom_point()


p <- ggplot(wages , aes ( x = EDUCATION, y = resid(M1)) )
p + geom_jitter(aes(color=SEX))

p <- ggplot( tempdf, aes ( x = f, y = r) ) 
p + geom_point()

summary(M5)
fM5<-fitted(M5)


# R^2 = 1 - (RSS/SYY)
RSS_M1 <- sum((wages$WAGE - fitted(M1))^2)
RSS_M4 <- sum((wages$WAGE - fitted(M4))^2)
RSS_M5 <- sum((wages$WAGE - exp(fitted(M5)))^2)

SYY <- sum((wages$WAGE - mean(wages$WAGE))^2)

1 - (RSS_M1 / SYY) # Wage ~ Edu + Sex
1 - (RSS_M4 / SYY) # Wage ~ Edu + Sex + Experience
1 - (RSS_M5 / SYY) # log(Wage) ~ Edu + Sex + Experience


RSS_M4 <- sum((wages$WAGE - exp(fitted(M4)))^2)
SYY <- sum((wages$WAGE - mean(wages$WAGE))^2)
R2_M4 <- 1 - (RSS_M4 / SYY)

wages <- mutate(wages, DEGREE = ifelse(EDUCATION %in% 0:11,"DNC HS",
                                ifelse(EDUCATION %in% 12,"High School",
                                ifelse(EDUCATION %in% 13:15,"Some University",
                                ifelse(EDUCATION %in% 13:16,"Bachelors",
                                ifelse(EDUCATION %in% 17:20,"Advanced Degree","None"))))))

wages$DEGREE <- as.factor(wages$DEGREE)
wages <- within(wages, DEGREE <- relevel(DEGREE, ref = "DNC HS"))

temp <- lm(WAGE ~ DEGREE + SEX, wages)
summary(temp)
levels(wages$DEGREE)
wages

temp2 <- lm(WAGE ~ DEGREE + SEX + EXPERIENCE, wages)

temp3 <- lm(log(WAGE) ~ DEGREE + SEX + EXPERIENCE, wages)

temp4 <- lm(WAGE ~ EDUCATION + EXPERIENCE + SEX*RACE*OCCUPATION*SECTOR*MARR*UNION*SOUTH, wages)
summary(temp4)



tmp <- powerTransform ( cbind ( EDUCATION, EXPERIENCE, SEX ) ~ 1, data = subset(wages, EXPERIENCE >0) )
summary(tmp)

tmp <- powerTransform ( cbind ( EDUCATION, EXPERIENCE, SEX,RACE,OCCUPATION,SECTOR,MARR,UNION,SOUTH ) ~ 1, data = subset(wages, EXPERIENCE >0) )
summary(tmp)

temp5 <- lm(WAGE ~ EDUCATION + SEX + sqrt(EXPERIENCE), wages)
summary(temp5)
powerTransform(temp5)
boxCox(temp5)
temp6 <- lm(log(WAGE) ~ EDUCATION + SEX + sqrt(EXPERIENCE), wages)

1 - (sum((wages$WAGE - fitted(temp5))^2)/SYY)
1-(sum((wages$WAGE - exp(fitted(temp6)))^2)/SYY)

tmp <- lm(log(WAGE) ~ bs(EDUCATION) + SEX + bs(EXPERIENCE), wages)
summary(tmp)
qplot(resid(tmp))

tmp <- lm(log(WAGE) ~ bs(EDUCATION) + bs(EXPERIENCE) + SEX*RACE*OCCUPATION*SECTOR*MARR*UNION*SOUTH, wages)
summary(tmp)
qplot(hatvalues(M5))

which(hatvalues(M5)>0.03)


outlierTest(M5)

sd(resid(M1))


cleanwages <- subset(wages, hatvalues(M5) < 0.02)

nrow(cleanwages)

M6 <- lm(log(WAGE) ~ EDUCATION + SEX + EXPERIENCE,cleanwages)
summary(M6)
summary(M5)
qplot(hatvalues(M5))
qplot(hatvalues(M6))


tmp <- lm(fitted(M1)~abs(resid(M1)), data=wages)

wts <- 1/fitted(lm(abs(residuals(M1)) ~ fitted(M1)))^2

wlstmp <- lm( WAGE ~ EDUCATION + SEX, data = wages, weights = wts)

p <- ggplot(wages , aes ( x = EDUCATION, y = WAGE) )
p + geom_jitter() + 
  geom_abline ( intercept = coef(M1)[1], slope = coef(M1)[2] ,color="red") + 
  geom_abline ( intercept = coef(wlstmp)[1], slope = coef(wlstmp)[2] ,color="blue")



temp <- wages [, c("EDUCATION", "EXPERIENCE") ]
prcomp(temp)
biplot(princomp(temp))

modEXP <- lm(WAGE ~ EXPERIENCE + SEX, wages)
modEDU <- lm(EDUCATION ~ EXPERIENCE + SEX, wages)

r1 <- resid(modEXP)
r2 <- resid(modEDU)
avp_ed_ex <- data.frame(r1, r2)
head(avp_ed_ex)

p_added_v_edu_exp <- ggplot( avp_ed_ex, aes ( x = r2, y = r1) ) 
p_added_v_edu_exp + geom_point() +
  geom_smooth(data=wages, method='lm',formula=y~x,color="pink")

tmp <- lm(r1~r2)
summary(tmp)
summary(modEXP)


tmp <- lm(WAGE ~ SEX, wages)


tmp <- lm( log(WAGE) ~ EDUCATION + EXPERIENCE + SEX + OCCUPATION + SEX:OCCUPATION, data = wages)
summary(tmp)


tmp <- lm( log(WAGE) ~ EXPERIENCE * SEX, data = wages)
summary(tmp)

tmp <- lm( log(WAGE) ~ EDUCATION * SEX, data = wages)
summary(tmp)

tmp <- lm( log(WAGE) ~ AGE * SEX, data = wages)
summary(tmp)

tmp <- lm( log(WAGE) ~ EDUCATION + AGE +SEX, data = wages)
summary(tmp)

tmp <- lm( log(WAGE) ~ EDUCATION + AGE + SEX*OCCUPATION, data = wages)
summary(tmp)

summary(M5)
summary(M6)
confint(M6)

summary(M1)
summary(M4)
summary(M5)

wages$EDU_P_AGE <- wages$EDUCATION + wages$AGE
wages$EDU_P_EXP <- wages$EDUCATION + wages$EXPERIENCE
ggpairs(wages,columns=c("WAGE","EDUCATION","EXPERIENCE","AGE","EDU_P_AGE","EDU_P_EXP","EXP2"))


tmp <- lm(WAGE ~ EDUCATION + AGE + SEX*OCCUPATION, data = wages)
p <- ggplot( temp, aes ( x = fitted.M1., y = resid_wg_edu) ) 
p + geom_point()

tmp <- lm(WAGE ~ bs(EDUCATION) + EXPERIENCE + SEX*RACE*SECTOR*MARR*UNION*SOUTH, wages)

tmp <- lm(log(WAGE) ~ EDUCATION + EXPERIENCE + SEX+RACE+OCCUPATION+SECTOR+MARR+UNION+SOUTH, wages)
avPlots(tmp)
residualPlots(tmp)

boxcox(tmp)
summary(tmp)

tmp <- lm(WAGE ~ EDUCATION + EXPERIENCE + SEX, wages)

resid_M5 <- resid(tmp)
fitted_M5 <- fitted(tmp)

df_M5 <- data.frame(resid_M5, fitted_M5)

f_vs_rs_M5 <- ggplot( df_M5, aes ( x = fitted_M5, y = resid_M5) ) 
f_vs_rs_M5 + geom_point() + #aes(color=wages$RACE)
  facet_wrap(~wages$OCCUPATION) + 
  ggtitle("Residuals vs Fitted Values for M5") + 
  xlab(expression(hat(y))) + 
  ylab(expression(paste(hat(e), "(M5)")))


fitted_M4 <- fitted(M5)
temp <- data.frame(fitted_M4,wages$WAGE)
inv_fitt_value_M1 <- ggplot( temp, aes ( x =wages.WAGE , y = fitted_M4) )
inv_fitt_value_M1 + geom_point()

tmp <- lm(log(WAGE) ~ EDUCATION + EXPERIENCE + SEX, wages)
summary(tmp)

tmp <- lm(log(WAGE) ~ EDUCATION + bs(EXPERIENCE,degree = 2) + SEX, wages)
summary(tmp)



# Mediation variable
tmp1 <- lm(WAGE~SEX,wages)
tmp2 <- lm(as.numeric(OCCUPATION)~SEX,wages)
tmp3 <- lm(WAGE~SEX+as.numeric(OCCUPATION),wages)
summary(tmp1)
summary(tmp2)
summary(tmp3)

tmp <- lm( log(WAGE) ~ EDUCATION + SEX + EXPERIENCE + UNION + OCCUPATION + RACE + MARR + SOUTH + SECTOR, data = wages)
tmp <- lm( log(WAGE) ~ EDUCATION + EXPERIENCE + SEX * UNION * OCCUPATION * RACE * MARR * SOUTH * SECTOR, data = wages)
tmp <- lm( log(WAGE) ~ bs(EDUCATION) + bs(EXPERIENCE) + SEX * UNION * OCCUPATION * RACE * MARR * SOUTH * SECTOR, data = wages)


tmp <- lm( log(WAGE) ~ EDUCATION + SEX + EXPERIENCE + UNION + OCCUPATION + RACE + MARR + SOUTH + SECTOR, data = wages)


fn_bal <- function(dta, variable, yvar) {
  dta$variable <- dta[, variable]
  dta$yvar <- dta[, yvar]
  tmp <-ggplot(dta, aes(x = variable, y = yvar))
  if(class(dta$variable) == "factor"){
    tmp + geom_boxplot() + 
      xlab(toString(variable)) +
      ylab("Resid M6")
  } else {
    tmp + geom_point() +
      xlab(toString(variable)) +
      ylab("Resid M6")
  }
    
}

grid.arrange(
  fn_bal(wages, "EDUCATION", "residM6"),
  fn_bal(wages, "SEX", "residM6"),
  fn_bal(wages, "EXPERIENCE", "residM6"),
  fn_bal(wages, "AGE", "residM6"),
  fn_bal(wages, "OCCUPATION", "residM6"),
  fn_bal(wages, "UNION", "residM6"),
  nrow = 3, widths = c(1, 0.8)
)

grid.arrange(
  fn_bal(wages, "EDUCATION", "residM1"),
  fn_bal(wages, "SEX", "residM1"),
  fn_bal(wages, "EXPERIENCE", "residM1"),
  fn_bal(wages, "AGE", "residM1"),
  fn_bal(wages, "OCCUPATION", "residM1"),
  fn_bal(wages, "UNION", "residM1"),
  nrow = 3, widths = c(1, 0.8)
)


wages$residM1 <- resid(M1)
wages$residM6 <- resid(M6)
p <- ggplot( wages, aes ( x = EXPERIENCE, y = residM6) )
p+geom_point()

M1 <- lm( WAGE ~ EDUCATION + SEX, data = wages)
M2 <- lm( WAGE ~ EDUCATION + SEX + SEX:EDUCATION, data = wages)
M3 <- lm( WAGE ~ EDUCATION + SEX + EXPERIENCE, data = wages)
M4 <- lm( log(WAGE) ~ EDUCATION + SEX + EXPERIENCE, data = wages)
M5 <- lm( log(WAGE) ~ EDUCATION + SEX*OCCUPATION + EXPERIENCE, data = wages)


qplot(wages$AGE)

summary(lm( log(WAGE) ~ EDUCATION + SEX*EXPERIENCE, data = wages))
confint(lm( log(WAGE) ~ EDUCATION + SEX*EXPERIENCE, data = wages))


p <- ggplot( wages, aes ( x = AGE, y = WAGE) ) 
p + geom_point()

p <- ggplot( wages, aes ( x = SEX, y = log(WAGE)) )
p + geom_boxplot() + ggtitle("log(Wage) vs Sex") + facet_wrap(~OCCUPATION)

wages %>% 
  group_by(UNION,OCCUPATION) %>%
  summarise(no_rows = length(UNION))

wages %>% 
  group_by(OCCUPATION,SECTOR,UNION) %>%
  summarise(no_rows = length(OCCUPATION))

wages$resid_M6 <- resid(M6)
wages$resid_M7 <- resid(M7)
p <- ggplot( wages, aes ( x = MARR, y = resid_M6) ) 
p + geom_boxplot()

p <- ggplot( wages, aes ( x = MARR, y = resid_M7) ) 
p + geom_boxplot()

p <- ggplot( wages, aes ( x = UNION, y = SECTOR) ) 
p + geom_boxplot()

tmp <- lm( log(WAGE) ~ EDUCATION + SEX + SEX:EDUCATION, data = wages)
summary(tmp)


tmp <- lm( WAGE ~ EDUCATION + SEX + SEX:EDUCATION, data = wages)
summary(tmp)

tmp <- lm( log(WAGE) ~ EDUCATION + SEX, data = wages)
wages$tmpr <- resid(tmp)

tmp2 <- lm( EXPERIENCE ~ EDUCATION + SEX, data = wages)
wages$tmp2r <- resid(tmp2)

p <- ggplot( wages, aes ( x = tmp2r, y = tmpr) ) 
p + geom_point()

tmp3 <- lm( tmpr ~ tmp2r, data = wages)
summary(tmp3)


residualPlots(M3,terms = ~ 1)


tmp <- lm( log(WAGE) ~ EDUCATION + tranEXPERIENCE+ SEX + SEX*OCCUPATION , data = wages)
summary(tmp)

tmp <- lm( log(WAGE) ~ EDUCATION + tranEXPERIENCE + SEX*OCCUPATION , data = wages)
summary(tmp)

subset(wages,EXPERIENCE==max(wages$EXPERIENCE))