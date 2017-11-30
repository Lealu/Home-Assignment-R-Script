

#Load data and packages
data.pain.3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv") 

library(psych)
library(car)
library(ggplot2)
library(cAIC4)
library(r2glmm)
library(influence.ME)
library(lattice)
library(reshape2)
library(MuMIn)

#__________________________________________________________________________________________

## asign ID and location as factors
data.pain.3$ID = factor(data.pain.3$ID)
data.pain.3$sex = factor(data.pain.3$sex)

names(data.pain.3)
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")

#Explore data

describe(data.pain.3)

hist(data.pain.3$age)
hist(data.pain.3$STAI_trait)
hist(data.pain.3$pain_cat)
hist(data.pain.3$mindfulness)
hist(data.pain.3$cortisol_serum)
hist(data.pain.3$cortisol_saliva)
hist(data.pain.3$weight)
hist(data.pain.3$pain1)
hist(data.pain.3$pain2)
hist(data.pain.3$pain3)
hist(data.pain.3$pain4)

cor(data.pain.3[,repeated_variables])

#Transform from wide to long format

data.pain.long = melt(data.pain.3, measure.vars=repeated_variables, variable.name = "time", value.name = "pain.rating")
data.pain.long = data.pain.long[order(data.pain.long[,"ID"]),]
data.pain.long$time = as.numeric(data.pain.long$time)

data.pain.long

#_________________________________________________________________________________________


#Analysis

#Build models

model.int = lmer(pain.rating ~ time + age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|ID), data = data.pain.long)
model.slope = lmer(pain.rating ~ time + age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + (time|ID), data = data.pain.long)

summary(model.int)
summary(model.slope)

#Plot pain rating with regression lines

data.pain.long$pred.int = predict(model.int)
data.pain.long$pred.slope = predict(model.slope)

#for random intercept model:
ggplot(data.pain.long, aes(y = pain.rating, x = time, group = ID))+
  geom_point(size = 2)+
  geom_line(color='blue', aes(y = pred.int, x = time))+
  facet_wrap( ~ ID, ncol = 5)

#for random intercept and slope model:
ggplot(data.pain.long, aes(y = pain.rating, x = time, group = ID))+
  geom_point(size = 2)+
  geom_line(color = 'green', aes(y = pred.slope, x = time))+
  facet_wrap(~ ID, ncol = 5)

#for both models
ggplot(data.pain.long, aes(y = pain.rating, x = time, group = ID))+
  geom_point(size = 2)+
  geom_line(color = 'green', aes(y = pred.slope, x = time))+
  geom_line(color = 'blue', aes(y = pred.int, x = time))+
  facet_wrap(~ ID, ncol = 5)

#Compare cAIC
cAIC(model.int)$caic
cAIC(model.slope)$caic

#Compare with ANOVA
anova(model.int, model.slope)

r2beta(model.slope, partial =F) 

#_________________________________________________________________________________________

#Add quadratic effect of time
model.slope.quadr = lmer(pain.rating ~ time + I(time^2) + age + sex 
                         + weight + STAI_trait + pain_cat + mindfulness 
                         + cortisol_serum + (time|ID), data = data.pain.long)
model.slope.quadr
r2beta(model.slope.quadr, partial =T)
summary(model.slope.quadr)

#Plot together with model_slope

data.pain.long$pred.slope.quadr = predict(model.slope.quadr)

ggplot(data.pain.long, aes(y = pain.rating, x = time, group = ID))+
     geom_point(size = 2)+
     geom_line(color = 'green', aes(y = pred.slope, x = time))+
     geom_line(color = 'red', aes(y = pred.slope.quadr, x = time))+
     facet_wrap(~ ID, ncol = 5)



#_______________________________________________________________________________________

# Data and model assumptions check
#Check for influential outliers

influence(model.slope.quadr, obs = T)$alt.fixed

#Check assumptions for model.slope.quadr

# 1.Normality of residuals

qqmath(model.slope.quadr, id=0.05)
shapiro.test(residuals(model.slope.quadr))

# 2.Linearity of prediction and standardized residuals
plot(resid(model.slope.quadr),data.pain.long$pain)

# Linearity of each predictor and the standardized residual

predictors=c("time", "age", "sex", "weight", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum")

for(i in 1:length(predictors)){
  predictor.to.test = data.pain.long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor.to.test, pearson=residuals(model.slope.quadr,type="pearson")),
           aes(x=x,y=pearson)),
      geom_point(),
      geom_smooth(method = 'loess'),
      theme_bw())}

# 3. Homoscedasticity (homogeneity of  variance)

data.pain.long$RES.model.slope.quadr<- residuals(model.slope.quadr)
data.pain.long$ABS.RES.model.slope.quadr <-abs(data.pain.long$RES.model.slope.quadr) #creates a new column with the absolute value of the residuals

data.pain.long$ABS.RES.2.model.slope.quadr <- data.pain.long$ABS.RES.model.slope.quadr^2
LEVENE.model.slope.quadr <- lm(ABS.RES.2.model.slope.quadr ~ ID, data = data.pain.long) #ANOVA of the squared residuals
anova(LEVENE.model.slope.quadr)

plot(model.slope.quadr)
summary(lm(residuals(model.slope.quadr)^2 ~ data.pain.long[,"ID"]))

# 4. Multicollinearity

orig.data.pain.long = melt(data.pain.3, measure.vars=repeated_variables,
                      variable.name = "time", value.name = "pain.rating")
windows()
pairs.panels(orig.data.pain.long, col = "red", lm = T)

#VIF

vif.mer <- function (fit) {
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]}
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v}

vif.mer(model.slope.quadr)
summary(model.slope.quadr)
#______________________________________________________________________________________

#Confidence intervals
confint(model.slope)
confint(model.slope.quadr)

#Standardized coefficients

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))}

stdCoef.merMod(model.slope.quadr)
stdCoef.merMod(model.slope)

# p-values

summary(model.slope)
summary(model.slope.quadr)

#___________________________________________________________________________________________

#Compare model.slope with model.slope.quadr

cAIC(model.slope)$caic
cAIC(model.slope.quadr)$caic

anova(model.slope, model.slope.quadr)

summary(model.slope)
summary(model.slope.quadr)
