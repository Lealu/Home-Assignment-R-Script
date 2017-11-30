#Load data

datasample1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv") 
datasample1 = as.data.frame(datasample1)

#Descriptive statistics

describe(datasample1)
summary(datasample1)

#Histograms

windows()
hist(datasample1$mindfulness, breaks = 160)
hist(datasample1$pain, breaks = 160)
hist(datasample1$STAI_trait, breaks = 160)
hist(datasample1$pain_cat, breaks = 160)
hist(datasample1$cortisol_serum, breaks = 160)
hist(datasample1$cortisol_saliva, breaks = 160)
hist(datasample1$weight, breaks = 160)

#Find coding errors
boxplot(datasample1$mindfulness)
which(datasample1$mindfulness<1)

#Delete coding errors
datasample2 <- datasample1[-c(15, 24, 25, 66),]

datasample2$sex <- factor(datasample2$sex)

describe(datasample2)
summary(datasample2)

#Univariate outliers 
describe(datasample2)
summary(datasample2)

#__________________________________________________________________________________________

#Regression model 1
model1 <- lm(pain ~ age + sex, data = datasample2)
model1
summary(model1)

#Regression model 2
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = datasample2)
model2
summary(model2)

#Influential outliers model1

cooks.distance(model = model1) #look at datapoints and identify why they are different than the others. Makes sense in the dataset? Good rational explanation if excluding

windows()

plot(model1, which = 4)
plot(model1, which = 5)

#Run regression model1 without influential outliers and compare outcome

model1.adj <- lm(pain ~ age + sex, data = datasample2, subset = -c(5, 70, 112))
model1
model1.adj
summary(model1)
summary(model1.adj)
AIC(model1)
AIC(model1.adj)

#Influential outliers model2
cooks.distance(model = model2)

windows()
plot(model2, which = 4)
plot(model2, which = 5)

#Run regression model2 without influential outliers and compare outcome

model2.adj <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva,
                 data = datasample2, subset = -c(37, 46, 112))
model2
model2.adj
summary(model2)
summary(model2.adj)

#__________________________________________________________________________________________

#Checking assumptions for model 1: 
#1. Normality of residuals

plot(model1, which = 2)
describe(residuals(model1))
hist(x = residuals(model1), xlab = "Value of residual", main = "Residuals model 1", breaks = 20)

shapiro.test(residuals(model1))

#2. Linearity of relationship

pred1 <- predict( object = model1 )
plot( x = pred1, y = datasample2$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = model1, which = 1)

residualPlots( model = model1 )

#3. Homogeneity of variance
plot(model1, which = 3)
ncvTest(model1)

#4. Multicollinearity
vif(model1)
pairs.panels(datasample2[,c("age", "sex")], col = "blue", lm = T)

#___________________________________________________________________________________________


#Checking assumptions for model 2: 
#1. Normality of residuals

plot(model2, which = 2)
describe(residuals(model2))
hist(x = residuals( model2), xlab = "Value of residual", main = "Residuals model 2", breaks = 20)

shapiro.test(residuals(model2))

#2. Linearity of relationship

pred2 <- predict( object = model2 )
plot( x = pred2, y = datasample2$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = model2, which = 1)

residualPlots( model = model2 )

#3. Homogeneity of variance
plot(model2, which = 3)
ncvTest(model2)

#4. Multicollinearity
vif(model2)

windows()
pairs.panels(datasample2[,c("pain_cat", "age", "sex", "mindfulness", "cortisol_saliva", "cortisol_serum", "STAI_trait")],
             col = "blue", lm = T)

#__________________________________________________________________________________________

#Eliminating predictor to adjust for high multicollinearity

test.without.saliva <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum,
                      data = datasample2)
test.without.serum <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva,
                         data = datasample2)

summary(test.without.serum)
summary(test.without.saliva)
AIC(test.without.serum, test.without.saliva)

final.model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum,
                   data = datasample2)
summary(final.model2)

#__________________________________________________________________________________________

#Rerun check for influential outliers
cooks.distance(model = final.model2)

windows()
plot(final.model2, which = 4)
plot(final.model2, which = 5)

#Rerun assumption checks for the final model

#1. Normality of residuals

plot(final.model2, which = 2)
describe(residuals(final.model2))
hist(x = residuals(final.model2), xlab = "Value of residual", main = "Residuals final model 2", breaks = 20)

shapiro.test(residuals(final.model2))

#2. Linearity of relationship

pred <- predict(object = final.model2)
plot(x = pred, y = datasample2$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = final.model2, which = 1)

residualPlots(model = final.model2)

#3. Homogeneity of variance

plot(final.model2, which = 3)
ncvTest(final.model2)

#4. Multicollinearity
vif(final.model2)

windows()
pairs.panels(datasample2[,c("pain_cat", "age", "sex", "mindfulness", "cortisol_saliva", "cortisol_serum", "STAI_trait")],
             col = "blue", lm = T)

#__________________________________________________________________________________________

#Final model test statistics
summary(model1)
summary(final.model2)
model1
final.model2

#Confidence intervals for regression coefficients
confint(model1)
confint(final.model2)

#Standardized coefficients
lm.beta(model1)
lm.beta(final.model2)

#Comparison of models
anova(model1, final.model2)
AIC(model1, final.model2)

summary(final.model2)


