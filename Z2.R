datasample1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv") 
datasample1 = as.data.frame(datasample1)

#Data diagnostics

summary(datasample1)
describe(datasample1)

#Find coding errors
boxplot(datasample1$mindfulness)
which(datasample1$mindfulness<1)

#Delete coding errors
datasample2 <- datasample1[-c(15, 24, 25, 66),]
describe(datasample2)
summary(datasample2)

#___________________________________________________________________________________________________-

#Create collegue's regression model
col.model <- lm(pain ~ age + sex + weight + STAI_trait + pain_cat + 
                  mindfulness + cortisol_serum,
                data = datasample2)

#Influential outliers col.model
cooks.distance(model = col.model)
windows()
plot(col.model, which = 5)

#Run col.model without influential outliers and compare outcome

col.model.adj <- lm(pain ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum,
                    data = datasample2, subset = -c(37, 112, 160))
col.model
col.model.adj
summary(col.model)
summary(col.model.adj)

#__________________________________________________________________________________________

#Check for assumptions: (col.model)
#1. Normality of residuals

plot(col.model, which = 2) #QQplot
describe(residuals(col.model))
hist(x = residuals(col.model), xlab = "Value of residual", main = "Residuals collegue's model", breaks = 20)

shapiro.test(residuals(col.model))

#2. Linearity of relationship

pred1 <- predict( object = col.model )
plot( x = pred1, y = datasample2$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = col.model, which = 1)

residualPlots( model = col.model )

#3. Homogeneity of variance
plot(col.model, which = 3)
ncvTest(col.model)

#4. Multicollinearity
vif(col.model)

#____________________________________________________________________________


#Run backward regression on col.model
step( object = col.model, direction = "backward")

backward.model <- lm(formula = pain ~ age + pain_cat + mindfulness + cortisol_serum, 
                      data = datasample2)
AIC(backward.model)

#Check for assumptions: (backward.model)
#1. Normality of residuals

plot(backward.model, which = 2) #QQplot
describe(residuals(backward.model))
hist(x = residuals(backward.model), xlab = "Value of residual", main = "Residuals backward model", breaks = 20)

shapiro.test(residuals(backward.model))

#2. Linearity of relationship

pred2 <- predict(object = backward.model)
plot(x = pred2, y = datasample2$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = backward.model, which = 1)

residualPlots(model = backward.model)

#3. Homogeneity of variance
plot(backward.model, which = 3)
ncvTest(backward.model)

#4. Multicollinearity
vif(backward.model)

#_________________________________________________________________________________________

#Compare initial and backward model
summary(col.model)
summary(backward.model)
AIC(col.model)
AIC(backward.model)
anova(col.model, backward.model)

#______________________________________________________________________________________________

#Recreate own final model from assignment part 1
theory.based.model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum,
                         data = datasample2)

#Compare theory based model and backward model

summary(backward.model)
summary(theory.based.model)

AIC(backward.model, theory.based.model)

anova(backward.model, theory.based.model)

#__________________________________________________________________________________________

new.data = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")

as.data.frame(new.data)

#Find coding errors

summary(new.data)
describe(new.data)
boxplot(new.data$mindfulness)
which(new.data$mindfulness<1)

#Delete coding errors
new.data <- new.data[-c(26, 30, 43, 78, 93, 158),]
describe(new.data)

#Predict values in new.data with backward model

pred.backward <- predict(backward.model, new.data)
RSS.backward = sum((new.data["pain"] - pred.backward)^2) #Residual Sum of Squares
RSS.backward

#Predict values in new.data with theory based model
pred.theory <- predict(theory.based.model, new.data)
RSS.theory = sum((new.data["pain"] - pred.theory)^2) #Residual Sum of Squares
RSS.theory


#Plots
plot(datasample2$pain, pch = 16, cex = 1.3, col = "blue",
     xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")
abline(lm(formula = pain ~ age + cortisol_serum, 
          data = datasample2))
