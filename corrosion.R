library(data.table)
library(stats)
library(car)
library(ggplot2)
library(caTools)
library(rpart.plot)
library(rpart)
library(reshape2)
set.seed(2004)

setwd("C:/Users/Xiao Lian/OneDrive - Nanyang Technological University/NBS Y2S1/BC2406 Analytics 1")
corrosion <- fread("corrosion.csv", stringsAsFactors=T)
str(corrosion)

#Data cleaning
summary(corrosion)
str(corrosion)
sum(is.na(corrosion)) #dont have any NA or Null values, dont need datacleaning
sum(duplicated(corrosion)) # no duplicates

setnames(corrosion, "Flow velocity", "Flow_velocity")
setnames(corrosion, "CO2 pressure", "CO2_pressure")
setnames(corrosion, "Internal pressure", "Internal_pressure")
setnames(corrosion, "Corrosion Inhibitor efficiency", "Corrosion_Inhibitor_efficiency")
setnames(corrosion, "Shear stress", "Shear_stress")
setnames(corrosion, "Corrosion rate", "Corrosion_rate")

library(e1071)
skewness(corrosion$Corrosion_rate)#0.4388697 not very skewed

#Exploratory Analysis
#Correlation Matrix

cormat <- round(cor(corrosion),2)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  labs(title = "Correlation matrix", x=NULL, y=NULL) +
  geom_text(aes(label = value), color = "black", size = 2) +
  scale_fill_gradient2(low = "lightsteelblue", high = "indianred") +
  theme_minimal() +  # Remove the background
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Visualizations 
corrosion$Temperature <- factor(corrosion$Temperature) #temporarily change it to factor for visual
#corrosion increases with higher temperature
a <- aggregate(Corrosion_rate ~ Temperature, data = corrosion, mean)
a$Corrosion_rate <- round(a$Corrosion_rate, 2)

ggplot(corrosion, aes(x = Temperature, y = Corrosion_rate, fill = Temperature)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", shape = 18, size = 3, show_guide = FALSE) +
  geom_text(data = a, aes(label = Corrosion_rate), vjust = -0.5, color = "darkred", size = 4) #scale_x_discrete(labels = c("Female","Male")) 

corrosion$Temperature <- as.integer(corrosion$Temperature) #return data type to int
class(corrosion$Temperature)

ggplot(corrosion, aes(x = Temperature, y = Corrosion_rate)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)

# higher pressure -> more corrosion
corrosion$`CO2_pressure` <- factor(corrosion$`CO2_pressure`) #temporarily change it to factor for visual
b <- aggregate(`Corrosion_rate` ~ `CO2_pressure`, corrosion, mean)
b$`Corrosion_rate` <- round(a$`Corrosion_rate`,2)
ggplot(corrosion, aes(x = `CO2_pressure`, y = `Corrosion_rate`, fill = `CO2_pressure`)) + geom_boxplot() + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show_guide = FALSE)  + geom_text(data = b, aes(label =  `Corrosion_rate`), vjust = -0.5, color = "darkred", size = 4) #scale_x_discrete(labels = c("Female","Male")) 

corrosion$Temperature <- as.integer(corrosion$Temperature) #return data type to int
class(corrosion$Temperature)

ggplot(corrosion, aes(x = Temperature, y = `Corrosion_rate`)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
corrosion$`CO2_pressure` <- as.numeric(corrosion$`CO2_pressure`) #return data type to int
ggplot(corrosion, aes(x = `CO2_pressure`, y = `Corrosion_rate`)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)


# lower efficiency -> more corrosion
corrosion$`Corrosion_Inhibitor_efficiency` <- factor(corrosion$`Corrosion_Inhibitor_efficiency`) #temporarily change it to factor for visual
c <- aggregate(`Corrosion_rate` ~ `Corrosion_Inhibitor_efficiency`, corrosion, mean)
c$`Corrosion_rate` <- round(c$`Corrosion_rate`,2)
ggplot(corrosion, aes(x = `Corrosion_Inhibitor_efficiency`, y = `Corrosion_rate`, fill = `Corrosion_Inhibitor_efficiency`)) + geom_boxplot() + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show_guide = FALSE)  + geom_text(data = c, aes(label =  `Corrosion_rate`), vjust = -0.5, color = "darkred", size = 4) 


corrosion$`Corrosion_Inhibitor_efficiency` <- as.integer(corrosion$`Corrosion_Inhibitor_efficiency`) #return data type to int
class(corrosion$`Corrosion_Inhibitor_efficiency`)

ggplot(corrosion, aes(x = `Corrosion_Inhibitor_efficiency`, y = Corrosion_rate)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)


#lower pH -> more corrosion
#pH = 2 in general is higher than pH =1 -> could be due to more pH = 2 cases with outliers
corrosion$pH <- factor(corrosion$pH) #temporarily change it to factor for visual
d <- aggregate(`Corrosion_rate` ~ pH, corrosion, mean)
d$`Corrosion_rate` <- round(d$`Corrosion_rate`,2)
ggplot(corrosion, aes(x = pH, y = `Corrosion_rate`, fill = pH)) + geom_boxplot() + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show_guide = FALSE)  + geom_text(data = d, aes(label =  `Corrosion_rate`), vjust = -0.5, color = "darkred", size = 4) #scale_x_discrete(labels = c("Female","Male")) 
corrosion$pH <- as.numeric(corrosion$pH) #return data type to int
ggplot(corrosion, aes(x = pH, y = `Corrosion_rate`)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)

ggplot(corrosion, aes(x = Corrosion_rate)) + geom_histogram(bins = 70) + labs(title = "Histogram of Corrosion Rate") #Histogram

#Linear regression
cr <-lm( Corrosion_rate ~ Temperature + Flow_velocity + CO2_pressure + Internal_pressure + Corrosion_Inhibitor_efficiency + Shear_stress + pH, data = corrosion)
summary(cr) # shear factor have NA

#Linear regression with backward elimination
full.model <- lm(Corrosion_rate ~ Temperature + Flow_velocity + CO2_pressure + Internal_pressure + Corrosion_Inhibitor_efficiency + Shear_stress + pH, data = corrosion)
reduced.model <- step(full.model, direction = "backward")
summary(reduced.model)

#checking multicollinearity
vif(reduced.model)

# 70% trainset. Stratify on Y = Corrosion.rate.
train <- sample.split(Y = corrosion$Corrosion_rate, SplitRatio = 0.7)
trainset <- subset(corrosion, train == T)
testset <- subset(corrosion, train == F)
trainset2 <- trainset #for CART 
testset2 <- testset #for CART 

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$Corrosion_rate)
summary(testset$Corrosion_rate)

# Develop model on trainset
model <- lm(Corrosion_rate ~ Temperature + Flow_velocity + CO2_pressure + Internal_pressure + Corrosion_Inhibitor_efficiency + pH, data = trainset)
summary(model)
residuals(model)
predict.model.train = (predict(model, newdata = trainset))


# Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.model.train <- sqrt(mean(residuals(model)^2))  # RMSE on trainset based on model.
summary(abs(residuals(model)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.model.test <- predict(model, newdata = testset)
testset.error <- testset$Corrosion_rate - predict.model.test

# Testset Errors
RMSE.model.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.model.train # 0.262654
RMSE.model.test # 0.2481164

# making table (trainset)
trainset$ModelPredictedCorrosionRate <- round(predict.model.train,1)
trainset$Trainset_error <- (trainset$Corrosion_rate - trainset$ModelPredictedCorrosionRate)
trainset$Trainset_error <- round(trainset$Trainset_error, digits =4)
trainset$Trainset_squared_error <- round((trainset$Trainset_error)^2, digits =4)
trainset$Trainset_error_percentage <- round((abs(trainset$Trainset_error/trainset$Corrosion_rate*100)),digits = 4)
value_range <- range(trainset$Trainset_error_percentage)
print(value_range) # 0.0000 23.8095
mean(trainset$Trainset_squared_error) #0.06936782
mean(trainset$Trainset_error_percentage) # 5.533229

# making table (testset)
testset$ModelPredictedCorrosionRate <- round(predict.model.test,1)
testset$Testset_error <- (testset$Corrosion_rate - testset$ModelPredictedCorrosionRate)
testset$Testset_error <- round(testset$Testset_error, digits =4)
testset$Testset_squared_error <- round((testset$Testset_error)^2, digits =4)
testset$Testset_error_percentage <- round((abs(testset$Testset_error/testset$Corrosion_rate*100)),digits = 4)
value_range2 <- range(testset$Testset_error_percentage)
print(value_range2) # 0.0000 19.0476
mean(testset$Testset_squared_error) # 0.0615942
mean(testset$Testset_error_percentage) # 5.059278

#CART
m <- rpart(Corrosion_rate ~ . - Shear_stress, data = trainset2, method = 'anova',
           control = rpart.control(minsplit = 20, cp = 0))
rpart.plot(m, type = 2, extra = 101, main = "Maximal Tree for Corrosion rate prediction")
print(m)
printcp(m)
plotcp(m, main = "Subtrees in Corrosion rate prediction")
CVerror.cap <- m$cptable[which.min(m$cptable[,"xerror"]), "xerror"] + 
  m$cptable[which.min(m$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (m$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(m$cptable[i,1] * m$cptable[i-1,1]), 1)
cp.opt 
m2 <- prune(m, cp = cp.opt) 
print(m2)
printcp(m2)
rpart.plot(m2, type = 2, extra = 101, main = "Optimal Tree for Corrosion rate prediction")
m2$variable.importance
scaled_importance <- round(100*m2$variable.importance/sum(m2$variable.importance))
var_importance <- data.frame(scaled_importance)


# Make predictions on the original dataset
#train
cart.train.predict <- predict(m2, newdata = trainset2, typep = "anova")
trainset2[,'cart predicted corrosion':= round(cart.train.predict,1)]
trainset2[,'test error':= round(cart.train.predict - Corrosion_rate,2)]
cart.train.error <- round(trainset2$`cart predicted corrosion` - trainset2$Corrosion_rate,1)
cart.train.error <- round(cart.train.predict - trainset2$Corrosion_rate,1)
RMSE.cart.train.test <- sqrt(mean(cart.train.error^2))
RMSE.cart.train.test #0.3171352

trainset2[,"Trainset_error_percentage"] <- round((abs(cart.train.error/trainset2$Corrosion_rate*100)), 4)
value_rangeT <- range(trainset2$Trainset_error_percentage)
value_rangeT #0.0000 20.5882
MAPE.train <- mean(trainset2$Trainset_error_percentage)
MAPE.train #6.120344

cart.predict <- predict(m2, newdata = testset2, typep = "anova")
testset2[,'cart predicted corrosion':= round(cart.predict,1)]
testset2[,'test error':= round(cart.predict - Corrosion_rate,2)]
cart.testset.error <- testset2$`cart predicted corrosion` - testset2$Corrosion_rate
RMSE.cart.test <- sqrt(mean(cart.testset.error^2))
RMSE.cart.test #0.3645525
#RMSE of CART > RMSE of linear regressions -> linear regression is better
testset2[,"Testset_error_percentage"] <- round((abs(cart.testset.error/testset2$Corrosion_rate*100)), 4)
value_rangeT <- range(testset2$Testset_error_percentage)
value_rangeT #0.0000 17.1429
MAPE.test <- mean(testset2$Testset_error_percentage)
MAPE.test #7.349235


# Make predictions on the original dataset
trainprediction <- predict(m2, newdata = trainset2)
testprediction <- predict(m2, newdata = testset2)

# Print the predictions
trainset2$ModelPredictedCorrosionRate <- round((trainprediction), digits=1)
trainset2$Trainset_error <- round((trainset$Corrosion_rate - trainset$ModelPredictedCorrosionRate),1)


trainset2$Trainset_squared_error <- ((trainset$Trainset_error)^2)
trainset2$Trainset_error_percentage <- round((abs(trainset$Trainset_error/trainset$Corrosion_rate*100)),digits = 4)
value_rangeT <- range(trainset2$Trainset_error_percentage)
print(value_rangeT)

Trainmse <- mean(trainset2$Trainset_squared_error)
RMSE.model.train<- sqrt(Trainmse)
RMSE.model.train
Trainmape <- mean(trainset2$Trainset_error_percentage)


print(Trainmape)
print(Trainmse)

testset2$ModelPredictedCorrosionRate <- round((testprediction), digits=1)
testset2$Testset_error <- (testset$Corrosion_rate-testset2$ModelPredictedCorrosionRate)
testset2$Testset_error <- round(testset2$Testset_error, digits =4)
testset2$Testset_squared_error <- ((testset2$Testset_error)^2)
testset2$Testset_error_percentage <- round((abs(testset2$Testset_error/testset2$Corrosion_rate*100)),digits = 4)
value_rangeTr <- range(testset2$Testset_error_percentage)
mse <- mean(testset2$Testset_squared_error)
RMSE.model.test <- sqrt(mse)
RMSE.model.test
mape <- mean(testset2$Testset_error_percentage)
print(value_rangeTr)
print(mape)
print(mse)

#RMSE of CART > RMSE of linear regressions -> linear regression is better
