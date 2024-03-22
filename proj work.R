library(data.table)
setwd("C:/Users/Xiao Lian/OneDrive - Nanyang Technological University/NBS Y2S1/BC2406 Analytics 1")
corrosion <- fread("Dataset-for-internal-CO2-corrosion-of-oil-pipeline.csv", stringsAsFactors=T)
summary(corrosion)
class(corrosion$Corrosion_rate)#numeric
class(corrosion$Temperature)#integer
class(corrosion$Flow_velocity)#numeric
class(corrosion$CO2_pressure)#numeric
class(corrosion$Internal_pressure)#integer
class(corrosion$Corrosion_Inhibitor_efficiency)#integer
class(corrosion$Shear_stress)#integer
class(corrosion$pH)#numeric
library(e1071)
skewness(corrosion$Corrosion_rate)#0.4388697 not v skewed
#Datacleaning
sum(is.na(dataset1))#dont have any NA or Null values, dont need datacleaning?
set.seed(2004)
train <- sample.split(Y = corrosion$Corrosion_rate, SplitRatio = 0.7)
trainset <- subset(corrosion, train == T)
testset <- subset(corrosion, train == F)


#Linear Regression - full model
cr <-lm( Corrosion_rate ~ Temperature + Flow_velocity + CO2_pressure + Internal_pressure + Corrosion_Inhibitor_efficiency + Shear_stress + pH, data =corrosion)
summary(cr)

#Linear regression with backward elimination
install.packages("stats")
library(stats)
full.model <- lm(Corrosion_rate ~ Temperature + Flow_velocity + CO2_pressure + Internal_pressure + Corrosion_Inhibitor_efficiency + Shear_stress + pH, data = dataset1)
reduced.model <- step(full.model, direction = "backward")
summary(reduced.model)

#checking multicollinearity
library(car)
vif(reduced.model)


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
testset.error <- testset$Corrosion.rate - predict.model.test

# Testset Errors
RMSE.model.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.model.train # 0.262654
RMSE.model.test # 0.2481164


plot(corrosion$pH, corrosion$Corrosion_rate, main="Effects of Temperature on Corrosion Rate",
     xlab="Temperature", ylab="Corrosion Rate", pch=19, col="blue")
library(ggplot2)
ggplot(corrosion, aes(Shear_stress,Corrosion_rate)) + labs(title ="Effects of Co2 pressure on Corrosion Rate") + 
  geom_point() + 
  geom_smooth(method = "lm",se= FALSE, color = "red") +  # red regression line
  theme_minimal() 


variables<- c("pH", "Temperature", "Flow_velocity", "Internal_pressure", "Shear_stress", "Co2_pressure", "Corrosion_Inhibitor_efficiency")
for (var in variables) {
  p <- ggplot(corrosion, aes_string(x = var, y = "Corrosion_rate", fill = var)) +
    geom_boxplot() +
    labs(x = var, y = "Corrosion_rate") +
    ggtitle(paste("Boxplot of", var, "against Corrosion Rate"))
  print(p)
  Sys.sleep(2)
}


install.packages("reshape2")
library(reshape2)
library(ggplot2)
cormat <- round(cor(dataset1),2)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)


# Plot half-triangle heatmap
#temperature has an effect on the corrosion rate
ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  labs(title = "Correlation matrix", x=NULL, y=NULL) +
  geom_text(aes(label = value), color = "black", size = 2) +
  scale_fill_gradient2(low = "lightsteelblue", high = "indianred") +
  theme_minimal() +  # Remove the background
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#DATASET2-------------------------------------------------------------------------------------------
setwd("C:/Users/Xiao Lian/OneDrive - Nanyang Technological University/NBS Y2S1/BC2406 Analytics 1")
dataset2 <- fread("database.csv", stringsAsFactors=T)
summary(dataset2)
colSums(is.na(dataset2))
View(dataset2)
str(dataset2)
library(data.table)
library(reshape2)
library(dplyr)
library(nnet)

oilspill <- fread("Oilspill.csv")

#prepare data
summary(oilspill)

#create shutdown time
a <- as.POSIXlt(oilspill$`Shutdown Date/Time`, format="%m/%d/%Y %H:%M")
b <- as.POSIXlt(oilspill$`Restart Date/Time`, format="%m/%d/%Y %H:%M")
time_diff <- difftime(b,a, units="hours")
time_diff
hours_diff <- as.numeric(time_diff)
hours_diff <- round(hours_diff,2)
oilspill$Shutdown_hours <- hours_diff
oilspill[is.na(`Shutdown_hours`), `Shutdown_hours` := 0]

#create % of barrel loss
oilspill$`Barrel loss (%)` <- oilspill$`Net Loss (Barrels)`/oilspill$`Unintentional Release (Barrels)` *100
which(is.na(oilspill), arr.ind = TRUE)
oilspill[is.na(`Barrel loss (%)`), `Barrel loss (%)` := 0]
#final dataset to be exported
oilspill <- oilspill[,c("Accident Year", "Cause Category", "Cause Subcategory", "Net Loss (Barrels)", 
                        "Barrel loss (%)", "Pipeline Location", "Pipeline Type", "Liquid Type", "Liquid Ignition", 
                        "Liquid Explosion", "Pipeline Shutdown", "Shutdown_hours",
                        "Property Damage Costs", "Lost Commodity Costs", 
                        "Public/Private Property Damage Costs", "Emergency Response Costs", 
                        "Environmental Remediation Costs", "Other Costs", "All Costs")]

sum(is.na(oilspill)) #51 NAs

which(is.na(oilspill), arr.ind = TRUE) #NA in column 13-18, which are all about costs -> replace with 0
oilspill[is.na(oilspill)] <- 0 #replace with NAs with 0
oilspill <- oilspill %>% mutate_if(is.character, list(~na_if(.,"")))

#need to factorize after mutating else it does not work
oilspill$`Accident Year`<- factor(oilspill$`Accident Year`)
oilspill$`Cause Category` <- factor(oilspill$`Cause Category`)
oilspill$`Cause Subcategory` <- factor(oilspill$`Cause Subcategory`)
oilspill$`Pipeline Shutdown` <- factor(oilspill$`Pipeline Shutdown`)
oilspill$`Pipeline Location` <- factor(oilspill$`Pipeline Location`)
oilspill$`Pipeline Type` <- factor(oilspill$`Pipeline Type`)
oilspill$`Liquid Type` <- factor(oilspill$`Liquid Type`)
oilspill$`Liquid Ignition` <- factor(oilspill$`Liquid Ignition`) 
oilspill$`Liquid Explosion` <- factor(oilspill$`Liquid Explosion`)
str(oilspill)

write.csv(oilspill, file = "oilspill_cleaned.csv", row.names = F) #to view it better in excel

View(oilspill)
#Cause Category, Cause Subcategory, Net Loss (Barrels), 
#Property Damage Costs, Lost Commodity Costs	, Public/Private Property Damage Costs,
#Emergency Response Costs, Environmental Remediation Costs, Other Costs, All Costs


#datacleaning by omitting rows with Empty/NA
cols_to_check <- c("Property Damage Costs", "Lost Commodity Costs", "Public/Private Property Damage Costs", "Emergency Response Costs", "Environmental Remediation Costs", "Other Costs")  # Add more columns as needed
for(col in cols_to_check) {
  dataset2 <- dataset2[!is.na(get(col)), ]
}

summary(dataset2)
str(dataset2)
linreg <-lm( dataset2$`Cause Category` ~ dataset2$`Property Damage Costs` + dataset2$ + CO2_pressure + Internal_pressure + Corrosion_Inhibitor_efficiency + Shear_stress + pH, data =dataset1)
summary(cr)

library(data.table)
library(reshape2)
library(ggplot2)
library(corrplot)
library(car)
library(caTools)
library(rpart)
library(rpart.plot) 
library(dplyr)
library(nnet)



#trying to build logistic regression
data2 <- dataset2[dataset2$`Cause Category` == "CORROSION",]
data2 <- dataset2[,c("Pipeline Location", "Pipeline Type", "Liquid Type", "Liquid Ignition", 
                  "Liquid Explosion", "Pipeline Shutdown")]
sum(is.na(data2))
which(is.na(data2))
data2 <- data2 %>% mutate_all(~ ifelse(. == "", NA, .))
data2 <- data2 %>% filter_at(vars("Pipeline Shutdown"), all_vars(!is.na(.))) #5NA remaining
data2 <- data2 %>% filter_at(vars("Pipeline Type"), all_vars(!is.na(.))) #5NA remaining

str(data2)
data2$`Pipeline Location` <- factor(data2$`Pipeline Location`)
data2$`Pipeline Type` <- factor(data2$`Pipeline Type`)
data2$`Liquid Type` <- factor(data2$`Liquid Type`)
data2$`Liquid Ignition` <- factor(data2$`Liquid Ignition`) 
data2$`Liquid Explosion` <- factor(data2$`Liquid Explosion`)
data2$`Pipeline Shutdown`<- factor(data2$`Pipeline Shutdown`)
levels(data2$Pipeline)
levels(data2$Pipeline Type)
levels(data2$Liquid Type)
levels(data2$Liquid Ignition)
levels(data2$Liquid Explosion) #only 1 level -> can omit


reg = glm(data2$`Pipeline Shutdown` ~ data2$`Pipeline Type` + data2$`Liquid Type` + data2$`Liquid Ignition`, 
          family = binomial, data = data2)
summary(reg)


prob = predict(reg, type = 'response')
prob
classifier = ifelse(prob>0.5, "YES", "NO")
table(classifier, data2$`Pipeline Shutdown`)
mean(classifier == data2$`Pipeline Shutdown`)*100