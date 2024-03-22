setwd("C:/Users/Xiao Lian/OneDrive - Nanyang Technological University/NBS Y2S1/BC2406 Analytics 1")

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

oilspill <- fread("oilspill_cleaned.csv")
View(oilspill)
set.seed(2004)

str(oilspill)
sum(is.na(oilspill))

oilspill$`Cause Category` <- factor(oilspill$`Cause Category`)
oilspill$`Cause Subcategory` <- factor(oilspill$`Cause Subcategory`)

#exploratory analysis
#corrosion is the 2nd most common cause for oil spill
library(dplyr)
oilspill_count <- oilspill %>%
  group_by(oilspill$`Accident Year`, oilspill$`Cause Category`) %>%
  tally()
Year<- oilspill$`Accident Year`
ggplot(oilspill_count, aes(x = Year, y = n, group = "Cause Category")) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Accidents by Year", x = "Year", y = "Number of Accidents") +
  theme_minimal()

ggplot(data = oilspill, aes(x = `Cause Category`, fill = `Cause Category`), group = `Cause Category`) + geom_bar() +  
  labs(x = "Cause", y = "Count", title = "Number of oil spill incidents by cause") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = guide_legend(title = "Cause Category"))

sum(oilspill$`All Costs`[oilspill$`Cause Category` == "EXCAVATION DAMAGE"])
ggplot(data = oilspill, aes(x = factor(`Cause Category`), y = `All Costs`, fill = factor(`Cause Category`))) +
  geom_bar(stat = "identity") +
  labs(x = "Cause Category", y = "Total Costs", title = "Total Costs by cause") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Cause Category")) 




#total of 40137 barrels of oil are lost, with an avg of nearly 68 barrels for each accident
#find out how much the money for each barrel costs
sum(oilspill$`Net Loss (Barrels)`[oilspill$`Cause Category` == "CORROSION"]) / sum(oilspill$`Cause Category` == "CORROSION")

oilspill2 <- oilspill[`Cause Category`=="CORROSION",]
summary(oilspill2)

#avg cost per incident caused by corrosion is $667780 - a huge number
sum(oilspill2$`All Costs`) / nrow(oilspill2) 
nrow(oilspill2[`Net Loss (Barrels)` != 0]) / nrow(oilspill2) * 100 #49.66216
sum(oilspill2$`Net Loss (Barrels)`) / nrow(oilspill2[`Net Loss (Barrels)` !=0]) #136.5194
sum(oilspill2$Shutdown_hours) #96712.97
sum(oilspill2$Shutdown_hours) / nrow(oilspill2[Shutdown_hours != 0]) #312.987
nrow(oilspill2[Shutdown_hours != 0]) / nrow(oilspill2) * 100 #52.19595

#Top most expensive costs involve: emergency response costs, environmental costs and property damage costs
a <- round(sum(oilspill2$`Property Damage Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`)*100,2)
b <- round(sum(oilspill2$`Lost Commodity Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
c <- round(sum(oilspill2$`Public/Private Property Damage Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
d <- round(sum(oilspill2$`Emergency Response Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
e <- round(sum(oilspill2$`Environmental Remediation Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
f <- round(sum(oilspill2$`Other Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
#Pie chart
values <- c(a, b, c, d, e, f)
a + b + c + d + e + f #check if it's = 100
labels <- c("Property Damage Costs", "Lost Commodity Costs", "Public/Private Property Damage Costs", "Emergency Response Costs", "Environmental Remediation Costs", "Other Costs")
pie_colors <- c("pink", "yellow", "green", "lightblue", "red","orange")
pie <- data.frame(labels, values)
ggplot(pie, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +  # Create a polar coordinate system
  coord_polar("y", start=0) +
  theme_void() +   # Customize the theme if needed
  labs(title = "Breakdown of costs for incidents caused by corrosion (%)") + 
  geom_text(data = pie, aes(label = values), size = 4, color = "white", position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette="Set2")

#internal corrosion contributes more to incidents because it is harder to detect than external corrosion
ggplot(data = oilspill2, aes(x = factor(`Cause Subcategory`), fill = factor(`Cause Subcategory`), group = factor(`Cause Subcategory`))) + geom_bar() +  
  labs(x = "Cause", y = "Count", title = "Number of oil spill incidents by cause") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = guide_legend(title = "Cause Category"))

#avg cost for external corrosion is actually more than internal corrosion
sum(oilspill2$`All Costs`[oilspill2$`Cause Subcategory` == "EXTERNAL"]) / sum(oilspill2$`Cause Subcategory` == "EXTERNAL") #1175124 
sum(oilspill2$`All Costs`[oilspill2$`Cause Subcategory` == "INTERNAL"]) / sum(oilspill2$`Cause Subcategory` == "INTERNAL") #345434


#build model to predict pipeline shutdown
oilspill3 <- oilspill[,c("Pipeline Location", "Pipeline Type", "Liquid Type", "Liquid Ignition", 
                         "Liquid Explosion", "Pipeline Shutdown")]

sum(is.na(oilspill3))

oilspill3 <- oilspill3 %>% filter_at(vars("Pipeline Shutdown"), all_vars(!is.na(.))) #5NA remaining
oilspill3 <- oilspill3 %>% filter_at(vars("Pipeline Type"), all_vars(!is.na(.))) #5NA remaining

oilspill3$`Pipeline Shutdown` <- factor(oilspill3$`Pipeline Shutdown`)
oilspill3$`Pipeline Location` <- factor(oilspill3$`Pipeline Location`)
oilspill3$`Pipeline Type` <- factor(oilspill3$`Pipeline Type`)
oilspill3$`Liquid Type` <- factor(oilspill3$`Liquid Type`)
oilspill3$`Liquid Ignition` <- factor(oilspill3$`Liquid Ignition`) 
oilspill3$`Liquid Explosion` <- factor(oilspill3$`Liquid Explosion`)

str(oilspill3)
levels(oilspill3$`Pipeline Shutdown`)
levels(oilspill3$`Pipeline Type`)
levels(oilspill3$`Pipeline Location`) #only 1 level
levels(oilspill3$`Liquid Type`)
levels(oilspill3$`Liquid Ignition`)
levels(oilspill3$`Liquid Explosion`) 

nrow(oilspill3[`Pipeline Location` == "ONSHORE",]) #problem here is pipeline location is 100% onshore 


reg = glm(`Pipeline Shutdown` ~ `Pipeline Type` + `Liquid Type` + `Liquid Ignition`, 
          family = binomial, data = oilspill3)
summary(reg)

prob = predict(reg, type = 'response')
classifier = ifelse(prob>0.5, "YES", "NO")
table(classifier, oilspill3$`Pipeline Shutdown`)
mean(classifier == oilspill3$`Pipeline Shutdown`)*100 #59.08028 correct 

train <- sample.split(Y = oilspill3$`Pipeline Shutdown`, SplitRatio = 0.7)
trainset <- subset(oilspill3, train==T)
testset <- subset(oilspill3, train==F)


m <- rpart(`Pipeline Shutdown` ~ ., data = trainset, method = 'class',
            control = rpart.control(minsplit = 20, cp = 0))
rpart.plot(m, nn= T, main = "Maximal Tree for Pipeline shutdown")
print(m)
printcp(m)
plotcp(m, main = "Subtrees for Pipeline shutdown")
cp1 <- 0.025
m2 <- prune(m, cp = cp1)
printcp(m2)
rpart.plot(m2, nn= T, main = "Optimal Tree for Pipeline shutdown")
m2$variable.importance

cart.predict <- predict(m2, newdata = trainset, type = "class")
results <- data.frame(trainset, cart.predict)
mean(cart.predict == results$Pipeline.Shutdown) #0.5991091

cart.predict.test <- predict(m2, newdata = testset, type = "class")
results <- data.frame(testset, cart.predict.test)
mean(cart.predict.test == results$Pipeline.Shutdown) #0.6064935

