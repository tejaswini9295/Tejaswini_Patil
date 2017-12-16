#loading the data
voice <- read.table("voice.csv", header = TRUE, sep = ",")
voice_test <- read.table("voice_test.csv", header = TRUE, sep = ",")

#loading libraries
#RWeka for J48 (aka C4.5 algorithm) and caret for loading ggplot2 and 
#lattice libraries, rpart and rattle for plotting functions,
#randomforest for variable importance plotting
install.packages("RWeka")
install.packages("caret")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("randomForest")
library(RWeka)
library(caret)
library(rattle)
library(rpart.plot)
library(randomForest)

#checking for missing values
table(is.na(voice))
table(is.na(voice_test))

#converting occupancy from numeric to factor for C4.5 algorithm
voice$label <- as.factor(voice$label)
voice_test$label <- as.factor(voice_test$label)

#reviewing data types for algo
str(voice)
str(voice_test)

#randomising voice table
set.seed(21)
g <- runif(nrow(voice))
voice <- voice[order(g),]
View(voice)

#applying C4.5 to voice
m1 <- J48(label~. , data = voice)
#summarizing the model m1
summary(m1)

#applying C4.5 to voice
m1 <- J48(label~. , data = voice_test)
#summarizing the model m1
summary(m1)

#plotting the model
png("dtre3.png", res = 100, height = 2000, width = 4000)
plot(m1)
dev.off()

#making predictions for voice_test
predictions <- predict(m1, voice_test[,1:21])
table(predictions, voice_test$label)
#predictions female male
#female    551    8
#male        5 1576
#accuracy: 99.3925 %

#plot cp
form <- J48(label ~ ., data = voice)
m1 <- rpart(form,data=voice,control=rpart.control(minsplit=20,cp=0))
summary(m1)
plotcp(m1)

#rattle() GUI is used for co-relation plot and distribution plot
rattle()
corrplot(cor(voice[,c(0,-21,-22)]))

#plotting variable importance chart
fit2 <- randomForest(label~.,data = voice)
varImpPlot(fit2, type = 2)
