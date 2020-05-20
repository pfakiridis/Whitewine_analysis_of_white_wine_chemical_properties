#1st Objective (partitioning clustering) 

#WHITEWINE COURSEWORK NO. 1 

library(readxl) 
Whitewine <- read_excel("C:/Users/Vicky/Desktop/DM&ML/CW1/Whitewine.xlsx") 
View(Whitewine) 
head(Whitewine) 
str(Whitewine) 
summary(Whitewine) 

## Once we view the structure of our date, we need to detect potential outliers.  #  We need to remove the 12th column as it consist of  

x=Whitewine[,-12] summary(x) 
boxplot(x) 

install.packages("tidyr") 
install.packages("ggplot2") 

library(tidyr) library(ggplot2) 

x %>% gather() %>% head() ggplot(gather(x), aes(value)) + geom_histogram(bins = 10) + facet_wrap(~key, scales = 'free_x') 

ggplot(stack(x), aes(x = ind, y = values)) + geom_boxplot() 

boxplot(x$`residual sugar`, main="residual sugar") 
boxplot(x$`free sulfur dioxide`, main=" free sulfur dioxide") 
boxplot(x$`total sulfur dioxide`, main=" total sulfur dioxide") 

Whitewine_nooutliers <- read_excel("C:/Users/Vicky/Desktop/DM&ML/CW1/Whitewine.nooutliers.xlsx") 
View(Whitewine_nooutliers) 

x<- Whitewine_nooutliers[,-12] 
str(x) 

# SCALING  
## The outliers have been removed form the data set so we can proceed with scaling the data 

data.train<-scale(x) 
summary(x) 

# CLUSTERING 

y=Whitewine_nooutliers$quality 

## We are now trying the NbClust package in order to determine the optimal number of clusters for this dataset  

pkgs <- c("factoextra",  "NbClust") install.packages(pkgs) 

library(factoextra) library(NbClust) 

set.seed(123) 
Clustno <- NbClust(x, distance = "euclidean", min.nc = 2, max.nc = 15,  
                   method = "kmeans") 


## First we will try manually to assign various numbers of clusters to our kmeans in order to validate the NbClust result 

kc2<-kmeans(x,2) kc2 
table(y, kc2$cluster) 

kc3<-kmeans(x,3) kc3 
table(y, kc3$cluster) 

kc4<-kmeans(x,4) kc4 
table(y, kc4$cluster) 

kc<-kmeans(x,5) 
table(y, kc$cluster) 

kc<-kmeans(x,6) 
table(y, kc$cluster) 


#2nd Objective (MLP) 

library(readxl) library(neuralnet) library(ggplot2) library(reshape2) library(gridExtra) library(neuralnet) library(grid) 
library(MASS) library(caret) library(e1071) library(Metrics) 

ExchangeUSD <- read_excel("C:/Users/Vicky/Desktop/DM&ML/CW1/ExchangeUSD.xlsx") 

View(ExchangeUSD) summary(ExchangeUSD) str(ExchangeUSD) 

qplot(ExchangeUSD$`USD/EUR`, geom="histogram")  

#normalisation 

ExchangeUSD <- ExchangeUSD[,3] 
rate <- ts (ExchangeUSD) ## The data set is defined as time series 

norm_data <- sapply(ExchangeUSD, function(x) (x - min(x))/(max(x) - min(x))) class(norm_data) norm_data <- data.frame(norm_data) head(norm_data) norm_data 
write.table(norm_data, file = "newdata.csv", sep=",", row.names = F) 

Mydata1 <- read_excel("C:/Users/Vicky/Desktop/DM&ML/CW1/newdata_2inp.xls.xlsx") View(Mydata1) str(Mydata1) summary(Mydata1) 

##Training a Model on the Data (using the neuralnet function) 
##MPL-NN WITH TWO INPUTS 

train1 <- Mydata1[1:400,] test1 <- Mydata1[401:498,] 

##1 HIDDEN LAYER, 3 NODES, LINEAR OUTPUT, LOGISSTIC ACTIVATION FUNCTION  

set.seed(1234) 
NN1 <- neuralnet(Output ~ Input1 + Input2, data = train1, hidden=3, act.fct= "logistic", threshold = 0.01, linear.output = T) 
plot(NN1)  
NN1 

## Evaluating Model Performance 
model1_results <- compute(NN1, test1[1:2]) 
predicted1 <- model1_results$net.result 
cor(predicted1, test1$Output) 

##MPL-NN WITH 1 HIDDEN LAYERs, 7 NODES, NON - LINEAR OUTPUT, TANH ACTIVATION FUNCTION  

set.seed(1234) 
NN2 <- neuralnet(Output ~ Input1 + Input2, train1, hidden=7, 
                 act.fct= "tanh", threshold = 0.01, linear.output = F) plot(NN2)  
NN2 
## Evaluating Model Performance 
model2_results <- compute(NN2, test1[1:2]) 
predicted2 <- model2_results$net.result 
cor(predicted2, test1$Output) 

##MPL-NN WITH 2 HIDDEN LAYERs - 4 and 2 NODES, NON - LINEAR OUTPUT, LOGISSTIC ACTIVATION FUNCTION  

set.seed(1234) 
NN3 <- neuralnet(Output ~ Input1 + Input2, train1, hidden=c(4,2),stepmax=1e6,  
                 act.fct= "logistic", threshold = 0.01, linear.output = F) plot(NN3)  
NN3 

## Evaluating Model Performance model3_results <- compute(NN3, test1[1:2]) predicted3 <- model3_results$net.result cor(predicted3, test1$Output) 

##MPL-NN WITH 2 HIDDEN LAYERs - 10 and 6 NODES, LINEAR OUTPUT, LOGISSTIC ACTIVATION FUNCTION  

set.seed(1234) 
NN4 <- neuralnet(Output ~ Input1 + Input2, train1, hidden=c(10,6), 
                 act.fct= "tanh", threshold = 0.01, linear.output = T) plot(NN4)  
NN4 

## Evaluating Model Performance model4_results <- compute(NN4, test1[1:2]) predicted4 <- model4_results$net.result cor(predicted4, test1$Output) 

##MPL-NN WITH THREE INPUTS 
Mydata2 <- read_excel("C:/Users/Vicky/Desktop/DM&ML/CW1/newdata_3inp.xls.xlsx") 
View(Mydata2) 
summary(Mydata2) 

train2 <- Mydata2[1:400,]
test2 <- Mydata2[401:497,] 

## 1 HIDDEN LAYER - 4 NODES, LINEAR OUTPUT, LOGISSTIC ACTIVATION FUNCTION  

set.seed(1234) 
NN5 <- neuralnet(Output ~ Input1 + Input2 + Input3, train2, hidden=4, 
                 act.fct= "logistic", threshold = 0.01, linear.output = T) plot(NN5)  
NN5 

## Evaluating Model Performance model5_results <- compute(NN5, test2[1:3]) predicted5 <- model5_results$net.result cor(predicted5, test2$Output) 

## 1 HIDDEN LAYER - 6 NODES, NON - LINEAR OUTPUT, TANH ACTIVATION FUNCTION  

set.seed(1234) 
NN6 <- neuralnet(Output ~ Input1 + Input2 + Input3, train2, hidden=6, act.fct= "tanh", threshold = 0.01, linear.output = F) 
plot(NN6)  
NN6 

## Evaluating Model Performance model6_results <- compute(NN6, test2[1:3]) predicted6 <- model6_results$net.result cor(predicted6, test2$Output) 

## 2 HIDDEN LAYERS - 3 and 6 NODES, LINEAR OUTPUT, TANH ACTIVATION FUNCTION  

set.seed(1234) 
NN7 <- neuralnet(Output ~ Input1 + Input2 + Input3, train2, hidden=c(3,6), 
                 act.fct= "tanh", threshold = 0.01, linear.output = T) 
plot(NN7)  
NN7 

## Evaluating Model Performance model7_results <- compute(NN7, test2[1:3]) predicted7 <- model7_results$net.result cor(predicted7, test2$Output) 

## 2 HIDDEN LAYERS - 10 and 4 NODES,  LINEAR OUTPUT, LOGISTIC ACTIVATION FUNCTION  

NN8<- neuralnet(Output ~ Input1 + Input2 + Input3, train2, hidden=c(10,4), 
                act.fct= "logistic", threshold = 0.01, linear.output = T) 
plot(NN8)  
NN8 

## Evaluating Model Performance model8_results <- compute(NN8, test2[1:3]) predicted8 <- model8_results$net.result cor(predicted8, test2$Output) 

## 2 HIDDEN LAYERS - 7 and 4 NODES, LINEAR OUTPUT, TANH ACTIVATION FUNCTION 

set.seed(1234) 
NN9 <- neuralnet(Output ~ Input1 + Input2 + Input3, train2, hidden=c(7,4), act.fct= "tanh", threshold = 0.01, linear.output = T) 
plot(NN9)  
NN9 

## Evaluating Model Performance 
model9_results <- compute(NN9, test2[1:3]) 
predicted9 <- model9_results$net.result 
cor(predicted9, test2$Output) 

##MPL-NN WITH FOUR INPUTS 

Mydata3 <- read_excel("C:/Users/Vicky/Desktop/DM&ML/CW1/newdata_4inp.xls.xlsx") 
View(Mydata3) 
summary(Mydata3) 

train3 <- Mydata3[1:400,] test3 <- Mydata3[401:496,] 

## 1 HIDDEN LAYER - 3 NODES, NON- LINEAR OUTPUT, TANH ACTIVATION FUNCTION 

set.seed(1234) 
NN10 <- neuralnet(Output ~ Input1 + Input2 + Input3 + Input4, train3, hidden=3, 
                  act.fct= "tanh", threshold = 0.01, linear.output = F) plot(NN10)  
NN10 

## Evaluating Model Performance model10_results <- compute(NN10, test3[1:4]) predicted10 <- model10_results$net.result cor(predicted10, test3$Output) 

## 1 HIDDEN LAYER - 5 NODES , NON- LINEAR OUTPUT, LOGISTIC ACTIVATION FUNCTION 

set.seed(1234) 
NN11 <- neuralnet(Output ~ Input1 + Input2 + Input3 + Input4, train3, hidden=5, 
                  act.fct= "logistic", threshold = 0.01, linear.output = F) 
plot(NN11)  
NN11 

## Evaluating Model Performance model11_results <- compute(NN11, test3[1:4]) predicted11 <- model11_results$net.result cor(predicted11, test3$Output) 


## 1 HIDDEN LAYER - 10 NODES, LINEAR OUTPUT, LOGISTIC ACTIVATION FUNCTION 

set.seed(1234) 
NN12 <- neuralnet(Output ~ Input1 + Input2 + Input3 + Input4, train3, hidden=10, 
                  act.fct= "logistic", threshold = 0.01, linear.output = T) plot(NN12)  
NN12 

## Evaluating Model Performance model12_results <- compute(NN12, test3[1:4]) predicted12 <- model12_results$net.result cor(predicted12, test3$Output) 

## 2 HIDDEN LAYERS - 9 and 4 NODES, LINEAR OUTPUT, TANH ACTIVATION FUNCTION 

set.seed(1234) 
NN13 <- neuralnet(Output ~ Input1 + Input2 + Input3 + Input4, train3, hidden=c(9,4), 
                  act.fct= "tanh", threshold = 0.01, linear.output = T) 
plot(NN13)  
NN13 

## Evaluating Model Performance model13_results <- compute(NN13, test3[1:4]) predicted13 <- model13_results$net.result cor(predicted13, test3$Output) 

## 2 HIDDEN LAYERS - 6 and 3 NODES, NON- LINEAR OUTPUT, LOGISTIC ACTIVATION FUNCTION set.seed(1234) 
NN14 <- neuralnet(Output ~ Input1 + Input2 + Input3 + Input4, train3, hidden=c(6,3), 
                  act.fct= "logistic", threshold = 0.01, linear.output = F) plot(NN14)  
NN14 

## Evaluating Model Performance model14_results <- compute(NN14, test3[1:4]) predicted14 <- model14_results$net.result cor(predicted14, test3$Output) 

## PERFORMANCE INDICES 

Original_train <- ExchangeUSD[1:400,3] 
Original_test <- (ExchangeUSD[401:500,3]) 
head(Original_train) 

Output_min <- min(Original_train) 
Output_max <- max(Original_train) 

Original_test <- apply(Original_test, 2, as.numeric) 

## Performance index - RMSE 

## NN1 

unnormalize <- function(x, min, max) {    return( (max - min)*x + min )} 

unnorm_data1 <- unnormalize(predicted1, Output_min, Output_max) 

rmse(Original_test[1:98,], unnorm_data1) mape(Original_test[1:98,], unnorm_data1) mae(Original_test[1:98,], unnorm_data1) 
## NN2 unnorm_data2 <- unnormalize(predicted2, Output_min, Output_max) unnorm_data2 

rmse(Original_test[1:98,], unnorm_data2) mape(Original_test[1:98,], unnorm_data2) mae(Original_test[1:98,], unnorm_data2) 

## NN3 unnorm_data3 <- unnormalize(predicted3, Output_min, Output_max) unnorm_data3 

rmse(Original_test[1:98,], unnorm_data3) mape(Original_test[1:98,], unnorm_data3) mae(Original_test[1:98,], unnorm_data3) 

## NN4 unnorm_data4 <- unnormalize(predicted4, Output_min, Output_max) unnorm_data4 

rmse(Original_test[1:98,], unnorm_data4) mape(Original_test[1:98,], unnorm_data4) mae(Original_test[1:98,], unnorm_data4) 

## NN5 unnorm_data5 <- unnormalize(predicted5, Output_min, Output_max) unnorm_data5 

rmse(Original_test[1:97,], unnorm_data5) mape(Original_test[1:97,], unnorm_data5) mae(Original_test[1:97,], unnorm_data5) 

## NN6 
unnorm_data6 <- unnormalize(predicted6, Output_min, Output_max) unnorm_data6 

rmse(Original_test[1:97,], unnorm_data6) mape(Original_test[1:97,], unnorm_data6) mae(Original_test[1:97,], unnorm_data6) 

## NN7 unnorm_data7 <- unnormalize(predicted7, Output_min, Output_max) unnorm_data7 

rmse(Original_test[1:97,], unnorm_data7) mape(Original_test[1:97,], unnorm_data7) mae(Original_test[1:97,], unnorm_data7) 

## NN8 unnorm_data8 <- unnormalize(predicted8, Output_min, Output_max) unnorm_data8 

rmse(Original_test[1:97,], unnorm_data8) mape(Original_test[1:97,], unnorm_data8) mae(Original_test[1:97,], unnorm_data8) 

## NN9 unnorm_data9 <- unnormalize(predicted9, Output_min, Output_max) unnorm_data9 

rmse(Original_test[1:97,], unnorm_data9) mape(Original_test[1:97,], unnorm_data9) mae(Original_test[1:97,], unnorm_data9) 

## NN10 unnorm_data10 <- unnormalize(predicted10, Output_min, Output_max) unnorm_data10 

rmse(Original_test[1:96,], unnorm_data10) mape(Original_test[1:96,], unnorm_data10) mae(Original_test[1:96,], unnorm_data10) 

## NN11 unnorm_data11 <- unnormalize(predicted11, Output_min, Output_max) unnorm_data11 

rmse(Original_test[1:96,], unnorm_data11) mape(Original_test[1:96,], unnorm_data11) mae(Original_test[1:96,], unnorm_data11) 

## NN12 unnorm_data12 <- unnormalize(predicted12, Output_min, Output_max) unnorm_data12 

rmse(Original_test[1:96,], unnorm_data12) mape(Original_test[1:96,], unnorm_data12) mae(Original_test[1:96,], unnorm_data12) 

## NN13 unnorm_data13 <- unnormalize(predicted13, Output_min, Output_max) unnorm_data13 

rmse(Original_test[1:96,], unnorm_data13) mape(Original_test[1:96,], unnorm_data13) mae(Original_test[1:96,], unnorm_data13) 

## NN14 unnorm_data14 <- unnormalize(predicted14, Output_min, Output_max) unnorm_data14 

rmse(Original_test[1:96,], unnorm_data14) 
mape(Original_test[1:96,], unnorm_data14) mae(Original_test[1:96,], unnorm_data14) 

##Exporting the results in Excel 

finalresults <- cbind.data.frame(Original_test[1:98,], unnorm_data2) data.frame(finalresults) 

write.table(finalresults, file = "C:/Users/Vicky/Desktop/DM&ML/CW1/finalresults.csv", sep=",", row.names = F) 

#3rd Objective (SVR) 

## 2 Inputs / Linear 

train1 <- Mydata1[1:400,] test1 <- Mydata1[401:498,] 

model1 <- svm(Output ~ Input1 + Input2,data=train1,kernel='linear', 
              epsilon=1,cost=0.1) 
preds1 <- predict(model1,test1) 
summary(model1) 

rmse(Original_test[1:98,], preds1) mape(Original_test[1:98,], preds1) mae(Original_test[1:98,], preds1) 


## 2 Inputs / Radial 
model2 <- svm(Output ~ Input1 + Input2,data=train1,kernel='radial',epsilon=0.01, 
gamma=0.05,cost=10) 
preds2 <- predict(model2,test1) 
summary(model2) 

rmse(Original_test[1:98,], preds2) 
mape(Original_test[1:98,], preds2) mae(Original_test[1:98,], preds2) 

## 3 Inputs / Linear train2 <- Mydata2[1:400,] test2 <- Mydata2[401:497,] 

model3 <- svm(formula = Output ~ Input1 + Input2 + Input3, data = train2,  
              kernel = "linear", epsilon = 1, cost = 0.1) 
preds3 <- predict(model3,test2) 
summary(model3) 

rmse(Original_test[1:97,], preds3) mape(Original_test[1:97,], preds3) mae(Original_test[1:97,], preds3) 

## 3 Inputs / Radial 
model4 <- svm(Output ~ Input1 + Input2 + Input3,data=train2,kernel='radial', epsilon=0.5, gamma=0.05,cost=3) 
preds4 <- predict(model4,test2) 
summary(model4) 

rmse(Original_test[1:97,], preds4) mape(Original_test[1:97,], preds4) mae(Original_test[1:97,], preds4) 

## 4 Inputs / Linear train3 <- Mydata3[1:400,] test3 <- Mydata3[401:496,] str(train3) 

model5 <- svm(formula = Output ~ Input1 + Input2 + Input3 + Input4, data = train3, kernel = "linear",  
              epsilon = 1, cost = 0.1) 
preds5 <- predict(model5,test3) 
summary(model5) 

rmse(Original_test[1:96,], preds5) mape(Original_test[1:96,], preds5) mae(Original_test[1:96,], preds5) 

## 4 Inputs / Radial 
model6 <- svm(formula = Output ~ Input1 + Input2 + Input3 + Input4, data = train3, kernel = "radial",  epsilon = 0.05, gamma =0.01, cost =3) 
preds6 <- predict(model6,test3) 
summary(model6) 

rmse(Original_test[1:96,], preds6) mape(Original_test[1:96,], preds6) mae(Original_test[1:96,], preds6) 

##Exporting the results in Excel 
final <- cbind.data.frame(Original_test[1:98,], preds1) data.frame(finalresults) 

write.table(final, file = "C:/Users/Vicky/Desktop/DM&ML/CW1/final123.csv", sep=",", row.names = F) 
