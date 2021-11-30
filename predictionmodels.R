install.packages("dplyr")
library(dplyr)
library(caret)
library(neuralnet)
library(forecast)
library(gains)
library(MASS)
library(ggplot2)

plot(density(data$hum))
plot(density(data$windspeed))


plot(data$mnth, data$cnt)
plot(data$holiday, data$cnt)
plot(data$weekday, data$cnt)
plot(data$atemp, data$cnt)
plot(data$temp, data$cnt)
plot(data$hum, data$cnt)
plot(data$windspeed, data$cnt)
plot(data$casual, data$cnt)
plot(data$registered, data$cnt)

ggplot(data, aes(x=hum))+
  geom_point(aes(y=cnt))+
  stat_smooth(method = "lm", formula = cnt ~ hum , size = 1) 
  

p <- ggplot(data, aes(x = hum, y = cnt)) + geom_point()
p
p+ stat_smooth(method = "lm", formula = cnt ~ hum, size = 1)

#reading the dataset
data <- read.csv("day.csv")

view(data)
# data summary
summary(data)
str(data)

#checking for missing values ----
data[rowSums(is.na(data)) > 0,] #no NA values
apply(data,2,function(x){sum(is.na(x))})

ncol(data)

#check for multicollinearity ----
#install.packages("corrplot")
library(corrplot)
#data$dteday <- as.Date(data$dteday)
corrplot(round(cor(data[,c(3:16)]),2),method='color')

#we can see that there is high correlation between month and season
# we might want to remove one of them and check our modelling results


#Checking if the season and month are classified correctly
data %>% select(season,mnth) %>% distinct()
#this shows that the months are incorrectly classified.

#creating new column season_new----
#dropping column season from the dataset and creating a new column for seasons
str(data)
data <- data %>% select(-season)
winter <- c(12,1,2) #1
spring <- c(3,4,5) #2
summer <- c(6,7,8) #3
autumn <- c(9,10,11) #4
data <- data %>% mutate(season_new=ifelse(mnth %in% winter,1,
                                          ifelse(mnth %in% summer,3,
                                                 ifelse(mnth %in% autumn,4,
                                                        ifelse(mnth %in% spring,2,"missing")))))

str(data)
summary(data_new)
#check
data <- data[,-c(3)]
data$season_new <- as.numeric(data$season_new)
data %>% select(season_new,mnth) %>% distinct()

str(data)
ncol(data)

corrplot(round(cor(data[,c(3:16)]),2),method='color')
#looking at this plot we see that there is very high correlation between
# registered, casual, temp, atemp, cnt
# so removing registered, casual, temp
corrplot(round(cor(data[,c(3,4,5,6,7,8,10,11,12,15,16)]),2),method='color')
#the matrix looks much better now.

data <- data[,c(3,4,5,6,7,8,10,11,12,15,16)]
str(data)
#creating factors for categorical variables ----
factor_variables <- c("season_new","yr","mnth","holiday",
                      "weekday","workingday","weathersit")

data1 <- data
str(data1)
data1[factor_variables] <- lapply(data1[factor_variables], factor)
str(data1)


#normalizing not required for temp, atemp, humidity, windspeed

# we might want to remove casual and registered, unless we want 2 prediction models   

#clustering
library(dplyr)

# removind date column as well
#data2 <- data1 %>% select(-cnt)
data2 <- data1[, -c(10)]
#clustering analysis using k-means ----
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data2, k )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# we select k=2 ----
km<- kmeans(data2,2)

ncol(data2)

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 10))
# label x-axes
axis(1, at = c(1:10), labels = names(data2))


# plot centroids
for (i in c(1:2))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 2, 3),
                                                       "black", "dark grey"))
# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:2)))

cor(data$season_new,data$mnth)
str(data2)

sum(km$cluster==1)/731 #50% of the dataset
sum(km$cluster==2)/731 #50% of the dataset
#our clusters divide our data equally

clusters <- km$cluster

#combining cluster results with original dataset ----
data_new <- cbind(data1,clusters)

#creating separate dataset for each of the clusters
cluster1_data <- subset(data_new,data_new$clusters==1)
cluster1_data <- data_new
cluster2_data <- subset(data_new,data_new$clusters==2)
summary(cluster1_data)
nrow(cluster1_data)
#check
nrow(cluster1_data)+nrow(cluster2_data)==nrow(data_new)

#checking seasons and months in each clusters ----
unique(cluster1_data$season_new) # this has 3, 4, 1 seasons summer, autumn, winter
unique(cluster1_data$mnth) # July till December data

unique(cluster2_data$season_new) # this has 1,2,3 winter, spring, summer
unique(cluster2_data$mnth) # January till June

# winter <- c(12,1,2) #1
# spring <- c(3,4,5) #2
# summer <- c(6,7,8) #3
# autumn <- c(9,10,11) #4

# please use cluster1_data and cluster2_data as your main datasets


#install.packages(("dplyr"))
#library(dplyr)
str(cluster1_data)
cluster1_data <- cluster1_data[,c('yr','mnth','holiday','weekday','workingday','weathersit',
                                  'atemp','hum','windspeed','season_new','cnt')]

cluster2_data <- cluster2_data[,c('yr','mnth','holiday','weekday','workingday','weathersit',
                               'atemp','hum','windspeed','season_new','cnt')]

combined_cluster <- data_new[,c('yr','mnth','holiday','weekday','workingday','weathersit',
                                'atemp','hum','windspeed','season_new','cnt')]









#----------------------this section looks at the neural nets-------------------#






str(cluster1_data)

#need to convert back to numeric to allow data into neural nets
cluster1_data$yr<- as.numeric(as.character(cluster1_data$yr))
cluster1_data$mnth<- as.numeric(as.character(cluster1_data$mnth))
cluster1_data$holiday<- as.numeric(as.character(cluster1_data$holiday))
cluster1_data$workingday<- as.numeric(as.character(cluster1_data$workingday))
cluster1_data$weathersit<- as.numeric(as.character(cluster1_data$weathersit))
cluster1_data$weekday<- as.numeric(as.character(cluster1_data$weekday))
cluster1_data$season_new<- as.numeric(as.character(cluster1_data$season_new))
str(cluster1_data)
length(cluster1_data$cnt)

# Normalize the data
#need to scale
maxs_cluster1 <- apply(cluster1_data, 2, max) 
mins_cluster1 <- apply(cluster1_data, 2, min)
scaled_cluster1<- as.data.frame(scale(cluster1_data, center = mins_cluster1, 
                              scale = maxs_cluster1 - mins_cluster1))
str(cluster1_data)




# Split the data into training and testing set
index_cluster1 <- sample(1:nrow(cluster1_data), round(0.7 * nrow(cluster1_data)))
train_cluster1 <- scaled_cluster1[index_cluster1,]
test_cluster1 <- scaled_cluster1[-index_cluster1,]






set.seed((1234))

train_rmse_1 = c()
test_rmse_1 = c()

# Build Neural Network
for (i in 1:10){
  nn_cluster1 <- neuralnet(cnt ~ ., 
                  data = train_cluster1, hidden = c(i,8), 
                  linear.output = TRUE)
  #plot(nn_cluster1)
  
  
  #predict on train
  pr.train_cluster1<- compute(nn_cluster1, train_cluster1[,1:10])
  
  # Compute mean squared error
  pr.train_cluster1 <- pr.train_cluster1$net.result * (max(cluster1_data$cnt) - min(cluster1_data$cnt)) + min(cluster1_data$cnt)
  train.r_cluster1 <- (train_cluster1$cnt) * (max(cluster1_data$cnt) - min(cluster1_data$cnt)) + 
    min(cluster1_data$cnt)
  MSE.nn_cluster1_train <- sqrt(sum((train.r_cluster1 - pr.train_cluster1)^2) / nrow(train_cluster1))
  MSE.nn_cluster1_train
  
  
  
  # Predict on test data
  pr.test_cluster1 <- compute(nn_cluster1, test_cluster1[,1:10])
  
  # Compute mean squared error
  pr.nn_cluster1 <- pr.test_cluster1$net.result * (max(cluster1_data$cnt) - min(cluster1_data$cnt)) + min(cluster1_data$cnt)
  test.r_cluster1 <- (test_cluster1$cnt) * (max(cluster1_data$cnt) - min(cluster1_data$cnt)) + 
    min(cluster1_data$cnt)
  MSE.nn_cluster1_test <- sqrt(sum((test.r_cluster1 - pr.nn_cluster1)^2) / nrow(test_cluster1))
  MSE.nn_cluster1_test
  
  train_rmse_1[i]=MSE.nn_cluster1_train
  test_rmse_1[i]=MSE.nn_cluster1_test
  
  
}  


one_ten <- (c(1,2,3,4,5,6,7,8,9,10))
one_ten.df <- data.frame(one_ten)
combined <- data.frame(one_ten.df, train_rmse_1, test_rmse_1)


ggplot(combined, aes(x= one_ten))+
  geom_line(data=combined,aes(y=train_rmse_1, color='Train RMSE'))+
  geom_line(data=combined, aes(y=test_rmse_1, color='Test RMSE'))+
  scale_color_manual(name = "RMSE", values = c("Train RMSE" = "red", "Test RMSE" = "blue" ))+
  labs(title="RMSE of Train and Test for n Neurons")+
  xlab("n") + ylab( 'RMSE')


train_rmse_1
test_rmse_1


histo_what <- train.r_cluster1 - pr.train_cluster1


hist(histo_what)
abline(v=train_rmse_1[3])
abline(v=-train_rmse_1[3])












#-----------#ignore--------------------#

#lets now look into dummy columns and scaled clusters
# Normalize the data
#need to scale
maxs_cluster2 <- apply(cluster2_data, 2, max) 
mins_cluster2 <- apply(cluster2_data, 2, min)
scaled_cluster2<- as.data.frame(scale(cluster2_data, center = mins_cluster2, 
                                      scale = maxs_cluster2 - mins_cluster2))
str(cluster2_data)
?scale
?unscale


#need to convert back to numeric to allow data into neural nets
cluster2_data$holiday<- as.numeric(as.character(cluster2_data$holiday))
cluster2_data$workingday<- as.numeric(as.character(cluster2_data$workingday))
cluster2_data$weathersit<- as.numeric(as.character(cluster2_data$weathersit))
cluster2_data$weekday<- as.numeric(as.character(cluster2_data$weekday))
str(cluster2_data)

# Split the data into training and testing set
index_cluster2 <- sample(1:nrow(cluster2_data), round(0.7 * nrow(cluster2_data)))
train_cluster2 <- scaled_cluster2[index_cluster2,]
test_cluster2 <- scaled_cluster2[-index_cluster2,]
#------------------------ignore----------------------------#























#----------------------this section looks at the neural nets-------------------#




#dummies and scaling from here with continuation of neural nets
library(fastDummies)
str(data_new)
cluster2_data <- subset(data_new,data_new$clusters==1)
str(cluster2_data)
cluster2_data <- cluster2_data[,c('yr','mnth','holiday','weekday','workingday','weathersit',
                                  'atemp','hum','windspeed','season_new','cnt')]
cluster2_data <- dummy_cols(cluster2_data, select_columns = c('yr','mnth','holiday',
                                                                'weekday','workingday',
                                                                'weathersit','season_new'))

str(cluster2_data)
length(cluster2_data)

index_cluster2 <- sample(1:nrow(cluster2_data), round(0.7 * nrow(cluster2_data)))
train_cluster2 <- cluster2_data[index_cluster2,]
test_cluster2 <- cluster2_data[-index_cluster2,]
length(train_cluster2)
train_cluster2 <- train_cluster2[,7:43]
train_cluster2 <- train_cluster2[,-4]
test_cluster2 <- test_cluster2[,7:43]
test_cluster2 <- test_cluster2[,-4]

str(train_cluster2)
length(train_cluster2)


train_rmse_2 = c()
test_rmse_2 = c()

# Build Neural Network
for (i in 1:15){
    
  # Build Neural Network
  nn_cluster2 <- neuralnet(cnt ~ ., 
                           data = train_cluster2, hidden = c(20), 
                           linear.output = TRUE)
  #plot(nn_cluster2)
  
  pr.train_cluster2<- compute(nn_cluster2, train_cluster2[,1:36])
  pr.test_cluster2<- compute(nn_cluster2, test_cluster2[,1:36])
  
  
  MSE.nn_cluster2_train <- sqrt(sum((train_cluster2$cnt - pr.train_cluster2$net.result)^2) / nrow(train_cluster2))
  MSE.nn_cluster2_test <- sqrt(sum((test_cluster2$cnt - pr.test_cluster2$net.result)^2) / nrow(test_cluster2))
  MSE.nn_cluster2_train
  MSE.nn_cluster2_test
  
  train_rmse_2[i]=MSE.nn_cluster2_train
  test_rmse_2[i]=MSE.nn_cluster2_test
  
  
}  


one_ten <- (c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
one_ten.df <- data.frame(one_ten)
combined_2 <- data.frame(one_ten.df, train_rmse_2, test_rmse_2)


ggplot(combined_2, aes(x= one_ten))+
  geom_line(data=combined_2,aes(y=train_rmse_2), color='blue')+
  geom_line(data=combined_2, aes(y=test_rmse_2), color='red')


#accuracy(train_cluster2$cnt,pr.train_cluster2$net.result)
#accuracy(test_cluster2$cnt,pr.test_cluster2$net.result)


#predict on train
pr.train_cluster2<- compute(nn_cluster2, train_cluster2[,1:36])

# Compute mean squared error
pr.train_cluster2 <- pr.train_cluster2$net.result * (max(cluster2_data$cnt) - min(cluster2_data$cnt)) + min(cluster2_data$cnt)
train.r_cluster2 <- (train_cluster2$cnt) * (max(cluster2_data$cnt) - min(cluster2_data$cnt)) + min(cluster2_data$cnt)
MSE.nn_cluster2_train <- sqrt(sum((train.r_cluster2 - pr.train_cluster2)^2) / nrow(train_cluster2))
MSE.nn_cluster2_train





# Predict on test data
pr.test_cluster2 <- compute(nn_cluster2, test_cluster2[,1:9])

# Compute mean squared error
pr.nn_cluster2 <- pr.test_cluster2$net.result * (max(cluster2_data$cnt) - min(cluster2_data$cnt)) + min(cluster2_data$cnt)
test.r_cluster2 <- (test_cluster2$cnt) * (max(cluster2_data$cnt) - min(cluster2_data$cnt)) + min(cluster2_data$cnt)
MSE.nn_cluster2_test <- sqrt(sum((test.r_cluster2 - pr.nn_cluster2)^2) / nrow(test_cluster2))
MSE.nn_cluster2_test



#MSE for cluster 1 
accuracy(train.r_cluster1, pr.train_cluster1) #train
accuracy(test.r_cluster1, pr.nn_cluster1) #test

#MSE for Cluster 2
accuracy(train.r_cluster2, pr.train_cluster2) #train
accuracy(test.r_cluster2, pr.nn_cluster2) #test















#------------------------- this section looks at boosted trees and random forests--------------------#


library(randomForest)
install.packages("rattle")
library(rattle)
library(rpart.plot)

?randomForest

index_cluster_RF <- sample(1:nrow(cluster1_data), round(0.7 * nrow(cluster1_data)))
train_cluster_RF <- cluster1_data[index_cluster_RF,]
test_cluster_RF <- cluster1_data[-index_cluster_RF,]
length(train_cluster2)
head(train_cluster_RF)

?randomForest
?matrix
randomForest <- matrix( nrow=20,ncol = 20)
#create random forest model
model1 <- randomForest(cnt ~ ., data = train_cluster_RF,
                      ntree=(600), ptry = 18,importance = TRUE)
for (i in 1:20) {
  for (j in 1:20) {
    model1 <- randomForest(cnt ~ ., data = train_cluster_RF,
                         ntree=(300+j*50), ptry = i,importance = TRUE)
  
    RMSE_RF <- sqrt(sum((model1$predicted - train_cluster_RF$cnt)^2)/length(train_cluster_RF$cnt))
    randomForest[i,j] <- RMSE_RF
  }
  #model1$rsq
  #accuracy statistics model1
  #accuracy(model1$predicted, train_cluster_RF$cnt)
}
min(randomForest)
max(randomForest)
randomForest

train_cluster_RF[,-c(11)]
train_cluster_RF[,c(11)]
#predict test set
predict_train_RF <- predict(model1, train_cluster_RF)
predict_Test_RF <- predict(model1, test_cluster_RF[,-c(11)])
str(test_cluster_RF)



accuracy(model1$predicted, train_cluster_RF$cnt)
accuracy(predict_Test_RF, test_cluster_RF$cnt)
length(predict_Test_RF)
length(model1$predicted)
length(train_cluster_RF$cnt)
length(predict_Test_RF$cnt)
sqrt(676448.3)







#---------------------------------simple tree---------------------------------------#
model_rpart <- rpart(cnt~., data =train_cluster_RF )
fancyRpartPlot(model_rpart, cex=0.8)
rpart.rules(auction_tree)

#predict test set
predict_train_tree <- predict(model_rpart, train_cluster_RF)

accuracy(predict_train_tree, train_cluster_RF$cnt)



#predict test set
predict_Test_tree <- predict(model_rpart, test_cluster_RF)

accuracy(predict_Test_tree, test_cluster_RF$cnt)


#------------------boosted trees------------#

install.packages("gbm")
require(gbm)
?gbm
bikes.Boost<-gbm(cnt ~ . ,data = train_cluster_RF,distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.0001, interaction.depth = 4)
bikes.Boost$fit
summary(bikes.Boost) #Summary gives a table of Variable Importance and a plot of Variable Importance


#Plot of Response variable with lstat variable
plot(bikes.Boost,i="lstat") 
#Inverse relation with lstat variable
plot(bikes.Boost,i="rm") 

#Generating a Prediction matrix for each Tree
trainmatrix<-predict(bikes.Boost,train_cluster_RF)#,n.trees = 10000)
accuracy(trainmatrix, train_cluster_RF$cnt)



#Generating a Prediction matrix for each Tree
predmatrix<-predict(bikes.Boost,test_cluster_RF)#,n.trees = 10000)
accuracy(predmatrix, test_cluster_RF$cnt)

dim(predmatrix) #dimentions of the Prediction Matrix



