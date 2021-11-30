#set working directory
dir <- choose.dir()
setwd(dir)
getwd()

#reading the dataset
setwd("C:/Users/Akisha Singh/Downloads")
data <- read.csv("day.csv")

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

x <- cor(data$weekday,data$workingday)
plot(data$weekday,data$workingday)

#we can see that there is high correlation between month and season
# we might want to remove one of them and check our modelling results

#creating factors for categorical variables ----
factor_variables <- c("season","yr","mnth","holiday",
                      "weekday","workingday","weathersit")

data1 <- data
data1[factor_variables] <- lapply(data1[factor_variables], factor)
str(data1)


#normalizing not required for temp, atemp, humidity, windspeed

# we might want to remove casual and registered, unless we want 2 prediction models   

#clustering
library(dplyr)

# removind date column as well
data2 <- data1 %>% select(-casual,-registered,-instant,-dteday,-cnt,-mnth)


#clustering analysis using k-means ----
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data2, k )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
install.packages("purr")
#library(purrr)
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# we select k=2 ----
km<- kmeans(data2,2)

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 11))
# label x-axes
axis(1, at = c(1:10), labels = names(data2))


# plot centroids
for (i in c(1:2))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))
# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:2)))

# sum(data$season==1)/731
# sum(data$season==2)/731
# sum(data$season==3)/731
# sum(data$season==4)/731

sum(km$cluster==1)/731 #57% of the dataset
sum(km$cluster==2)/731 #43% of the dataset
#our clusters divide our data equally

clusters <- km$cluster

#combining cluster results with original dataset ----
data_new <- cbind(data,clusters)
#View(data_new)
#creating separate dataset for each of the clusters
cluster1_data <- subset(data_new,data_new$clusters==1)
cluster2_data <- subset(data_new,data_new$clusters==2)

#check
nrow(cluster1_data)+nrow(cluster2_data)==nrow(data_new)

#checking which all seasons and months are in each of the 2 clusters
unique(cluster1_data$weekday) # this has 0,1,2 -- sunday, monday tuesday, wed

unique(cluster2_data$weekday) # this has thur, fri, sat


# randomly generate training and validation sets
#training <- sample(toyota.corolla.df$Id, 600)
#validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

### removing season of cluster 1
cluster1_data_1 <- cluster1_data %>% select(-season,-registered,-casual,-clusters,-instant,-yr, dteday )
str(cluster1_data_1)

#partitioning data of cluster 1

train.index <- sample(row.names(cluster1_data_1), 0.7*dim(cluster1_data_1)[1])  
valid.index <- setdiff(row.names(cluster1_data_1), train.index)  
train.df <- cluster1_data_1[train.index, ]
valid.df <- cluster1_data_1[valid.index, ]

#running linear regression on cluster 1
reg <- lm(cnt~., data=cluster1_data_1, subset= train.df,
          na.action=na.exclude)
pred_t <- predict(reg, na.action=na.pass)
pred_v <- predict(reg, data=valid.df, na.action=na.pass)

## evaluate performance on cluster 1
library(forecast)
accuracy(pred_t, train.df$cnt)
accuracy(pred_v, valid.df$cnt)

### removing season of cluster 2
cluster2_data_2 <- cluster2_data %>% select(-season,-registered,-casual,-clusters,-instant,-yr, -dteday )
str(cluster1_data_1)

#partitioning data of cluster 2

train.index <- sample(row.names(cluster2_data_2), 0.7*dim(cluster2_data_2)[1])  
valid.index <- setdiff(row.names(cluster2_data_2), train.index)  
train.df2 <- cluster2_data_2[train.index, ]
valid.df2 <- cluster2_data_2[valid.index, ]

#running linear regression on cluster 2
reg2 <- lm(cnt~., data=cluster2_data_2, subset = train.df2,
          na.action=na.exclude)
pred_t2 <- predict(reg2, na.action=na.pass)
pred_v2 <- predict(reg2,data=valid.df2, na.action=na.pass)

## evaluate performance on cluster 2
accuracy(pred_t2, train.df2$cnt)
accuracy(pred_v2, valid.df2$cnt)

### removing season of combined cluster
data_new_1 <- data_new %>% select(-season,-registered,-casual,-clusters,-instant,-yr, dteday )
str(data_new_1)

#partitioning data of combined cluster

train.index <- sample(row.names(data_new_1), 0.7*dim(data_new_1)[1])  
valid.index <- setdiff(row.names(data_new_1), train.index)  
train.df_new <- data_new_1[train.index, ]
valid.df_new <- data_new_1[valid.index, ]

#running linear regression on combined cluster
regd <- lm(cnt~., data=data_new_1,subset=train.df_new,
          na.action=na.exclude)
pred_d <- predict(regd, na.action=na.pass)
pred_v <- predict(regd, data = valid.df_new, na.action=na.pass)

## evaluate performance on combined cluster
accuracy(pred_d, train.df_new$cnt)
accuracy(pred_v, valid.df_new$cnt)
