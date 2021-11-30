rm(list=ls())
options(scipen=999)

#set working directory
dir <- choose.dir()
setwd(dir)
getwd()

#reading the dataset
data <- read.csv("day.csv")

str(data)

# data summary
summary(data)
str(data)

#checking for missing values ----
data[rowSums(is.na(data)) > 0,] #no NA values
apply(data,2,function(x){sum(is.na(x))})
    
ncol(data)

# check for outliers and missing ----

#IQR method
quantile(data$cnt)

# using box plot for checking outliers
boxplot(data$cnt)$out
  components_boxplot <- boxplot(data$cnt,main='Total rentals')

#checking the number of rows within the 1st quarter or below 25% quantile
length(data$cnt[data$cnt<3152])/length(data)
#that is a lot of data. We can not use IQR method

#looking at z score method

stats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min,max=max, UC=UC, LC=LC ))
}

str(data)

vars <- c("atemp","hum","windspeed","cnt")

diag_stats<-t(data.frame(apply(data[vars], 2, stats)))
#View(diag_stats) #there are outliers in column windspeed and humidity

#Outliers
length(data$hum[data$hum<=0.20060678])
length(data$windspeed[data$windspeed>0.4229798])

#removing the 4 rows
data <- data[data$hum>0.20060678,]
data <- data[data$windspeed<0.4229798,]

#check for multicollinearity ----
#install.packages("corrplot")
library(corrplot)
#data$dteday <- as.Date(data$dteday)
corrplot(round(cor(data[,c(3:16)]),2),method='color')

#we can see that there is high correlation between month and season
# we might want to remove one of them and check our modelling results

library(dplyr)
#Checking if the season and month are classified correctly
#data %>% select(season,mnth) %>% distinct()
#this shows that the months are incorrectly classified.

#creating new column season_new----
#dropping column season from the dataset and creating a new column for seasons
#data <- data %>% select(season)
# winter <- c(12,1,2) #1
# spring <- c(3,4,5) #2
# summer <- c(6,7,8) #3
# autumn <- c(9,10,11) #4
# data <- data %>% mutate(season_new=ifelse(mnth %in% winter,1,
#                                        ifelse(mnth %in% summer,3,
#                                               ifelse(mnth %in% autumn,4,
#                                                      ifelse(mnth %in% spring,2,"missing")))))
# 
# #check
# data$season_new <- as.numeric(data$season_new)
# write.csv(data,"data.csv")

#str(data)
#data %>% select(season_new,mnth) %>% distinct()

str(data)
#ncol(data)

corrplot(round(cor(data[,c(3:16)]),2),method='color')
#looking at this plot we see that there is very high correlation between
# registered, casual, temp, atemp, cnt
# so removing registered, casual, temp
corrplot(round(cor(data[,c(3,4,5,6,7,8,10,11,12,15,16)]),2),method='color')
#the matrix looks much better now.
colnames(data)
data <- data[,c(3,4,5,6,7,8,9,11,12,13,16)]

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
data2 <- data1 %>% select(-cnt)

#clustering analysis using k-means ----
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data2, k )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

library(purrr)
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

cor(data$season,data$mnth)
str(data2)

sum(km$cluster==1)/731 #50% of the dataset
sum(km$cluster==2)/731 #50% of the dataset
#our clusters divide our data equally

clusters <- km$cluster

#combining cluster results with original dataset ----
data_new <- cbind(data1,clusters)

#creating separate dataset for each of the clusters
cluster1_data <- subset(data_new,data_new$clusters==1)
cluster2_data <- subset(data_new,data_new$clusters==2)

#check
nrow(cluster1_data)+nrow(cluster2_data)==nrow(data_new)

#checking seasons and months in each clusters ----
unique(cluster1_data$season) # this has 3, 4, 1 seasons summer, autumn, winter
unique(cluster1_data$mnth) # July till December data

unique(cluster2_data$season) # this has 1,2,3 winter, spring, summer
unique(cluster2_data$mnth) # January till June

# please use cluster1_data and cluster2_data as your main datasets

cor(data$mnth,data$season_new)
