#Pseudo COde for K Means clustering

#Unsupervised Learning
#Read Data
library(readxl)
BathSoap_Data <- read_excel("BathSoap_Data.xls", sheet = "DM_Sheet")
View(BathSoap_Data)
data <- BathSoap_Data
rm(BathSoap_Data)

str(data)
#All are numeric

#Variables that define purchase behavior and brand loyalty:
#No. of brands, Brand runs, Total Volume, Number of Transactions, Value, Average Price, 
#share to other brands, max to one brand

library(dplyr)
df <- data %>% select(`No. of Brands`, `Brand Runs`, `Total Volume`, `No. of  Trans`, Value, `Trans / Brand Runs`, `Avg. Price`, `Vol/Tran`)
#data frame created with 8 variables

str(df)
names(df)[1] <- "brand_count"
summary(df)


#Normalizing numerical variables: 
#1: Row, 2: Column
mins <- apply(df, 2, min)
maxs <- apply(df, 2, max)
scaled_data <- as.data.frame(scale(df, center = mins, scale = maxs-mins))

#K-Means clustering: 
set.seed(7)
km1 <- kmeans(scaled_data, 10, nstart = 1000)


#Using K-Means Clustering: 
library(funModeling)
df_status(data)
#2 observations with null values

Data<-data[-which(apply(data,1,function(x)any(is.na(x)))),]
#Removing null valued rows from the data

df_status(Data)
df = subset(Data, select = c(`Member id`, 
                             SEX,
                             HS,
                             CS,
                             `Affluence Index`,
                             `No. of Brands`,
                             `Trans / Brand Runs`,
                             `Avg. Price`,
                             `Pur Vol Promo 6 %`))

rownames(df) <- df$`Member id`
df <- as.data.frame(df)

df1 = subset(df, select = -`Member id`)


#Normalization
mins <- apply(df1, 2, min)
maxs <- apply(df1, 2, max)
scaled_data <- as.data.frame(scale(df1, center = mins, scale = maxs-mins))

set.seed(7)
km2 <- kmeans(scaled_data, centers=2, nstart =100)
km3 <- kmeans(scaled_data, centers=3, nstart =100)
km4 <- kmeans(scaled_data, centers=4, nstart =100)
km5 <- kmeans(scaled_data, centers=5, nstart =100)
km2$cluster
km2$centers
km2$size
km2$withinss


#install.packages("ggplot2") 
library(ggplot2) 
#install.packages("factoextra") 
library(factoextra)
p1 <- fviz_cluster(km2, geom = "point", data = scaled_data) + ggtitle("k=2") 
p2 <- fviz_cluster(km3, geom = "point", data = scaled_data) + ggtitle("k=3") 
p3 <- fviz_cluster(km4, geom = "point", data = scaled_data) + ggtitle("k=4") 
p4 <- fviz_cluster(km5, geom = "point", data = scaled_data) + ggtitle("k=5")
#install.packages("gridExtra") 
library(gridExtra) 
grid.arrange(p1,p2,p3,p4, nrow = 2)

#Check for the optimal number of clusters given the data mydata <- data
wss <- (nrow(df1)-1)*sum(apply(df1,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(df1, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)
options(scipen = 99)


set.seed(7)
km3 <- kmeans(scaled_data, 3, nstart=100)
fviz_cluster(km3, geom = "point", data = scaled_data) + ggtitle("kmeans clusters for k=3")


library(cluster)
ss <- silhouette(km3$cluster, dist(scaled_data)) 
mean(ss[ ,3])


a <- km3$cluster
clusters <- cbind(df1, a)
clusters <- as.data.frame(clusters)

c1 <- clusters[a == 1,]
c2 <- clusters[a == 1,]
c3 <- clusters[a == 1,]

library(rpart)
tree <- rpart(a~., data = clusters)
library(rpart.plot)
rpart.plot(tree)



