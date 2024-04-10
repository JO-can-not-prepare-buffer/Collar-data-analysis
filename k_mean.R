# try use k-mean to cluster data #
# install.packages('factoextra');install.packages('pacman')
library(factoextra)
library(dplyr)
library(pacman)
library(cluster)

df0 <- read.csv(file.choose())
head(df0)

df <- cbind(df0[2:5],scale(df0[6:9]))

d <- dist(df[5:8])###计算欧氏距离
km <- eclust(df[5:8], "kmeans", nstart = 25) #聚类的散点图

###删掉偏离值，视情况而定
df1 <- df0[-(521:529),]
df1 <- df0[-(245:249),]
df <- cbind(df1[2:5],scale(df1[6:11]))

d <- dist(df[5:10])###计算欧氏距离
km <- eclust(df[5:10], "kmeans", nstart = 25) #聚类的散点图


fviz_gap_stat(km$gap_stat) # 不同K值下Gap 统计图，指导选择最佳K值
fviz_silhouette(km) # 轮廓图，每种聚类下面的分布情况

set.seed(666)
kmeans1<-kmeans(df[5:8],centers=2,nstart = 25)
fviz_cluster(object=kmeans1,data=df[5:8],
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = ("point"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())

# set.seed(666)
# kmeans2<-kmeans(df[5:8],centers=3,nstart = 25)
# fviz_cluster(object=kmeans2,data=df[5:8],
#              ellipse.type = "euclid",star.plot=T,repel=T,
#              geom = ("point"),palette='jco',main="",
#              ggtheme=theme_minimal())+
#   theme(axis.title = element_blank())

# set.seed(666)
# kmeans3<-kmeans(df[5:10],centers=4,nstart = 25)
# fviz_cluster(object=kmeans3,data=df[5:10],
#              ellipse.type = "euclid",star.plot=T,repel=T,
#              geom = ("point"),palette='jco',main="",
#              ggtheme=theme_minimal())+
#   theme(axis.title = element_blank())

summary(kmeans1)
kmeans1$cluster
kmeans1$size

str(kmeans1$cluster)
df$Season <- kmeans1$cluster

write.csv(df,"data/PKU002_2.csv")



countdata <- read.csv(file.choose())
head(countdata)

library(ggplot2)
library(dplyr)

datelabel <- countdata %>% filter(all>0)
sp <- datelabel %>% ggplot(aes(x=label))+geom_density()

MaxY1_index <- which.max(density(datelabel$label)$y)
MaxY1_index


MaxX1 <- density(datelabel$label)$x[MaxY1_index]
MaxX1

MaxY2_index <- which.max(density(datelabel$label)$y[density(datelabel$label)$x > 200])
MaxY2_index


MaxX2 <- density(datelabel$label)$x[MaxY2_index]
MaxX2
