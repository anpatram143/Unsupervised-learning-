getwd()
setwd("D:\\mypro")

#first we will do normalization due to that every attribute must have equal weightage
#second distance forms
uni<-read.csv('Universities.csv')
head(uni)
View(uni)

#normalize data i.e. to make data unitless
normalize_uni<-scale(uni[,2:7])
normalize_uni
library('moments')
skewness(normalize_uni)
kurtosis(normalize_uni)

#compute distance
?dist
d<-dist(normalize_uni,method='euclidean')  # distance matrix
d

#now do clustering i.e hirarichal and non heirarichal
fit<-hclust(d,method = 'complete')  #alwasy use complete linkage beacuse we work on large population
fit
#plot dendrogram
plot(fit)
plot(fit,hang = -1)

rect.hclust(fit,k=3,border = 'red')

groups<-cutree(fit,k=3) #max number of element lie in cluster is number 1 

#add group column into data
membership<-as.matrix(groups)

final<-data.frame(uni,membership)


View(final)

write.csv(final,file = 'final.csv',row.names = F)

#on an avreage student average summary

aggregate(uni[,-1],by=list(final$membership),mean)

#labeling




#Clustering of cars data
df<-read.csv('Cars.csv')
head(df)
dim(df)

#make it normalized
norm_df<-scale(df)
norm_df

#find distance of each sample with each other
?dist
dist_df<-dist(norm_df,method = 'euclidean')
dist_df

#fit the model for clustering
fit_df<-hclust(dist_df,method = 'complete')
fit_df
#plot dendrogram
plot(fit_df,hang = -1)

rect.hclust(fit_df,k=3,border = 'green')
group_df<-cutree(fit_df,k=3)
group_df
avg<-as.matrix(group_df)
final_model<-data.frame(df,avg)
final_model

#find the average aggregation
aggregate(df[,-1],by=list(final_model$avg),mean)

#k means clustering

install.packages('animation')
library('animation')
install.packages('plyr')
library('plyr')
x<-runif(50)
y<-runif(50)
data<-cbind(x,y)
data

plot(data)
plot(data,type='n')
text(data,rownames(data))

km<-kmeans(data,5)
km
str(km)


#by animation
km1<-kmeans.ani(data,5)
km1$centers
#elbow curve and k ~sqrt(n/2) to decide k value