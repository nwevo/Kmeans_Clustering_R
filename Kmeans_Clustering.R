#Data import procedure
setwd("C:/Users/faco9/Downloads")
getwd()

library(readxl)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(factoextra)
library(tidyr)
library(cluster)

#read in and inspect dataset
retail_data<-read_xlsx("Online Retail.xlsx")

head(retail_data)
tail(retail_data)
str(retail_data)
summary(retail_data)

#data cleansing and preparation process-identify columns with NA and negative values
colSums(is.na(retail_data))
colSums((retail_data<0))
any(is.null(retail_data))

#omit missing values from Description and CustomerID 
retail_data=na.omit(retail_data)

#replace the negative values in Quantity and UnitPrice 
retail_data<-subset(retail_data, Quantity>0)
retail_data<-subset(retail_data, UnitPrice>0)
colSums((retail_data<0))


#split components of InvoiceDate into Day, Month, Year and Hour
retail_data$InvoiceDate<-as.character(retail_data$InvoiceDate)
retail_data$Date<-sapply(retail_data$InvoiceDate, FUN = function(x){strsplit(x, split = '[ ]')[[1]][1]})
retail_data$Time<-sapply(retail_data$InvoiceDate, FUN = function(x){strsplit(x, split = '[ ]')[[1]][2]})

#create Month, Year, WeekOfDay, HourOfDay and TotalSales variables
retail_data$Month<-sapply(retail_data$Date, FUN = function(x){strsplit(x, split = '[-]')[[1]][2]})

#split does not give year in appropriate format
retail_data$Year<-sapply(retail_data$Date, FUN = function(x){strsplit(x, split = '[-]')[[1]][1]})
retail_data$HourOfDay<-sapply(retail_data$Time, FUN = function(x){strsplit(x, split = '[:]')[[1]][1]})
retail_data$DayOfWeek<-wday(retail_data$InvoiceDate,label=TRUE)
retail_data<-retail_data%>%mutate(retail_data, TotalSales=Quantity*UnitPrice)

glimpse(retail_data)

#recall point
mydata<-retail_data
retail_data<-mydata

#convert date variable to datetime class
retail_data$InvoiceDate<- as_datetime(retail_data$InvoiceDate)


#convert appropriate variables to factor to enable us do more with them
retail_data$Country<-as.factor(retail_data$Country)
retail_data$Month<-as.factor(retail_data$Month)
retail_data$Year<-as.factor(retail_data$Year) 
levels(retail_data$Year)
HourOfDay<-as.factor(retail_data$HourOfDay)
retail_data$DayOfWeek<-as.factor(retail_data$DayOfWeek)


glimpse(retail_data)
head(retail_data)

#Analyse revenue by country, year and month
Top10_Countries<-top_n(arrange(summarise(group_by(retail_data, Country), 'TotalSales' = n()), desc(`TotalSales`)), 10)

ggplot(summarise(group_by(Top10_Countries, Country), Revenue=sum(TotalSales)), aes(x= Country, y=Revenue))+
  geom_bar(stat = "identity", fill = 'steel blue')+
  labs(x='Country', y='Revenue in £', title='Revenue by Country')+
  theme_minimal()

ggplot(summarise(group_by(retail_data, Year), Revenue=sum(TotalSales)), aes(x=Year, y=Revenue))+
  geom_bar(stat = 'identity', fill = 'steel blue')+
  labs(x='Year', y='Revenue in £', title='Revenue by Year')+
  theme_minimal()

ggplot(summarise(group_by(retail_data, Month), Revenue=sum(TotalSales)), aes(x=Month, y=Revenue))+
  geom_bar(stat = 'identity', fill = 'steel blue')+
  labs(x='Month', y='Revenue in £', title='Revenue by Month of year')+
  theme_minimal()


#calculate RFM for analysis
max_date<-max(retail_data$InvoiceDate, na.rm = TRUE)
retail_data=mutate(retail_data, Diff=difftime(max_date, InvoiceDate, units = "days"))
retail_data$Diff<-floor(retail_data$Diff)
RFM<-summarise(group_by(retail_data, CustomerID),Frequency=n(),
               Monetary=sum(TotalSales), Recency=min(Diff))
RFM$Recency<-as.numeric(RFM$Recency)
RFM$Monetary[is.na(RFM$Monetary)]<-0
glimpse(RFM)
head(RFM)

#Export to Workbook for SAS analysis
install.packages("writexl")
library(writexl)
write_xlsx(RFM, "c:/Users/faco9/Downloads/RFM.xlsx")

#scale data frame
RFM<-data.frame(RFM)
head(Scaled_RFM)
row.names(RFM)<-RFM$CustomerID
RFM<-RFM[,-1]
Scaled_RFM<-scale(RFM)
Scaled_RFM<-data.frame(Scaled_RFM)

#determine optimum number of clusters
fviz_nbclust(Scaled_RFM, kmeans, method = "wss") #elbow method k=3
#fviz_nbclust(Scaled_RFM, kmeans, method = "silhouette") #silhouette method k=4

#visualizing clusters
km3<-kmeans(Scaled_RFM, centers = 3, nstart = 25)
fviz_cluster(km3, geom ="point", data =Scaled_RFM, pointsize = 0.2)+
  ggtitle("Clusters for k=3")
#kmeans_4<-kmeans(Scaled_RFM, centers= 4, nstart=25)

#create new dataframe with clusterid 
km3_results<-cbind(RFM, ClusterID = km3$cluster)
km3_results<-as.data.frame(km3_results)
head(km3_results)
tail(km3_results)

aggregate(km3_results, by = list(km3_results$ClusterID),FUN = mean)

#clusterid plots
ggplot(km3_results, aes(x=ClusterID, y=Frequency, group=ClusterID, fill=as.factor(ClusterID))) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()

ggplot(km3_results, aes(x=ClusterID, y=Recency, group=ClusterID, fill=as.factor(ClusterID))) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()

ggplot(km3_results, aes(x=ClusterID, y=Monetary, group=ClusterID, fill=as.factor(ClusterID))) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()

grid.arrange(R_clustID, F_clustID, M_clustID, ncol=3)

