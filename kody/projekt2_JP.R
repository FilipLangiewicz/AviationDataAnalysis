# cat("Wisla Krakow to duma Polakow")
# Wczytanie wszystkich ramek
# install.packages("tidyverse")  
# install.packages("scales")
# install.packages("stringr")
# install.packages("Hmisc") 
# install.packages("forcats")
# install.packages("ggthemes")
# library("tidyverse")
# library("scales")
# library("stringr")
# library("Hmisc")
# library("forcats")
# library("ggthemes")


# data_1987<-read.csv("C:\\Users\\jakub\\OneDrive\\Dokumenty\\R\\1987.csv")
# data_1988<-read.csv("D:\\R\\1988.csv")
# data_1989<-read.csv("D:\\R\\1989.csv")
# data_1990<-read.csv("D:\\R\\1990.csv")
# data_1991<-read.csv("D:\\R\\1991.csv")
# data_1992<-read.csv("D:\\R\\1992.csv")
# data_1993<-read.csv("D:\\R\\1993.csv")
# data_1994<-read.csv("D:\\R\\1994.csv")
# data_1995<-read.csv("D:\\R\\1995.csv")
# data_1996<-read.csv("D:\\R\\1996.csv")
# data_1997<-read.csv("D:\\R\\1997.csv")
# data_1998<-read.csv("D:\\R\\1998.csv")
# data_1999<-read.csv("D:\\R\\1999.csv")
# data_2000<-read.csv("D:\\R\\2000.csv")
# data_2001<-read.csv("D:\\R\\2001.csv")
# data_2002<-read.csv("D:\\R\\2002.csv")
# data_2003<-read.csv("D:\\R\\2003.csv")
# data_2004<-read.csv("D:\\R\\2004.csv")
# data_2005<-read.csv("D:\\R\\2005.csv")
# data_2006<-read.csv("D:\\R\\2006.csv")
# data_2007<-read.csv("D:\\R\\2007.csv")


library("dplyr")
glue<-function(x,y){
  w<-sort(c(x,y))
  paste(w,collapse="")
}
glue<-Vectorize(glue)



#Pierwszy plan zapytania
# group_by(data_1987,Origin,Dest,Month) %>% summarise(avg=length(select(data_1987$DepDelay)))
a<-data_1987 %>% filter(!is.na(DepDelay)) %>% group_by(Origin,Dest,Month,Distance) %>% summarise(avg=mean(DepDelay),count=length(Origin),) %>% arrange(desc(count))
b<-data_1987 %>% filter(!is.na(DepDelay)) %>% group_by(Origin) %>% summarise(avg=mean(DepDelay),count=length(Origin)) %>% arrange(desc(avg))
data_1987 %>% group_by(Month,DayofMonth,Origin) %>% summarise(count=sum(Cancelled),count2=length(Month)) %>% arrange(desc(count))
data_1987 %>% group_by(Month,DayofMonth,Origin) %>% summarise(count=sum(Cancelled),count2=length(Month),wspolczynnik=count/count2) %>% arrange(desc(wspolczynnik)) %>% filter(count2>20) 
data_1987 %>% group_by(Origin,Dest) %>% summarise(count=length(Origin)) %>% arrange(desc(count))

data_1987 %>% group_by(Connection) %>% summarise(count=length(Origin)) %>% arrange(desc(count))
data_1987<-data_1987 %>% mutate(Connection=glue(Origin,Dest))
data_1987 %>% group_by(Connection) %>% summarise(count=length(Origin),cancellations=sum(Cancelled),ratio=cancellations/count) %>% arrange(desc(ratio))


data_1988 %>% filter(!is.na(DepDelay)) %>% select(DepDelay) %>% arrange(desc(DepDelay)) 
data_1988 %>% group_by(Connection) %>% summarise(count=length(Origin),cancellations=sum(Cancelled),ratio=cancellations/count) %>% arrange(desc(ratio))
data_1988 %>% group_by(Origin,Month) %>% summarise(count=length(Origin),cancellations=sum(Cancelled),ratio=cancellations/count) %>% arrange(desc(ratio)) 
data_1988 %>% group_by(Connection) %>% summarise(count=length(Origin),cancellations=sum(Cancelled),ratio=cancellations/count) %>% arrange(desc(ratio)) %>% filter(cancellations>20, ratio>0.01)



data_1987 %>% group_by(Connection) %>% summarise(count=length(Origin),cancellations=sum(Cancelled),ratio=cancellations/count) %>% arrange(desc(ratio)) %>% filter( ratio>0.01) -> c

data_1988 %>% group_by(Connection) %>% summarise(count=length(Origin),cancellations=sum(Cancelled),ratio=cancellations/count) %>% arrange(desc(ratio)) %>% filter(cancellations>20, ratio>0.01) ->d

inner_join(c,d,by="Connection") %>% mutate(ratioCombined=(cancellations.x+cancellations.y)/(count.x+count.y)) %>% arrange(desc(ratioCombined))

#Wydajność wszystkich firm Przewozowych

data_1988 %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),cancellationCount=sum(Cancelled)) %>% arrange(desc(count))
#Z ilością obsłużonych lotów i ilością odwołań

data_1988 %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),cancellationCount=sum(Cancelled)) %>% mutate(ratio=cancellationCount/count) %>% arrange(desc(ratio))
#Z ilością obsłużonych lotów, odwołań i ratio

data_1988 %>% filter(!is.na(ArrDelay)) %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),cancellationCount=sum(Cancelled),bigArrivalDelay=sum(ArrDelay>30)) %>% mutate(ratio=cancellationCount/count) %>% arrange(desc(ratio))

data_1988 %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),cancellationCount=sum(Cancelled),bigArrivalDelay=sum(ifelse(!is.na(ArrDelay),ArrDelay,0)>30)) %>% mutate(ratio=cancellationCount/count) %>% arrange(desc(ratio))
#Z ilością opóźnień powyżej 30 min

data_1988 %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),cancellationCount=sum(Cancelled),bigArrivalDelay=sum(ifelse(!is.na(ArrDelay),ArrDelay,0)>30)) %>% mutate(CancellationRatio=cancellationCount/count,BigDelayCount=bigArrivalDelay/count) %>% arrange(desc(CancellationRatio))
#Z ratio dla opóźnień większych niż 30 min

data_1989 %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),cancellationCount=sum(Cancelled),bigArrivalDelay=sum(ifelse(!is.na(ArrDelay),ArrDelay,0)>30)) %>% mutate(CancellationRatio=cancellationCount/count,BigDelayCount=bigArrivalDelay/count) %>% arrange(desc(CancellationRatio))
#To samo co wyżej dla roku 1989

#Więcej danych o EA

data_1989 %>% filter(UniqueCarrier=='EA') %>% group_by(Month) %>% summarize(count=length(Month),cancellations=sum(Cancelled)) 

data_1988 %>% group_by(UniqueCarrier) %>% summarize(count=length(UniqueCarrier),delay_100=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>(100),1,0),0))) %>% mutate(ratio_100=delay_100/count) %>% arrange(desc(ratio_100)) ->a
#Ratio dla opoznien >100

b<-data_1988 %>% group_by(UniqueCarrier) %>% summarize(count=length(UniqueCarrier),delay_50_100=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>50,ifelse(ArrDelay<=100,1,0),0),0))) %>% mutate(ratio_50_100=delay_50_100/count) %>% arrange(desc(ratio_50_100))
#Ratio dla opoznien w przedziale 50_100

c<-data_1988 %>% group_by(UniqueCarrier) %>% summarize(count=length(UniqueCarrier),delay_30_50=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>30,ifelse(ArrDelay<=50,1,0),0),0))) %>% mutate(ratio_30_50=delay_30_50/count) %>% arrange(desc(ratio_30_50))
#Ratio dla opoznien w przedziale 30_100

d<-data_1988 %>% group_by(UniqueCarrier) %>% summarize(count=length(UniqueCarrier),delay_10_30=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>10,ifelse(ArrDelay<=30,1,0),0),0))) %>% mutate(ratio_10_30=delay_10_30/count) %>% arrange(desc(ratio_10_30))
#Ratio dla opoznien w przedziale 10_30

e<-data_1988 %>% group_by(UniqueCarrier) %>% summarize(count=length(UniqueCarrier),delay__10_10=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>-10,ifelse(ArrDelay<=10,1,0),0),0))) %>% mutate(ratio__10_10=delay__10_10/count) %>% arrange(desc(ratio__10_10))
#Ratio dla opoznien w przedziale -10_10

f<-data_1988 %>% group_by(UniqueCarrier) %>% summarize(count=length(UniqueCarrier),delay__10=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay<=(-10),1,0),0))) %>% mutate(ratio__10=delay__10/count) %>% arrange(desc(ratio__10))
#Ratio dla opoznien w przedziale -inf_-10

g<-data_1988 %>% group_by(UniqueCarrier) %>% summarize(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio_cancellations=cancellations/count) %>% arrange(desc(ratio_cancellations))
#Ratio dla odwołań

h<-data_1988 %>% group_by(UniqueCarrier) %>% summarize(count=length(UniqueCarrier),nas=sum(ifelse(is.na(ArrDelay),ifelse(Cancelled==0,1,0),0))) %>% mutate(ratio_nas=nas/count) %>% arrange(desc(ratio_nas))
#Ratio dla NA

joined_1988<-select(inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(f,e,by="UniqueCarrier"),d,by="UniqueCarrier"),c,by="UniqueCarrier"),b,by="UniqueCarrier"),a,by="UniqueCarrier"),g,by="UniqueCarrier"),h,by="UniqueCarrier"),UniqueCarrier,count.x,ratio__10,ratio__10_10,ratio_10_30,ratio_30_50,ratio_50_100,ratio_100,ratio_cancellations,ratio_nas) %>% mutate(all=ratio__10+ratio__10_10+ratio_10_30+ratio_30_50+ratio_50_100+ratio_100+ratio_cancellations+ratio_nas)
#Joinowanie wszystkich ratio

# data_1988 %>% group_by(UniqueCarrier) %>% summarize(count=length(UniqueCarrier),naValues=sum(ifelse(is.na(ArrDelay),1,0))) %>% mutate(ratio=naValues/count)
# #Dane z na




join_and_group_data_first_concept<-function(data){
  a<-data %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),delay_100=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>(100),1,0),0))) %>% mutate(ratio_100=delay_100/count) %>% arrange(desc(ratio_100))  
  
  b<-data %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),delay_50_100=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>50,ifelse(ArrDelay<=100,1,0),0),0))) %>% mutate(ratio_50_100=delay_50_100/count) %>% arrange(desc(ratio_50_100))
  
  c<-data %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),delay_30_50=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>30,ifelse(ArrDelay<=50,1,0),0),0))) %>% mutate(ratio_30_50=delay_30_50/count) %>% arrange(desc(ratio_30_50))
  
  d<-data %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),delay_10_30=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>10,ifelse(ArrDelay<=30,1,0),0),0))) %>% mutate(ratio_10_30=delay_10_30/count) %>% arrange(desc(ratio_10_30)) 
  
  e<-data %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),delay__10_10=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>-10,ifelse(ArrDelay<=10,1,0),0),0))) %>% mutate(ratio__10_10=delay__10_10/count) %>% arrange(desc(ratio__10_10))
  
  f<-data %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),delay__10=sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay<=(-10),1,0),0))) %>% mutate(ratio__10=delay__10/count) %>% arrange(desc(ratio__10))
  
  g<-data %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio_cancellations=cancellations/count) %>% arrange(desc(ratio_cancellations))
  
  h<-data %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),nas=sum(ifelse(is.na(ArrDelay),ifelse(Cancelled==0,1,0),0))) %>% mutate(ratio_nas=nas/count) %>% arrange(desc(ratio_nas))
  
  i<-data %>% group_by(UniqueCarrier) %>% summarise(count=length(UniqueCarrier),count_non_na_flights=sum(ifelse(!is.na(ArrDelay),1,0)),mean_delay=mean(ArrDelay,na.rm = TRUE),median_delay=median(ArrDelay,na.rm=TRUE))
  
  joined<-select(inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(f,e,by="UniqueCarrier"),d,by="UniqueCarrier"),c,by="UniqueCarrier"),b,by="UniqueCarrier"),a,by="UniqueCarrier"),g,by="UniqueCarrier"),h,by="UniqueCarrier"),i,by="UniqueCarrier"),UniqueCarrier,count.x,ratio__10,ratio__10_10,ratio_10_30,ratio_30_50,ratio_50_100,ratio_100,ratio_cancellations,ratio_nas,mean_delay,median_delay) %>% mutate(all=ratio__10+ratio__10_10+ratio_10_30+ratio_30_50+ratio_50_100+ratio_100+ratio_cancellations+ratio_nas) %>% arrange(desc(count.x))
  
  j<-data %>% summarise(UniqueCarrier="All_data",count.x=length(Month),ratio__10=(sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay<=(-10),1,0),0))/count.x),ratio__10_10=(sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>-10,ifelse(ArrDelay<=10,1,0),0),0))/count.x),ratio_10_30=(sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>10,ifelse(ArrDelay<=30,1,0),0),0))/count.x),ratio_30_50=(sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>30,ifelse(ArrDelay<=50,1,0),0),0))/count.x),ratio_50_100=(sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>50,ifelse(ArrDelay<=100,1,0),0),0))/count.x),ratio_100=(sum(ifelse(!is.na(ArrDelay),ifelse(ArrDelay>(100),1,0),0))/count.x),ratio_cancellations=(sum(Cancelled)/count.x),ratio_nas=(sum(ifelse(is.na(ArrDelay),ifelse(Cancelled==0,1,0),0))/count.x),mean_delay=mean(ArrDelay,na.rm = TRUE),median_delay=median(ArrDelay,na.rm=TRUE)) %>% mutate(all=ratio__10+ratio__10_10+ratio_10_30+ratio_30_50+ratio_50_100+ratio_100+ratio_cancellations+ratio_nas) 
  
  joined <- joined %>% add_row(j)
  
  
  joined
}
a[c(1,15),c(1:9)] -> test

test2 %>% mutate(names=rownames(test2)) %>% gather(key=Firma,value = Value,V1:V2) ->test3

test3 %>% filter(names!="count.x") %>% ggplot(aes(names,Value,fill=Firma))+geom_col(position = "dodge")+labs(title="1988 DL",caption="Liczba obsłużonych połączeń w roku: 199888",x="Aa")


ratio_plot<-function(data,company){
  tested<-join_and_group_data_first_concept(data)
  
  attributes(tested)$year<-data$Year[1]
  
  plot_data<- tested %>% filter(UniqueCarrier %in% c(company,"All_data")) 
  
  transposed_data<-as.data.frame(t(plot_data))
  
  c(transposed_data[1,1],"Średnia")->colnames(transposed_data)
  
  count_company<-as.numeric(transposed_data[2,1])
  
  count_yearly<-as.numeric(transposed_data[2,2])
  
  transposed_data<- transposed_data[3:9,]
  
  rownames(transposed_data)<- c("(-inf;-10]","(-10;10]","(10;30]","(30;50]", "(50;100]","(100;+inf)","cancelled")

  transposed_data[,1]<-as.numeric(transposed_data[,1])
  
  transposed_data[,2]<-as.numeric(transposed_data[,2])
  
  paste("Liczba obsłużonych połączeń w roku przez firmę:",count_company)->text1
  
  paste("Liczba obsłużonych połączeń w roku przez wszystkie firmy w US:",count_yearly)->text2
  
  transposed_data %>% mutate(przedziały=rownames(transposed_data)) %>% gather(key = Company,value=Value,c(company,"Średnia")) %>% ggplot(aes(przedziały,Value,fill=Company))+geom_col(position = "dodge")+labs(title=paste(as.character(attributes(tested)$year),company),caption=(paste(text1,text2,sep="\n")),x="Przedziały",y="Ratio")  
}