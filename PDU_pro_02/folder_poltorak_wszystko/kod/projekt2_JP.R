# Wczytanie wszystkich pakietow
# install.packages("tidyverse")  
# install.packages("scales")
# install.packages("stringr")
# install.packages("Hmisc") 
# install.packages("forcats")
# install.packages("ggthemes")
# install.packages("viridis") 
# library("viridis")
# library("tidyverse")
# library("scales")
# library("stringr")
# library("Hmisc")
# library("forcats")
# library("ggthemes")
# library("shiny")
# library("gifski")
# library("gganimate")
# library("viridisLite")
# library("scales")
# library("lubridate")
# library("stringr")
# library("dplyr")
# library("purrr")
# library("readr")
# library("tidyr")
# library("tibble")
# library("ggplot2")
# library("stats")
# library("graphics")
# library("grDevices")
# library("utils")
# library("datasets")
# library("methods")
# library("base")


# Wczytanie wszystkich ramek danych
# data_1987<-read.csv("D:\\R\\1987.csv")
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


# funkcja pomocnicza 
glue<-function(x,y){
  w<-sort(c(x,y))
  paste(w,collapse="")
}
glue<-Vectorize(glue)



#Pierwszy plan zapytania i testy, szukanie jakichś ciekawych danych
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





#pierwsza kluczowa funkcja, na której opiera się wiele kolejnych, swojego rodzaju baza do dalszych analiz

join_and_group_data_first_concept<-function(year){
  data<-read.csv(paste("D:\\R\\",year,".csv",sep=""))
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
  
  write.csv(joined,paste("D:\\R\\app_data_",year,".csv",sep=""))
  
  joined
}


# testy grupowania i łączenia ramek, nic ważnego
# a[c(1,15),c(1:9)] -> test
# 
# test2 %>% mutate(names=rownames(test2)) %>% gather(key=Firma,value = Value,V1:V2) ->test3
# 
# test3 %>% filter(names!="count.x") %>% ggplot(aes(names,Value,fill=Firma))+geom_col(position = "dodge")+labs(title="1988 DL",caption="Liczba obsłużonych połączeń w roku: 199888",x="Aa")


#funkcja do generowania wykresu z przedziałami 
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
  
  positions<-c("(-inf;-10]","(-10;10]","(10;30]","(30;50]","(50;100]","(100;+inf)","cancelled")
  
  transposed_data<-transposed_data %>% mutate(przedziały=rownames(transposed_data)) %>% gather(key = Company,value=Value,c(company,"Średnia"))
  
  transposed_data$Company<-factor(transposed_data$Company,levels=c(company,"Średnia"))
  
  transposed_data %>% ggplot(aes(przedziały,Value,fill=Company))+geom_col(position = "dodge")+labs(title=paste(as.character(attributes(tested)$year),company),caption=(paste(text1,text2,sep="\n")),x="Przedziały",y="Ratio")+scale_x_discrete(limits=positions)  
}


# funkcja do generowania wykresu średnich opóźnień
mean_plot<-function(year){
  tested<-join_and_group_data_first_concept(eval(parse(text=(paste("data",year,sep = "_")))))
  
  tested %>% ggplot(aes(x=reorder(UniqueCarrier,mean_delay),mean_delay,fill=factor(ifelse(UniqueCarrier=="All_data","Highlighted","Normal"))))+geom_bar(stat="identity",show.legend = FALSE) + scale_fill_manual(name = "UniqueCarrier", values=c("#FF9999","#56B4E9"))+labs(title=paste("Średnie opóźnienie przylotu dla poszczególnych firm w roku",as.character(year)),x="Firmy",y="Średnie opóźnienie")+theme_minimal()+ coord_flip()
}

# testowa funkcja średnich opóźnień dla Eastern Airlines
mean_test_plot_ea<- function(year){
  data<-read.csv(paste("D:\\R\\",as.character(year),".csv",sep=""))
  
  mean_test<- join_and_group_data_first_concept(data)
  
  mean_test %>% ggplot(aes(x=reorder(UniqueCarrier,mean_delay),mean_delay,fill=factor(ifelse(UniqueCarrier=="All_data","Highlighted",ifelse(UniqueCarrier=="EA","Normal","Last")))))+geom_bar(stat="identity",show.legend = FALSE) + scale_fill_manual(name = "UniqueCarrier", values=c("#FF9999","#56B4E9","#ff5252"))+labs(title=paste("Średnie opóźnienie przylotu dla poszczególnych firm w roku",as.character(year)),x="Firmy",y="Średnie opóźnienie")+theme_minimal()+ coord_flip()
}

# testowa funkcja średnich opóźnień dla Northwest Airlines
mean_test_plot_nw<- function(year){
  data<-read.csv(paste("D:\\R\\",as.character(year),".csv",sep=""))
  
  mean_test<- join_and_group_data_first_concept(data)
  
  mean_test %>% ggplot(aes(x=reorder(UniqueCarrier,mean_delay),mean_delay,fill=factor(ifelse(UniqueCarrier=="All_data","Highlighted",ifelse(UniqueCarrier=="","Normal","Last")))))+geom_bar(stat="identity",show.legend = FALSE) + scale_fill_manual(name = "UniqueCarrier", values=c("#FF9999","#56B4E9","#ff5252"))+labs(title=paste("Średnie opóźnienie przylotu dla poszczególnych firm w roku",as.character(year)),x="Firmy",y="Średnie opóźnienie")+theme_minimal()+ coord_flip()
}

# testowy wykres
# transposed_data %>% ggplot(aes(przedzialy,Value,fill=Company))+geom_col(position="dodge")+labs(x="Przedziały",y="Ratio")+scale_x_discrete(limits=positions)+scale_fill_manual(values=c("red","blue","green"))+ geom_text(aes(label=round(Value,2),y=round(Value,2)),position=position_dodge(0.9),vjust=-0.8,size=3)

# funkcja do tworzenia wykresu srednich opoznien w przedzialach dla wielu firm, w pozniejszym rozrachunku tylko pomocnicza i nigdzie nieuzywana
ratio_with_multiple_companies<-function(data,...){
  v<-c(...)
  
  tested<-join_and_group_data_first_concept(data)
  
  attributes(tested)$year<-data$Year[1]
  
  plot_data<- tested %>% filter(UniqueCarrier %in% c(v,"All_data")) 
  
  transposed_data<-as.data.frame(t(plot_data))
  
  c(transposed_data[1,1:(ncol(transposed_data)-1)],"Średnia")->colnames(transposed_data)
  
  transposed_data<- transposed_data[3:9,]
  
  rownames(transposed_data)<- c("(-inf;-10]","(-10;10]","(10;30]","(30;50]", "(50;100]","(100;+inf)","cancelled")
  
  for (i in 1:ncol(transposed_data)) {
    transposed_data[,i]<-as.numeric(transposed_data[,i])
  }
  
  positions<-c("(-inf;-10]","(-10;10]","(10;30]","(30;50]","(50;100]","(100;+inf)","cancelled")
  
  transposed_data<-transposed_data %>% mutate(przedzialy=rownames(transposed_data)) %>% gather(key=Company,value=Value,c(all_of(v),"Średnia")) 
  
  transposed_data$Company<-factor(transposed_data$Company,levels=c(v,"Średnia"))
  
  transposed_data %>% ggplot(aes(przedzialy,Value,fill=Company))+geom_col(position="dodge")+labs(x="Przedziały",y="Ratio")+scale_x_discrete(limits=positions)+scale_fill_grey()+scale_fill_manual(values = c("Średnia"="yellow")) 
}


# testowe przetworzenia

# xoxo %>% ggplot(aes(przedzialy,Value,fill=Company))+geom_col(position="dodge")+labs(x="Przedziały",y="Ratio")+scale_x_discrete(limits=positions)+scale_fill_manual(values=c("red","blue","green"))+ geom_text(aes(label=round(Value,2),y=round(Value,2)),position=position_dodge(0.9),vjust=-0.8,size=3) + theme_classic() + transition_states(states=Year,transition_length=2,state_length=1)+ enter_fade()+exit_shrink()+ease_aes('sine-in-out') -> considered_data


# transposed_data %>% mutate(przedzialy=rownames(transposed_data)) %>% gather(key=Company,value=Value,c("AA","CO","US","WN","Średnia")) -> transposed_data
# 
# transposed_data$Company<-factor(transposed_data$Company,levels=c("AA","CO","US","WN","Średnia"))
# 
# transposed_data %>% ggplot(aes(przedzialy,Value,fill=Company))+geom_col(position="dodge")+labs(title="considered_data",x="Przedziały",y="Ratio")

# Cancellations testsowe zapytanie:

data_1988 %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(desc(ratio))

# Firma EA
# Testowe przekształcenia
# 
# data_1989 %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(desc(ratio)) %>% filter(UniqueCarrier=="EA") %>% arrange(Month) 
# 
# as.data.frame(a %>% gather(key=type,value=Value,c("count","cancellations"))) ->b
# 
# as.character(b[,2])->b[,2]
# 
# b$Month<-factor(b$Month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
# 
# b %>% ggplot(aes(Month,Value,fill=type))+geom_col(position="dodge")

#funkcja generująca wykres odwołań i lotów w poszczególnych miesiącach 
month_cancellations_plot<-function(data,company){
  filtered_data<-data %>% filter(UniqueCarrier==company) %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(Month) 
  
  gathered_data<-as.data.frame(filtered_data %>% gather(key=type,value=Value,c("count","cancellations"))) 
  
  as.character(gathered_data[,2])->gathered_data[,2]
  
  gathered_data$Month<-factor(gathered_data$Month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  
  gathered_data %>% ggplot(aes(Month,Value,fill=type))+geom_col(position="dodge")
}

# uproszczenie ramki do odwołań dla danej firmy
filter_month_data_cancellations<-function(data,company){
  data %>% filter(UniqueCarrier==company) %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(Month)
}

#dane dla US i WN
data_1988 %>% group_by(UniqueCarrier) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) %>% ggplot(aes(UniqueCarrier,count,fill=UniqueCarrier))+geom_col(position="dodge")

data_1989 %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> b

#Wykresy słupkowe dla US i WN na przestrzeni lat

# read.csv("D:\\R\\1988.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> a
# read.csv("D:\\R\\1989.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> b
# read.csv("D:\\R\\1990.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> c
# read.csv("D:\\R\\1991.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> d
# read.csv("D:\\R\\1992.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> e
# read.csv("D:\\R\\1993.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> f
# read.csv("D:\\R\\1994.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> g
# read.csv("D:\\R\\1995.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> h
# read.csv("D:\\R\\1996.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> i
# read.csv("D:\\R\\1997.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> j
# read.csv("D:\\R\\1998.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> k
# read.csv("D:\\R\\1999.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> l
# read.csv("D:\\R\\2000.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> m
# read.csv("D:\\R\\2001.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> n
# read.csv("D:\\R\\2002.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> o
# read.csv("D:\\R\\2003.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> p
# read.csv("D:\\R\\2004.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> r
# read.csv("D:\\R\\2005.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> s
# read.csv("D:\\R\\2006.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> t
# read.csv("D:\\R\\2007.csv") %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier %in% c("US","WN")) %>% summarise(count=length(UniqueCarrier)) -> u

# przepraszam za dużo redundancji

# testowe generowanie wykresu 
# rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u)-> binded
# 
# as.character(binded[,2,drop=TRUE]) -> binded[,2]
# 
# binded %>% ggplot(aes(Year,count,fill=UniqueCarrier)) +geom_label(position="dodge")+ scale_y_continuous(labels=scales::comma)
# 
# binded %>% filter(UniqueCarrier=="US") %>% ggplot() + geom_line(aes(x=Year,y=count,group=1),color="red")+ scale_y_continuous(labels=scales::comma)
# 
# binded %>% filter(UniqueCarrier=="WN") %>% ggplot() + geom_line(aes(x=Year,y=count,group=1),color="blue")+ scale_y_continuous(labels=scales::comma)
# 
# binded %>% filter(UniqueCarrier=="WN") -> blue_data
# binded %>% filter(UniqueCarrier=="US") -> red_data
# 
# ggplot() + geom_line(data= blue_data,aes(x=Year,y=count,group=1),color="blue")+ scale_y_continuous(labels=scales::comma) + geom_line(data=red_data,aes(x=Year,y=count,group=1),color="red")+ scale_y_continuous(labels=scales::comma)

#procentowy udział US i WN w rynku lotniczym w danym roku, wykres liniowo, punktowy
# read.csv("D:\\R\\1988.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1988
# summarised_year_1988[,c(3,1,2)] -> summarised_year_1988
# all_flights_1988<- summarised_year_1988[,3]
# mutate(a,ratio=count/all_flights_1988) %>% select(UniqueCarrier,Year,ratio) -> aa
# 
# read.csv("D:\\R\\1989.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1989
# summarised_year_1989[,c(3,1,2)] -> summarised_year_1989
# all_flights_1989<- summarised_year_1989[,3]
# mutate(b,ratio=count/all_flights_1989) %>% select(UniqueCarrier,Year,ratio) -> bb
# 
# read.csv("D:\\R\\1990.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1990
# summarised_year_1990[,c(3,1,2)] -> summarised_year_1990
# all_flights_1990<- summarised_year_1990[,3]
# mutate(c,ratio=count/all_flights_1990) %>% select(UniqueCarrier,Year,ratio) -> cc
# 
# read.csv("D:\\R\\1991.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1991
# summarised_year_1991[,c(3,1,2)] -> summarised_year_1991
# all_flights_1991<- summarised_year_1991[,3]
# mutate(d,ratio=count/all_flights_1991) %>% select(UniqueCarrier,Year,ratio) -> dd
# 
# read.csv("D:\\R\\1992.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1992
# summarised_year_1992[,c(3,1,2)] -> summarised_year_1992
# all_flights_1992<- summarised_year_1992[,3]
# mutate(e,ratio=count/all_flights_1992) %>% select(UniqueCarrier,Year,ratio) -> ee
# 
# read.csv("D:\\R\\1993.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1993
# summarised_year_1993[,c(3,1,2)] -> summarised_year_1993
# all_flights_1993<- summarised_year_1993[,3]
# mutate(f,ratio=count/all_flights_1993) %>% select(UniqueCarrier,Year,ratio) -> ff
# 
# read.csv("D:\\R\\1994.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1994
# summarised_year_1994[,c(3,1,2)] -> summarised_year_1994
# all_flights_1994<- summarised_year_1994[,3]
# mutate(g,ratio=count/all_flights_1994) %>% select(UniqueCarrier,Year,ratio) -> gg
# 
# read.csv("D:\\R\\1995.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1995
# summarised_year_1995[,c(3,1,2)] -> summarised_year_1995
# all_flights_1995<- summarised_year_1995[,3]
# mutate(h,ratio=count/all_flights_1995) %>% select(UniqueCarrier,Year,ratio) -> hh
# 
# read.csv("D:\\R\\1996.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1996
# summarised_year_1996[,c(3,1,2)] -> summarised_year_1996
# all_flights_1996<- summarised_year_1996[,3]
# mutate(i,ratio=count/all_flights_1996) %>% select(UniqueCarrier,Year,ratio) -> ii
# 
# read.csv("D:\\R\\1997.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1997
# summarised_year_1997[,c(3,1,2)] -> summarised_year_1997
# all_flights_1997<- summarised_year_1997[,3]
# mutate(j,ratio=count/all_flights_1997) %>% select(UniqueCarrier,Year,ratio) -> jj
# 
# read.csv("D:\\R\\1998.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1998
# summarised_year_1998[,c(3,1,2)] -> summarised_year_1998
# all_flights_1998<- summarised_year_1998[,3]
# mutate(k,ratio=count/all_flights_1998) %>% select(UniqueCarrier,Year,ratio) -> kk
# 
# read.csv("D:\\R\\1999.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_1999
# summarised_year_1999[,c(3,1,2)] -> summarised_year_1999
# all_flights_1999<- summarised_year_1999[,3]
# mutate(l,ratio=count/all_flights_1999) %>% select(UniqueCarrier,Year,ratio) -> ll
# 
# read.csv("D:\\R\\2000.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_2000
# summarised_year_2000[,c(3,1,2)] -> summarised_year_2000
# all_flights_2000<- summarised_year_2000[,3]
# mutate(m,ratio=count/all_flights_2000) %>% select(UniqueCarrier,Year,ratio) -> mm
# 
# read.csv("D:\\R\\2001.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_2001
# summarised_year_2001[,c(3,1,2)] -> summarised_year_2001
# all_flights_2001<- summarised_year_2001[,3]
# mutate(n,ratio=count/all_flights_2001) %>% select(UniqueCarrier,Year,ratio) -> nn
# 
# read.csv("D:\\R\\2002.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_2002
# summarised_year_2002[,c(3,1,2)] -> summarised_year_2002
# all_flights_2002<- summarised_year_2002[,3]
# mutate(o,ratio=count/all_flights_2002) %>% select(UniqueCarrier,Year,ratio) -> oo
# 
# read.csv("D:\\R\\2003.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_2003
# summarised_year_2003[,c(3,1,2)] -> summarised_year_2003
# all_flights_2003<- summarised_year_2003[,3]
# mutate(p,ratio=count/all_flights_2003) %>% select(UniqueCarrier,Year,ratio) -> pp
# 
# read.csv("D:\\R\\2004.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_2004
# summarised_year_2004[,c(3,1,2)] -> summarised_year_2004
# all_flights_2004<- summarised_year_2004[,3]
# mutate(r,ratio=count/all_flights_2004) %>% select(UniqueCarrier,Year,ratio) -> rr
# 
# read.csv("D:\\R\\2005.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_2005
# summarised_year_2005[,c(3,1,2)] -> summarised_year_2005
# all_flights_2005<- summarised_year_2005[,3]
# mutate(s,ratio=count/all_flights_2005) %>% select(UniqueCarrier,Year,ratio) -> ss
# 
# read.csv("D:\\R\\2006.csv") %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year_2006
# summarised_year_2006[,c(3,1,2)] -> summarised_year_2006
# all_flights_2006<- summarised_year_2006[,3]
# mutate(t,ratio=count/all_flights_2006) %>% select(UniqueCarrier,Year,ratio) -> tt

# przepraszam za redundancje

# tworzenie pierwszego wykresu w mojej czesci prezentacji
rbind(aa,bb,cc,dd,ee,ff,gg,hh,ii,jj,kk,ll,mm,nn,oo,pp,rr,ss,tt,uu) -> binded_lines
as.character(binded_lines[,2,drop=TRUE]) -> binded_lines[,2]
binded_lines %>% filter(UniqueCarrier=="WN") -> blue_data
binded_lines %>% filter(UniqueCarrier=="US") -> red_data

lined_plot<-ggplot() + geom_line(data= blue_data,aes(x=Year,y=ratio,group=1),color="blue")+ scale_y_continuous(labels=scales::percent) + geom_line(data=red_data,aes(x=Year,y=ratio,group=1),color="red")+ scale_y_continuous(labels=scales::percent)

ggplot(data=binded,aes(Year,count,fill=UniqueCarrier)) +geom_col(position="dodge")+geom_line(data= blue_data,aes(x=Year,y=ratio*4800000,group=1),color="blue",size=2) + geom_line(data=red_data,aes(x=Year,y=ratio*4800000,group=1),color="red",size=2) +scale_y_continuous(sec.axis = sec_axis(~./4800000),name="considered_data")

ggplot(data=binded,aes(Year,count,fill=UniqueCarrier)) +geom_col(position="dodge") +scale_fill_grey()+geom_line(data= blue_data,aes(x=Year,y=ratio*4800000,group=1),color="blue",size=2) + geom_line(data=red_data,aes(x=Year,y=ratio*4800000,group=1),color="red",size=2) +scale_y_continuous(sec.axis = sec_axis(~./4800000),name="considered_data")+ theme_bw()

ggplot(data=binded,aes(Year,count,fill=UniqueCarrier)) +geom_col(position="dodge") +scale_fill_manual(values=viridis(10)[c(2,8)])+geom_line(data= blue_data,aes(x=Year,y=ratio*4800000,group=1),color="blue",size=2) + geom_line(data=red_data,aes(x=Year,y=ratio*4800000,group=1),color="red",size=2) +scale_y_continuous(sec.axis = sec_axis(~./4800000),name="considered_data")+ theme_bw()

#finalny na godzine 22:30, testowy
ggplot(data=binded,aes(Year,count,fill=UniqueCarrier)) +geom_col(position="dodge") + scale_fill_manual(values=viridis(10)[c(2,8)])+geom_line(data= blue_data,aes(x=Year,y=ratio*4800000,group=1),color="blue",size=2) + geom_line(data=red_data,aes(x=Year,y=ratio*4800000,group=1),color="red",size=2) +scale_y_continuous(sec.axis = sec_axis(~./4800000,name="considered_data",labels=scales::percent))+  theme_bw()
#20:44, finalny
ggplot(data=binded,aes(Year,count,fill=UniqueCarrier)) +geom_col(position="dodge") + scale_fill_manual(name="Firma",values=viridis(10)[c(2,8)])+geom_line(data= blue_data,aes(x=Year,y=ratio*4800000,group=1),color="blue",size=2) + geom_line(data=red_data,aes(x=Year,y=ratio*4800000,group=1),color="red",size=2) +scale_y_continuous(sec.axis = sec_axis(~./4800000,name="Procentowy udział w rynku lotniczym",labels=scales::percent))+  theme_bw() + labs(title = "Loty i udział w rynku lotniczym w poszczególnych latach dla firm US Airlines(US) i Southwest Airlines(WN)", x="Rok", y="Liczba lotów",caption="Kolorem czerwonym oznaczono zmianę procentowego udziału w rynku lotniczym w Stanach Zjednoczonych firmy US Airlines\n Kolorem niebieskim oznaczono zmianę procentowego udziału w rynku lotniczym w Stanach Zjednoczonych firmy Southwest Airlines") + geom_point(data=blue_data,aes(y=ratio*4800000),size=2) +geom_point(data=red_data,aes(y=ratio*4800000),size=2)
#20:49, zły kolor
ggplot(data=binded,aes(Year,count,fill=UniqueCarrier)) +geom_col(position="dodge") + scale_fill_manual(name="Firma",values=c("#F96167","#F9E795"))+geom_line(data= blue_data,aes(x=Year,y=ratio*4800000,group=1),color="blue",size=2) + geom_line(data=red_data,aes(x=Year,y=ratio*4800000,group=1),color="#AA96DA",size=2) +scale_y_continuous(sec.axis = sec_axis(~./4800000,name="Procentowy udział w rynku lotniczym",labels=scales::percent))+  theme_bw() + labs(title = "Loty i udział w rynku lotniczym w poszczególnych latach dla firm US Airlines(US) i Southwest Airlines(WN)", x="Rok", y="Liczba lotów",caption="Kolorem jasno-fioletowym oznaczono zmianę procentowego udziału w rynku lotniczym  w Stanach Zjednoczonych firmy US Airlines\n Kolorem ciemno-niebieskim oznaczono zmianę procentowego udziału w rynku lotniczym w Stanach Zjednoczonych firmy Southwest Airlines") + geom_point(data=blue_data,aes(y=ratio*4800000),size=2) +geom_point(data=red_data,aes(y=ratio*4800000),size=2)
# 
# #porównywanie średnich, rozpoczęte ale niedokończone gromadzenie danych
# read.csv("D:\\R\\1988.csv") %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% cbind(UniqueCarrier="All_data") %>% select(UniqueCarrier,count,mean) -> summarised_year_1988_2
# read.csv("D:\\R\\1988.csv") %>% group_by (UniqueCarrier) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) -> aaa
# rbind(aaa,summarised_year_1988_2) %>% mutate(compare=ifelse(mean<summarised_year_1988_2[,3],"L",ifelse(mean==summarised_year_1988_2[,3],"All_data","H"))) %>% group_by(compare) %>% summarise(count=sum(count)) %>% mutate(ratio=count/summarised_year_1988_2[,2]) 
# 
# read.csv("D:\\R\\1988.csv") %>%  group_by(Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% cbind(UniqueCarrier="All_data") %>% select(UniqueCarrier,Year,count,mean) ->summarised_year_1988_2
# read.csv("D:\\R\\1988.csv") %>% group_by (UniqueCarrier,Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% select(UniqueCarrier,Year,count,mean) -> aaa
# rbind(aaa,summarised_year_1988_2) %>% mutate(compare=ifelse(mean<summarised_year_1988_2[,4],"L",ifelse(mean==summarised_year_1988_2[,4],"All_data","H")))  %>% group_by(compare,Year) %>% summarise(count=sum(count),) %>% mutate(ratio=count/summarised_year_1988_2[,3]) %>% tail(2) -> aaa
# 
# 
# read.csv("D:\\R\\1989.csv") %>%  group_by(Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% cbind(UniqueCarrier="All_data") %>% select(UniqueCarrier,Year,count,mean) ->summarised_year_1989_2
# read.csv("D:\\R\\1989.csv") %>% group_by (UniqueCarrier,Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% select(UniqueCarrier,Year,count,mean) -> bbb
# rbind(bbb,summarised_year_1989_2) %>% mutate(compare=ifelse(mean<summarised_year_1989_2[,4],"L",ifelse(mean==summarised_year_1989_2[,4],"All_data","H")))  %>% group_by(compare,Year) %>% summarise(count=sum(count),) %>% mutate(ratio=count/summarised_year_1989_2[,3]) %>% tail(2) -> bbb
# 
# 
# read.csv("D:\\R\\1990.csv") %>%  group_by(Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% cbind(UniqueCarrier="All_data") %>% select(UniqueCarrier,Year,count,mean) ->summarised_year_1990_2
# read.csv("D:\\R\\1990.csv") %>% group_by (UniqueCarrier,Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% select(UniqueCarrier,Year,count,mean) -> ccc
# rbind(ccc,summarised_year_1990_2) %>% mutate(compare=ifelse(mean<summarised_year_1990_2[,4],"L",ifelse(mean==summarised_year_1990_2[,4],"All_data","H")))  %>% group_by(compare,Year) %>% summarise(count=sum(count),) %>% mutate(ratio=count/summarised_year_1990_2[,3]) %>% tail(2) -> ccc
# 
# 
# read.csv("D:\\R\\1991.csv") %>%  group_by(Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% cbind(UniqueCarrier="All_data") %>% select(UniqueCarrier,Year,count,mean) ->summarised_year_1991_2
# read.csv("D:\\R\\1991.csv") %>% group_by (UniqueCarrier,Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% select(UniqueCarrier,Year,count,mean) -> ddd
# rbind(ddd,summarised_year_1991_2) %>% mutate(compare=ifelse(mean<summarised_year_1991_2[,4],"L",ifelse(mean==summarised_year_1991_2[,4],"All_data","H")))  %>% group_by(compare,Year) %>% summarise(count=sum(count),) %>% mutate(ratio=count/summarised_year_1991_2[,3]) %>% tail(2) -> ddd
# 
# 
# read.csv("D:\\R\\1992.csv") %>%  group_by(Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% cbind(UniqueCarrier="All_data") %>% select(UniqueCarrier,Year,count,mean) ->summarised_year_1992_2
# read.csv("D:\\R\\1992.csv") %>% group_by (UniqueCarrier,Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% select(UniqueCarrier,Year,count,mean) -> eee
# rbind(eee,summarised_year_1992_2) %>% mutate(compare=ifelse(mean<summarised_year_1992_2[,4],"L",ifelse(mean==summarised_year_1992_2[,4],"All_data","H")))  %>% group_by(compare,Year) %>% summarise(count=sum(count),) %>% mutate(ratio=count/summarised_year_1992_2[,3]) %>% tail(2) -> eee
# 
# 
# read.csv("D:\\R\\1993.csv") %>%  group_by(Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% cbind(UniqueCarrier="All_data") %>% select(UniqueCarrier,Year,count,mean) ->summarised_year_1993_2
# read.csv("D:\\R\\1993.csv") %>% group_by (UniqueCarrier,Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% select(UniqueCarrier,Year,count,mean) -> fff
# rbind(fff,summarised_year_1993_2) %>% mutate(compare=ifelse(mean<summarised_year_1993_2[,4],"L",ifelse(mean==summarised_year_1993_2[,4],"All_data","H")))  %>% group_by(compare,Year) %>% summarise(count=sum(count),) %>% mutate(ratio=count/summarised_year_1993_2[,3]) %>% tail(2) -> fff
# 
# 
# read.csv("D:\\R\\1994.csv") %>%  group_by(Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% cbind(UniqueCarrier="All_data") %>% select(UniqueCarrier,Year,count,mean) ->summarised_year_1994_2
# read.csv("D:\\R\\1994.csv") %>% group_by (UniqueCarrier,Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% select(UniqueCarrier,Year,count,mean) -> ggg
# rbind(ggg,summarised_year_1994_2) %>% mutate(compare=ifelse(mean<summarised_year_1994_2[,4],"L",ifelse(mean==summarised_year_1994_2[,4],"All_data","H")))  %>% group_by(compare,Year) %>% summarise(count=sum(count),) %>% mutate(ratio=count/summarised_year_1994_2[,3]) %>% tail(2) -> ggg
# 
# 
# read.csv("D:\\R\\1995.csv") %>%  group_by(Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% cbind(UniqueCarrier="All_data") %>% select(UniqueCarrier,Year,count,mean) ->summarised_year_1995_2
# read.csv("D:\\R\\1995.csv") %>% group_by (UniqueCarrier,Year) %>% summarise(count=length(UniqueCarrier),mean=mean(ArrDelay,na.rm = T)) %>% select(UniqueCarrier,Year,count,mean) -> hhh
# rbind(hhh,summarised_year_1995_2) %>% mutate(compare=ifelse(mean<summarised_year_1995_2[,4],"L",ifelse(mean==summarised_year_1995_2[,4],"All_data","H")))  %>% group_by(compare,Year) %>% summarise(count=sum(count),) %>% mutate(ratio=count/summarised_year_1995_2[,3]) %>% tail(2) -> hhh



# funkcja do generowania ramek dla US Airways i Southwest Airlines
automatyzacja_us_wn<- function(year){
 data<- read.csv(paste("D:\\R\\",year,".csv",sep=""))
 
 tested<-join_and_group_data_first_concept(data)
 
 attributes(tested)$year<-data$Year[1]
 
 plot_data<- tested %>% filter(UniqueCarrier %in% c(v,"All_data")) 
 
 transposed_data<-as.data.frame(t(plot_data))
 
 c(transposed_data[1,1:(ncol(transposed_data)-1)],"Średnia")->colnames(transposed_data)
 
 transposed_data<- transposed_data[3:9,]
 
 rownames(transposed_data)<- c("(-inf;-10]","(-10;10]","(10;30]","(30;50]", "(50;100]","(100;+inf)","cancelled")
 
 for (i in 1:ncol(transposed_data)) {
   transposed_data[,i]<-as.numeric(transposed_data[,i])
 }
 
 positions<-c("(-inf;-10]","(-10;10]","(10;30]","(30;50]","(50;100]","(100;+inf)","cancelled")
 
 transposed_data<-transposed_data %>% mutate(przedzialy=rownames(transposed_data)) %>% gather(key=Company,value=Value,c(all_of(v),"Średnia")) 
 
 transposed_data$Company<-factor(transposed_data$Company,levels=c(v,"Średnia"))
 
 transposed_data<- cbind(transposed_data,Year=year)
 
 write.csv(transposed_data,paste("transposed_data_",year,".csv",sep=""))
}

#wczytywanie powyższych ramek
wczytaj_dane_animacja_us_wn<-function(){
  data<-read.csv(paste("C:\\Users\\jakub\\OneDrive\\Dokumenty\\transposed_data_","1988.csv",sep=""))
  for (i in 1989:2007) {
    data<-rbind(data,read.csv(paste("C:\\Users\\jakub\\OneDrive\\Dokumenty\\transposed_data_",as.character(i),".csv",sep="")))
  }
  data
}

#generowanie ramek dla Delta Airlines
automatyzacja_delta_airlines<-function(year){
  data<- read.csv(paste("D:\\R\\",year,".csv",sep=""))
  
  company<-c("DL")
  
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
  
  positions<-c("(-inf;-10]","(-10;10]","(10;30]","(30;50]","(50;100]","(100;+inf)","cancelled")
  
  transposed_data<-transposed_data %>% mutate(przedziały=rownames(transposed_data)) %>% gather(key = Company,value=Value,c(company,"Średnia"))
  
  transposed_data$Company<-factor(transposed_data$Company,levels=c(company,"Średnia"))
  
  transposed_data <- cbind(transposed_data, Year=as.character(year))
  
  write.csv(transposed_data,paste("data_dl_",year,".csv",sep=""))
}

# wczytywanie powyższych ramek
wczytaj_dane_animacja_dl<-function(){
  data<-read.csv(paste("D:\\R\\data_dl_","1988.csv",sep=""))
  for (i in 1989:2007) {
    data<-rbind(data,read.csv(paste("D:\\R\\data_dl_",as.character(i),".csv",sep="")))
  }
  data
}

#testowany wykres
testowane_dane %>% ggplot(aes(przedzialy,Value,fill=Company))+geom_col(position="dodge")+labs(x="Przedziały",y="Ratio",title="Podział współczynników opóźnień w poszczególnych przedziałach czasowych dla firm US Airways(US) i Northwest Airlines(WN) w roku: {closest_state}")+scale_x_discrete(limits=positions)+scale_fill_manual(values = c("#2171b5","#6baed6","#bdd7e7"))+ geom_text(aes(label=sprintf("%.3f",Value),y=round(Value,2)),position=position_dodge(0.9),vjust=-0.8,size=3) + theme(plot.title=element_text(size=13,face="bold")) + transition_states(states=Year,transition_length=2,state_length=1)+ enter_fade()+exit_shrink()+ease_aes('sine-in-out')+ theme_bw() -> considered_data

testowane_dane %>% ggplot(aes(przedzialy,Value,fill=Company))+geom_col(position="dodge")+labs(x="Przedziały",y="Ratio",title="Podział współczynników opóźnień w poszczególnych przedziałach czasowych dla firm US Airways(US) i Southwest Airlines(WN) w roku: {closest_state}")+scale_x_discrete(limits=positions)+scale_fill_manual(name="Firma",values = c("#2171b5","#6baed6","#bdd7e7"))+ geom_text(aes(label=sprintf("%.3f",Value),y=round(Value,2)),position=position_dodge(0.9),vjust=-0.8,size=3) + theme(plot.title=element_text(size=13,face="bold")) + transition_states(states=Year,transition_length=2,state_length=1)+ enter_fade()+exit_shrink()+ease_aes('sine-in-out')+ theme_bw() -> considered_data

#generowanie ramek rocznych średnich opóźnień dla danych firm 
mean_write_values<-function(year){
  data<- read.csv(paste("D:\\R\\",year,".csv",sep=""))
  
  tested<-join_and_group_data_first_concept(data)
  
  mean_year_delay<-as.numeric(tested[nrow(tested),11])
  
  attributes(tested)$year<-data$Year[1]
  
  tested %>% select(UniqueCarrier,mean_delay) %>% mutate(Year=as.character(year)) -> plot_data
  
  plot_data$mean_delay- mean_year_delay -> plot_data$mean_delay
  
  colnames(plot_data)[2] <- "delta_mean_delay"
  
  write.csv(plot_data[(-nrow(plot_data)),],paste("data_mean_delta_",year,".csv",sep=""))
}

# wczytanie powyższych ramek
read_mean_values<-function(){
  data<-read.csv(paste("D:\\R\\data_mean_delta_","1988.csv",sep=""))
  for (i in 1989:2007) {
    data<-rbind(data,read.csv(paste("D:\\R\\data_mean_delta_",as.character(i),".csv",sep="")))
  }
  data
}
#unikalne firmy na przestrzeni lat, łączenie to w ramkę danych
rbind_data_mean_values<-function(data,companies){
  for (i in 1988:2007){
    for(j in 1:29){
      tested_data<-as.data.frame(matrix(1:3,nrow=1))
      colnames(tested_data)<- c("UniqueCarrier","delta_mean_delay","Year")
      tested_data[,1]<-companies[j]
      tested_data[,2]<-0
      tested_data[,3]<-as.character(i)
      ifelse(sum(str_detect(data$Year,as.character(i)) & str_detect(data$UniqueCarrier,companies[j])),0,data<-rbind(data,tested_data))
    }
  }
  data
}

# testowe wykresy
teeest2 %>% arrange(Year,UniqueCarrier) %>% slice(1:29) %>% ggplot(aes(x=UniqueCarrier,y=delta_mean_delay,label=round(delta_mean_delay,2)))+ geom_point(stat="identity",fill="black",size=8)+ geom_segment(aes(y=0,x=UniqueCarrier,yend=delta_mean_delay,xend=UniqueCarrier),color="black")+ geom_text(color="white",size=2)+coord_flip()+theme_bw()
+ transition_states(states=Year,transition_length=2,state_length=1)+ enter_fade()+exit_shrink()+ease_aes('sine-in-out') -> considered_data
teeest2 %>% arrange(Year,UniqueCarrier) %>% mutate(color=ifelse(UniqueCarrier=="WN","blue",ifelse(UniqueCarrier=="DL","green","default"))) %>% ggplot(aes(x=UniqueCarrier,y=delta_mean_delay,label=round(delta_mean_delay,2)))+ geom_point(stat="identity",fill="black",size=12)+ geom_segment(aes(y=0,x=UniqueCarrier,yend=delta_mean_delay,xend=UniqueCarrier),color="black")+ geom_text(color="white",size=4)+labs(x="Firmy przelotowe",y="Unormowane opóźnienie przylotu",title="Średnie opóźnienia przylotów poszczególnych firm w stosunku do średniej całościowej dla roku: {closest_state}")+ theme(plot.title=element_text(size=13,face="bold"))+ scale_color_manual(values=c(`green`="green",`blue`="blue",`default`="black"))+coord_flip()+theme_bw() 
teeest2 %>% arrange(Year,UniqueCarrier) %>% mutate(color=ifelse(UniqueCarrier=="WN","blue",ifelse(UniqueCarrier=="DL","green","default"))) %>% ggplot(aes(x=UniqueCarrier,y=delta_mean_delay,label=round(delta_mean_delay,2)))+ geom_point(stat="identity",fill="black",size=12)+ geom_segment(aes(y=0,x=UniqueCarrier,yend=delta_mean_delay,xend=UniqueCarrier),color="black")+ geom_text(color="white",size=4)+labs(x="Firmy przelotowe",y="Unormowane opóźnienie przylotu",title="Średnie opóźnienia przylotów poszczególnych firm w stosunku do średniej całościowej dla roku: {closest_state}")+ theme(plot.title=element_text(size=13,face="bold"))+ scale_color_manual(values=c(`green`="green",`blue`="blue",`default`="black"))+coord_flip()+theme_bw() + transition_states(states=Year,transition_length=2,state_length=1)+ enter_fade()+exit_shrink()+ease_aes('sine-in-out') -> considered_data
teeest2 %>% arrange(Year,UniqueCarrier) %>% ggplot(aes(x=UniqueCarrier,y=delta_mean_delay,label=round(delta_mean_delay,2)))+ geom_point(stat="identity",fill="black",size=12)+ geom_segment(aes(y=0,x=UniqueCarrier,yend=delta_mean_delay,xend=UniqueCarrier),color="black")+ geom_text(color="white",size=4)+labs(x="Firmy przelotowe",y="Unormowane opóźnienie przylotu",title="Średnie opóźnienia przylotów poszczególnych firm w stosunku do średniej całościowej dla roku: {closest_state}")+ theme(plot.title=element_text(size=13,face="bold"))+coord_flip()+theme_bw() + transition_states(states=Year,transition_length=2,state_length=1)+ enter_fade()+exit_shrink()+ease_aes('sine-in-out') -> considered_data

teeest2 %>%  arrange(Year,UniqueCarrier) %>% mutate(color=ifelse(UniqueCarrier=="WN","WN",ifelse(UniqueCarrier=="DL","DL","default"))) -> teeest2

#finalny na 00.00
teeest2 %>% ggplot(aes(x=UniqueCarrier,y=delta_mean_delay,label=round(delta_mean_delay,2)))+ geom_point(stat="identity",aes(col=Color),size=12)+ geom_segment(aes(y=0,x=UniqueCarrier,yend=delta_mean_delay,xend=UniqueCarrier,col=Color))+ scale_color_manual(name="Firmy przelotowe: ", labels=c("Inne","Delta Airlines","Northwest Airlines"),values=c("WN"="#6a51a3","DL"="#cb181d","default"="black"))+ geom_text(color="white",size=4)+labs(x="Firmy przelotowe",y="Unormowane opóźnienie przylotu",title="Średnie opóźnienia przylotów poszczególnych firm w stosunku do średniej całkowitej dla roku: {closest_state}")+ theme(plot.title=element_text(size=13,face="bold"))+coord_flip()+theme_bw() + transition_states(states=Year,transition_length=2,state_length=1)+ enter_fade()+exit_shrink()+ease_aes('sine-in-out') -> considered_data
#finalny na 18  
teeest2 %>% ggplot(aes(x=UniqueCarrier,y=delta_mean_delay,label=round(delta_mean_delay,2)))+ geom_point(stat="identity",aes(col=Color),size=12)+ geom_segment(aes(y=0,x=UniqueCarrier,yend=delta_mean_delay,xend=UniqueCarrier,col=Color))+ scale_color_manual(name="Firmy przelotowe: ", labels=c("Inne","Delta Airlines","Southwest Airlines"),values=c("WN"="#6a51a3","DL"="#cb181d","default"="black"))+ geom_text(aes(label=sprintf("%.2f",delta_mean_delay)),color="white",size=4)+labs(x="Firmy przelotowe",y="Unormowane opóźnienie przylotu",title="Średnie opóźnienia przylotów poszczególnych firm w stosunku do średniej całkowitej dla roku: {closest_state}")+ theme(plot.title=element_text(size=13,face="bold"))+coord_flip()+theme_bw() + transition_states(states=Year,transition_length=2,state_length=1)+ enter_fade()+exit_shrink()+ease_aes('sine-in-out') + ylim(-12.5, 12.5) -> considered_data

#stworzenie ramek dla Delta Airlines
proceed_dl_data<-function(year){
  read.csv(paste("D:\\R\\",year,".csv",sep="")) -> data
  data %>% group_by(UniqueCarrier,Year) %>%  filter(UniqueCarrier=="DL") %>% summarise(count=length(UniqueCarrier)) -> data
  write.csv(data,paste("dl_data_count_",year,".csv"))
}

#Wczytanie powyższych ramek z ratio 
proceed_dl_data_with_ratio<-function(year){
  read.csv(paste("D:\\R\\",year,".csv",sep=""))-> data 
  data %>% group_by(Year) %>% summarise(count=length(UniqueCarrier)) %>% cbind(UniqueCarrier="All_data") -> summarised_year
  summarised_year[,c(3,1,2)] -> summarised_year
  all_flights<- summarised_year[,3]
  csv_file <- read.csv(paste("dl_data_count_",year,".csv"))
  mutate(csv_file,ratio=count/all_flights) %>% select(UniqueCarrier,Year,ratio) -> csv_file
  write.csv(csv_file,paste("consolidated_data_dl_",year,".csv"))
}

# testowe wczytanie bez ratio
read_all_dl_data<-function(){
  data<-read.csv(paste("D:\\R\\dl_data_count_","1988 .csv"))
  for (i in 1989:2007) {
    data<-rbind(data,read.csv(paste("D:\\R\\dl_data_count_",as.character(i),".csv")))
  }
  data_2<-read.csv(paste("D:\\R\\consolidated_data_dl_","1988 .csv"))
  for (i in 1989:2007) {
    data_2<-rbind(data_2,read.csv(paste("D:\\R\\consolidated_data_dl_",as.character(i),".csv")))
  }
  result<-inner_join(data,data_2,by="Year")
  result
}
# generowanie wykresu
read_all_dl_data() -> result
result<- result[,c(2,3,4,7)]
colnames(result)[1]<- "UniqueCarrier"
result$Year <- as.character(result$Year)
ggplot(data=result,aes(Year,count,fill=UniqueCarrier)) +geom_col(position="dodge") + scale_fill_manual(values=viridis(10)[c(2,8)])+geom_line(data= result,aes(x=Year,y=ratio*4800000,group=1),color="blue",size=2)+ geom_point(aes(y=ratio*4800000),size=5)+scale_y_continuous(sec.axis = sec_axis(~./4800000,name="considered_data",labels=scales::percent))+  theme_bw()
ggplot(data=result,aes(Year,count,fill=UniqueCarrier)) +geom_col(position="dodge") + scale_fill_manual(name="Firma",values=c("#ff7400"))+geom_line(data= result,aes(x=Year,y=ratio*4800000,group=1),color="#6baed6",size=2)+ geom_point(aes(y=ratio*4800000),size=4)+scale_y_continuous(sec.axis = sec_axis(~./4800000,name="Procentowy udział w rynku lotniczym",labels=scales::percent))+  theme_bw()+ labs(title = "Liczba lotów i udział w rynku lotniczym w poszczególnych latach dla firmy Delta Airlines(DL)", x="Rok", y="Liczba lotów",caption="Niebieskim kolorem oznaczono zmianę procentowego udziału w rynku lotniczym  w Stanach Zjednoczonych firmy Delta Airlines") 
ggplot(data=result,aes(Year,count,fill=UniqueCarrier)) +geom_col(position="dodge") + scale_fill_manual(name="Firma",values=c("#ff7400"))+geom_line(data= result,aes(x=Year,y=ratio*4800000,group=1),color="#6baed6",size=2)+ geom_point(aes(y=ratio*4800000),size=4)+scale_y_continuous(sec.axis = sec_axis(~./4800000,name="Procentowy udział w rynku lotniczym",labels=scales::percent))+  theme_bw()+ labs(title = "Liczba lotów i udział w rynku lotniczym w poszczególnych latach dla firmy Delta Airlines(DL)", x="Rok", y="Liczba lotów",caption="Niebieską linią oznaczono zmianę procentowego udziału w rynku lotniczym  w Stanach Zjednoczonych firmy Delta Airlines") 


# Generowanie ramek dla strajku Northwest
group_data_Northwest_1998<-function(){
  data <- read.csv("D:\\R\\1998.csv")
  filtered_data<-data %>% filter(UniqueCarrier=="NW") %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(Month) 
  gathered_data<-as.data.frame(filtered_data %>% gather(key=type,value=Value,c("count","cancellations"))) 
  
  as.character(gathered_data[,2])->gathered_data[,2]
  
  gathered_data$Month<-factor(gathered_data$Month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  write.csv(gathered_data,"gathered_data_nw_1998.csv")
  gathered_data
}
# wykresy dla strajków
northwest_strike_data %>% ggplot(aes(Month,Value,fill=type))+geom_col(position="dodge") + labs(title="Liczba lotów i odwołań w poszczególnych miesiącach dla firmy Northwest Airlines w roku 1998",x="Miesiąc",y="Liczba lotów") +scale_fill_manual(name="Rodzaj", values=c("#967bb6","#008080"),labels=c("Odwołania","Wszystkie loty"))
#ostateczne
northwest_strike_data %>% ggplot(aes(Month,Value,fill=type))+geom_col(position="dodge") + labs(title="Liczba lotów i odwołań w poszczególnych miesiącach dla firmy Northwest Airlines w roku 1998",x="Miesiąc",y="Liczba lotów") +scale_fill_manual(name="Rodzaj", values=c("#967bb6","#008080"),labels=c("Odwołania","Wszystkie loty")) +theme_hc()

# generowanie ramek dla strajku EA 
group_data_eastern_airlines_1989<-function(){
  data <- read.csv("D:\\R\\1989.csv")
  filtered_data<-data %>% filter(UniqueCarrier=="EA") %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(Month) 
  gathered_data<-as.data.frame(filtered_data %>% gather(key=type,value=Value,c("count","cancellations"))) 
  
  as.character(gathered_data[,2])->gathered_data[,2]
  
  gathered_data$Month<-factor(gathered_data$Month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  write.csv(gathered_data,"gathered_data_ea_1989.csv")
  gathered_data
}
# generowanie wykresu
eastern_airlines_strike_data %>% ggplot(aes(Month,Value,fill=type))+geom_col(position="dodge") + labs(title="Liczba lotów i odwołań w poszczególnych miesiącach dla firmy Eastern Airlines w roku 1989",x="Miesiąc",y="Liczba lotów") +scale_fill_manual(name="Rodzaj", values=c("#408EC6","#7A2048"),labels=c("Odwołania","Wszystkie loty")) +theme_hc()
#Dane dal eastern Airlines rok przed strajkami
group_data_eastern_airlines_1988<-function(){
  data <- read.csv("D:\\R\\1988.csv")
  filtered_data<-data %>% filter(UniqueCarrier=="EA") %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(Month) 
  gathered_data<-as.data.frame(filtered_data %>% gather(key=type,value=Value,c("count","cancellations"))) 
  
  as.character(gathered_data[,2])->gathered_data[,2]
  
  gathered_data$Month<-factor(gathered_data$Month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  write.csv(gathered_data,"gathered_data_ea_1988.csv")
  gathered_data
}
#wykres
eastern_airlines_1988 %>% ggplot(aes(Month,Value,fill=type))+geom_col(position="dodge") + labs(title="Liczba lotów i odwołań w poszczególnych miesiącach dla firmy Eastern Airlines w roku 1988",x="Miesiąc",y="Liczba lotów") +scale_fill_manual(name="Rodzaj", values=c("#408EC6","#7A2048"),labels=c("Odwołania","Wszystkie loty")) +theme_hc()
# Dane dla Eastern Airlines rok po strajku
group_data_eastern_airlines_1990<-function(){
  data <- read.csv("D:\\R\\1990.csv")
  filtered_data<-data %>% filter(UniqueCarrier=="EA") %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(Month) 
  gathered_data<-as.data.frame(filtered_data %>% gather(key=type,value=Value,c("count","cancellations"))) 
  
  as.character(gathered_data[,2])->gathered_data[,2]
  
  gathered_data$Month<-factor(gathered_data$Month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  write.csv(gathered_data,"gathered_data_ea_1990.csv")
  gathered_data
}
#wykres
eastern_airlines_1990 %>% ggplot(aes(Month,Value,fill=type))+geom_col(position="dodge") + labs(title="Liczba lotów i odwołań w poszczególnych miesiącach dla firmy Eastern Airlines w roku 1990",x="Miesiąc",y="Liczba lotów") +scale_fill_manual(name="Rodzaj", values=c("#408EC6","#7A2048"),labels=c("Odwołania","Wszystkie loty")) +theme_hc() +ylim(0, 30000)

#Dane dla northwest Airlines rok przed strajkiem
group_data_Northwest_1997<-function(){
  data <- read.csv("D:\\R\\1997.csv")
  filtered_data<-data %>% filter(UniqueCarrier=="NW") %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(Month) 
  gathered_data<-as.data.frame(filtered_data %>% gather(key=type,value=Value,c("count","cancellations"))) 
  
  as.character(gathered_data[,2])->gathered_data[,2]
  
  gathered_data$Month<-factor(gathered_data$Month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  write.csv(gathered_data,"gathered_data_nw_1997.csv")
  gathered_data
}
# wykres
northwest_1997 %>% ggplot(aes(Month,Value,fill=type))+geom_col(position="dodge") + labs(title="Liczba lotów i odwołań w poszczególnych miesiącach dla firmy Northwest Airlines w roku 1997",x="Miesiąc",y="Liczba lotów") +scale_fill_manual(name="Rodzaj", values=c("#967bb6","#008080"),labels=c("Odwołania","Wszystkie loty")) +theme_hc()

# dane dla northwest airlines rok po strajku
group_data_Northwest_1999<-function(){
  data <- read.csv("D:\\R\\1999.csv")
  filtered_data<-data %>% filter(UniqueCarrier=="NW") %>% group_by(UniqueCarrier,Month) %>% summarise(count=length(UniqueCarrier),cancellations=sum(Cancelled)) %>% mutate(ratio=cancellations/count) %>% arrange(Month) 
  gathered_data<-as.data.frame(filtered_data %>% gather(key=type,value=Value,c("count","cancellations"))) 
  
  as.character(gathered_data[,2])->gathered_data[,2]
  
  gathered_data$Month<-factor(gathered_data$Month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  write.csv(gathered_data,"gathered_data_nw_1999.csv")
  gathered_data
}
# wykres
northwest_1999 %>% ggplot(aes(Month,Value,fill=type))+geom_col(position="dodge") + labs(title="Liczba lotów i odwołań w poszczególnych miesiącach dla firmy Northwest Airlines w roku 1999",x="Miesiąc",y="Liczba lotów") +scale_fill_manual(name="Rodzaj", values=c("#967bb6","#008080"),labels=c("Odwołania","Wszystkie loty")) +theme_hc()