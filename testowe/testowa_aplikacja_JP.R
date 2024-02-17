#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
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


all_companies<-function(year){
  sort(unique(eval(parse(text=(paste("data",year,sep = "_"))))$UniqueCarrier))
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel(title="Test"),
  sidebarLayout(
    sidebarPanel(
      selectInput("rok","Wybierz rok",choices=c("1988","1989","1990","1991","1992","1993","1994",
                                                "1995","1996","1997","1998","1999","2000","2001","2002","2003"
                                                ,"2004","2005","2006","2007")),
      selectInput("firma","Wybierz firme",choices = NULL)
    ),
  mainPanel(
    plotOutput("flight_data")
  )
  ))



# Define server logic required to draw a histogram
server <- function(input, output,session) {
  observe({
    company<-all_companies(input$rok)
    updateSelectInput(session,"firma","Wybierz firme",choices = company)
  })
  output$flight_data<-renderPlot({
    ratio_plot(eval(parse(text=(paste("data",input$rok,sep = "_")))),as.character(input$firma))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
