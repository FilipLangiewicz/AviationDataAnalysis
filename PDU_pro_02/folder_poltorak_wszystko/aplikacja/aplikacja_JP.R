#Trzeba otworzyć w przeglądarce bo z jakiegoś powodu nie działa w okienku
#Trzeba ustawić ścieżkę w ratio_plot i all_companies, tam gdzie znajdują się ramki do aplikacji

ratio_plot<-function(year,company){
  tested<-read.csv(paste("D:\\R\\app_data_",year,".csv",sep=""))[,(-1)]
  
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
  
  transposed_data %>% ggplot(aes(przedziały,Value,fill=Company))+geom_col(position = "dodge")+labs(title=paste("Podział współczynników opóźnień przylotów dla firmy",company,"w roku:",as.character(year)),caption=(paste(text1,text2,sep="\n")),x="Przedziały",y="Ratio")+scale_x_discrete(limits=positions)+ theme_bw() + scale_fill_manual(name="Firma",values=c("#fdbe85","#d94701"))
}


all_companies<-function(year){
  tested<-read.csv(paste("D:\\R\\app_data_",year,".csv",sep=""))[,(-1)]
  tested<-tested[tested$UniqueCarrier!="All_data",]
  sort(unique(tested$UniqueCarrier))
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel(title="Opóźnienia przylotów w poszczególnych przedziałach czasowych"),
  sidebarLayout(
    sidebarPanel(
      selectInput("rok","Wybierz rok",choices=c("1988","1989","1990","1991","1992","1993","1994",
                                                "1995","1996","1997","1998","1999","2000","2001","2002","2003"
                                                ,"2004","2005","2006","2007")),
      selectInput("firma","Wybierz firmę",choices = NULL)
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
    ratio_plot(as.character(input$rok),as.character(input$firma))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
