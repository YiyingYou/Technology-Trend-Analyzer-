#setwd("C:/Users/3y/Desktop/4100R")
#install.packages('ggeffects')
library(shiny)
library(dplyr)
library(ggplot2)
library(ggeffects)
library(gridExtra)

jl = read.csv("Job_titles.csv")
tl = read.csv("freq.csv")
cates <- unique(tl$Category)


ui <- fluidPage(
  titlePanel("Technology Trend Analyzer"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6, actionButton("go", "TRACK")),
      ),
      downloadButton("dldata","Download .csv")
    )
    ,
    mainPanel(
      plotOutput("plot1",height = '2000px'),
      hr(),
      plotOutput("plot2",height = '2000px'),
      hr(),
      plotOutput("plot3",height = '1500px')
    )
  )
)

server <- function(input, output, session) {
  
  df = read.csv('technologies_counted.csv')
  
  v <- reactiveValues(data = NULL)
  
  output$dldata <- downloadHandler(
    filename = function() {
      paste0("technologies_frequency", ".csv")
    },
    content = function(file) {
      vroom::vroom_write(tl[, c(2, 3, 4)], file,',')
    }
  )
  
  
  single.plot <- function(i){
    ggplot(tl[tl$Category==cates[i],],aes(x=reorder(Technology,desc(Frequency)),y=Frequency))+ 
      geom_bar(stat = "identity")+
      facet_wrap(vars(Category),nrow = 11)+
      theme(axis.text.x=element_text(angle=30, hjust=1))+ 
      labs(title="Mentioned times of technologies in " %>% paste0(cates[i]), 
           x="Technology", y = "Frequency")
  }
  

  
  bps <- list()
  for (i in 1:length(cates)){
    bpi <- eval(substitute(ggplot(tl[tl$Category==cates[i],],aes(x=reorder(Technology,desc(Frequency)),y=Frequency))+ 
                             geom_bar(stat = "identity")+
                             facet_wrap(vars(Category),nrow = 11)+
                             theme(axis.text.x=element_text(angle=30, hjust=1))+ 
                             labs(title="Mentioned times of technologies in " %>% paste0(cates[i]), 
                                  x="Technology", y = "Frequency"),list(i=i)))
    bps[[i]] <- bpi
    ggsave(filename = paste0('plot',i,'.png'),plot = last_plot())
  }
  
  
  bp <- df[order(-df$count_tech_total),][1:10,] %>% 
    ggplot(aes(x=reorder(tech,desc(count_tech_total)),y=count_tech_total))+ 
    geom_bar(stat = "identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1))+ 
    labs(title="Mentioned times of technologies in job descriptions", 
         x="Technology", y = "Frequency")
  observeEvent(input$go,{
    v$data <- 1
  })
  observeEvent(input$reset,{
    v$data <- NULL
  })
  output$plot1 <- renderPlot({
    if (is.null(v$data)){
      return()} else {
        return({plot(arrangeGrob(bps[[1]],bps[[2]],bps[[3]],bps[[4]],nrow=4))})
      }
  })
  output$plot2 <- renderPlot({
    if (is.null(v$data)){
      return()} else {
        return({plot(arrangeGrob(bps[[5]],bps[[6]],bps[[7]],bps[[8]],nrow=4))})
      }
  })
  output$plot3 <- renderPlot({
    if (is.null(v$data)){
      return()} else {
        return({plot(arrangeGrob(bps[[9]],bps[[10]],bps[[11]],nrow=3))})
      }
  })
}

shinyApp(ui, server)
