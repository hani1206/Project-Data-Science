library(shiny)
library(vroom)
library(here)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(SnowballC)
library(Rstem)
library(sentiment)
library(plyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("Analisis Sentimen Binel vs Trump"),
  headerPanel("dengan Naive Bayes"),
  mainPanel(
    tabsetPanel(
      tabPanel("Data Twitter Biden", DT::dataTableOutput('Bidenall')),
      tabPanel("Data Twitter Trump", DT::dataTableOutput('Trumpall')),
      tabPanel("Data Cleaned Biden", DT::dataTableOutput('dataCleanedBiden')),
      tabPanel("Data Cleaned Trump", DT::dataTableOutput('dataCleanedTrump')),
      tabPanel("Data Sentimen Biden", DT::dataTableOutput('tbl')),
      tabPanel("Data Sentimen Trump", DT::dataTableOutput('tbl2')),
      tabPanel("Plot Tweet Biden", plotOutput("sent2"))
      tabPanel("Plot Tweet Trump", plotOutput("sent4"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  Bidenall<- vroom(here("Bidenall.csv"))
  Bidenall<- data.frame(Bidenall)
  # Output Data
  output$Bidenall = DT::renderDataTable({
    DT::datatable(Bidenall, options = list(lengthChange = FALSE))
  })
  
  server2 <- function(input, output) {
    Trumpall<- vroom(here("Trumpall.csv"))
    Trumpall<- data.frame(Trumpall)
    # Output Data
    output$Trumpall = DT::renderDataTable({
      DT::datatable(Trumpall, options = list(lengthChange = FALSE))
    })
    
    sent_df<- vroom(here("dataSentimenBiden.csv"))
    sent_df <- data.frame(sent_df)
    dataCleanedBiden<- vroom(here("dataCleanedBiden.csv"))
    dataCleanedBiden<- data.frame(dataCleanedBiden)
    
    sent_df2<- vroom(here("dataSentimenTrump.csv"))
    sent_df2 <- data.frame(sent_df2)
    dataCleanedTrump<- vroom(here("dataCleanedTrump.csv"))
    dataCleanedTrump<- data.frame(dataCleanedTrump)
    
    # Output Data
    output$dataCleanedBiden = DT::renderDataTable({
      DT::datatable(dataCleanedBiden, options = list(lengthChange = FALSE))
    })
    
    # Output Data
    output$dataCleanedTrump = DT::renderDataTable({
      DT::datatable(dataCleanedTrump, options = list(lengthChange = FALSE))
    })
    
    sent_df<- vroom(here("dataSentimenBiden.csv"))
    sent_df <- data.frame(sent_df)
    # Output Data
    output$tbl = DT::renderDataTable({
      DT::datatable(sent_df, options = list(lengthChange = FALSE))
    })
    
    sent_df2<- vroom(here("dataSentimenTrump.csv"))
    sent_df2 <- data.frame(sent_df2)
    # Output Data
    output$tbl2 = DT::renderDataTable({
      DT::datatable(sent_df2, options = list(lengthChange = FALSE))
    })
    
    # plot distribution of emotions Biden
    ggplot(sent_df, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Joe Biden",
           plot.title = element_text(size=12))
    plotSentiments1 <- function(sentiment_dataframe, title) 
    {
      library(ggplot2)
      ggplot(sentiment_dataframe, aes(x=emotion)) + 
        geom_bar(aes(y=..count.., fill=emotion)) + 
        scale_fill_brewer(palette="Dark2") + 
        ggtitle(title) + 
        theme(legend.position="right") + 
        ylab("Number of Tweets") + 
        xlab("Emotion Categories")
    }
    #plotting tweets emotions
    output$sent1 <- renderPlot({
      plotSentiments1(sent_df, "Sentiment Analysis of Joe Biden")
    })
    
    # plot distribution of polarity
    ggplot(sent_df, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="polarity categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Joe Biden",
           plot.title = element_text(size=12))
    plotSentiments2 <- function(sent_df, title)
    {
      library(ggplot2)
      ggplot(sent_df, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette="RdGy") +
        ggtitle(title) +
        theme(legend.position="right") +
        ylab("Number of Tweets") +
        xlab("Polarity Categories")
    }
    
    output$sent2 <- renderPlot({
      plotSentiments2(sent_df, "Sentiment Analysis of Joe Biden")
    })
    
    # plot distribution of emotions Trump
    ggplot(sent_df2, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Trump",
           plot.title = element_text(size=12))
    plotSentiments3 <- function(sentiment_dataframe, title) 
    {
      library(ggplot2)
      ggplot(sentiment_dataframe, aes(x=emotion)) + 
        geom_bar(aes(y=..count.., fill=emotion)) + 
        scale_fill_brewer(palette="Dark2") + 
        ggtitle(title) + 
        theme(legend.position="right") + 
        ylab("Number of Tweets") + 
        xlab("Emotion Categories")
    }
    #plotting tweets emotions
    output$sent3 <- renderPlot({
      plotSentiments3(sent_df2, "Sentiment Analysis of Trump")
    })
    
    # plot distribution of polarity
    ggplot(sent_df2, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="polarity categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Trump",
           plot.title = element_text(size=12))
    plotSentiments4 <- function(sent_df2, title)
    {
      library(ggplot2)
      ggplot(sent_df2, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette="RdGy") +
        ggtitle(title) +
        theme(legend.position="right") +
        ylab("Number of Tweets") +
        xlab("Polarity Categories")
    }
    
    output$sent4 <- renderPlot({
      plotSentiments4(sent_df2, "Sentiment Analysis of Trump")
    })
    
    
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

