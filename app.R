
library(shiny)

ui <- fluidPage(

    titlePanel("tweetCloud"),
    theme = shinythemes::shinytheme('flatly'),
    sidebarLayout(
        sidebarPanel(
           textInput('username','Enter a twitter username:','johncena'),
           uiOutput("github"),
           uiOutput("linkedin")
        ),

        mainPanel(
           tabsetPanel(
               tabPanel('WordCloud',plotOutput('cloud')),
               tabPanel('WordFrequency',plotOutput('bar')),
               tabPanel('Tweets',tableOutput('tweets'))
           )
        )
    )
)

server <- function(input, output) {
    library(tm)
    library(SnowballC)
    library(wordcloud)
    library(RColorBrewer)
    library(dplyr)
    #These are hidden :D
    rtweet::create_token(app = 'tweetDisect',
                         consumer_key = '*****************',
                         consumer_secret = '*******************',
                         access_token = '************',
                         access_secret = '*************')
   timeline <- reactive({rtweet::get_timeline(input$username,n=50)})
   tweets <- reactive({unlist(timeline()%>%select(text))})
    clean_tweets <- reactive({gsub("http\\S+",'',gsub('@\\w+ ?','',gsub('#\\w+ ?','',tweets())))})
    docs2 <- reactive({Corpus(VectorSource(clean_tweets()))})
    docs3 <- reactive({tm_map(docs2(), removeNumbers)})
    docs4 <- reactive({tm_map(docs3(), removeWords, stopwords("english"))})
    docs5 <- reactive({tm_map(docs4(), removePunctuation)})
    docs6 <- reactive({tm_map(docs5(), stemDocument)})
    dtm <- reactive({TermDocumentMatrix(docs6())})
    m <- reactive({as.matrix(dtm())})
    v <- reactive({sort(rowSums(m()),decreasing=TRUE)})
    d <- reactive({data.frame(word = names(v()),freq=v())})
    set.seed(1234)
    cloud <-reactive({wordcloud(words = d()$word, freq = d()$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))})
    barplott <- reactive({barplot(d()[1:10,]$freq, las = 2, names.arg = d()[1:10,]$word,
                                 col ="white", main ="Most tweeted words",
                                 ylab = "Word frequencies")})
   output$cloud <- renderPlot({cloud()},height = 700, width = 600 )
   output$bar <- renderPlot({barplott()})
   output$tweets <- renderTable({tweets()})
   code <- a("MuhammadEzzatHBK", href="https://github.com/MuhammadEzzatHBK/tweetCloud")
   output$github <- renderUI({tagList("Source Code:", code)})
   account <- a("Muhammad Ezzat", href="https://www.linkedin.com/in/muhammad-ezzat-27600b19b/")
   output$linkedin <- renderUI({tagList("Meet my creator:", account)})
}

shinyApp(ui = ui, server = server)
