library(shiny)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(plotly)
library(ggthemes)
library(tidyverse)
library(tidytext)
library(plotrix)
library(syuzhet)
library(reshape2)


ui <- fluidPage( titlePanel("tweetCloud2"),theme = shinythemes::shinytheme('flatly'),
sidebarLayout(
        sidebarPanel(
                textInput('username','Enter a twitter username:','joerogan'),
                uiOutput("github"),
                uiOutput("linkedin"),
                sliderInput("n","Querry Size (in tweets):",1,100,50),
                uiOutput("user_name"),
                uiOutput("screen_name"),
                uiOutput("location"),
                uiOutput("followers_count"),
                uiOutput("description"),
                uiOutput("profile_url")),
        
        mainPanel(
            tabsetPanel(
                tabPanel('WordCloud',plotOutput('cloud')),
                tabPanel("Tweets",tableOutput('tweets')),
                tabPanel("Top15 Tweeted Words",plotlyOutput('top15',height = '800px')),
                navbarMenu("Sentiment Analysis",
                           tabPanel("Intution Chart",plotOutput("sentiment_chart",width="75%")),
                           tabPanel("Emotion Chart",plotOutput('emotions_plot',width="85%",height = '600px')),
                           tabPanel("Comparison Cloud",plotOutput("comparison_cloud",width="70%",height = '600px')))
                
            )
         
                                    )
))
server <- function(input, output) {
    #SideBar stuff
    code <- a("MuhammadEzzatHBK", href="https://github.com/MuhammadEzzatHBK/tweetCloud")
    output$github <- renderUI({tagList("Source Code:", code)})
    account <- a("Muhammad Ezzat", href="https://www.linkedin.com/in/muhammad-ezzat-27600b19b/")
    output$linkedin <- renderUI({tagList("Meet the creator:", account)})
    user <- reactive({rtweet::lookup_users(input$username)})
    output$user_name <- renderUI({tagList(user()$name)})
    output$screen_name <- renderUI({tagList("@", user()$screen_name)})
    output$location <- renderUI({tagList("Location: ", user()$location)})
    output$followers_count<- renderUI({tagList("Number of followers: ", user()$followers_count)})
    output$description<- renderUI({tagList("BIO: ", user()$description)})
    profile <- reactive({a(user()$profile_url,href=user()$profile_url)})
    output$profile_url<- renderUI({tagList("Website:  ", profile())})
    
    #Authentication
    rtweet::create_token(app = '',
                         consumer_key = '',
                         consumer_secret = '',
                         access_token = '',
                         access_secret = '')
    
    #Extract Tweets
    timeline <- reactive({rtweet::get_timeline(input$username,n=input$n)})
    tweets <- reactive({unlist(timeline()%>%select(text))})
    
    
    #Processing Tweets
    clean_tweets <- reactive({gsub("http\\S+",'',gsub('@\\w+ ?','',gsub('#\\w+ ?','',tweets())))})
    corpus <- reactive({Corpus(VectorSource(clean_tweets()))})
    removed_numbers <- reactive({tm_map(corpus(), removeNumbers)})
    removed_stopwords <- reactive({tm_map(removed_numbers(), removeWords, stopwords("english"))})
    removed_punctuation <- reactive({tm_map(removed_stopwords(), removePunctuation)})
    stemmed_document <- reactive({tm_map(removed_punctuation(), stemDocument)})
    document_matrix <- reactive({TermDocumentMatrix(stemmed_document())})    
    
    #Getting WordFrequency
    sorted_sum_of_rows <- reactive({sort(rowSums(as.matrix(document_matrix())),decreasing=TRUE)})
    frequency_array <- reactive({data.frame(word = names(sorted_sum_of_rows()),freq=sorted_sum_of_rows())})
    
    #Drawing WordCloud
    set.seed(1234)
    word_cloud <-reactive({wordcloud(words = frequency_array()$word, freq = frequency_array()$freq,
                                min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, 
                                colors=brewer.pal(8, "Dark2"))})
    output$cloud <- renderPlot({word_cloud()},height = 700, width = 600 )
    output$tweets <- renderTable({tweets()[1:input$n]})
    
    #Top15Words BarPlot
    top15 <- reactive({ggplotly(ggplot(frequency_array()[1:15,],
                                          aes(x=reorder(frequency_array()$word[1:15],
                                          frequency_array()$freq[1:15]),y=frequency_array()$freq[1:15]))+
                                          geom_col()+theme_hc()+coord_flip()+ggtitle('Top15 Tweeted Words')+
                                          xlab('Word')+ylab("Frequency"))})
    output$top15 <- renderPlotly(top15())
    
    #Sentiment Analysis
      #Inutition Chart
    text = reactive({paste(tweets(), collapse = ' ')})
    tokens <- reactive({data_frame(text = text()) %>% unnest_tokens(word, text)})
    sentiment_table <- reactive({tokens() %>%
            inner_join(get_sentiments("bing")) %>%
            count(sentiment)})
    output$sentiment_table <- renderTable(sentiment_table())
    sentiment_chart <- reactive({pie3D(sentiment_table()$n,labels = c("Negative","Positive"),
                                     explode=0.075,main = "Sentiments Intuition")})
    output$sentiment_chart = renderPlot(sentiment_chart())
    
      #Emotion Chart
    emotion_matrix <- reactive({get_nrc_sentiment(text())})
    transposed_matrix <- reactive({data.frame(t(as.matrix(emotion_matrix())))})
    values <- reactive({as.vector(transposed_matrix()[,1])})
    emotion_data <- reactive({data.frame(names(emotion_matrix()),values())[1:8,]})
    emotions_plot <- reactive({ggplot(emotion_data(),aes(reorder(emotion_data()[,1],-emotion_data()[,2]),
                              emotion_data()[,2]))+geom_col()+theme_hc()+xlab("Emotion")+ylab("Count")+
                              ggtitle("Sentiments Emotion")})
    output$emotions_plot <- renderPlot(emotions_plot())
    
      #Comparison Cloud
    comparison_cloud <-reactive({tokens() %>%inner_join(get_sentiments("bing")) %>%
                       count(word, sentiment, sort = TRUE) %>% acast(word ~ sentiment, value.var = "n", fill = 0) %>%
                       comparison.cloud(colors = c("dark red", "dark green"), title.size =2)}) 
    output$comparison_cloud <- renderPlot(comparison_cloud())
    
}

shinyApp(ui = ui, server = server)
