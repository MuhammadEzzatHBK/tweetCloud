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
library(waffle)
library(emojifont)
library(waiter)

loading_screen <- tagList(
  h1("Please Wait", style = "color:white;"),
  img(src = 'logo.png', height = "400px"))


ui <- fluidPage(use_waiter(),titlePanel("TweetDisect"),theme = shinythemes::shinytheme('flatly'),
sidebarLayout(
        sidebarPanel(
                textInput('username','Enter a twitter username:','hadleywickham'),
                sliderInput("n","Querry Size (in tweets):",1,100,75),
                uiOutput("user_name"),
                uiOutput("screen_name"),
                uiOutput("location"),
                uiOutput("followers_count"),
                uiOutput("description"),
                uiOutput("profile_url"),
                HTML("<br>"),
                downloadButton("report", "Download custom report"),
                HTML("<br><br>"),
                img(src='logo.png', height = 140, width = 140,align='center'),
                uiOutput("github"),
                uiOutput("linkedin")),
                
        mainPanel(
            tabsetPanel(
                navbarMenu("Frequency Plots",
                           tabPanel("Tweet Frequency",plotlyOutput('ts'),DT::DTOutput("tweets")),
                           tabPanel("Top15 Tweeted Words",plotOutput('top15',height = '800px'))),
                tabPanel("Mutualism",HTML("<b>Mutual/Disparate Relationships</b>"),plotOutput('relationships'),
                         actionButton("waffle", "i.e")),
                navbarMenu("Sentiment Analysis",
                           tabPanel('WordCloud',HTML("<b>Word Cloud</b>"),plotOutput('cloud'),HTML("<br><br><br><br><br><br><br><br><br><br><br><br>"),
                                    actionButton("cloud", "i.e")),
                           tabPanel("+/-",fluidRow(
                             column(5,HTML("<b>Sentiments Intution</b>"),plotOutput("sentiment_chart",width="100%")),
                             column(7,HTML("<b>Comparison Cloud</b>"),plotOutput("comparison_cloud",width="100%",height = '600px'))
                           ),actionButton("pie", "i.e")),                  
                           tabPanel("Emotion Chart",plotOutput('emotions_plot',width="85%",height = '600px'),actionButton("emotions", "i.e")))

                
            )
         
                                    )
))
server <- function(input, output) {
    #Waiter
    w <- Waiter$new(html = loading_screen, color = "lightblue")
    w$show()
    Sys.sleep(2)
    w$hide()
    
    #Observers
    observeEvent(input$waffle, {
      showModal(modalDialog(
        title = "Mutual/Disparate Relationships",
        paste("Who follwos",input$username,"back after",input$username,"follows them?"),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    observeEvent(input$cloud, {
      showModal(modalDialog(
        title = "WordCloud",
        paste("The more",input$username,"says a particular word, the bigger it gets on the cloud"),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    observeEvent(input$pie, {
      showModal(modalDialog(
        title = "Positive / Negative sentiments",
        paste("What does",input$username,"bring to the twitter society?  Findout how we compare his 
              positive & negative sentiments to each other in terms of both quantity & quality."),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    observeEvent(input$emotions, {
      showModal(modalDialog(
        title = "Emotion Chart",
        paste("Let's take a deep dive into ",input$username," feelings & listen to what his heart has to say to us."),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
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
    output$profile_url<- renderUI({tagList("External:  ", profile())})
    
    #Authentication
    token <- rtweet::create_token(app = 'tweetDisect',
                         consumer_key = 'HIDDEN',
                         consumer_secret = 'HIDDEN',
                         access_token = 'HIDDEN-HIDDEN',
                         access_secret = 'HIDDEN')
    
    #Extract Tweets
    timeline <- reactive({rtweet::get_timeline(input$username,n=input$n,token = token)})
    tweets <- reactive({unlist(timeline()%>%select(text))})
    output$tweets <- DT::renderDT(as.data.frame(tweets()))
    
    
    
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

    #Top15Words BarPlot
    top15 <- reactive({ggplot(frequency_array()[1:15,],
                                          aes(x=reorder(frequency_array()$word[1:15],
                                          frequency_array()$freq[1:15]),y=frequency_array()$freq[1:15]))+
                                          geom_col()+theme_hc()+coord_flip()+ggtitle(paste(
                                          "Top 15 Tweeted Words by",user()$name))+
                                          xlab('Word')+ylab("Frequency")})
    output$top15 <- renderPlot(top15())
    
    #Sentiment Analysis
      #Inutition Chart
    text = reactive({paste(tweets(), collapse = ' ')})
    tokens <- reactive({data_frame(text = text()) %>% unnest_tokens(word, text)})
    sentiment_table <- reactive({tokens() %>%
        inner_join(get_sentiments("bing")) %>%
        dplyr::count(sentiment)})
    output$sentiment_table <- renderTable(sentiment_table())
    sentiment_chart <- reactive({pie3D(sentiment_table()$n,labels = c("Negative","Positive"),
                                       explode=0,col = c('firebrick2','seagreen2'))})
    output$sentiment_chart = renderPlot(sentiment_chart())
    
      #Emotion Chart
    matrix <- reactive({get_nrc_sentiment(text())})
    emotion_matrix <- reactive({matrix()[,1:8]})
    transposed_matrix <- reactive({data.frame(t(as.matrix(emotion_matrix())))})
    values <- reactive({as.vector(transposed_matrix()[,1])})
    emotion_data <- reactive({data.frame(names(emotion_matrix()),values())[1:8,]})
    emotions_plot <- reactive({ggplot(emotion_data(),aes(reorder(emotion_data()[,1],-emotion_data()[,2]),
                              emotion_data()[,2]))+geom_col()+theme_hc()+xlab("Emotion")+ylab("Count")+
                              ggtitle("Sentiments Emotion")})
    output$emotions_plot <- renderPlot(emotions_plot())
    
      #Comparison Cloud
    comparison_cloud <-reactive({tokens() %>%inner_join(get_sentiments("bing")) %>%
                       dplyr::count(word, sentiment, sort = TRUE) %>% acast(word ~ sentiment, value.var = "n", fill = 0) %>%
                       comparison.cloud(colors = c("firebrick2", "seagreen4"), title.size =2)}) 
    output$comparison_cloud <- renderPlot(comparison_cloud())
    
    #Mutual/Disaprate ratio
    followers <- reactive({rtweet::get_followers(input$username)})
    friends <- reactive({rtweet::get_friends(input$username)})
    common <- reactive({length(intersect(followers()$user_id, friends()$user_id))})
    uncommon <- reactive({length(setdiff(followers()$user_id, friends()$user_id))})
    relationships <- reactive({c(`Mutual` = as.integer(common()),`Disparate`=as.integer(uncommon()))})
    relationships_chart <- reactive({waffle(relationships()/20,colors = c('seagreen4','firebrick2'))})
    output$relationships <- renderPlot(relationships_chart())
    
    #TimeSeries
    ts <- reactive({rtweet::ts_plot(rtweet::get_timeline(input$username,token = token),"month")+theme_minimal()+xlab('Month')+ylab('Number Of Tweets')+
        ggtitle("Tweet Frequency")})
    ts_plotly <- reactive({ggplotly(ts())})
    output$ts <- renderPlotly(ts_plotly())
    
    #Report
    output$report <- downloadHandler(
      
      filename = "report.docx", ###### 1
      content = function(file) {
        
        tempReport <- file.path(tempdir(), "report.Rmd") ###### 2
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        
        params <- list(username = input$username,
                       waffle = relationships_chart(),
                       ts = ts(),
                       tweets = as.data.frame(tweets())[1:3,],
                       top15 = top15(),
                       emotions = emotions_plot()) ###### 3
        
        
        rmarkdown::render(tempReport, output_file = file, ###### 4
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
     
    )


}

shinyApp(ui = ui, server = server)




















