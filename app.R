#Twitter Dataset Generator
#Duncan McKinnon

run_all<-function(HasRun = F)
{
  if(!HasRun){
    suppressPackageStartupMessages(
      {
        #install or load required packages into the workspace
        package_list <- c('stringr', 'twitteR', 'shiny')
        non_installed <- package_list[!(package_list %in% installed.packages()[,"Package"])]
        if(length(non_installed)) install.packages(non_installed)
        require('stringr')
        require('twitteR')
        require('shiny')
      }
    )
    
#TWITTER APP DATA
#    MYCONSUMERKEY<- #Your Consumer Key
#    MYCONSUMERSECRET<- #Your Consumer Secret
#    MYACCESSTOKEN<- #Your Access Token
#    MYACCESSSECRET<-  #Your Access Secret
    
    origop <- options("httr_oauth_cache")
    options(httr_oauth_cache=TRUE)
    setup_twitter_oauth(MYCONSUMERKEY, MYCONSUMERSECRET, MYACCESSTOKEN, MYACCESSSECRET)
    options(httr_oauth_cache=origop)
  }
}



FormatTweetSearchData<-function(term = "", num = 3)
{
  dataset <- data.frame("Number" = c(), "Tweet_ID" = c(), "Num_Favorited" = c(), "Num_Retweets" = c(), "Time" = c(), "Tweet" = c())
  if(term == "") 
  {
    data <- Rtweets(n = num, lang = 'en-us', resultType = "popular")
  }
  else
  {
    data <- searchTwitter(term, n = num, lang = 'en-us', resultType = "popular")
  }
  if(length(data) >= 1)
  {
    for(i in 1:length(data))
    {
      dataset <- rbind(dataset, collectTweetData(data[[i]], i))
    }
  }
  colnames(dataset) <- c("Number", "Tweet_ID", "Num_Favorited", "Num_Retweets", "Time", "Tweet")
  return(dataset)
}

collectTweetData <- function(data, number)
{
  id <- data$id
  num_likes <- data$favoriteCount
  num_retweets <- data$retweetCount
  time <- data$created
  tweet <- data$text
  return(data.frame(number, id, num_likes, num_retweets, time, tweet))
}

ui <- fluidPage(
  titlePanel("Collect Twitter Dataset"),
  sidebarLayout(
   sidebarPanel(
    textInput("hash", "Hashtag (No #): ", value = ""),
      numericInput("num", "Desired Size of Dataset: ", value = 10, min = 3, max = 500),
      actionButton("runButton","RUN"),
      downloadLink("downloadData", "Download CSV")
    ),
    mainPanel(
      h2("Dataset"),
      dataTableOutput("TweetData")
    )
  )
)

server <- function(input, output) 
{
  #Run once
  run_all() 
  
  #Run on button
  dataset <- eventReactive(input$runButton, {FormatTweetSearchData(str_c("#", input$hash), input$num)})
  output$TweetData <- renderDataTable(dataset(), searchDelay = 1000)
  
  #Run on download
  output$downloadData <- downloadHandler(filename = str_c("tweetdata-", Sys.Date(), ".csv"), 
                                         content = function(file){return(write.csv(dataset(), file))}, 
                                         contentType = "text/csv")
}

# Run the application 
shinyApp(ui = ui, server = server)
