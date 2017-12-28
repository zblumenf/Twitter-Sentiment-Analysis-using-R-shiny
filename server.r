#SERVER.R
# Server is a function used to render the objects created in the User Interface function of the shiny Application
# It takes the input and output as an argument
server = function(input, output)
{
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  ##ggmap query

  data0 = eventReactive(input$goButton, {
    # Conditional data extraction based on user input, if the InputID selected by user = hashtag
    if (input$typeInput == "hashtag") 
    {
      #Google lat-lon location. Zip or city or address....anything googlable.  Draw error if poorly entered
      google_loc = geocode(input$loc,output = "latlona",source="google")
      validate(need(!is.na(google_loc$lon),"Please enter a different location.  Cannot geo-locate"))
      
      #clean radius variable and draw error if poorly entered
      radius  = gsub("[^(0-9)|\\.]",'',input$rad) %>% as.numeric()
      validate(need(!is.na(radius) & radius > 0,"Please enter a valide number for radius in miles")) 
      tweetOutput = searchTwitter(input$hashtagInput, n = input$numberInput
                    ,geocode = paste0(google_loc$lat, ",", google_loc$lon, ",",radius,"mi"))

    } 
    # Conditional data extraction based on user input, if the InputID selected by user = username
    else if (input$typeInput == "username") 
    {
      #tweetOutput <- userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
      tweetOutput <- userTimeline(input$usernameInput,n = input$numberInput)
    }
    else {stop("Logical error in data1().  Please contact developers")}
    #Cleans the tweet
    df.tweets = cleanTweets(tweetOutput)
  })
  
  tm_sentiment = reactive({
    nrc.lexicons = get_nrc_sentiment(data0()$text_clean)
  })
  tm_sentiment_vec = reactive({
    colSums(tm_sentiment())
  })
  tm_emotion_vec = reactive({
    aa = sort(tm_sentiment_vec()[1:8])
   #print(aa)
    #print(str(aa))
    aa
  })
  tm_valence_vec = reactive({
    tm_sentiment_vec()[9:10]
  })
  # ## Rendering TM plots ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot1 and plot2
  callModule(percentage_bar,id = "Sentiment_Plots_TM",vec = tm_emotion_vec,title = "Emotions in tweets", xlab="Percentage")
  callModule(percentage_bar,id = "Pos_Neg_Plots_TM",vec = tm_valence_vec, 
             title = "Ratio of positive to negative tweets",  xlab="Percentage")
  tfidf_sentiment = reactive({
    searchtweet.tdm.tfidf = tdm.TFIDF(data0())
    nrc.lex = getSentiments.TF_IDF.nrc(searchtweet.tdm.tfidf)
  })
  tfidf_sentiment_vec = reactive({
    colSums(tfidf_sentiment())
  })
  tfidf_emotion_vec = reactive({
    aa = sort(tfidf_sentiment_vec()[1:8])
    #print(aa)
    #print(str(aa))
    aa
  })
  tfidf_valence_vec = reactive({
    tfidf_sentiment_vec()[9:10]
  }) 
  callModule(percentage_bar,id = "Sentiment_Plots_TFIDF",vec = tfidf_emotion_vec,title = "Emotions in tweets", xlab="Percentage")
  callModule(percentage_bar,id = "Pos_Neg_Plots_TFIDF",vec = tfidf_valence_vec, 
             title = "Ratio of positive to negative tweets",  xlab="Percentage")

  term_doc_mat_tm = reactive({ #eventReactive(input$goButton, {
    searchtweet.tdm.tm.stopword = tdm.tmStopWord(data0())
  })
  pos_tweets_tm = reactive({ #eventReactive(input$goButton, {
    tweets.positive = generateWordCloud.positive.tmStopWords(term_doc_mat_tm())
  })
  neg_tweets_tm = reactive({ #eventReactive(input$goButton, {
    tweets.negative = generateWordCloud.negative.tmStopWords(term_doc_mat_tm())
  })
  
  # ## Render Positive Wordcloud TM ##
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud1
  wordCloud_pos_tm = reactive({ #run reactive to make it update automatically
    dd =  pos_tweets_tm()
    wordcloud2(data =dd)
  })
  output$wordCloud_pos_tm = renderWordcloud2({wordCloud_pos_tm()})
  # ## Render negative wordcloud TM ##
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud2
  wordCloud_neg_tm = reactive({ #run reactive to make it update automatically
    dd =  neg_tweets_tm()
    wordcloud2(data =dd)
  })
  output$wordCloud_neg_tm = renderWordcloud2({wordCloud_neg_tm()})
  
  term_doc_tfidf  = reactive ({
    tdm.tfidf = tdm.TFIDF(data0())
    
    tdm.tm.nostop = tdm.tm(data0())
    d_list = list()
    d_list$tweets.positive = generateWordCloud.positive.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    d_list$tweets.negative = generateWordCloud.negative.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    d_list
  })
  # ##Rendering positive wordcloud for TFIDF ## #
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud3
  # wordCloud_pos_tfidf= reactive({ #run reactive to make it update automatically
  #   dd =  pos_tweets_tm()
  #   wordcloud2(data =dd)
  # })
  # out
  output$wordCloud_pos_tfidf = renderWordcloud2({wordcloud2(data = term_doc_tfidf()$tweets.positive)})
  # ##Render negative wordcloud TFIDF## #
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud4
  output$wordCloud_neg_tfidf = renderWordcloud2({wordcloud2(data = term_doc_tfidf()$tweets.negative)})
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data7 = eventReactive(input$goButton, {
    # 
    # if (input$typeInput == "hashtag") 
    # {
    #   
    #   geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
    #   
    #   tweetOutput = searchThis(search_string = input$hashtagInput,
    #                              number.of.tweets = input$numberInput, geocode_string = geocode.string)
    #   
    # } 
    # 
    # else if (input$typeInput == "username") 
    # {
    #   tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    # }
    # 
    # else {}
    # 
    # #Converting the Tweets into data frame
    # df.tweets = twListToDF(tweetOutput)
    
    #only displaying Text, Created, Screen Name, RT count, and Location
    
    # Remove all nongraphical characters
    text = str_replace_all(data0()$text,"[^[:graph:]]", " ")
    df.tweets = cbind(text, data0()[c(5,11,3,12,17)])
    
    #Changing column names
    colnames(df.tweets) = c("Tweets", "Date", "Username", "Fav Count", "RT Count", "Location")
    tweetOutput = df.tweets
  })
  
  # ##Render tweets## #
  # renderDataTable - Renders a reactive data table that is suitable for assigning to an output slot.
  # In this case the the object used is tweetTable
  output$tweetTable = renderDataTable({data7()}, options = list(lengthMenu = c(10, 30, 50), pageLength = 5))

}



