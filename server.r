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
      #getLatLong.zip - to extract the zipcode from the user input and generate a user defined radius 
      #geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      tweetOutput = searchTwitter(input$hashtagInput, n = input$numberInput
                    ,geocode = paste0(google_loc$lat, ",", google_loc$lon, ",",radius,"mi"))
      #tweetOutput - extracts the hashtag provided as input in the shiny application along with the number of tweets and the geographical location
      # tweetOutput = searchThis(search_string = input$hashtagInput,
      #                            number.of.tweets = input$numberInput, 
      #                          geocode_string = paste0(google_loc$lat, ",", google_loc$lon, ",",radius,"mi"))
      # 
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
    sort(tm_sentiment_vec()[1:8])
  })
  tm_valence_vec = reactive({
    tm_sentiment_vec()[9:10]
  })
  # ## Rendering TM plots ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot1 and plot2
  callModule(percentage_bar,id = "Sentiment_Plots_TM",vec = tm_emotion_vec)
  callModule(percentage_bar,id = "Pos_Neg_Plots_TM",vec = tm_valence_vec)
  # output$plot1 = renderPlot({
  #     # ## creating Barplot for emotions ##
  #     barplot(
  #       #Sort  - (or order) a vector or factor (partially) into ascending or descending order
  #       #prop.table - Express Table Entries as Fraction of Marginal Table
  #       sort(colSums(prop.table(data1()[, 1:8]))), 
  #       horiz = TRUE, 
  #       cex.names = 0.8, 
  #       las = 1, 
  #       main = "Emotions in tweets", xlab="Percentage", xlim = c(0,.4))}, 
  #         width = 700, height = 500)
  
  # output$plot2 = renderPlot({
  #   
  #     # ## Creating barplot for positive vs negative ##
  #     barplot(
  #       sort(colSums(prop.table(data1()[, 9:10]))), 
  #       horiz = TRUE, 
  #       cex.names = 0.75, 
  #       las = 1, 
  #       main = "Ratio of positive to negative tweets",xlab="Percentage", xlim = c(0,1))},
  #         width = 700, height = 500)
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data2 = reactive ({#eventReactive(input$goButton, {
    
    # if (input$typeInput == "hashtag") 
    # {
    #   
    #   # ## Generate geocode string ## #
    #   geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
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
    # searchtweet.clean = cleanTweets(tweetOutput)
    
    searchtweet.tdm.tfidf = tdm.TFIDF(data0())
    nrc.lex = getSentiments.TF_IDF.nrc(searchtweet.tdm.tfidf)
  })
  
  # ## Creating a Render plots for TFIDF ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot3 and plot4
  output$plot3 = renderPlot({
    barplot(
      sort(colSums(prop.table(data2()[, 1:8]))), 
      horiz = TRUE, 
      cex.names = 0.75, 
      las = 1, 
      main = "Emotions in tweets", xlab="Percentage",xlim = c(0,.4))}, width = 700, height = 500)
  
  output$plot4 = renderPlot({
    barplot(
      sort(colSums(prop.table(data2()[, 9:10]))), 
      horiz = TRUE, 
      cex.names = 0.8,
      las = 1, 
      main = "Polarity in tweets", xlab="Percentage", xlim = c(0,1))}, width = 700, height = 500)
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data3 = reactive({ #eventReactive(input$goButton, {
    
    # if (input$typeInput == "hashtag") 
    # {
    #   geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
    #   
    #   tweetOutput = searchThis(search_string = input$hashtagInput,
    #                              number.of.tweets = input$numberInput, geocode_string = geocode.string)
    #   
    # } 
    # 
    # else if (input$typeInput == "username") 
    # {
    #   userTL = function(user.name,number.of.tweets = 100)
    #   {
    #     userTimeline(user.name,n = number.of.tweets)
    #   }
    #   
    #   tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    # }
    # 
    # else {}
    
    
    # df.tweets <- cleanTweets(tweetOutput)
  
    searchtweet.tdm.tm.stopword = tdm.tmStopWord(data0())
    
    tweets.positive = generateWordCloud.positive.tmStopWords(searchtweet.tdm.tm.stopword)
    
  })

  # ## Render Positive Wordcloud TM ##
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud1
  output$wordCloud1 = renderWordcloud2({wordcloud2(data = data3())})
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data4 = reactive({ #eventReactive(input$goButton, {
    
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
    #   userTL = function(user.name,number.of.tweets = 100)
    #   {
    #     userTimeline(user.name,n = number.of.tweets)
    #   }
    #   
    #   tweetOutput <- userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    # }
    # 
    # else {}
    # 
    # 
    # df.tweets = cleanTweets(tweetOutput)
    
    searchtweet.tdm.tm.stopword = tdm.tmStopWord(data0())
    
    tweets.negative = generateWordCloud.negative.tmStopWords(searchtweet.tdm.tm.stopword)
    
  })
  
  # ## Render negative wordcloud TM ##
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud2
  output$wordCloud2 = renderWordcloud2({wordcloud2(data = data4())})
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data5 = reactive ({ #eventReactive(input$goButton, {
    
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
    #   userTL = function(user.name,number.of.tweets = 100)
    #   {
    #     userTimeline(user.name,n = number.of.tweets)
    #   }
    #   
    #   tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    # }
    # 
    # else {}
    # 
    # 
    # df.tweets = cleanTweets(tweetOutput)
    
    tdm.tfidf = tdm.TFIDF(data0())
    
    tdm.tm.nostop = tdm.tm(data0())
    
    tweets.positive = generateWordCloud.positive.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    
  })
  
  # ##Rendering positive wordcloud for TFIDF ## #
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud3
  output$wordCloud3 = renderWordcloud2({wordcloud2(data = data5())})
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data6 = eventReactive(input$goButton, {
    
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
    #   userTL = function(user.name,number.of.tweets = 100)
    #   {
    #     userTimeline(user.name,n = number.of.tweets)
    #   }
    #   
    #   tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    # }
    # 
    # else {}
    # df.tweets = cleanTweets(tweetOutput)
    
    tdm.tfidf = tdm.TFIDF(data0())
    
    tdm.tm.nostop = tdm.tm(data0())
    
    tweets.negative = generateWordCloud.negative.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    
  })
  
  # ##Render negative wordcloud TFIDF## #
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud4
  output$wordCloud4 = renderWordcloud2({wordcloud2(data = data6())})
  
  
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



