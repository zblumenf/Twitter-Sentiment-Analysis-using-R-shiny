options(httr_oauth_cache = TRUE) # enable using a local file to cache OAuth access credentials between R sessions
# Twitter authentication key
oauth = setup_twitter_oauth(consumer_key = "LhLzIn0nbz5mORcE3wPdSmWjP",
                            consumer_secret = "xILbs2S5IbNiZyFwXU7VITcVCxxzf3SpA2Gbvn3qBNF8LY8woQ",
                            access_token = "110651492-aB9iL1exrmkb3Q2gmM2DEqCzz6eo0TQiqqjRRXec",
                            access_secret = "8XZIQ6eVvOwAwhvGSSIL4SEitrttjAkf6SYAJHFXMFxz9")
