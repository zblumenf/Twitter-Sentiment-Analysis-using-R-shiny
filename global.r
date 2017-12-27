#GLOBAL.R
## Allocate memory
options(java.parameters = "-Xmx10g")
## clear console
cat("\014")
## clear global variables
#rm(list=ls()) # ZB: Unecessary for Shiny Session
## list of packages required
list.of.packages = c("git2r","digest","devtools",
                     "RCurl","RJSONIO","stringr","syuzhet","httr",
                     "rjson","tm","NLP","RCurl","wordcloud","wordcloud2",
                     "tidytext","dplyr","zipcode","bit", "shiny", "shinythemes",
                     "ggmap","plotly")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
## for devtools
library(git2r);library(digest)
#require(devtools)
#install_github("hadley/devtools")
library(devtools)
install_github("geoffjentry/twitteR")
## data manipultion
library(dplyr);library(stringr)
# loading the libraries
## Linked to importing tweets
library(rjson);library(httr);library(twitteR);library(zipcode)
## Linked to generating a wordcloud
library(tm);library(NLP);library(RCurl);library(RJSONIO)
library(stringr);library(wordcloud);library(wordcloud2); 
#To create Shiny environment
library(shinythemes)
## Linked to sentiment analysis
library(syuzhet)
##  to get lat lon from location entry
#library(ggmap)
#devtools::install_github("dkahle/ggmap") #So we can register an API key
library(ggmap)
#register_google(key = "AIzaSyC7NVLGTjqwYj_AS35Eki5oX9CLtBfMnYU")
# to gnerate plots
library(plotly)
#############################################################
source("twitter_oauth.R")
cat("\014") #clear console
source("helper_functions.R")
source("modules.R")
