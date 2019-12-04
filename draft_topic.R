# read in the libraries we're going to use
library(tidyverse) # general utility & workflow functions 
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming

  
# load required packages
library(readr) # reading and writing delimited text files
library(dplyr) # SQL-style data processing
library(tidytext) # text analysis in R
library(stringr) # working with text strings
library(lubridate) # working with times and dates
library(jsonlite) # reading and writing JSON
library(tidyr) # data reshaping


# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = F, # return a plot? TRUE by defult
                                   number_of_topics = 2) # number of topics (4 by default)
{    
    # create a corpus (type of object expected by tm) and document term matrix
    Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
    

  Corpus <- tm_map(Corpus, content_transformer(tolower)) 
  Corpus <- tm_map(Corpus, content_transformer(function(x) gsub("[[:punct:]]+"," ", x)))
  Corpus <- tm_map(Corpus, function(x) removePunctuation(x,
                                    preserve_intra_word_contractions = FALSE,
                                    preserve_intra_word_dashes = FALSE))
  Corpus <- tm_map(Corpus, removeNumbers)
  Corpus <- tm_map(Corpus, removeWords, stopwords("english"))
  #c_ham <- tm_map(c_ham, stemDocument, lazy = TRUE)

    DTM <- DocumentTermMatrix(Corpus) # get the count of words/document

    # remove any empty rows in our document term matrix (if there are any 
    # we'll get an error when we try to run our LDA)
    unique_indexes <- unique(DTM$i) # get the index of each unique value
    DTM <- DTM[unique_indexes,] # get a subset of only those indexes
    
    # preform LDA & get the words/topic in a tidy text format
    lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
    topics <- tidy(lda, matrix = "beta")

    # get the top ten terms for each topic
    top_terms <- topics  %>% # take the topics data frame and..
      group_by(topic) %>% # treat each topic as a different group
      top_n(10, beta) %>% # get the top 10 most informative words
      ungroup() %>% # ungroup
      arrange(topic, -beta) # arrange words in descending informativeness
    one_term <- top_terms[seq(1,nrow(top_terms),10),]
    

    # if the user asks for a plot (TRUE by default)
    if(plot == T){
        # plot the top ten terms for each topic in order
        top_terms %>% # take the top terms
          mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
          ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
          geom_col(show.legend = FALSE) + # as a bar plot
          facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
          labs(x = NULL, y = "Beta") + # no x label, change y label 
          coord_flip() # turn bars sideways
    }else{ 
        # if the user does not request a plot
        # return a list of sorted terms instead
        return(one_term)
        #return(top_terms)
    }
}


 
patents <- read.csv("data/patent_text.csv", header = TRUE, stringsAsFactors = FALSE)
glimpse(patents)
# Get abstracts
sample <- data.frame(txt=as.character(patents$abstract)) 
sample$txt<-str_split(sample$txt, " ")
df<-top_terms_by_topic_LDA(sample, number_of_topics = nrow(patents))
# 
topic_df <- cbind(patents, df))
head(topic_df)
draft_topic_file <- write.csv(df,"data/topic_df.csv")
