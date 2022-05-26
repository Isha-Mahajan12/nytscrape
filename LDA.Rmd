---
title: "Preprocessing and Topic Modelling"
author: "Isha A Mahajan"
description: |
  An attempt to gather data from the New York Times API
output:
  html_document:
    toc: yes
    toc_depth: 5
    toc_float: yes
    code_folding: hide
    df_print: paged
    theme: spacelab
    highlight: pygment
  pdf_document:
    toc: yes
    toc_depth: '5'
  html_notebook:
    toc: yes
    toc_depth: '5'
---

In my last blog post, I successfully pulled 390 articles from the New York Times which consisted of the keywords Afghanistan and Refugee. I loaded that data into a CSV file. In addition, I also saved the meta data into a separate csv file which is labeled as meta.csv and is part of this repository. 


## Load Required Packages 

```{r}
library(knitr) 
library(DT)
library(tm)
library(topicmodels)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(tidyverse)
library(quanteda)
```


## Load the Data

In my previous post, I created two csv files in order to save the metadata and the content for my analysis. I begin my analysis by loading those in.

```{r}
times <- read_csv("/Users/isha/Desktop/GitHub/TextAnalysisSp22/_posts/2022-04-20-new-york-times-api-data-gathering/Times_API_Data.csv")
head(times)
meta <- read_csv("/Users/isha/Desktop/GitHub/TextAnalysisSp22/_posts/2022-04-20-new-york-times-api-data-gathering/meta.csv")
```


## Important Pre-Processing Decisions

For cleaning my corpus, I have chosen the following pre-processing techniques. 

1.*StopWords* : I remove stop words to keep the meat of the text and reduce computational power. 

2.*LowerCase* : Keeping my text in correct grammatical format is not necessarily going to contribute to my analysis. Therefore, for now, I move all objects in my corpus to lowercase

3.*WhiteSpace Removal*: Removed for computational space. As I had web scraped articles from NYT, there was a lot of White Space that had come with the text. 
So white space is extra for the data set and is not necessarily helpful. 

4.*Remove Numbers* : While I think numbers would be useful to provide statistical information to look for refugees, for now I leave those on the back burner as well. 

5.*Stemming* : Stemming was a difficult choice to make. Given that the web-scraping happened in a pretty untidy manner, and also that I'm looking mostly for a refugee theme within these articles, words like afghan-afghanistan, migrant-refugee might make the corpus larger than needed.Bcause I'm not looking at framing effects art the moment, stemming will not serve useful.

## Pre Processing Text

In order to pre-process my text, I create a function to perform all the tasks at once. See function below:

```{r}
cleaning <- function (x) {
  x <- tolower (x)
  x <- removeWords(x, stopwords('en'))
  x <- removePunctuation(x)
  x <- stripWhitespace(x)
  x <- removeNumbers(x)
  x <- stemDocument(x, language = "en" )
}
```

After creating my corpus, I apply the function to the same. 

```{r}
#create corpus of content from csv
corpus_times<- corpus(times$Content)
#pre-process using the function in 64-72
clean_times <- cleaning(corpus_times)
#re-create a corpus to retain data structure
clean_times_corpus <- corpus(clean_times)
```

## Document Term Matrix

```{r}
a <- list()
for (i in seq_along(clean_times_corpus)) {
    a[i] <- gettext(clean_times_corpus[[i]][[1]]) #Do not use $content here!
}
clean_times_corpus$text <- unlist(a) 
corpus <- Corpus(VectorSource(clean_times_corpus$text)) #This action restores the corpus.
```

```{r}
minimumFrequency <- 10 #set minimum frequency of words
DTM <- dfm(tokens(clean_times_corpus)) #use dfm function to create dfm from tokens
# have a look at the number of documents and terms in the matrix
dim(DTM)
raw.sum=apply(DTM,1,FUN=sum) #sum by raw each raw of the table
DTM =DTM[raw.sum!=0,]
#sel_idx <- slam::row_sums(DTM) > 0
#DTM <- DTM[sel_idx, ]
#times <- times[sel_idx, ]
DTM
```

## LDA 

I begin with topic modeling as this serves as a useful technique to explore large text data in a computational way. This is a form of supervised learning.

```{r}
result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
```


```{r}
FindTopicsNumber_plot(result)
```


```{r}
K <- 20
topicModel <- topicmodels::LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))
```


```{r}
tmResult <- topicmodels::posterior(topicModel)
attributes(tmResult)
```


```{r}
beta <- tmResult$terms #get beta from results
dim(beta)  
```

```{r}
rowSums(beta)  
```

```{r}
theta <- tmResult$topics 
dim(theta)  
rowSums(theta)[1:10] 
```

```{r}
rowSums(theta)[1:10] 
```

```{r}
#terms(tmResult, 10)
```

```{r}
exampleTermData <- terms(topicModel, 10)
exampleTermData[, 1:8]
```

```{r}
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
```

```{r}
topicToViz <- 7 # change for your own topic of interest
topicToViz <- grep('refugee', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilities of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words,probabilities,random.order = FALSE, colors = mycolors )
```


```{r}
countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:ndoc(DTM)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)
```


```{r}
so <- sort(countsOfPrimaryTopics, decreasing = TRUE)
paste(so, ":", names(so))
```


```{r}
topicToFilter <- 4  # you can set this manually ...
# ... or have it selected by a term in the topic name (e.g. 'children')
topicToFilter <- grep('refugee', topicNames)[1] 
topicThreshold <- 0.2
selectedDocumentIndexes <- which(theta[, topicToFilter] >= topicThreshold)
filteredCorpus <- corpus[selectedDocumentIndexes]
# show length of filtered corpus
filteredCorpus
```
