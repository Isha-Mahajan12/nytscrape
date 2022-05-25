---
title: 'New York Times API: Data Gathering'
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

## Connect to the New York Times API

```{r}
# Save the API key in an object
library(tidyverse)
csv <- read_csv("nyt.api.csv")
api_key <-csv$key
```

## Install Relevant Packages 

```{r}
library(nytimes)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(ggthemes)
```

The New York Times API returns 10 results at one time. So one page consists of information on ten articles. We can ask the API for multiple pages depending on the requirements for our analysis. For this chunk, I am setting a search query of keywords refugee and Afghanistan and giving the begin date as one week before the US withdrawal. The end date is more or less up until today. 

```{r}
term <- "refugee+afghanistan"
begin_date <- "20210823"
end_date <- "20220419"

# set URL to extract data
baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",api_key, sep="")

# use jsonlite to overcome the paging issue. This enables you to get data for each page. 
initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 
```


```{r}
# Creating an empty list object to store all the pages
pages <- list()

# Running a for loop to extract data from all 39 pages
for(i in 1:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch 
  
  # tryCatch provides a mechanism for handling unusual conditions, including errors and warnings.
  tryCatch(myfunc(), error=function(e) Sys.sleep(7)) 
}
```

```{r}
# bind pages as tabular object
allNYTSearch <- rbind_pages(pages)
```

### Web Scraping 

```{r}
library(rvest)
library(magrittr)
library(xml2)
library(purrr)

articles <- NULL

url <- as.list(allNYTSearch$response.docs.web_url)
article <- url %>% map(read_html)
content <-
   #find html tag that consists of the body of the article
    article %>% map_chr(. %>% html_nodes(".StoryBodyCompanionColumn") %>% 
                          html_text() %>% 
                          paste(., collapse = ""))
article_table <- data.frame("Content" = content)
head(article_table)
```


## Exploratory Data Analysis

# In which sections were the scraped articles included

*note: clean up ggplot, theme, color, labs, scale_y*

```{r}
section <- allNYTSearch %>% 
  group_by(response.docs.type_of_material) %>%
  summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>% 
  arrange(desc(percent)) %>% 
  slice(1:5)

  ggplot(section,aes(y=percent, x=response.docs.type_of_material,         fill=response.docs.type_of_material)) +
  geom_bar(stat = "identity") + 
  coord_flip()+ 
  theme_fivethirtyeight()
```


# Which authors were responsible for the most amount of coverage

*note: clean up ggplot, theme, color, labs, scale_y, depending on poster theme*

```{r}
author <- data.frame(table(allNYTSearch$response.docs.byline.original))
author

author <- author%>% 
  arrange(desc(Freq)) %>% 
  slice(1:15) 

ggplot(author,aes(reorder(Var1, Freq),Freq, fill = Freq)) + 
geom_bar( stat="identity") +   
coord_flip()+
theme_fivethirtyeight()
```

## What dates included the most coverage of the afghanistan issue 
*note: clean up ggplot, theme, color, labs, scale_y depending on poster theme*

```{r}
publishing_date <- allNYTSearch %>%
  mutate(pubDay=gsub("T.*","",response.docs.pub_date)) %>%
  group_by(pubDay) %>%
  summarise(count=n()) 

  
  ggplot(publishing_date,aes(reorder(pubDay,count), count, fill = count)) + 
  geom_bar(width = 0.8, stat="identity") + 
  coord_flip()+
    theme_fivethirtyeight()
```

## Combining articles of all authors who have written more than 5 articles.
*note: clean up ggplot, theme, color, labs, scale_y depending on poster theme*

```{r}
#bind columns into a data frame
allNYTSearch <- as.data.frame(bind_cols(allNYTSearch,article_table))
head(allNYTSearch)
# writecsv for meta data
write_csv(meta, "meta.csv")
```


```{r}
nat <- allNYTSearch %>% 
  mutate(pubDay=gsub("T.*","",response.docs.pub_date)) %>% 
  select(pubDay, response.docs.byline.original, Content)
write.csv(nat,"Times_API_Data.csv")
```

