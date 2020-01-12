setwd("C:/Users/Stef/Desktop/prelucrarea statistica")
getwd()

reviews <- read.csv(file='huawei_reviews.csv', stringsAsFactors = FALSE)
attach(reviews)

summary(reviews)

#Barplot pentru variabila rating
table(reviews$rating)
counts <- table(reviews$rating)
barplot(counts, main="Rating", names.arg=c("1 s","2 s", "3 s", "4 s", "5 s"), xlab = "Nr. stelu??e", ylab = "Nr. recenzii", col = "blue", border = "red")

install.packages("tidyr")
library(tidyr)
install.packages("DT")
library("DT")
install.packages("tidytext")
library(tidytext)
install.packages("dplyr")
library("dplyr")
install.packages("stringr")
library(stringr)
install.packages("sentimentr")
library(sentimentr)
install.packages("textdata")
library(textdata)
install.packages("SnowballC")
library(SnowballC)
install.packages("ggplot2")
library(ggplot2)
install.packages("RColorBrewer")
library("RColorBrewer")
install.packages("wordcloud")
library(wordcloud)
install.packages("backports")
library(backports)



words <- reviews %>%
  select(c("author", "title", "content","rating")) %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

datatable(head(words))

afinn <- get_sentiments("afinn") %>% mutate(word=wordStem(word))
reviews.afinn <- words %>%
  inner_join(afinn, by= "word")
head(reviews.afinn)


word_summary <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating=mean(rating), score=max(value), count_word=n()) %>%
  arrange(desc(count_word))



ggplot(filter(word_summary, count_word < 50000), aes(mean_rating, score)) + geom_text(aes(label = word, color = count_word, size=count_word), position= position_jitter()) + scale_color_gradient(low = "lightblue", high = "darkblue") + coord_cartesian(xlim=c(3.5,4.5)) + guides(size = FALSE, color=FALSE)

wordcloud(words = word_summary$word, freq = word_summary$count_word, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Set2"))

good <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(value), count_word = n()) %>%
  filter(mean_rating>mean(mean_rating)) %>%
  arrange(desc(mean_rating))

          
wordcloud(words = good$word, freq = good$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))
          
          
bad <- reviews.afinn %>%
    group_by(word) %>%
    summarise(mean_rating = mean(rating), score = max(value), count_word = n()) %>%
    filter(mean_rating<mean(mean_rating)) %>%
    arrange(mean_rating)
          
wordcloud(words = bad$word, freq = bad$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))
          
review_summary <- reviews.afinn %>%
    group_by(author) %>%
    summarise(mean_rating = mean(rating), sentiment = mean(value))
    datatable(head(review_summary))
          
          y_mid = 0
          x_mid = 3.5
          
          review_summary %>% 
            mutate(quadrant = case_when(mean_rating > x_mid & sentiment > y_mid   ~ "Positive Review/Postive Sentiment",
                                        mean_rating <= x_mid & sentiment > y_mid  ~ "Negative Review/Positive Sentiment",
                                        mean_rating <= x_mid & sentiment <= y_mid ~ "Negative Review/Negative Sentiment",
                                        TRUE                                      ~ "Positive Review/Negative Sentiment")) %>% 
            ggplot(aes(x = mean_rating, y = sentiment, color = quadrant)) + 
            geom_hline(yintercept=y_mid, color = "black", size=.5) + 
            geom_vline(xintercept=x_mid, color = "black", size=.5) +
            guides(color=FALSE) +
            scale_color_manual(values=c("lightgreen", "pink", "pink","lightgreen")) +
            ggtitle("Amazon Product Rating vs Sentiment Rating of Review") +
            ggplot2::annotate("text", x = 4.33, y=3.5,label="Positive Review/Postive Sentiment") +
            ggplot2::annotate("text", x = 2, y=3.5,label="Negative Review/Positive Sentiment") +
            ggplot2::annotate("text", x = 4.33, y=-2.5,label="Positive Review/Negative Sentiment") +
            ggplot2::annotate("text", x = 2, y=-2.5,label="Negative Review/Negative Sentiment") +
            geom_point()
          
          
          install.packages("backports")
          library(backports)
      