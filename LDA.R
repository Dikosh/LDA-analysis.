# Let's try to to some topic modeling
install.packages("topicmodels")
library(topicmodels)
data("AssociatedPress")
AssociatedPress

# set a seed so that the output of the model is predictable

ap_lda <- LDA(AssociatedPress,k=2,control = list(seed=1234))
ap_lda # k is number of topics

# Word topic probabilites
install.packages("tidytext")
library(tidytext)
topics <- tidy(ap_lda,matrix='beta')
topics # here we see the model turned into a one-topic-per-term-per-row format

# lets find the 10 terms that are most common within each topic.
library(ggplot2)
library(dplyr)

ap_top_terms <- topics %>% 
  group_by(topic) %>% 
  top_n(10,beta) %>% 
  ungroup() %>% 
  arrange(topic,-beta)

ap_top_terms %>% 
  mutate(term=reorder(term,beta)) %>% 
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic,scales='free')+
  coord_flip()

# The 
library(tidyr)
beta <- topics %>% 
  mutate(topic=paste0('topic',topic)) %>% 
  spread(topic,beta) %>% 
  filter(topic1 >.001 | topic2 >.001) %>% 
  mutate(log_ratio=log2(topic2/topic1))

beta  

# here we can see that first topic more about politics

# Dcoument topic modeling
 
ap_documents <- tidy(ap_lda,matrix="gamma")
ap_documents

# Each of these values is an estimated proportion
# of words from that document that are generated
# from that topic. For example, the model estimates
# that only about 24.8% of the words in document 1
# were generated from topic 1.

# We can see that many of these documents were
# drawn from a mix of the two topics, but that
# document 6 was drawn almost entirely from topic
# 2, having a  γγ  from topic 1 close to zero. To
# check this answer, we could tidy() the
# document-term matrix (see Chapter 5.1) and check
# what the most common words in that document were.


tidy(AssociatedPress) %>% 
  filter(document==6) %>% 
  arrange(desc(count))


# library heist
titles <- c("Twenty Thousand Laugues under the Sea","The War of the Worlds",
            "Pride and Prejudice","Great Expectation")

install.packages("gutenbergr")
library(gutenbergr)
library(devtools)
books <- gutenberg_works(title %in% titles) %>% 
  gutenberg_download(meta_fields='title')
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")
library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

word_counts

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
