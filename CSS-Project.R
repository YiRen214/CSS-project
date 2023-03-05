library(tidyverse)
library(tidytext)
library(textdata)
library(stopwords)
library(wordcloud)
library(igraph)
library(ggraph)
library(quanteda)
library(quanteda.textplots)
library(rtweet)
library(ggmap)
library(stringr)
library(vader)
library(partykit)
library(strucchange)
require(quanteda.textstats)
require(quanteda.textplots)
require(quanteda.corpora)
library(RColorBrewer)
library(base)
library(topicmodels)
library(syuzhet)
my_auth <- rtweet::create_token()

biontech_tweets<-search_tweets("biontech",lang="en", n=5000,
                                  include_rts = T)

biontech_tweets$date<-gsub("[0-9]{2}:[0-9]{2}:[0-9]{2}","",biontech_tweets$created_at)
uniq_biontech_tweets<-biontech_tweets[!duplicated(biontech_tweets$text),]
st.value<-get_nrc_sentiment(uniq_biontech_tweets$text)

st.value$positive.self<-st.value$joy+st.value$trust+st.value$positive
st.value$negative.self<-st.value$anger+st.value$fear+st.value$negative
uniq_biontech_vader<-add_column(uniq_biontech_tweets,st.value)
uniq_biontech_pos<-uniq_biontech_vader[uniq_biontech_vader$positive.self>uniq_biontech_vader$negative.self,]

st.value.all<-get_nrc_sentiment(biontech_tweets$text)

st.value.all$positive.self<-st.value.all$joy+st.value.all$trust+st.value.all$positive
st.value.all$negative.self<-st.value.all$anger+st.value.all$fear+st.value.all$negative
biontech_vader<-add_column(biontech_tweets,st.value.all)
biontech_pos<-biontech_vader[biontech_vader$positive.self>biontech_vader$negative.self,]

bion_pos_score<-colSums(uniq_biontech_pos[,c(45:52)])
bion_pos_score_df<-data.frame(bion_pos_score)
bion_pos_score_df
bion_pos_st.score<-cbind(sentiment=row.names(bion_pos_score_df),bion_pos_score_df,row.names=NULL)

ggplot(data = bion_pos_st.score,aes(x=sentiment,y=bion_pos_score,fill=sentiment))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
uniq_biontech_pos$text <- gsub("http.*", "", uniq_biontech_pos$text)
uniq_biontech_pos$text <- gsub("https.*", "",uniq_biontech_pos$text)
uniq_biontech_pos$text <- gsub("&amp;", "&", uniq_biontech_pos$text)
uniq_biontech_pos$text <- gsub("@.*","",uniq_biontech_pos$text)
tidy_uniq_biontech_pos<-uniq_biontech_pos%>%select(text)%>%unnest_tokens(word,text)
stop_english <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)

tidy_uniq_biontech_pos <- tidy_uniq_biontech_pos %>% 
  anti_join(stop_english)
count_uniq_tidy_biontech_pos<- tidy_uniq_biontech_pos %>% 
  count(word, sort = TRUE)
count_uniq_tidy_biontech_pos

selfdef<-data.frame(word=c("biontech","pfizer","moderna","rt","vaccine","covid","19","product","done",
                                                     "relevant","vaccines","people","florida","court","make
                                                     ","asking","v","first","grand","jury","100","12","15",
                                                     "16","91","1","2","may","just","like","never","get","buy"
                           ,"biontech's","billion","high","know","year","2022","covid19","sonia_elijah","drsmeena","can","also"))
tidy_uniq_biontech_pos<-tidy_uniq_biontech_pos%>%anti_join(selfdef)
pal <- brewer.pal(8, "Dark2")

tidy_uniq_biontech_pos %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20, colors = pal))

corpus_uniq_biontech_pos <- corpus(uniq_biontech_pos, 
                      docid_field = "id",
                      text_field = "text") 

corpus_uniq_biontech_pos_clean <- tokens(corpus_uniq_biontech_pos, 
                            remove_punct = TRUE, 
                            remove_numbers = TRUE, 
                            remove_symbols = TRUE) %>%tokens_remove(c("biontech","pfizer","moderna","rt","vaccine","covid","19","product","done",
                                                                      "relevant","vaccines","people","florida","court","make
                                                     ","asking","v","first","grand","jury","100","12","15","covid-19",
                                                                      "16","91","1","2","may","just","like","never","get","buy"
                                                                      ,"biontech's","billion","high","know","year","2022","covid19",
                                                                      "#covid19","#pfizer","#biontech","germani","dose","can","also",padding=TRUE))%>%
tokens_tolower() 

corpus_uniq_biontech_pos_cleaner <- corpus_uniq_biontech_pos_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1) 


DTM_uniq_biontech_pos <- dfm(corpus_uniq_biontech_pos_cleaner)

corpus_biontech_pos <- corpus(biontech_pos, 
                                   docid_field = "id",
                                   text_field = "text") 

corpus_biontech_pos_clean <- tokens(corpus_biontech_pos, 
                                         remove_punct = TRUE, 
                                         remove_numbers = TRUE, 
                                         remove_symbols = TRUE) %>%tokens_remove(c("biontech","pfizer","moderna","rt","vaccine","covid","19","product","done",
                                                                                   "relevant","vaccines","people","florida","court","make",
                                                     "asking","v","first","grand","jury","100","12","15","covid-19",
                                                                                   "16","91","1","2","may","just","like","never","get","buy"
                                                                                   ,"biontech's","billion","high","know","year","2022","covid19",
                                                                                   "#covid19","#pfizer","#biontech","germani","dose","can","also",padding=TRUE))%>%
  tokens_tolower() 

corpus_biontech_pos_cleaner <- corpus_biontech_pos_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1) 


DTM_biontech_pos <- dfm(corpus_biontech_pos_cleaner)


user_DTM_biontech_pos <- dfm_select(DTM_biontech_pos, pattern = "@*")
user_FCM_biontech_pos <- fcm(user_DTM_biontech_pos, context = "document")

topuser_biontech_pos <- names(topfeatures(user_DTM_biontech_pos, 50))
user_FCM_biontech_pos <- fcm_select(user_FCM_biontech_pos, pattern = topuser_biontech_pos)

textplot_network(user_FCM_biontech_pos, min_freq = 1)


###########################
uniq_biontech_neg<-uniq_biontech_vader[uniq_biontech_vader$positive.self<uniq_biontech_vader$negative.self,]
biontech_neg<-biontech_vader[biontech_vader$positive.self<biontech_vader$negative.self,]
bion_neg_score<-colSums(uniq_biontech_neg[,c(45:52)])
bion_neg_score_df<-data.frame(bion_neg_score)
bion_neg_score_df
bion_neg_st.score<-cbind(sentiment=row.names(bion_neg_score_df),bion_neg_score_df,row.names=NULL)

ggplot(data = bion_neg_st.score,aes(x=sentiment,y=bion_neg_score,fill=sentiment))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
uniq_biontech_neg$text <- gsub("http.*", "", uniq_biontech_neg$text)
uniq_biontech_neg$text <- gsub("https.*", "",uniq_biontech_neg$text)
uniq_biontech_neg$text <- gsub("&amp;", "&", uniq_biontech_neg$text)
uniq_biontech_neg$text <- gsub("@.*","",uniq_biontech_neg$text)
tidy_uniq_biontech_neg<-uniq_biontech_neg%>%select(text)%>%unnest_tokens(word,text)
stop_english <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)

tidy_uniq_biontech_neg <- tidy_uniq_biontech_neg %>% 
  anti_join(stop_english)
count_uniq_tidy_biontech_neg<- tidy_uniq_biontech_neg %>% 
  count(word, sort = TRUE)
count_uniq_tidy_biontech_neg
selfdef.2<-data.frame(word=c("biontech","pfizer","moderna","rt","vaccine","covid","19","product","done",
                           "relevant","vaccines","people","years","now","make",
                           "asking","v","first","biggest","read","100",
                           "1","2","may","just","like","never","get","buy"
                           ,"biontech's","billion","high","know","year","2022","covid19",
                           "use","first","shot","following","covid_19","one","gavi","id2020"
                           ,"world's","coronavirüs","dsö","korona","koronavirues","koronawirus",
                           "yenidünyadüzeni","edv1694","way","link","3","list","can","days"))
tidy_uniq_biontech_neg<-tidy_uniq_biontech_neg%>%anti_join(selfdef.2)
pal <- brewer.pal(8, "Dark2")

tidy_uniq_biontech_neg %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20, colors = pal))
corpus_uniq_biontech_neg<- corpus(uniq_biontech_neg, 
                      docid_field = "id",
                      text_field = "text") 

corpus_uniq_biontech_neg_clean <- tokens(corpus_uniq_biontech_neg, 
                            remove_punct = TRUE, 
                            remove_numbers = TRUE, 
                            remove_symbols = TRUE) %>%tokens_remove(c("biontech","pfizer","moderna","covid-19","rt","vaccine","covid","19","product","done",
                                                                      "relevant","vaccines","people","years","now","make",
                                                                      "asking","v","first","biggest","read","100",
                                                                      "1","2","may","just","like","never","get","buy"
                                                                      ,"biontech's","billion","high","know","year","2022","covid19",
                                                                      "use","first","shot","following","covid_19","one","gavi","id2020",
                                                                      "world's","coronavirüs","dsö","korona","koronavirues","koronawirus",
                                                                      "yenidünyadüzeni","edv1694","way","link","3","list","can","days","#biontech","#pfizer",
                                                                      "#covid19","#covid","#covid_19","#gate","#rockefel","#pandem",padding=TRUE))%>%
  tokens_tolower() 
corpus_uniq_biontech_neg_cleaner <- corpus_uniq_biontech_neg_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_uniq_biontech_neg <- dfm(corpus_uniq_biontech_neg_cleaner)

corpus_biontech_neg<- corpus(biontech_neg, 
                                  docid_field = "id",
                                  text_field = "text") 

corpus_biontech_neg_clean <- tokens(corpus_biontech_neg, 
                                         remove_punct = TRUE, 
                                         remove_numbers = TRUE, 
                                         remove_symbols = TRUE) %>%tokens_remove(c("biontech","pfizer","moderna","covid-19","rt","vaccine","covid","19","product","done",
                                                                                   "relevant","vaccines","people","years","now","make",
                                                                                   "asking","v","first","biggest","read","100",
                                                                                   "1","2","may","just","like","never","get","buy"
                                                                                   ,"biontech's","billion","high","know","year","2022","covid19",
                                                                                   "use","first","shot","following","covid_19","one","gavi","id2020",
                                                                                   "world's","coronavirüs","dsö","korona","koronavirues","koronawirus",
                                                                                   "yenidünyadüzeni","edv1694","way","link","3","list","can","days","#biontech","#pfizer",
                                                                                   "#covid19","#covid","#covid_19","#gate","#rockefel","#pandem",padding=TRUE))%>%
  tokens_tolower() 
corpus_biontech_neg_cleaner <- corpus_biontech_neg_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_biontech_neg <- dfm(corpus_biontech_neg_cleaner)

user_DTM_biontech_neg <- dfm_select(DTM_biontech_neg, pattern = "@*")
user_FCM_biontech_neg <- fcm(user_DTM_biontech_neg, context = "document")

topuser_biontech_neg <- names(topfeatures(user_DTM_biontech_neg, 50))
user_FCM_biontech_neg <- fcm_select(user_FCM_biontech_neg, pattern = topuser_biontech_neg)

textplot_network(user_FCM_biontech_neg, min_freq = 1)


############################################################
uniq_biontech_neu<-uniq_biontech_vader[uniq_biontech_vader$positive.self==uniq_biontech_vader$negative.self,]
biontech_neu<-biontech_vader[biontech_vader$positive.self==biontech_vader$negative.self,]
bion_neu_score<-colSums(uniq_biontech_neu[,c(45:52)])
bion_neu_score_df<-data.frame(bion_neu_score)
bion_neu_score_df
bion_neu_st.score<-cbind(sentiment=row.names(bion_neu_score_df),bion_neu_score_df,row.names=NULL)

ggplot(data = bion_neu_st.score,aes(x=sentiment,y=bion_neu_score,fill=sentiment))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
uniq_biontech_neu$text <- gsub("http.*", "", uniq_biontech_neu$text)
uniq_biontech_neu$text <- gsub("https.*", "",uniq_biontech_neu$text)
uniq_biontech_neu$text <- gsub("&amp;", "&", uniq_biontech_neu$text)
uniq_biontech_neu$text <- gsub("@.*","", uniq_biontech_neu$text)
tidy_uniq_biontech_neu<-uniq_biontech_neu%>%select(text)%>%unnest_tokens(word,text)
stop_english <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)

tidy_uniq_biontech_neu <- tidy_uniq_biontech_neu %>% 
  anti_join(stop_english)
count_uniq_tidy_biontech_neu<- tidy_uniq_biontech_neu %>% 
  count(word, sort = TRUE)
count_uniq_tidy_biontech_neu
selfdef.3<-data.frame(word=c("biontech","pfizer","moderna","rt","vaccine","covid","19","product","done",
                             "relevant","vaccines","people","years","now","make","two","l","also",
                             "asking","v","first","biggest","read","100","biontech_group","cut",
                             "1","2","may","just","like","never","get","buy","sent","said","got","12",                             "biontech's","billion","high","know","year","2022","covid19",
                             "use","first","shot","following","covid_19","one","gavi","id2020",
                             "world's","coronavirüs","dsö","korona","koronavirues","koronawirus",
                             "yenidünyadüzeni","edv1694","way","link","3","list","can","days","2022","co"))
tidy_uniq_biontech_neu<-tidy_uniq_biontech_neu%>%anti_join(selfdef.3)
pal <- brewer.pal(8, "Dark2")

tidy_uniq_biontech_neu %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20, colors = pal))
corpus_uniq_biontech_neu<- corpus(uniq_biontech_neu, 
                                  docid_field = "id",
                                  text_field = "text") 

corpus_uniq_biontech_neu_clean <- tokens(corpus_uniq_biontech_neu, 
                                         remove_punct = TRUE, 
                                         remove_numbers = TRUE, 
                                         remove_symbols = TRUE) %>%tokens_remove(c("biontech","pfizer","moderna","rt","vaccine","covid","19","product","done",
                                                                                   "relevant","vaccines","people","years","now","make","two","l","also",
                                                                                   "asking","v","first","biggest","read","100","biontech_group","cut",
                                                                                   "1","2","may","just","like","never","get","buy","sent","said","got","12",                             "biontech's","billion","high","know","year","2022","covid19",
                                                                                   "use","first","shot","following","covid_19","one","gavi","id2020",
                                                                                   "world's","coronavirüs","dsö","korona","koronavirues","koronawirus",
                                                                                   "yenidünyadüzeni","edv1694","way","link","3","list","can","days","2022","co",padding=TRUE))%>%
  tokens_tolower() 
corpus_uniq_biontech_neu_cleaner <- corpus_uniq_biontech_neu_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_uniq_biontech_neu <- dfm(corpus_uniq_biontech_neu_cleaner)

corpus_biontech_neu<- corpus(biontech_neu, 
                             docid_field = "id",
                             text_field = "text") 

corpus_biontech_neu_clean <- tokens(corpus_biontech_neu, 
                                    remove_punct = TRUE, 
                                    remove_numbers = TRUE, 
                                    remove_symbols = TRUE) %>%tokens_remove(c("biontech","pfizer","moderna","rt","vaccine","covid","19","product","done",
                                                                              "relevant","vaccines","people","years","now","make","two","l","also",
                                                                              "asking","v","first","biggest","read","100","biontech_group","cut",
                                                                              "1","2","may","just","like","never","get","buy","sent","said","got","12",                             "biontech's","billion","high","know","year","2022","covid19",
                                                                              "use","first","shot","following","covid_19","one","gavi","id2020",
                                                                              "world's","coronavirüs","dsö","korona","koronavirues","koronawirus",
                                                                              "yenidünyadüzeni","edv1694","way","link","3","list","can","days","2022","co",padding=TRUE))%>%
  tokens_tolower() 
corpus_biontech_neu_cleaner <- corpus_biontech_neu_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_biontech_neu <- dfm(corpus_biontech_neu_cleaner)

user_DTM_biontech_neu <- dfm_select(DTM_biontech_neu, pattern = "@*")
user_FCM_biontech_neu <- fcm(user_DTM_biontech_neu, context = "document")

topuser_biontech_neu <- names(topfeatures(user_DTM_biontech_neu, 50))
user_FCM_biontech_neu <- fcm_select(user_FCM_biontech_neu, pattern = topuser_biontech_neu)

textplot_network(user_FCM_biontech_neu, min_freq = 1)


############################################
AZ_tweets<-search_tweets("astrazeneca",lang="en", n=5000,
                              include_rts = T)

AZ_tweets$date<-gsub("[0-9]{2}:[0-9]{2}:[0-9]{2}","",AZ_tweets$created_at)
uniq_AZ_tweets<-AZ_tweets[!duplicated(AZ_tweets$text),]
st.value.AZ<-get_nrc_sentiment(uniq_AZ_tweets$text)

st.value.AZ$positive.self<-st.value.AZ$joy+st.value.AZ$trust+st.value.AZ$positive
st.value.AZ$negative.self<-st.value.AZ$anger+st.value.AZ$fear+st.value.AZ$negative
uniq_AZ_vader<-add_column(uniq_AZ_tweets,st.value.AZ)
uniq_AZ_pos<-uniq_AZ_vader[uniq_AZ_vader$positive.self>uniq_AZ_vader$negative.self,]

st.value.all.AZ<-get_nrc_sentiment(AZ_tweets$text)

st.value.all.AZ$positive.self<-st.value.all.AZ$joy+st.value.all.AZ$trust+st.value.all.AZ$positive
st.value.all.AZ$negative.self<-st.value.all.AZ$anger+st.value.all.AZ$fear+st.value.all.AZ$negative
AZ_vader<-add_column(AZ_tweets,st.value.all.AZ)

AZ_pos<-AZ_vader[AZ_vader$positive.self>AZ_vader$negative.self,]

AZ_pos_score<-colSums(uniq_AZ_pos[,c(45:52)])
AZ_pos_score_df<-data.frame(AZ_pos_score)
AZ_pos_score_df
AZ_pos_st.score<-cbind(sentiment=row.names(AZ_pos_score_df),AZ_pos_score_df,row.names=NULL)

ggplot(data = AZ_pos_st.score,aes(x=sentiment,y=AZ_pos_score,fill=sentiment))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
uniq_AZ_pos$text <- gsub("http.*", "", uniq_AZ_pos$text)
uniq_AZ_pos$text <- gsub("https.*", "",uniq_AZ_pos$text)
uniq_AZ_pos$text <- gsub("&amp;", "&", uniq_AZ_pos$text)
uniq_AZ_pos$text <- gsub("@.*","",uniq_AZ_pos$text)
tidy_uniq_AZ_pos<-uniq_AZ_pos%>%select(text)%>%unnest_tokens(word,text)
stop_english <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)

tidy_uniq_AZ_pos <- tidy_uniq_AZ_pos %>% 
  anti_join(stop_english)
count_uniq_tidy_AZ_pos<- tidy_uniq_AZ_pos %>% 
  count(word, sort = TRUE)
count_uniq_tidy_AZ_pos
selfdef.4<-data.frame(word=c("astrazeneca","rt","boss","us","get","covid","man","going","hicksyalex","one","barbarasghost1",
                             "like","please","also","based","garetheve","lisa","rees","sh","shaw's","jones","2022","can",
                             "sole","day","di's","jab","amne","pi","used","go","just","131","170","2nd","ama","bedelabee",
                             "j","knew","people","pistolannie31","michaelpbreton","vaccine","ceo","says","one","19","get","now",
                             "big","gets","vaccines","astrazeneca's","first","2","2021","make","don't","nakedemperoruk","years","sir",
                             "see","2020"
                           ))
tidy_uniq_AZ_pos<-tidy_uniq_AZ_pos%>%anti_join(selfdef.4)
pal <- brewer.pal(8, "Dark2")

tidy_uniq_AZ_pos %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20, colors = pal))
corpus_uniq_AZ_pos<- corpus(uniq_AZ_pos, 
                                  docid_field = "id",
                                  text_field = "text") 

corpus_uniq_AZ_pos_clean <- tokens(corpus_uniq_AZ_pos, 
                                         remove_punct = TRUE, 
                                         remove_numbers = TRUE, 
                                         remove_symbols = TRUE) %>%tokens_remove(c("astrazeneca","rt","boss","us","get","covid","man","going","hicksyalex","one","barbarasghost1",
                                                                                   "like","please","also","based","garetheve","lisa","rees","sh","shaw's","jones","2022","can",
                                                                                   "sole","day","di's","jab","amne","pi","used","go","just","131","170","2nd","ama","bedelabee",
                                                                                   "j","knew","people","pistolannie31","michaelpbreton","vaccine","ceo","says","one","19","get","now",
                                                                                   "big","gets","vaccines","astrazeneca's","first","2","2021","make","don't","nakedemperoruk","years","sir",
                                                                                   "see","2020",padding=TRUE))%>%
  tokens_tolower() 
corpus_uniq_AZ_pos_cleaner <- corpus_uniq_AZ_pos_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_uniq_AZ_pos <- dfm(corpus_uniq_AZ_pos_cleaner)

corpus_AZ_pos<- corpus(AZ_pos, 
                             docid_field = "id",
                             text_field = "text") 

corpus_AZ_pos_clean <- tokens(corpus_AZ_pos, 
                                    remove_punct = TRUE, 
                                    remove_numbers = TRUE, 
                                    remove_symbols = TRUE) %>%tokens_remove(c("astrazeneca","rt","boss","us","get","covid","man","going","hicksyalex","one","barbarasghost1",
                                                                              "like","please","also","based","garetheve","lisa","rees","sh","shaw's","jones","2022","can",
                                                                              "sole","day","di's","jab","amne","pi","used","go","just","131","170","2nd","ama","bedelabee",
                                                                              "j","knew","people","pistolannie31","michaelpbreton","vaccine","ceo","says","one","19","get","now",
                                                                              "big","gets","vaccines","astrazeneca's","first","2","2021","make","don't","nakedemperoruk","years","sir",
                                                                              "see","2020",padding=TRUE))%>%
  tokens_tolower() 
corpus_AZ_pos_cleaner <- corpus_AZ_pos_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_AZ_pos <- dfm(corpus_AZ_pos_cleaner)

user_DTM_AZ_pos <- dfm_select(DTM_AZ_pos, pattern = "@*")
user_FCM_AZ_pos <- fcm(user_DTM_AZ_pos, context = "document")

topuser_AZ_pos <- names(topfeatures(user_DTM_AZ_pos, 50))
user_FCM_AZ_pos <- fcm_select(user_FCM_AZ_pos, pattern = topuser_AZ_pos)

textplot_network(user_FCM_AZ_pos, min_freq = 1)


##############################################
uniq_AZ_neg<-uniq_AZ_vader[uniq_AZ_vader$positive.self<uniq_AZ_vader$negative.self,]
AZ_neg<-AZ_vader[AZ_vader$positive.self<AZ_vader$negative.self,]
AZ_neg_score<-colSums(uniq_AZ_neg[,c(45:52)])
AZ_neg_score_df<-data.frame(AZ_neg_score)
AZ_neg_score_df
AZ_neg_st.score<-cbind(sentiment=row.names(AZ_neg_score_df),AZ_neg_score_df,row.names=NULL)

ggplot(data = AZ_neg_st.score,aes(x=sentiment,y=AZ_neg_score,fill=sentiment))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
uniq_AZ_neg$text <- gsub("http.*", "", uniq_AZ_neg$text)
uniq_AZ_neg$text <- gsub("https.*", "",uniq_AZ_neg$text)
uniq_AZ_neg$text <- gsub("&amp;", "&", uniq_AZ_neg$text)
uniq_AZ_neg$text <- gsub("@.*","",uniq_AZ_neg$text)
tidy_uniq_AZ_neg<-uniq_AZ_neg%>%select(text)%>%unnest_tokens(word,text)
stop_english <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)

tidy_uniq_AZ_neg <- tidy_uniq_AZ_neg %>% 
  anti_join(stop_english)
count_uniq_tidy_AZ_neg<- tidy_uniq_AZ_neg %>% 
  count(word, sort = TRUE)
count_uniq_tidy_AZ_neg
selfdef.5<-data.frame(word=c("astrazeneca","rt","go","must","co","react19org","tells","ceo","days","3","matter","jab","j","due",
                             "people","fingers","fr","vaccine","o","shoes","socks","drelidavid","covid","pfizer","like","mhragovuk",
                             "pistolannie31","now","just","comes","get","along","next","astrazeneca's","hicksyalex",
                             "az","also","barbarasghost1","isn't","says","even","first","vaccines","lisa","one","ama","astrazeneca's",
                             "know","said","many","2","got"
))
tidy_uniq_AZ_neg<-tidy_uniq_AZ_neg%>%anti_join(selfdef.5)
pal <- brewer.pal(8, "Dark2")

tidy_uniq_AZ_neg %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20, colors = pal))
corpus_uniq_AZ_neg<- corpus(uniq_AZ_neg, 
                            docid_field = "id",
                            text_field = "text") 

corpus_uniq_AZ_neg_clean <- tokens(corpus_uniq_AZ_neg, 
                                   remove_punct = TRUE, 
                                   remove_numbers = TRUE, 
                                   remove_symbols = TRUE) %>%tokens_remove(c("astrazeneca","rt","go","must","co","react19org","tells","ceo","days","3","matter","jab","j","due",
                                                                             "people","fingers","fr","vaccine","o","shoes","socks","drelidavid","covid","pfizer","like","mhragovuk",
                                                                             "pistolannie31","now","just","comes","get","along","next","astrazeneca's","hicksyalex",
                                                                             "az","also","barbarasghost1","isn't","says","even","first","vaccines","lisa","one","ama","astrazeneca's",
                                                                             "know","said","many","2","got",padding=TRUE))%>%
  tokens_tolower() 
corpus_uniq_AZ_neg_cleaner <- corpus_uniq_AZ_neg_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_uniq_AZ_neg <- dfm(corpus_uniq_AZ_neg_cleaner)

corpus_AZ_neg <- corpus(AZ_neg, 
                        docid_field = "id",
                        text_field = "text") 

corpus_AZ_neg_clean <- tokens(corpus_AZ_neg, 
                              remove_punct = TRUE, 
                              remove_numbers = TRUE, 
                              remove_symbols = TRUE) %>%tokens_remove(c("astrazeneca","rt","go","must","co","react19org","tells","ceo","days","3","matter","jab","j","due",
                                                                        "people","fingers","fr","vaccine","o","shoes","socks","drelidavid","covid","pfizer","like","mhragovuk",
                                                                        "pistolannie31","now","just","comes","get","along","next","astrazeneca's","hicksyalex",
                                                                        "az","also","barbarasghost1","isn't","says","even","first","vaccines","lisa","one","ama","astrazeneca's",
                                                                        "know","said","many","2","got",padding=TRUE))%>%
  tokens_tolower() 
corpus_AZ_neg_cleaner <- corpus_AZ_neg_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1) 
DTM_AZ_neg <- dfm(corpus_AZ_neg_cleaner)
user_DTM_AZ_neg <- dfm_select(DTM_AZ_neg, pattern = "@*")
user_FCM_AZ_neg <- fcm(user_DTM_AZ_neg, context = "document")
topuser_AZ_neg <- names(topfeatures(user_DTM_AZ_neg, 50))
user_FCM_AZ_neg <- fcm_select(user_FCM_AZ_neg, pattern = topuser_AZ_neg)

textplot_network(user_FCM_AZ_neg, min_freq = 1)

##################################
uniq_AZ_neu<-uniq_AZ_vader[uniq_AZ_vader$positive.self==uniq_AZ_vader$negative.self,]
AZ_neu<-AZ_vader[AZ_vader$positive.self==AZ_vader$negative.self,]
AZ_neu_score<-colSums(uniq_AZ_neu[,c(45:52)])
AZ_neu_score_df<-data.frame(AZ_neu_score)
AZ_neu_score_df
AZ_neu_st.score<-cbind(sentiment=row.names(AZ_neu_score_df),AZ_neu_score_df,row.names=NULL)

ggplot(data = AZ_neu_st.score,aes(x=sentiment,y=AZ_neu_score,fill=sentiment))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
uniq_AZ_neu$text <- gsub("http.*", "", uniq_AZ_neu$text)
uniq_AZ_neu$text <- gsub("https.*", "",uniq_AZ_neu$text)
uniq_AZ_neu$text <- gsub("&amp;", "&", uniq_AZ_neu$text)
uniq_AZ_neu$text <- gsub("@.*","",uniq_AZ_neu$text)
tidy_uniq_AZ_neu<-uniq_AZ_neu%>%select(text)%>%unnest_tokens(word,text)
stop_english <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)

tidy_uniq_AZ_neu <- tidy_uniq_AZ_neu %>% 
  anti_join(stop_english)
count_uniq_tidy_AZ_neu<- tidy_uniq_AZ_neu %>% 
  count(word, sort = TRUE)
count_uniq_tidy_AZ_neu
selfdef.6<-data.frame(word=c("astrazeneca","rt","go","must","co","react19org","tells","ceo","days","3","matter","jab","j","due",
                             "people","fingers","fr","vaccine","o","shoes","socks","drelidavid","covid","pfizer","like","mhragovuk",
                             "pistolannie31","now","just","comes","get","along","next","astrazeneca's","hicksyalex",
                             "az","also","barbarasghost1","isn't","says","used","first","vaccines","lisa","one","ama","astrazeneca's",
                             "know","said","many","2","got","last","herpaderpa5","kushpuss4ever","2021","even","gets","got","jack",
                             "johan15109896","guy","man","2022","ago","hey"
))
tidy_uniq_AZ_neu<-tidy_uniq_AZ_neu%>%anti_join(selfdef.6)
pal <- brewer.pal(8, "Dark2")

tidy_uniq_AZ_neu %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20, colors = pal))
corpus_uniq_AZ_neu<- corpus(uniq_AZ_neu, 
                            docid_field = "id",
                            text_field = "text") 

corpus_uniq_AZ_neu_clean <- tokens(corpus_uniq_AZ_neu, 
                                   remove_punct = TRUE, 
                                   remove_numbers = TRUE, 
                                   remove_symbols = TRUE) %>%tokens_remove(c("astrazeneca","rt","go","must","co","react19org","tells","ceo","days","3","matter","jab","j","due",
                                                                             "people","fingers","fr","vaccine","o","shoes","socks","drelidavid","covid","pfizer","like","mhragovuk",
                                                                             "pistolannie31","now","just","comes","get","along","next","astrazeneca's","hicksyalex",
                                                                             "az","also","barbarasghost1","isn't","says","even","first","vaccines","lisa","one","ama","astrazeneca's",
                                                                             "know","said","many","2","got",padding=TRUE))%>%
  tokens_tolower() 
corpus_uniq_AZ_neu_cleaner <- corpus_uniq_AZ_neu_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_uniq_AZ_neu <- dfm(corpus_uniq_AZ_neu_cleaner)
corpus_AZ_neu <- corpus(AZ_neu, 
                        docid_field = "id",
                        text_field = "text") 

corpus_AZ_neu_clean <- tokens(corpus_AZ_neu, 
                              remove_punct = TRUE, 
                              remove_numbers = TRUE, 
                              remove_symbols = TRUE) %>%tokens_remove(c("astrazeneca","rt","go","must","co","react19org","tells","ceo","days","3","matter","jab","j","due",
                                                                        "people","fingers","fr","vaccine","o","shoes","socks","drelidavid","covid","pfizer","like","mhragovuk",
                                                                        "pistolannie31","now","just","comes","get","along","next","astrazeneca's","hicksyalex",
                                                                        "az","also","barbarasghost1","isn't","says","even","first","vaccines","lisa","one","ama","astrazeneca's",
                                                                        "know","said","many","2","got",padding=TRUE))%>%
  tokens_tolower() 
corpus_AZ_neu_cleaner <- corpus_AZ_neu_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1) 
DTM_AZ_neu <- dfm(corpus_AZ_neu_cleaner)
user_DTM_AZ_neu <- dfm_select(DTM_AZ_neu, pattern = "@*")
user_FCM_AZ_neu <- fcm(user_DTM_AZ_neu, context = "document")
topuser_AZ_neu <- names(topfeatures(user_DTM_AZ_neu, 50))
user_FCM_AZ_neu <- fcm_select(user_FCM_AZ_neu, pattern = topuser_AZ_neu)

textplot_network(user_FCM_AZ_neu, min_freq = 1)

############################################### time series 
library(text2vec)
library(stm)
library(widyr)
library(irlba)
library(Matrix)
library(wordcloud2)
library(pals)
library(reshape2)
topfeatures(DTM_biontech_pos)
bion_pos_names<-c("safe","effectiv")
bion_pos_freq <- dfm_select(DTM_biontech_pos,pattern = bion_pos_names,selection="keep",valuetype  = "fixed")
colnames(bion_pos_freq) <- bion_pos_names
bion_pos_freq <- data.frame(date= biontech_pos$date ,
                       as.data.frame(as.matrix(bion_pos_freq))
)

bion_pos_freq$id <- 1:nrow(bion_pos_freq)

df_bion_pos_name <- melt(bion_pos_freq, id.vars = c("id", "date"), variable.name = "term", value.name = "count")
df_bion_pos_name<-df_bion_pos_name%>%group_by(date,term)%>%summarise(sum_count=sum(count))%>%as.data.frame()
ggplot(df_bion_pos_name, aes(x = date, y = sum_count,group=term)) + geom_point()+geom_line(aes(color=term))


#########################
topfeatures(DTM_biontech_neg)
bion_neg_names<-c("cancer","mrna")
bion_neg_freq <- dfm_select(DTM_biontech_neg,pattern = bion_neg_names,selection="keep",valuetype  = "fixed")
colnames(bion_neg_freq) <- bion_neg_names
bion_neg_freq <- data.frame(date= biontech_neg$date ,
                            as.data.frame(as.matrix(bion_neg_freq))
)

bion_neg_freq$id <- 1:nrow(bion_neg_freq)

df_bion_neg_name <- melt(bion_neg_freq, id.vars = c("id", "date"), variable.name = "term", value.name = "count")
df_bion_neg_name<-df_bion_neg_name%>%group_by(date,term)%>%summarise(sum_count=sum(count))%>%as.data.frame()
ggplot(df_bion_neg_name, aes(x = date, y = sum_count, group = term)) + geom_line(aes(color=term)) + geom_point()
############################
topfeatures(DTM_biontech_neu)
bion_neu_names<-c("news","scientist")
bion_neu_freq <- dfm_select(DTM_biontech_neu,pattern = bion_neu_names,selection="keep",valuetype  = "fixed")
colnames(bion_neu_freq) <- bion_neu_names
bion_neu_freq <- data.frame(date= biontech_neu$date ,
                            as.data.frame(as.matrix(bion_neu_freq))
)

bion_neu_freq$id <- 1:nrow(bion_neu_freq)

df_bion_neu_name <- melt(bion_neu_freq, id.vars = c("id", "date"), variable.name = "term", value.name = "count")
df_bion_neu_name<-df_bion_neu_name%>%group_by(date,term)%>%summarise(sum_count=sum(count))%>%as.data.frame()
ggplot(df_bion_neu_name, aes(x = date, y =sum_count,group=term)) + geom_line(aes(color=term)) + geom_point()
############################
topfeatures(DTM_AZ_pos)
AZ_pos_names<-c("cincor","effect")
AZ_pos_freq <- dfm_select(DTM_AZ_pos,pattern = AZ_pos_names,selection="keep",valuetype  = "fixed")
colnames(AZ_pos_freq) <- AZ_pos_names
AZ_pos_freq <- data.frame(date= AZ_pos$date ,
                            as.data.frame(as.matrix(AZ_pos_freq))
)

AZ_pos_freq$id <- 1:nrow(AZ_pos_freq)

df_AZ_pos_name <- melt(AZ_pos_freq, id.vars = c("id", "date"), variable.name = "term", value.name = "count")
df_AZ_pos_name<-df_AZ_pos_name%>%group_by(date,term)%>%summarise(sum_count=sum(count))%>%as.data.frame()
ggplot(df_AZ_pos_name, aes(x = date, y = sum_count, group = term)) + geom_line(aes(color=term)) + geom_point()
#############################
topfeatures(DTM_AZ_neg)
AZ_neg_names<-c("cancer","die")
AZ_neg_freq <- dfm_select(DTM_AZ_neg,pattern = AZ_neg_names,selection="keep",valuetype  = "fixed")
colnames(AZ_neg_freq) <- AZ_neg_names
AZ_neg_freq <- data.frame(date= AZ_neg$date ,
                          as.data.frame(as.matrix(AZ_neg_freq))
)

AZ_neg_freq$id <- 1:nrow(AZ_neg_freq)

df_AZ_neg_name <- melt(AZ_neg_freq, id.vars = c("id", "date"), variable.name = "term", value.name = "count")
df_AZ_neg_name<-df_AZ_neg_name%>%group_by(date,term)%>%summarise(sum_count=sum(count))%>%as.data.frame()
ggplot(df_AZ_neg_name, aes(x = date, y =sum_count, group = term)) + geom_line(aes(color=term)) + geom_point()
############################
topfeatures(DTM_AZ_neu)
AZ_neu_names<-c("amp","cincor")
AZ_neu_freq <- dfm_select(DTM_AZ_neu,pattern = AZ_neu_names,selection="keep",valuetype  = "fixed")
colnames(AZ_neu_freq) <- AZ_neu_names
AZ_neu_freq <- data.frame(date= AZ_neu$date ,
                          as.data.frame(as.matrix(AZ_neu_freq))
)

AZ_neu_freq$id <- 1:nrow(AZ_neu_freq)

df_AZ_neu_name <- melt(AZ_neu_freq, id.vars = c("id", "date"), variable.name = "term", value.name = "count")
df_AZ_neu_name<-df_AZ_neu_name%>%group_by(date,term)%>%summarise(sum_count=sum(count))%>%as.data.frame()
ggplot(df_AZ_neu_name, aes(x = date, y =sum_count, group = term)) + geom_line(aes(color=term)) + geom_point()
#########################topics for biontech
corpus_uniq_biontech_tweets<- corpus(uniq_biontech_tweets, 
                                  docid_field = "id",
                                  text_field = "text") 

corpus_uniq_biontech_tweets_clean <- tokens(corpus_uniq_biontech_tweets, 
                                         remove_punct = TRUE, 
                                         remove_numbers = TRUE, 
                                         remove_symbols = TRUE) %>%tokens_remove(c("biontech","pfizer","moderna","covid-19","rt","vaccine","covid","19","product","done",
                                                                                   "relevant","vaccines","people","years","now","make",
                                                                                   "asking","v","first","biggest","read","100",
                                                                                   "1","2","may","just","like","never","get","buy"
                                                                                   ,"biontech's","billion","high","know","year","2022","covid19",
                                                                                   "use","first","shot","following","covid_19","one","gavi","id2020",
                                                                                   "world's","coronavirüs","dsö","korona","koronavirues","koronawirus",
                                                                                   "yenidünyadüzeni","edv1694","way","link","3","list","can","days","#biontech","#pfizer",
                                                                                   "#covid19","#covid","#covid_19","#gate","#rockefel","#pandem",
                           
                                                                                   "two","l","also",
                                                                                   "asking","v","first","biggest","read","100","biontech_group","cut",
                                                                                   "1","2","may","just","like","never","get","buy","sent","said","got","12",                             "biontech's","billion","high","know","year","2022","covid19",
                                                                                   
                                                                                   "2022",padding=TRUE))%>%
  tokens_tolower() 
corpus_uniq_biontech_tweets_cleaner <- corpus_uniq_biontech_tweets_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_uniq_biontech_tweets <- dfm(corpus_uniq_biontech_tweets_cleaner)
rowtotals.1<-apply(DTM_uniq_biontech_tweets, 1, sum)
DTM_uniq_biontech_tweets<-DTM_uniq_biontech_tweets[rowtotals.1>0,]
lda<-LDA(DTM_uniq_biontech_tweets,k=6)
top10_terms_uniq_biontech_tweets<-terms(lda,7)
top10_terms_uniq_biontech_tweets
############################ topics for AZ
corpus_uniq_AZ_tweets<- corpus(uniq_AZ_tweets, 
                                     docid_field = "id",
                                     text_field = "text") 

corpus_uniq_AZ_tweets_clean <- tokens(corpus_uniq_AZ_tweets, 
                                            remove_punct = TRUE, 
                                            remove_numbers = TRUE, 
                                            remove_symbols = TRUE) %>%tokens_remove(c("astrazeneca","rt","boss","us","get","covid","man","going","hicksyalex","one","barbarasghost1",
                                                                                      "like","please","also","based","garetheve","lisa","rees","sh","shaw's","jones","2022","can",
                                                                                      "sole","day","di's","jab","amne","pi","used","go","just","131","170","2nd","ama","bedelabee",
                                                                                      "j","knew","people","pistolannie31","michaelpbreton","vaccine","ceo","says","19","get","now",
                                                                                      "big","gets","vaccines","astrazeneca's","first","2","2021","make","don't","nakedemperoruk","years","sir",
                                                                                      "see","2020",
                                                                                      "must","co","react19org","tells","days","3","matter","jab","due",
                                                                                      "fingers","fr","o","shoes","socks","drelidavid","covid","pfizer","like","mhragovuk",
                                                                                      "now","comes","along","next",
                                                                                      "az","isn't","even","lisa",
                                                                                      "know","said","many","2","got",
                                                                                      
                                                                                      "barbarasghost1",
                                                                                      padding=TRUE))%>%
  tokens_tolower() 
corpus_uniq_AZ_tweets_cleaner <- corpus_uniq_AZ_tweets_clean %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_wordstem(language = "en") %>% 
  tokens_ngrams(1)
DTM_uniq_AZ_tweets <- dfm(corpus_uniq_AZ_tweets_cleaner)
rowtotals.2<-apply(DTM_uniq_AZ_tweets, 1, sum)
DTM_uniq_AZ_tweets<-DTM_uniq_AZ_tweets[rowtotals.2>0,]
lda.2<-LDA(DTM_uniq_AZ_tweets,k=6)
top10_terms_uniq_AZ_tweets<-terms(lda.2,7)
top10_terms_uniq_AZ_tweets
############################### pie plot for sentiments from biontech and AZ
data_bion<-data.frame(sentiment=c("positive", "neutral","negative"),
                      value=c(nrow(uniq_biontech_pos),nrow(uniq_biontech_neu),
                              nrow(uniq_biontech_neg))) %>%arrange(desc(sentiment))
data_bion<-data_bion%>%
  mutate(prop = value / sum(data_bion$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(data_bion, aes(x="", y=prop, fill=sentiment)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)+theme_void()+labs(title="Biontech")+
  geom_text(aes(y = ypos, label = value), color = "white", size=6) 
  

data_AZ<-data.frame(sentiment=c("positive", "negative","neutral"),
                      value=c(nrow(uniq_AZ_pos),nrow(uniq_AZ_neg),
                              nrow(uniq_AZ_neu)))%>%arrange(desc(sentiment))
data_AZ<-data_AZ%>%
  mutate(prop = value / sum(data_bion$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
ggplot(data_AZ, aes(x="", y=prop, fill=sentiment)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)+theme_void()+labs(title="Astrazeneca")+
  geom_text(aes(y = ypos, label = value), color = "white", size=6) 
