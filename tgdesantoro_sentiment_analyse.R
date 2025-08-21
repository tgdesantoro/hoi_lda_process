####menentukan sentiment per topik####

####open-19-topic-after-modelling####
library(readr)
topic_model15a <- read.csv2("D:/011_git/002_text_mining/sentiment/009_top_terms_gibbs15.csv")
sentimentpost_id <- readLines("D:/011_git/002_text_mining/sentiment/fajri-sentiment-lexicon/2018-fajri-positive.tsv")
sentimentneg_id <- readLines("D:/011_git/002_text_mining/sentiment/fajri-sentiment-lexicon/2018-fajri-negative.tsv")

head(topic_model15a)
head(sentimentpost_id)
head(sentimentneg_id)

str(sentimentpost_id)
str(sentimentneg_id)

topic_model15a %>% 
  count(topic)  # Hitung jumlah kata per topik


install.packages("tidyr")
library(magrittr)
library(dplyr)
library(tidyr)
library(stringdist)  # Untuk menghitung kesamaan kata


#Ubah file positive ke bentuk data frame
positive_words <- data.frame(words = sentimentpost_id, stringsAsFactors = FALSE)
negative_words <- data.frame(words = sentimentneg_id, stringsAsFactors = FALSE)
head(positive_words)

# **1️⃣ Memisahkan kata dan skor dari format tab ('\t')**
# Pastikan 'words' dalam positive_words dan negative_words memiliki format yang benar
positive_words <- positive_words %>%
  separate(words, into = c("term", "score"), sep = "\t", convert = TRUE)

negative_words <- negative_words %>%
  separate(words, into = c("word", "score"), sep = "\t", convert = TRUE)

####identify word based approoached JW####

# **2️⃣ Identifikasi kata positif terlebih dahulu**
topic_model15a <- topic_model15a %>%
  rowwise() %>%
  mutate(
    match_word_pos = list(positive_words$word[which(stringdist(term, positive_words$word, method = "jw") <= 0.1)]),
    words = ifelse(length(match_word_pos) > 0, paste(match_word_pos, collapse = ","), NA),
    sentiment = ifelse(!is.na(words) & words != "", "positive", NA),
    score = ifelse(!is.na(words), sum(as.numeric(positive_words$score[positive_words$word %in% match_word_pos])), NA)
  ) %>%
  ungroup() %>%
  select(-match_word_pos)  # Hapus kolom bantuan

# **3️⃣ Identifikasi kata negatif jika belum diberi label 'positive'**
topic_model15a <- topic_model15a %>%
  rowwise() %>%
  mutate(
    match_word_neg = list(negative_words$word[which(stringdist(term, negative_words$word, method = "jw") <= 0.1)]),
    words = ifelse(is.na(sentiment) & length(match_word_neg) > 0, paste(match_word_neg, collapse = ","), words),
    sentiment = ifelse(is.na(sentiment) & !is.na(words) & words != "", "negative", sentiment),
    score = ifelse(sentiment == "negative", sum(as.numeric(negative_words$score[negative_words$word %in% match_word_neg])), score)
  ) %>%
  ungroup() %>%
  select(-match_word_neg)  # Hapus kolom bantuan

#mengecek distribusi sentiment per topik
table(topic_model19$sentiment)  # Melihat distribusi sentimen
table(topic_model19$topic, topic_model19$sentiment)  # Cek jumlah per topik
table(topic_model19$sentiment, useNA = "always")  # Apakah ada NA?

topic_model19 %>%
  summarise(total_words = n(),
            recognized_words = sum(!is.na(sentiment)),
            unrecognized_words = sum(is.na(sentiment)))


print(topic_model15a)
View(topic_model15a)

###identify based real word###
head()


#### Count sentiment per topic ####
head(positive_words)
View(positive_words)
head(negative_words)
head(topic_model15)

#change name of coloumn term into word on topic_model19
topic_model15 <- topic_model15 %>%
  rename(word = term)

# Beri label "positive" jika kata ditemukan di lexicon positif
topic_model15 <- topic_model15 %>%
  mutate(sentiment = ifelse(word %in% positive_words$word, "positive", NA))

# Beri label "negative" hanya jika sentiment masih NA dan kata ditemukan di lexicon negatif
topic_model15 <- topic_model15 %>%
  mutate(sentiment = ifelse(is.na(sentiment) & word %in% negative_words$word, "negative", sentiment))


#create coloumn score
topic_model15 <- topic_model15 %>%
  mutate(score = NA_real_)  # Pastikan bertipe numerik


# Lakukan join dengan positive_words (gunakan suffix untuk mencegah bentrok nama)
topic_model15 <- topic_model15 %>%
  left_join(positive_words, by = "word", suffix = c("", "_pos")) %>%
  mutate(
    # Jika sentiment sudah 'positive' dan kata ada dalam positive_words, isi score
    score = ifelse(sentiment == "positive" & !is.na(score_pos), as.numeric(score_pos), score)
  ) %>%
  select(-score_pos)  # Hapus kolom tambahan dari join agar lebih rapi

# Lakukan join dengan negative_words (gunakan suffix agar tidak terjadi konflik nama)
topic_model15 <- topic_model15 %>%
  left_join(negative_words, by = "word", suffix = c("", "_neg")) %>%
  mutate(
    score = ifelse(sentiment == "negative" & !is.na(score_neg), as.numeric(score_neg), score)
  ) %>%
  select(-score_neg)  # Hapus kolom tambahan dari join


View(topic_model15)


#Cek hasil akhir
head(topic_model15)


sentiment_per_topic <- topic_model15 %>% 
  group_by(topic, sentiment) %>% 
  summarise(count = n(), .groups = "drop")  # Hitung jumlah 'positive' dan 'negative'

head(sentiment_per_topic)
View(sentiment_per_topic)

write.csv2(sentiment_per_topic,"D:/011_git/002_text_mining/sentiment/sentiment_per_topic15.csv")
write.csv2(topic_model15,"D:/011_git/002_text_mining/sentiment/sentiment_analyse_topic15.csv")


         
         
         
         