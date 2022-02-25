################################################################################
# load the necessary library
library(tidyverse)
library(tidytext)
library(Rcpp)
library(textdata)
library(rvest)
library(maps)
library(stringr)
library(SnowballC)
library(udpipe)
library(widyr)
library(lubridate)
library(gmodels)
theme_set(theme_bw(16))

mytheme <- theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14,face="bold")) +
  theme_grey(base_size = 10)

################################################################################
# Question 2
################################################################################

# Idea Flow-Chart
# Each issue consists of 10 paragraph with 4 type of different stories as 
# mentioned below. 
# 
# paragraph 1 - title 
# paragraph 2 3 4 - top stories
# paragraph 5 6 7 - stories to watch
# paragraph 8 - blank - video 
# paragraoh 9 - get inspired note
# paragraph 10 - awareness raising - do you know?
# 
# Title and video blank note carried no important information and will not use in 
# analysis. Each story has the leading topic country, but some have two countries 
# in the title. The country names can also appear in the paragraph, 
# but those names convey the supportive information of the leading title cause. 
# Therefore, this exercise will analyze the information based on the 
# "topic country names" instead of all country names. 
# 
# Step 1: web scraping
# Step 2: stemming for text classification
# Step 3: extract country names and identify the key topic country names
# Step 4: prepare for sentiment analysis
# Step 5: results presentation
# 
# this issue have exta note
# The Refugee Brief - 17 December 2021

################################################################################


################################################################################
## DEVELOP TIDY WEB SCRAPING AND DATA-FORMATTING FUNCTION ##
################################################################################

tidy_unhcr <- function(date_input, xpath_num ){
  
  ## step 1: web scraping
  url <- paste0("https://www.unhcr.org/refugeebrief/the-refugee-brief-", date_input , "/")
  xpath_input <- paste0('//*[@id="post-', xpath_num, '"]')

  request <- read_html(url)
  
  # extract paragraph in line item
  article <- html_node(request, xpath = xpath_input)   
  paragraphs <- html_nodes(article, "p")
  text_list <- html_text(paragraphs)
  
  df_text <- data.frame(text_list) %>%
    mutate(n = row_number()) 
  
  # this type of correction can only performed because of knowledge from Q1
  # in reality this is not possible and we may loss some country in extracting 
  # and have to find the better way to dead with the word US or USA 
  if(xpath_num == 3153){ 
    df_text <- df_text %>%
      mutate(text_list = ifelse(n == 2, 
                                str_replace_all(text_list, "Côte d'Ivoire", "Ivory Coast"), 
                                text_list), 
             text_list = ifelse(n == 3, 
                                str_replace_all(text_list, "US", "USA"), 
                                text_list))
  }

  if(nrow(df_text) > 10){
    df_text <- df_text %>% 
      filter(n != 2) %>% 
      mutate(n = row_number())
  }

  # story categorization
  df_top <- df_text[2:4,] %>% mutate(category = "Top Stories")
  df_watch <- df_text[5:7,] %>% mutate(category = "Stories to Watch")
  df_inspire <- df_text[9,] %>% mutate(category = "Inspiration")
  df_toknow <- df_text[10,] %>% mutate(category = "Awareness")
  
  df_text <- rbind(df_top, df_watch, df_inspire, df_toknow) 

  ## Step 2: stemming for text classification
  # sentence by obs
  sentence_df <-     unnest_tokens(df_text, sent_tokens,  text_list, token = "sentences") %>%
    mutate(sentence_id = row_number())
  
  # keep only topic obs df
  topic_sent_df <- sentence_df %>%
    group_by(n) %>%
    slice(1) 
  
  sentence_df_ngram <- unnest_tokens(sentence_df, ngram_tokens, sent_tokens, token = "ngrams", n = 5)
  topic_sent_df_ngram <- unnest_tokens(topic_sent_df, ngram_tokens, sent_tokens, token = "ngrams", n = 5)
  
  # process word from the all sentence
  words_df <-     unnest_tokens(sentence_df, word_tokens,  sent_tokens, token = "words")

  # stemming
  words_df$stem <- wordStem(words_df$word_tokens, language = "porter")

  # remove stop words
  nosw_df_combined <- anti_join(words_df, stop_words, by = c("stem" = "word")) 

  ## Step 3: extract country names and identify the key topic country names
  # country list from world.cities dataset
  all_countries <- str_c(unique(tolower(world.cities$country.etc)), collapse = "|")
  all_countries <- str_replace_all(all_countries, "usa", " usa|usa ")
  
  sentence_df_ngram$country <- sapply(str_extract(sentence_df_ngram$ngram_tokens, all_countries), toString)
  topic_sent_df_ngram$country <- sapply(str_extract(topic_sent_df_ngram$ngram_tokens, all_countries), toString)
  
  # all countries' names from the whole briefing issue
  country_df <- sentence_df_ngram %>%
    filter(country != "NA") %>%
    select(country, sentence_id) %>%
    mutate(country = ifelse(country == "usa " | country == " usa", "usa", country)) %>%
    group_by(country, sentence_id) %>%
    slice(1) %>%
    arrange(sentence_id) %>%
    select(sentence_id, country)
  
  # only topic country list
  topic_country_df <- topic_sent_df_ngram %>%
    filter(country != "NA") %>%
    select(country, n) %>%
    mutate(country = ifelse(country == "usa " | country == " usa", "usa", country)) %>%
    group_by(country, n) %>%
    slice(1) %>%
    arrange(n) %>%
    rename(topic_country = country)
  
  # make sure source of data remained as one variable
  if (date_input == "14-January-2021"){
    date_input <- "14-January-2022"
  }
  
  ## Step 4: prepare for sentiment analysis
  sentiment_nrc <-   get_sentiments("nrc")   
  ysentiment_afinn <- get_sentiments("afinn") 
  sentiment_bing <-  get_sentiments("bing") 
  
  for (s in c("nrc", "afinn", "bing")) {
    nosw_df_combined <- nosw_df_combined %>%
      left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
      plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
  }
  
  # merge with country and topic country var
  nosw_df_combined_final <- left_join(nosw_df_combined, topic_country_df , by = c("n" = "n")) 
  
  nosw_df_combined_final <- left_join(nosw_df_combined_final, country_df , by = c("sentence_id" = "sentence_id")) %>%
    mutate(date = as.Date(date_input, format = "%d-%B-%Y"), # https://www.stat.berkeley.edu/~s133/dates.html
           month = month(date), 
           year = year(date)) %>%
    rename(paragraph_id = n)
  
  # nosw_df_combined_final$date_str <- format(as.POSIXct(nosw_df_combined_final$date, 
  #                                                      format = "%d-%B-%Y"),
  #                                           format = "%d%b%y")

  nosw_df_combined_final$date_str <- format(nosw_df_combined_final$date,
                                            format = "%d%b%y")  
  
  return(nosw_df_combined_final)
}

################################################################################
## LOAD INTERESTED UNHRC REFUGEE BRIEFING ISSUES ##
################################################################################

# Understanding UNHCR URL and Xpath Structure

# Each web url are almost identical and only different in date
# https://www.unhcr.org/refugeebrief/the-refugee-brief-28-january-2022/
# https://www.unhcr.org/refugeebrief/the-refugee-brief-3-december-2021/ 
# Noted that having extra line in line 2 for notification of last article of 
# the issue year

# Xpath - did not have a regular pattern number - not haveing serial increasing 
# pattern with date - can't apply math function here
# The Refugee Brief - 28 January 2022   '//*[@id="post-3153"]/div[2]'
# The Refugee Brief - 21 January 2022   '//*[@id="post-3147"]/div[2]'
# The Refugee Brief - 14 January 2021   '//*[@id="post-3139"]/div[2]'
# The Refugee Brief - 7 January 2022    '//*[@id="post-3133"]/div[2]'
# The Refugee Brief - 17 December 2021  '//*[@id="post-3127"]/div[2]'
# The Refugee Brief - 10 December 2021  '//*[@id="post-3121"]/div[2]'
# The Refugee Brief - 3 December 2021   '//*[@id="post-3115"]/div[2]'
# The Refugee Brief - 26 November 2021  '//*[@id="post-3109"]/div[2]'
# The Refugee Brief - 19 November 2021  '//*[@id="post-3103"]/div[2]'
# The Refugee Brief - 12 November 2021  '//*[@id="post-3097"]/div[2]'


## loop - pairwise looping simultaneously
date <- c("28-january-2022", 
          "21-January-2022",
          "14-January-2021", # labeling error in original website as 2021
          "7-January-2022", 
          "17-December-2021",
          "10-december-2021", 
          "3-December-2021", 
          "26-November-2021", 
          "19-November-2021", 
          "12-November-2021")

xpath_num <- c(3153, 
               3147,
               3139, 
               3133, 
               3127, 
               3121, 
               3115, 
               3109,
               3103,
               3097)

file_list <- list()

for (i in 1:length(date)){ # https://stackoverflow.com/questions/46909024/r-loop-over-two-or-more-vectors-simultaneously-paralell
  
  text <- tidy_unhcr(date[i], xpath_num[i])
  
  file_list[[i]] <- (text) 
}


df_combined <- bind_rows(file_list)

# CRITICAL CEHCK - there should be no zero count in column n - story id
all(as.matrix(table(df_combined$date, df_combined$paragraph_id)) != 0)

################################################################################
## SAVE AS RDA & CSV FILE FOR FURTURE PROCESSING ##
################################################################################

# note: 
# because of multiple countries from one sentence joined to original dataset, 
# the observation of lemma and sentiment value become duplicate.
# therefore, require to collapse the dataset into the unique values of 
# country name, sentence id, token id and lemma value before performing any analysis 

save(df_combined, file = "df_combined.rda")
write.csv(df_combined,"df_combined.csv", row.names = FALSE)


################################################################################
## Step 4: results presentation
################################################################################


# result 1: countries coverage by topic nature
df_combined %>% 
  filter(!is.na(country)) %>%
  group_by(category, country) %>%
  slice(1) %>%
  select(category, country) %>%
  view()


# result 2: sentiment analysis by briefing issue
# AFINN
afinn <- df_combined %>% 
  group_by(date_str, sentence_id, word_tokens, stem, afinn) %>%
  slice(1) %>%
  filter(!is.na(afinn)) %>%
  group_by(date_str, stem) %>%
  mutate(n = row_number()) %>%
  slice(1) %>%
  ggplot(aes(n * afinn, date_str, fill = n * afinn > 0)) + 
  geom_col(show.legend = FALSE) +
  labs(title = "Contribution to Sentiment Values by Each Issue (by AFINN)", 
       x = "Sentiment Value (by AFINN) * Number of Occurances", 
       y = "Issue Date") + mytheme 

afinn
ggsave("question2_plot_afinn_by_issue.png", plot = afinn,
       width = 8, height = 8, dpi = 150)

# NRC
df_combined %>% 
  group_by(date_str, sentence_id, word_tokens, stem, nrc) %>%
  slice(1) %>%
  filter(!is.na(nrc)) %>%
  ggplot() +
  geom_histogram(aes(nrc, fill = date_str), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 1)) +
  labs(title = "Occurances of NRC Emotion Lexicon by Each Issue", 
       y = "Number of Occurances", 
       x = "NRC Word-Emotion Association Lexicon") +
  facet_wrap(facets = vars(date_str)) + 
  coord_flip() +
  mytheme + theme(legend.position = "none")

# BING
df_combined %>% 
  group_by(date_str, sentence_id, word_tokens, stem, bing) %>%
  mutate(bing_value = ifelse(bing == "positive", 1, -1)) %>%
  slice(1) %>%
  filter(!is.na(bing)) %>%
  group_by(date_str, stem) %>%
  mutate(n = row_number()) %>%
  slice(1) %>%
  ggplot(aes(n * bing_value, date_str, fill = n * bing_value > 0)) + 
  geom_col(show.legend = FALSE) +
  labs(title = "Contribution to Sentiment Values by Each Issue (by BING)", 
       x = "Sentiment Value (by BING) * Number of Occurances", 
       y = "Issue Date") + mytheme


# correlation among sections
# ref: https://www.tidytextmining.com/ngrams.html#pairwise-correlation
# we need to filter for at least relatively common words first
word_cors <- df_combined %>%
  group_by(stem) %>%
  filter(n() >= 2) %>%
  pairwise_cor(stem, paragraph_id, sort = TRUE) %>%
  mutate(item2 = ifelse(item2 == "burkina", "burkina faso", item2))

topic_country_vector <- unique(df_combined$topic_country)

word_cors_plot <- word_cors %>%
  filter(!is.nan(correlation)) %>%
  filter(item2 %in% topic_country_vector) %>%
  mutate(item2 = toupper(item2)) %>%
  group_by(item2) %>%
  arrange(-correlation) %>%
  filter(row_number() < 5 | row_number() > (n() - 5)) %>% 
  ungroup() %>%
  mutate(item1 = reorder(item1, correlation)) %>%
  ggplot(aes(item1, correlation, fill = item2)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item2, scales = "free") +
  labs(title = "Words that were most correlated with each Country Story", 
       y = "Correlation Value", 
       x = "4 Most and Less Correlated Words - with the topic country") + 
  mytheme + theme(legend.position = "none") + coord_flip() 

word_cors_plot

ggsave("question2_plot_word_cors_per_country.png", plot = word_cors_plot,
       width = 20, height = 20, dpi = 150)


# FINAL NOTE:
# Generating static plots and tables might not be very helpful and exciting to 
# present my report to my boss. Therefore, the shinny app was prepared to please 
# my boss (although it is not a requirement for this HW3 assignment). 
# 
# https://nicholustintzaw.shinyapps.io/homework-3-nicholustintzaw/


