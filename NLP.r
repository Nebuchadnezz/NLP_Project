librarian::shelf(dplyr, tm, data.table, SnowballC, wordcloud, ggrepel)

if(!file.exists("./data")){
   dir.create("./data")
   url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
   download.file(Url, destfile="./data/NLP_data.zip", mode = "wb")
   unzip(zipfile="./data/NLP_data.zip", exdir="./data")
}

filelist <- list.files(path = "./data/en_US", recursive = TRUE,
                       pattern = "\\.txt$", 
                       full.names = TRUE)

blogs <- readLines(filelist[1], skipNul = T, encoding = "UTF-8") 
news <- readLines(filelist[2], skipNul = T, encoding = "UTF-8")
twitter <- readLines(filelist[3], skipNul = T, encoding = "UTF-8")

summary <- data.frame(
   fileName = c("Blogs","News","Twitter"),
   fileSize = c(round(file.size(filelist[1])/1024^2, 1), 
                round(file.size(filelist[2])/1024^2, 1),
                round(file.size(filelist[3])/1024^2, 1)),
   lineLength = c(length(blogs),
                  length(news),
                  length(twitter)),
   wordCount = c(sum(sapply(strsplit(blogs, "\\s+"), length)),
                 sum(sapply(strsplit(news, "\\s+"), length)),
                 sum(sapply(strsplit(twitter, "\\s+"), length)))
)
summary

set.seed(69)

blog_corpus <- paste(sample(blogs, size = 15000), collapse = " ") %>% VCorpus(VectorSource())
news_corpus <- paste(sample(news, size = 15000), collapse = " ") %>% VCorpus(VectorSource())
twitter_corpus <- paste(sample(twitter, size = 15000), collapse = " ") %>% VCorpus(VectorSource())

clean <- function (x) {
   x %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords, tm::stopwords(kind = "en")) %>%
      tm_map(stripWhitespace) %>%
      tm_map(stemDocument)
}

common <- function (x) {
   x <- x %>%
      TermDocumentMatrix() %>%
      findMostFreqTerms(n=100)
   data.frame(freq = x$`1`, word = rownames(data.frame(x$`1`)))
}

ordered <- function (x) {
   x %>% 
      arrange(desc(freq)) %>% 
      top_n(20)
}

blog_corpus <- clean(blog_corpus)
news_corpus <- clean(news_corpus)
twitter_corpus <- clean(twitter_corpus)

blog_common <- common(blog_corpus)
news_common <- common(news_corpus)
twitter_common <- common(twitter_corpus)


blog_common %>%
   top_n(20) %>%
   ggplot(., aes(x=reorder(word,desc(freq)), y=freq, fill = freq)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      xlab("Word") +
      ylab("Frequency") +
      labs(fill = "Frequency") +
      ggtitle("Frequency of Words in Blog Sample")

wordcloud(blog_common$word, freq = blog_common$freq,  colors=brewer.pal(10, "Paired"))

news_common %>%
   top_n(20) %>%
   ggplot(., aes(x=reorder(word,desc(freq)), y=freq, fill = freq)) +
   geom_bar(stat = "identity") +
   theme_bw() +
   xlab("Word") +
   ylab("Frequency") +
   labs(fill = "Frequency") +
   ggtitle("Frequency of Words in News Sample")

wordcloud(news_common$word, freq = news_common$freq,  colors=brewer.pal(10, "Paired"))

twitter_common %>%
   top_n(20) %>%
   ggplot(., aes(x=reorder(word,desc(freq)), y=freq, fill = freq)) +
   geom_bar(stat = "identity") +
   theme_bw() +
   xlab("Word") +
   ylab("Frequency") +
   labs(fill = "Frequency") +
   ggtitle("Frequency of Words in Twitter Sample")
wordcloud(twitter_common$word, freq = twitter_common$freq,  colors=brewer.pal(10, "Paired"))

gram <- function(x, grams){
   paste(x, collapse = " ") %>%
   tokens(what ="word", remove_numbers = TRUE, 
          remove_punct = TRUE, remove_separators = TRUE, remove_symbols =TRUE) %>%
   tokens_tolower() %>%
   tokens_select(stopwords(), selection ="remove") %>%
   tokens_wordstem() %>%
   tokens_ngrams(n = grams) %>%
   dfm()
}

blog_bigram <- gram(blog_sample, 2) 
blog_trigram <- gram(blog_sample, 3)

news_bigram <- gram(news_sample, 2)
news_trigram <- gram(news_sample, 3)

twitter_bigram <- gram(twitter_sample, 2)
twitter_trigram <- gram(twitter_sample, 3)

topfeatures(twitter_trigram, 20)
