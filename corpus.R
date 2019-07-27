################################################################################
# LOAD libraries
################################################################################ 
library(wordcloud)
library(RColorBrewer)
library(tm)
library(wordVectors)
library(magrittr)

################################################################################
# create text CORPUS and clean text
################################################################################
# process abstracts to text corpus
# todo: bundle n-grams (bigram example is 'olive oil', bundled to 'olive_oil')
corpus <- pubmed.subset$Abstract %>%
      # create corpus
      VectorSource() %>%
      Corpus() %>%
      # clean corpus
      tm_map(tolower) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords('english')) %>%
      tm_map(stripWhitespace)

# collapse corpus for exporting as single string
corpus.export <- lapply(corpus, as.character) %>%
      unlist %>%
      paste(collapse = 'a')

# write corpus to file
write(corpus.export, file = './corpus.txt')

################################################################################
# word2vec MODEL
################################################################################
# train
model <- train_word2vec('corpus.txt','corpus_model.bin',
                        vectors=200, threads=2, window=12,
                        iter=8, negative_samples=0, force = TRUE)

# similarity search
search.word1 <- 'pvat'
search.word2 <- 'atherosclerosis'
model %>% closest_to(search.word1)

## plot similar terms to two searched words
similarity <- model[[c(search.word1, search.word2), average=F]]

# compute cosine similarities restricted to the most common words in the set
cosine <- model[1:200,] %>% cosineSimilarity(similarity)

# Filter to the top 20 terms.
cosine <- cosine[
      rank(-cosine[,1])<20 |
            rank(-cosine[,2])<20,
      ]

plot(cosine, type = 'n')
text(cosine, labels = rownames(cosine))

################################################################################
# WORDCLOUD OUTPUT
################################################################################
# compute term-document matrix which can be used for complex tasks as clustering
# TermDocumentMatrix(control = list(stemDocument = TRUE,
#                                   removePunctuation = TRUE,
#                                   stripWhitespace = TRUE,
#                                   stopwords = c(stopwords('english')),
#                                   removeNumbers = TRUE, tolower = TRUE))
# 
# # plot wordcloud
# folder <- './graphics/'
# ifelse(!dir.exists(folder), dir.create(folder), FALSE)
# for(i in seq_along(qux_df)) {
#       par(mar = c(1, 1, 1, 1))
#       png(filename = paste0(folder, query, '.png'))
#       wordcloud(qux_df[[i]][['word']], qux_df[[i]][['freq']],
#                 random.order = FALSE,
#                 max.words = 200,
#                 rot.per = 0.35,
#                 colors = brewer.pal(8, 'Dark2'))
#       dev.off()
# }