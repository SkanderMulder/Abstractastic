################################################################################
# LOAD libraries
################################################################################ 
library(wordcloud)
library(RColorBrewer)
library(tm)
library(wordVectors)
library(magrittr)
library(wordVectors)

################################################################################
# (uncomment code to) INSTALL word2vec
################################################################################ 
# install.packages("devtools")
# install.packages("pkgload")
# library(devtools)
# library(pkgload)
# install_github("bmschmidt/wordVectors")

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
      tm_map(removePunctuation) %>% # caution: protein names contain '- or /'
      tm_map(removeNumbers) %>% # tricky: protein names or chemicals
      tm_map(removeWords, stopwords('english')) %>%
      tm_map(stripWhitespace)

# collapse corpus for exporting as single string
corpus.export <- lapply(corpus, as.character) %>%
      unlist %>%
      paste(collapse = 'a')

# write corpus to file
write(corpus.export, file = './corpus.txt')

# compute term-document matrix which can be used for complex tasks as clustering
corpus.tdm <- TermDocumentMatrix(corpus)
inspect(word.freq)

################################################################################
# word2vec MODEL
# using Python, it is possible to load Google's pre-trained word2vec model,
# it is 1.5GB and trained on 3 mil words x 300 features
################################################################################
# train
model <- train_word2vec('corpus.txt','corpus_model.bin',
                        vectors=200, threads=2, window=12,
                        iter=8, negative_samples=0, force = TRUE)

# similarity search
search.word1 <- 'hibernation'
search.word2 <- 'cbs'
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
cosine

################################################################################
# WORDCLOUD OUTPUT
################################################################################
# setup folder
folder <- './graphics/'
ifelse(!dir.exists(folder), dir.create(folder), FALSE)

# set margins
par(mar = c(1, 1, 1, 1))

# create wordcloud
wordcloud(words = corpus,
          random.order = FALSE,
          max.words = 300,
          rot.per = 0.35,
          random.color = TRUE)
