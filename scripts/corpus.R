################################################################################
# INSTALL word2vec
################################################################################ 
if (!require(wordVectors)) {
      if (!(require(devtools))) {
            install.packages("devtools")
      }
      devtools::install_github("bmschmidt/wordVectors")
}

################################################################################
# LOAD libraries
################################################################################ 
if (!require(pacman)) {
      install.packages('pacman')
}
pacman::p_load(wordcloud,
               RColorBrewer,
               tm,
               wordVectors,
               tsne,
               magrittr,
               ggrepel)

################################################################################
# create text CORPUS and clean text
################################################################################
# process abstracts to text corpus
# todo: bundle n-grams (bigram example is 'olive oil', bundled to 'olive_oil')
corpus <- pubmed.subset$Abstract %>%
      # create corpus
      VectorSource() %>% Corpus() %>%
      # clean corpus
      tm_map(tolower) %>%
      tm_map(removePunctuation) %>% # caution: protein names contain '-' or /'
      tm_map(removeNumbers) %>% # tricky: protein names or chemicals
      tm_map(removeWords, stopwords('english')) %>%
      tm_map(stripWhitespace)

# collapse corpus for exporting as single string
corpus.export <- lapply(corpus, as.character) %>%
      unlist %>% paste(collapse = 'a')

# write corpus to file
write(corpus.export, file = 'temp/corpus.txt')

# compute term-document matrix which can be used for complex tasks as clustering
corpus.tdm <- TermDocumentMatrix(corpus)
corpus.dtm <- DocumentTermMatrix(corpus)

################################################################################
# word2vec MODEL | build
# using Python, it is possible to load Google's pre-trained word2vec model,
# it is 1.5GB and trained on 3 mil words x 300 features
################################################################################
# convert words to phrases ('New' and 'York' give 'New_York')
word2phrase(train_file = 'temp/corpus.txt',
            output_file = 'temp/corpus_phrased.txt',
            force = TRUE, debug_mode = 2)
# train
model <- train_word2vec('temp/corpus_phrased.txt','temp/corpus_model.bin',
                        vectors=200, threads=2, window=10,
                        iter=4, negative_samples=10, force = TRUE)

################################################################################
# word2vec MODEL | similarity search
################################################################################
# the user inputs two search words,
# or the default will be the two most frequent words

# search.word1 <- 'hibernation'
# search.word2 <- 'cbs'
search.word1 <- 'macrophage'
search.word2 <- 'constriction'
search.word1 <- rownames(model)[2]
search.word2 <- rownames(model)[3]
model %>% closest_to(search.word1)
model %>% closest_to(search.word2)

# find words that are 'similar' to the ~ combination of ... AND ...
model %>% closest_to(~'macrophage'+'anticontractile',
                     fancy_names = TRUE)
# find words that are 'similar' to the ~ combination of ... BUT NOT ...
model %>% closest_to(~'macrophage'-'anticontractile')
# find analogies
model %>% closest_to(~'noradrenaline'-'anticontractile'+'macrophage')

## plot similar terms to two searched words
similarity <- model[[c(search.word1, search.word2), average=F]]

# cosineSimilarity is more powerful, because it can compare two matrices to each other
# closest_to can only take a vector or vectorlike object as its second argument
# compute cosine similarities restricted to the most common words in the set
cosine <- model[1:200,] %>% cosineSimilarity(similarity)

# Filter to the top terms.
cosine <- cosine[rank(-cosine[,1])<10 | rank(-cosine[,2])<10,] %>%
      as.data.frame()

colnames(cosine) <- c('word1', 'word2')
cosine$word <- rownames(cosine)

pdf('graphics/word2vec_cosine_similarity.pdf', width = 6, height = 6)
ggplot(data = cosine, aes(x = word1, y = word2, label = word)) +
      geom_label_repel() +
      labs(title = 'Cosine similarity of most two frequent terms',
           x = search.word1, y = search.word2)
dev.off()

# this optional code can plot many words from model without selection
# wordVectors::plot(x = model, method = 'tsne')

################################################################################
# WORDCLOUD
################################################################################
# read in phrased corpus
corpus.phrased <- scan('temp/corpus_phrased.txt', what = 'character') %>%
      VectorSource() %>% Corpus()

# setup folder
folder <- './graphics/'
ifelse(!dir.exists(folder), dir.create(folder), FALSE)

# setup
par(mar = c(1, 1, 1, 1))
set.seed(1)

# create wordcloud
pdf('graphics/wordcloud.pdf', width = 6, height = 4)
wordcloud(words = corpus.phrased,
          random.order = FALSE,
          max.words = 200,
          rot.per = 0.35,
          random.color = TRUE,
          color = brewer.pal(7, 'Paired'))
dev.off()
