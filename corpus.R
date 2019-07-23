################################################################################
# LOAD libraries
################################################################################ 
library(wordcloud)
library(RColorBrewer)
library(tm)

################################################################################
# 'tm' library CORPUS creation and text cleaning
################################################################################
baz <- list()
qux <- list()
qux_freq <- list()
qux_df <- list()
data <- pubmed_data[,-3]
for(i in seq_along(data)) {
      # 1. Create Corpus
      baz[[i]] <- Corpus(VectorSource(data[[i]]))
      # 2. Clean Corpus
      baz_i  <- baz[[i]]
      qux[[i]] <- TermDocumentMatrix(x = baz_i,
                                     control = list(stemDocument = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE,
                                                    stopwords = c(stopwords('english')),
                                                    removeNumbers = TRUE, tolower = TRUE))
      # 3. Corpus to matrix
      qux_freq[[i]] <- as.matrix(qux[[i]])
      qux_freq[[i]] <- sort(rowSums(qux_freq[[i]]), decreasing = TRUE)
      # 4. Corpus to data.frame
      qux_df[[i]]   <- data.frame(word=names(qux_freq[[i]]), freq=qux_freq[[i]])
}

################################################################################
# WORDCLOUD OUTPUT
################################################################################
folder <- './graphics/'
ifelse(!dir.exists(folder), dir.create(folder), FALSE)
for(i in seq_along(qux_df)) {
      par(mar = c(1, 1, 1, 1))
      png(filename = paste0(folder, query, '.png'))
      wordcloud(qux_df[[i]][['word']], qux_df[[i]][['freq']],
                random.order = FALSE,
                max.words = 200,
                rot.per = 0.35,
                colors = brewer.pal(8, 'Dark2'))
      dev.off()
}