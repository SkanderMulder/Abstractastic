################################################################################
# Author: Dalibor Nakladal
# Date: 23/10/2015
# Description: This script queries PubMed, mines text of title, author names,
# year, abstract and outputs a wordcloud and an html with formatted mined text.
################################################################################
# LOAD libraries
################################################################################ 
library(rmarkdown)
library(RISmed)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
################################################################################
# QUERY and actual data download
################################################################################
# this is the text string with authors name
# as plugged into the PubMed search bar
query <- 'Pirnik Z[Author]'
# create object of class 'EUtilsSummary'
foo <- EUtilsSummary(query)
# create the main object of class 'Medline'
# which contains probably all mined data
# and has most methods from package RISmed applicable
bar <- EUtilsGet(foo)

################################################################################
# SOME HOUSEKEEPING
################################################################################
# Format the author character strings
author <- Author(bar)

### it would be great to simplify this for loop perhaps with lapply
for(i in seq_along(author)) {
      author[[i]] <- author[[i]]$LastName
      author[[i]] <- paste(author[[i]], collapse = ', ')
}
author <- unlist(author)

# JOURNALS
# get all the journal abbreviations
journals <- MedlineTA(bar)

# create umm some variable, * come back to this
pubmed_data <- data.frame('Title'=ArticleTitle(bar),
                          'Author'=author,
                          'Year'=YearPubmed(bar),
                          'Abstract'=AbstractText(bar))
nrow(pubmed_data)
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
################################################################################ 
# EXTRACT title and article text strings
################################################################################ 
title <- as.character(pubmed_data$Title)
author <- as.character(pubmed_data$Author)
year <- as.character(pubmed_data$Year)
abstract <- as.character(pubmed_data$Abstract)
################################################################################
# CREATE pre-formatted R markdown (.Rmd) files
# RENDER readable .html from .Rmd
################################################################################
folder <- './abstracts/'
ifelse(!dir.exists(folder), dir.create(folder), FALSE)

# ! I can not render/knit to pdf because of greek alphabet letters in text

# setup file names and file path
file <- paste0(query, '.Rmd')
path <- paste0(folder, file)

# set up file YAML header
cat(c("---",
      "output:",
      paste0("      ", "html_document:"),
      paste0("            ", "self_contained: no"),
      "---",
      ""),
    file = path,
    append = TRUE,
    fill = TRUE,
    sep = "\n")

# write title list and ToC
# for(i in seq_along(title)) {
#       cat(title[i], '\n', sep = '\n', file = path, append = TRUE, fill = TRUE)
# }

# paste header formatting before title
title <- paste('###', title)
# append new line after each abstract
abstract <- paste0(abstract, '\n')

# write combined title and abstract text to our file
for (i in seq_along(title)) {
      cat(c(title[i], year[i], author[i], abstract[i]),
          sep = '\n',
          file = path,
          append = TRUE,
          fill = TRUE)
}
# render the file into .html
render(input = path, output_dir = folder)
# clean up workspace (remove source .Rmd file)
# file.remove(path)
