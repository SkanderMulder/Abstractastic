################################################################################
# Authors: Dalibor Nakladal, Skander Mulder
# Date: 23/10/2015
# Description: This script queries PubMed, mines text of title, author names,
# year, abstract and outputs a wordcloud and an html with formatted mined text.
################################################################################
# LOAD libraries
################################################################################ 
library(rmarkdown)
library(RISmed)
library(SnowballC)

################################################################################
# QUERY and actual data download
################################################################################
# this is the text string with name of author as passed to the PubMed search bar
query <- 'Pirnik Z[Author]'
# create object of class 'EUtilsSummary'
foo <- EUtilsSummary(query)
# create the main object of class 'Medline' which contains probably all mined data
# and has most methods from package RISmed applicable
bar <- EUtilsGet(foo)

################################################################################
# SOME HOUSEKEEPING
################################################################################
# Format the author character strings
author <- Author(bar)

# it would be great to simplify this for loop perhaps with lapply
for(i in seq_along(author)) {
      author[[i]] <- author[[i]]$LastName
      author[[i]] <- paste(author[[i]], collapse = ', ')
}
author <- unlist(author)

################################################################################
# EXTRACT interesting data
################################################################################
# JOURNALS
# journal information can be extracted with functions
# MedlineTA, Title and ISOAbbreviation (probs best for later snatching IF's)
journals <- ISOAbbreviation(bar)

# Country (!)
Country(bar)

# the Volume component (and RISmed function) can be used to hint at the age
# of the journal (and impact?)

# create umm some variable, * come back to this
pubmed_data <- data.frame('Title'=ArticleTitle(bar),
                          'Author'=author,
                          'Year'=YearPubmed(bar),
                          'Abstract'=AbstractText(bar))
nrow(pubmed_data)

################################################################################ 
# FORMAT title and article text strings
################################################################################ 
title <- as.character(pubmed_data$Title)
author <- as.character(pubmed_data$Author)
year <- as.character(pubmed_data$Year)
abstract <- as.character(pubmed_data$Abstract)

################################################################################
# CREATE pre-formatted R markdown (.Rmd) file
################################################################################
# can not render/knit to pdf because of greek alphabet letters in text

# setup file file path
folder <- './abstracts/'
ifelse(!dir.exists(folder), dir.create(folder), FALSE)
# setup file names
file <- paste0(query, '.Rmd')
path <- paste0(folder, file)

# set up YAML header
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

# paste header formatting before title
title <- paste('####', title)

# write combined title and abstract text to our file
for (i in seq_along(nrow(pubmed_data))) {
      cat(c(title[i],
            year[i],
            author[i],
            abstract[i],
            '\n'),
          file = path,
          append = TRUE,
          fill = TRUE,
          sep = '\n')
}

################################################################################
# RENDER readable .html from .Rmd
################################################################################
render(input = path, output_dir = folder)
# clean up workspace (remove source .Rmd file)
# file.remove(path)
