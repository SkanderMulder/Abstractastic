################################################################################
# LOAD libraries
################################################################################ 
library(tidyverse)
library(rvest)

################################################################################
# graphics SETTINGS
################################################################################ 
# dev.off()
par(las=2)

################################################################################
# N PUBLICATIONS (total, per year)
################################################################################ 
ppy <- table(pubmed.subset$Year)
ppy.df <- data.frame(ppy)

# total number of publications
pubs.tot <- length(pubmed.subset$Year)

# plot number of publications per year
pdf('graphics/pubs_per_year.pdf', width = 6, height = 4)
ggplot(data = ppy.df, aes(x = Var1, y = Freq)) +
      geom_col(aes(fill = Freq)) +
      guides(fill = 'none') +
      geom_hline(yintercept = mean(ppy.df$Freq)) +
      annotate(geom = 'text', label = 'mean',
               x = 1.5, y = mean(ppy.df$Freq)*1.1) +
      annotate(geom = 'text',
               label = paste('Total publications:', pubs.tot),
               x = max(as.numeric(ppy.df$Var1))*0.85,
               y = max(ppy.df$Freq)) +
      labs(title = 'Publications per year', y = 'N publications', x = '') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

################################################################################
# most frequent co-AUTHORS
################################################################################
# create variable with author names
author <- Author(pubmed)

# extract author last names
for(i in seq_along(author)) {
      author[[i]] <- author[[i]]$LastName
}
author <- unlist(author)

# count number of occurences of author name and sort by most frequent
coauthors <- table(author) %>%
      sort(decreasing = TRUE)

# plot top 10 coauthors
barplot(coauthors[2:10],
        horiz = TRUE,
        cex.names = 0.8,
        xlab = 'number of publications')

################################################################################
# IMPACT FACTOR (total, per year)
################################################################################
# - journal impact factors may be mined with Clarivate Analytics API for InCites,
# although this may require an academic subscription
# for now, we use Wikipedia to obtain the impact factor, which of course
#is limited
# wiki.url <- read_html('https://en.wikipedia.org/wiki/Proceedings_of_the_National_Academy_of_Sciences_of_the_United_States_of_America')
# wikipage <- wiki.url %>%
#       html_node('div table') %>%
#       html_text()
# 
# impact.coords <- str_locate(wikipage,
#                             pattern = regex(pattern = 'Impact factor......'))
# 
# impact <- wikipage %>%
#       str_sub(start = impact.coords[2]+1,
#               end = impact.coords[2]+4) %>%
#       as.numeric()

# create new column in pubmed data with journal names where whitespaces are
# replaced by underscore, so that the strings can be passed to wikipedia url
pubmed.subset$Journal._ <- str_replace_all(pubmed.subset$Journal, pattern = '\\s', replacement = '_')

# this code works when used on a single page
# but in a for loop gives Error 400: Bad request
for (i in 1:nrow(pubmed.subset)) {
      # create wiki url from journal string and store as html
      wiki.url <- paste0('https://en.wikipedia.org/wiki/',
                          pubmed.subset$Journal._[i])
      content <- read_html(wiki.url)
      # download.file(wiki.url, destfile = 'scrapedpage.html', quiet = TRUE)
      # content <- read_html('scrapedpage.html')
      # scrape a wikipedia 'div table' panel using rvest
      wikipage <- content %>%
            html_node('div table') %>%
            html_text()
      # locate the coordinates of impact factor in the downloaded string
      impact.coords <- str_locate(wikipage,
                                  pattern = regex(pattern = 'Impact factor......'))
      pubmed.subset$Impact <- wikipage %>%
            str_sub(start = impact.coords[2]+1,
                    end = impact.coords[2]+4) %>%
            as.numeric()
      Sys.sleep(1)
}
