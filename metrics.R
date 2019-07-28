################################################################################
# LOAD libraries
################################################################################ 
library(tidyverse)
library(rvest)
library(curl)

################################################################################
# graphics SETTINGS
################################################################################ 
# dev.off()
par(las=2)

################################################################################
# N PUBLICATIONS (total, per year)
################################################################################ 
# 
ppy <- table(pubmed.data$Year)

# total number of publications
length(pubmed.data$Year)

# plot number of publications per year    <-- make in ggplot
barplot(height = ppy$Freq, width = ppy$Var1,
        horiz = TRUE, cex.names = 0.8,
        xlab = 'number of publications')
abline(v = mean(ppy$Freq), col = 'red', lwd = 3) # mark mean with vertical line
text(labels = c('mean', round(mean(ppy$Freq), 1)),
     x = mean(ppy$Freq),
     y = 2,
     pos = c(3, 4),
     # adj = 34,
     col = 'red')

ppy <- data.frame(ppy)
ggplot(data = ppy) + geom_bar(aes(group = Var1, x = Freq))

################################################################################
# most frequent co-AUTHORS
################################################################################
# create variable with author names
author <- Author(bar)

# extract author last names
for(i in seq_along(author)) {
      author[[i]] <- author[[i]]$LastName
}
author <- unlist(author)

# count number of occurences of author name and sort by most frequent
coauthors <- sort(table(author),
                  decreasing = TRUE)

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
wiki.url <- read_html('https://en.wikipedia.org/wiki/Journal_of_Pharmacology_and_Experimental_Therapeutics')

# create new column in pubmed data with journal names where whitespaces are
# replaced by underscore, so that the strings can be passed to wikipedia url
pubmed.subset$Journal._ <- str_replace_all(pubmed.subset$Journal, pattern = '\\s', replacement = '_')

# this code works when used on a single page
# but in a for loop gives Error 400: Bad request
for (i in 1:nrow(pubmed.subset)) {
      # create wiki url from journal string and store as html
      wiki.url <- paste0('https://en.wikipedia.org/wiki/',
                          pubmed.subset$Journal._[i])
      content <- read_html(curl(wiki.url),
                           handle = curl::new_handle("useragent" = "Mozilla/51.0.1"))
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
                    end = impact.coords[2]+5) %>%
            as.numeric()
      
}
