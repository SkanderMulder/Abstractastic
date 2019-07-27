################################################################################
# LOAD libraries
################################################################################ 
library(tidyverse)

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
# journal impact factors may be mined with Clarivate Analytics API for InCites,
# although this may require an academic subscription