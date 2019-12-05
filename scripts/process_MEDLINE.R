# dest.dir <- 'C:/Users/dadon/Downloads/MEDLINE/updatefiles'

if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,
               hellno,
               xml2,
               XML,
               RCurl,
               R.utils)

setwd('C:/Users/dadon/Downloads/MEDLINE/base/')

xml_file <- gunzip(list.files()[1], remove = FALSE)

## Parse the first file
doc <- read_xml(xml_files)

###############################################################################
# convert XML to csv
###############################################################################

# test using dummy file
doc <- xmlParse(file = 'data/example.xml') %>%
      xmlToList() %>% plyr::ldply(data.frame)

doc <- doc %>% select(1:5)

write.csv(doc, file = 'data/example.csv')
