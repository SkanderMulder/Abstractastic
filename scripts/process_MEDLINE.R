# dest.dir <- 'C:/Users/dadon/Downloads/MEDLINE/updatefiles'

if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,
               hellno,
               xml2,
               RCurl,
               R.utils)

setwd('C:/Users/dadon/Downloads/MEDLINE/base/')

xml_file <- gunzip(list.files()[1], remove = FALSE)

## Parse the first file
doc <- read_xml(xml_files)