if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,
               hellno,
               RCurl)

# base database
url.base <- 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/'

filenames <- getURL(url.base, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
      strsplit('\r\n') %>%
      unlist()

dest.dir <- 'C:/Users/dadon/Downloads/MEDLINE/base'

for (filename in filenames) {
      download.file(url = paste(url.base, filename, sep = ""),
                    destfile = paste(dest.dir, "/", filename, sep = ""))
}

# daily update
url.update <- 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/updatefiles/'

filenames <- getURL(url.update, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
      strsplit('\r\n') %>%
      unlist()

dest.dir <- 'C:/Users/dadon/Downloads/MEDLINE/updatefiles'

for (filename in filenames) {
      download.file(url = paste(url.update, filename, sep = ""),
                    destfile = paste(dest.dir, "/", filename, sep = ""))
}
