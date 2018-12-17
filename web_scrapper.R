library(rvest)
library(tm)
library(SnowballC)

base_url <- 'https://www.lyrics.com'
artist_url_page <- 'https://www.lyrics.com/artist/Imagine-Dragons/1184089'

#Function to scrape songs links for a given artist
scrape.songs.urls <- function(artist_url_page) {
  webpage <- read_html(artist_url_page)
  link <- html_nodes(webpage, 'tbody > tr > td.tal.qx > strong > a')
  link_text <- html_attr(link,'href')
  return(link_text)
}

songs.info <- data.frame()

#Function to scrape a given song info
scrape.song.info <- function(url) {
  webpage <- read_html(paste(base_url,url,sep = ""))

  title <- html_text(html_nodes(webpage, '#lyric-title-text'))
  artist <- html_text(html_nodes(webpage, '#content-body > div:nth-child(1) > div.lyric.clearfix > hgroup > h3 > a'))
  album <-html_text(html_nodes(webpage, '#content-aside > div:nth-child(3) > div > hgroup > h3 > a'))
  genre <-html_text(html_nodes(webpage, 'div.lyric-infobox.clearfix:nth-of-type(2) > div > div > div > a'))[1]
  year <-html_text(html_nodes(webpage, '#content-body > div:nth-child(1) > div.lyric.clearfix > div:nth-child(2) > div.artist-meta > div.lyric-details > dl > dd.dd-margin > a'))
  lyrics <-html_text(html_nodes(webpage, '#lyric-body-text'))

  #Replace attribute with NA if missing
  if (mode(album) == 'logical' | identical(album,character(0))) {album <- NA}
  if (mode(year) == 'logical' | identical(year,character(0))) {year <- NA}
  if (mode(genre) == 'logical' | identical(genre,character(0))) {genre <- NA}
  
  info <- as.data.frame(cbind(title, artist, album, genre, year, lyrics))
  songs.info <<- rbind(songs.info, info)
  
}

#Scrape songs url for a given artist on lyrics.com
urls <- scrape.songs.urls(artist_url_page)

#Scrape each song info
for (i in 1:length(urls)) {
  print(paste('Scraping song ', i, ' out of ', length(urls)))
  scrape.song.info(urls[i])
}


#Remove duplicate if found
songs.info <- songs.info[!duplicated(songs.info$title), ]

#Dataset cleaning (special characters, punctuation, stopwrods etc...)
docs <- Corpus(VectorSource(songs.info$lyrics))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "\n")
docs <- tm_map(docs, toSpace, "\r")
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
#docs <- tm_map(docs, removeWords, c("hey", "just","can"))
docs <- tm_map(docs, stemDocument)

# Frequent words
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
