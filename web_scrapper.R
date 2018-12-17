library(rvest)
#library(stringr)

base_url <- 'https://www.lyrics.com'

scrape.songs.urls <- function(artist_url_page) {
  webpage <- read_html(artist_url_page)
  link <- html_nodes(webpage, 'tbody > tr > td.tal.qx > strong > a')
  link_text <- html_attr(link,'href')
  return(link_text)
}

songs.info <- data.frame()

scrape.song.info <- function(url) {

  webpage <- read_html(paste(base_url,url,sep = ""))
  
  title <- html_text(html_nodes(webpage, '#lyric-title-text'))
  artist <- html_text(html_nodes(webpage, '#content-body > div:nth-child(1) > div.lyric.clearfix > hgroup > h3 > a'))
  album <-html_text(html_nodes(webpage, '#content-aside > div:nth-child(3) > div > hgroup > h3 > a'))
  genre <-html_text(html_nodes(webpage, 'div.lyric-infobox.clearfix:nth-of-type(2) > div > div > div > a'))[1]
  year <-html_text(html_nodes(webpage, '#content-body > div:nth-child(1) > div.lyric.clearfix > div:nth-child(2) > div.artist-meta > div.lyric-details > dl > dd.dd-margin > a'))
  lyrics <-html_text(html_nodes(webpage, '#lyric-body-text'))
  #lyrics <- str_replace_all(lyrics,"[\r]","")
  #lyrics <- str_replace_all(lyrics,"[\n]"," ")

  if (mode(album) == 'logical' | identical(album,character(0))) {album <- NA}
  if (mode(year) == 'logical' | identical(year,character(0))) {year <- NA}
  if (mode(genre) == 'logical' | identical(genre,character(0))) {genre <- NA}
  
  
  info <- as.data.frame(cbind(title, artist, album, genre, year, lyrics))
  songs.info <<- rbind(songs.info, info)
  
}

urls <- scrape.songs.urls('https://www.lyrics.com/artist/Imagine-Dragons/1184089')

for (i in 1:length(urls)) {
  print(paste('Scraping song ', i, ' out of ', length(urls)))
  scrape.song.info(urls[i])
}

songs.info <- songs.info[!duplicated(songs.info$title), ]
print(songs.info)

