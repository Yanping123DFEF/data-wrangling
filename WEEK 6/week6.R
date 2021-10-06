library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)

cleaner <- function(x){
  str_replace_all(x, " \\([0-9]+\\)|(\n)", "")
}

cleaner <- function(x){
  str_replace_all(x, "(^[ \t]+|[ \t]+$)|(\n)", "")
}

alpine <- "https://en.wikipedia.org/wiki/List_of_FIS_Alpine_Ski_World_Cup_women%27s_champions"  %>% read_html() %>% html_table() %>% .[[3]]

alpine_clean <- map_df(alpine, cleaner)

alpine_clean %>% count(Overall, sort = TRUE) %>% head(n = 3)

alpine_clean %>% count(Downhill, sort = TRUE) %>% head(n = 3)

alpine_clean %>% count(Slalom, sort = TRUE) %>% head(n = 3)


"https://www.govtrack.us/congress/members/NJ#representatives" %>%
  read_html() %>%
  html_nodes(".plain") %>%
  html_text()

billboard.html <- "https://www.billboard.com/charts/hot-100" %>% read_html()
songs <- billboard.html %>%
  html_nodes(".color--primary") %>%
  html_text() %>%
  str_replace_all("\\n", "")
artists <- billboard.html %>%
  html_nodes(".color--secondary") %>%
  html_text() %>%
  str_extract("^[[:alpha:] ]+$") %>%
   na.omit()


tibble(artists = artists, songs = songs)

spotify.df <- "https://spotifycharts.com/regional" %>%
  read_html() %>%
  html_table() %>%
  .[[1]]
spotify.df <- spotify.df[, c(-1, -3)]
names(spotify.df)[1] <- "rank"
spotify.df <- spotify.df %>% separate(Track, c("Song", "Artist"), sep = "by") %>% map_df(cleaner)

youtube <- "https://www.youtube.com/feed/trending" %>%
  read_html() %>% html_nodes("div") %>%
  html_text()
youtube <- youtube[2] %>% str_extract_all("(?<= ago).*(?=views)") %>% unlist() %>%
# youtube <- youtube[2] %>% str_extract_all("(?<= . ).*(?= ago)") %>%
#   unlist() %>%
#   str_extract_all('\\w+ \\w+$') %>%
#    unlist()
str_replace_all("[,]|[ ]", "") %>% as.numeric()
youtube <- tibble(rank = 1:length(youtube), views = youtube)
ggplot(youtube, aes(rank, views)) + geom_point()
summary(lm(rank ~ views, youtube))

aos <- "https://www.imstat.org/journals-and-publications/annals-of-statistics/" %>%
  read_html() %>% html_nodes("td") %>%
  html_text() %>% tibble()
names(aos) <- "associate_editor"
aos <- aos %>% filter(associate_editor != "") %>% .[5:51,]

youtube <- "https://www.youtube.com/feed/trending" %>%
  read_html() %>% html_nodes("h3") %>%
  html_text()
youtube <- tibble(text = youtube)
youtube <-  youtube %>% separate(text, c("title", "duration"), sep = "- Duration: ") %>%
  subset(!is.na(duration))
youtube$duration <- str_replace_all(youtube$duration, ".$", "")
youtube <- youtube %>% separate(duration, c("minute", "second"), sep = ":")
youtube$minute <- strtoi(youtube$minute)
youtube$second <- strtoi(youtube$second)
youtube <- youtube %>% mutate(rank = 1:dim(youtube)[1], time = minute + second/60)
ggplot(youtube, aes(rank, time)) + geom_point() + geom_smooth()
summary(lm(rank ~ time, youtube))

library(httr)
library(curl)
library(jsonlite)

bike.df <- "http://citibikenyc.com/stations/json" %>%
  curl() %>%
  readLines() %>%
  fromJSON() %>%
  .[[2]] %>%
  as.data.frame()

bike.df %>%
  mutate(prop_empty = availableDocks/totalDocks) %>%
  ggplot(aes(prop_empty)) + geom_density(fill = "lightblue")


omdb_title_actors <- function(title){
  title <- "All about Eve"
  library(RCurl)
  library(stringr)
  if (length(title) > 1) stop("Only one movie title at a time")
  omdb_result <- getForm(
    uri = "http://www.omdbapi.com/",
    apikey = api.key.omdb,
    t = title
  )
  actors <- omdb_result %>%
    fromJSON() %>%
    .[["Actors"]]
  str_trim(unlist(str_split(actors, ",")))
}








