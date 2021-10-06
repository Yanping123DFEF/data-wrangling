library(tidyverse)

library(repurrrsive)
GoT <- tibble(
  name = got_chars %>% map_chr("name"),
  aliases = got_chars %>% map("aliases"),
  allegiances = got_chars %>% map("allegiances")
)


GoT <- GoT %>% mutate(Lannister_alligent = str_detect(allegiances, "Lannister"))
GoT_l <- GoT %>% filter(Lannister_alligent)

GoT_l <- GoT_l %>% select(-allegiances, -Lannister_alligent)

GoT_l[c(1,3,4)] %>% unnest()

library(dbplyr)
library(RSQLite)
library(RMySQL)
example_sql <- lahman_sqlite()

inner_join(tbl(example_sql, "Managers"), tbl(example_sql, "HallOfFame")) 

inner_join(tbl(example_sql, "Managers"), tbl(example_sql, "HallOfFame")) %>%
  select(category, playerID) %>% distinct()

inner_join(tbl(example_sql, "Managers"), tbl(example_sql, "HallOfFame")) %>%
  select(category, playerID) %>% distinct %>% 
  collect() %>% 
  count(category)

library(pool)
library(dplyr)

my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest"
)
Country <- tbl(my_db, "Country")
City <- tbl(my_db, "City")
CountryL <- tbl(my_db, "CountryLanguage")
Chick <- tbl(my_db, "ChickWeight")






