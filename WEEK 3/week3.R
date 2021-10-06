
## In class exercise about dates

library(tidyverse)
library(lubridate)
library(magrittr)

restaurants <- read_csv('https://data.cityofnewyork.us/api/views/9w7m-hzhe/rows.csv?accessType=DOWNLOAD&bom=true&query=select+*')
susanos.tbl <- filter(restaurants, DBA == "SUSANO'S PIZZERIA & RESTAURANT")

susanos.tbl <- susanos.tbl %>% mutate(`INSPECTION DATE` = mdy(`INSPECTION DATE`)) %>% arrange(`INSPECTION DATE`)

ggplot(susanos.tbl, aes(`INSPECTION DATE`, SCORE)) + geom_point() +geom_line()

susanos.tbl %$% unique(`INSPECTION DATE`) %>% sort()

susanos.tbl $ `INSPECTION DATE` %>% sort()


library(babynames)
babynames.gen <- babynames %>%
  mutate(generation = case_when(
    between(year, 1883, 1900) ~ "Lost",
    between(year, 1901, 1924) ~ "GI",
    between(year, 1925, 1942) ~ "Silent",
    between(year, 1943, 1964) ~ "Baby Boomer",
    between(year, 1965, 1980) ~ "Gen X",
    between(year, 1981, 1995) ~ "Millenial",
    between(year, 1996, 2018) ~ "Gen Z"
  ))

babynames.gen %>% 
  filter(generation == "Gen Z") %>%
  group_by(name, sex) %>%
  summarize(total = sum(n, na.rm = TRUE)) %>%
  arrange(desc(total)) %>% 
  head()

?summarize


### Joins
gini <- read_tsv("/Users/huiwang/Downloads/gini.txt", col_names = FALSE, skip = 2)
names(gini) <- c("row", "country", "gini", "X4")

library(gapminder)
gapminder2007 <- filter(gapminder, year == 2007)

# gapminder2007 <- gapminder2007 %>% left_join(select(gini, country, gini))

gapminder2007 %>% 
  anti_join(select(gini, country, gini)) %>% 
  select(country) %>% 
  arrange(country) %>% 
  print(n = Inf)

gini %>% 
  anti_join(select(gapminder2007, country)) %>%  
  select(country) %>% 
  arrange(country) %>% 
  print(n = Inf)  ## 与下面setdiff(gini$country, gapminder2007$country) %>% sort() 一样

setdiff(gini$country, gapminder2007$country) %>%
  sort()

setdiff(gapminder2007$country, gini$country) %>%
  sort()



test <- c("restaurants rating it", "rat", " It is rat.")
table(str_detect(test, "\\brat\\b|\\brestaurants\\b & \\bit\\b"))



library(Lahman)
?Lahman

SeriesPost.plus <- SeriesPost %>% 
  left_join(Teams %>% select(teamID, yearID, name), by = c("teamIDwinner" = "teamID", "yearID" = "yearID")) 

SeriesPost.plus %>%  select(yearID, round, name)

SeriesPost  %>% 
  anti_join(Teams %>% select(teamID, yearID, name), by = c("teamIDwinner" = "teamID", "yearID" = "yearID")) 

######################################
reg <- c("colour", "food", "color", "favour", "favorite", "or", "our")
str_detect(reg, "or$|our$")
str_detect(reg, ".or|our.|or.|.our")
replace <- "([fvr])"
str_replace_all(reg, replace, "\\1\\1\\1")

names <- unique(babynames$name)
twovow <- "^[^aeiou]*[aeiou]{4}[^aeiou]*$"
sum(str_detect(names, twovow))
subset(names, str_detect(names, twovow))

vowcons <- "^[^aeiou][aeiou]"
sum(str_detect(names, vowcons))
subset(names, str_detect(names, vowcons))

# ^[aeyiuo]+$ for only vowels

# ^[^aeyiuo]+$ for only consonants.

