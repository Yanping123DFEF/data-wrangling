library(tidyverse)


map(mtcars, ~length(unique(.x)))
map_int(sw_people, ~length(.x$films)) %>% table()
sw_people[map_int(sw_people, ~length(.x$films)) == 7] %>% map_chr("name")
sw_people <- sw_people %>% set_names(map_chr(sw_people, "name"))
as.numeric(map(mtcars, mean))

devtools::install_github("jennybc/repurrrsive")
data(package = "repurrrsive")
library(repurrrsive)

unique(map_chr(sw_people, "hair_color"))

map_chr(sw_people, "hair_color") %>% table() %>% sort(.,decreasing=TRUE)

sort(map_int(sw_films, ~length(.x$characters))) %>% table()
sw_films[map_int(sw_films, ~length(.x$characters)) == 40] %>% map_chr("title")


names <- tibble(films = map_int(sw_people, ~ length(.$films)), name = map_chr(sw_people, "name"))

exercise <- c("colour", "food", "color", "favour", "favorite", "or", "our")

exercise[str_detect(exercise, "ou?r$")]

exercise[str_detect(exercise, ".ou?r$")]

str_replace_all(exercise, "(.)ou?r$", "\\1or")

library(babynames)
bnames <- unique(babynames$name)
glimpse(bnames)

sum(str_detect(bnames, "[aeiou]{2}"))
sum(str_detect(bnames, "[aeiou]{3,}"))
sum(str_detect(bnames, "[aeiou]{4}"))
bnames[str_detect(bnames, "[aeiou]{4}")]

bnames[str_detect(bnames, "^([^aeiou][aeiou]){6}")]



restaurants <- read_csv('https://data.cityofnewyork.us/api/views/9w7m-hzhe/rows.csv?accessType=DOWNLOAD&bom=true&query=select+*')


rest.small <- restaurants %>% 
  select(-starts_with("VIOL"), -`CRITICAL FLAG`) %>%
  distinct()

library(lubridate)
rest.small <- rest.small %>% mutate(`INSPECTION DATE` = mdy(`INSPECTION DATE`))


rest.small.dba <- rest.small %>% group_by(DBA) 
rest.small.dba <- rest.small.dba %>%
  mutate(date.rank = min_rank(desc(`INSPECTION DATE`)))
head(rest.small.dba)
rest.small.dba.top2 <- filter(rest.small.dba, date.rank <= 2)



rest.small.dba.top2 %>% ungroup() %>%
  group_by(quintile = ntile(SCORE, 5)) %>%
  summarise(mean_score = mean(SCORE))


rest.small.dba.top1 <- filter(rest.small.dba, date.rank == 1) %>% ungroup()

temp <- rest.small.dba.top1 %>%
  group_by(quintile = ntile(SCORE, 5), BORO) %>%
  summarise(mean_score = mean(SCORE))


print(temp, n = Inf)



map_chr(sw_people, "hair_color")

############################################

lm.example <- tibble(x = rnorm(100), y = 5 * x + 2 * rnorm(100)) %>% lm(y ~ x, data = .)


get_r_squared <- function(obj){
  summary(obj)$r.squared
}


get_r_squared <- function(obj, adjusted = FALSE){
  if (adjusted){
    result <- summary(obj)$adj.r.squared
  } else {
    result <- summary(obj)$r.square
  }
  result
}










get_r_squared <- function(obj, adjusted = FALSE){
  if (!("lm" %in% class(obj))) stop ("obj must have class 'lm'") 
  # objects can have multiple classes, but 'lm' must be one of them
  
  if (adjusted){
    result <- summary(obj)$adj.r.squared
  } else {
    result <- summary(obj)$r.square
  }
  result
}

get_resid <- function(obj, num) {
if( num > length(summary(obj)$residuals) ) stop("cannot exceed number of residuals")
else {
position <-  order(abs(summary(obj)$residuals), decreasing = TRUE)[1:num]
summary(obj)$residuals[position] }
}

########################################
########################################
########################################

mtcars %>% map_dbl(mean)
mtcars %>% map_dbl( ~ length(unique(.x)))

map(1:5, rnorm)
