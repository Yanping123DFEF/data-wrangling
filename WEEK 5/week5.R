library(tidyverse)

devtools::install_github("jennybc/repurrrsive")
data(package = "repurrrsive")
library(repurrrsive)

unique(map_chr(sw_people, "hair_color"))

map_chr(sw_people, "hair_color") %>% table() %>% sort(decr = TRUE)

names <- tibble(films = map_int(sw_people, ~ length(.$films)), name = map_chr(sw_people, "name"))

map(sw_films, "characters") %>% 
  set_names(map_chr(sw_films, "title")) %>% 
  map_int(length) %>% 
  sort(decr = TRUE)

##########################################################

temp1 <- read_csv("https://raw.githubusercontent.com/jennybc/lotr-tidy/master/data/The_Fellowship_Of_The_Ring.csv")
temp2 <- read_csv("https://raw.githubusercontent.com/jennybc/lotr-tidy/master/data/The_Two_Towers.csv")
temp3 <- read_csv("https://raw.githubusercontent.com/jennybc/lotr-tidy/master/data/The_Return_Of_The_King.csv")

lotr <- bind_rows(temp1, temp2, temp3)

lotr_long <- lotr %>% gather(Female:Male, key = "Gender", value = "Words")

lotr_long %>% group_by(Film) %>% summarize(Word_Total = sum(Words))
lotr_long %>% group_by(Gender) %>% summarize(Word_Total = sum(Words))
lotr_long %>% group_by(Race) %>% summarize(Word_Total = sum(Words))

by_race_film <- lotr_long %>% group_by(Race, Film) %>% summarize(Word_Total = sum(Words)) %>% ungroup()

ggplot(by_race_film, aes(x = Film, y = Word_Total, fill = Race)) + 
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + guides(fill = guide_legend(reverse = TRUE))

lotr_long %>% spread(key = Race, value = Words)

lotr_long %>% spread(key = Gender, value = Words)

lotr_long %>% unite(race_gender, Race, Gender, sep = ":")

##########################################################

library(gapminder)
library(broom)

gap.fit <- lm(lifeExp ~ log10(gdpPercap), data = gapminder)
tidy(gap.fit)
augment(gap.fit) %>% ggplot(aes(.fitted, .resid)) + geom_point()
augment(gap.fit) %>% ggplot(aes(sample = .resid)) + geom_qq() + geom_qq_line()
glance(gap.fit)


gapminder %>% group_by(year) %>% do(tidy(lm(lifeExp ~ log10(gdpPercap), data = .), conf.int = TRUE)) %>% filter(term == "log10(gdpPercap)") %>% ggplot(aes(x = estimate, y = year)) +
  geom_point() + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0)

##########################################################

library(gutenbergr)
bronte_books <- gutenberg_download(gutenberg_id = c(768, 1260), 
                                   meta_fields = "title")
library(tidytext)

tidy_books <- bronte_books %>%
  group_by(title) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()  %>%
  unnest_tokens(word, text)

tidy_books %>% group_by(title) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

tidy_books_sentiment <- tidy_books %>% inner_join(get_sentiments("bing")) %>%
  count(title, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(tidy_books_sentiment, aes(index, sentiment, fill = title)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, ncol = 3, scales = "free_x") + 
  theme(legend.position = "none") +
  geom_smooth(span = .15)

library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))



