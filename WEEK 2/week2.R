library(tidyverse)
library(babynames)
library(nycflights13)
library(magrittr)


tv <- read_csv('https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv')

tv %>% filter(genres == "Action,Adventure,Drama", date > ymd("17/1/1")) %>% 
  group_by(title, seasonNumber, date) %>% 
  arrange(desc(av_rating))

tv %>% group_by(title) %>% summarise(frequency = n()) %>% arrange(desc(frequency))
dat <- filter(tv, title == "Criminal Minds")
dat <- filter(tv, title == "Law & Order" | title == "Law & Order: Special Victims Unit")
ggplot(dat, aes(date, av_rating, color = title)) + 
  geom_point() + geom_line() + geom_smooth() +
  ggtitle("date and rating") + 
  xlab("date") + 
  ylab("rating")

ggplot(dat, aes(date, av_rating, color = title)) + 
  geom_point() + geom_line() + geom_smooth() +
  ggtitle("date and rating") + 
  xlab("date") + 
  ylab("rating") + facet_wrap(~ title, nrow = 2)

babynames %>% 
  group_by(name, sex) %>% 
  summarize(total = sum(n)) %>% 
  arrange(desc(total))

baby2000 <- babynames %>% 
 filter(year >= 2000) %>%
  group_by(name, sex) %>% 
  summarize(total = sum(n)) %>% 
   arrange(desc(total))

filter(baby2000, name == "Jason")
filter(baby2000, name == "Jia")
filter(baby2000, name == "Dora")
filter(baby2000, name == "Hui")

temp <- tibble(a = 1:10, b = rnorm(10))
temp

temp[rep(c(TRUE, FALSE), 2),]

flights %>% .$arr_time
flights %>% .[["arr_time"]]
flights$arr_time


## ggplot  in-class practice
flights %>% filter(month == 9, day == 12) %>%
  ggplot(aes(sched_dep_time, dep_delay, colour = origin)) + 
  geom_point() + 
  ggtitle("Delays on Sept 12, 2013") + 
  xlab("Scheduled Departure Time") + 
  ylab("Departure Delay") + 
  scale_colour_discrete(name = "Airport", labels = c("Newark", "JFK", "LaGuardia")) +
  geom_smooth()

library(gapminder)
library(ggrepel)

ggplot(data = filter(gapminder, year == 2007),  aes(gdpPercap, lifeExp)) + 
  geom_point(aes(gdpPercap, lifeExp, colour = continent, size = pop)) + 
  scale_x_log10() + geom_smooth()  + 
  coord_cartesian(ylim = c(75, 83), xlim = c(5000, 55000)) + 
  geom_label_repel(aes(label = country), alpha = .75, data = gapminder %>% filter(year == 2007, lifeExp >= 75, pop > 35000000))
