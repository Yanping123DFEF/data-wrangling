### First example using the gapminder package

# load needed packages
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('gapminder')
library(ggplot2)
library(tidyverse)
library(help = gapminder)
library(gapminder)

# An excerpt of the data available at Gapminder.org.
# For each of 142 countries, the package provides
# values for life expectancy, GDP per capita, and
# population, every five years, from 1952 to 2007.


head(gapminder)  
glimpse(gapminder)

with(gapminder, table(continent))
with(gapminder, table(year))
with(gapminder, table(country))

with(gapminder, table(year, continent))










subset(gapminder, country == "Canada")  # One way of subsetting in base R
filter(gapminder, country == "United States")  # tidyverse way of subsetting
filter(gapminder, continent == "Americas" , year == 2007, lifeExp <70)  # tidyverse way of subsetting


subset(gapminder, continent == "Americas" & year == 2007)  # tidyverse way of subsetting

print(filter(gapminder, continent == "Americas", year == 2007), n = Inf, width = Inf)  # tidyverse way of subsetting
View(filter(gapminder, continent == "Americas",  year == 2007))














### Now some visualization

ggplot(data = filter(gapminder, country == "United States"))

ggplot(data = filter(gapminder, country == "United States")) + geom_point(mapping = aes(year, gdpPercap))
ggplot(data = filter(gapminder, country == "United States")) + geom_point(mapping = aes(year, gdpPercap)) + geom_line(mapping = aes(year, gdpPercap))


ggplot(data = filter(gapminder, country == "United States"), mapping = aes(year, gdpPercap)) + geom_point() + geom_line()

ggplot(data = filter(gapminder, country == "Japan")) + geom_point(mapping = aes(year, pop)) + geom_line(mapping = aes(year, pop))

ggplot(data = filter(gapminder, country == "China")) + geom_point(mapping = aes(year, lifeExp)) + geom_line(mapping = aes(year, lifeExp))

ggplot(data = filter(gapminder, country == "Korea, Rep.")) + geom_point(mapping = aes(year, pop)) + geom_line(mapping = aes(year, pop))

ggplot(data = filter(gapminder, country == "United States")) + geom_point(mapping = aes(year, lifeExp)) + geom_line(mapping = aes(year, lifeExp))

ggplot(data = filter(gapminder, country == "United States"), mapping = aes(year, lifeExp)) + geom_point() + geom_line()

ggplot(data = filter(gapminder, country == "United States"), mapping = aes(year, lifeExp)) + geom_point() + geom_line()

ggplot(data = filter(gapminder, country == "United States"), mapping = aes(gdpPercap, lifeExp)) + geom_point() + geom_line()












### Let's try looking at all the countries at once, restricting to 2007
ggplot(data = filter(gapminder, year == 2007),  aes(gdpPercap, lifeExp)) + geom_point() + geom_smooth()


ggplot(data = filter(gapminder, year == 2007),  aes(gdpPercap, lifeExp)) + geom_point() + scale_x_log10()

ggplot(data = filter(gapminder, year == 2007),  aes(gdpPercap, lifeExp)) + geom_point() + scale_x_log10() + geom_smooth()

ggplot(data = filter(gapminder, year == 2007),  aes(gdpPercap, lifeExp)) + geom_point(aes(gdpPercap, lifeExp, colour = continent)) + scale_x_log10() + geom_smooth()

ggplot(data = filter(gapminder, year == 2007),  aes(gdpPercap, lifeExp, colour = continent)) + geom_point() + scale_x_log10() + geom_smooth(span = 1)

ggplot(data = filter(gapminder, year == 2007),  aes(gdpPercap, lifeExp)) + geom_point(aes(gdpPercap, lifeExp, colour = continent, size = pop)) + scale_x_log10() + geom_smooth()










# What are those countries with high GDP but low life expectancy?
filter(gapminder, year == 2007, lifeExp < 60, gdpPercap > 3000)
# let's save the names of those countries
unexpected <- select(filter(gapminder, year == 2007, lifeExp < 60, gdpPercap > 3000), country)


# Looking at all the years in different panels---note the facet_wrap addition
ggplot(data = filter(gapminder),  aes(gdpPercap, lifeExp)) + geom_point(mapping = aes(gdpPercap, lifeExp, colour = continent, size = pop)) + geom_smooth() + scale_x_log10() + facet_wrap(~ year, nrow = 3)

# Looking just a China---notice how the colour option is changed from above
ggplot(data = filter(gapminder),  aes(gdpPercap, lifeExp)) + 
  geom_point(
    mapping = aes(gdpPercap, lifeExp, colour = (country == "China"), size = pop)
  ) + 
  geom_smooth() + 
  scale_x_log10() + 
  facet_wrap(~ year, nrow = 2)

# those unexpected countries. Note the use of %in%
ggplot(data = filter(gapminder),  aes(gdpPercap, lifeExp)) + geom_point(mapping = aes(gdpPercap, lifeExp, colour = (country %in% unexpected$country), size = pop)) + geom_smooth() + scale_x_log10() + facet_wrap(~ year, nrow = 2)


animated.gap <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country)) +
  scale_x_log10()

library(plotly)

ggplotly(animated.gap)













### switching to flights

# On-time data for all flights that departed NYC (i.e. JFK, LGA or EWR) in 2013.
library(nycflights13)


summary(flights)
glimpse(flights)


with(flights, table(carrier))
with(flights, table(origin))




# let's look at departure delay by time of day

ggplot(flights, mapping = aes(sched_dep_time, dep_delay)) + geom_point() + geom_smooth()

# let's restrict that to a single airport and day of the year
ggplot(filter(flights, origin == "EWR", month == 9, day == 13), mapping = aes(sched_dep_time, dep_delay)) + geom_point() + geom_smooth()

# flights just to SFO or LAX

ggplot(filter(flights, origin == "EWR", month == 9, day == 12, dest %in% c("SFO", "LAX")), mapping = aes(sched_dep_time, dep_delay)) + geom_point() + geom_smooth()



# all the major NYC airports
ggplot(filter(flights, month == 9, day == 13), mapping = aes(sched_dep_time, dep_delay)) + geom_point() + facet_wrap(~ carrier) + geom_smooth()

head(weather)
ggplot(filter(weather, month == 5, day == 21), mapping = aes(hour, wind_speed)) + geom_point() + geom_line() + facet_wrap(~ origin)
