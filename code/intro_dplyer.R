# Intro to dplyr

library(dplyr)
library(ggplot2)

# load gapminder data as sample dataset
gapminder <-  read.csv("data/gapminder_data.csv", stringsAsFactors = F)


# convert colums to factors and characters
gapminder$continent <-as.factor(gapminder$continent)
gapminder$continent <-as.character(gapminder$continent)

mean(gapminder[gapminder$continent == "Africa", "gdpPercap"])

# Functions we will learn today from dplyr:
#1. select()
#2. filter()
#3. group_by()
#4. summarize()
#5. mutate()

# what attributes in gapminder
colnames(gapminder)

# elect three attributes from gapminder
subset_1 <-  gapminder %>% 
  select(country, continent, lifeExp)

# select every attribute except 2:
subset_2 <-  gapminder %>%
  select(-lifeExp, -pop)

# select some attributes but rename a few of them for clarity
subset_3 <-  gapminder %>%
  select(country, population = pop, lifeExp, gdp = gdpPercap)

# using filter()
africa <- gapminder %>% 
  filter(continent == "Africa") %>% 
  select(country, population = pop, lifeExp)

table(africa$country)


# select year, population, country, for Europe
europe <- gapminder %>% 
  filter(continent == "Europe") %>% 
  select(year, country, population = pop)

europe_table <-  table(europe$country)
View(europe_table)

# working with group_by() & summarize()

str(gapminder %>%  group_by(continent))

# summarize the mean gdp per continent
gdp_continent <-  gapminder %>% 
  group_by(continent) %>% 
  summarize(mean_gdp = mean(gdpPercap), mean_lifeExp = mean(lifeExp))


# plot it 
summary_plot <- gdp_continent %>% 
  ggplot(aes(x = mean_gdp, y = mean_lifeExp)) + geom_point(stat = "identity") + 
            theme_bw()
summary_plot

# calculate mean population for all the continents
mean_pop <-  gapminder %>% 
  group_by(continent) %>% 
  summarize(mean_pop = mean(pop))
mean_pop

# count() and n()
gapminder %>% 
  filter(year == 2002) %>% 
  count(continent, sort = TRUE)

gapminder %>% 
  group_by(continent) %>% 
  summarize(se = sd(lifeExp)/sqrt(n()))

# mutate() is my friend (add column based on existing columns)
xy <- data.frame(x = rnorm(100), y = rnorm(100))

head(xy)
xyz <- xy %>% mutate(z = x*y)
head(xyz)

# add a column that gives full gdp per continent
gdp_per_cont <- gapminder %>% 
  mutate(total_gdp = pop*gdpPercap) %>% 
  group_by(continent) %>% 
  summarize(cont_gdp = sum(total_gdp))


View(gdp_per_cont)






