library(tidyverse)

#Read in the data
gapminder_data <- read_csv("data/gapminder_data.csv")


#What is the mean life expectancy? -- Using summarize()
?summarize()
summarize(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>% 
  summarize (averageLifeExp = mean(lifeExp))

#What is the mean population in the gapminder dataset?
gapminder_data_popmean <- gapminder_data %>% 
  summarize(averagePop = mean(pop))

#What is the mean population AND the mean life expectancy?
gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp), meanPop = mean(pop))


#What is the mean life expectancy for the most recent year? -- Using filter() and max()
gapminder_data %>% 
  summarize(recentYear = max(year))

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(meanLifeExp = mean(lifeExp))

gapminder_data %>% 
  filter(year == max(year)) %>% 
  summarize(meanLifeExp = mean(lifeExp))

#What is the mean GDP per capita for the first/earliest year?
gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize(meanGDPperCap = mean(gdpPercap))


#what is the mean life expectancy for EACH year? -- Using group_by()
gapminder_data %>% 
  group_by(year) %>% 
  summarize(meanLifeExp = mean(lifeExp))

#What is the mean life expectancy for each continent?
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp))

#What is the mean life expectancy AND mean GDP per capita for each continent?
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp), meanGDP = mean(gdpPercap))


#What is the GDP (not per capita)? -- Using mutate()
gapminder_data %>% 
  mutate(gdp = gdpPercap * pop)

#Make a new column for population in millions (popInMillions).
gapminder_data %>% 
  mutate(popInMillions = pop/1000000)

#To make them in the same dataset/function...
gapminder_data_popmilgdp <- gapminder_data %>% 
  mutate(gdp = gdpPercap * pop, popInMillions = pop/1000000)


#Using select() -- Chooses a subset of columns from a dataset
gapminder_data %>% 
  select(year, pop)

gapminder_data %>% 
  select(-continent)

#Create a tibble with only country, continent, year, and lifeExp
gapminder_data %>% 
  select(-pop, -gdpPercap)

#Select Helper Function: starts_with(), ends_with(), contains()
gapminder_data %>% 
  select(year, starts_with("c"))

#Side Lesson: Vectors -- All of one type, cannot be a mix of characters or numbers; Using c()
my_vec <- c("dog", "cat", "horse")
num_vec <- c(1, 2, 3, 4)
proof <- gapminder_data %>% 
  pull(year)


#Using pivot_longer() and pivot_wider()
gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

#Pivot_wider, but populate values with gdpPercap
gapminder_data %>% 
  select(country, continent, year, gdpPercap) %>% 
  pivot_wider(names_from = year, values_from = gdpPercap)

#Pivot_longer
gapminder_data %>% 
  pivot_longer(cols = c(pop, lifeExp, gdpPercap), names_to = "measurement_type", values_to = "measurement")


#Is there a relationship between GDP and CO2 emissions? -- Focused on the year 2007
#gapminder_data_2007
#Filtered for year 2007 and continent Americas
#Remove the year and continent columns
gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)

#Read in the CO2 data
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2, col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

#Using inner_join()
inner_join(gapminder_data_2007, co2_emissions, by = "country")










