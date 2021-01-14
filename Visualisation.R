library(tidyverse)
set.seed(1234)
indices <- sample(nrow(diamonds), 5000)
diamonds2 <- diamonds[indices,]

diamonds2 %>% ggplot(aes(x=cut)) + geom_bar()

diamonds2 %>% ggplot() + aes(x=price) + geom_histogram(bins = 20, aes(fill = cut), col="black")

diamonds2 %>% ggplot() + aes(x=carat, y=price, color=cut) + geom_point()

D <- data.frame(X=seq(-2*pi,2*pi,by=0.01))
ggplot(D) + aes(x=X,y=sin(X)) + geom_line()


ggplot(diamonds2) + aes(x=price) + geom_histogram(bins=40, col="black", fill="white")

ggplot(diamonds2) + aes(x=price, y=..count..) + geom_histogram(bins=40, col="black", fill="white")

ggplot(diamonds2) + aes(x=price, y=..density..) + geom_histogram(bins=40, col="black", fill="white")

ggplot(diamonds2) + aes(x=price) + stat_bin(bins=40, col="black", fill="white")

ggplot(diamonds2) + aes(x=price, y=..density..) + stat_bin(bins=40, col="black", fill="white")


library(ggmap)
lat <- -24.66
lon <- 176.9
zoom <- 4
MaCarte <- get_map(location=c(lon,lat),zoom=zoom)
ggmap(MaCarte)

gapminder %>% filter(continent == "Africa" & year==2012) %>% ggplot(aes(x=fertility, y=life_expectancy, color=region)) + geom_point()
df <- gapminder %>% filter(year==2012 & continent=="Africa" & life_expectancy >= 70 & fertility <= 3) %>% select(country, region)  
df
tab <- gapminder %>% filter(year %in% seq(1960,2010) & country %in% c("United States", "Vietnam"))
tab                            

p <- tab %>% ggplot(aes(year, life_expectancy, color=country)) + geom_line()
p

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

gapminder %>% filter(year %in% seq(1960,2010) & country %in% c("Cambodia")) %>% ggplot(aes(year, life_expectancy, color=country)) + geom_line()

daydollars <- gapminder %>% filter (!is.na(gdp) & continent=="Africa" & year==2010) %>% mutate(dollars_per_day = gdp/population/365)
daydollars %>% ggplot(aes(dollars_per_day)) + geom_density() + scale_x_continuous(trans="log2")

daydollars <- gapminder %>% filter (!is.na(gdp) & continent=="Africa" & year %in% c(1970, 2010)) %>% mutate(dollars_per_day = gdp/population/365)
daydollars %>% ggplot(aes(dollars_per_day, fill=region)) + geom_density(bw=0.5, position="stack") + scale_x_continuous(trans="log2") + facet_grid(. ~year)

gapminder_Africa_2010 <- gapminder %>% filter (!is.na(gdp) & continent=="Africa" & year == 2010) %>% mutate(dollars_per_day = gdp/population/365)
gapminder_Africa_2010 %>% ggplot(aes(x=dollars_per_day, y=infant_mortality, color=region)) + geom_point()

gapminder_Africa_2010 %>% ggplot(aes(x=dollars_per_day, y=infant_mortality, color=region)) + geom_point() + scale_x_continuous(trans="log2")

gapminder_Africa_2010 %>% ggplot(aes(x=dollars_per_day, y=infant_mortality,color=region, label=country)) +
  geom_point() + scale_x_continuous(trans="log2") + geom_text(aes(label=country))

gapminder %>% filter(!is.na(gdp) & continent=="Africa" & year %in% c(1970, 2010)) %>% mutate(dollars_per_day = gdp/population/365) %>% 
ggplot(aes(x=dollars_per_day, y=infant_mortality,color=region, label=country)) + geom_point() + scale_x_continuous(trans="log2") + 
geom_text(aes(label=country)) + facet_grid(. ~year) 

gapminder %>% filter(!is.na(gdp) & !is.na(infant_mortality) & continent=="Africa" & year %in% c(1970, 2010)) %>% mutate(dollars_per_day = gdp/population/365) %>% 
  ggplot(aes(x=dollars_per_day, y=infant_mortality,color=region, label=country)) + geom_point() + scale_x_continuous(trans="log2") + 
  geom_text(aes(label=country)) + facet_grid(year ~ .) 
  
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)

color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)

data(murders)
murders %>% mutate(murder_rate = total / population * 100000) %>%
  mutate(state = reorder(state, murder_rate)) %>%
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("")

library(dplyr)
library(ggplot2)
library(dslabs)
dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
state <- reorder(state,rate)
state
levels(state)

library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

library(tidyverse)
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
murders %>% mutate(rate = total/population*100000) %>% mutate(region=reorder(region, rate, FUN=median)) %>% 
  ggplot(aes(region, rate)) +  geom_boxplot() + geom_point()

murders %>% mutate(rate=total/population*100000) %>% group_by(region) %>% summarize(mediane=median(rate))
# region        mediane
# <fct>           <dbl>
#   1 Northeast        1.80
# 2 South            3.40
# 3 North Central    1.97
# 4 West             1.29
murders %>% mutate(rate=total/population*100000) %>% group_by(region) %>% summarize(max=max(rate))
# A tibble: 4 x 2
# region          max
# <fct>         <dbl>
#   1 Northeast      3.60
# 2 South         16.5 
# 3 North Central  5.36
# 4 West           3.63

data("us_contagious_diseases")
str(us_contagious_diseases)

