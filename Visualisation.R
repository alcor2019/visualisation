library(tidyverse)
set.seed(1234)
indices <- sample(nrow(diamonds), 5000)
diamonds2 <- diamonds[indices,]

diamonds2 %>% ggplot(aes(x=cut)) + geom_bar()

diamonds2 %>% ggplot() + aes(x=price) + geom_histogram(bins = 20, aes(fill = cut), col="black")

diamonds2 %>% ggplot() + aes(x=carat, y=price, color=cut) + geom_point()


