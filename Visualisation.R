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
