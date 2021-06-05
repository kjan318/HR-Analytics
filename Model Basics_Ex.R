library(tidyverse)
library(modelr)
options(na.action = na.warn)

ggplot(sim1, aes(x, y)) +
  geom_point()

sim1 %>%
  view()


models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/5) +
  geom_point()


model1 <- function(a, data){
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

#One common way to do this in statistics to use the “root-mean-squared deviation”.
#We compute the difference between actual and predicted, square them, average them,
#and the take the square root. This distance has lots of appealing mathematical properties,
#which we’re not going to talk about here. You’ll just have to take my word for it!

measure_distance <- function(mod, data){
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff^2))
}

measure_distance(c(7,1.5), sim1)


sim1_dist <- function(a1, a2){
  measure_distance(c(a1,a2), sim1)
}


models <- models %>%
  mutate(dist = purrr::map2_dbl(a1,a2, sim1_dist))

models



ggplot(sim1, aes(x,y)) +
  geom_point(size=2, colours = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist),
   # data = models
    data = filter(models, rank(dist) <= 10)
  )


#We can no longer directly see how the model compares to the data, but we can see many models at once. Again,
#I’ve highlighted the 10 best models, this time by drawing red circles underneath them.


ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))


#generate a spaced grid of points

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
 ) %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))


grid %>%
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour ="red") +
  geom_point(aes(colour = -dist))


#When you overlay the best 10 models back on the original data, they all look pretty good:

ggplot(sim1, aes(x, y)) +
  geom_point(size =2, colour = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist),
    data = filter(grid, rank(dist) <= 10)
    )


#The intuition of Newton-Raphson is pretty simple: you pick a starting point and look around for the steepest slope.
#You then ski down that slope a little way, 
#and then repeat again and again, until you can’t go any lower. In R, we can do that with optim():

best <- optim(c(0,0), measure_distance, data = sim1)

ggplot(sim1, aes(x, y))+
  geom_point(size = 2, colour="grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])




#---------------------environment cleaning

rm(list = ls())
