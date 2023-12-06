#Time:        48     93     84     66
#Distance:   261   1192   1019   1063
t <- c(48  ,   93  ,   84   ,  66)
d <- c(261 ,  1192,   1019,   1063)

#input is how long you hold the button down
howfar <- function(button_time, race_time = t[1]){
  out <- (race_time - button_time)*button_time
  return(out)
}

library(tidyverse)
vec <- c()
for (i in 1:4){
vec[i] <- sum(map_int(0:t[i], howfar, race_time = t[i]) > d[i])
}

prod(vec)


#part2
library(gmp)
t <- as.bigz(48938466)
d <- as.bigz(261119210191063)
# d <- 261119210191063
# t <- 48938466

howfar <- function(button_time, race_time = t){
  out <- (race_time - button_time)*button_time
  return(out)
}

find0 <- function(button_time){
  dist <- (t - button_time)*button_time
  out <- (dist - d)
  return(out)
}


find0(6094682)
find0(42843785-1)

length(6094682:42843784)

find0(40000000)

int <- c(0,10000000)
point <- mean(int)
for (i in 1:100){print(point)
temp <- find0(point)
if (temp < 0){int <- c(point,int[2])}
if (temp > 0){int <- c(int[1],point)}
point <- mean(int)
}

int <- c(40000000,50000000)
point <- mean(int)
for (i in 1:100){print(point)
  temp <- find0(point)
  if (temp > 0){int <- c(point,int[2])}
  if (temp < 0){int <- c(int[1],point)}
  point <- mean(int)
}





library(gmp)
distance = xt -x^2
#I want to know where distance is greter than d. 
(t-x)*x  > d 
-x^2 + tx - d > 0

a <- -1
b <- t
c <- d

(- b + sqrt(b^2 - 4*a*c))/(2*a)
(- b - sqrt(b^2 - 4*a*c))/(2*a)

#calculus?



