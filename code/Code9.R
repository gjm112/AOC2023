dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data9.txt", sep = " ", header = FALSE)
dat <- as.matrix(dat)
#dat <- rbind(c(0, 3, 6, 9, 12, 15),c(1, 3, 6, 10, 15, 21),c(10, 13, 16, 21, 30, 45))
# x <- dat[1,]
getends <- function(x){
end <- c()
temp <- x
while (!all(temp == 0)){
print(temp)
end <- c(end,temp[length(temp)])
temp <- diff(temp)
}

return(sum(end))
}
lll <- apply(dat,1,getends)
sum(lll)

1479039823


#Part 3
dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data9.txt", sep = " ", header = FALSE)
dat <- as.matrix(dat)
#dat <- rbind(c(0, 3, 6, 9, 12, 15),c(1, 3, 6, 10, 15, 21),c(10, 13, 16, 21, 30, 45))
# x <- dat[1,]
getbegin <- function(x){
  start <- c()
  #temp <- c(10,13,16,21,30,45)
  temp <- x
  while (!all(temp == 0)){
    print(temp)
    start <- c(start,temp[1])
    temp <- diff(temp)
  }
  
  count <- 0
  for (i in length(start):1){
  count <- start[i] - count
}
  
  return(count)
}
lll <- apply(dat,1,getbegin)
sum(lll)



1479039823