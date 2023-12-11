dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data9.txt", sep = " ", header = FALSE)
dat <- as.matrix(dat)
x <- dat[1,]
# dat <- 
# rbind(c(0, 3, 6, 9, 12, 15),
# c(1, 3, 6, 10, 15, 21),
# c(10, 13, 16, 21, 30, 45))
getends <- function(x){
temp <- x
while (any(temp > 0)){
print(temp)
end <- c(end,temp[length(temp)])
temp <- diff(temp)
}
return(sum(end))
}

lll <- apply(dat,1,getends)
sum(lll)

1479039823
