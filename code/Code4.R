dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data4.txt", header= FALSE)

amt <- c()
for (i in 1:nrow(dat)){
#winning numbers
win <- as.numeric(strsplit(strsplit(dat[i,],"[:|]")[[1]][2]," ")[[1]])
win <- win[!is.na(win)]
#numbers you have
num <- as.numeric(strsplit(strsplit(dat[i,],"[:|]")[[1]][3]," ")[[1]])
num <- num[!is.na(num)]

#Amount of win
n <- sum(num %in% win)
if (n == 0){amt <- c(0,amt)}
if (n > 0){amt <- c(2^(n- 1),amt)}


}

sum(amt)


#Part 2.  
dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data4.txt", header= FALSE)

cards <- rep(1,220)
names(cards) <- 1:220
winners <- function(i){
  win <- as.numeric(strsplit(strsplit(dat[i,],"[:|]")[[1]][2]," ")[[1]])
  win <- win[!is.na(win)]
  #numbers you have
  num <- as.numeric(strsplit(strsplit(dat[i,],"[:|]")[[1]][3]," ")[[1]])
  num <- num[!is.na(num)]
  
  n <- sum(num %in% win)
return(n)
}

cards <- rep(1,220)
names(cards) <- 1:220
for (i in 1:220){
num <- winners(i)
if (num > 0){
cards[(i+1):(i+num)] <- cards[(i+1):(i+num)] + cards[i]
}
}

sum(cards[1:220])
