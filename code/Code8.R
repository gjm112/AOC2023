directions <- c("LRRLRRRLRRLRRLRRRLRRLRLLRRRLRRRLRRRLRRRLRRRLRRLLRRRLLRLRRRLRRLRRRLLRRRLRLLRRLRLLRLRLLRRLRRRLRRLLRRRLRRRLRLRRRLRRRLRRRLRRRLRLRRRLLRRRLRRLRRRLRLRRRLRRLRLLLLLRRRLRRRLRRRLRRRLRRLLRLRLRRLRRLLRRRLRRRLRRRLLLRRRLRRRLRRRLRLRRRLLRLRLRRLRRLRRRLRRLRRRLRRRLRRRLRRLLRRRLRRLRRLRLLRRRR")
directions <- strsplit(directions,"")[[1]]

dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data8.txt", sep = " ", header = FALSE)
dat$V3 <- gsub("(\\(|\\)|,)","",dat$V3)
dat$V4 <- gsub("(\\(|\\))","",dat$V4)
dat <- dat[,c(1,3,4)]
names(dat) <- c("A","L","R")

#Start at AAA
i <- 1
loc <- "AAA"
nexttt <- function(loc,i){
  dir <- directions[i]
  return(dat[dat$A == loc,dir])
}
count <- 0
loc <- "AAA"
i <- 1
while (loc != "ZZZ"){
  loc <- nexttt(loc,i)
  count <- count + 1
  if (i < 269){i <- i + 1} else {i <- 1}
  print(loc)
  
}

QXA, AAA, PDA, TDA, QQA, PPA, 
c(12643, 16409, 14257, 15871, 18023, 19637)/269
count <- 0
loc <- "QXA"
i <- 1
while (!grepl("..Z",loc)){
  loc <- nexttt(loc,i)
  count <- count + 1
  if (i < 269){i <- i + 1} else {i <- 1}
  print(loc)
  
}
print(count)

library(pracma)
pracma::Lcm(12643, 16409)
pracma::Lcm(771223, 16409) 
pracma::Lcm(771223,  14257) 
pracma::Lcm(40874819,  15871) 
pracma::Lcm(2411614321     ,  18023) 
library(gmp)
as.bigz(pracma::Lcm(161578159507,19637))








#Step 2
nexttt <- function(loc,i){
  dir <- directions[i]
  return(dat[dat$A %in% loc,dir])
}

start <- dat$A[substring(dat$A,3,3) == "A"]

count <- 0
loc <- start
i <- 1
while (any(substring(loc,3,3) != "Z")){
  loc <- nexttt(loc,i)
  count <- count + 1
  if (i < 269){i <- i + 1} else {i <- 1}
  if (count %% 100000 == 0){print(count)}
}
count


