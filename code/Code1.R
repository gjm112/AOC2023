dat <- read.delim("./AOC2023/data1.txt", header = FALSE)$V1
dat <- gsub("[A-z]","",dat)
out <- sum(as.numeric(paste0(substring(dat,1,1),substring(dat,nchar(dat),nchar(dat)))))
out

#Second Part
dat <- read.delim("./AOC2023/data1.txt", header = FALSE)$V1
# oneight
# twone
# threeight
# fiveight
# sevenine
# eightwo
# eighthree
# nineight
dat <- gsub("oneight","18",dat)
dat <- gsub("twone","21",dat)
dat <- gsub("threeight","38",dat)
dat <- gsub("fiveight","58",dat)
dat <- gsub("sevenine","79",dat)
dat <- gsub("eightwo","82",dat)
dat <- gsub("eighthree","83",dat)
dat <- gsub("nineight","98",dat)

dat <- gsub("one","1",dat)
dat <- gsub("two","2",dat)
dat <- gsub("three","3",dat)
dat <- gsub("four","4",dat)
dat <- gsub("five","5",dat)
dat <- gsub("six","6",dat)
dat <- gsub("seven","7",dat)
dat <- gsub("eight","8",dat)
dat <- gsub("nine","9",dat)

dat <- gsub("[A-z]","",dat)

out <- as.numeric(paste0(substring(dat,1,1),substring(dat,nchar(dat),nchar(dat))))
sum(out)





