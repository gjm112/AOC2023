dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data10.txt", sep = " ", header = FALSE)
dat <- apply(dat,1,function(x){strsplit(x,"")[[1]]})

#Which row
i <- which(apply(dat,1,function(x){sum(x == "S")>0}))
#Which column
j <- which(apply(dat,2,function(x){sum(x == "S")>0}))

dat[i,j]

dat[(i-1):(i+3),(j-3):(j+3)]


