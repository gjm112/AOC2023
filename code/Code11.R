dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data11.txt", sep = " ", header = FALSE)
dat <- t(apply(dat,1,function(x){strsplit(x,"")[[1]]}))

#Add rows and columns that are blank
#Rows
rowind <- which(apply(dat,1,function(x){sum(x == "#")}) == 0)
colind <- which(apply(dat,2,function(x){sum(x == "#")}) == 0)

for (i in rowind){
  adj <- which(rowind == i) - 1
  dat <- rbind(dat[(1:(i+adj)),],rep(".",ncol(dat)),dat[((i+adj)+1):nrow(dat),])
}


#Columns
for (i in colind){
  adj <- which(colind == i) - 1
  dat <- cbind(dat[,(1:(i+adj))],rep(".",nrow(dat)),dat[,(i+adj+1):ncol(dat)])
}

#Replace pounds with numbers. 
dat[dat == "#"] <- c(1:sum(dat == "#"))

#426
#Now calculate distances.  
mat <- matrix(0, nrow = 426, ncol = 426)
for (i in 1:435){print(i)
  for (j in (i+1):426){
    iloc <- which(dat == i, TRUE)
    jloc <- which(dat == j, TRUE)
    
    mat[i,j] <- sum(abs(jloc - iloc))
  }
}
sum(mat)


#Part 2. 
dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data11.txt", sep = " ", header = FALSE)
dat <- t(apply(dat,1,function(x){strsplit(x,"")[[1]]}))

#Add rows and columns that are blank
#Rows
rowind <- which(apply(dat,1,function(x){sum(x == "#")}) == 0)
colind <- which(apply(dat,2,function(x){sum(x == "#")}) == 0)


#Replace pounds with numbers. 
dat[dat == "#"] <- c(1:sum(dat == "#"))

#426
#Now calculate distances.  
mat <- matrix(0, nrow = 426, ncol = 426)
for (i in 1:425){print(i)
  for (j in (i+1):426){
    iloc <- which(dat == i, TRUE)
    jloc <- which(dat == j, TRUE)
    
    rowadd <- sum(rowind >= min(iloc[1],jloc[1]) & rowind <= max(iloc[1],jloc[1]))
    coladd <- sum(colind >= min(iloc[2],jloc[2]) & colind <= max(iloc[2],jloc[2]))
    
    mat[i,j] <- sum(abs(jloc - iloc) + c(999999*rowadd,999999*coladd))
  }
}


sum(mat)


710675618476

