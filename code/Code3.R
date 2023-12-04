dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data3.txt", header= FALSE)
nums <- c()
#row i
for (i in 1:140){print(i)

loc <- gregexpr("[0-9]+",dat[i,])[[1]]
len <- attr(loc, "match.length")

for (j in 1:length(loc)){
#What do I need to check around it.  
  a <- b <- c <- d <- NULL
#Above
if (i > 1){a <- strsplit(substring(dat[i-1,], loc[j]-1, loc[j]+len[j]),"")[[1]]}
#Below
if (i < 140){b <- strsplit(substring(dat[i+1,], loc[j]-1, loc[j]+len[j]),"")[[1]]}
#left side
if (loc[j] > 1){c <- substring(dat[i,], loc[j]-1, loc[j]-1)}
#right side
if (loc[j]+len[j] < 140){d <- substring(dat[i,], loc[j]+len[j], loc[j]+len[j])}

if (any(c(a,b,c,d) %in% c("-","@","*","/","&","#","%","+","=","$"))){
  nums <- c(nums,substring(dat[i,],loc[j],loc[j]+len[j]-1))
}

}

}


sum(as.numeric(nums))



#Part 2
dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data3.txt", header= FALSE)


prods <- c()

for (i in 1:140){
#i <- 2
loc <- gregexpr("[*]",dat[i,])[[1]]

for (j in 1:length(loc)){
#Check above
if (i > 1){above <- substring(dat[i-1,],loc[j]-1,loc[j]+1)}
#Check below
if (i < 140){below <- substring(dat[i+1,],loc[j]-1,loc[j]+1)}
#Check left
if (loc[j] >1){left <- substring(dat[i,],loc[j]-1,loc[j]-1)}
#Check right
if (loc[j] < 140){right <- substring(dat[i,],loc[j]+1,loc[j]+1)}
  

adj <- 0
if (length(grep("[0-9]",left)) > 0){adj <- adj + 1}
if (length(grep("[0-9]",right)) > 0){adj <- adj + 1}
if (length(grep("[0-9]",above))){adj <- adj + 1}
if (length(grep("[0-9]",below))){adj <- adj + 1}
if (length(grep("[0-9][.][0-9]",above))){adj <- adj + 1}
if (length(grep("[0-9][.][0-9]",below))){adj <- adj + 1}

gears <- c()
if (adj ==2){
#if adj equals exactly 2, then it's a gear.  
if (length(grep("[0-9]",left)) > 0){gears <- c(gears,gsub("[.]","",substring(dat[i,],loc[j]-3,loc[j]-1)))}
if (length(grep("[0-9]",right)) > 0){gears <- c(gears,gsub("[.]","",substring(dat[i,],loc[j]+1,loc[j]+3)))}

  #Check for 2 above or below
if (length(grep("[0-9][.][0-9]",above)) > 0){
  gears <- c(gears,gsub("[.]","",substring(dat[i-1,],loc[j]-3,loc[j]-1)))
  gears <- c(gears,gsub("[.]","",substring(dat[i-1,],loc[j]+1,loc[j]+3)))
} 

  if (length(grep("[0-9][.][0-9]",below)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i+1,],loc[j]-3,loc[j]-1)))
    gears <-c(gears,gsub("[.]","",substring(dat[i+1,],loc[j]+1,loc[j]+3)))
  }
  
  
  #Now check all cases above
  if (length(grep("[0-9][.][.]",above)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i-1,],loc[j]-3,loc[j]-1)))
  } 
  if (length(grep("[.][.][0-9]",above)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i-1,],loc[j]+1,loc[j]+3)))
  } 
  if (length(grep("[.][0-9][0-9]",above)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i-1,],loc[j],loc[j]+2)))
  } 
  if (length(grep("[0-9][0-9][.]",above)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i-1,],loc[j]-2,loc[j])))
  } 
  if (length(grep("[0-9][0-9][0-9]",above)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i-1,],loc[j]-1,loc[j]+1)))
  } 
  
  #Check cases below 
  if (length(grep("[0-9][.][.]",below)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i+1,],loc[j]-3,loc[j]-1)))
  } 
  if (length(grep("[.][.][0-9]",below)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i+1,],loc[j]+1,loc[j]+3)))
  } 
  if (length(grep("[.][0-9][0-9]",below)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i+1,],loc[j],loc[j]+2)))
  } 
  if (length(grep("[0-9][0-9][.]",below)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i+1,],loc[j]-2,loc[j])))
  } 
  if (length(grep("[0-9][0-9][0-9]",below)) > 0){
    gears <-c(gears,gsub("[.]","",substring(dat[i+1,],loc[j]-1,loc[j]+1)))
  } 
  
    
}

if (!is.null(gears)){
prods <- c(prods, prod(as.numeric(gears)))
}


}

}




