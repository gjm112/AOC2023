possible <- function(x){
  #find out where green, blue, and red are located
  gind <- grep("green",x)
  bind <- grep("blue",x)
  rind <- grep("red",x)
  
  red_num <- ifelse(length(rind) ==0, 0, gsub("red","",x[rind]))
  green_num <- ifelse(length(gind) ==0, 0, gsub("green","",x[gind]))
  blue_num <- ifelse(length(bind) ==0, 0, gsub("blue","",x[bind]))
  
  out <- all(as.numeric(c( red_num, green_num, blue_num)) <= c(12,13,14))
  return(out)
}


dat <- read.delim("./AOC2023/data2.txt", header= FALSE)

gsub("Game ", "",strsplit(dat[1,], ":")[[1]][1])

vec <- c()
for (i in 1:nrow(dat)){
temp <- strsplit(dat[i,], ":")[[1]][2]
temp2 <- strsplit(temp,";")[[1]]
temp3 <- strsplit(temp2, ",","")
temp3 <- lapply(temp3, function(x){gsub(" ","",x)})
vec[i] <- all(unlist(lapply(temp3,possible)))

}


#12 red cubes, 13 green cubes, and 14 blue cubes

sum(vec * 1:100)



#Part 2
nums <- function(x){
  #find out where green, blue, and red are located
  gind <- grep("green",x)
  bind <- grep("blue",x)
  rind <- grep("red",x)
  
  red_num <- ifelse(length(rind) ==0, 0, gsub("red","",x[rind]))
  green_num <- ifelse(length(gind) ==0, 0, gsub("green","",x[gind]))
  blue_num <- ifelse(length(bind) ==0, 0, gsub("blue","",x[bind]))
  
  out <- as.numeric(c( red_num, green_num, blue_num)) 
  return(out)
}


dat <- read.delim("./AOC2023/data2.txt", header= FALSE)

gsub("Game ", "",strsplit(dat[1,], ":")[[1]][1])

vec2 <- c()
for (i in 1:nrow(dat)){
  temp <- strsplit(dat[i,], ":")[[1]][2]
  temp2 <- strsplit(temp,";")[[1]]
  temp3 <- strsplit(temp2, ",","")
  temp3 <- lapply(temp3, function(x){gsub(" ","",x)})
  
  vec2[i] <-  prod(apply(do.call(rbind,lapply(temp3,nums)),2,max))
  
}

sum(vec2)





