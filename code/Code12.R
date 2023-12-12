dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data12.txt", sep = " ", header = FALSE)

library(stringr)

j <- 3
string <- dat$V1[j]
pat <- dat$V2[j]
pat <- as.numeric(strsplit(pat,",")[[1]])
numq <- str_count(dat$V1[j],"\\?")
string <- strsplit(string,"")[[1]]
dat[j,]

getcount <- function(string, pat){
poss <- as.matrix(expand.grid(rep(list(c(".","#")),numq)))
count <- 0
for (i in 1:nrow(poss)){
  temp <- string
  temp[temp == "?"] <- c(poss[i,])
  temp <- paste0(temp, collapse = "")
  rege <- paste0("\\.*",paste0("#{",paste0(pat,collapse = "}\\.+#{"),"}"),"\\.*")
  #if (str_detect(temp,rege) & str_count(temp,"#") == sum(pat)){print(temp)}
  count <- count + (str_detect(temp,rege) & str_count(temp,"#") == sum(pat))
}
return(count)
}

vec <- c()
for (j in 1:nrow(dat)){print(j)
string <- dat$V1[j]
pat <- dat$V2[j]
pat <- as.numeric(strsplit(pat,",")[[1]])
numq <- str_count(dat$V1[j],"\\?")
string <- strsplit(string,"")[[1]]
vec[j] <- getcount(string,pat)

}
sum(vec)



#Part 2. 


