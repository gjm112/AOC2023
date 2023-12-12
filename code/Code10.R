dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data10.txt", sep = " ", header = FALSE)
dat <- t(apply(dat,1,function(x){strsplit(x,"")[[1]]}))

#Which row
i <- which(apply(dat,1,function(x){sum(x == "S")>0}))
#Which column
j <- which(apply(dat,2,function(x){sum(x == "S")>0}))

#dir UDLR
#Pipes: F7JL-|



getnext <- function(i, j, dir){
  current <- dat[i,j]
iold <- i
jold <- j
#L
if (dir == "L"){
  if (current == "L"){i <- i - 1}
  if (current == "F"){i <- i + 1}
  if (current == "-"){j <- j - 1}
}
if (dir == "R"){
  if (current == "7"){i <- i + 1}
  if (current == "J"){i <- i - 1}
  if (current == "-"){j <- j + 1}
}
if (dir == "U"){
  if (current == "7"){j <- j - 1}
  if (current == "F"){j <- j + 1}
  if (current == "|"){i <- i - 1}
}
if (dir == "D"){
  if (current == "J"){j <- j - 1}
  if (current == "L"){j <- j + 1}
  if (current == "|"){i <- i + 1}
}
current <- dat[i,j]
if (i < iold){dir <- "U"}
if (i > iold){dir <- "D"}
if (j < jold){dir <- "L"}
if (j > jold){dir <- "R"}

return(list(i,j,dir))

}
i 
iold
  dat[i,j]
  dir
  
  
  #current <- "L"
  #Starting
  i <- 26
  j <- 32
  dir <- "L"
  # i <- 2
  # j <- 3
  # dir <- "R"
  
  # i <- 5
  # j <- 14
  # dir <- "R"
  count <- 0
  dat[i,j]
  order <- c(dat[i,j])
  orderi <- c(i)
  orderj <- c(j)
while (dat[i,j] != "S"){print(count)
out <- getnext(i,j,dir)
count <- count + 1
i <- out[[1]]           
j <- out[[2]]  
order <- c(order,dat[i,j])
orderi <- c(orderi,i)
orderj <- c(orderj,j)
dir <- out[[3]]
}

dir
i
j




#dat[i,j-1]
#dat[(i-6):(i+6),(j-6):(j+6)]


#Part 2
check <- data.frame(i = orderi, j = orderj, val = order)
inside <- 0
library(tidyverse)
for (iii in 1:nrow(dat)){print(iii)
  for (jjj in 1:ncol(dat)){
    if (check %>% filter(i == iii & j == jjj) %>% nrow() == 0){
    if (check %>% filter(i == iii & j >= jjj + 1) %>% nrow() > 0){
      
        string_row <- check %>% filter(i == iii & j >= jjj + 1) %>% arrange(j)
        string_row <- merge(string_row,data.frame(j = (jjj+1):140), by = "j", all.y = TRUE)
        string_row$val[is.na(string_row$val)] <- "."
        string_row <- string_row %>% pull(val)
        string_row <- paste0(string_row, collapse = "")
        string_row <- gsub("((F-*J)|(L-*7)|(LS7))","|",string_row)
        totest_row <- strsplit(string_row,"")[[1]]
        
        
        
          inside <- inside + sum(totest_row %in% c("|","J","F","7","L")) %% 2

         # if (sum(totest_row %in% c("|","J","F","7","L")) %% 2 == 1){print(c(iii,jjj))}
      
      }
    }
  }
}

inside
#8339 is too high
#1572 is too high
#1638 is too high

#Not 897
#Not 831
#not 524