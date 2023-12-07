dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data7.txt", header= FALSE)
dat <- t(apply(dat,1,function(x){strsplit(x, " ")[[1]]}))



#Five of a kind 7
#four of a kind 6
#full house 5
#3 of a kind 4
#2 pair 3
#one pair 2
#high card 1


x <- "AKKKK"
value <- function(x){
  tab <- as.numeric(c(table(strsplit(x[1],"")[[1]])))
  names(tab) <- NULL
  if (identical(sort(tab), c(1,1,1,1,1))){hand <- 1}
  if (identical(sort(tab), c(1,1,1,2))){hand <- 2}
  if (identical(sort(tab), c(1,2,2))){hand <- 3}
  if (identical(sort(tab), c(1,1,3))){hand <- 4}
  if (identical(sort(tab), c(2,3))){hand <- 5}
  if (identical(sort(tab), c(1,4))){hand <- 6}
  if (identical(sort(tab), c(5))){hand <- 7}
  temp <- strsplit(x,"")[[1]]
  temp[temp == "T"] <- 10
  temp[temp == "J"] <- 11
  temp[temp == "Q"] <- 12
  temp[temp == "K"] <- 13
  temp[temp == "A"] <- 14
  
  return(data.frame(hand,one = as.numeric(temp[1]),
                    two = as.numeric(temp[2]),
                    three = as.numeric(temp[3]),
                    four = as.numeric(temp[4]),
                    five = as.numeric(temp[5]),
                    bid = as.numeric(x[2])))
}

library(tidyverse)
greg <- apply(dat,1, value)
results <- do.call(rbind,greg)
test <- results %>% arrange(-hand,-one, -two, -three, -four, -five)
test$rank <- nrow(test):1
test <- test %>% mutate(dollar = bid*rank)

sum(test$dollar)


#part 2.
x <- "AKKKJ"
value <- function(x){
  if (!("J" %in% strsplit(x[1],"")[[1]])){
  tab <- as.numeric(c(table(strsplit(x[1],"")[[1]])))
  names(tab) <- NULL
  if (identical(sort(tab), c(1,1,1,1,1))){hand <- 1}
  if (identical(sort(tab), c(1,1,1,2))){hand <- 2}
  if (identical(sort(tab), c(1,2,2))){hand <- 3}
  if (identical(sort(tab), c(1,1,3))){hand <- 4}
  if (identical(sort(tab), c(2,3))){hand <- 5}
  if (identical(sort(tab), c(1,4))){hand <- 6}
  if (identical(sort(tab), c(5))){hand <- 7}
  temp <- strsplit(x,"")[[1]]
  temp[temp == "T"] <- 10
  temp[temp == "J"] <- 11
  temp[temp == "Q"] <- 12
  temp[temp == "K"] <- 13
  temp[temp == "A"] <- 14
  
  return(data.frame(hand,one = as.numeric(temp[1]),
                    two = as.numeric(temp[2]),
                    three = as.numeric(temp[3]),
                    four = as.numeric(temp[4]),
                    five = as.numeric(temp[5]),
                    bid = as.numeric(x[2])))
  
  }
  
  
  
  #Five of a kind 7
  #four of a kind 6
  #full house 5
  #3 of a kind 4
  #2 pair 3
  #one pair 2
  #high card 1
  #For a single J
  if (sum("J" == strsplit(x[1],"")[[1]]) == 1){
    check <- strsplit(x[1],"")[[1]][strsplit(x[1],"")[[1]] != "J"]
    tab <- as.numeric(c(table(check)))
    names(tab) <- NULL
    
    #if (identical(sort(tab), c(1,1,1,1))){hand <- 1}
    if (identical(sort(tab), c(1,1,1,1))){hand <- 2}
    #if (identical(sort(tab), c())){hand <- 3}
    if (identical(sort(tab), c(1,1,2))){hand <- 4}
    if (identical(sort(tab), c(2,2))){hand <- 5}
    if (identical(sort(tab), c(1,3))){hand <- 6}
    if (identical(sort(tab), c(4))){hand <- 7}
    temp <- strsplit(x,"")[[1]]
    temp[temp == "T"] <- 10
    temp[temp == "J"] <- 0
    temp[temp == "Q"] <- 12
    temp[temp == "K"] <- 13
    temp[temp == "A"] <- 14
    
    return(data.frame(hand,one = as.numeric(temp[1]),
                      two = as.numeric(temp[2]),
                      three = as.numeric(temp[3]),
                      four = as.numeric(temp[4]),
                      five = as.numeric(temp[5]),
                      bid = as.numeric(x[2])))
  }
  
  #Five of a kind 7
  #four of a kind 6
  #full house 5
  #3 of a kind 4
  #2 pair 3
  #one pair 2
  #high card 1
  #For two Js
  if (sum("J" == strsplit(x[1],"")[[1]]) == 2){
    check <- strsplit(x[1],"")[[1]][strsplit(x[1],"")[[1]] != "J"]
    tab <- as.numeric(c(table(check)))
    names(tab) <- NULL
    
    #if (identical(sort(tab), c(1,1,1,1))){hand <- 1}
    #if (identical(sort(tab), c(1,1,1,1))){hand <- 2}
    #if (identical(sort(tab), c())){hand <- 3}
    if (identical(sort(tab), c(1,1,1))){hand <- 4}
    #if (identical(sort(tab), c(2,2))){hand <- 5}
    if (identical(sort(tab), c(1,2))){hand <- 6}
    if (identical(sort(tab), c(3))){hand <- 7}
    temp <- strsplit(x,"")[[1]]
    temp[temp == "T"] <- 10
    temp[temp == "J"] <- 0
    temp[temp == "Q"] <- 12
    temp[temp == "K"] <- 13
    temp[temp == "A"] <- 14
    
    return(data.frame(hand,one = as.numeric(temp[1]),
                      two = as.numeric(temp[2]),
                      three = as.numeric(temp[3]),
                      four = as.numeric(temp[4]),
                      five = as.numeric(temp[5]),
                      bid = as.numeric(x[2])))
  }
  
  #Five of a kind 7
  #four of a kind 6
  #full house 5
  #3 of a kind 4
  #2 pair 3
  #one pair 2
  #high card 1
  #For three Js
  if (sum("J" == strsplit(x[1],"")[[1]]) == 3){
    check <- strsplit(x[1],"")[[1]][strsplit(x[1],"")[[1]] != "J"]
    tab <- as.numeric(c(table(check)))
    names(tab) <- NULL
    
    #if (identical(sort(tab), c(1,1,1,1))){hand <- 1}
    #if (identical(sort(tab), c(1,1,1,1))){hand <- 2}
    #if (identical(sort(tab), c())){hand <- 3}
    #if (identical(sort(tab), c(1,1,1))){hand <- 4}
    #if (identical(sort(tab), c(2,2))){hand <- 5}
    if (identical(sort(tab), c(1,1))){hand <- 6}
    if (identical(sort(tab), c(2))){hand <- 7}
    temp <- strsplit(x,"")[[1]]
    temp[temp == "T"] <- 10
    temp[temp == "J"] <- 0
    temp[temp == "Q"] <- 12
    temp[temp == "K"] <- 13
    temp[temp == "A"] <- 14
    
    return(data.frame(hand,one = as.numeric(temp[1]),
                      two = as.numeric(temp[2]),
                      three = as.numeric(temp[3]),
                      four = as.numeric(temp[4]),
                      five = as.numeric(temp[5]),
                      bid = as.numeric(x[2])))
  }
  
  #Five of a kind 7
  #four of a kind 6
  #full house 5
  #3 of a kind 4
  #2 pair 3
  #one pair 2
  #high card 1
  #For four Js
  if (sum("J" == strsplit(x[1],"")[[1]]) == 4){
    check <- strsplit(x[1],"")[[1]][strsplit(x[1],"")[[1]] != "J"]
    tab <- as.numeric(c(table(check)))
    names(tab) <- NULL
    
    #if (identical(sort(tab), c(1,1,1,1))){hand <- 1}
    #if (identical(sort(tab), c(1,1,1,1))){hand <- 2}
    #if (identical(sort(tab), c())){hand <- 3}
    #if (identical(sort(tab), c(1,1,1))){hand <- 4}
    #if (identical(sort(tab), c(2,2))){hand <- 5}
    #if (identical(sort(tab), c(1,1))){hand <- 6}
    if (identical(sort(tab), c(1))){hand <- 7}
    temp <- strsplit(x,"")[[1]]
    temp[temp == "T"] <- 10
    temp[temp == "J"] <- 0
    temp[temp == "Q"] <- 12
    temp[temp == "K"] <- 13
    temp[temp == "A"] <- 14
    
    return(data.frame(hand,one = as.numeric(temp[1]),
                      two = as.numeric(temp[2]),
                      three = as.numeric(temp[3]),
                      four = as.numeric(temp[4]),
                      five = as.numeric(temp[5]),
                      bid = as.numeric(x[2])))
  }
  
  #Five of a kind 7
  #four of a kind 6
  #full house 5
  #3 of a kind 4
  #2 pair 3
  #one pair 2
  #high card 1
  #For four Js
  if (sum("J" == strsplit(x[1],"")[[1]]) == 5){
    check <- strsplit(x[1],"")[[1]][strsplit(x[1],"")[[1]] != "J"]
    tab <- as.numeric(c(table(check)))
    names(tab) <- NULL
    
    #if (identical(sort(tab), c(1,1,1,1))){hand <- 1}
    #if (identical(sort(tab), c(1,1,1,1))){hand <- 2}
    #if (identical(sort(tab), c())){hand <- 3}
    #if (identical(sort(tab), c(1,1,1))){hand <- 4}
    #if (identical(sort(tab), c(2,2))){hand <- 5}
    #if (identical(sort(tab), c(1,1))){hand <- 6}
    hand <- 7
    temp <- strsplit(x,"")[[1]]
    temp[temp == "T"] <- 10
    temp[temp == "J"] <- 0
    temp[temp == "Q"] <- 12
    temp[temp == "K"] <- 13
    temp[temp == "A"] <- 14
    
    return(data.frame(hand,one = as.numeric(temp[1]),
                      two = as.numeric(temp[2]),
                      three = as.numeric(temp[3]),
                      four = as.numeric(temp[4]),
                      five = as.numeric(temp[5]),
                      bid = as.numeric(x[2])))
  }
  
    
    
    
  }

library(tidyverse)
greg <- apply(dat,1, value)
results <- do.call(rbind,greg)
test <- results %>% arrange(-hand,-one, -two, -three, -four, -five)
test$rank <- nrow(test):1
test <- test %>% mutate(dollar = bid*rank)

sum(test$dollar)

