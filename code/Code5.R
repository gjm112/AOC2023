seeds <- c(4043382508, 113348245, 3817519559 ,177922221 ,3613573568 ,7600537 ,773371046, 400582097 ,2054637767 ,162982133 ,2246524522, 153824596 ,1662955672, 121419555, 2473628355, 846370595, 1830497666 ,190544464, 230006436, 483872831)

from_to <- list()
from_to[[1]] <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data5_seed2soil.txt", sep = " ", header = FALSE)
from_to[[2]] <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data5_soil2fert.txt", sep = " ", header = FALSE)
from_to[[3]] <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data5_fert2water.txt", sep = " ", header = FALSE)
from_to[[4]] <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data5_water2light.txt", sep = " ", header = FALSE)
from_to[[5]] <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data5_light2temp.txt", sep = " ", header = FALSE)
from_to[[6]] <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data5_temp2humid.txt", sep = " ", header = FALSE)
from_to[[7]] <- read.delim("/Users/gregorymatthews/Dropbox/AOC2023/data/data5_humid2loc.txt", sep = " ", header = FALSE)
names(from_to[[1]]) <- c("dest","source","range")
names(from_to[[2]]) <- c("dest","source","range")
names(from_to[[3]]) <- c("dest","source","range")
names(from_to[[4]]) <- c("dest","source","range")
names(from_to[[5]]) <- c("dest","source","range")
names(from_to[[6]]) <- c("dest","source","range")
names(from_to[[7]]) <- c("dest","source","range")


#takes a row of the seed2soil data and returns true is the seed is in that row, FALSE otherwise
findrow <- function(r, seed){
  out <- ((seed <= r[2] + r[3] - 1) & (seed >= r[2]))
  return(out)
}

#Note if now row is found, then the map is the identity. 

mapping <- function(xseed, data = from_to[[1]]){
  #Find the correct row
  therow <- apply(data,1,find, seed = xseed)
  #find the map.  
  if (sum(therow)==0){out <- xseed }
  if (sum(therow)>0){out <- data[therow,"dest"] + xseed - data[therow,"source"]}
  return(out)
  
}


soil_to_loc <- function(seed){
temp <- mapping(seed, from_to[[1]])
for (i in 2:7){
temp <- mapping(temp, from_to[[i]])
}
return(temp)
}

library(tidyverse)
map(seeds,soil_to_loc) %>% unlist() %>% min()









#Part 2
from_to[[1]] %>% arrange(source)

#One row of the mapping and one row of the sourcerange
findrows2check <- function(r, sourcerange){
  #find when the intervals DON'T overlap
  out <- !((sourcerange[1] + sourcerange[2] -1  < r[2]) | sourcerange[1] > (r[2] + r[3] - 1))
  return(out)
}

therow <- apply(from_to[[1]],1,findrows2check, sourcerange = c(seeds[1], seeds[2]))
from_to[[1]][therow,]
#apply(from_to[[2]],1,findrows2check, sourcerange = from_to[[1]][therow,][1,2:3])
#apply(from_to[[2]],1,findrows2check, sourcerange = from_to[[1]][therow,][2,2:3])


therow2 <- apply(from_to[[1]][therow,c(1,3)],1,function(x){apply(from_to[[2]],1,findrows2check, sourcerange = x)})
therow2 <- apply(therow2,1,sum)>0

therow3 <- apply(from_to[[2]][therow2,c(1,3)],1,function(x){apply(from_to[[3]],1,findrows2check, sourcerange = x)})
therow3 <- apply(therow3,1,sum)>0

therow4 <- apply(from_to[[3]][therow3,c(1,3)],1,function(x){apply(from_to[[4]],1,findrows2check, sourcerange = x)})
therow4 <- apply(therow4,1,sum)>0

therow5 <- apply(from_to[[4]][therow4,c(1,3)],1,function(x){apply(from_to[[5]],1,findrows2check, sourcerange = x)})
therow5 <- apply(therow5,1,sum)>0

therow6 <- apply(from_to[[5]][therow5,c(1,3)],1,function(x){apply(from_to[[6]],1,findrows2check, sourcerange = x)})
therow6 <- apply(therow6,1,sum)>0

therow7 <- apply(from_to[[6]][therow6,c(1,3)],1,function(x){apply(from_to[[7]],1,findrows2check, sourcerange = x)})
therow7 <- apply(therow7,1,sum)>0

from_to[[7]][therow7,] %>% arrange(dest)
from_to[[7]] %>% arrange(dest)


c(seeds[1],seeds[1] + seeds[2] - 1)


mapping <- function(xseed, data = from_to[[1]]){
  #Find the correct row
  therow <- apply(data,1,findrows2check, seed = seedrange)
  #find the map.  
  data[therow,]
  if (sum(therow)==0){out <- xseed}
  if (sum(therow)>0){out <- data[therow,"dest"] + xseed - data[therow,"source"]}
  return(out)
  
}




