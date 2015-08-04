library(readr)
library(stringr)
library(stringi)


eurotours = list.files("euro_tour/")


output = data.frame(t(rep(NA, 4)))
names(output) = c("Iter", "Mean", "Best", "Type")

for(tour in eurotours){
  pat1 = "(?<=_)(rw|to)"
  pat2 = "(?<=_)(cx|ox|pbx|pmx)"
  pat3 = "(?<=_)(dm|ism|scr|sim|sw)"
  
  reg1 = regexpr( pat1, tour, perl = T )
  reg2 = regexpr( pat2, tour, perl = T )
  reg3 = regexpr( pat3, tour, perl = T )
  
  mat1 = regmatches(tour,reg1)
  mat2 = regmatches(tour,reg2)
  mat3 = regmatches(tour,reg3)
  
  type = paste(mat1,mat2,mat3,sep = "_")
  # print(tour)
  # print(type)
  
  tourString = read_file(paste0("euro_tour/",tour))
  
  tourString = str_split(tourString, "\r\n")
  tourString = unlist(tourString)
  gaDat = str_subset(tourString, "Iter")
  
  gaDat = str_extract_all(gaDat, "\\d(?:(?!\\|).)*")
  
  gaDat = unlist(gaDat)
  gaDat = as.numeric(gaDat)
  gaDat = matrix(gaDat, ncol=3, byrow=T)
  gaDat = as.data.frame(gaDat)
  names(gaDat) = c("Iter", "Mean", "Best")
  gaDat$Type = type
  
  
  
  output = rbind(output, gaDat)
}

output = output[2:nrow(output),]





