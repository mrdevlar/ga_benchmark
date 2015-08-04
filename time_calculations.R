library(readr)
library(stringr)
library(stringi)

eurotours = list.files("euro_tour/")
length(eurotours)


output = data.frame(t(rep(NA, 5)))
names(output) = c("Type", "Mean", "SD", "Median", "MAD")

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
  
  timePat = "(\\d\\d\\.\\d\\d)"
  tourString = str_split(tourString, "\r\n")
  tourString = unlist(tourString)
  times = str_subset(tourString, timePat)
  # times = str_split(times, "\\s")
  
  times = stri_extract_first(tourString, regex = timePat)
  times = times[!is.na(times)]
  # print(times)
  times = as.numeric(times)
  
  out = (c(type, mean(times), sd(times), median(times), mad(times)))
  output = rbind(output, out)
  print(out)
}

output = output[2:nrow(output),]
write.table(output, "clipboard", sep="\t", row.names=FALSE)
