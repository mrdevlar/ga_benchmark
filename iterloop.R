
# use output

output$Rep = NA

r = 1
for(i in 1:nrow(output)){
  backIter = output[i-1,"Iter"]
  currIter = output[i  ,"Iter"]
  
  backType = output[i-1, "Type"]
  currType = output[i  , "Type"]
  
  if(length(backIter) == 0 ) {
    backIter = 0
  }
  if(length(backType) == 0) {
    backType = 0
  }
  
  if (backType != currType ) {
    r = 1
  } else
  if (currIter - backIter != 1) {
    r = r + 1
  }
  output[i,"Rep"] = r
  

}



