


library(devtools)
source_url('https://raw.githubusercontent.com/guanruhao/changchun-city-subway/main/train%20schedule.R')


direction=list(0)

for(i in 1:nrow(simulation_data)){
  data=simulation_data[i,]
  line_usage=line_frame[unlist(data$path_edge),3]
  node_usage=unlist(data$path_node)
  long_direction=numeric(length(node_usage)-1)
  for(j in 1:(length(node_usage)-1)){
    start=node_usage[j]
    end=node_usage[j+1]
    line=unlist(get(paste0("line",line_usage[j])))
    if(which(line==start)<which(line==end)){long_direction[j]=1}else{long_direction[j]=2}
  }
  direction[i]=list(long_direction)
}

















#this function is to transfer node path eg:(1,2,3,4)to many single paths 
#combination(1,2,2,3,3,4)=(1,2),(2,3),(3,4)
broken_path<- function(vec) {
  if(length(unlist(vec)>1)){vec=unlist(vec)
  new_vec <- c(vec[1], rep(vec[-c(1, length(vec))], each = 2), vec[length(vec)])}
  else (new_vec=vec)
  return(new_vec)
}

broken_path(test$path_node)











