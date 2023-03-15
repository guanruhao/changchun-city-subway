



library(devtools)
source_url('https://raw.githubusercontent.com/guanruhao/changchun-city-subway/main/3%20train%20schedule.R')

#this is a frame that shows directions for passengers for all links
#this should be used together with "simulation_data"
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
#remove useless names
rm(data,i,j,name,line,line_usage,long_direction,node_usage,start,end,new_matrix)
#import function from github repo
#this function is to deal with 2 vectors
#remove local repeated elements of first  vector 
#and retain the correspond order elements of the second vector
#It can be ensured that all the elements in the second row 
#corresponding to the same number group in the first row are the same.
#eg: first vector is (1,1,1,2,2,2,3,3,3,4,4,4,2,2,2,1,1,1)
#second vector is    (1,1,1,1,1,1,2,2,2,3,3,3,3,3,3,2,2,2)
#output are (1,2,3,4,2,1) and (1,1,2,3,3,2)
# function name is "remove_local_duplicates(vec1,vec2)"
library(devtools)
source_url('https://raw.githubusercontent.com/guanruhao/function-repo/main/remove%20local%20duplicates.R')




























