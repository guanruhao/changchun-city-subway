
library(devtools)
source_url('https://raw.githubusercontent.com/guanruhao/changchun-city-subway/main/4%20upgrade%20of%20passenger%20info.R')
#this function is to return the more detailed information about the passenger


#according to data "simulation_data"
simu=function(aaa,bbb,ccc,ddd,eee){
  aaa=unlist(aaa)
  bbb=unlist(bbb)
  ccc=unlist(ccc)
  ddd=unlist(ddd)
  simp_aaa=unlist(rle(aaa)[2])
  n_stop=unlist(rle(aaa)[1])
  sequence=c(1,1+cumsum(n_stop))
  start_station=bbb[sequence[1]]
  #the sequence to find station number need to depart, transfer and arrive
  #do the first step individually
  station_col=which(get(paste0("line",simp_aaa[1]))==start_station)
  time_sequence=get(paste0("line",simp_aaa[1],"_direction",ccc[sequence[1]]))[,station_col]
  depart_time=min(time_sequence[time_sequence>=eee+2])
  arrive_time=depart_time+6*n_stop[1]
  storied_values=c(depart_time,arrive_time)
  if(length(simp_aaa)>=2){
    for(i in 2:length(simp_aaa)){
      #consider the next line
      next_station=bbb[sequence[i]]
      next_station_col=which(get(paste0("line",simp_aaa[i]))==next_station)
      next_time_sequence=get(paste0("line",simp_aaa[i],"_direction",ccc[cumsum(n_stop)[i]]))[,next_station_col]
      next_depart_time=min(next_time_sequence[next_time_sequence>=arrive_time+2])
      next_arrive_time=next_depart_time+6*n_stop[i]
      storied_values=c(storied_values,next_depart_time,next_arrive_time)
      #renew names
      start_station=next_station
      station_col=next_station_col
      time_sequence=next_time_sequence
      arrive_time=next_arrive_time}}
  result <- c()
  for (i in 1:length(unlist(rle(aaa)[1]))) {
    group_start <- (i - 1) * 2 + 1
    group_end <- group_start + 1
    group <- storied_values[group_start:group_end]
    new_group <- seq(group[1], group[2], length.out = unlist(rle(aaa)[1])[i] + 1)
    result <- c(result, new_group)
  }
  return(list(result,1+unlist(rle(aaa)[1]),ddd,unlist(rle(aaa)[1]),bbb))}

#this function is to find trajectory a passenger at according to 
#output information of the function "simu"
find_trajectory=function(time,num_passenger) {
  # a function to return trajectory given time
  # initialize group and rip ID
  group_idx= 1
  gap_idx=1
  subgroups=split_AAA
  #loop over subgroup
  for (group in subgroups) {
    if (time >= group[1] & time <= tail(group, n=1)) {
      prev_element=group[1]
      #loop over elements in the group
      for (element in group[-1]) {
        if (time >= prev_element & time < element) {
          return(split_DDD[[group_idx]][gap_idx])
        }
        prev_element=element
        gap_idx=gap_idx + 1
      }
      #if failed to find rip, return 0
      return(0)
    }
    group_idx=group_idx + 1
    gap_idx=1
  }
  #if failed to find, return 0
  return(0)
}

#or do the following step to fine source in github
library(devtools)
source_url("https://raw.githubusercontent.com/guanruhao/function-repo/main/function%20simu%20and%20find%20trajectory.R")



#build the output matrix and then export them to csv for further analysis if needed
output=matrix(0,nrow=n_passengers,ncol=length(600:2880))

#the for loop to give values on output matrix
for (i in 1:n_passengers){
  k=i
  aaa=simulation_data[k,]$long_line
  bbb=simulation_data[k,]$path_node
  ccc=direction[k]
  ddd=simulation_data[k,]$path_edge
  eee=simulation_data[k,]$time
  #this function is to calculate a passenger's position along time
  #given passenger necessary information.
  temp=simu(aaa,bbb,ccc,ddd,eee)
  AAA=as.vector(unlist(temp[1]))#AAA时间
  BBB=as.vector(unlist(temp[2]))#BBB跨度
  DDD=as.vector(unlist(temp[3]))#DDD轨道编号
  EEE=as.vector(unlist(temp[4]))#BBB-1
  #GROUP AAA and DDD according to BBB and EEE
  split_AAA=split(AAA, rep(1:length(BBB), BBB))
  split_DDD=split(DDD, rep(1:length(EEE), EEE))
  time_start=split_AAA[[1]][1]
  time_end=split_AAA[[length(split_AAA)]][length(split_AAA[[length(split_AAA)]])]
  sequence=c(time_start:time_end)
  for (j in 1:length(sequence)){
    output[i,sequence[j]-600]=find_trajectory(sequence[j],i)}
}


#reflect the daily usage \
#this is the directed data counts link usages
# for example 2->1 and 1->2 are different and their usage will be counted separately
total_usage=matrix(0,ncol=1,nrow=nrow(line_frame))
for (i in 1:nrow(line_frame)){
  total_usage[i,]=sum(apply(output==i,1,sum)>0)}
line_frame=cbind(line_frame,total_usage)
colnames(line_frame)=c("start","end","line_number","daily_usage")
directed_daily_usage=as.data.frame(line_frame)
#
#
#
#generate a new frame to show the link usage (un-directed)
#combine directed_daily_usage's usage to undirected link daily usage
#note that the generated new frame's 1st and 2nd column only represent
#the link between these two adjacent stations
library(dplyr)
undirected_daily_usage=directed_daily_usage %>% 
  group_by(Start = pmin(start, end), End = pmax(start, end)) %>% 
  summarise(Line = first(line_number), Usage = sum(daily_usage))

print(directed_daily_usage)
print(undirected_daily_usage)



#now build a big matrix to reflect the directed link usage along time
#this is to reflect how many peoples are at a particular path given moment
#path means directed
timed_usage=matrix(0,nrow=nrow(line_frame),ncol=ncol(output))

for(i in 1:ncol(output)) {
  timed_usage[, i]=apply(output[, i, drop=FALSE],2,function(x) table(factor(x, levels = 1:nrow(line_frame))))
}
# assign row and column names
timed_usage=as.data.frame(timed_usage)
rownames(timed_usage)=paste0("path_number",1:nrow(line_frame))
colnames(timed_usage)=paste0("time", 600+c(1:ncol(output)))
#we don't need to calculate timed undirected link usage as a link has two directed and independent trajectories
print(timed_usage)







#danger zone
#
#

#do this if need to export them to csv for further analysis.
write.csv(output,"C:/Users/10274/Desktop/output.csv")
#do this if this big matrix is occupying too much RAM
rm(output)
#
#
#
