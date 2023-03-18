
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


output=matrix(0,nrow=10,ncol=length(600:2880))


for (i in 1:10){
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
    output[i,sequence[j]]=find_trajectory(sequence[j],i)}
}

write.csv(output,"C:/Users/10274/Desktop/output.csv")











