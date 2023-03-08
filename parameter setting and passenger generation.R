
# read the system
library(devtools)
SourceURL <- "https://raw.githubusercontent.com/guanruhao/changchun-city-subway/main/system%20set%20up.R"
source_url(SourceURL)
rm(SourceURL)

# 设置/回顾模拟参数 set up/review simulation parameters
n_lines  # number of lines
n_stations#number of station
n_trains <- 20  # 每条线路的列车数number of train per line
train_capacity <- 200  # 列车容量 train capacity
n_passengers <- 20000  # 要模拟的乘客总数 total number of passenger in simulation
max_time <- 2*60*24  #  simulation duration 30s*2(1min)*(60mins/h)*24h


graph=graph_from_adjacency_matrix(
  network,
  mode = c("directed"),
  weight=T,
  diag = TRUE,
  add.colnames = NULL,
  add.rownames = NA
)
line_info<-list(
line1,
line2,
line3,
line4
)
# Match the subway lines to the edges in the graph based on the nodes they connect

line_frame=cbind(get.edgelist(graph),numeric(length(E(graph)))) #for assigning line number later
for (i in 1:length(line_info)) {
  nodes <- line_info[[i]]
  edges <- which(t(apply(get.edgelist(graph), 1, function(x) all(x %in% nodes))))
  line_frame[edges,3]=i
}
rm(nodes,edges,i)


# 创建模拟数据框build simulation data frame
simulation_data <- data.frame(
  time = numeric(n_passengers), 
  line = character(n_passengers), 
  start_station = numeric(n_passengers),
  end_station = numeric(n_passengers),
  path_node=list(0),
  path_edge=list(0)
)
# 模拟生成乘客
for (i in 1:n_passengers) {
  # 随机选择起点和终点站 assign origin and destination
  OD<- sample(1:n_stations, 2)
  start_station=OD[1]
  end_station <- OD[2]
  # find the shortest path
  path=unlist(shortest_paths(graph, from = start_station, to=end_station, output = "both")$epath)
  path_v=unlist(shortest_paths(graph, from = start_station, to=end_station, output = "both")$vpath)
  aaa=rle(line_frame[path,3])
  # Extract only the values that appear once or that have a transition in their value
  taken_lines<- aaa$values[c(TRUE, aaa$lengths[-length(aaa$lengths)] > 1)]
  
  # 将乘客信息添加到模拟数据框中
  simulation_data$time[i] <-runif(1,600,2640)#from 5am to 10pm
  simulation_data$line[i] <- list(taken_lines)
  simulation_data$start_station[i] <- start_station
  simulation_data$end_station[i] <- end_station
  simulation_data$path_edge[i]<-list(path)
  simulation_data$path_node[i]<-list(path_v)
}
simulation_data$X0=NULL
simulation_data$X0.1=NULL
#sort the frame by time
simulation_data=simulation_data[order(simulation_data$time,decreasing = F),]
rm(start_station,end_station,i,OD,path,taken_lines,aaa,path_v)

















