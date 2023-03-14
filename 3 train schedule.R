

#load previous works
library(devtools)
source_url('https://raw.githubusercontent.com/guanruhao/changchun-city-subway/main/2%20parameter%20setting%20and%20passenger%20generation.R')
rm(n_trains)
#the simulation of schedule is from 4am up to 2am next day.
#it covers the time of passenger generation in order to make sure 
#all passengers can successfully reach their destinations.
# also this method can let me no need to do the train initialization work
#
#
#
#now assume at starting time 3am, all first trains depart at station number 1 16 5 4
#for line 1 2 3 4 respectively.it takes 3 minutes (6 time blocks)for a train from
#departure to departure at the next station (return back if reach the terminal).
#and assume the next trains will departs 9 minutes after than the previous trains
#until all trains are in the system. we should assume all trains are having the
#same speed
series_for_all<- seq(from = 360, to = 3120, by = 6)
length(series_for_all)
#number of trains for all 4 lines from 1 to 4
number_train=c(10,12,20,10)
options (warn = -1)
for(i in 1:n_lines){
  name_A=paste0("line", i)
  name_B=paste0("line",i,"_train","1")
  AAA=matrix(series_for_all,ncol=length(get(name_A)),byrow=T)
  AAA=AAA[-(nrow(AAA)),]
  assign(name_B,AAA)
  for (j in 1:number_train[i]-1) {
    # 计算新矩阵的元素getting elements for new matrix
    new_matrix <- get(name_B)+(18*j)
    # 为新矩阵命名 giving names to new matrix
    matrix_name <- paste0("line",i,"_train", j+1)
    # 将新矩阵存储为对象 assign matrix with a name
    assign(matrix_name, new_matrix)
  }
}
#remove useless names
rm(i,j,matrix_name,AAA)
options(warn = 1)

#
for(i in 1:n_lines){
  name1=paste0("line", i,"_direction1")
  name2=paste0("line", i,"_direction2")
  AAA=matrix(ncol=length(get(paste0("line",i))))
  BBB=matrix(ncol=length(get(paste0("line",i))))
  for (j in ls(pattern=paste0("line",i,"_train"))) {
    # get current matrix
    m <- get(j)
    # get row number
    nrows <- nrow(m)
    # extract odd rows to one frame
    odd_rows <- seq(1, nrows, by=2)
    odd_rows_data <- m[odd_rows, ]
    # combine the extracted data
    AAA <- rbind(AAA, odd_rows_data)
  }
  assign(name1,AAA)
  for (j in ls(pattern=paste0("line",i,"_train"))) {
    # get current matrix
    m <- get(j)
    # get row number
    nrows <- nrow(m)
    # extract even rows to one frame
    even_rows <- seq(2, nrows, by=2)
    even_rows_data <- m[even_rows, ]
    # combine extracted data
    BBB <- rbind(BBB, even_rows_data)
  }
  #delete the first empty rows
  AAA=AAA[-1,]
  BBB=BBB[-1,]
  #sort all columns independently and then conbine them
  AAA=cbind(apply(AAA, 2, sort))
  BBB=cbind(apply(BBB, 2, sort))
  assign(name1,AAA)
  assign(name2,BBB)
}
#columns of matrices represent station numbers of corresponding lines
#(1 16 5 4) to (15 32 59 71) from left to right columns respectively.
#but actually they are reversed for direction2 (15 32 59 71) to (1 16 5 4)
#we need to fix them
for(i in 1:n_lines){
  name=paste0("line",i,"_direction2")
  assign(name,get(name)[, ncol(get(name)):1])}
#till now, "line()_direction()"are time schedules for trains
#for any matrix "line" number means the number of line
#"direction" has 2 elements: 1 is from (1 16 5 4) to (15 32 59 71)
#2 means opposite directions
#columns of matrices represent station numbers of corresponding lines
#(1 16 5 4) to (15 32 59 71) from left to right columns respectively.


#remove useless names
rm(AAA,BBB,even_rows,even_rows_data,odd_rows,odd_rows_data,i,j,m,nrows,name_A
   ,name_B,name1,name2)










