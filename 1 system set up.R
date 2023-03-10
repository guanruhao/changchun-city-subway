
# Import network
network<- scan(file="https://raw.githubusercontent.com/guanruhao/changchun-subway-admatrix-and-line-info/main/home%20network.csv",, what = "numeric", skip =1, sep = ',')
network=matrix(network,nrow=floor(sqrt(length(network))),byrow="T")
network=network[,-1]
library(igraph)
n_stations <- nrow(network)


# Specify paths from CSV file
station_seq <- read.csv("https://raw.githubusercontent.com/guanruhao/changchun-subway-admatrix-and-line-info/main/line%20number.csv",header = F)
n_lines <- ncol(station_seq)
station_seq=matrix(station_seq,nrow=n_lines,byrow="T")
line_list= as.list(station_seq)
for (i in 1:length(line_list)) {
  assign(paste("line", i, sep=""), as.numeric(line_list[[i]][1:length(na.omit(line_list[[i]]))]))
}

#build transfer matrix
transfer_matrix <- matrix(0, ncol = n_lines, nrow = n_lines)
for (i in 1:n_lines) {
  for (j in 1:n_lines) {
    if (length(intersect(na.omit(station_seq[[i]]), na.omit(station_seq[[j]])))> 0) {
      transfer_matrix[i, j] <- 1
      transfer_matrix[j, i] <- 1
    }
  }
}
diag(transfer_matrix)=c(rep(0,n_lines))

# build a subway system
subway_system <- list(adj_matrix = network, 
                      transfer_matrix = transfer_matrix, 
                      station_seq = station_seq)
rm(i,j)
print(subway_system)






