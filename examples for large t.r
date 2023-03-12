#horce racing setting
observation10.1<-matrix(c(1,2,3,4,5,6,7,8,9,0, 
                          1,2,3,5,6,0,9,7,8,4,
                          2,1,4,5,0,3,8,7,6,9,
                          0,2,1,8,5,4,9,6,7,3),4,10,byrow=T)
observation10.2<-matrix(c(1,2,3,4,5,6,7,8,0,0,
                        1,2,3,5,6,0,0,7,8,4,
                        2,1,4,5,0,3,8,7,6,0,
                        0,2,1,8,5,4,0,6,7,3),4,10,byrow=T)
observation10.3<-matrix(c(1,2,3,4,5,6,7,0,0,0, 
                          1,2,3,0,6,0,5,7,8,4,
                          2,1,4,5,0,3,0,7,6,0,
                          0,2,1,6,5,4,0,0,7,3),4,10,byrow=T)
observation10.4<-matrix(c(1,2,3,4,5,6,0,0,0,0, 
                          1,2,3,0,0,0,5,6,0,4,
                          2,0,1,4,0,3,0,5,6,0,
                          0,2,1,6,5,4,0,0,0,3),4,10,byrow=T)
observation10.5<-matrix(c(1,2,3,4,5,0,0,0,0,0, 
                          1,2,0,0,0,0,4,5,0,3,
                          2,0,1,4,0,3,0,0,5,0,
                          1,2,0,0,5,4,0,0,0,3,
                          0,0,0,3,0,0,1,5,4,2),5,10,byrow=T)

#observation<-matrix(c(0,6,7,4,8,0,5,2,3,1)) #2 item missing different distribution
#observation<-matrix(c(0,2,1,0,5,4,0,6,7,3)) #3 item missing
#observation<-c(1,3,2,4,5,0,7,8,9,6)  #1 item missing

#generate complete ranking compatible with an uncomplete ranking
source("Compatible.R")
Compatible<-list()
for(i in 1:nrow(observation)){
  Compatible[[i]]<-Two_stage_complete(observation[i,])
}

#test if the complete set is compatible
#N<-sample(90,10)
for(i in N){
  print(rank(Compatible10.2[[1]][i,c(-9,-10)]))
} 

#save generated complete set
write.table(Compatible,file="horse ranking compatible set.txt",row.names=F,col.names=F)
a<-read.table("horse ranking compatible set.txt")
