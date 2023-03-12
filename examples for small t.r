#scientific contest setting
observation3<-matrix(c(1,2,0,1,0,2,0,1,2),3,3,byrow=T)
observation4.1<-matrix(c(1,2,0,3,
                       1,3,2,0,
                       1,0,3,2,
                       0,1,2,3),4,4,byrow=T)
observation4.2<-matrix(c(1,2,0,3,
                         1,3,2,0,
                         1,0,3,2,
                         0,1,2,3,
                         0,1,2,0),5,4,byrow=T)
observation5.1<-matrix(c(1,2,3,4,0,
                       1,3,4,0,2,
                       2,1,4,0,3,
                       1,3,2,4,0
                       ),4,5,byrow=T)
observation5.2<-matrix(c(1,2,3,0,0,
                         1,3,0,0,2,
                         2,1,0,0,3,
                         0,2,1,3,0),4,5,byrow=T)
observation5.3<-matrix(c(1,2,3,4,0,
                         1,3,2,0,0,
                         4,2,0,3,1,
                         0,0,3,2,1),4,5,byrow=T)
observation6<-matrix(c(1,2,3,0,0,0,
                       1,3,0,2,0,0,
                       0,2,0,3,1,0,
                       1,0,2,0,0,3,
                       0,0,3,0,1,2),5,6,byrow=T)



#generate complete ranking compatible with an uncomplete ranking
source("Compatible.R")
Compatible<-list()
for(i in 1:nrow(observation)){
  Compatible[[i]]<-Two_stage_complete(observation[i,])
}
