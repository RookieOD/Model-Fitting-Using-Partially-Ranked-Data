
library(gtools)

Com<-function(s,o){
  return(all(rank(o[which(o!=0)])==rank(s[which(o!=0)])))
}

#algorithm 1
Two_stage_complete<-function(observation){
  p2<-which(observation==0)
  p1<-c(1:length(observation))[-which(observation==0)]
  t<-length(observation)
  k<-length(p1)
  B<-combinations(t,k)
  l<-observation[-p2]
  r<-sort(l)
  p<-numeric(k)
  for (j in 1:k){
    p[j]<-which(l==r[j])
  }
  for(i in 1:nrow(B)){
    B[i,p]<-sort(B[i,])
  }
  C<-matrix(0,factorial(t)/factorial(k),t)
  for (i in 1:nrow(B)){
    E<-seq(t)
    for(l in B[i,]){E=E[-which(E==l)]}
    M<-permutations(length(p2),length(p2),E)
    for(j in 1:nrow(M)){
      C[(i-1)*factorial(t-k)+j,p1]<-B[i,]
      C[(i-1)*factorial(t-k)+j,p2]<-M[j,]
    }
  }
  return(C)
}

#algorithm 2
Complete_permutation<-function(observation){
  t<-length(observation)
  M<-permutations(t,t)
  C<-matrix(0,1,t)
  for (i in 1:nrow(M)){
    if(Com(M[i,],observation)){
      C<-rbind(C,M[i,])
    }
  }
  C<-C[-1,]
  return(C)
}

#algorithm 3
Complete_sampling<-function(observation){
  t<-length(observation)
  k<-sum(observation!=0)
  n<-factorial(t)/factorial(k)
  count<-1
  C<-matrix(observation,1,t,byrow=T)
  C[1,which(C[1,]==0)]=c((k+1):t)
  while(count<n){
    r<-sample(t)
    if(Com(r,observation))
      if(any(apply(C,1,function(row) all(row==r)))==0){
        C<-rbind(C,r)
        count<-count+1
      }
  }
  return(C)
}
#(30,25)
observation<-matrix(c(0,1:10,rep(0,4),11:20,25,24,23,21,22),1,30)
#(20,15)
observation<-matrix(c(0,1:5,rep(0,4),11,15,14,13,12,6:10),1,20)
#(10,5)
observation<-matrix(c(5,0,1:4,0,0,0,0),1,10)
#(10,6)
observation<-matrix(c(5,0,1:4,0,0,0,6),1,10)
#(10,7)
observation<-matrix(c(5,0,1:4,0,0,7,6),1,10)
#(10,8)
observation<-matrix(c(5,0,1:4,0,8,7,6),1,10)
#(10,9)
observation<-matrix(c(5,0,1:4,9,8,7,6),1,10)
#(5,2)
observation<-matrix(c(0,1,2,0,0),1,5)
#(5,3)
observation<-matrix(c(0,3,2,1,0),1,5)
#(4,2)
observation<-matrix(c(0,1,2,0),1,4)
#(4,3)
observation<-matrix(c(0,1:3),1,4)

random_sample<-function(t,k,n){
  observation<-matrix(0,n,t)
  for(i in 1:n){
    loc<-sample(t,k)
    observation[i,loc]<-sample(k)
    observation[i,-loc]<-0
  }
  return(observation)
}


observation<-random_sample(30,25,1)


#test 1
Compatible<-list()
time<-list()
for (i in 1:nrow(observation)){
    time[[i]]<-system.time(Compatible[[i]]<-Two_stage_complete(observation[i,]))
  }
avg_time<-(time[[1]][3]+time[[2]][3]+time[[3]][3]+
             time[[4]][3]+time[[5]][3]+time[[6]][3]+
             time[[7]][3]+time[[8]][3]+time[[9]][3]+time[[10]][3])/10
print(avg_time)

#test 2
Compatible<-list()
time<-list()
for (i in 1:nrow(observation)){
  time[[i]]<-system.time(Compatible[[i]]<-Complete_sampling(observation[i,]))
}
avg_time<-(time[[1]][3]+time[[2]][3]+time[[3]][3]+
             time[[4]][3]+time[[5]][3]+time[[6]][3]+
             time[[7]][3]+time[[8]][3]+time[[9]][3]+time[[10]][3])/10
print(avg_time)



#test 3
Compatible<-list()
time<-list()
for (i in 1:nrow(observation)){
  time[[i]]<-system.time(Compatible[[i]]<-Complete_permutation(observation[i,]))
}
avg_time<-(time[[1]][3]+time[[2]][3]+time[[3]][3]+
             time[[4]][3]+time[[5]][3]+time[[6]][3]+
             time[[7]][3]+time[[8]][3]+time[[9]][3]+time[[10]][3])/10
print(avg_time)




#Verify
N<-sample(90000,10)
for(i in N){
  print(rank(Compatible[[1]][i,-which(observation[1,]==0)]))
} 
