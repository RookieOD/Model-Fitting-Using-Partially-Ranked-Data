#Algorithm 1
#generate complete compatible ranking set by two stages
#part1 enumerate the cases  for the fiexed ranks
#part2 fill in the rest randomly

library(gtools)

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
      #C[((i-1)*factorial(t-k)+1):(i*factorial(t-k)),p2]<-M
      C[(i-1)*factorial(t-k)+j,p2]<-M[j,]
    }
  }
  return(C)
}


#Algorithm 2
#generate complete ranking compatible with an uncomplete ranking via permutation

library(gtools)

Com<-function(s,o){
  return(all(rank(o[which(o!=0)])==rank(s[which(o!=0)])))
}

Complete<-function(observation){
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

#Algorithm 3
#generate complete ranking compatible with an uncomplete ranking via sampling
Complete<-function(observation){
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


