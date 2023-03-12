observation<-matrix(c(1:10,rep(0,10)),1,20)


EM<-function(observation,Compatible,epsilon=0.000001,maxit=100)
{
  continue=TRUE
  counter=1
  n<-nrow(observation)
  t<-ncol(observation)
  k<-numeric(n)
  ni<-numeric(n)
  #first iteration
  sum0<-matrix(0,n,t,byrow=T)
  C<-list()
  for (i in 1:n){
    k[i]<-sum(observation[i,]!=0)
    ni[i]<- nrow(Compatible[[i]])
    C[[i]]<-Compatible[[i]]
    s<-numeric(t)
    for (j in 1:ni[i]){
      s<-s+C[[i]][j,]
    }
    sum0[i,]<-factorial(k[i])/factorial(t)*s
  }
  r0<-apply(sum0,2,sum)
  r0.norm<-sqrt(sum(r0^2))
  r0tilde<-r0.norm/n
  mu<-r0/r0.norm
  d<-t-1
  kappa<-r0tilde*(t-1-r0tilde^2)/(1-r0tilde^2)
  
  # update (p+1) iteration using information from (p) iteration
  sum1<-matrix(0,n,t,byrow=T)
  pi<-numeric(n)
  mu.new<-mu
  kappa.new<-kappa
  while((counter<maxit)&&(continue==TRUE)){
    for (i in 1:n){
      #s<-numeric(t)
      s<-matrix(0,ni[i],t)
      p<-numeric(ni[i])
      for (j in 1:ni[i]){
        p[j]<-exp(kappa*t(mu)%*%C[[i]][j,])
        #s<-s+C[[i]][j,]*p[j]
        s[j,]<-C[[i]][j,]*p[j]
      }
      #sum1[i,]<-s
      sum1[i,]<-apply(s,2,sum)
      pi[i]<-sum(p)
    }
    r<-apply(sum1,2,sum)
    r.norm<-sqrt(sum(r^2))
    r.tilde<-r.norm/sum(pi)
    mu<-r/r.norm
    kappa<-r.tilde*(t-1-r.tilde^2)/(1-r.tilde^2)
    diff<-sqrt(sum((mu.new-mu)^2))
    if (diff<epsilon){
      continue=FALSE
    }
    mu.new<-mu
    kappa.new<-kappa
    counter<-counter+1
  }
  return(list(kappa=kappa.new,mu=mu.new,iteration=counter))
}

N=10^7
#experiment 1
Compatible = list()
Compatible[[1]]<-matrix(rep(1:20,N),N,20,byrow=T)

#experiment 2
for(i in 1:N){
  Compatible[[1]][i,]<-sample(1:20)
}

t<-ncol(observation)
for(i in 1:nrow(observation)){
  Compatible[[i]]<-Compatible[[i]]/sqrt(t*(t+1)*(2*t+1)/6)
}
system.time(result<-EM(observation,Compatible,0.1,100))


