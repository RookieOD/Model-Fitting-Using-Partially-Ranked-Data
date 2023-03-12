

prob_complete<-function(v, mu, kappa){
  t<-length(v)
  Ct<-kappa^(t/2-3/2)/(2^(t/2-3/2)*factorial(t)*besselI(kappa,t/2-3/2)*gamma(t/2-1/2))
  p<-Ct*exp(kappa*(t(mu)%*%v))
  return(p)
}

prob_incomplete<-function(observation,compatible,mu,kappa){
  pj<-numeric(nrow(compatible))
  for(j in 1:nrow(compatible)){
    pj[j]<-prob_complete(compatible[j,],mu,kappa)
  }
  p<-mean(pj)
  return(p)
}

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
    ni[i]<- factorial(t)/factorial(k[i])
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
  if(r0tilde<0.5){
    kappa<-d*r0tilde*(1+d/(d+2)*r0tilde^2+d^2*(d+8)/(d+2)^2/(d+4)*r0tilde^4)
  } else if(r0tilde>=0.5){kappa<-(d-1)/(2*(1-r0tilde))}
  #kappa<-r0tilde*(t-1-r0tilde^2)/(1-r0tilde^2)
  
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
    #d<-t-1
    #if(r.tilde<0.5){
    #  kappa<-d*r.tilde*(1+d/(d+2)*r.tilde^2+d^2*(d+8)/(d+2)^2/(d+4)*r.tilde^4)
    #}
    #else if(r.tilde>=0.5){
    #  kappa<-(d-1)/(2*(1-r.tilde))
    #}
    #diff<-sum((mu.new-mu)^2)+(kappa.new-kappa)^2
    diff<-sqrt(sum((mu.new-mu)^2))
    if (diff<epsilon){
      continue=FALSE
    }
    mu.new<-mu
    kappa.new<-kappa
    counter<-counter+1
  }
  prob<-prob_complete(mu.new,mu.new,kappa.new)
  return(list(kappa=kappa.new,mu=mu.new,prob=prob,iteration=counter))
}
