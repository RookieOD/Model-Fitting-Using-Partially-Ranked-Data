


  continue=TRUE
  counter=1
  n<-nrow(observation)
  t<-ncol(observation)
  K<-length(initial$alpha)
  K<-2
  ki<-numeric(n)
  ni<-numeric(n)
  #basic computation
  C<-list()
  for (i in 1:n){
    ki[i]<-sum(observation[i,]!=0)
    ni[i]<- factorial(t)/factorial(ki[i])
    C[[i]]<-Compatible[[i]]
  }
  # initialization
  mu<-list()
  kappa<-list()
  alpha<-list()
  for (k in 1:K){
    alpha[[k]]<-initial$alpha[k]
    mu[[k]]<-initial$mu[k,]
    kappa[[k]]<-initial$kappa[k]
  }
  
  mu.old<-mu
  kappa.old<-kappa
  alpha.old<-alpha
  
  
  # compute the parameters in the first iteration
  # population 1
  sum0<-list()
  pi<-list()
  s<-list()
  p<-list()
  r0<-list()
  r0.norm<-list()
  r0.tilde<-list()
  for(k in 1:K){
    sum0[[k]]<-matrix(0,n,t,byrow=T)
    pi[[k]]<-numeric(n)
    for (i in 1:n){
      s[[k]]<-matrix(0,ni[i],t)
      p[[k]]<-numeric(ni[i])
      for (j in 1:ni[i]){
        p[[k]][j]<-exp(kappa.old[[k]]*t(mu.old[[k]])%*%C[[i]][j,])
        s[[k]][j,]<-C[[i]][j,]*p[[k]][j]*alpha.old[[k]]
      }
      sum0[[k]][i,]<-apply(s[[k]],2,sum)
      pi[[k]][i]<-sum(p[[k]])*alpha.old[[k]]
    }
    r0[[k]]<-apply(sum0[[k]],2,sum)
    r0.norm[[k]]<-sqrt(sum(r0[[k]]^2))
    r0.tilde[[k]]<-r0.norm[[k]]/sum(pi[[k]])
    mu[[k]]<-r0[[k]]/r0.norm[[k]]
    d<-t-1
    if(r0.tilde[[k]]<0.5){
      kappa[[k]]<-d*r0.tilde[[k]]*(1+d/(d+2)*r0.tilde[[k]]^2+d^2*(d+8)/(d+2)^2/(d+4)*r0.tilde[[k]]^4)
    } else if(r0.tilde[[k]]>=0.5){kappa[[k]]<-(d-1)/(2*(1-r0.tilde[[k]]))}
  }
  
    z<-list()
    z[[1]]<-list()
    gamma<-list()
    gamma[[1]]<-numeric(n)
    for(i in 1:n){
      z[[1]][[i]]<-numeric(ni[i])
      for (j in 1:ni[i]){
      z[[1]][[i]][j]<-exp(kappa.old[[1]]*t(mu.old[[1]])%*%C[[i]][j,])*alpha.old[[1]]
      }
    gamma[[1]][i]<-sum(z[[1]][[i]])
    }
    z[[2]]<-list()
    gamma[[2]]<-numeric(n)
    for(i in 1:n){
      z[[2]][[i]]<-numeric(ni[i])
      for (j in 1:ni[i]){
        z[[2]][[i]][j]<-exp(kappa.old[[2]]*t(mu.old[[2]])%*%C[[i]][j,])*alpha.old[[2]]
      }
      gamma[[2]][i]<-sum(z[[2]][[i]])
    }
    gamma.sum<-numeric(n)
    for(i in 1:n){
      gamma.sum[i]<-gamma[[1]][i]+gamma[[2]][i]
    }
    gamma.z<-list()
    for(k in 1:K){
      gamma.z[[k]]<-numeric(n)
      for(i in 1:n){
        gamma.z[[k]][i]<-gamma[[k]][i]/gamma.sum[i]
      }
    }
    for(k in 1:K){
      alpha[[k]]<-sum(gamma.z[[k]])/n
    }
    
  
  mu.old<-mu
  kappa.old<-kappa
  alpha.old<-alpha
  
  # update (p+1) iteration using information from (p) iteration
  while((counter<maxit)&&(continue==TRUE)){
  sum0<-list()
  pi<-list()
  s<-list()
  p<-list()
  r0<-list()
  r0.norm<-list()
  r0.tilde<-list()
  for(k in 1:K){
    sum0[[k]]<-matrix(0,n,t,byrow=T)
    pi[[k]]<-numeric(n)
    for (i in 1:n){
      s[[k]]<-matrix(0,ni[i],t)
      p[[k]]<-numeric(ni[i])
      for (j in 1:ni[i]){
        p[[k]][j]<-exp(kappa.old[[k]]*t(mu.old[[k]])%*%C[[i]][j,])
        s[[k]][j,]<-C[[i]][j,]*p[[k]][j]*alpha.old[[k]]
      }
      sum0[[k]][i,]<-apply(s[[k]],2,sum)
      pi[[k]][i]<-sum(p[[k]])*alpha.old[[k]]
    }
    r0[[k]]<-apply(sum0[[k]],2,sum)
    r0.norm[[k]]<-sqrt(sum(r0[[k]]^2))
    r0.tilde[[k]]<-r0.norm[[k]]/sum(pi[[k]])
    mu[[k]]<-r0[[k]]/r0.norm[[k]]
    d<-t-1
    if(r0.tilde[[k]]<0.5){
      kappa[[k]]<-d*r0.tilde[[k]]*(1+d/(d+2)*r0.tilde[[k]]^2+d^2*(d+8)/(d+2)^2/(d+4)*r0.tilde[[k]]^4)
    } else if(r0.tilde[[k]]>=0.5){kappa[[k]]<-(d-1)/(2*(1-r0.tilde[[k]]))}
  }
  
  z<-list()
  z[[1]]<-list()
  gamma<-list()
  gamma[[1]]<-numeric(n)
  for(i in 1:n){
    z[[1]][[i]]<-numeric(ni[i])
    for (j in 1:ni[i]){
      z[[1]][[i]][j]<-exp(kappa.old[[1]]*t(mu.old[[1]])%*%C[[i]][j,])*alpha.old[[1]]
    }
    gamma[[1]][i]<-sum(z[[1]][[i]])
  }
  z[[2]]<-list()
  gamma[[2]]<-numeric(n)
  for(i in 1:n){
    z[[2]][[i]]<-numeric(ni[i])
    for (j in 1:ni[i]){
      z[[2]][[i]][j]<-exp(kappa.old[[2]]*t(mu.old[[2]])%*%C[[i]][j,])*alpha.old[[2]]
    }
    gamma[[2]][i]<-sum(z[[2]][[i]])
  }
  gamma.sum<-numeric(n)
  for(i in 1:n){
    gamma.sum[i]<-gamma[[1]][i]+gamma[[2]][i]
  }
  gamma.z<-list()
  for(k in 1:K){
    gamma.z[[k]]<-numeric(n)
    for(i in 1:n){
      gamma.z[[k]][i]<-gamma[[k]][i]/gamma.sum[i]
    }
  }
  for(k in 1:K){
    alpha[[k]]<-sum(gamma.z[[k]])/n
  }
  # stop criterion
  diff<-numeric(K)
  for(k in 1:K){
    diff[k]<-sum((mu[[k]]-mu.old[[k]])^2)+sum((kappa[[k]]-kappa.old[[k]])^2)+sum((alpha[[k]]-alpha.old[[k]])^2)
  }
  if (sum(diff)<epsilon){
    continue=FALSE
  }
  mu.old<-mu
  kappa.old<-kappa
  alpha.old<-alpha
  counter<-counter+1
  }

  
  

 
  

   

  # 
  # sum0.2<-matrix(0,n,t,byrow=T)
  # pi<-numeric(n)
  # for (i in 1:n){
  #   s<-matrix(0,ni[i],t)
  #   p<-numeric(ni[i])
  #   for (j in 1:ni[i]){
  #     p[j]<-exp(kappa[[2]]*t(mu[[2]])%*%C[[i]][j,])
  #     s[j,]<-C[[i]][j,]*p[j]*alpha[[2]]
  #   }
  #   sum0.2[i,]<-apply(s,2,sum)
  #   pi[i]<-sum(p)*alpha[[2]]
  # }
  # r0.2<-apply(sum0.2,2,sum)
  # r0.2.norm<-sqrt(sum(r0.2^2))
  # r0.2.tilde<-r0.2.norm/sum(pi)
  # mu.2<-r0.2/r0.2.norm
  # d<-t-1
  # if(r0.2.tilde<0.5){
  #   kappa<-d*r0.2.tilde*(1+d/(d+2)*r0.2.tilde^2+d^2*(d+8)/(d+2)^2/(d+4)*r0.2.tilde^4)
  # } else if(r0.2.tilde>=0.5){kappa<-(d-1)/(2*(1-r0.2.tilde))}

  
  
observation<-matrix(c(1,2,3,0,
                      1,3,0,2,
                      3,2,0,1,
                      0,3,2,1),4,4,byrow=T)

# observation<-matrix(c(1,2,0,1,0,2),2,3,byrow=T)
# Compatible1<-matrix(c(1,2,3,1,3,2,2,3,1),3,3,byrow=T)
# Compatible2<-matrix(c(1,2,3,1,3,2,2,1,3),3,3,byrow=T)
# Compatible<-list(Compatible1,Compatible2)


initial=list(mu=matrix(c(1/sqrt(10),1/sqrt(10),1/sqrt(10),1/sqrt(10),1/sqrt(10),1/sqrt(10),1/sqrt(10),1/sqrt(10),1/sqrt(10),1/sqrt(10),
                         1/sqrt(2),1/4,1/4,1/4,1/4,1/sqrt(20),1/sqrt(20),1/sqrt(20),1/sqrt(20),1/sqrt(20)),2,byrow=T),kappa=c(-0.1,0.1),alpha=c(0.4,0.6))
EM.mix(observation,Compatible,initial,epsilon=0.000001,maxit=100)
