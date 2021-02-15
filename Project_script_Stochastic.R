library(deSolve)
library(optimization)
library(tidyverse)
library(MASS)
library(ggplot2)



buffer<-data.frame(matrix(ncol = 5))
soft_lockdown_weeks<- 0
medium_lockdown_weeks<- 0
strong_lockdown_weeks<- 0
no_lockdown_weeks<- 0
lockdown_weeks<-0
out<-data.frame(matrix(ncol = 5))
colnames(buffer) <- c("time", "S","I","R","D")
pop_tot<-60000000
delta_inf<-0
k<-c(1,1.2,1.8)
#k<-c(1,1,1)
#rate_infection_threshold<-0.000001
rate_infection_threshold<-0.0000005
score<-0
p_vector<-numeric(400)


#p_vector <- runif(400, 0.000, 1.000)
#save(p_vector,file="p_vector.rda")
load("p_vector.rda")

closed.sir.model <- function (t, x, params) {
  ## first extract the state variables
  S <- x[1]
  I <- x[2]
  R <- x[3]
  D <- x[4]
  ## now extract the parameters
  beta <- params["beta"]
  gamma <- params["gamma"]
  delta <- params["delta"]
  N <- S+I+R
  ## now code the model equations
  dSdt <- -beta*S*I/N
  dIdt <- beta*S*I/N-gamma*I
  dRdt <- gamma*I
  dDdt <- gamma*delta*I
  ## combine results into a single vector
  dxdt <- c(dSdt,dIdt,dRdt,dDdt)
  ## return result as a list!
  list(dxdt)
  #  L <- ifelse(I=> 10000, 0.5, 1)

}
parms <- c(beta=0.4,gamma=0.12,delta=0.03)
xstart <- c(S=pop_tot
            ,I=1,R=0,D=0)

for(i in 0:52) {
  


times <- seq(from=0+i*7,to=7+i*7,by=1)





ode(
  func=closed.sir.model,
  y=xstart,
  times=times,
  parms=parms
) %>%
  as.data.frame() -> out

peppo<-as.data.frame(t(as.matrix(data.frame(Reduce(rbind,out)))))

colnames(peppo) <- c("time", "S","I","R","D")

peppo<-peppo[-8,]

buffer<-rbind(buffer,peppo)

#print(out$S)


xstart <- c(S=out$S[8],I=out$I[8],R=out$R[8],D=out$D[8])


if(delta_inf/pop_tot > rate_infection_threshold & p_vector[i+1] >= 0.5*k[1]) {
  
  parms <- c(beta=0.4*0.5,gamma=0.12,delta=0.03)
  #  print(buffer[(i+1)*7,]$time) 
  #  print(buffer[(i+1)*7,]$I)
  print("-LOW")
}

if(delta_inf/pop_tot > rate_infection_threshold*5 & p_vector[i+1] >= 0.5*k[2]) {
  
  parms <- c(beta=0.4*0.1,gamma=0.12,delta=0.03)
#  print(buffer[(i+1)*7,]$time) 
#  print(buffer[(i+1)*7,]$I)
  print("-MEDIUM")
  
  
}

if(delta_inf/pop_tot > rate_infection_threshold*10 & p_vector[i+1] >= 0.5*k[3]) {
  
  parms <- c(beta=0.4*0.05,gamma=0.12,delta=0.03)
#  print(buffer[(i+1)*7,]$time)
#  print(buffer[(i+1)*7,]$I)
  
  print("HIGH")
  
  
}

if(abs(delta_inf/pop_tot) < rate_infection_threshold & p_vector[i+1] >= 0.5*k[1] ) {
  
  parms <- c(beta=0.4,gamma=0.12,delta=0.03)
  
  
}

if(parms == c(beta=0.4)) {
   no_lockdown_weeks <- no_lockdown_weeks +1
   print("NO RESTRICTION")
}

  
if(parms == c(beta=0.4*0.5)) {
    soft_lockdown_weeks <- soft_lockdown_weeks +1
    print("SOFT")
  }

if(parms == c(beta=0.4*0.1)) {
  medium_lockdown_weeks <- medium_lockdown_weeks +1
    print("MEDIUM")
}

if(parms == c(beta=0.4*0.05)) {
  
  strong_lockdown_weeks <- strong_lockdown_weeks +1
  print("HIGH")
}


if(parms != c(beta=0.4)) {
  lockdown_weeks <- lockdown_weeks +1
}

if (i>0) {
delta_inf<-buffer[(i+1)*7,]$I-buffer[(i)*7,]$I

}

#print(delta_inf)

}

soft_lockdown_weeks
medium_lockdown_weeks
strong_lockdown_weeks
lockdown_weeks
buffer$D[370]

score <- soft_lockdown_weeks+medium_lockdown_weeks*10+strong_lockdown_weeks*100
score


buffer %>%
  gather(variable,value,-time) %>%
  ggplot(aes(x=time,y=value,color=variable))+geom_line(size=2)+
   scale_y_log10() +
  labs(x='time (days)',y='Log number of individuals')


buffer %>%
  gather(variable,value,-time) %>%
  ggplot(aes(x=time,y=value,color=variable))+geom_line(size=2)+
#  scale_y_log10() +
  labs(x='time (days)',y='Log number of individuals')


