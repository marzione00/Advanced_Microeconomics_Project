library(deSolve)
library(optimization)
library(tidyverse)



buffer<-data.frame(matrix(ncol = 5))
colnames(buffer) <- c("time", "S","I","R","D")


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
parms <- c(beta=0.4,gamma=0.12,delta=0.1)
xstart <- c(S=99999
            ,I=1,R=0,D=0)

for(i in 0:30) {
  


times <- seq(from=0+i*7,to=7+i*7,by=1)





ode(
  func=closed.sir.model,
  y=xstart,
  times=times,
  parms=parms
) %>%
  as.data.frame() -> out

buffer<-rbind(buffer,out)

#print(out$S)


xstart <- c(S=out$S[8],I=out$I[8],R=out$R[8],D=out$D[8])






if(buffer[(i+1)*7,]$I > 1000) {
  
  parms <- c(beta=0.4*0.05,gamma=0.12,delta=0.1)
  print(buffer[(i+1)*7,]$time)
  
}

if(buffer[(i+1)*7,]$I < 1000) {
  
  parms <- c(beta=0.4,gamma=0.12,delta=0.1)
  
}




}



buffer %>%
  gather(variable,value,-time) %>%
  ggplot(aes(x=time,y=value,color=variable))+
  geom_line(size=2)+
  theme_classic()+
  labs(x='time (days)',y='number of individuals')

