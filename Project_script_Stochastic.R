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
selling_effect<-data.frame(matrix(ncol = 0,nrow=7))
colnames(buffer) <- c("time", "S","I","R","D")
pop_tot<-60000000
delta_inf<-0
#k<-c(1.5,1.5,1.5)
k<-c(1,1,1)
#rate_infection_threshold<-0.000001
rate_infection_threshold<-0.000007
score<-0
p_vector<-numeric(400)
R_t<-numeric(52)


#DID_selling_matrix <- read_excel("Risultati/DID_selling_matrix.xlsx")
#as.data.frame(DID_selling_matrix)
#save(DID_selling_matrix,file="DID_selling_matrix.rda")
load("DID_selling_matrix.rda")

#DID_selling_matrix_error <- read_excel("Risultati/DID_selling_matrix_error.xlsx")
#as.data.frame(DID_selling_matrix_error)
#save(DID_selling_matrix_error,file="DID_selling_matrix_error.rda")
load("DID_selling_matrix_error.rda")


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
  dIdt <- beta*S*I/N-gamma*I-delta*I
  dRdt <- gamma*I
  dDdt <- delta*I
  ## combine results into a single vector
  dxdt <- c(dSdt,dIdt,dRdt,dDdt)
  ## return result as a list!
  list(dxdt)
  #  L <- ifelse(I=> 10000, 0.5, 1)

}
parms <- c(beta=0.5,gamma=0.14,delta=0.01)
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


if(buffer[(i+1)*7,]$I/pop_tot > rate_infection_threshold && p_vector[i+1] >= 0.5*k[1]) {
  
  parms <- c(beta=0.5*0.7,gamma=0.14,delta=0.01)
  #  print(buffer[(i+1)*7,]$time) 
  #  print(buffer[(i+1)*7,]$I)
#  print("-LOW")
}

if(buffer[(i+1)*7,]$I/pop_tot > rate_infection_threshold*10 & p_vector[i+1] >= 0.5*k[2]) {
  
  parms <- c(beta=0.5*0.25,gamma=0.14,delta=0.01)
#  print(buffer[(i+1)*7,]$time) 
#  print(buffer[(i+1)*7,]$I)
#  print("-MEDIUM")
  
  
}

if(buffer[(i+1)*7,]$I/pop_tot > rate_infection_threshold*50 & p_vector[i+1] >= 0.5*k[3]) {
  
  parms <- c(beta=0.5*0.025,gamma=0.14,delta=0.01)
#  print(buffer[(i+1)*7,]$time)
#  print(buffer[(i+1)*7,]$I)
  
#  print("HIGH")
  
  
}

if(buffer[(i+1)*7,]$I/pop_tot < rate_infection_threshold*0.5 & p_vector[i+1] >= 0.5*k[1] ) {
  
  parms <- c(beta=0.5,gamma=0.14,delta=0.01)
  
}

if(parms == c(beta=0.5)) {
   no_lockdown_weeks <- no_lockdown_weeks +1
   R_t[i]=0.4/0.14
   print("NO RESTRICTION")
}

  
if(parms == c(beta=0.5*0.7)) {
    soft_lockdown_weeks <- soft_lockdown_weeks +1
    R_t[i]=0.4*0.5/0.14
    print("SOFT")
  }

if(parms == c(beta=0.5*0.25)) {
  medium_lockdown_weeks <- medium_lockdown_weeks +1
    R_t[i]=0.4*0.1/0.14
    print("MEDIUM")
}

if(parms == c(beta=0.5*0.025)) {
  R_t[i]=0.4*0.05/0.14
  strong_lockdown_weeks <- strong_lockdown_weeks +1
  print("HIGH")
}


if(parms != c(beta=0.5)) {
  lockdown_weeks <- lockdown_weeks +1
}

if (i>0) {
delta_inf<-buffer[(i+1)*7,]$I-buffer[(i)*7,]$I

}


}

soft_lockdown_weeks
medium_lockdown_weeks
strong_lockdown_weeks
lockdown_weeks
buffer$D[370]


score <- soft_lockdown_weeks+(1/7)+medium_lockdown_weeks*(1/0.25)+strong_lockdown_weeks*(1/0.025)
score

selling_effect$Type <- DID_selling_matrix$Type
selling_effect$Effect <- DID_selling_matrix$High*strong_lockdown_weeks+DID_selling_matrix$Medium*medium_lockdown_weeks
selling_effect$Effect_error <- DID_selling_matrix_error$High*strong_lockdown_weeks+DID_selling_matrix_error$Medium*medium_lockdown_weeks+DID_selling_matrix_error$Low*soft_lockdown_weeks
selling_effect$P_value <- 2 * pt(abs(selling_effect$Effect/selling_effect$Effect_error),64,lower.tail = FALSE)
selling_effect$High <- strong_lockdown_weeks
selling_effect$Medium <- medium_lockdown_weeks
selling_effect$Low <- soft_lockdown_weeks


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


