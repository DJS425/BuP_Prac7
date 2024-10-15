#Multi-species interactions
#The predator-prey Lotka-Volterra model

install.packages("deSolve")
library("deSolve")


# 1. Predator-Prey

# Implementation of the base Lotka-Volterra model

    #First we have to implement an R function representing the 
    #differential equation representing LOGISTIC growth.: 
    #dP/dt = r(1-P/K)*P

LG <- function(t,state,parameters){ ##logistic grown function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dP <- r*(1-P/K)*P ##this is our logistic equation governing the rate of change of P
    
    return(list(dP)) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}


    #Now modify this function to include two differential equations 
    #representing a predator-prey Lotka-Volterra model (the two equations 
    #must be included within the same function):

#dx/dt=αx-βxy
#dy/dt=δxy-γy 

#x=prey
#y=predator
#x(0)=initial population 
#y(0)=initial population
#α or a=growth rate of the prey population
#β or b=effect of predators on prey growth rate
#δ or s=effect of prey on predator growth rate
#γ or g=predator death rate

LV <- function(t,state,parameters){ ##LV function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dx <- a*x - b*x*y ##this is our prey LV 
    dy <- s*x*y - g*y ##this is our predator LV
    
    return(list(c(dx,dy))) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}



#The ode function is used to solve the predator-prey Lotka-Volterra model
#Now you fill in the following code


state <- c( x=10, y= 10 ) ## the initial population values
parameters <- c(a=0.1, b=0.02, s=0.02, g=0.4) ## the equation parameters
times <- seq(0,500, by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y=state, times = times, func = LV, parms = parameters)
out.df <- data.frame(out)


install.packages("ggplot2")
library("ggplot2")


ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

#This gives the changes in the two populations through time. 
#As we saw during the lecture, it is also useful to plot the changes 
#in populations through time in the phase space, i.e. to plot one predator 
#population as a function of the prey population. To do so, we can use the following code:



ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")

#Same thing put the parameters and initial conditions are different 

state <- c( x=30, y= 25 ) ## the initial population values
parameters <- c(a=0.2, b=0.03, s=0.05, g=0.2) ## the equation parameters
times <- seq(0,500, by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y=state, times = times, func = LV, parms = parameters)
out.df <- data.frame(out)


ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")


ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")



#Prey Growth Rate: Exponential vs Logistic 

#In the original Lotka-Volterra model, the prey growth rate is exponential in the absence of predator. Indeed, if y = 0, the equation governing prey growth becomes:
  
  #dx/dt=αx

#Let’s change the equation to include a logistic growth instead:
  
  #dx/dt=αx(1-x/K)


#LV with logistic growth



  LV.lg <- function(t,state,parameters){ ##LV function, that takes a set of parameter values, initial conditions and a time sequence
    with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
      
      dx <- a*x* (1-x/K) - b*x*y ##this is our prey LV 
      dy <- s*x*y - g*y ##this is our predator LV
      
      return(list(c(dx,dy))) ## return the rate of change - it needs to be a list
    }) # end with(as.list ...
  }


state <- c( x=10, y= 10 ) ## the initial population values
parameters <- c(a=0.1, b=0.02, s=0.02, g=0.4, K=30) ## the equation parameters
times <- seq(0,500, by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y=state, times = times, func = LV.lg, parms = parameters)
out.df <- data.frame(out)


ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")


ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")

#Both populations reached the K and plataued?


#Incorporating Functional Response 


#Functional Response: The rate at which predators can consume preys 

#Three Types of Functional Response: 
#Linear (type 1, caputured in the OG LV)

#A type II functional response can be modelled by the following equations:

#dx/dt=αx-βxy/(1+A*x)
#dy/dt=δxy/(1+A*x)-γy 

#A high value of A indicates poor hunting efficiency by the predator.
#plot a type II functional response as in the figure above) using the following code:

x <- seq(0,50,0.1)
A <- 1.0 ###test values here, 0.01 is high hunting effiency, 1 is low hunting efficiency 
y <- x/(1+A*x) 
ggplot()+
  geom_line(mapping=aes(x=x,y=x/(1+A*x)),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed")



#Include a type II functional response in your implementation of the Lotka-Volterra model in R


LV.fr <- function(t,state,parameters){ ##LV function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dx <- a*x - b*x*y / (1+A/x)  ##this is our prey LV 
    dy <- s*x*y / (1+A*x) - g*y ##this is our predator LV
    
    return(list(c(dx,dy))) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}


state <- c( x=10, y= 10 ) ## the initial population values
parameters <- c(a=0.1, b=0.02, s=0.02, g=0.4, A=0.02) ## the equation parameters
times <- seq(0,500, by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y=state, times = times, func = LV.fr, parms = parameters)
out.df <- data.frame(out)


ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")


ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,200) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")


# 3. LV with logistic growth and Functional Response

x <- seq(0,30,0.1)
A <- 0.01 ###test values here, 0.01 is high hunting effiency, 1 is low hunting efficiency 
y <- x/(1+A*x) 
ggplot()+
  geom_line(mapping=aes(x=x,y=x/(1+A*x)),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed")



LV.lg.fr <- function(t,state,parameters){ ##LV function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dx <- a*x*(1-x/K) - b*x*y / (1+A/x)  ##this is our prey LV 
    dy <- s*x*y / (1+A*x) - g*y ##this is our predator LV
    
    return(list(c(dx,dy))) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}


state <- c( x=10, y= 10 ) ## the initial population values
parameters <- c(a=0.1, b=0.02, s=0.02, g=0.4, A=0.01, K=30) ## the equation parameters
times <- seq(0,500, by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y=state, times = times, func = LV.lg.fr, parms = parameters)
out.df <- data.frame(out)


ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")


ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")


#How does it change the outputs for different values of A? Discuss.
  #It looks the same with different A values so I am confused


##############################################################################################

# 2. Competition



#A three-species competition Lotka-Volterra model: limiting similarity



#First implement 2 species using these equations and do it yourself:

#(dx_1)/dt=r_1 x_1 (1-x_1/K_1 -(α_12 x_2)/K_1 )
#(dx_2)/dt=r_2 x_2 (1-x_2/K_2 -(α_21 x_1)/K_2 )


parameters <- c(a12=1, a21=0.9, r=0.3, K = 100)
state <- c(x1=50, x2=10)


LV.ls <- function(t,state,parameters){ ##LV function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dx1 <- r*x1*(1-(x1+a12*x2)/K)  ##this is our prey LV 
    dx2 <- r*x2*(1-(x2+a21*x1)/K) ##this is our predator LV
    
    return(list(c(dx1,dx2))) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}


times <- seq(0,1000, by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y=state, times = times, func = LV.ls, parms = parameters)
out.df <- data.frame(out)


ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x1),color="blue") +
  geom_line(mapping=aes(x=time,y=x2),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")


ggplot(data = out.df)+
  geom_path(mapping=aes(x=x1,y=x2),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Species 1", y = "Species 2")

#So species 1 and 2 both slowly increase and then suddenly, Species 1 drastically declines while 
#Species 2 drastically increases


#Now implement 3 species using these equations and copy from document:



#The αij parameters will be determined by the relative niches of the species. 
#As seen during the lecture, if I have species 1 and 2 with a Gaussian niche, 
#α12 and α21 are computed as: 

#just look at document or refer back to population and ecology module


alpha.func <- function(mu1,sig1,mu2,sig2,K1,K2,start,end){ ##this is the function to compute the 
  #alpha coefficients from the mean and standard deviations of the Gaussian niches of the species 
  #and the start and end values of the environment
  niche1 <- K1 *  dnorm(seq(start,end,length.out=100),mean=mu1,sd=sig1) ##dnorm() generates the values of the Gaussian. Check ?dnorm
  niche2 <- K2 * dnorm(seq(start,end,length.out=100),mean=mu2,sd=sig2)
  a <- sum(niche1*niche2)/sum(niche1*niche1) ##because we have discrete values, we use a sum to approximate the integral
  return(a)
}

##Let's try different parameter values
D <- 5 ##distance between the niche optima
mu1 <- 10 ##niche optima of species 1
mu2 <- mu1+D ##niche optima of species 2
mu3 <- mu1+2*D ##niche optima of species 3
sig1 <- sig2 <- sig3 <- 10 ##all species niches have the same standard deviation for simplicity
K1 <- 200 ##carrying capacity species 1 and 3
K2 <- 250 ##carrying capacity species 2
start <- 0
end <- 30
a12 <- alpha.func(mu1,sig1,mu2,sig2,K1,K2,start,end)
a13 <- alpha.func(mu1,sig1,mu3,sig3,K1,K1,start,end)
a21 <- alpha.func(mu2,sig2,mu1,sig1,K2,K1,start,end)
a23 <- alpha.func(mu2,sig2,mu3,sig3,K2,K1,start,end)
a31 <- alpha.func(mu3,sig3,mu1,sig1,K1,K1,start,end)
a32 <- alpha.func(mu3,sig3,mu2,sig2,K1,K2,start,end)

##visualise the niches
resource <- seq(start,end,length.out=100)
niche1 <- dnorm(resource,mean=mu1,sd=sig1)*K1
niche2 <- dnorm(resource,mean=mu2,sd=sig2)*K2
niche3 <- dnorm(resource,mean=mu3,sd=sig3)*K1
ggplot()+
  geom_line(mapping=aes(x=resource,y=niche1),color="blue")+
  geom_line(mapping=aes(x=resource,y=niche2),color="red")+
  geom_line(mapping=aes(x=resource,y=niche3),color="darkgreen")


##setup and solve the system of differential equations
parameters <- c(a12=a12, a13=a13, a21=a21, a23=a23, a31=a31, a32=a32, r=0.3, K1 = K1, K2 = K2)
state <- c(X1=10, X2=10, X3=10)

LS2 <- function(t,state,parameters){
  with(as.list(c(state, parameters)),{
    # rate of change
    ##you need to complete this
    dX1 <- r*X1*(1-(X1+a12*X2+a13*X3)/K1)
    dX2 <- r*X2*(1-(X2+a21*X1+a23*X3)/K2)
    dX3 <- r*X3*(1-(X3+a31*X1+a32*X2)/K1)
    # return the rate of change
    list(c(dX1, dX2, dX3)) ##you need to complete this
  }) # end with(as.list ...
}

times <- seq(0,200,by=0.01)
out <- ode(y=state, times = times, func = LS2, parms = parameters)
out.df <- data.frame(out)

##plot the populations
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=X1),color="blue") +
  geom_line(mapping=aes(x=time,y=X2),color="red") +
  geom_line(mapping=aes(x=time,y=X3),color="darkgreen") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")












