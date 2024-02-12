
# enter Nid as positive 
# when lower are better outcomes, I use subtract from 1

nid=function(u0,u1,  n,   alpha,  NId=0, nSims=100000, higher="higher better outcomes")  #  
{
  
  if (higher!="higher better outcomes") {u1=1-u1; u0=1-u0}
 
  sigma0=sqrt(u0*(1-u0))  # se
  
  sigma1=sqrt(u1*(1-u1))  # se
  
  aveN=0; power=0; u1=u1+NId   # non inf.   
  
  for (i in 1:nSims) {
    
    y0=rnorm(1, u0, sigma0/sqrt(n))  # ref
    
    y1=rnorm(1, u1, sigma1/sqrt(n))  # test
    
    z1=(y1-y0)/sqrt(sigma1^2/n+sigma0^2/n)  # note direction
    
    t1=1-pnorm(z1)  # note one sided
    
    if(t1<=alpha){power=power+1/nSims}
    
  }
  
  return (c( "power=", power ))
}


##fda duplication Kcentra!!

# fda 
n=83
nid(u0=.85, u1=.90,   # u0 is control u1 new treatment
    n=n , alpha=0.025, 
    NId=0.10)  #  "0.83"

# check type I error
nid(u0=.9, u1=.9+.1,  # u0 is control u1 new treatment
    n=n , alpha=0.025,
    NId=-0.10)  #  "0.025

n=1375
nid(u0=.3, u1=.35,  higher="lower better outcomes",  # u0 is control u1 new treatment
    n=n , alpha=0.025,
    NId=0.10)  #  "0.80


n=13
nid(u0=.7, u1=.90,   # u0 is control u1 new treatment
     n=n , alpha=0.15, higher="higher better outcomes",
     NId=0.15)  #  90%

# checking to PASS
n=223
nid(u0=.7, u1=.7,   # u0 is control u1 new treatment
     n=n , alpha=0.15, higher="lower better outcomes",
     NId=0.1)  #  90%


# fda 
n=83
nid(u0=.85, u1=.90,   # u0 is control u1 new treatment
     n=n , alpha=0.025, higher="higher better outcomes",
     NId=0.10)  #  "0.832869999998844"

# check type I error
nid(u0=.85, u1=.85-.1,   # u0 is control u1 new treatment
     n=n , alpha=0.025, higher="higher better outcomes",
     NId=0.10)  #  

# check type I error when lower is better
nid(u0=.3, u1=.3+.1,   # u0 is control u1 new treatment
     n=n , alpha=0.025, higher="no",
     NId=0.10) 


#https://search.r-project.org/CRAN/refmans/epiR/html/epi.ssninfb.html
# duplicate some example at link above
## EXAMPLE 2:
## Assume the true mean cure rate for a treatment group to be 0.40 and the true
## mean cure rate for a control group to be the same, 0.40. We consider a 
## difference of 0.10 in cured proportions (i.e., delta = 0.10) to be of no 
## clinical importance.

## Assuming a one-sided test size of 5% and a power of 30% how many 
## subjects should be included in the trial?

n <- epi.ssninfb(treat = 0.4, control = 0.4, delta = 0.10, n = NA, power = 0.3, 
                 r = 1, nfractional = TRUE, alpha = 0.05)$n.total
n

n=60 # number per arm
nid(u0=.4, u1=.4,   # u0 is control u1 new treatment
    n=n , alpha=0.05, 
    NId=0.10)  


## A total of 120 subjects need to be enrolled in the trial, 60 in the 
## treatment group and 60 in the control group.

## Re-run the function using n = 120 to confirm that power equals 0.30:

epi.ssninfb(treat = 0.4, control = 0.4, delta = 0.10, n = 120, power = NA, 
            r = 1, nfractional = TRUE, alpha = 0.05)$power

## With 120 subjects the estimated study power is 0.30.




#------------------------------------------------

# Grifols Fiona
# 
n=139
nid(u0=.724, u1=.724,
    n=n , alpha=0.025,
    NId=0.15)

# this is my recommendation 80% power!!!!!!!!!!!!!!!!!!!!!!!!
n=98
nid(u0=.724, u1=.75, # u1 is better than above  so less patients needed
    n=n , alpha=0.025,
    NId=0.15)

# this is my recommendation 90% power!!!!!!!!!!!!!!!!!!!!!!!!
n=132
nid(u0=.724, u1=.75,
    n=n , alpha=0.025,
    NId=0.15)

#  here we postualte a wonderful new treatment

n=21
nid(u0=.7, u1=.9,
    n=n , alpha=0.05,
    NId=0.15)


#-------------------------------------------------------------------------
# verification of my recommendation for 80% power, more volatile simulation results 

n=98
NId=0.15
u1=.75
u0=.724
s1 <- sqrt(u0*(1-u0))
s2 <- sqrt(u1*(1-u1))

# 2 sided 0.05 we match
mean(replicate(999, 
               t.test( rnorm( n,   u0,       s1  ) ,
                       rnorm( n,   u1+.15,   s2) )$p.value<0.05  
               
))

#-------------------------------------------------

# a simulation in which lower outcomes are better


n=1375
nid(u0=.3, u1=.35,  higher="lower better outcomes",  # u0 is control u1 new treatment
    n=n , alpha=0.025,
    NId=0.10)  #  "0.80

 
NId=0.1
u1=.35
u0=.3
s1 <- sqrt(u0*(1-u0))
s2 <- sqrt(u1*(1-u1))

# 2 sided 0.05 we match
# note the subtraction from 1 , I flip the proportion
mean(replicate(999, 
               t.test( rnorm( n,   1-u0,       s1  ) ,
                       rnorm( n,   1-u1+NId,   s2) )$p.value<0.05  
               
))

#-------------------------------------------------Alex ?

# n=149
# diff=-0.20072
# alpha_adj_interim=0.002970073
# sims=5000
# pval <- replicate(sims, 
#                   t.test(rnorm(n, 2.5-.25 + diff,   1.05), 
#                               rnorm(n, 2.5,              1.05),
#                               alternative = "less", correct=FALSE)$p.value)
# 
#  
# #---------------------------
# 
# (pow <- sum(pval < alpha_adj_interim)/sims)


## simplyfy so the function outputs numeric power only to help plot

nidx=function(u0,u1,  n,   alpha,  NId=0, nSims=100000, higher="higher better outcomes")  #  
{
  
  if (higher!="higher better outcomes") {u1=1-u1; u0=1-u0}
  
  sigma0=sqrt(u0*(1-u0))  # se
  
  sigma1=sqrt(u1*(1-u1))  # se
  
  aveN=0; power=0; u1=u1+NId   # non inf.   
  
  for (i in 1:nSims) {
    
    y0=rnorm(1, u0, sigma0/sqrt(n))  # ref
    
    y1=rnorm(1, u1, sigma1/sqrt(n))  # test
    
    z1=(y1-y0)/sqrt(sigma1^2/n+sigma0^2/n)  # note direction
    
    t1=1-pnorm(z1)  # note one sided
    
    if(t1<=alpha){power=power+1/nSims}
    
  }
  
  return (   power )
}

require(tidyverse) # for expand grid

df_plot1 <- expand_grid(u0=0.3,
            u1=0.35,
            n=c(150,200,250),
            delta=seq(0,0.1, by=0.02)) 



df_plot1 <- expand_grid(u0=0.75,
                        u1=c(0.75),
                        n=c(150,200,250),
                        delta=seq(0,0.1, by=0.02)) 

df_plot1 <- expand_grid(u0=0.75,
                        u1=c(0.85),
                        n=c(150,200,250),
                        delta=seq(0,0.1, by=0.02)) 


df_plot1$pwr <- NA
  
for(i in 1 : dim(df_plot1)[1]) {
  
  df_plot1$pwr[i] <- 
    nidx (df_plot1$u0[i], 
          df_plot1$u1[i],  
          df_plot1$n[i],   
          alpha=0.025,  
          NId=df_plot1$delta[i], 
          nSims=100000, higher="higher better outcomes")
  
  
}


library(ggplot2)

ggplot(data=df_plot1, aes(x=delta, y=pwr, colour=factor(n  ))) +
  geom_line()+
  labs(x=expression("Non-inferiority margin " * Delta),
       y= "Power", colour ="Sample size/group") +
  theme_bw() +
  theme(legend.position = "bottom")


# another plot


df_plot1 <- expand_grid(u0=0.75,
                        u1=c(0.85, .9),
                        n=c(seq(50,100, by= 10)),
                        delta=seq(0, 0.1, by=0.05)) 


df_plot1$pwr <- NA

for(i in 1 : dim(df_plot1)[1]) {
  
  df_plot1$pwr[i] <- 
    nidx (df_plot1$u0[i], 
          df_plot1$u1[i],  
          df_plot1$n[i],   
          alpha=0.025,  
          NId=df_plot1$delta[i], 
          nSims=100000, higher="higher better outcomes")
  
  
}


library(ggplot2)

ggplot(data=df_plot1, aes(x=n, y=pwr)) +
  geom_line()+
  labs(x=expression("Non-inferiority margin " * Delta),
       y= "Power", colour ="Sample size/group") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~delta+u1)



#----------------------------------------------------------------------------------------

n=98
NId=0.15
u1=.75
u0=.724
s1 <- sqrt(u0*(1-u0))
s2 <- sqrt(u1*(1-u1))

# 2 sided 0.05 we match
mean(replicate(999, 
               t.test( rnorm( n,   u0,       s1  ) ,
                       rnorm( n,   u1+.15,   s2) )$p.value<0.05  
               
))




#------------------------------------------



nSims=1e6
y0 <- rnorm(nSims, u0, sqrt(u0*(1-u0))/sqrt(n))  # soc
y1 <- rnorm(nSims, u1, sqrt(u1*(1-u1))/sqrt(n))  # new trt 
par(mfrow=(c(2,2)))
hist(y0, breaks=200, xlim = c(.5,1))
hist(y1-y0+0, breaks=200, xlim= c(-.4,.4))
hist(y1, breaks=200, xlim = c(.5,1 ))

hist(y1-y0+NId, breaks=200, xlim= c(-.4,.4))
par(mfrow=(c(1,1)))

stdnorm <- (y1-y0+NId)/sqrt(s1^2/n+s2^2/n) ; mean(stdnorm <0)
hist((stdnorm), breaks=200) #
mean(stdnorm <0) # p value expected if alt is true

(1 - pnorm(qnorm(0.975)+qnorm(0.800)))


 





