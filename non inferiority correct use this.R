

# add the non inferiority to the test and perform ttest


nid=function(u0,u1,  n,   alpha,  NId=0, nSims=100000)  #  
{
  
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

# alternative parameterisation*
nid2=function(u0,u1,  n,   alpha,  NId=0, nSims=100000)  #  
{
  
  sigma0=sqrt(u0*(1-u0))  # se
  
  sigma1=sqrt(u1*(1-u1))  # se
  
  aveN=0; power=0; #u1=u1+NId   # non inf.   
  
  for (i in 1:nSims) {
    
    y0=rnorm(1, u0, sigma0/sqrt(n))  # ref
    
    y1=rnorm(1, u1, sigma1/sqrt(n))  # test
    
    z1=(y1-y0+NId)/sqrt(sigma1^2/n+sigma0^2/n)  #   here*
    
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
    NId=0.10)  #  "0.832869999998844"

# check type I error
nid(u0=.9, u1=.9+.1,  # u0 is control u1 new treatment
    n=n , alpha=0.025,
    NId=-0.10)  #  "0.025

#------------------------------------------------

# Grifols Fiona
# 
n=139
nid(u0=.724, u1=.724,
    n=n , alpha=0.025,
    NId=0.15)

# this is my recommendation 80% power!!!!!!!!!!!!!!!!!!!!!!!!
n=98
nid(u0=.724, u1=.75,
    n=n , alpha=0.025,
    NId=0.15)

# this is my recommendation 90% power!!!!!!!!!!!!!!!!!!!!!!!!
n=132
nid(u0=.724, u1=.75,
    n=n , alpha=0.025,
    NId=0.15)

#  here we postualte a wonderful new treatment
 
n=14
nid(u0=.7, u1=.9,
    n=n , alpha=0.15,
    NId=0.15)

 
#-------------------------------------------------------------------------
# verification of my recommendation for 80% power

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

 

y0 <- rnorm(nSims, u0, sqrt(u0*(1-u0))/sqrt(n))  # soc
y1 <- rnorm(nSims, u1, sqrt(u1*(1-u1))/sqrt(n))  # new trt 
par(mfrow=(c(3,1)))
hist(y0, breaks=200, xlim = c(.6,1))
hist(y1, breaks=200, xlim = c(.6,1 ))
hist(y1-y0, breaks=200, abline=0)


#--------------------------------------------------


# df <- data.frame(group = c(rep('y0', 1e4), rep('y1', 1e4) ),
#                  x = c(y0, y1))
# df$group <- factor(df$group)
# 
# require(ggplot2)
# ggplot(df, aes(x = x, fill = group)) + 
#   geom_histogram(color = 2, alpha = 0.75,
#                  position = "identity") +
#   scale_fill_manual(values = c("#8795E8", "#FF6AD5"))
# 
# 
# ggplot(df, aes(x=x, fill=group)) +
#   geom_histogram() +
#   scale_x_discrete(limits=1:14)
# 
# 
# df <- data.frame(var = c(rep('y0-y1', 1e4)),
#                  value = c(y0- y1))
# 
# ggplot(df, aes(x=value, fill=var)) +
#   geom_histogram( color='#e9ecef', alpha=0.6, position='identity',  bins = 200)
# 
# 
# 
# 
# df <- data.frame(var = c(rep('y0-y1', 1e4)),
#                  value = c(z1))
# 
# ggplot(df, aes(x=value, fill=var)) +
#   geom_histogram( color='#e9ecef', alpha=0.6, position='identity', bins=200)

#-------------------------------------------------------------------?????






























 