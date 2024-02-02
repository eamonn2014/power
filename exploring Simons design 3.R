
############ code from R function

n<-20;p0=.2;p1=.4; k=3

(s  <- power.ctepd::n.pick.winner(n=20, p0 =p0, p1 =  p1, k =k ))

(n = ceiling(n))
(x = 0:n)
(Bi0 = pbinom(x, n, p0)) # cumulative, probability that the number of successes will be less than or equal to x.
barplot(Bi0, names.arg = x)

Bi1 = pbinom(x, n, p1) # cumulative, probability that the number of successes will be less than or equal to x.
barplot(Bi1, names.arg = x)

bi0 = dbinom(x, n, p0)
barplot(bi0, names.arg = x)

bi1 = dbinom(x, n, p1)
barplot(bi1, names.arg = x)


Bix = c(0, Bi0)[1:(n + 1)] # ad zero to beginning and keep 


fx = Bi0^(k - 1) - Bix^(k - 1)

barplot(Bi0^(k - 1), names.arg = x)
barplot(Bix^(k - 1), names.arg = x)
barplot(fx, names.arg = x)


# second part of eq 2
gj = 0
for (j in 1:(k - 1)) {
  ckj = choose(k - 1, j)
  gj = gj + ckj * bi0^j * Bix^(k - 1 - j)/(j + 1)  # divide total arms
}

#----

eq1 <- sum(fx * (1 - Bi1))
eq2 <- sum(bi1 * gj)
pb = eq1 + eq2

pb




#-----------------------------------------------------------------------------------
# pick a winner function for simulation
# this matches for 3 groups!
# key is have 1 group with best response and all others worst response
# enter best response in third position of mu and others are all the worst response

  n=47
  
  (s  <- power.ctepd::n.pick.winner(n=n, p0 =.1, p1 =  .14, k =3 ))
 

pick.winnner = function(nSims=1e6,  mu=c(0.12, 0.12, 0.15), nStg1=470) { 

  NumOfArms = length(mu)
  p = matrix(NA, nrow=NumOfArms, ncol=nSims) # to store simulated data
 
  #-------------------------create data
  for (i in 1:NumOfArms)  {
    
    p[i,] = rbinom(nSims, nStg1, prob=mu[i])  #  
    
  }
 
  #-----------------------------------------------------------------------examine simulations
  # A wins outright
  A1 <- (p[1, ] >  p[2, ])  & (p[1, ] >  p[3, ])  # indices where this condition is met
  Z1 <- length(A1[A1==T])/nSims

  # B wins outright
  A2 <- (p[2, ] >  p[1, ])  & (p[2, ] >  p[3, ])  # ditto
  Z2 <- length(A2[A2==T])/nSims
  
  # C wins outright
  A3 <- (p[3, ] >  p[1, ])  & (p[3, ] >  p[2, ])  # ditto
  Z3 <- length(A3[A3==T])/nSims
  
  # B and C draw and A less
  A4 <- (p[2, ] ==  p[3, ])  & (p[1, ] <  p[3, ]) 
  Z4 <- length(A4[A4==T])/nSims
  
  # A and C draw and B less
  A5 <- (p[1, ] ==  p[3, ])  & (p[2, ] <  p[1, ]) 
  Z5 <- length(A5[A5==T])/nSims
  
  # A and B draw and C less
  A6 <- (p[1, ] ==  p[2, ])  & (p[3, ] <  p[1, ]) 
  Z6 <- length(A6[A6==T])/nSims
  
  # all draw
  A7 <- (p[1, ] ==  p[2, ])  & (p[3, ] ==  p[1, ]) 
  Z7 <- length(A7[A7==T])/nSims 
  
  #-----------------------------------------------------------------------
  cat("A wins outright", Z1)
  cat("\nB wins outright", Z2)
  cat("\nC wins outright", Z3)          # C winning is of interest
  cat("\nB and C draw and A less", Z4)  # 50:50 we chose C correctly
  cat("\nA and C draw and B less", Z5)  # 50:50 we chose C correctly
  cat("\nA and B draw and C less", Z6)
  cat("\nAll A B C draw", Z7 )
  cat("\nProbability of selecting best group",  Z4/2 + Z5/2 + Z3)

}
  #-----------------------------------------------------------------------

# execute function
  
pick.winnner()  # expecting 85%
pick.winnner(mu=c(0.25, 0.25, 0.5), nStg1=28)  # expecting 95%
pick.winnner(mu=c(0.2, 0.2, 0.3), nStg1=93)  # expecting 90%

#---------------------------------------------------------------------

# alejhandara:
# 20% AKI wants to see effects of 20*.7 20*.6 20*.5, so 14 12 10

pick.winnner(mu=c(0.8, 0.8, 0.86), nStg1=196)  # 90%
pick.winnner(mu=c(0.8, 0.8, 0.86), nStg1=144)  # 85%
pick.winnner(mu=c(0.8, 0.8, 0.86), nStg1=109)  # 80%
  
pick.winnner(mu=c(0.8, 0.8, 0.88), nStg1=105)  # 90%
pick.winnner(mu=c(0.8, 0.8, 0.88), nStg1=78)  # 85%
pick.winnner(mu=c(0.8, 0.8, 0.88), nStg1=59)  # 80%
  
pick.winnner(mu=c(0.8, 0.8, 0.9), nStg1=64)  # 90%
pick.winnner(mu=c(0.8, 0.8, 0.9), nStg1=47)  # 85%
pick.winnner(mu=c(0.8, 0.8, 0.9), nStg1=36)  # 80%  
  
# verification
power.ctepd::n.pick.winner(power=.9,  p0 =.8, p1 =  .86, k =3 )
power.ctepd::n.pick.winner(power=.85, p0 =.8, p1 =  .86, k =3 )
power.ctepd::n.pick.winner(power=.8,  p0 =.8, p1 =  .86, k =3 )
  
power.ctepd::n.pick.winner(power=.9,  p0 =.8, p1 =  .88, k =3 )
power.ctepd::n.pick.winner(power=.85, p0 =.8, p1 =  .88, k =3 )
power.ctepd::n.pick.winner(power=.8,  p0 =.8, p1 =  .88, k =3 )
  
power.ctepd::n.pick.winner(power=.9,  p0 =.8, p1 =  .9,  k =3 )
power.ctepd::n.pick.winner(power=.85, p0 =.8, p1 =  .9,  k =3 )
power.ctepd::n.pick.winner(power=.8,  p0 =.8, p1 =  .9,  k =3 )

#---------------------------------------------------------------------

# what if there is no true effect , you will pick a winner 1/3 time with 3 groups
# so experimental arms will be the winner 2/3 time when there is no true difference!
pick.winnner(mu=c(0.8, 0.8, 0.8), nStg1=30)  # 80%  
  
  
  
  
  
  
  # p[1:NumOfArms,1:10]
  # A[1:10]
  # 
  # p[ p[1,] < p[2,],]
  # 
  # 
  # m[m[, "three"] == 11,]
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # p[1:NumOfArms,1:10]
  # 
  # m <- apply( p,2, which.max )
  # m[1:10]
  # sum(m %in% 4)/nSims
  # 
  # # find dupicates in ro[,w
  # u <- function(p) {!p %in% p[duplicated(p)]}
  # m1 <- apply( p,2, u )
  # m1[1:NumOfArms,1:10]
  # 
  # 
  # dup <- apply(m1,2, function(x) {sum(x)==4})
  # which(dup %in% T)
  # 
  # 
  
  
#    #----------main loop
#   
#   for (iSim in 1:nSims) 
#   {
#     
#     order[iSim] <- sum (xObs==sort(xObs)) == NumOfArms      # check order is correct
#     
#     MaxRsp =xObs[1]; SelectedArm=1  # first arm is the control
#     
#     for (i in 1:NumOfArms) {
#       
#       if (xObs[i]> MaxRsp) {SelectedArm=i; MaxRsp=xObs[i]}  # select best response
#       
#       
#     }
#     
#     best[iSim] <-  SelectedArm
#     
#   }
#   
#   newList <- list("beat" = best, "order" = order)
#   
# }
# 
# res <- WinnerDesign(nSims=1000, 
#                     mu=c(0.12, 0.15, 0.18, 0.22) , 
#                     nStg1=20 )
# 
# 



