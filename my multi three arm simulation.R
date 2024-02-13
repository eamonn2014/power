
#ref https://stats.stackexchange.com/questions/467595/how-do-you-calculate-sample-sizes-for-multiple-treatments

# seems to be in agreement with PASS and literature examples
# three arm binary trial power with bonferroni adjustment
# comes from book on group sequential that I adapted 
# https://stats.stackexchange.com/questions/113602/test-if-two-binomial-distributions-are-statistically-different-from-each-other
  
formatz4 <- function(x){
  sprintf(x, fmt = '%#.4f')  
}

three.arm=function(u0, u1, u2, n,  alpha1=0.05 ,  nSims=1e5)
  {
  
      sigma0=sqrt(u0*(1-u0)) #placebo se
      sigma1=sqrt(u1*(1-u1)) #arm1 se
      sigma2=sqrt(u2*(1-u2)) #arm2 se
   
    P1 <- P2 <- P3 <- P4 <- P5 <- P6 <- 0 # counter
    
    for (i in 1:nSims) {
      
      y0=rnorm(1, u0, sigma0/sqrt(n)) # responses
      y1=rnorm(1, u1, sigma1/sqrt(n))
      y2=rnorm(1, u2, sigma2/sqrt(n))
      
      z1=(y1-y0)/sqrt(sigma1^2/n+sigma0^2/n) # arm1 v placebo z value
      z2=(y2-y0)/sqrt(sigma2^2/n+sigma0^2/n) # arm2 v placebo z value
      z3=(y2-y1)/sqrt(sigma2^2/n+sigma1^2/n) # arm2 v arm1 z value
      
      t1=2*(1-pnorm(z1))  # 2 sided p-valu
      t2=2*(1-pnorm(z2))
      t3=2*(1-pnorm(z3))
      
      #power
      if(t1<=alpha1/2){P1=P1+1/nSims} # bonferroni
      if(t2<=alpha1/2){P2=P2+1/nSims} # bonferroni
      if(t2<=alpha1/2 & t1<=alpha1/2){P3=P3+1/nSims}  # bonferroni
      if(t2<=alpha1/3 & t1<=alpha1/3 & t3<=alpha1/3){P4=P4+1/nSims}  # bonferroni 3
      if(t2<=alpha1/3 | t1<=alpha1/3 | t3<=alpha1/3){P5=P5+1/nSims}  # bonferroni 3
      if(t2<=alpha1/2 | t1<=alpha1/2){P6=P6+1/nSims}  # bonferroni
    }
     
    return (c( 
              "power arm1 v placebo being signif. ",   formatz4(P1),
              "power arm2 v placebo being signif. ",formatz4(P2),
              "power for both arms v placebo being signif.", formatz4(P3) ,
              "conjunctive (power to reject all null hypoth)", formatz4(P4) ,
              "disjunctive (power to reject at least one null hypothesis) ", formatz4(P5) ,
              "power for at least one arm signif. v placebo ", formatz4(P6) 
              ))
  }

# three.arm(u0=0.2, u1=0.3, u2=0.4, nStg1=85)
# three.arm(u0=0.2, u1=0.3, u2=0.4, nStg1=100)
# three.arm(u0=0.4, u1=0.5, u2=0.6, nStg1=98)

  #https://stats.stackexchange.com/questions/467595/how-do-you-calculate-sample-sizes-for-multiple-treatments
three.arm(u0=0.1, u1=0.15, u2=.20, n=1093) # cross validated example

three.arm(u0=0.4, u1=0.5, u2=0.6, n=159) # p139 thomas ryan Sample Size Determination and Power

three.arm(u0=0.4, u1=0.5, u2=0.6, n=337, alpha=.15) # verified with PASS can only have alpha 0.15 on trial version

three.arm(u0=0.4, u1=0.55, u2=0.6, n=50, alpha=.15)

three.arm(u0=0.2, u1=0.3, u2=0.3, n=257, alpha=.15)

three.arm(u0=0.2, u1=0.3, u2=0.3, n=257, alpha=.15)


## alehandra

three.arm(u0=0.8, u1=0.9, u2=0.9, n=245, alpha=.05)
three.arm(u0=0.8, u1=0.88, u2=0.88, n=400, alpha=.05)
three.arm(u0=0.8, u1=0.86, u2=0.86, n=750, alpha=.05)

three.arm(u0=0.8, u1=0.86, u2=0.9, n=93, alpha=.05)
 

#--------------------------------------------------------------------------------
 
three.arm2=function(u0, u1, u2, n,  alpha1=0.05 ,  nSims=1e5)
{
  
  sigma0=sqrt(u0*(1-u0)) #placebo se
  sigma1=sqrt(u1*(1-u1)) #arm1 se
  sigma2=sqrt(u2*(1-u2)) #arm2 se
  
  P1 <- P2 <- P3 <- P4 <- P5 <- P6 <- 0 # counter
  
  for (i in 1:nSims) {
    
    y0=rnorm(1, u0, sigma0/sqrt(n)) # responses
    y1=rnorm(1, u1, sigma1/sqrt(n))
    y2=rnorm(1, u2, sigma2/sqrt(n))
    
    z1=(y0-y1)/sqrt(sigma1^2/n+sigma0^2/n) # arm1 v placebo z value
    z2=(y0-y2)/sqrt(sigma2^2/n+sigma0^2/n) # arm2 v placebo z value
    z3=(y2-y1)/sqrt(sigma2^2/n+sigma1^2/n) # arm2 v arm1 z value
    
    t1=2*(1-pnorm(z1))  # 2 sided p-valu
    t2=2*(1-pnorm(z2))
    t3=2*(1-pnorm(z3))
    
    #power
    if(t1<=alpha1/2){P1=P1+1/nSims} # bonferroni
    if(t2<=alpha1/2){P2=P2+1/nSims} # bonferroni
    if(t2<=alpha1/2 & t1<=alpha1/2){P3=P3+1/nSims}  # bonferroni
    if(t2<=alpha1/3 & t1<=alpha1/3 & t3<=alpha1/3){P4=P4+1/nSims}  # bonferroni 3
    if(t2<=alpha1/3 | t1<=alpha1/3 | t3<=alpha1/3){P5=P5+1/nSims}  # bonferroni 3
    if(t2<=alpha1/2 | t1<=alpha1/2){P6=P6+1/nSims}  # bonferroni
  }
  
  return (c( 
    "power arm1 v placebo being signif. ",   formatz4(P1),
    "power arm2 v placebo being signif. ",formatz4(P2),
    "power for both arms v placebo being signif.", formatz4(P3) ,
    "conjunctive (power to reject all null hypoth)", formatz4(P4) ,
    "disjunctive (power to reject at least one null hypothesis) ", formatz4(P5) ,
    "power for at least one arm signif. v placebo ", formatz4(P6) 
  ))
}
 
## alehandra

three.arm2(u0=0.2, u1=0.10, u2=0.10, n=238, alpha=.05)
three.arm2(u0=0.2, u1=0.12, u2=0.12, n=400, alpha=.05)
three.arm2(u0=0.2, u1=0.14, u2=0.14, n=750, alpha=.05)

 

three.arm2(u0=0.2, u1=0.16, u2=0.16, n=1266, alpha=.15) # checking to pass















