
#ref https://stats.stackexchange.com/questions/467595/how-do-you-calculate-sample-sizes-for-multiple-treatments

# this isnot proven correct!

  three.arm=function(u0,u1,u2, nStg1,  alpha1=0.05 ,  nSims=100000)
  {
  
      sigma0=sqrt(u0*(1-u0)) #placebo
      sigma1=sqrt(u1*(1-u1)) #arm1
      sigma2=sqrt(u2*(1-u2)) #arm2
   
    ESP1=0; ESP2=0; ESP3=0; ESP4=0; ESP5=0;  ESP6=0;
    
    for (i in 1:nSims) {
      
      y0Stg1=rnorm(1, u0, sigma0/sqrt(nStg1))
      y1Stg1=rnorm(1, u1, sigma1/sqrt(nStg1))
      y1Stg2=rnorm(1, u2, sigma2/sqrt(nStg1))
      
      z1=(y1Stg1-y0Stg1)/sqrt(sigma1^2/nStg1+sigma0^2/nStg1)
      z2=(y1Stg2-y0Stg1)/sqrt(sigma2^2/nStg1+sigma0^2/nStg1)
      
      z23=(y1Stg2-y1Stg1)/sqrt(sigma2^2/nStg1+sigma1^2/nStg1)
      
      t1=2*(1-pnorm(z1))  # 2 sided pvalues
      t2=2*(1-pnorm(z2))
      t3=2*(1-pnorm(z23))
      
      if(t1<=alpha1){ESP1=ESP1+1/nSims}
      if(t2<=alpha1){ESP2=ESP2+1/nSims}
      if(t2<=alpha1/2 & t1<=alpha1/2){ESP3=ESP3+1/nSims}  # bonferroni
      if(t2<=alpha1/3 & t1<=alpha1/3 & t3<=alpha1/3){ESP4=ESP4+1/nSims}  # bonferroni
      if(t2<=alpha1/3 | t1<=alpha1/3 | t3<=alpha1/3){ESP5=ESP5+1/nSims}  # bonferroni
      if(t2<=alpha1/2 | t1<=alpha1/2){ESP6=ESP6+1/nSims}  # bonferroni
    }
     
    return (c( 
              "power arm1 v placebo being signif. ", ESP1,
              "power arm2 v placebo being signif. ", ESP2,
              "power for both arms v placebo being signif.", ESP3 ,
              "power for all v each other significant ", ESP4 ,
              "power for at least one signif. difference ", ESP5 ,
              "power for at least one arm signif. v placebo ", ESP6 
              ))
  }

# three.arm(u0=0.2, u1=0.3, u2=0.4, nStg1=85)
# three.arm(u0=0.2, u1=0.3, u2=0.4, nStg1=100)
# three.arm(u0=0.4, u1=0.5, u2=0.6, nStg1=98)

three.arm(u0=0.1, u1=0.15, u2=.20, nStg1=1093)

