
#Let's simulate data by ourselves
#We simulated 1000 replicates and we want to assess power (Beta1 < alpha), bias, or computing 
#any statistic

#https://stats.stackexchange.com/questions/604914/r-power-analysis-for-a-logistic-regression
#https://stats.stackexchange.com/questions/467595/how-do-you-calculate-sample-sizes-for-multiple-treatments

#Assuming these values for the log odds ratio

# beta0 placebo  0.2
# trt1 proportion 0.1
# trt2 proportion 0.1

p1 <- .2
p2 <- .1

p1odd <- p1/(1-p1)
p2odd <- p2/(1-p2)
or <- p2odd/p1odd


#not on log scale
beta0= log(p1odd)  # 0.25 intrecept odd of pacebo  #-1.386294
beta1= log(or)  # odds ratio v placebo   #-.8109302
#beta2= 0.25*(.1/.9)  # odds ratio v placebo 


#Your significance threshold
alpha=0.05

#Number of simulations 
n = 5000

N=400  # total sample size

#If we are interested in rejecting the null
power.vec = c()

#If we are interested in assessing parameter bias
bias.vec = c()

#If we are interested in computing Mac Fadden pseudo R2 
R2.vec = c()

for(i in 1:n){
  
  #Assuming one trt predictor and N individuals
  X = rbinom(N, 1, 0.5)
  
  
  #Our logistic model
  pr = exp(beta0+X*beta1)/(1+exp(beta0+X*beta1))
  
  y = rbinom(N, 1,pr)
  
  model = glm(y~X, family = binomial(link="logit"))
  sm = summary(model)
  
  power.vec = c(power.vec, sm$coefficients[2,4]<alpha)
  bias.vec = c(bias.vec, beta1-sm$coefficients[2,1])
  R2.vec = c(R2.vec, 1-(sm$deviance/sm$null.deviance))
}

#The power (here rejecting the null for beta 1 depends on the number of individuals)
sum(power.vec)/n

#The bias is reducing when increasing the number of individuals
mean(abs(bias.vec))

#Mean pseudo R2
mean(R2.vec)

#Sample size calculation 

library(WebPower)

#p0 is probability of Y = 1 when X = 0: You should change this parameter
#p1 is probability of Y=1 when X=1: You should change this parameter

#alpha: your significance threshold 
#power is the target power
#family: the nature of your predictor


WebPower::wp.logistic(n = NULL, p0 = 0.2, p1 = 0.1, alpha = 0.05,
                      power = .8, family = "Bernoulli" )



#https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html

library(pwr)
(p.out <- pwr.2p.test(h = ES.h(p1 = 0.2, p2 = 0.1),  # given powern what is n
                    sig.level = 0.05,
                    power = 0.80, 
                    alternative="two.sided"))

plot(p.out)


Hmisc::bpower(p1=p1, p2=p2, n1=N/2, n2=N/2, alpha=0.05)

pwr::pwr.2p.test(pwr::ES.h(p1, p2), n = N/2)  #what is the power?


## ad another covariate



#================================================================================================================

#Assuming these values for the log odds ratio

# If the trial is exploratory, and any findings will be tested in further trials, 
# then there is less need for a multiple-testing correction, as any false-positive 
# findings will not change practice. In fact, recent evidence has shown that from an 
# efficiency standpoint, exploratory multi-arm studies should use high significance
# levels when they are 
# followed by a confirmatory trial
 


#------ parameters ------
n<- 3279   # total sample size
n<-159*3
N <- 1000  # simulations
alpha=0.05   # 3 comparisons"s!
# hypothesised AE rates, so high is bad

p1 <- .1
p2 <- .15
p3 <- .20

p1 <- .4
p2 <- .5
p3 <- .6

p1odd <- p1/(1-p1)
p2odd <- p2/(1-p2)
p3odd <- p3/(1-p3)
or2 <- p2odd/p1odd
or3 <- p3odd/p1odd


beta0 <- log(p1odd) 
betaB <- log(or2)
betaC <- log(or3)
 

#------ initialisation ------
beta0Hat <- rep(NA, N)
betaBHat <- rep(NA, N)
betaCHat <- rep(NA, N)

#If we are interested in rejecting the null
power.vec1 <- power.vec2 <- power.vec3 <- power.vec4 <- c()

#If we are interested in assessing parameter bias
bias.vec1 <- bias.vec2 <- c()

#If we are interested in computing Mac Fadden pseudo R2 
R2.vec = c()
#----------------------------


 for(i in 1:N)
   {
       #data generation
      x <- sample(x=c("A","B", "C" ), 
                                size=n, replace=TRUE, prob=rep(1/3, 3))  #(a)
      x <- factor(x)
      
      x <- relevel(x, ref = "A")  # revert back
      
      linpred <- cbind(1, lme4::dummy(x)) %*% c(beta0, betaB, betaC)  #(b)
      
        pi <- exp(linpred) / (1 + exp(linpred))  #(c)
        
       y <- rbinom(n=n, size=1, prob=pi)  #(d)
      
         #fit the logistic model
          x <- factor(x)
          
          mod <- glm(y ~ x, family="binomial" )
        
          #save the estimates
           beta0Hat[i] <- mod$coef[1]
           betaBHat[i] <- mod$coef[2]
           betaCHat[i] <- mod$coef[3]
           
           sm = summary(mod)
           
           pv1 <- sm$coefficients[2,4]
           pv2 <- sm$coefficients[3,4]
           
           power.vec1 = c(power.vec1, pv1<alpha)
           power.vec2 = c(power.vec2, pv2<alpha)
           power.vec3 = c(power.vec3, pv1<alpha/2 && pv2<alpha/2)
           
           bias.vec1 = c(bias.vec1, betaB-mod$coef[2]) # 
           bias.vec2 = c(bias.vec2, betaC-mod$coef[3])
           R2.vec = c(R2.vec, 1-(sm$deviance/sm$null.deviance))
           
           #-------------get the third comparison pvalues---------------
           x <- relevel(x, ref = "C")
           mod1 <- glm(y ~ x, family="binomial" )
           sm1 = summary(mod1)
           pv3 <- sm1$coefficients[3,4]  #BvC
           power.vec4 = c(power.vec4, pv1<alpha/3 && pv2<alpha/3 && pv3<alpha/3) # bonferroni
            
          # power.vec4 = c(power.vec4, pv1<alpha/3 | pv2<alpha/3 | pv3<alpha/3)
          
         }
 #-------------------------

 #------ results ------
#The power (here rejecting the null for beta 1 depends on the number of individuals)
sum(power.vec1)/N
sum(power.vec2)/N
sum(power.vec3)/N
sum(power.vec4)/N

#The bias is reducing when increasing the number of individuals
mean(abs(bias.vec1))
mean(abs(bias.vec2))

#Mean pseudo R2
mean(R2.vec)

round(c(beta0e=mean(beta0Hat), 
               betaBe=mean(betaBHat), 
                   betaCe=mean(betaCHat)), 3)

 #---------------------
beta0
betaB
betaC








####----------------------------increase arms



#------ parameters ------
n<- 1200   # total sample size
N <- 1000# simulations
alpha=0.05
# hypothesised AE rates, so high is bad

p1 <- .2
p2 <- .2
p3 <- .2
p4 <- .2
p5 <- .2
p6 <- .2

p1odd <- p1/(1-p1)
p2odd <- p2/(1-p2)
p3odd <- p3/(1-p3)
p4odd <- p4/(1-p4)
p5odd <- p5/(1-p5)
p6odd <- p6/(1-p6)


or2 <- p2odd/p1odd
or3 <- p3odd/p1odd
or4 <- p4odd/p1odd
or5 <- p5odd/p1odd
or6 <- p6odd/p1odd


beta0 <- log(p1odd) 
betaB <- log(or2)
betaC <- log(or3)
betaD <- log(or4)
betaE <- log(or5)
betaF <- log(or6)

#------ initialisation ------
beta0Hat <- rep(NA, N)
betaBHat <- rep(NA, N)
betaFHat <- betaEHat <- betaDHat <- betaCHat <- rep(NA, N)



#If we are interested in rejecting the null
power.all <- power.vec1 <- power.vec2 <- power.vec3 <- power.vec4 <- power.vec5 <- power.vec6<- c()

#If we are interested in assessing parameter bias
bias.vec1 <- bias.vec2 <- bias.vec3 <- bias.vec4 <- bias.vec5 <- bias.vec6 <- c()

#If we are interested in computing Mac Fadden pseudo R2 
R2.vec = c()
#----------------------------


for(i in 1:N)
{
  #data generation
  x <- sample(x=c("A","B", "C" , "D","E", "F"), 
              size=n, replace=TRUE, prob=rep(1/6, 6))  #(a)
  
  linpred <- cbind(1, lme4::dummy(x)) %*% c(beta0, betaB, betaC, betaD, betaE, betaF)  #(b)
  
  pi <- exp(linpred) / (1 + exp(linpred))  #(c)
  
  y <- rbinom(n=n, size=1, prob=pi)  #(d)
  
  #fit the logistic model
  x <- factor(x)
  
  mod <- glm(y ~ x, family="binomial" )
  
  #save the estimates
  beta0Hat[i] <- mod$coef[1]
  betaBHat[i] <- mod$coef[2]
  betaCHat[i] <- mod$coef[3]
  betaDHat[i] <- mod$coef[4]
  betaEHat[i] <- mod$coef[5]
  betaFHat[i] <- mod$coef[6]
  
  sm = summary(mod)
  
  pv1 <- sm$coefficients[2,4]
  pv2 <- sm$coefficients[3,4]
  pv3 <- sm$coefficients[4,4]
  pv4 <- sm$coefficients[5,4]
  pv5 <- sm$coefficients[6,4]
 
  
  power.vec1 = c(power.vec1, pv1<alpha)
  power.vec2 = c(power.vec2, pv2<alpha)
  power.vec3 = c(power.vec3, pv3<alpha)
  power.vec4 = c(power.vec4, pv4<alpha)
  power.vec5 = c(power.vec5, pv5<alpha)
    
  
  
  
  power.all = c(power.all, pv1<alpha && pv2<alpha && pv3<alpha && pv4<alpha && pv5<alpha )
  bias.vec1 = c(bias.vec1, betaB-mod$coef[2]) # 
  bias.vec2 = c(bias.vec2, betaC-mod$coef[3])
  bias.vec3 = c(bias.vec3, betaD-mod$coef[4]) # 
  bias.vec4 = c(bias.vec4, betaE-mod$coef[5])
  bias.vec5 = c(bias.vec5, betaE-mod$coef[6])
  R2.vec = c(R2.vec, 1-(sm$deviance/sm$null.deviance))
  
  
}
#-------------------------

#------ results ------
#The power (here rejecting the null for beta 1 depends on the number of individuals)
sum(power.vec1)/N
sum(power.vec2)/N
sum(power.vec3)/N
sum(power.vec4)/N
sum(power.vec5)/N
sum(power.all)/N 

#The bias is reducing when increasing the number of individuals
mean(abs(bias.vec1))
mean(abs(bias.vec2))

#Mean pseudo R2
mean(R2.vec)

round(c(beta0e=mean(beta0Hat), 
        betaBe=mean(betaBHat), 
        betaCe=mean(betaCHat)), 3)

#---------------------
beta0
betaB
betaC













