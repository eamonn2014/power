
#set.seed(9067)  # this makes the example exactly reproducible
n     = 1093   # number of patients per arm
B     = 1000   # number of iterations in the simulation
p.mat = matrix(NA, nrow=3, ncol=B)                 # matrix to store the p-values
cond  = rep(c("C", "T1", "T2"), each=n)            # condition variable
y.mat = matrix(c(rbinom(n*B, size=1, prob=.20),    # resulting data
                 rbinom(n*B, size=1, prob=.15),
                 rbinom(n*B, size=1, prob=.10) ),
               nrow=n*3, ncol=B, byrow=T)

cond <- factor(cond)

for(j in 1:B){                         # fitting the models & storing the p-values
  
  cond <- relevel(cond, ref = "C")  # revert back
  m <- glm(y.mat[,j] ~ cond, family="binomial" )
 
  p.mat[1,j]   = summary(m)$coefficients[2,4] #  p-value
  p.mat[2,j]   = summary(m)$coefficients[3,4] #  p-value
  
  cond <- relevel(cond, ref = "T2")  # relevel
  m <- glm(y.mat[,j] ~ cond, family="binomial" )
  p.mat[3,j]   = summary(m)$coefficients[3,4] 
  
}

## power: i.e., the proportion of runs where all p's were significant
mean(apply(p.mat , 2, function(j){  mean(j<.05/3)==1  }))  # three comparisons divde by 3
mean(apply(p.mat , 2, function(j){  sum(j<.05/3) >=1  }))   #at least one