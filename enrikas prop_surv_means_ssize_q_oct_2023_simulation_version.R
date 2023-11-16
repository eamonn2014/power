

# proportions ----------------------------------------------------------------------------
#https://www.r-bloggers.com/2009/02/r-good-practice-%e2%80%93-adding-footnotes-to-graphics/
rm(list=ls())
library(rms)
library(ggplot2)
library(grid)


makeFootnote <- function(footnoteText=
format(Sys.time(), "%d %b %Y"),
size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}


makeFootnoteL <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(250, "mm"),
            y= unit(2, "mm"),
            just=c("left", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}





# Define the parameters of the power calculation
alpha <- 0.05

ration <- 1/3

# Calculate the power for a range of sample sizes
# sample_sizes <- seq(10, 800, by = 10)
# power_values_1 <- bpower(n = sample_sizes, p1 = .2, p2 = .15, alpha = alpha)
# power_values_2 <- bpower(n = sample_sizes, p1 = .2, p2 = .10, alpha = alpha)
# 
# power_values_5 <- bpower(n = sample_sizes, p1 = .25, p2 = .20, alpha = alpha)
# power_values_6 <- bpower(n = sample_sizes, p1 = .25, p2 = .15, alpha = alpha)
# 
# power_values_3 <- bpower(n = sample_sizes, p1 = .3, p2 = .25, alpha = alpha)
# power_values_4 <- bpower(n = sample_sizes, p1 = .3, p2 = .2,  alpha = alpha)
# 
# power_values_1a <- bpower(n1 = sample_sizes*ration, n2 = sample_sizes*(1-ration), p1 = .2, p2 = .15, alpha = alpha)
# power_values_2a <- bpower(n1 = sample_sizes*ration, n2 = sample_sizes*(1-ration), p1 = .2, p2 = .10, alpha = alpha)
# 
# power_values_5a <- bpower(n1 = sample_sizes*ration, n2 = sample_sizes*(1-ration), p1 = .25, p2 = .20, alpha = alpha)
# power_values_6a <- bpower(n1 = sample_sizes*ration, n2 = sample_sizes*(1-ration), p1 = .25, p2 = .15, alpha = alpha)
# 
# 
# power_values_3a <- bpower(n1 = sample_sizes*ration, n2 = sample_sizes*(1-ration), p1 = .3, p2 = .25, alpha = alpha)
# power_values_4a <- bpower(n1 = sample_sizes*ration, n2 = sample_sizes*(1-ration), p1 = .3, p2 = .2,  alpha = alpha)

# sims=75000
# n <- seq(10, 800, by = 10)
# p1 <- c(.20,.20, .30,.30,.25,.25)
# p2 <- c(.15,.10, .25,.20,.20,.15)
# 
# res = matrix(NA, length(n), 6)
# 
# 
# 
# rbinom(10,n,.20) - rbinom(10,n,.10)


#https://stats.stackexchange.com/questions/354556/what-is-the-standard-error-for-hypothesis-testing-of-two-proportions-using-a-z-t
#https://stats.stackexchange.com/questions/354556/what-is-the-standard-error-for-hypothesis-testing-of-two-proportions-using-a-z-t
#https://stats.stackexchange.com/questions/258522/how-to-calculate-the-power-of-a-test-that-compares-two-proportions
 
p1 = runif(1, .5,.9) 
p2 = runif(1, .5,.9)   

ration=1
n=100  # total sample size
r <- ration 
n1 <- round(n / (r + 1))
n2 <- round(r * n1)
  


n.reject = 0
n.rep = 1000
for(k in 1:n.rep)
{ 
  x1 = sum(runif(n1)<p1)
  x2 = sum(runif(n2)<p2)
  ptest = prop.test( c(x1,x2), c(n1,n2), correct=FALSE)
  if(  ptest$p.value <= 0.05 ) n.reject = n.reject + 1
}
n.reject/n.rep  #power

Hmisc::bpower(p1=p1, p2=p2, n1=n1, n2=n2, alpha=0.05)
pwr::pwr.2p.test(pwr::ES.h(p1, p2), n = n/2)



 

#------------------- nice ....
  Z1 <- abs(p2-p1)
  
  Z2 <- ( p1*n1 + p2*n2 )  / (n1+n2)  # weight by sample size
  
  Z3<- sqrt( Z2*(1-Z2)*( 1/n1 + 1/n2) )
  
   Z <- Z1/Z3
  
   ZB <-  qnorm(0.025) + Z
  
   ZL <- -qnorm(0.025) + Z
  
  pnorm(ZB) + 1-pnorm(ZL)  #power
#-----------------------
   
 
  L <- pnorm(qnorm(.025), Z, 1)
  R <- 1-pnorm(qnorm(.975), Z, 1)
  L + R
  
#-----------------------
  
  Hmisc::bpower(p1=p1, p2=p2, n1=n1, n2=n2, alpha=0.05)
  
  
  #https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html
  
  library(pwr)
  p.out <- pwr.2p.test(h = ES.h(p1 = p1, p2 = p2),
                       sig.level = 0.05,
                       power = 0.80 ,
                       alternative="two.sided")
  p.out
  plot(p.out)
  
  

#----------------------------------------------------------------------- 
  # start of bendix approach
  
  n <- seq(50, 800, by = 10)
  p1 <- c(.20, .30,.25)
  p2 <- c(.15,.10, .25,.20,.15)
 # p1 <- c(.20,.30 )
  #p2 <- c(.10)
  
  ration <- c(1, 2)
  
 # res = matrix(NA, length(n), 6)
  
  dnam = list( n=n, p1=p1, p2=p2, ration=ration,  What=c("power" ) )
  pwpr <- array( NA, dim=sapply( dnam, length ), dimnames=dnam )
  str( pwpr )
  
 #----------------------------------------------------------------------- 
  
  simp <- function(n=n,p1=p1, p2=p2, ration=1, n.rep=1000){
    
    if (ration != 1) {
      
    r <- ration/(ration+1) 
    n1 <- round(n*(1-r)*1)        # n1 smaller group eg 33 associated with p1
    n2 <- round(n*(r)*1) #        n2 larger  group eg 66 associated with p2
    } else {
      
      n1 <- n/2        
      n2 <- n/2  
      
    }
    
 
    
    n.reject = 0
    for(k in 1:n.rep)
    { 
      x1 = sum(runif(n1)<p1)
      x2 = sum(runif(n2)<p2)
      ptest = prop.test( c(x1,x2), c(n1,n2), correct=FALSE)
      if(  ptest$p.value < 0.05 ) n.reject = n.reject + 1
    }
   
    c( n.reject/n.rep )
    
  } 
  
  
  simp2 <- function(n=n,p1=p1, p2=p2, ration=1 ){
    
    if (ration != 1) {
      
      r <- ration/(ration+1) 
      n1 <- round(n*(1-r)*1)        # n1 smaller group eg 33 associated with p1
      n2 <- round(n*(r)*1) #        n2 larger  group eg 66 associated with p2
    } else {
      
      n1 <- n/2        
      n2 <- n/2  
      
    }  
    
  
  Z1 <- p2-p1
  
  Z2 <- ( p1*n1 + p2*n2 )  / (n1+n2)  # weight by sample size
  
  Z3<- sqrt( Z2*(1-Z2)*( 1/n1 + 1/n2) )
  
  Z <- Z1/Z3
  
  ZB <-  qnorm(0.025) + Z
  
  ZR <- -qnorm(0.025) + Z
  
  c(pnorm(ZB) + 1-pnorm(ZR))  #power
  
  }
  
  
  #----------------------------------------------------------------------- 

  

 # simp  (n=100,p1=.2, p2=.3, ration=1)
  
  system.time(
    for (i in 1:length (n)) 
     for (pp1 in 1: length(p1))  
      for (pp2 in 1: length(p2))  
        for (r in 1: length(ration))  
       pwpr[i,pp1,pp2,r,] <- simp(  n=n[i], p1=p1[pp1], p2=p2[pp2],  ration=ration[r] , n.rep=199 )

)
   
  
  system.time(
    for (i in 1:length (n)) 
      for (pp1 in 1: length(p1))  
        for (pp2 in 1: length(p2))  
          for (r in 1: length(ration))  
            pwpr[i,pp1,pp2,r,] <- simp2(  n=n[i], p1=p1[pp1], p2=p2[pp2],  ration=ration[r]  )
    
  )
  
  
  
  

  round( ftable( pwpr[,1:2,,,], row.vars=1:2, col.vars=3:4 ), 2 )
  
  
  L <- reshape2::melt(pwpr)
  L$ratio <- factor(L$ration)
  L$variable <- paste(L$p1, L$p2)
  L <- L[L$p1 != L$p2,]
  
  
  all <-ggplot(L, aes(x = n, y = value, linetype=ratio,
                                         group = interaction(ratio, variable),
                                      colour = variable)) +
                       geom_line() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),  limits = c(0,1)) +
    scale_x_continuous(breaks = seq(0,800,50), limits = c(0,800)) +
    theme_bw() +
    #  theme(legend.position="none") +
    theme(
      plot.title=element_text(size = 12), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
      legend.text=element_text(size=10),
      legend.title=element_text(size=10),
      legend.position="right",

      legend.key.size = unit(12, "pt"),  # legend horizontal bar length

      axis.text.x  = element_text(size=10),
      axis.text.y  = element_text(size=10),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      plot.caption=element_text(hjust = 0, size = 7),
      strip.text.x = element_text(size = 16, colour = "black", angle = 0),
      axis.title.y = element_text(size = rel(1), angle = 90),
      axis.title.x = element_text(size = rel(1), angle = 0 ),


      strip.background = element_rect(colour = "black", fill = "white"),
      panel.background = element_rect(fill = 'white', colour = 'white'),
      plot.background = element_rect(fill = 'white', colour = 'white')
    ) +


    labs(y="Power", x = 'Total Sample Size', color = "Population probabilities", linetype="Ratio p1:p2" )+
    labs(caption = paste("- Alpha the type I assertion probability = 0.05 two sided. Ratio 1:2 pertains to larger sample in experimental arm.\n- Uses method of Fleiss, Tytun, and Ury (but without the continuity correction) to estimate the power (or the sample size to achieve a given power) of a two-sided test for the difference in two proportions. \n- Fleiss JL, Tytun A, Ury HK (1980): A simple approximation for calculating sample sizes for comparing independent proportions. Biometrics 36:343–6.\n" )) +
    ggtitle(paste0("Endpoint option 1. Power (or sample size) for difference in two proportions for evaluation of treatment effect on mortality rate.") )


  #https://stackoverflow.com/questions/15059093/ggplot2-adjust-the-symbol-size-in-legends
  all <- all + guides(color = guide_legend(override.aes = list(linewidth = 3 )))

  all
  
  

#-------------------------------------------------------------------------------------
# this code works but I do not want to use other peoples functions
  #-------------------------------------------------------------------------------------



#   sims=75000
#   n <- seq(10, 800, by = 10)
#   p1 <- c(.20,.20, .30,.30,.25,.25)
#   p2 <- c(.15,.10, .25,.20,.20,.15)
# 
#   res = matrix(NA, length(n), 6)
# 
# 
# 
# for (k in 1: length(p1)) {
# 
#     for (i in 1:length (n)) {
# 
# 
#   res[i,k] <- bpower.sim(p1 = p1[k], p2 = p2[k],
#                       n=n[i], # n1 = n[i] , n2 = n[i], 
#                     alpha=0.05, nsim=sims)[1][[1]]  # just take the point estimate
#   
#       }
# 
# }
# 
# 
# 
# res1 = matrix(NA, length(n), 6)
# 
# 
# for (k in 1: length(p1)) {
#   
#   for (i in 1:length (n)) {
#     
#     n1 <- round(n[i]*ration*1)        # n1 smaller group eg 33 associated with p1
#     n2 <- round(n[i]*((1-ration)*1))  # n2 larger  group eg 66 associated with p2
#     
#     res1[i,k] <- bpower.sim(p1 = p1[k], p2 = p2[k],
#                            n1 = n1 , n2 = n2, 
#                            alpha=0.05, nsim=sims)[1][[1]]  # just take the point estimate
#     
#   }
#   
# }
# 
# 
# res1 <- data.frame(res1)
# res1 <- cbind(n,res1)
# 
# res  <- data.frame(res)
# res  <- cbind(n,res )
# 
# 
# x <-  reshape2::melt(res, id.vars="n")
# x1 <- reshape2::melt(res1, id.vars="n")
# 
# x$ratio  <- "1:1"
# x1$ratio <- "1:2"
# 
#  
# x <- rbind(x,x1)
# 
# all <-ggplot(x, aes(x = n, y = value, linetype=ratio,
#                     group = interaction(ratio, variable),
#                     colour = variable)) +
#   geom_line() +
#   scale_color_manual(labels = c("p1=0.20, p2=0.15", "p1=0.20, p2=0.10", 
#                                 "p1=0.30, p2=0.25", "p1=0.30, p2=0.20" ,
#                                 "p1=0.25, p2=0.20", "p1=0.25, p2=0.15" 
#                                 
#   ), values = c("red", "green", "blue", "purple","black","orange")) +
#   
#   scale_linetype_manual(values=c("solid",   "dashed")) +
#   
#   
#   #  geom_vline(xintercept = seq(0, 700, by = 50),  col = "gray", linetype = "solid",  linewidth = .25) +
#   #   geom_hline(yintercept = seq(0, 1, by = 0.1), col = "gray", linetype = "solid",  linewidth = .25) +
#   
#   scale_y_continuous(breaks = seq(0, 1, by = 0.1),  limits = c(0,1)) +
#   scale_x_continuous(breaks = seq(0,800,50), limits = c(0,800)) +
#   theme_bw() +
#   #  theme(legend.position="none") +
#   theme( 
#     plot.title=element_text(size = 12), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
#     legend.text=element_text(size=10),
#     legend.title=element_text(size=10),
#     legend.position="right",
#     
#     legend.key.size = unit(12, "pt"),  # legend horizontal bar length
#     
#     axis.text.x  = element_text(size=10),
#     axis.text.y  = element_text(size=10),
#     axis.line.x = element_line(color="black"),
#     axis.line.y = element_line(color="black"),
#     plot.caption=element_text(hjust = 0, size = 7),
#     strip.text.x = element_text(size = 16, colour = "black", angle = 0),
#     axis.title.y = element_text(size = rel(1), angle = 90),
#     axis.title.x = element_text(size = rel(1), angle = 0 ),
# 
#     
#     strip.background = element_rect(colour = "black", fill = "white"),
#     panel.background = element_rect(fill = 'white', colour = 'white'),
#     plot.background = element_rect(fill = 'white', colour = 'white')
#   ) +
# 
#   
#   labs(y="Power", x = 'Total Sample Size', color = "Population probabilities", linetype="Ratio p1:p2" )+
#   labs(caption = paste("- Alpha the type I assertion probability = 0.05 two sided. Ratio 1:2 pertains to larger sample in experimental arm.\n- Uses method of Fleiss, Tytun, and Ury (but without the continuity correction) to estimate the power (or the sample size to achieve a given power) of a two-sided test for the difference in two proportions. \n- Fleiss JL, Tytun A, Ury HK (1980): A simple approximation for calculating sample sizes for comparing independent proportions. Biometrics 36:343–6.\n" )) +
#   ggtitle(paste0("Endpoint option 1. Power (or sample size) for difference in two proportions for evaluation of treatment effect on mortality rate.") ) 
# 
# 
# #https://stackoverflow.com/questions/15059093/ggplot2-adjust-the-symbol-size-in-legends
# all <- all + guides(color = guide_legend(override.aes = list(linewidth = 3 )))
# 
# all
# 
# 
# 
# pdf("Endpoint option 1 mortality reduction proportions.pdf", width=10, height=6) # open an appropriate graphics device
# print(all)
# makeFootnote(paste(Sys.Date() )) 
# makeFootnoteL("Page 3 of 5") 
# dev.off()
#-------------------------------------------------------------------------------------
# end
#-------------------------------------------------------------------------------------



#-survival--------------------------------------------------------------------------------

require(survival)

# just dealing with events

 
# n<-400

#sim = function(h,h2,n) {
#   
#   # Simulate some time to event data
#   T1 <- 1/h* (-log(runif(n)))^(1)   
#   T2 <- 1/h2*(-log(runif(n)))^(1) 
#   
#   # data
#   ti = c(T1, T2)
#   gr = c(rep(1, n), rep(0, n))
#   
#   l = coxph(Surv(ti) ~ gr)    # Fit Cox model
#   anova(l)$`Pr(>|Chi|)`[2]   # P-value 
# }

# unequal size
sim1 = function(h,h2,n, r) {
  
  n1 <- round(n / (r + 1))
  n2 <- round(r * n1)
  
  # Simulate some time to event data
  T1 <- 1/h* (-log(runif(n1)))^(1)   
  T2 <- 1/h2*(-log(runif(n2)))^(1) 
  
  # data
  ti = c(T1, T2)
  gr = c(rep(1, n1), rep(0, n2))
  
  l = coxph(Surv(ti) ~ gr)    # Fit Cox model
  anova(l)$`Pr(>|Chi|)`[2]   # P-value 
}

# total sample size
pv <-  replicate(1999, sim1(n= 100 , h =0.0866434, h2=0.1237763, r=1) )  #placebo trt, 
  mean(pv <= 0.05)  


# pop parameters--------------------------------

sims=2000

n <- seq(10, 400, by = 10)  #sample sizes
 
p1 <- c(8   ,   8)          # time at median survival
p2 <- c(8*.7,8*.5)          # trt effect hypothesised for time at median survival

p1 <- log(2)/p1  # placebo hazard
p2 <- log(2)/p2  # trt hazard

hr <- p1/p2 # hazard ratios

#-----------------------------------------------



# res = matrix(NA, length(n), length(p1))  # store results
# 
# 
# for (k in 1: length(p1)) {   # explore all hypotheses
#   
#   for (i in 1:length (n)) {  # explore all sample sizes
#   
#     #execute my function
#     pv <-  replicate(sims, sim(n = n[i]*.5, h = p1[k], h2=p2[k]) )  #placebo trt,  note the 0.5
#     res[i,k] <-  mean(pv <= 0.05)  
#     
#   }
#   
# }


res1 = matrix(NA, length(n), length(p1))


for (k in 1: length(p1)) {
  
  for (i in 1:length (n)) {
    
    #execute my function
    pv <-  replicate(sims, sim1(n= n[i] , h = p1[k], h2=p2[k] , r=1) )  #placebo trt, 
    res1[i,k] <-  mean(pv <= 0.05)  
    
  }
  
}


res1 <- data.frame(res1)
res1 <- cbind(n,res1)

# res  <- data.frame(res)
# res  <- cbind(n,res )

#names(res) <-  c("n", paste0("HR=",eval(p1[1]/p2[1])), paste0("HR=",eval(p1[2]/p2[2])))
names(res1) <- c("n", paste0("HR=",eval(p1[1]/p2[1])), paste0("HR=",eval(p1[2]/p2[2]))) # unequal

#x <-  reshape2::melt(res,  id.vars="n")
x1 <- reshape2::melt(res1, id.vars="n")

#x$ratio  <- "1:1"
x1$ratio <- "1:1"


x <-  x1 

names(x) <- c("n","hr","Power", "ratio")
 
all <-ggplot(x, aes(x = n, y = Power, linetype=ratio,
                    group = interaction(ratio, hr),
                    colour = hr)) +
  geom_line() +
  #scale_color_manual(labels = c("HR=0.5\n(50% reduction)", "HR=0.7\n(30% reduction)"
  scale_color_manual(labels = c("HR=0.7\n(70% reduction)", "HR=0.5\n(50% reduction)"
                                                              
                                
  ), values = c("green", "red", "blue", "purple")) +
  
  scale_linetype_manual(values=c("solid",   "dashed")) +
  
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),  limits = c(0,1)) +
  scale_x_continuous(breaks = seq(0,max(n),50), limits = c(0,max(n))) +
  theme_bw() +
  #  theme(legend.position="none") +
  theme( 
    plot.title=element_text(size = 12), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
    legend.text=element_text(size=10),
    legend.title=element_text(size=10),
    legend.position="right",
    
    legend.key.size = unit(12, "pt"),  # legend horizontal bar length
   
  
    axis.text.x  = element_text(size=10),
    axis.text.y  = element_text(size=10),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    plot.caption=element_text(hjust = 0, size = 7),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = rel(1), angle = 90),
    axis.title.x = element_text(size = rel(1), angle = 0 ),
 
    
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    plot.background = element_rect(fill = 'white', colour = 'white')
  ) +
 
  
  labs(y="Power", x = 'Total Number of events', color = "Hazard ratio", linetype="Ratio" )+
  #labs(caption = "- Alpha the type I assertion probability = 0.05, (note total number of patients requires information on accrual and total follow up times) \n- Uses method of Schoenfeld two-sided test for the ratio of two hazards. \n- Schoenfeld DA. Sample-size formula for the proportional-hazards regression model. Biometrics 1983;39:499-503.\n- Ratio of experimental to control sample size where 'experimental' is the same as the group with hazard represented in the numerator of the hazard ratio  ") +
  labs(caption = paste("- Alpha the type I assertion probability = 0.05 two sided, (total number of patients requires information on accrual and total follow up times). Ratio 2 pertains to larger sample in experimental arm \n- Uses method of Schoenfeld two-sided test for the ratio of two hazards. Assuming exponential hazards.\n- Schoenfeld DA. Sample-size formula for the proportional-hazards regression model. Biometrics 1983;39:499-503.\n" )) +
  
   ggtitle(paste0("Endpoint option 2. Power (or number of events) for hazard ratio, for the evaluation of LOS in ICU \n(baseline ICU of 8 days and the evaluation of 50% and 30% reduction in LOS)") ) 


#https://stackoverflow.com/questions/15059093/ggplot2-adjust-the-symbol-size-in-legends
all <- all + guides(color = guide_legend(override.aes = list(linewidth = 3 )))

all


pdf("Endpoint option 2 ICU LOS based on time to event approach.pdf", width=10, height=6) # open an appropriate graphics device
print(all)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 4 of 5") 
dev.off()











#-----------------------------------MEANS
#background, where I get SD from

# from paper
p2 <- c( rep(43*.5/352, 2),  rep(77*.5/352, 2)  , rep(89*.5/352, 2)  , rep(65*.5/352, 2)  , rep(33*.5/352, 2) ,
         rep(24*.5/352, 2) , rep(21*(1/13)/352, 13)   )

X <- rep(0 : 24, 352 * p2)
c(mean=mean(X), median=median(X)) # mean is lower than 8
sd(X)
#------------------------------------------------------------------

#using harrel code pomodm, transform to higher baseline mean in keeping with request

ip <- function(ep) {
  p <- c(-diff(ep), ep[length(ep)])
  if (abs(sum(p) - 1) > 1e-07) 
    stop("logic error")
  p
}

cp <- function(p) c(1, 1 - cumsum(p)[-length(p)])

pmod <- function(p, or) {
  ep <- cp(p)
  ep <- plogis(qlogis(ep) + log(or))
  ip(ep)
}

# new baseline mean 8, px
px <- pmod(p2, or=2.7269)  # new baseline
X <- rep(0 : 24, 100000 * px)
c(mean=mean(X), median=median(X))
sd(X)

#-----------start
n= seq(3,75, 1)

sdx <- sd(X)

res = matrix(NA, length(n), 5)

sofamean2 <- function(u0, u1, sd, ratio=1){
  
  for (i in 1: length(n)) {
    
    zz <- MESS::power_t_test(n=n[i], sd=sd, power=NULL, ratio=ratio, sd.ratio=1, delta=u0-u1)
    
    res[i,] <- c( u0, u1 , round(zz$power,3), zz$n[1], zz$n[2])
    
  }
  
  
  res <- data.frame(res)
  names(res) <- c( "u0", "u1","power" , "n1", "n2")
  return(res)
  
  
}

A <-  sofamean2(u0=5, u1=8, sd=sdx,  ratio=1)
B <-  sofamean2(u0=4, u1=8, sd=sdx,  ratio=1)
C <-  sofamean2(u0=3, u1=8, sd=sdx,  ratio=1)
D <-  sofamean2(u0=5, u1=8, sd=sdx,  ratio=2)
E <-  sofamean2(u0=4, u1=8, sd=sdx,  ratio=2)
F <-  sofamean2(u0=3, u1=8, sd=sdx,  ratio=2)



res <- rbind(A, B , C, D, E, F) 


#----------------------------------------------------------------------------------

# mean(rep(0 : 24, 100000 * pmod(px, or=c(OR))))

x <- res

x$ratio <- ifelse(is.na(res$n2), "1:1","2:1")

x$n2 <- ifelse(is.na(x$n2), x$n1, x$n2)


x$n <- rowSums( cbind (x$n1 ,x$n2), na.rm=TRUE)


x$ratio <- factor(x$ratio)
x$OR <- factor(paste(x$u0, "->",x$u1))


all <-ggplot(x, aes(x = n, y = power, linetype=ratio,
                    group = interaction(ratio, OR),
                    colour = OR)) +
  geom_line() +
  scale_color_manual(labels = c("Mean SOFA 8->3", "Mean SOFA 8->4", "Mean SOFA 8->5"
                                
                                
  ), values = c("red", "green", "blue")) +
  
  scale_linetype_manual(values=c("solid",   "dashed")) +
  
  
  #  geom_vline(xintercept = seq(0, 700, by = 50),  col = "gray", linetype = "solid",  linewidth = .25) +
  #   geom_hline(yintercept = seq(0, 1, by = 0.1), col = "gray", linetype = "solid",  linewidth = .25) +
  
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),  limits = c(0,1)) +
  scale_x_continuous(breaks = seq(0,150,10), limits = c(0,150)) +
  theme_bw() +
  #  theme(legend.position="none") +
  theme( 
    plot.title=element_text(size = 12), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
    legend.text=element_text(size=10),
    legend.title=element_text(size=10),
    legend.position="right",
    
    legend.key.size = unit(12, "pt"),  # legend horizontal bar length
    # legend.key.height = unit(0.1, "mm"),
    
    
    
    #  legend.key.width = unit(5, "pt"),
    #legend.title = element_text(size = 16),
    #legend.text = element_text(size = 12),
    # legend.key.size = unit(.2, "cm"),
    
    
    
    axis.text.x  = element_text(size=10),
    axis.text.y  = element_text(size=10),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    plot.caption=element_text(hjust = 0, size = 7),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = rel(1), angle = 90),
    axis.title.x = element_text(size = rel(1), angle = 0 ),
    # panel.grid.major.x = element_line(color = "grey80", linetype="dotted", linewidth = 1),
    # panel.grid.major.y = element_line(color = "grey80", linetype="dotted", linewidth = 1),
    # strip.background = element_rect(colour = "black", fill = "#ececf0"),
    # panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
    # plot.background = element_rect(fill = '#ececf0', colour = '#ececf0')
    
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    plot.background = element_rect(fill = 'white', colour = 'white')
  ) +
  # geom_hline(yintercept=1,  coloaur="#008000", linetype="solid") +
  
  # labs(y="Power", x = 'Total Sample Size', color = "Population probabilities") +  
  
  labs(y="Power", x = 'Total sample size', color = "Total sample size", linetype="Ratio p1:p2" )+
  labs(caption = paste("- Alpha the type I assertion probability = 0.05 two sided. Ratio 2:1 pertains to larger sample in experimental arm\n- SD and mean informed from, SOFA baseline distribution Fig 1a JAMA 10 Oct 2001 Vol 286,14 'Serial Evaluation of the SOFA Score...'\n- The SOFA score is an ordinal score from 0-24. Power/sample size is based on ttest and assuming normal distributed means due to central limit theorem.\n" )) +  
  
  # labs(caption = c(as.character(Sys.Date()),
  #                  "My caption")) + 
  
  ggtitle(paste0("Endpoint option 3. Power (or sample size) for SOFA score (difference in means)") ) 


#https://stackoverflow.com/questions/15059093/ggplot2-adjust-the-symbol-size-in-legends
all <- all + guides(color = guide_legend(override.aes = list(linewidth = 3 )))

all



pdf("Endpoint option 3 SOFA score reduction based on means.pdf", width=10, height=6) # open an appropriate graphics device
print(all)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 5 of 5") 
dev.off()

 


txt1 <-  c("A note on study design, sample size and power")

txt2 <-  c("When designing studies the Type II error is often referred to as the sponsor’s risk as it is upon the sponsor that the cost of this error falls. Often,
instead of referring to the Type II error, reference is made to the power of a study. The power is the probability that we will detect a difference of a specified size,
if there is one. That is, it is the probability of not making a Type II error. The power, therefore, is probability of accepting the alternative hypothesis when it is true.\n
[Julious, Steven An introduction to statistics in early phase trials / Steven Julious, David Machin, Say Beng Tan.3.2.3 TYPE I AND TYPE II ERROR]")

txt3 <-  c("An issue with sample size calculations for early phase trials, is that, by definition, we often have very little information on which to base the sample size calculation.
Most studies use established values for alpha of 5% (rarely higher) and power of 80% (occasionally 90%). Note, moving from 90% to 80% power does not seem \nsuch a great step to make, but in effect we are doubling the Type II error for a modest actual reduction (around 25%) in the sample size.\n 
[Julious, Steven An introduction to statistics in early phase trials Steven Julious, David Machin, Say Beng Tan. p39]")

txt4 <-  c("The FDA's draft guidance on power for phase III clinical trials is as follows: \n\n'The power of a phase III clinical trial should be sufficient to detect a clinically meaningful difference in efficacy between the investigational drug and \nthe control treatment. In general, a power of 80% or higher is considered to be adequate. However, the specific power level required will depend on the 
nature of the disease being treated, the expected treatment effect size, and the acceptable risk of false positives.'\n") 

txt5 <-  c("Note that the FDA will usually require that two phase III trials are ‘significant’, the two-trials rule (Although the FDA may permit exceptions). 
The probability of type II error (1-Power) is conventionally set at 10% to 20%; \nit is in the sponsor’s interest to keep this figure as low as feasible especially in the case of trials that are difficult or impossible to repeat. \n
[Statistical Issues in Drug Development Second Edition Stephen Senn 12.2.8 The two-trials rule;  https://www.fda.gov/media/172166/download]\n")

txt6 <-  c("\nCont'd\n\nStephen Senn makes the point that 'Studies are often designed or claimed to have 80% power against a key alternative when using a 0.05 significance level, 
although in execution often have less power due to unanticipated problems such as low subject recruitment. \nThus, if the alternative is correct and the actual power of two studies is 80%, the chance that the studies will both show P<0.05 will at best be only 0.80(0.80) = 64%; 
furthermore, the chance that one study shows P < 0.05 and the other does not (and thus will be misinterpreted as showing conflicting results) 
is 2(0.80)0.20 = 32% or about 1 chance in 3.\n 
[https://link.springer.com/content/pdf/10.1007/s10654-016-0149-3.pdf]")

txt7 <-  c("FDA have stated 'Typically, the number of subjects in Phase 2 studies ranges from a few dozen to about 300' and Ph III 'The number of subjects usually ranges from \nseveral hundred to about 3,000 people'. \n\nSenn again, 'If many successful trials of a given type have been run in a given indication, then the typical size of such a trial gives a good indication of what is likely to be \nsuccessful'.\n 
[https://www.fda.gov/drugs/information-consumers-and-patients-drugs/fdas-drug-review-process-ensuring-drugs-are-safe-and-effective, \nhttps://www.fda.gov/media/82381/download]")

txt8 <-  c("Footnote:  Concepts used in designing clinical trials: \n\nA type I error is committed by rejecting the null hypothesis when it is true and a type II error is committed by
rejecting the alternative hypothesis when it is true. The probability of not committing a type II error, given that the alternative hypothesis is true, \nis referred to as the power of the test.")

# pdf("Note.pdf", width=10, height=6)
# print(txt)
# makeFootnote(paste(Sys.Date() )) 
# makeFootnoteL("Page 1 of 4") 
# dev.off()
#  

cexx <- .6
pdf("Note.pdf", width=10, height=6)
#pdf("Note.pdf", paper="a4")
plot.new()
text(0, 1, txt1, font=4, cex=cexx, col="black", pos = 4)
text(0, .8, txt2, font=4, cex=cexx, col="black", pos = 4)
text(0, .6, txt3, font=4, cex=cexx, col="black", pos = 4)
text(0, .4 ,txt4, font=4, cex=cexx, col="black", pos = 4)
text(0, .2 ,txt5, font=4, cex=cexx, col="black", pos = 4)
#text(0, .1, txt6, font=4, cex=cexx, col="black", pos = 4)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 1 of 5") 
dev.off()

cexx <- .6
pdf("Note2.pdf", width=10, height=6)
#pdf("Note.pdf", paper="a4")
plot.new()
#text(0, .8 ,txt5, font=4, cex=cexx, col="black", pos = 4)
text(0, .8, txt6, font=4, cex=cexx, col="black", pos = 4)
text(0, .5, txt7, font=4, cex=cexx, col="black", pos = 4)
#text(0, .2, txt8, font=4, cex=cexx, col="black", pos = 4)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 2 of 5") 
dev.off()









qpdf::pdf_combine(input = c("Note.pdf","Note2.pdf","Endpoint option 1 mortality reduction proportions.pdf",
                            "Endpoint option 2 ICU LOS based on time to event approach.pdf" ,
                            "Endpoint option 3 SOFA score reduction based on means.pdf"),
                  output = "20231024_Power_sample_size_request.pdf")  









