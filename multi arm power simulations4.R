
# eamonn extending binary functino to 3 arm trial

#ref https://stats.stackexchange.com/questions/467595/how-do-you-calculate-sample-sizes-for-multiple-treatments
# https://stats.stackexchange.com/questions/113602/test-if-two-binomial-distributions-are-statistically-different-from-each-other


rm(list=ls())
set.seed(874)

#global specs

upp <-200
xlow <- 0.1
sims=100000  # 1000 is quick , 10000 couple mins so ok. 100000 ?

formatz4 <- function(x){
  sprintf(x, fmt = '%#.4f')  
}
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

three.arm.cont=function(u0, u1, u2, n,  sigma0, sigma1, sigma2, alpha1=0.05 ,  nSims=sims)
{
  
  # sigma0=sqrt(u0*(1-u0)) #placebo se
  # sigma1=sqrt(u1*(1-u1)) #arm1 se
  # sigma2=sqrt(u2*(1-u2)) #arm2 se
  
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

## 

### this matches PASS , multi arm test for difference between treatmetn and control
# v  
# ###
# three.arm.cont(u0=8, u1=5, u2=5, sigma0=5.22, sigma1=5.22, sigma2=5.22, n=58, alpha=.05)
# 
#
three.arm.cont(u0=8, u1=6, u2=6, sigma0=5.22, sigma1=5.22, sigma2=5.22, n=170, alpha=.05)
three.arm.cont(u0=6, u1=4, u2=4, sigma0=5.22, sigma1=5.22, sigma2=5.22, n=170, alpha=.05)
three.arm.cont(u0=6, u1=4, u2=4, sigma0=5.22, sigma1=5.22, sigma2=5.22, n=130, alpha=.05)

#--------------------------------------------------------------------------------------
# the SD base on paper
# on masse simulations
require(tidyverse)
df_plot1 <- expand_grid(u0=8,
                        u1=c(6,5,4),   # deltas of 2 3 4 
                        n=seq(10,200, 1),
                        alpha=.05) 

df_plot1$u2 <- df_plot1$u1

df_plot1$disjunctive <- df_plot1$arm2 <- df_plot1$arm1 <- NA

for(i in 1 : dim(df_plot1)[1]) {
  
  x <- 
    three.arm.cont (u0=df_plot1$u0[i], sigma0=5.22, sigma1=5.22, sigma2=5.22,
                    u1=df_plot1$u1[i],  
                    u2=df_plot1$u2[i],
                    n=df_plot1$n[i],
                    alpha=df_plot1$alpha[i],  
    )
  
  df_plot1$arm1[i]   <-    as.numeric(x[2])
  df_plot1$arm2[i]   <-    as.numeric(x[4])
  df_plot1$disjunctive[i] <-  as.numeric(x[12]) 
}

#--------------------------------------------------------------------------------------
#global specs

upp <-200
xlow <- 0.1

#-- all in one plot

df <- df_plot1

df$u2 <- NULL
df$alpha <- NULL
df$arm2 <- NULL

names(df) <- c("u0","u1", "n", "separate", "disjunctive")
L <- reshape2::melt(df, id=c("u0", "u1","n"), variable.name="Test")


##---------------------------------------------------


df_plot1$u1[df_plot1$u1 == 4 ] <- 4 # paste("\u0394","4") 
df_plot1$u1[df_plot1$u1 == 5 ] <- 3# paste("\u0394","3") 
df_plot1$u1[df_plot1$u1 == 6 ] <- 2#paste("\u0394","2")  

legend_ord <- c(4,   #more power
                3,
                2     

)


df_plot1$u1  <- as.character(df_plot1$u1)


#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

threearm <- ggplot(data=df_plot1, aes(x=n, y=arm1, colour=factor(u1 ))) +
  geom_line()+
  labs(x=expression("Sample size in each arm"),
       y= "Power", colour =paste0("SOFA\n -\u0394 shift:")) +
  theme_bw() +
  theme(legend.position = "right")  +
  
  
  scale_color_manual(breaks=legend_ord, values= c(
    
    "4" ="red", 
    "3" ="green" ,
    "2" ="blue"

  )) + 
  
  scale_y_continuous(breaks = seq(xlow , 1, by = 0.1),
                     limits = c(xlow , 1)) +
  scale_x_continuous(breaks = seq(10, upp, 10), limits =   c(10,upp)) +
  theme(plot.caption = element_text(hjust = 0)) + # set the left align here
  
  geom_hline(yintercept=c(.8,.9), linetype="dotted", 
             color = "black", linewidth=.2) +
  
  ggtitle(paste0("Power and sample size, for a three arm RCT study, for delta reductions in outcome measure SOFA score\nPlacebo/SOC and two active treatment arms. Assumed placebo/SOC SOFA SD 5.22") ) +
  labs(caption = paste("- Alpha the type I assertion probability 0.05\n- Bonferroni adjusted z tests with unpooled variance; SOFA baseline SD derived from Fig 1a JAMA 10 Oct 2001 Vol 286,14\n- Final calculations will be performed using PASS/nQuery software. Ref: multi arm power simulations4.R" )) +
  
  
  guides(color = guide_legend(override.aes = list(linewidth = 3 )))


#-----------------------------------------------------------------------------------------
##prepare a table of power and N

y<- x<- matrix(NA, 3,3)

p1 <- c(paste("\u0394","4"),paste("\u0394","3"), paste("\u0394","2"))#, 0.35, 0.2, 0.25, 0.3,0.35)

# get n when power is met!
index <- min(which(df_plot1$arm1 > .8 & df_plot1$u1 %in% "2"))
delta2.80 <- df_plot1$n[index]
index <- min(which(df_plot1$arm1 > .9 & df_plot1$u1 %in% "2"))
delta2.90 <- df_plot1$n[index]

index <- min(which(df_plot1$arm1 > .8 & df_plot1$u1 %in% "3"))
delta3.80 <- df_plot1$n[index]
index <- min(which(df_plot1$arm1 > .9 & df_plot1$u1 %in% "3"))
delta3.90 <- df_plot1$n[index]

index <- min(which(df_plot1$arm1 > .8 & df_plot1$u1 %in% "4"))
delta4.80 <- df_plot1$n[index]
index <- min(which(df_plot1$arm1 > .9 & df_plot1$u1 %in% "4"))
delta4.90 <- df_plot1$n[index]

p80 <- c(delta4.80,delta3.80, delta2.80)
p90 <- c(delta4.90,delta3.90, delta2.90)

res <-as.data.frame(cbind(p1,p80, p90))

names(res) <- c(paste0("SOFA -\u0394 shift:"),"N per arm, power 80%", "N per arm, power 90%")


#---------------------------------------------------------------------------------------
require(gridExtra)
g <-   ttheme_default(base_size = 8,
                      core=list(
                        fg_params = list(col = c("black","black","white" )),
                        
                        bg_params = 
                          list(fill=c("red","green","blue" ))
                      )
                      
)

all2 <- threearm + theme(legend.position = "none")

gg <- all2+ annotation_custom(tableGrob(res, rows=NULL,
                                        theme = g), 
                              xmin=130, xmax=190, ymin=0, ymax=0.4)

pdf("Three arm SOFA.pdf", width=10, height=6) # open an appropriate graphics device
print(gg)
#makeFootnote(paste(Sys.Date() )) 
#makeFootnoteL("Page 1 of 2") 
dev.off()

ggsave("Three arm SOFA.png", width = 30, height = 20, units = "cm")


# pdf("Endpoint SOFA score reduction based on means (3 arms).pdf", width=10, height=6) # open an appropriate graphics device
# print(threearm)
# #makeFootnote(paste(Sys.Date() )) 
# #makeFootnoteL("Page 1 of 1") 
# dev.off()

