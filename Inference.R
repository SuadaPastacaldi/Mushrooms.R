########################### INFERENCE  #########################################

sigma<-var(champi2$`Stem Height (m)`)
mu<-mean(champi2$`Stem Height (m)`)


champish500<-sample(champi2$`Stem Height (m)`,500)

n<-500
x<-mean(champish500)
#5.97508

#measure of accuracy
#standard error
SE.x<-sqrt(sigma^2/n)

#99% confidence interval 
alpha=0.01

lower <- x - qnorm(p=1-alpha/2) * SE.x
lower
upper <- x + qnorm(p=1-alpha/2) * SE.x
upper
round(c(lower, upper), 2)
#[5.208159-6.742001]

#test wheter the mean stem height is significantly different from 6.5cm (1%significance level)
#against the hypothesis that is bigger

#H0:mu=6.5
#H1:mu>6.5

tsobs <- (x-6.5)/SE.x
#Reject H0 if 
tsobs > qnorm(p=1-alpha)  

#FALSE, tsobs not in rejection zone
#Cannot reject H0 at 1% significance level

#p-value
(1-pnorm(tsobs))

#z.test
install.packages("BSDA")
library(BSDA)
z.test(champish500, sigma.x = sigma)
z1 <- z.test(champish500, sigma.x = sigma, mu=6.5, alternative = "greater", conf.level = 0.99)
z1$statistic
tsobs

z1$p.value

z1$conf.int
c(lower, upper)
z1$stderr
z1$estimate


# Inference on a normal population unknown variance

#estimate mean age
x<-mean(champish500)
#measure of accuracy
SE.xun<-sd(champish500)/sqrt(n)
SE.xun
#99% confidence interval 
alpha=0.01
lower <- (x - qt(p=1-alpha/2, df=n-1) * SE.xun)
upper <- (x + qt(p=1-alpha/2, df=n-1) * SE.xun)
round(c(lower, upper), 2)
#[5.65 - 6.30]

#test wheter the stem height mean is significantly different from 6.5 (1%significance level)
tsobs.un <- (x-6.5)/SE.xun
#Reject H0 if 
tsobs> qt(p=1-0.01, df=n-1)
#Cannot Reject H0 at 1 % significance level

#we can also use t.test
t.test(champish500)
t1 <- t.test(champish500, mu=6.5, alternative = "greater")
t1$statistic
tsobs.un

t1$parameter
t1$p.value

t1$conf.int
c(lower, upper)
t1$stderr
