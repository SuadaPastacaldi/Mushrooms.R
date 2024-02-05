#################################################################################################################################
#Univariate frequency distribution

############################ STEM HEIGHT ####################################
#stemm's height is a numeric continuous variable
#so that it can be described by an histogram
hist(Champi2$`Stem Height (cm)`, 
     main = "Stem's height",
     xlab = "Height classes (cm)",
     ylab = "Frequency",
     breaks = 20,
     col = "lightblue", border = "blue")


mean(Champi2$`Stem Height (cm)`)
quantile(Champi2$`Stem Height (cm)`)
#distribution is unimodal with mode equal to 5.73cm, slaigthly positevely skewed.
#with mean = 5.8345cm

#investigating the variability of the population
#variance
sigmasquared<-mean(Champi2$`Stem Height (cm)`^2)-mean(Champi2$`Stem Height (cm)`)^2

#standard deviation <- sigma
sigma<-sqrt(sigmasquared)
sd(Champi2$`Stem Height (cm)`)
sigma

#coefficent of variation
CV<-sd(Champi2$`Stem Height (cm)`)/mean(Champi2$`Stem Height (cm)`)
CV

#moderate variability 


############################ GILL COLOR ####################################
#qualitative nominal variable
#can be described by a barplot or a piechart

#changing colors
#palette for gill colors
champi.gill<-c( "black", "brown","antiquewhite","darkgrey", "green","lightgray","orange", "pink" ,
                "purple","red","white","yellow")

#graphs
barplot(table(Champi2$`Gill Color`), col=champi.gill)
pie(table(Champi2$`Gill Color`), col=champi.gill)

#measures of central tendency
table(Champi2$`Gill Color`)
summary(Champi2$`Gill Color`)
nrow(Champi2)
freq<-table(Champi2$`Gill Color`)/ nrow(Champi2)
freqperc<-freq*100
freqperc
barplot(freqperc, 
        col= champi.gill,
        ylab = "Percentage",
        xlab = "Gill colors",
        main = "Percentage frequencies of Gill colors")
#highest percentage in white-> mode
#the mode is to have white gills (30%), followed by brown (14%) and yellow (14%)

#####################################################################################################################################
#Bivariate analysis

############################## GILL COLOR - CLASS ####################################


#is gill color a proxy for poisonousness?
table(Champi2$`Gill Color`,Champi2$Class)
summary(table(Champi2$`Gill Color`,Champi2$Class))
barplot(table(Champi2$`Gill Color`,Champi2$Class),
        col=champi.gill,
        xlab="Class",
        ylab = "Number of specimens",
        main="Is gill color a proxy for poisonousness?")

#gill colour is not a good proxy for mushroom's poisonouness, 
#most colors are evenly distributed between edibles and poisonous mushrooms
#the color green makes an exeption,according to this data if one has green gills then it is poisonous.

#interdependence

#chi square test
ctest <-chisq.test(table(Champi2$`Gill Color`,Champi2$Class))
xsq<-ctest$statistic

#cramer v 
v<-sqrt(xsq/nrow(Champi2))
v

#being cramer v index a value between 0 and 1, where 0 is perfect independence
#in this case we found a poor interdependance, confirming that gill color it is not a very good proxy for poisonouness

######################################## BRUSE/BLEED - CLASS ###############################


#Is bruising/bleeding a proxy for poisonousness?
#palette bb
color.bb<-c("#FFF8DC","#9932CC")

summary(Champi2$`Bruise or Bleed`)
table(Champi2$`Bruise or Bleed`,Champi2$Class)
freq1<-table(Champi2$`Bruise or Bleed`,Champi2$Class)/nrow(Champi2) * 100
freq1

barplot(freq1,
        col = color.bb, xlab="Class",
        ylab="Percentage",
        main="Is bruising/bleeding a proxy for poisonousness?",
        ylim=c(0,70))


legend("bottomright",
       title=NULL,
       cex = 0.6,
       legend = c("Neither","Bruise or Bleed"),
       fill=color.bb,
       bty = "o")

#as we can see from the barplot both the mushrooms that bleed or bruise and the ones who do not can be found in both classes


#interdependence
#chi square test
ctest1 <-chisq.test(table(Champi2$`Bruise or Bleed`,Champi2$Class))
xsq1<-ctest1$statistic

#cramer v 
v<-sqrt(xsq1/nrow(Champi2))
v

#being cramer v index a value between 0 and 1, where 0 is perfect independence
#in this case we found very poor interdependance, confirming that bruising and bleeding are not a proxy for poisonouness
#not a proxy

#################################### STEM HEIGHT - STEM WIDTH ###############################
#two numeric continuous variables
#best representation with a satterplot
#scatterplot
plot(Champi2$`Stem Height (cm)`, Champi2$`Stem Width (cm)`,
     xlab ="Width",
     ylab = "Height",
     main = "Stem's dimentions (cm)",
     col= "black",
     cex= .3,
     pch=20)

#most individuals have stems around 2-10 cm wide and 2-30 cm long

#variance
varheight<-var(Champi2$`Stem Height (cm)`)
varwidth<-var(Champi2$`Stem Width (cm)`)

#covariance
co<-cov(Champi2$`Stem Height (cm)`, Champi2$`Stem Width (cm)`, use="complete.obs")

#correlation coefficient
cor(Champi2$`Stem Height (cm)`, Champi2$`Stem Width (cm)`, use="complete.obs")

r<-co/sqrt(varheight*varwidth)
r
#r=cor=0.3
#being r a value between -1 and 1 where 0 is the perfect independence of the two variables
#in this case we found a positive correlation but very poor, the coefficent is very close to 0

#regression line in scatterplot
model<-lm(Champi2$`Stem Height (cm)` ~ Champi2$`Stem Width (cm)`)
model
summary(model)
abline(model, col="red",lwd=2)


#function that describes correlation
#Y=beta1*x + beta0 
beta0<-4.80983 #intercept
beta1<-0.08994
#beta1 close to 0
#the regression line being almost parallel to the x axis confirms that the dependence between the two variables is almost null


#------------------------------------------------------------------------------------------------------------------
####SUADA####
#statistical inference on the variable stem height
#INTERVAL ESTIMATION
#since we have available a very large sample we can assume a normal distribution
#x~N(mu,sigma^2)
#where
sigma<-var(Champi2$`Stem Height (cm)`)
mu<-mean(Champi2$`Stem Height (cm)`)

#extract a random sample from the initial population
champish500<-sample(Champi2$`Stem Height (cm)`,500)

n<-500
x<-mean(champish500)


#to find a measure of accuracy of the estimate (x) of the actual mean (mu)
#standard error
SE.x<-sqrt(sigma^2/n)

#99% confidence interval for the estimate (x) of the actual mean (mu)
alpha=0.01

lower <- x - qnorm(p=1-alpha/2) * SE.x
lower
upper <- x + qnorm(p=1-alpha/2) * SE.x
upper
round(c(lower, upper), 2)
#we can say with a confidence of 99% that the actual mean can be found in this interval [5.208159-6.742001]

#HYPOTHESIS TESTING
#test wether the true mean of stem height is significantly different from 6cm (1%significance level)
#against the hypothesis that it is bigger

#H0:mu=6
#H1:mu>6

tsobs <- (x-6)/SE.x
#the null hypothesis is rejected if te observed testst tstatistics(tsobs) fall in the rejection zone
#Reject H0 if
tsobs > qnorm(p=1-alpha)  

#FALSE, tsobs does not fall in rejection zone
#though we cannot reject H0 at 1% significance level

#p-value
(1-pnorm(tsobs))
#every value of alpha bigger than 0.7824674 would lead to a rejection, 
#since our alpha was smaller the p-value confirms our result

library(BSDA)
#z.test
z.test(champish500, sigma.x = sigma)
z1 <- z.test(champish500, sigma.x = sigma, mu=6, alternative = "greater", conf.level = 0.99)
z1$statistic
tsobs
z1$p.value

#same tsobs and p value as previous calculations

z1$conf.int
c(lower, upper)
z1$estimate


# Inference on a normal population unknown variance
# we now assuming that the 500 individuals sample comes from a population with unknown variance
# estimate mean age
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

#test wheter the stem height mean is significantly different from 6 (1%significance level)
tsobs.un <- (x-6)/SE.xun
#Reject H0 if
tsobs> qt(p=1-0.01, df=n-1)
#Cannot Reject H0 at 1 % significance level

#we can also use t.test
t.test(champish500)
t1 <- t.test(champish500, mu=6, alternative = "greater")
t1$statistic
tsobs.un

t1$parameter
t1$p.value
#since our alpha was smaller results confirmed
t1$conf.int
c(lower, upper)
t1$stderr
#all same values obtained with previous calculations
