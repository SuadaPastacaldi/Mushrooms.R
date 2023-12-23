#################################################################################################################################
#Univariate frequency distribution

############################ STEM HEIGHT ####################################
#histogram
hist(champi2$`Stem Height (m)`, 
     main = "Stem's height",
     xlab = "Height classes (cm)",
     ylab = "Frequency",
     breaks = 20,
     col = "lightblue", border = "blue")

mean(champi2$`Stem Height (m)`)
quantile(champi2$`Stem Height (m)`)
sd(champi2$`Stem Height (m)`)

############################ GILL COLOR ####################################
##changing colors
#palette for gill colors
champi.gill<-c( "black", "brown","antiquewhite","darkgrey", "green","lightgray","orange", "pink" ,
               "purple","red","white","yellow")
               
barplot(table(champi2$`Gill Colour`), col=champi.gill)

#measures of central tendency
table(champi2$`Gill Colour`)
summary(champi2$`Gill Colour`)
nrow(champi2)
freq<-table(champi2$`Gill Colour`)/ nrow(champi2)
freqperc<-freq*100
freqperc
barplot(freqperc, 
        col= champi.gill,
        ylab = "Percentage",
        xlab = "Gill colors",
        main = "Percentage frequencies of Gill colors")
#highest percentage in white-> mode

#####################################################################################################################################
#Bivariate analysis

############################## GILL COLOR - CLASS ####################################

#is gill color a proxy for poisonousness?
table(champi2$`Gill Colour`,champi2$Class)
summary(table(champi2$`Gill Colour`,champi2$Class))
barplot(table(champi2$`Gill Colour`,champi2$Class),
        col=champi.gill,
        xlab="Class",
        ylab = "Number of specimens",
        main="Is gill color a proxy for poisonousness?")

#interdependence

#chi square test
ctest <-chisq.test(table(champi2$`Gill Colour`,champi2$Class))
xsq<-ctest$statistic

#cramer v 
v<-sqrt(xsq/nrow(champi2))
v
#v=0.3 poor interdependance

######################################## BRUSE/BLEED - CLASS ###############################


#Is bruising/bleeding a proxy for poisonousness?
#palette bb
color.bb<-c("#FFF8DC","#9932CC")

summary(champi2$`Bruise or Bleed`)
table(champi2$`Bruise or Bleed`,champi2$Class)
freq1<-table(champi2$`Bruise or Bleed`,champi2$Class)/nrow(champi2) * 100
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

#interdependence
#chi square test
ctest1 <-chisq.test(table(champi2$`Bruise or Bleed`,champi2$Class))
xsq1<-ctest1$statistic

#cramer v 
v<-sqrt(xsq1/nrow(champi2))
v
#v=0.13 very poor interdependance

#################################### STEM HEIGHT - STEM WIDTH ###############################
#scatterplot
plot(champi2$`Stem Height (m)`, champi2$`Stem Width (m)`,
     xlab ="Width",
     ylab = "Height",
     main = "Stem's dimentions",
     col= "black",
     cex= .3,
     pch=20)

#variance
varheight<-var(champi2$`Stem Height (m)`)
varwidth<-var(champi2$`Stem Width (m)`)

#covariance
co<-cov(champi2$`Stem Height (m)`, champi2$`Stem Width (m)`, use="complete.obs")

#correlation coefficient
cor(champi2$`Stem Height (m)`, champi2$`Stem Width (m)`, use="complete.obs")
r<-co/sqrt(varheight*varwidth)
r
#r=cor=0.3
#positive correlation but very poor, cor coeff is very close to 0

#regression line in scatterplot
model<-lm(champi2$`Stem Height (m)` ~ champi2$`Stem Width (m)`)
model
summary(model)
abline(model, col="red",lwd=2)

#function that describes correlation
#Y=beta1*x + beta0 
beta1<-4.80983
beta0<-0.08994
#slaitely positive correlation, almost none-> regression line with beta1 close to 0

