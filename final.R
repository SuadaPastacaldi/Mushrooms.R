####### Data Cleaning #######

#First we take the data downloaded from internet which is in a csv file
#and assign to a dataframe named Champi
Champi <- read.table("secondary_data.csv", header=T, sep=';', stringsAsFactors = T)

#We use the command View and summary to see and analyse the data
View(Champi)
summary(Champi)

#Eliminate the columns we don't need 
#And assign to a new dataframe named Champi1
Champi1 <- Champi[,-c(4,7,12,13,14,15,16,18,19)]

#We use the command View and summary to see and analyse the data
View(Champi1)
summary(Champi1)

#Check the type of data
#Change the type to Character
#Control the change
#Eliminate all the rows in which there is no data
#And assign to a new dataframe named Champi2
typeof(Champi1$gill.spacing)
Champi1$gill.spacing <- as.character(Champi1$gill.spacing)
typeof(Champi1$gill.spacing)
Champi2 <- Champi1 [-c(which(Champi1$gill.spacing=="")),]

#We use the command View and summary to see and analyse the data
View(Champi2)
summary(Champi2)

#Change the names of each column
names(Champi2) <- c("Class", "Cap Diameter (cm)", "Cap Shape", "Cap Color", 
                    "Bruise or Bleed", "Gill Spacing",
                    "Gill Color", "Stem Height (cm)", "Stem Width (cm)", 
                    "Ring", "Habitat", "Season")

###Change the names
Champi2$Class <- as.character(Champi2$Class)
typeof(Champi2$Class)
Champi2$Class[Champi2$Class=="p"] <- "Poisonous"
Champi2$Class[Champi2$Class=="e"] <- "Edible"
Champi2$Class <- as.factor(Champi2$Class)

#Check the type of variable
typeof(Champi2$`Cap Diameter (cm)`)

#Change type to character
Champi2$`Cap Shape` <- as.character(Champi2$`Cap Shape`)

#Change the names
Champi2$`Cap Shape`[Champi2$`Cap Shape`=="b"] <- "Bell"
Champi2$`Cap Shape`[Champi2$`Cap Shape`=="c"] <- "Conical"
Champi2$`Cap Shape`[Champi2$`Cap Shape`=="x"] <- "Convex"
Champi2$`Cap Shape`[Champi2$`Cap Shape`=="f"] <- "Flat"
Champi2$`Cap Shape`[Champi2$`Cap Shape`=="s"] <- "Sunken"
Champi2$`Cap Shape`[Champi2$`Cap Shape`=="p"] <- "Spherical"
Champi2$`Cap Shape`[Champi2$`Cap Shape`=="o"] <- "Others"

#Change type to factor
Champi2$`Cap Shape` <- as.factor(Champi2$`Cap Shape`)

#Check the variable
summary(Champi2$`Cap Shape`)

#Check the variable
summary(Champi2$Class)

#Change the type to character
Champi2$`Cap Color` <- as.character(Champi2$`Cap Color`)

#Change the names
Champi2$`Cap Color`[Champi2$`Cap Color`=="n"] <- "Brown"
Champi2$`Cap Color`[Champi2$`Cap Color`=="b"] <- "Buff"
Champi2$`Cap Color`[Champi2$`Cap Color`=="g"] <- "Gray"
Champi2$`Cap Color`[Champi2$`Cap Color`=="r"] <- "Green"
Champi2$`Cap Color`[Champi2$`Cap Color`=="p"] <- "Pink"
Champi2$`Cap Color`[Champi2$`Cap Color`=="u"] <- "Purple"
Champi2$`Cap Color`[Champi2$`Cap Color`=="e"] <- "Red"
Champi2$`Cap Color`[Champi2$`Cap Color`=="w"] <- "White"
Champi2$`Cap Color`[Champi2$`Cap Color`=="y"] <- "Yellow"
Champi2$`Cap Color`[Champi2$`Cap Color`=="l"] <- "Blue"
Champi2$`Cap Color`[Champi2$`Cap Color`=="o"] <- "Orange"
Champi2$`Cap Color`[Champi2$`Cap Color`=="k"] <- "Black"

#change again to the original type of variable
Champi2$`Cap Color` <- as.factor(Champi2$`Cap Color`)

summary(Champi2$`Cap Color`)

Champi2$`Bruise or Bleed` <- as.character(Champi2$`Bruise or Bleed`)
Champi2$`Bruise or Bleed`[Champi2$`Bruise or Bleed`=="t"] <- TRUE
Champi2$`Bruise or Bleed`[Champi2$`Bruise or Bleed`=="f"] <- FALSE
Champi2$`Bruise or Bleed` <- as.logical(Champi2$`Bruise or Bleed`)
summary(Champi2$`Bruise or Bleed`)

Champi2$`Gill Spacing` <- as.character(Champi2$`Gill Spacing`)
Champi2$`Gill Spacing`[Champi2$`Gill Spacing`=="c"] <- "Close"
Champi2$`Gill Spacing`[Champi2$`Gill Spacing`=="d"] <- "Distant"
Champi2$`Gill Spacing`[Champi2$`Gill Spacing`=="f"] <- "None"
Champi2$`Gill Spacing` <- as.factor(Champi2$`Gill Spacing`)
summary(Champi2$`Gill Spacing`)

Champi2$`Gill Color` <- as.character(Champi2$`Gill Color`)
Champi2$`Gill Color`[Champi2$`Gill Color`=="n"] <- "Brown"
Champi2$`Gill Color`[Champi2$`Gill Color`=="b"] <- "Buff"
Champi2$`Gill Color`[Champi2$`Gill Color`=="g"] <- "Gray"
Champi2$`Gill Color`[Champi2$`Gill Color`=="r"] <- "Green"
Champi2$`Gill Color`[Champi2$`Gill Color`=="p"] <- "Pink"
Champi2$`Gill Color`[Champi2$`Gill Color`=="u"] <- "Purple"
Champi2$`Gill Color`[Champi2$`Gill Color`=="e"] <- "Red"
Champi2$`Gill Color`[Champi2$`Gill Color`=="w"] <- "White"
Champi2$`Gill Color`[Champi2$`Gill Color`=="y"] <- "Yellow"
Champi2$`Gill Color`[Champi2$`Gill Color`=="l"] <- "Blue"
Champi2$`Gill Color`[Champi2$`Gill Color`=="o"] <- "Orange"
Champi2$`Gill Color`[Champi2$`Gill Color`=="k"] <- "Black"
Champi2$`Gill Color`[Champi2$`Gill Color`=="f"] <- "Other"
Champi2$`Gill Color` <- as.factor(Champi2$`Gill Color`)
summary(Champi2$`Gill Color`)

typeof(Champi2$`Stem Height (cm)`)
typeof(Champi2$`Stem Width (cm)`)

Champi2$`Ring` <- as.character(Champi2$`Ring`)
Champi2$`Ring`[Champi2$`Ring`=="t"] <- TRUE
Champi2$`Ring`[Champi2$`Ring`=="f"] <- FALSE
Champi2$`Ring` <- as.logical(Champi2$`Ring`)
summary(Champi2$`Ring`)

Champi2$`Habitat` <- as.character(Champi2$`Habitat`)
Champi2$`Habitat`[Champi2$`Habitat`=="g"] <- "Grasses"
Champi2$`Habitat`[Champi2$`Habitat`=="l"] <- "Leaves"
Champi2$`Habitat`[Champi2$`Habitat`=="m"] <- "Meadows"
Champi2$`Habitat`[Champi2$`Habitat`=="p"] <- "Paths"
Champi2$`Habitat`[Champi2$`Habitat`=="h"] <- "Heaths"
Champi2$`Habitat`[Champi2$`Habitat`=="u"] <- "Urban"
Champi2$`Habitat`[Champi2$`Habitat`=="w"] <- "Waste"
Champi2$`Habitat`[Champi2$`Habitat`=="d"] <- "Woods"
Champi2$`Habitat` <- as.factor(Champi2$`Habitat`)
summary(Champi2$Habitat)


Champi2$`Season` <- as.character(Champi2$`Season`)
Champi2$`Season`[Champi2$`Season`=="s"] <- "Spring"
Champi2$`Season`[Champi2$`Season`=="u"] <- "Summer"
Champi2$`Season`[Champi2$`Season`=="a"] <- "Autumn"
Champi2$`Season`[Champi2$`Season`=="w"] <- "Winter"
Champi2$`Season` <- as.factor(Champi2$`Season`)
summary(Champi2$Season)


Champi2$HeightClass <- cut(Champi2$`Stem Height (cm)`,
                           breaks = c(-1,4.48,6.89,30),labels = 
                             c("Short","Medium","Tall"))
summary(Champi2$HeightClass)

write.csv(Champi2,"Champi_Clean.csv",row.name=TRUE)

######### Descriptive Analysis #########

library(RColorBrewer)

summary(Champi2$`Stem Width (cm)`)
boxplot(Champi2$`Stem Width (cm)`, main="Stem Width",
        xlab = "Stem Width",
        ylab = "Width (cm)")
hist(Champi2$`Stem Width (cm)`,breaks = 200,
     col="khaki",main="Stem Width",
     xlab = "Width (cm)",
     ylab = "Frequency")

table(Champi2$Ring)
pie(table(Champi2$Ring),main="Ring Presence",
    labels = c("No (77.45%)","Yes (22.55%)"),
    col = c("khaki","lightblue"))


table(Champi2$Habitat)
barplot(table(Champi2$Habitat),
        main="Habitats",
        xlab = "Habitat Type",
        ylab = "Frequency",
        col = brewer.pal(n=8,name="Spectral"))

table(Champi2$Season)
barplot(table(Champi2$Season),
        main="Seasons",
        xlab = "Season",
        ylab = "Frequency",
        col = brewer.pal(n=4,name="Spectral"))


boxplot(
  Champi2$`Stem Height (cm)`[Champi2$Season=="Summer"],
  Champi2$`Stem Height (cm)`[Champi2$Season=="Autumn"],
  Champi2$`Stem Height (cm)`[Champi2$Season=="Winter"],
  Champi2$`Stem Height (cm)`[Champi2$Season=="Spring"],
  outline = FALSE
)




barplot(table(Champi2$`Cap Color`[Champi2$Class=="Edible"]), col=brewer.pal(n=12, name="Paired"))

pie(table(Champi2$Class), col= c("red", "blue"))

barplot(table(Champi2$'Cap Shape'[Champi2$Class=="Poisonous"]), col=brewer.pal(n=7, name="Spectral"))

plot(Champi2$`Cap Diameter (cm)`, Champi2$`Stem Width (cm)`, xlab = "diameter", ylab="width", main="Mushrooms", col="blue")

plot(Champi2$`Cap Diameter (cm)`, Champi2$`Stem Height (cm)`, xlab = "diameter", ylab="height", main="Mushrooms", col="green")

plot(table(Champi2$'Cap Diameter (cm)'[Champi2$Class=="Poisonous"]))  ##no se que esta graphing ni como poner nombres a los axis

boxplot(table(Champi2$'Cap Diameter (cm)'[Champi2$Class=="Poisonous"]))

##cual de los sunken edible son mas grande que 5?
##freq of flat cap shape?

###covariance
mean(Champi2$`Cap Diameter (cm)`*Champi2$`Stem Height (cm)`)-mean(Champi2$`Cap Diameter (cm)`)*mean(Champi2$`Stem Height (cm)`)
cov(Champi2$`Cap Diameter (cm)`,Champi2$`Stem Height (cm)`)
#correlation
cor(Champi2$`Cap Diameter (cm)`,Champi2$`Stem Height (cm)`)







#################################################################################################################################
#Univariate frequency distribution

############################ STEM HEIGHT ####################################
#histogram
hist(Champi2$`Stem Height (cm)`, 
     main = "Stem's height",
     xlab = "Height classes (cm)",
     ylab = "Frequency",
     breaks = 20,
     col = "lightblue", border = "blue")


mean(Champi2$`Stem Height (cm)`)
quantile(Champi2$`Stem Height (cm)`)

#variance
sigmasquared<-mean(Champi2$`Stem Height (cm)`^2)-mean(Champi2$`Stem Height (cm)`)^2

#standard deviation <- sigma
sigma<-sqrt(sigmasquared)
sd(Champi2$`Stem Height (cm)`)
sigma

############################ GILL COLOR ####################################
##changing colors
#palette for gill colors
champi.gill<-c( "black", "brown","antiquewhite","darkgrey", "green","lightgray","orange", "pink" ,
                "purple","red","white","yellow")

barplot(table(Champi2$`Gill Color`), col=champi.gill)

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

#interdependence

#chi square test
ctest <-chisq.test(table(Champi2$`Gill Color`,Champi2$Class))
xsq<-ctest$statistic

#cramer v 
v<-sqrt(xsq/nrow(Champi2))
v
#v=0.3 poor interdependance

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

#interdependence
#chi square test
ctest1 <-chisq.test(table(Champi2$`Bruise or Bleed`,Champi2$Class))
xsq1<-ctest1$statistic

#cramer v 
v<-sqrt(xsq1/nrow(Champi2))
v
#v=0.13 very poor interdependance

#################################### STEM HEIGHT - STEM WIDTH ###############################
#scatterplot
plot(Champi2$`Stem Height (cm)`, Champi2$`Stem Width (cm)`,
     xlab ="Width",
     ylab = "Height",
     main = "Stem's dimentions",
     col= "black",
     cex= .3,
     pch=20)

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
#positive correlation but very poor, cor coeff is very close to 0

#regression line in scatterplot
model<-lm(Champi2$`Stem Height (cm)` ~ Champi2$`Stem Width (cm)`)
model
summary(model)
abline(model, col="red",lwd=2)

#function that describes correlation
#Y=beta1*x + beta0 
beta1<-4.80983
beta0<-0.08994
#slaitely positive correlation, almost none-> regression line with beta1 close to 0




#####Inference####
###PEDRO####

library(BSDA)
require(BSDA)

CapDiamMeanPop <- mean(Champi2$`Cap Diameter (cm)`)
CapDiamVarPop <- var(Champi2$`Cap Diameter (cm)`)
n <- 500
SECapDiam <- sqrt(CapDiamVarPop/n)

SampleCapDiam <- sample(Champi2$`Cap Diameter (cm)`,500)
SampleCapDiamMean <- mean(SampleCapDiam)

#98% confidence interval 
alpha=0.02
lower <- SampleCapDiamMean - qnorm(p=1-alpha/2) * SECapDiam
upper <- SampleCapDiamMean + qnorm(p=1-alpha/2) * SECapDiam
round(c(lower, upper), 2)

#test if the mean Cap Diameter is 
#5.5 centimeters (2%significance level)
tsobsCapDiam <- (SampleCapDiamMean-5.5)/SECapDiam
#Reject H0 if 
tsobsCapDiam< -qnorm(p=1-0.02/2)
tsobsCapDiam> qnorm(p=1-0.02/2)
#Reject H0 at 2%significance level
#calculate the p-value
2*(1-pnorm(abs(tsobsCapDiam)))



#inference on bernoulli population
#estimate the probability that a mushroom has ring present
pi = 0.2255

#Test that the true probability is 
#equal to 0.2 against the alternative that it is greater
#at 2% significance level)
pi0=0.2
tsobsBernoulli <- (pi-pi0)/sqrt((pi0*(1-pi0))/nrow(Champi2))
#Reject H0 if 
tsobsBernoulli> qnorm(p=1-0.02)


####ISABEL####

#mean
widthstemmean <- mean(Champi2$`Stem Width (cm)`)
#variance
varstemvar <-var(Champi2$`Stem Width (cm)`)

#measure of accuracy
stemwsample<- sample(Champi2$`Stem Width (cm)`, 500)
n <- 500
SEstemwidth <-sqrt(varstemvar/n)

#95% confidence interval
alpha=0.05
lower <- widthstemmean - qnorm(p=1-alpha/2) *
  sqrt(varstemvar/n)
upper <- widthstemmean + qnorm(p=1-alpha/2) *
  sqrt(varstemvar/n)

round(c(lower, upper), 2)

#test whether the mean stem width is significantly different from 11 cm (5% significance)
#against the hypothesis that it is smaller
#H0: mu = 11 cm
#H1: mu < 11 cm

tsobs <- (mean(Champi2$`Stem Width (cm)`)-11)/sqrt(varstemvar/n)
tsobs
#Reject H0 if
tsobs< -qnorm(p=1-0.05)

#Reject H0 at 5%significance level
#calculate the p-value
pnorm(tsobs)


#inference on bernoulli population
#estimate the probability that a mushroom is edible
pi = 0.45
pi
#Test that the true probability is
#equal to 0.5 against the alternative that it is greater
#at 5% significance level)
pi0=0.5
tsobsBernoulli <- (pi-pi0)/sqrt((pi0*(1-pi0))/nrow(Champi2))
#Reject H0 if
tsobsBernoulli> qnorm(p=1-0.05)



####SUADA####

sigma<-var(Champi2$`Stem Height (cm)`)
mu<-mean(Champi2$`Stem Height (cm)`)


champish500<-sample(Champi2$`Stem Height (cm)`,500)

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

