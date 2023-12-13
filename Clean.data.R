#Mushrooms

setwd("C:/Users/suada/OneDrive - University of Pisa/Desktop/MushroomDataset/MushroomDataset")

champi<-read.table("secondary_data.csv", header=T, sep=";", stringsAsFactors = T)
class(champi)
typeof(champi)
summary(champi)
ncol(champi)
nrow(champi)

View(champi)

# check for NA
is.na(champi)

#filtering
#eliminate columns
champi1<-champi[ , -c(4,7,12,13,14,15,16,18,19)]
View(champi1)

summary(champi1)

#remove rows with NA value in any column
champi2<-na.omit(champi1)
champi2

# check for NA
is.na(champi1)
which(is.na(champi1))

#eliminate rows with blanks in column "gill.spacing"
champi2<-champi1[-which(champi1$gill.spacing==""), ]


summary(champi2)
is.na(champi2)

#columns
colnames(champi2)<-c("Class", "Cap Diameter (m)", "Cap Shape", "Cap Colour","Bruise or Bleed", "Gill Spacing", "Gill Colour", "Stem Height (m)","Stem Width (m)", "Ring", "Habitat", "Season" )

##Class
champi2$Class<-as.character(champi2$Class)
typeof(champi2$Class)

champi2$Class[ champi2$Class == "p"] <- "Poisonous"
champi2$Class[ champi2$Class == "e"] <- "Edible"

champi2$Class<-as.factor(champi2$Class)


##Cap Shape
summary(champi2$`Cap Shape`)
champi2$`Cap Shape`

champi2$`Cap Shape`<-as.character(champi2$`Cap Shape`)
typeof(champi2$`Cap Shape`)

champi2$`Cap Shape`[champi2$`Cap Shape`=="f"] <- "Flat"
champi2$`Cap Shape`[champi2$`Cap Shape`=="b"] <- "Bell"
champi2$`Cap Shape`[champi2$`Cap Shape`=="c"] <- "Conical"
champi2$`Cap Shape`[champi2$`Cap Shape`=="s"] <- "Sunken"
champi2$`Cap Shape`[champi2$`Cap Shape`=="p"] <- "Spherical"
champi2$`Cap Shape`[champi2$`Cap Shape`=="o"] <- "Others"
champi2$`Cap Shape`[champi2$`Cap Shape`=="x"] <- "Convex"

champi2$`Cap Shape`<-as.factor(champi2$`Cap Shape`)


##Cap Colour
champi2$`Cap Colour`<-as.character(champi2$`Cap Colour`)

champi2$`Cap Colour`[champi2$`Cap Colour`=="n"]<- "Brown"
champi2$`Cap Colour`[champi2$`Cap Colour`=="b"]<- "Buff"
champi2$`Cap Colour`[champi2$`Cap Colour`=="g"]<- "Gray"
champi2$`Cap Colour`[champi2$`Cap Colour`=="r"]<- "Green"
champi2$`Cap Colour`[champi2$`Cap Colour`=="p"]<- "Pink"
champi2$`Cap Colour`[champi2$`Cap Colour`=="u"]<- "Purple"
champi2$`Cap Colour`[champi2$`Cap Colour`=="e"]<- "Red"
champi2$`Cap Colour`[champi2$`Cap Colour`=="w"]<- "White"
champi2$`Cap Colour`[champi2$`Cap Colour`=="y"]<- "Yellow"
champi2$`Cap Colour`[champi2$`Cap Colour`=="l"]<- "Blue"
champi2$`Cap Colour`[champi2$`Cap Colour`=="o"]<- "Orange"
champi2$`Cap Colour`[champi2$`Cap Colour`=="k"]<- "Black"

champi2$`Cap Colour`<-as.factor(champi2$`Cap Colour`)

##Bruise or Bleed
champi2$`Bruise or Bleed`<-as.character(champi2$`Bruise or Bleed`)

champi2$`Bruise or Bleed`[champi2$`Bruise or Bleed`=="t"]<-T
champi2$`Bruise or Bleed`[champi2$`Bruise or Bleed`=="f"]<-F

champi2$`Bruise or Bleed`<-as.logical(champi2$`Bruise or Bleed`)


##Gill Spacing
champi2$`Gill Spacing`<-as.character(champi2$`Gill Spacing`)

champi2$`Gill Spacing`[champi2$`Gill Spacing`=="f"]<-"None"
champi2$`Gill Spacing`[champi2$`Gill Spacing`=="d"]<-"Distant"
champi2$`Gill Spacing`[champi2$`Gill Spacing`=="c"]<-"Close"

champi2$`Gill Spacing`<-as.factor(champi2$`Gill Spacing`)


##Gill Colour
champi2$`Gill Colour`<-as.character(champi2$`Gill Colour`)

champi2$`Gill Colour`[champi2$`Gill Colour`=="n"]<- "Brown"
champi2$`Gill Colour`[champi2$`Gill Colour`=="b"]<- "Buff"
champi2$`Gill Colour`[champi2$`Gill Colour`=="g"]<- "Gray"
champi2$`Gill Colour`[champi2$`Gill Colour`=="r"]<- "Green"
champi2$`Gill Colour`[champi2$`Gill Colour`=="p"]<- "Pink"
champi2$`Gill Colour`[champi2$`Gill Colour`=="u"]<- "Purple"
champi2$`Gill Colour`[champi2$`Gill Colour`=="e"]<- "Red"
champi2$`Gill Colour`[champi2$`Gill Colour`=="w"]<- "White"
champi2$`Gill Colour`[champi2$`Gill Colour`=="y"]<- "Yellow"
champi2$`Gill Colour`[champi2$`Gill Colour`=="l"]<- "Blue"
champi2$`Gill Colour`[champi2$`Gill Colour`=="o"]<- "Orange"
champi2$`Gill Colour`[champi2$`Gill Colour`=="k"]<- "Black"
champi2$`Gill Colour`[champi2$`Gill Colour`=="f"]<- "None"

champi2$`Gill Colour`<-as.factor(champi2$`Gill Colour`)


## Ring
champi2$Ring<-as.character(champi2$Ring)

champi2$Ring[champi2$Ring=="t"]<-T
champi2$Ring[champi2$Ring=="f"]<-F

champi2$Ring<-as.logical(champi2$Ring)


## Habitat
champi2$Habitat<-as.character(champi2$Habitat)

champi2$Habitat[champi2$Habitat=="g"]<-"Grasses"
champi2$Habitat[champi2$Habitat=="l"]<-"Leaves"
champi2$Habitat[champi2$Habitat=="m"]<-"Meadows"
champi2$Habitat[champi2$Habitat=="p"]<-"Paths"
champi2$Habitat[champi2$Habitat=="h"]<-"Heaths"
champi2$Habitat[champi2$Habitat=="u"]<-"Urban"
champi2$Habitat[champi2$Habitat=="w"]<-"Waste"
champi2$Habitat[champi2$Habitat=="d"]<-"Woods"

champi2$Habitat<-as.factor(champi2$Habitat)

## Season
champi2$Season<-as.character(champi2$Season)

champi2$Season[champi2$Season=="a"]<-"Autumn"
champi2$Season[champi2$Season=="w"]<-"Winter"
champi2$Season[champi2$Season=="s"]<-"Spring"
champi2$Season[champi2$Season=="u"]<-"Summer"

champi2$Season<-as.factor(champi2$Season)


#adding a column named Class Height
champi2$`Class Height`<-cut(champi2$`Stem Height (m)`, breaks = c(-1, 4.480, 6.890, 30), labels = c("Short", "Medium",
                                                                                          "Tall"))
table(champi2$`Class Height`)
summary(champi2$`Class Height`)

write.csv(champi2, "champi_clean.csv", row.names=TRUE)
