
library(readxl)

MUSCA_isotopes <- read_excel("C:/Users/Lindsay/Dropbox/MUSCA/ISOTOPES/MUSCA isotopes.xlsx")
View(MUSCA_isotopes)
scallopiso<-subset(MUSCA_isotopes, Species=="S")
musseliso<-subset(MUSCA_isotopes, Species=="M")
View(scallopiso)
scallopiso$`C:N`

aggregate(`C:N` ~ Time + Depth, data=scallopiso, mean)
aggregate(`C:N` ~ Time + Depth, data=scallopiso, sd)
aggregate(`C:N` ~ Time + Depth, data=scallopiso, length)

aggregate(`C:N` ~ Time + Depth, data=musseliso, mean)
aggregate(`C:N` ~ Time + Depth, data=musseliso, sd)
aggregate(`C:N` ~ Time + Depth, data=musseliso, length)

aggregate(`d15N vs Air N2 (permil)` ~ Time + Depth, data=scallopiso, mean)
aggregate(`d15N vs Air N2 (permil)` ~ Time + Depth, data=scallopiso, sd)
aggregate(`d15N vs Air N2 (permil)` ~ Time + Depth, data=scallopiso, length)

aggregate(`d15N vs Air N2 (permil)` ~ Time + Depth, data=musseliso, mean)
aggregate(`d15N vs Air N2 (permil)` ~ Time + Depth, data=musseliso, sd)
aggregate(`d15N vs Air N2 (permil)` ~ Time + Depth, data=musseliso, length)

aggregate(`d13C vs VPDB (permil)` ~ Time + Depth, data=musseliso, mean)
aggregate(`d13C vs VPDB (permil)` ~ Time + Depth, data=musseliso, sd)
aggregate(`d13C vs VPDB (permil)` ~ Time + Depth, data=musseliso, length)

aggregate(`d13C vs VPDB (permil)` ~ Time + Depth, data=scallopiso, mean)
aggregate(`d13C vs VPDB (permil)` ~ Time + Depth, data=scallopiso, sd)
aggregate(`d13C vs VPDB (permil)` ~ Time + Depth, data=scallopiso, length)




aov<-aov(`d15N vs Air N2 (permil)`~Time*Depth*Species,data=MUSCA_isotopes)
summary(aov)
TukeyHSD(aov)

aov<-aov(`d15N vs Air N2 (permil)`~Time*Depth,data=MUSCA_isotopes)
summary(aov)
TukeyHSD(aov)

aov<-aov(`d13C vs VPDB (permil)`~Time*Depth*Species,data=MUSCA_isotopes)
summary(aov)
TukeyHSD(aov)


aov<-aov(`d15N vs Air N2 (permil)`~Time*Depth,data=scallopiso)
summary(aov)
TukeyHSD(aov)

aov<-aov(`d15N vs Air N2 (permil)`~ as.factor(Depth)*Time,data=musseliso)
summary(aov)
TukeyHSD(aov)

aov<-aov(`d13C vs VPDB (permil)`~Time*Depth,data=scallopiso)
summary(aov)
TukeyHSD(aov)

aov<-aov(`d13C vs VPDB (permil)`~Time*as.factor(Depth),data=musseliso)
summary(aov)
TukeyHSD(aov)

aov<-aov(`C quantity (ug)`~Title,data=musseliso) 
summary(aov)
TukeyHSD(aov)

aov<-aov(`C:N`~Time*Depth,data=scallopiso)
summary(aov)
TukeyHSD(aov)

aov<-aov(`C:N`~Time*Depth,data=musseliso)
summary(aov)
TUKEY<-TukeyHSD(aov)
plot(TUKEY , las=1 , col="brown")

# I need to group the treatments that are not different each other together.
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS <- generate_label_df(TUKEY , "data$treatment")


# A panel of colors to draw each group with the same color :
my_colors <- c( 
  rgb(143,199,74,maxColorValue = 255),
  rgb(242,104,34,maxColorValue = 255), 
  rgb(111,145,202,maxColorValue = 255)
)

# Draw the basic boxplot
a <- boxplot(data$value ~ data$treatment , ylim=c(min(data$value) , 1.1*max(data$value)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="value" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(data$treatment)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )


library(readxl)
MUSCA_isotopes_scallop_only <- read_excel("C:/Users/Lindsay/Dropbox/MUSCA/SCA only/MUSCA isotopes scallop only.xlsx")
View(MUSCA_isotopes_scallop_only)

MUSCA_isotopes <- read_excel("C:/Users/Lindsay/Dropbox/MUSCA/ISOTOPES/MUSCA isotopes.xlsx")
View(MUSCA_isotopes)


library(ggplot2)
library(plyr)
work <- "C:/Users/Lindsay/Dropbox/MUSCA/SCA only"
setwd(work)

##Scallops
#note you have some missing data
nomissing <- na.omit(MUSCA_isotopes_scallop_only) #chull function does not work with missing data

#getting the convex hull of each unique point set
df <- MUSCA_isotopes_scallop_only
find_hull <- function(df) df[chull(df$`d13C vs VPDB (permil)`, df$`d15N vs Air N2 (permil)`), ]
hulls <- ddply(df, "Title", find_hull)

#colors <- c("gray","#191970", "deepskyblue", "sienna3", "#CC9933")
#colors<- c("#d7191c","#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6")
#colors<- c("#003333", "#006666", "#993300", "#ff9966", "#669999")
colors<- c("#CC6600", "steelblue4", "#000066","#CC3333", "#990033")

plot <- ggplot(data = df, aes(x = `d13C vs VPDB (permil)`, 
                              y = `d15N vs Air N2 (permil)`, 
                              colour = Title, fill = Title)) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  geom_point() + 
  geom_polygon(data = hulls, alpha = 0.5) +  xlim(-24.5,-19) + ylim(6.5,11.5)+
  labs(x = "d13C", y = "d15N")

plot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



##mussels
M<-subset(MUSCA_isotopes, Species=="M")
df <- M
find_hull <- function(df) df[chull(df$`d13C vs VPDB (permil)`, df$`d15N vs Air N2 (permil)`), ]
hulls <- ddply(df, "Title", find_hull)

plot <- ggplot(data = df, aes(x = `d13C vs VPDB (permil)`, 
                              y = `d15N vs Air N2 (permil)`, 
                              colour = Title, fill = Title)) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  geom_point() + 
  geom_polygon(data = hulls, alpha = 0.5) +  xlim(-24.5,-19) + ylim(6.5,11.5)+
  labs(x = "d13C", y = "d15N")

plot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


