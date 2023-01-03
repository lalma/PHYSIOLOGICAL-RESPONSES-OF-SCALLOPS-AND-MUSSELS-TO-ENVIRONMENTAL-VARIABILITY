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


