library(vegan)
library(ggplot2)

#load dataset
data(varespec)
head(varespec)

#setting a seed will make output reproducible
set.seed(123456)

#now creating a vector that will be the length of the number of rows in dataset
nrow(varespec)

#create vector
grp<-rep(NA,24)

#sample half of rows
ind<-sample(1:nrow(varespec), 12)

#assign those rows to group A
grp[ind]<-"A"

grp

#assign rest to group B
grp[is.na(grp)]<-"B"

grp

#create NMDS
vare.mds<-metaMDS(varespec)
vare.mds

#plot with text, looks like garbage
plot(vare.mds, type = "t")

#pull out scores, or really x,y coordinates
data.scores<-as.data.frame(scores(vare.mds))

#add sites to go along with the coordinates
data.scores$site<-rownames(data.scores)

#apply the previously constructed grouping
data.scores$grp<-grp

head(data.scores)

#now can do the same for species
species.scores<-as.data.frame(scores(vare.mds, "species"))
species.scores$species<-rownames(species.scores)
head(species.scores)

#fit loading vectors
vec.spec<-envfit(vare.mds, varespec, permutations = 1000)
vec.sp.df<-as.data.frame(vec.spec$vectors$arrows*sqrt(vec.spec$vectors$r))
vec.sp.df$species<-rownames(vec.sp.df)

#change one to be redundant, the problem that scott has
vec.sp.df$species[42]<-"Cladphyl"

#get the list of individual levels (should now be total rows minus the one redundant row)
vec.names<-levels(as.factor(vec.sp.df$species))

vec.sp.df

#create empty data frame
new.arrows<-data.frame()

#populate dataframe with the species name plus the mean of x,y coordinates (NMDS values)
for (i in vec.names) {
  n<-i
  x<-mean(subset(vec.sp.df$NMDS1, vec.sp.df$species == i))
  y<-mean(subset(vec.sp.df$NMDS2, vec.sp.df$species == i))
  new.arrows[i,1]=n
  new.arrows[i,2]=x
  new.arrows[i,3]=y
}

#create meaningful column names
colnames(new.arrows)<-c("species", "NMDS1", "NMDS2")
new.arrows

#plot NMDS with new, averaged arrow for both values of "Cladphyl"
ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=3) + # add the point markers
  geom_text(data=new.arrows,aes(x=NMDS1,y=NMDS2,label=species),size=5)+
  geom_segment(data=new.arrows,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="grey",inherit_aes=FALSE) + 
  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw()


#and compare with original, unaveraged, showing two cases of "Cladphyl"
ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=3) + # add the point markers
  geom_text(data=vec.sp.df,aes(x=NMDS1,y=NMDS2,label=species),size=5)+
  geom_segment(data=vec.sp.df,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="grey",inherit_aes=FALSE) + 
  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw()
