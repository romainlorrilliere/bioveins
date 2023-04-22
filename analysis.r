d <- read.csv2("bird.csv",stringsAsFactors=FALSE)
head(d)

d$species_code[d$species_code=="PICMAJ"] <- "DENMAJ"
d$species_code[d$species_code=="BRATRI"] <- "CERBRA"

d <- subset(d,species_code != "SPESPE")


dsp <- read.csv("espece.csv",encoding="UTF-8")




length(unique(d$species_code))


d <- merge(d,dsp,by.x="species_code",by.y="pk_species",all.x=TRUE)

write.csv2(d,"bird_with_names.csv")

d <- read.csv2("bird_with_names.csv")

spfr <- c(as.character(unique(subset(d,city=="Paris")$french_name)))



ab_sp <- aggregate(abondance~french_name+id+site+city,d,sum,na.rm=TRUE)


orderSp <- subset(dsp,pk_species %in% d$species_code)
ab_sp$french_name <- factor(ab_sp$french_name, levels = orderSp$french_name)

ab_sp$isParis <- ab_sp$city == "Paris"


library(ggplot2)
gg <- ggplot(ab_sp,aes(x=abondance,group=isParis,fill=isParis))+geom_histogram(alpha=.75) + facet_wrap(french_name~.)
gg <- gg + labs(title="Abondance par parcs")
##gg
ggsave("abondance_parcs.png",gg,width=16,height=9)



ddiv <- unique(subset(d,!(is.na(abondance)),select=c("id","site","city","species_code")))
ddiv <- aggregate(species_code~id+site+city,ddiv,length)
colnames(ddiv)[4] <- "div"
ddiv$isParis <- ddiv$city == "Paris"


gg <- ggplot(ddiv,aes(x=div,group=isParis,fill=isParis))+geom_histogram(alpha=.75)
gg <- gg + labs(title="Diversité dans les parcs")
ggsave("div_parcs.png",gg,width=7,height=7)

