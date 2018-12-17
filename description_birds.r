data_directory <- "data/"
data_file <- "bird_with_names.csv"

filedata <- paste(data_directory,data_file,sep="")
filesp <- paste(data_directory,"espece.csv",sep="")
dsp <- read.csv("data/espece.csv",encoding="UTF-8")

d <- read.csv2(filedata)
spfr <- c(as.character(unique(subset(d,city=="Paris")$french_name)))

ab_sp <- aggregate(abondance~french_name+park_id+site+city,d,sum,na.rm=TRUE)

orderSp <- subset(dsp,pk_species %in% d$species_code)
ab_sp$french_name <- factor(ab_sp$french_name, levels = orderSp$french_name)

ab_sp$isParis <- ab_sp$city == "Paris"

library(ggplot2)
gg <- ggplot(ab_sp,aes(x=abondance,group=isParis,fill=isParis))+geom_histogram(alpha=.75) + facet_wrap(french_name~.)
gg <- gg + labs(title="Abondance par parcs")
##gg
ggsave("abondance_parcs.png",gg,width=16,height=9)



ddiv <- unique(subset(d,!(is.na(abondance)),select=c("park_id","site","city","species_code")))
ddiv <- aggregate(species_code~park_id+site+city,ddiv,length)
colnames(ddiv)[4] <- "div"
ddiv$isParis <- ddiv$city == "Paris"


gg <- ggplot(ddiv,aes(x=div,group=isParis,fill=isParis))+geom_histogram(alpha=.75)
gg <- gg + labs(title="Diversité dans les parcs")
ggsave("div_parcs.png",gg,width=7,height=7)
