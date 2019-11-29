
## 2018

data_directory <- "data/"
data_file <- "bird_with_names.csv"

filedata <- paste(data_directory,data_file,sep="")
filesp <- paste(data_directory,"espece.csv",sep="")
dsp <- read.csv("data/espece.csv",encoding="UTF-8")

d <-  read.csv2(filedata)
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




## ----------------------------------------------------------------------

## 2019

library(data.table)
library(dplyr)

data_directory <- "data/"
data_file0 <- "bird_with_names.csv"
data_file <- "bird_city_datasheet_FR.csv"

filedata0 <- paste(data_directory,data_file0,sep="")
filedata <- paste(data_directory,data_file,sep="")
filesp <- paste(data_directory,"espece.csv",sep="")

dsp <- fread("data/espece.csv",encoding="UTF-8")
d0 <- fread(filedata0)
d <-  fread(filedata)

spsc <- dsp[,c("pk_species","scientific_name"),with=FALSE]
colnames(spsc)[1] <- "sp"

d0 <- d0[passage > 0]
d0 <- unique(d0[,c("park_id","latitude_wgs84","longitude_wgs84"),with=FALSE])
d0 <- d0[park_id == "Pa492bis",park_id := "Pa492"]
colnames(d0)[2:3] <- c("lat","lon")

colonnes <- colnames(d)
dim(d)
dim(d0)

d <- data.table(inner_join(d,d0))
dim(d)
d[is.na(lat)]
d[is.na(sp)]


d <- d[,latitude_wgs84 := lat]
d <- d[,longitude_wgs84 := lon]


d <- data.table(inner_join(d,spsc))
dim(d)

d[is.na(scientific_name)]
d <- d[,sp := scientific_name]

d <- d[,colonnes,with=FALSE]

write.csv(d,"data/bird_city_datasheet_FR_Paris.csv",row.names=FALSE)
