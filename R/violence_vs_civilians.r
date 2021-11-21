################################################################################
#                  Animated bubble map and lineplot
#                  Milos Popovic
#                  2021/11/21
################################################################################
#                 INSTALL AND LOAD LIBRARIES
################################################################################
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("plyr")) install.packages("plyr")
if(!require("data.table")) install.packages("data.table")
if(!require("tweenr")) install.packages("tweenr")
if(!require("animation")) install.packages("animation")
if(!require("rgeos")) install.packages("rgeos")
if(!require("rgdal")) install.packages("rgdal")
if(!require("maptools")) install.packages("maptools")
if(!require("scales")) install.packages("scales")
if(!require("gridExtra")) install.packages("gridExtra")
if(!require("rmapshaper")) install.packages("rmapshaper")
if(!require("sp")) install.packages("sp")
if(!require("grid")) install.packages("grid")
if(!require("ggrepel")) install.packages("ggrepel")
if(!require("extrafont")) install.packages("extrafont")

library(tidyverse, quietly=T)
library(plyr, quietly=T)
library(data.table, quietly=T)
library(tweenr, quietly=T)
library(animation, quietly=T)
library(rgeos, quietly=T)
library(rgdal, quietly=T)
library(maptools, quietly=T)
library(scales, quietly=T)
library(gridExtra, quietly=T)
library(rmapshaper, quietly=T)
library(sp, quietly=T) 
library(grid, quietly=T)
library(ggrepel, quietly=T)

#use Georgia font
windowsFonts(georg = windowsFont('Georgia'))

# download and load UCDP data
u <- "https://ucdp.uu.se/downloads/ged/ged211-csv.zip"
download.file(u, basename(u), mode="wb")
unzip("ged211-csv.zip")
a <- read.csv("ged211.csv", header=T)
names(a)

#only post 2001
b <- subset(a, year>=2001)

# keep only militant groups
c <- subset(b, !grepl("Government", b[[15]]), drop = TRUE)
d <- subset(c, type_of_violence == 3) %>% 
     select(year, side_a, deaths_civilians, where_coordinates, latitude, longitude)

################################################################################
# MAP DATA
################################################################################
# load world shapefile from Eurostat
url <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2013-01m.shp.zip" # location on the Eurostat website
download.file(url, basename(url), mode="wb") #download Eurostat country shapefiles
unzip("ref-countries-2013-01m.shp.zip") # unzip the boundary data
unzip("CNTR_RG_01M_2013_4326.shp.zip")

#load shapefile
world <- readOGR(getwd(),
        "CNTR_RG_01M_2013_4326", 
         verbose = TRUE, 
         stringsAsFactors = FALSE) %>%
        subset(NAME_ENGL!="Antarctica") # get rid of Antarctica 

# transform into Robinson projection
prj <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
wrld <- spTransform(world, CRS(prj)) # transform shp to Robinson projection
w <- fortify(wrld)

# retrieve coordinates of killings by location and year
b1 <- ddply(d, c("where_coordinates", "year"), summarise, 
	lat=max(latitude), 
	long=max(longitude),
	attack=sum(deaths_civilians))

# transform coordinates into Robinson projection
bb <- project(cbind(b1$long, b1$lat), prj)
bb1 <- data.frame(place=b1$where_coordinates, 
  year=b1$year, 
  long=bb[,1], 
  lat=bb[,2], 
  attack=b1$attack)

# calculate cumulative attacks for every place
cc <- ddply(bb1, c("place", "year"), summarise,
  long=max(long),
  lat=max(lat),
  attacks=sum(attack)) %>% 
as_tibble() %>%
group_by(place) %>% 
dplyr::mutate(attacks = cumsum(attacks))

# create a table with year range
year_range <- seq(min(cc$year), max(cc$year), by=1)
data_expand <- expand.grid(year_range, cc$place)
colnames(data_expand) <- c("year", "place")

# populate the table with missing years for every place
dd <- merge(cc, data_expand, by=c("place", "year"), all.y = T)%>% 
ddply(c("place", "year"), 
	summarise, 
	year=max(year), 
	long=max(long),
	lat=max(lat),
	value=max(attacks)) 

# fill the missing values with last available info
df <- dd %>%
group_by(place) %>% 
complete(year = seq(min(year), max(year), by=1)) %>%
fill(value)%>%
fill(long)%>%
fill(lat)%>%
ungroup()

x <- split(df, df$year)

tm <- tween_states(x, 
	tweenlength= 2, 
	statelength=3, 
	ease=rep('cubic-in-out', 50),
	nframes=70) %>% # 59 frames
  data.table()

tm$year <- round(tm$year, 0) # get rounded years for gif
tm$value[tm$value==0] <- NA
vmin <- min(tm$value, na.rm=T)
vmax <- max(tm$value, na.rm=T)
times <- c(rep(0.1, max(tm$.frame)-1), 5) #0.15 sec for 58 frames, 5 sec for the last

################################################################################
# LINE PLOT DATA
################################################################################
# aggregate by year and perpetrator and calculate cummulative attacks
e <- d %>% select(side_a, year, deaths_civilians)  %>% 
group_by(side_a, year) %>% 
dplyr::summarise(attack = sum(deaths_civilians)) %>%
dplyr::mutate(attacks = cumsum(attack)) %>%
select(side_a, year, attacks)
e %>% filter(side_a == 'IS')

# backfill year column for every group
data_expand <- expand.grid(year_range, e$side_a)
colnames(data_expand) <- c("year", "side_a")
f <- merge(e, data_expand, by=c("side_a", "year"), all.y = T) %>% 
ddply(c("side_a", "year"), 
  summarise, 
  year=max(year), 
  value=max(attacks))

# now fill the successive years with missing values
ff <- f %>%
group_by(side_a) %>% 
complete(year = seq(min(year), max(year), by=1)) %>%
fill(value)%>%
ungroup()
ff$value[is.na(ff$value)] <- 0 #missing values to 0

# inspect groups with most attacks
ff %>% 
  group_by(side_a) %>%
  dplyr::summarise(sum_attacks = sum(value)) %>%
  arrange(desc(sum_attacks)) %>%
  print(n=25)

# let's choose 4 most notorious and still active groups 
groups <- c("Al-Shabaab", "IS", "LRA", "Taleban", "al-Qaida")
h <- subset(ff, side_a%in%groups)

# enumerate years to use for animation stages
k <- h %>% dplyr::select(year, side_a, value) %>% 
mutate(num=as.numeric(year-min(year)+1), ease="linear")

#define animation elements
s <- tween_elements(k, time = "num",  
  group="side_a", 
  ease="ease", 
  nframes = as.numeric(max(tm$.frame)))

# determine when lines/points will appear based on lead time
td <- tween_appear(s, 
  time='num', 
  nframes = as.numeric(max(tm$.frame)))

# extend every frame by 1
td$.frame <- td$.frame + 1

# create a log scale for attacks
td$value_log <- log(td$value)
td$value_log[td$value_log=="-Inf"] <- 0
# capitalize group name
td$.group <- toupper(td$.group)

################################################################################
# ANIMATE PLOT
################################################################################
saveGIF({
for(i in 1:max(tm$.frame)) {
  map <-
    tm %>% filter(.frame==i) %>%
    ggplot(aes(x=long, y=lat)) +
geom_map(data = w, map = w,
             aes(map_id = id),
             color = "white", size = 0.01, fill = "grey80") +
geom_point(aes(size=value), 
	fill="#AA4499", 
	alpha = .45,
	col="#AA4499", 
	stroke=.25) +
    scale_size(breaks=c(1, 10, 100, 1000, 2753), 
    	labels=c(1, 10, 100, 1000, 3000),
    	range = c(2, 10),
    	limits = c(vmin,vmax),
    	name="Attacks:")+
guides(size = guide_legend(override.aes = list(alpha = 1),
            direction = "vertical",
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 0,
            byrow = T,
            reverse = F,
            label.position = "right"
          )
	) +
    coord_equal() +
  labs(y="", x="",
         title="Civilian Killings by Militant Organizations (2001-2020)",
         subtitle=paste0("Year of", " " ,as.character(as.factor(tail(tm %>% filter(.frame==i),1)$year))),
         caption="©2021 Milos Popovic https://milospopovic.net\nData: UCDP, https://ucdp.uu.se/")+
theme_minimal() +
  theme(text=element_text(family="georg"),
        legend.position = c(.1, .4),
  	    plot.margin=unit(c(1,-4.5,-4.5,-4.5), "cm"),
        legend.text = element_text(size=12, color="grey20"),
        legend.direction = "horizontal",
        legend.title = element_text(size=14, color="grey20", face="bold"),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_blank(),
	      plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA), 
        legend.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(size=20, color="grey20", hjust=0, vjust=0),
	      plot.caption = element_text(size=12, color="grey20", hjust=.85, vjust=10),
	      plot.subtitle = element_text(size=32, color="#AA4499", face="bold", hjust=0),
        strip.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

p <- td %>% filter(.frame==i, .age> -3.5) %>%
ggplot()+
  geom_line(aes(x=year, y=value_log, group=.group, color=.group), size=1.5)+
  geom_point(data=. %>% filter(year==max(year)), mapping=aes(x=year, y=value_log, color=.group, fill=.group), size=3)+
  geom_point(data=. %>% filter(year==max(year)), mapping=aes(x=year, y=value_log, color=.group, fill=.group), size=2)+
  geom_text_repel(data=. %>% filter(year==max(year)), mapping=aes(x=year, y=value_log,label=.group, color=.group), hjust=.75, fontface="bold")+
  theme_minimal()+
  scale_color_manual(values=c("#D65D00", "#481FBF", "grey10", "#ED0C6E", "#25E30B")
  	)+
    scale_x_continuous(limits = c(2001,2021), breaks=c(2002:2020, 2))  +
    scale_y_continuous(limits=c(2.302585, 10.40895), 
    	breaks=c(2.302585, 4.60517, 6.907755, 9.21034, 10.30895),
    	labels=c(10, 100, 1000, 10000, 30000
    		   		))+
    theme(text=element_text(family="georg"),
    	    plot.margin=unit(c(2,0,0,1), "cm"),
          legend.position="none",
          axis.text.y = element_text(size=14),
          axis.text.x = element_text(size=14),
		      axis.title.y = element_text(face="bold",size=16,angle=0, vjust=.5),
          axis.title.x = element_text(face="bold",size=16,angle=0, vjust=-.15, hjust=.5),
          plot.title=element_text(size=18, vjust=-5),
          plot.caption=element_text(hjust=0),
          panel.grid.major.x = element_line(color="grey80", linetype="dotted", size=.5),
          panel.grid.minor.x = element_line(color="grey80", linetype="dotted", size=.5),
          panel.grid.major.y = element_line(color="grey80", linetype="dotted", size=.5),
          panel.grid.minor.y = element_line(color="grey80", linetype="dotted", size=.5))+
    labs(x="Year",
    	y="Killings",
         title="Civilian Killings by Deadliest Groups\n(logarithmic scale)",
         subtitle="",
         caption="")

print(paste(i,"out of", max(td$.frame)))
ani.pause()
gg <- grid.arrange(map, p, ncol = 1, nrow = 2)
print(gg)
;}
 
},movie.name="violence_vs_civilians.gif",
interval = times, 
ani.width = 1024, 
ani.height = 768,
other.opts = " -framerate 10  -i image%03d.png -s:v 1024x768 -c:v libx264 -profile:v high -crf 20  -pix_fmt yuv420p")