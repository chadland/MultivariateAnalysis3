###### Create additional time based variables i.e. date, season, month, day and hour ##############
attach(beer.data.cleaned)
beer.data.cleaned$date<-as.POSIXlt(beer.data.cleaned$Time, origin="1970-01-01")
require("lubridate")
beer.data.cleaned$hour<- hour(beer.data.cleaned$date)
beer.data.cleaned$day<- wday(beer.data.cleaned$date)
beer.data.cleaned$month<- month(beer.data.cleaned$date)
beer.data.cleaned$year<- year(beer.data.cleaned$date)
beer.data.cleaned$season<- rep(1,nrow(beer.data.cleaned))
beer.data.cleaned$season[beer.data.cleaned$month==c("6","7","8")]<- 2
beer.data.cleaned$season[beer.data.cleaned$month==c("9","10","11")]<- 3
beer.data.cleaned$season[beer.data.cleaned$month==c("12","1","2")]<- 4

#### Basic EDA of the new variables ######

summary(beer.data.cleaned$season)
summary(beer.data.cleaned$day)
summary(beer.data.cleaned$hour)
tz(beer.data.cleaned$date)
summary(beer.data.cleaned$year)

### standardise the data to deal with the large differences in ABV across beers #####
## remove NAs from the cleaned beer data prior to scaling ##
beer.data.cleaned<- na.omit(beer.data.cleaned)
standardised.clean.time <- as.data.frame(scale(rs.cleandata.na[is.na(rs.cleandata.na$ABV)==FALSE,c(6,8,9,10,11,12,14:18)])) # standardise the variables
standardised.clean.year <- as.data.frame(scale(rs.cleandata.na[is.na(rs.cleandata.na$ABV)==FALSE,c(6,8,9,10,11,12,18)])) # standardise the variables

##### Run PCA inc different variables ##########

pca.year<- prcomp(standardised.clean.year)
pca.year
summary(pca.year)

pca.month<- prcomp(standardised.clean.time[c(1:6,8)])
pca.month
summary(pca.month)

pca.season<- prcomp(standardised.clean.time[c(1:6,9)])
pca.season
summary(pca.season)

#### calculate number (proportion) of ratings per month #####

ratings.pm<- tapply(beer.data.cleaned$rating, beer.data.cleaned$month, function(x) {length(x)/length(beer.data.cleaned$rating)})
order(ratings.pm)

### look for the most prevalent beer styles in the winter months (Nov, Dec and Jan) #####

xmas.period<- beer.data.cleaned[beer.data.cleaned$month==c("11","12","1"),]

## scale the data and keep only ABV and Review variables for use in clustering and PCA ##
standardised.clean.xmas<- as.data.frame(scale(xmas.period[is.na(xmas.period$ABV)==FALSE,c(6,8:12)])) # standardise the variables
standardised.clean.xmas$beerStyle<- xmas.period$beerStyle

pca.xmas<- prcomp(standardised.clean.xmas)
pca.xmas
## Scores for PC1 to PC6 ###
xmas.scores<- data.frame(pca.xmas$x)
xmas.plot.data<- cbind(standardised.clean.xmas, xmas.scores,clusters.scaled)
length(xmas.plot.data$clusters[xmas.plot.data$clusters!=3])
cluster3.subset<- xmas.plot.data[xmas.plot.data$clusters==3,]
#cluster3.subset$beerStyle<- standardised.clean.xmas$beerStyle[row.names(cluster3.subset)]

other.clusters<- xmas.plot.data[xmas.plot.data$clusters!=3,]


#tapply(cluster3.subset$rating, cluster3.subset$beerStyle, function(x) {length(x)/length(cluster3.subset$rating)})

xmas.beerstyle<- tapply(xmas.period$rating,xmas.period$beerStyle,function(x) {length(x)/length(xmas.period$rating)})


unique(xmas.beerstyle)
length(unique(xmas.beerstyle))

#### get top 10 most rated beers during the xmas period ####
xmas.top10.index<- order(xmas.beerstyle, decreasing=TRUE)[1:10]
xmas.top10<- xmas.beerstyle[xmas.top10.index]

### get the names of the beerstyles #####
xmas.top10.beerstyles<- names(xmas.top10)

bdc.not.xmas<- beer.data.cleaned[beer.data.cleaned$month==c("2","3","4","5","6","7","8","9","10"),]
bdc.not.xmas.top10<- bdc.not.xmas[bdc.not.xmas$beerStyle %in% xmas.top10.beerstyles,]

### Get proportions of the top10 xmas beerstyles relative to their ratings during the rest of the year #####
xmas.top10.beerstyle.not.xmas<- tapply(bdc.not.xmas.top10$rating,bdc.not.xmas.top10$beerStyle,function(x) {length(x)/length(bdc.not.xmas$rating)})

length(beer.data.cleaned$beerStyle[beer.data.cleaned$beerStyle=="American IPA"])/nrow(beer.data.cleaned)


#cluster3.subset$mergeID<-row.names(cluster3.subset)
#xmas.period$mergeID<-row.names(xmas.period)
#cluster3.subset<-merge(cluster3.subset,xmas.period[,c("mergeID","beerStyle")],by="mergeID")
#beer.data.cleaned$reviewAppearance_STND<-scale(beer.data.cleaned$reviewAppearance)
#beer.data.cleaned$reviewAroma_STND<-scale(beer.data.cleaned$reviewAroma)

cluster3.beerStyles<- tapply(cluster3.subset$rating, cluster3.subset$beerStyle, function(x) {length(x)/nrow(cluster3.subset)})
top10.cluster3<- sort(cluster3.beerStyles, decreasing=TRUE)[1:10]
#other.clusters$mergeID<-row.names(other.clusters)
#xmas.period$mergeID<-row.names(xmas.period)
#other.clusters<-merge(other.clusters,xmas.period[,c("mergeID","beerStyle")],by="mergeID")
#beer.data.cleaned$reviewAppearance_STND<-scale(beer.data.cleaned$reviewAppearance)
#beer.data.cleaned$reviewAroma_STND<-scale(beer.data.cleaned$reviewAroma)

other.clusters.beerStyles<- tapply(other.clusters$rating, other.clusters$beerStyle, function(x) {length(x)/nrow(other.clusters)})


#c3.tempdata<- data.frame(table(cluster3.subset$beerStyle.x))

## Get top 10 most frequently rated beer styles in cluster 3 ##

top10.other.clusters<- names(sort(other.clusters.beerStyles, decreasing=TRUE)[1:10])


## select only obs associated with the top 10 most rated beerStyles in cluster 3, amongst all other clusters ##
top10.cluster3.other.clusters<- other.clusters[other.clusters$beerStyle %in% top10.cluster3,]

## Now check the proportion of reviews for the top 10 most rated beerStyles in cluster 3, in all other clusters ##
top10.cluster3.other.clusters.beerStyles<- tapply(top10.cluster3.other.clusters$rating, top10.cluster3.other.clusters$beerStyle, function(x) {length(x)/nrow(top10.cluster3.other.clusters)})

