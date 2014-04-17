#Data Set Description
#
#The data set contains approx. 1.5 million online beer reviews from 
#www.beeradvocate.com during the time period from 1998 to 2011.   
#Reviewers have scored a large variety of beers (from various countries) on 
#5 different categories on a scale from 1-5: 
#appearance, 
#aroma, 
#palate, 
#taste 
#overall rating.  
#
#In addition the data set contains the following variables:  
#beer style, 
#brewing company id, 
#alcohol percentage of the beer, 
#name of the beer, 
#timestamp (contains the date from when the review was registered), 
#profile name of the reviewer and a free text field containing a 
#personal review from the beer consumer. 

#Assign necessary libraries
library(sqldf)
library(corrplot)
library(plyr)
library(psych)
library(corrgram)
library(plyr)
library(ggplot2)

load.beer.data.fast.method <- function(filepath="C:\\Users\\Bollen\\Google Drive\\My StAndrews\\Applied Multivariate Analysis\\Project Work Group 1\\DataSets\\beeradvocate.txt", random.sample.percentage=0.1) {
  #Purpose: Load Beer Review data into R so it may be used for analysis purposes
  # Inputs:
  # filepath                     -  location of beer file
  # random.sample.percentage     -  percentage of data set to return as random sample
  #
  # Outputs:
  # df                            -  dataframe containing data 
  
  my.read.lines2=function(fname) {
    s = file.info( fname )$size 
    buf = readChar( fname, s, useBytes=T)
    strsplit( buf,"\n",fixed=T,useBytes=T)[[1]]
  }
  
  #Read in file 
  table <- my.read.lines2("C:\\Users\\creem013\\Documents\\Temp\\beeradvocate.txt")
  
  #Get Variables 
  name <- substring(table[seq.int(1L, length(table), 14L)], first=12)
  beerId <- as.numeric(substring(table[seq.int(2L, length(table), 14L)], first=14))
  brewerId <- as.numeric(substring(table[seq.int(3L, length(table), 14L)], first=16))
  ABV <- as.numeric(substring(table[seq.int(4L, length(table), 14L)], first=11))
  beerStyle <- substring(table[seq.int(5L, length(table), 14L)], first=13)
  reviewAppearance <- as.numeric(substring(table[seq.int(6L, length(table), 14L)], first=20))
  reviewAroma <- as.numeric(substring(table[seq.int(7L, length(table), 14L)], first=15))
  reviewPalate <- as.numeric(substring(table[seq.int(8L, length(table), 14L)], first=16))
  reviewTaste <- as.numeric(substring(table[seq.int(9L, length(table), 14L)], first=15))
  rating <- as.numeric(substring(table[seq.int(10L, length(table), 14L)], first=17))
  time <- as.numeric(substring(table[seq.int(11L, length(table), 14L)], first=14))
  profileName <- substring(table[seq.int(12L, length(table), 14L)], first=21)
  
  #Return data frame
  beer.data <- data.frame(name = name,
                          beerId = beerId,
                          brewerId = brewerId,
                          ABV= ABV,
                          beerStyle = beerStyle,
                          reviewAppearance = reviewAppearance, 
                          reviewAroma = reviewAroma,
                          reviewPalate = reviewPalate,
                          reviewTaste = reviewTaste,
                          rating = rating,
                          Time = time,
                          profileName = profileName,stringsAsFactors=FALSE)
  
  return(beer.data[sample((1:nrow(beer.data)),floor(nrow(beer.data)*random.sample.percentage)),])
  
}

clean.beer.data<- function() {
  #Purpose: Explore the data for missing values and clean the data
  #
  #Output:  Data set containing a cleaned data set of the beer data
  
  #Count the number of rows in the data set
  nrow(beer.data)  
  
  #Exploratory data analysis
  summary(beer.data)
  
  #Points to note
  #ABV-variable missing data, does this mean missing or that the 
  #alcohol content is 0 or just missing data? May it be alcohol-free beer or 
  #some other types of issue. 
  missing.abv.data <- beer.data[is.na(beer.data$ABV),] 
  print(paste("Number of rows with missing ABV variable: ", nrow(missing.abv.data)))
  
  #Rating-variable = 0, This seems to be error in the data? 
  #Since we have som much data I think it makes sense to delete the observations 
  missing.rating.data <- beer.data[beer.data$rating==0,] 
  print(paste("Number of rows with zero-Rating: ",nrow(missing.rating.data)))
  
  #Check number of values that have 0 in reviewApparance, should they be 
  #taken out?#Since we have som much data I think it makes sense to delete the observations 
  missing.reviewApperance.data <- beer.data[beer.data$reviewAppearance==0,]   
  print(paste("Number of rows with zero reviewApperance: ",nrow(missing.reviewApperance.data)))
  
  #Check zero data in any of the other data
  missing.aroma.data <- beer.data[beer.data$reviewAroma==0,] 
  missing.palate.data <- beer.data[beer.data$reviewPalate==0,] 
  missing.taste.data <- beer.data[beer.data$reviewTaste==0,] 
  print(paste("Number of rows with zero reviewAroma: ",nrow(missing.aroma.data)))
  print(paste("Number of rows with zero reviewPalate: ",nrow(missing.palate.data)))
  print(paste("Number of rows with zero reviewTaste: ",nrow(missing.taste.data)))
  
  #Check for empty beer names and bee style
  empty.beer.name <- beer.data[beer.data$name == "",]
  print(paste("Number of rows with missing beer name: ", nrow(empty.beer.name)))
  empty.beer.style <- beer.data[beer.data$beerStyle == "",]
  print(paste("Number of rows with missing beer style: ", nrow(empty.beer.style)))
  empty.profile.name <- beer.data[beer.data$profilName == "",]
  print(paste("Number of rows with missing profile name: ", nrow(empty.profile.name)))
  
  #Delete data that has missing values overall ratings
  beer.data.cleaned <- beer.data[!beer.data$rating==0,] 
  print(paste("Data Set size after cleaning: ",nrow(beer.data.cleaned)))
  
  #Identify duplicate ratings, meaning if the user have made a ratings on the same beer several times, should be deleted? 
  duplicate.ratings <- sqldf("Select profileName, beerId,max(Time) as max,count(Time) as count from 'beer.data.cleaned' group by profileName, beerId")
  sqldf()
  #duplicate.ratings <- ddply(beer.data.cleaned, .(profileName, beerId), summarise, count = length(profileName), max = max(Time))
  print(paste("Nr. of duplicate ratings through time (same user rating the same beer): ",
              sum(duplicate.ratings$count > 1)))
  duplicate.ratings.over.one <- duplicate.ratings[which(duplicate.ratings$count>1),]
  
  #Identify rows to delete 
  all.rows.duplicate <- merge(beer.data.cleaned,duplicate.ratings.over.one,by=c("profileName","beerId"))
  all.rows.excluding.last.rating <- all.rows.duplicate[which(all.rows.duplicate$Time<all.rows.duplicate$max),]
  
  all.rows.excluding.last.rating$EXTRACOL <- 1 # use a column name that is not present among 
  
  # the original data.frame columns
  new.cleaned.data<- subset(merge(beer.data.cleaned,  all.rows.excluding.last.rating,by=c("profileName","beerId", "Time"), suffixes=c("", ".y"),all.x = TRUE),is.na(EXTRACOL))[,c(1:12)]
  print(paste("Nr. of rows after only keeping the last rating a profile has on the same beer
              : ",
              nrow(new.cleaned.data)))
  return(new.cleaned.data)
}

create.frequency.table <- function(data.frame.rating){
  #Purpose: Create a quantitative frequency table for a rating variable
  #
  # Inputs:
  # data.frame.rating - Input rating vector or data frame
  #
  # Outputs:
  # xout            - Data frame containing xout
  #
  # Create frequency tables 
  factorx <- factor(cut(data.frame.rating, breaks=seq(0.5,5,by=0.5)))
  #Tabulate and turn into data.frame
  xout <- as.data.frame(table(factorx))
  #Add cumFreq and proportions
  xout <- transform(xout, cumFreq = cumsum(Freq), relative = prop.table(Freq))
  return(xout)
}

descriptive.analysis<- function() {
  #Purpose:   -   Create descriptive analysis of the cleaned beer data set
  
  #Turn off graph warnings
  options(warn=-1)
  
  #Frequency plots
  hist(beer.data.cleaned$reviewAppearance, breaks=12, col="blue", main = "Apperance Review", xlab="Rating")
  hist(beer.data.cleaned$reviewAroma, breaks=12, col="blue", main = "Aroma Review", xlab="Rating")
  hist(beer.data.cleaned$reviewPalate, breaks=12, col="blue", main = "Apperance Review", xlab="Rating")
  hist(beer.data.cleaned$reviewTaste, breaks=12, col="blue", main = "Taste Review", xlab="Rating")
  hist(beer.data.cleaned$rating , breaks=12, col="blue", main = "Overall Rating Review ", xlab="Rating")
  hist(beer.data.cleaned$ABV, breaks=20, col="blue", main = "Alcohol pr. Volume ", xlab="Alcohol %")
  
  #Summary of cleaned beer data
  print(summary(beer.data.cleaned))
  
  #Summary of the the data
  library(psych)
  print(describe(beer.data.cleaned))
  
  #Frequency data tables and cummulative percentages
  print("Frequency Apperance")
  print(create.frequency.table(beer.data.cleaned$reviewAppearance))
  print("Frequency Aroma")
  print(create.frequency.table(beer.data.cleaned$reviewAroma))
  print("Frequency Palete")
  print(create.frequency.table(beer.data.cleaned$reviewPalate))
  print("Frequency Taste")
  print(create.frequency.table(beer.data.cleaned$reviewTaste))
  print("Frequency Rating")
  print(create.frequency.table(beer.data.cleaned$rating))
  factorx <- factor(cut(beer.data.cleaned$ABV, breaks=seq(0,40,by=2)))
  
  #Print frequency distribution table for alcohol content
  #Tabulate and turn into data.frame
  xout <- as.data.frame(table(factorx))
  #Add cumFreq and proportions
  xout <- transform(xout, cumFreq = cumsum(Freq), relative = prop.table(Freq))
  print("Alcohol Content")
  print(xout)
  
  #Correlations 
  print("Correlation Matrix")
  print(cor(beer.data.cleaned[,c(6,8:12)],beer.data.cleaned[,c(6,8:12)], use="complete.obs"))
  
  # Third Correlation Example
  #library(corrgram)
  #corrgram(beer.data.cleaned[,c(6,8:12)], order=NULL, lower.panel=panel.shade,
  #         upper.panel=NULL, text.panel=panel.txt,
  #        main="Correlations Review Data")
  
  #Identify beer groups
  unique.beer.types <- unique(beer.data.cleaned$beerStyle)
  print("Unique beer groups")
  print(sort(unique.beer.types))
  print("Count of number of unique beer groups")
  print(NROW(unique.beer.types))
  
  #Identify unique profile names
  unique.profile.names <- unique(beer.data.cleaned$profileName)
  print("Reviews pr.profile")
  print(NROW(beer.data.cleaned)/NROW(unique.profile.names))
  
  #Select top 20% raters and identify how many reviews they represent of total ratings
  require(plyr)
  count.by.profile.name <- ddply(beer.data.cleaned, .(profileName), summarise, count = length(profileName))
  count.by.profile.name$of.total <- count.by.profile.name$count/NROW(beer.data.cleaned)
  count.by.profile.name[order(-count.by.profile.name$count),][c(1:(NROW(count.by.profile.name)*0.2)),]
  top.twenty.percent.profiles <- sum(count.by.profile.name[order(-count.by.profile.name$count),][c(1:(NROW(count.by.profile.name)*0.2)),3],  na.rm = TRUE)
  
  print(paste("20% of the raters stand for:",
              top.twenty.percent.profiles*100, " % of the ratings"))
  
  #Top 10 beer Styles, probability of getting a 5 with total number of ratings pr. group over 10 
  beer.groups.rating <- sqldf("Select beerStyle, 
                              sum(case when rating = 5 then 1 else 0 end) as number_of_fives,
                              count(rating) as total_number_of_ratings
                              from 'beer.data.cleaned'
                              group by beerStyle
                              having total_number_of_ratings > 99
                              ")
  sqldf()
  beer.groups.rating$probability_of_five <- beer.groups.rating$number_of_fives/beer.groups.rating$total_number_of_ratings
  beer.groups.rating.top10 <- beer.groups.rating[order(-beer.groups.rating$probability_of_five),][c(1:10),]
  
  #Plot Top 10 Beer Styles (where number of ratings have been over 100)
  par(las=2) # make label text perpendicular to axis
  par(mar=c(5,14,4,2)) # increase y-axis margin.
  
  barplot( beer.groups.rating.top10$probability_of_five, main="Top 10 Beer Styles (>= 99 ratings)", horiz=TRUE,
           names.arg= beer.groups.rating.top10$beerStyle, xlab="Probability of receiving five as rating")
  
  #Plot Bottom 10 Beer Styles (where number of ratings have been over 100)
  par(las=2) # make label text perpendicular to axis
  par(mar=c(5,14,4,2)) # increase y-axis margin.
  beer.groups.rating.bottom10 <- beer.groups.rating[order(beer.groups.rating$probability_of_five),][c(1:10),]
  
  barplot( beer.groups.rating.bottom10$probability_of_five, main="Bottom 10 Beer Styles (>= 99 ratings)", horiz=TRUE,
           names.arg= beer.groups.rating.bottom10$beerStyle, xlab="Probability of receiving five as rating")
  
  #Top 10 beers, probability of getting a 5 with total number of ratings pr. group over 100
  beer.names.rating <- sqldf("Select name, 
                             sum(case when rating = 5 then 1 else 0 end) as number_of_fives,
                             sum(case when rating = 1 then 1 else 0 end) as number_of_ones,
                             count(rating) as total_number_of_ratings
                             from 'beer.data.cleaned'
                             group by name
                             having total_number_of_ratings > 99 
                             ")
  sqldf()
  beer.names.rating$probability_of_five <- beer.names.rating$number_of_fives/beer.names.rating$total_number_of_ratings
  beer.names.rating$probability_of_one <- beer.names.rating$number_of_ones/beer.names.rating$total_number_of_ratings
  
  beer.names.rating.top10 <- beer.names.rating[order(-beer.names.rating$probability_of_five),][c(1:10),]
  
  #Plot Top 10 Beer Names (where number of ratings have been over or equal 100)
  par(las=2) # make label text perpendicular to axis
  par(mar=c(5,14,4,2)) # increase y-axis margin.
  
  barplot( beer.names.rating.top10$probability_of_five, main="Top 10 Beers (>= 100 ratings)", horiz=TRUE,
           names.arg= beer.names.rating.top10$name, xlab="Probability of receiving five as rating")
  
  #Plot Bottom 10 Beer Styles (where number of ratings have been over or equal100)
  par(las=2) # make label text perpendicular to axis
  par(mar=c(5,14,4,2)) # increase y-axis margin.
  beer.names.rating.bottom10 <- beer.names.rating[order(-beer.names.rating$probability_of_one),][c(1:10),]
  
  barplot( beer.names.rating.bottom10$probability_of_one, main="Bottom 10 Beers (>= 100 ratings)", horiz=TRUE,
           names.arg= beer.names.rating.bottom10$name, xlab="Probability of receiving one as rating")
  
  #Turn off graph warnings
  options(warn=0)
}

pca.analysis.falseratings<- function() {
  #Purpose:   -   Create PCA analysis
  
  #Standardize coefficients
  standardisedconcentrations <- as.data.frame(scale(beer.data.cleaned[,c(8,9,10,11,12)])) # standardise the variables 
  
  #PCA 
  beer.pca <- prcomp(standardisedconcentrations,retx=TRUE) # do a PCA
  print("PCA components without ABV (PCA seems to cover weird/false ratings)")
  print(beer.pca)
  
  print("First 6 observations that have highest scoring on PCA5 (notice reviewTaste and rating")
  print(beer.data.cleaned[head(order(-beer.pca$x[,5])),])
  
  #Order scoring on PCA component 5 take the top 1000
  PCA5.top.scores <- beer.data.cleaned[head(order(-beer.pca$x[,5]), 10000),]
  
  #Plot review vs taste rating for PCA6
  print(ggplot() +
          geom_histogram(data = PCA6.top.scores,aes(x=rating, y=..count../sum(..count..)),alpha = 0.2,fill = "steelblue") +
          geom_histogram(data = PCA6.top.scores,aes(x=reviewTaste, y=..count../sum(..count..)),alpha = 0.2,fill = "red") +
          xlab("Rating (Blue=Overall Rating), Taste (Red=Taste Rating)") +
          ylab("Count of reviews / Total reviews within group") +
          ggtitle("Taste Rating vs. Overall Rating for Top 1000 Scores for PCA5 Component") 
  )
  
  print("Frequency of overall rating for top 10 000 scores in PCA5")
  print(create.frequency.table(PCA5.top.scores$rating))
  print("Frequency of taste rating for top 10 000  scores in PCA5")
  print(create.frequency.table(PCA5.top.scores$reviewTaste))
}
frequency.ggplot.function <- function(){
  #Function for pretty frequency ggplots
  m <- ggplot(beer.data.cleaned, aes(x=reviewAppearance))
  m + geom_histogram(aes(fill = ..count..)) +
    xlab("Apperance Rating") +
    ylab("Nr. of reviews") +
    ggtitle("Frequency Plot: Apperance Rating")
  
  m <- ggplot(beer.data.cleaned, aes(x=reviewAroma))
  m + geom_histogram(aes(fill = ..count..)) +
    xlab("Aroma Rating") +
    ylab("Nr. of reviews") +
    ggtitle("Frequency Plot: Aroma Rating")
  
  m <- ggplot(beer.data.cleaned, aes(x=reviewPalate))
  m + geom_histogram(aes(fill = ..count..)) +
    xlab("Palete Rating") +
    ylab("Nr. of reviews") +
    ggtitle("Frequency Plot: Palete Rating")
  
  m <- ggplot(beer.data.cleaned, aes(x=reviewTaste))
  m + geom_histogram(aes(fill = ..count..)) +
    xlab("Taste Rating") +
    ylab("Nr. of reviews") +
    ggtitle("Frequency Plot: Taste Rating")
  
  m <- ggplot(beer.data.cleaned, aes(x=rating))
  m + geom_histogram(aes(fill = ..count..)) +
    xlab("Overall Rating") +
    ylab("Nr. of reviews") +
    ggtitle("Frequency Plot: Overall Rating")
  
  m <- ggplot(beer.data.cleaned, aes(x=ABV))
  m + geom_histogram(aes(fill = ..count..)) +
    xlab("Alcohol Percentage") +
    ylab("Nr. of reviews") +
    ggtitle("Frequency Plot: ABV")
}

pca.frequent.raters.vs.non.frequent.raters <- function() {
  #Purpose:   -   Create PCA analysis for raters that have made more than 10 beer ratings 
  #and the ones that have made less than equal 10 reviews.
  
  #Select high raters
  high.raters.profile.name <- sqldf("Select profileName, 
                                    count(rating) as total_number_of_ratings
                                    from 'beer.data.cleaned'
                                    group by profileName
                                    having total_number_of_ratings > 10
                                    ")
  #Nr of high raters
  NROW(high.raters.profile.name) 
  
  #Select low raters
  low.raters.profile.name <- sqldf("Select profileName, 
                                   count(rating) as total_number_of_ratings
                                   from 'beer.data.cleaned'
                                   group by profileName
                                   having total_number_of_ratings < 11
                                   ")
  NROW(low.raters.profile.name)
  
  #Standardize coefficients
  standardisedconcentrations.high.raters <- 
    as.data.frame(scale(beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE & beer.data.cleaned$profileName %in% high.raters.profile.name$profileName,c(6,8,9,10,11,12)])) # standardise the variables 
  print("Number of High Frequent Raters (ratings > 10) ")
  print(NROW(standardisedconcentrations.high.raters))
  standardisedconcentrations.low.raters <- 
    as.data.frame(scale(beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE & beer.data.cleaned$profileName %in% low.raters.profile.name$profileName,c(6,8,9,10,11,12)])) # standardise the variables
  print("Number of Low Frequent Raters (ratings < 11) ")
  print(NROW(standardisedconcentrations.low.raters))
  
  #PCA of high raters
  beer.high.raters.pca <- prcomp(standardisedconcentrations.high.raters) # do a PCA
  beer.low.raters.pca <- prcomp(standardisedconcentrations.low.raters)
  
  print("Frequent Raters PCA Summary (ratings > 10)")
  print(summary(beer.high.raters.pca))
  print("Non Frequent Raters PCA Summary (ratings =< 10)")
  print(summary(beer.low.raters.pca))
  
  print("Frequent Raters Loadings (ratings > 10)")
  print(beer.high.raters.pca$rotation[,])
  print("Non Frequent Raters Loadings (ratings =< 10)")
  print(  beer.low.raters.pca$rotation[,])
  
  #Classify Frequencies
  high.raters.raw <- as.data.frame((beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE & beer.data.cleaned$profileName %in% high.raters.profile.name$profileName,c(6,8,9,10,11,12)])) # standardise the variables 
  low.raters.raw <- as.data.frame((beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE & beer.data.cleaned$profileName %in% low.raters.profile.name$profileName,c(6,8,9,10,11,12)])) # standardise the variables 
  
  high.raters.raw$DataSource = 'High Frequent Raters'
  low.raters.raw$DataSource = 'Low Freqeunt Raters'
  
  #Merge both datasets
  both <- rbind( high.raters.raw,low.raters.raw)
  
  library(scales)
  print(ggplot() +
          geom_histogram(data = high.raters.raw,aes(x=rating, y=..count../sum(..count..)),alpha = 0.2,fill = "steelblue") +
    geom_histogram(data = low.raters.raw,aes(x=rating, y=..count../sum(..count..)),alpha = 0.2,fill = "red") +
          xlab("Rating") +
          ylab("Count of reviews / Total reviews within group") +
    ggtitle("Overall Rating, High Frequent Raters (> 10 Ratings) (Blue) \n vs. Low Frequent Raters (<= 10 Ratings) (Red)") 
  )
  
  print("High Frequent Raters (> 10 Ratings)  distribution")
  print(create.frequency.table(high.raters.raw$rating))
  print("Low Frequent Raters (> 10 Ratings)  distribution")
  print(create.frequency.table(low.raters.raw$rating))
  
}

pca.analysis<- function(filter.ABV=TRUE) {
  #Purpose: - Create PCA analysis
  
  #Standardize coefficients
  if(filter.ABV){
    standardisedconcentrations <- as.data.frame(scale(beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE,c(6,8,9,10,11,12)])) # standardise the variables
  }
  else{
    standardisedconcentrations <- as.data.frame(scale(beer.data.cleaned[,c(8,9,10,11,12)])) # standardise the variables
  }
  
  #PCA
  beer.pca <- prcomp(standardisedconcentrations) # do a PCA
  
  #Summary
  print(summary(beer.pca))
  
  #Screeplot
  screeplot(beer.pca, type="lines",main="Screeplot of PCA on beer data")
  
  #Loadings
  print(paste("Loadings"))
  print(beer.pca$rotation[,])
  
  #Plot pca
  plot(beer.pca$x[,1],beer.pca$x[,2]) # make a scatterplot
  plot(beer.pca$x[,1],beer.pca$x[,3])
  #text(beer.pca$x[,1],beer.pca$x[,2], beer.data.cleaned$beerStyle, cex=0.7, pos=4, col="red") # add labels
  
  #Plot component 1 against overall rating
  plot(beer.pca$x[,1],beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE,c(12)],ylab="Overall Rating",xlab="Principal Component 1")
  
  #Plot component 2 against overall rating
  plot(beer.pca$x[,2],beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE,c(12)],ylab="Overall Rating",xlab="Principal Component 2")
  
  #Plot component 1 against ABV
  plot(beer.pca$x[,1],beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE,c(6)],ylab="Alcohol Percentage %",xlab="Principal Component 1")
  
  #Plot component 2 against ABV
  plot(beer.pca$x[,2],beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE,c(6)],ylab="Alcohol Percentage %",xlab="Principal Component 2")
  
  #Find Mardia criterion (0.7 times the largest eigenvector)
  pca1.mardia.criterion <- max(beer.pca$rotation[,1])*0.7
  pca2.mardia.criterion <- max(beer.pca$rotation[,2])*0.7
  
  #Idenitfy egienvectors fullfilling criterion
  pca1.mardia.eigenvectors<-beer.pca$rotation[abs(beer.pca$rotation[,1])>pca1.mardia.criterion,1]
  pca2.mardia.eigenvectors<-beer.pca$rotation[abs(beer.pca$rotation[,2])>pca2.mardia.criterion,2]
  
  #Use Kaiser Criterion to pick out PCAs that are greater than the
  #meran of the eigenvector.
  mean.eigenvalues <- mean(beer.pca$sdev^2)
  
  #Only pick PC with eigenvalues > mean
  nr.of.pcas.kaiser <- beer.pca$sdev[beer.pca$sdev^2>mean(beer.pca$sdev^2)]
  
}

#Change path to local folder
#Load Beer Data, 10% as random sample. 
#beer.data <- load.beer.data.fast.method("C:\\Users\\creem013\\Documents\\Temp\\beeradvocate.txt", random.sample.percentage=0.1)

#Load R data set with all ratings
beer.data <- readRDS("C:/Users/Bollen/Google Drive/My StAndrews/Applied Multivariate Analysis/Project Work Group 1/DataSets/beerdatafull.RDS")

#Clean the data
#beer.data.cleaned<-clean.beer.data()
#head(beer.data.cleaned)
#saveRDS(beer.data.cleaned,"C:/Users/Bollen/Google Drive/My StAndrews/Applied Multivariate Analysis/Project Work Group 1/DataSets/beerdatacleaned.rds")

#Load cleane data set with all ratings
beer.data.cleaned <- readRDS("C:/Users/Bollen/Google Drive/My StAndrews/Applied Multivariate Analysis/Project Work Group 1/DataSets/beerdatacleaned.rds")

#Create 20% Random sample which should be a good representation of the data 
#as we have a lot of data
#beer.data.cleaned <- beer.data.cleaned[sample(1:nrow(beer.data.cleaned), nrow(beer.data.cleaned)*0.2,replace=FALSE),]

#Do summary on the data
descriptive.analysis()

#Frequency plots ggplot
frequency.ggplot.function()

#PCA frequent raters vs. non frequent raters
pca.frequent.raters.vs.non.frequent.raters()

#PCA Analysis Weird Ratings
pca.analysis.falseratings()

#Do PCA-Analysis
pca.analysis(filter.ABV=TRUE)


