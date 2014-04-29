library(sqldf)
library(corrplot)
library(plyr)
library(psych)
library(corrgram)
library(plyr)
library(ggplot2)
create.recommendation.matrix.pca1 <- function(){
  #Purpose:   -   Create recommendation matrix for top 20 beers
  #Returns the correlation matrix 
  
  #Standardize coefficients
  standardisedconcentrations <- as.data.frame(scale(beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE,c(6,8,9,10,11,12)]))
  
  #PCA 
  beer.pca <- prcomp(standardisedconcentrations,retx=TRUE) # do a PCA
  print("PCA components including all input variables")
  print(beer.pca)
  print(summary(beer.pca))
  
  print("First 6 observations that have highest scoring on PCA1")
  print(cbind(beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE,],beer.pca$x[,1])[head(order(-beer.pca$x[,1])),])
  
  #Identify top 20 rated beers
  top.20.rated.beers <- sqldf("Select name, 
                              count(rating) as total_number_of_ratings
                              from 'beer.data.cleaned'
                              group by name
                              order by total_number_of_ratings desc
                              limit 50
                              ")
  
  #Select dataset that only contains 20 top beer and PCA scoring
  top.20.rated.beers.and.PCA1.score <- cbind(beer.data.cleaned[is.na(beer.data.cleaned$ABV)==FALSE,],
                                             PCA1.score=beer.pca$x[,1])
  
  top.20.rated.beers.and.PCA1.score <- top.20.rated.beers.and.PCA1.score[top.20.rated.beers.and.PCA1.score$name 
                                                                         %in% top.20.rated.beers$name ,] 
  
  #Check if new dataset contains top 20 beers
  unique(top.20.rated.beers.and.PCA1.score$name)
  
  #Check nr. of rows in new data set
  nrow(top.20.rated.beers.and.PCA1.score)
  
  #Create all beer combinations
  beer.combinations <- expand.grid(beer1=top.20.rated.beers$name, beer2=top.20.rated.beers$name)
  
  #Remove comibination with itself
  beer.combinations <- beer.combinations[beer.combinations$beer1!=beer.combinations$beer2, ]
  
  #Create PCA1 correlations between the beer names, meaning similar preference in beer
  results <- ddply(beer.combinations, .(beer1, beer2), function(x) {                                   
    #Collect reviews of people who have made reviews on beer1 and beer2
    reviews1 <- subset(top.20.rated.beers.and.PCA1.score, name==x$beer1)
    reviews2 <- subset(top.20.rated.beers.and.PCA1.score, name==x$beer2)
    reviewers_sameset <- intersect(reviews1[,'profileName'],
                                   reviews2[,'profileName'])
    
    #Get PCA1 score for beer one
    top.20.rated.beers.and.PCA1.score.beer1 <- subset(top.20.rated.beers.and.PCA1.score, 
                                                      top.20.rated.beers.and.PCA1.score$name==x$beer1 &
                                                        top.20.rated.beers.and.PCA1.score$profileName %in% reviewers_sameset)
  
  
    #Get PCA1 score for beer one
    top.20.rated.beers.and.PCA1.score.beer2 <- subset(top.20.rated.beers.and.PCA1.score, 
                                                      top.20.rated.beers.and.PCA1.score$name==x$beer2 &
                                                        top.20.rated.beers.and.PCA1.score$profileName %in% reviewers_sameset)
    #Remove duplicates
    o <- order(top.20.rated.beers.and.PCA1.score.beer1$profileName)
    top.20.rated.beers.and.PCA1.score.beer1 <- top.20.rated.beers.and.PCA1.score.beer1[o,]
    dups <- duplicated(top.20.rated.beers.and.PCA1.score.beer1$profileName)==FALSE
    top.20.rated.beers.and.PCA1.score.beer1 <- top.20.rated.beers.and.PCA1.score.beer1[dups,]
    
    o <- order(top.20.rated.beers.and.PCA1.score.beer2$profileName)
    top.20.rated.beers.and.PCA1.score.beer2 <- top.20.rated.beers.and.PCA1.score.beer2[o,]
    dups <- duplicated(top.20.rated.beers.and.PCA1.score.beer2$profileName)==FALSE
    top.20.rated.beers.and.PCA1.score.beer2 <- top.20.rated.beers.and.PCA1.score.beer2[dups,]
    
    #Create PCA1 correlation
    c(cor(top.20.rated.beers.and.PCA1.score.beer1$PCA1.score, 
        top.20.rated.beers.and.PCA1.score.beer2$PCA1.score))
  })
  
  #Return recommendation
  setwd("C:/Users/creem013/Google Drive/My StAndrews/Applied Multivariate Analysis/Project Work Group 1/RecommendationSystem/")
  saveRDS(results,"results.rds")
  
  help(saveRDS)
  return(results)
}

get.recoomendations <- function(beer.that.you.like="Duvel"){
  #Purpose:   -   Create recommendation matrix for top 20 beers
  #Returns the correlation matrix
  #Fetch Recommendation
  recommendation.matrix <- create.recommendation.matrix.pca1()
  head(recommendation.matrix)
  
  #Produce list of other beers that the user might like
  unsorted.recommendations <- recommendation.matrix[recommendation.matrix$beer1==beer.that.you.like, ]
  sorted.recommendations <- unsorted.recommendations[order(-unsorted.recommendations$V1),]
  print("Top 5 recommendations based on input")
  print(head(sorted.recommendations[,2:3],5))
}

get.recoomendations()



