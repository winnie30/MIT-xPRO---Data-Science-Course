#duplicated version!!!!!
#### Load Dataset #### 

setwd(choose.dir())
movies <- read.table("u.data", header = FALSE)

#pre-proccessing
colnames(movies) <- c('userId', 'itemId', 'rating', 'timestamp')

movies <- movies[,-4]
# movies <- movies[,-which(names(movies) %in% c('timSestamp'))] # use if you dont know the col number

#### Explore the dataset ####

str(movies)
table(movies$rating)

hist(x = movies$rating, xlab = "Rating", main = 'Histogram of Movie Rating', col = 'lightblue')

#### Calculating Data Sparsity ####

NumOfRatings  <- nrow(movies)
NumofUsers    <- length(unique(movies$userId))
NumofMovies   <- length(unique(movies$itemId))

sparsity <- NumOfRatings / (NumofUsers * NumofMovies)*100

#### Subsetting the data ####

movies.ss <- movies[movies$userId %in% names(table(movies$userId))[table(movies$userId)>50],]

movies.ss <- as(movies.ss, "realRatingMatrix")
movies.ss

#### Load recommender library ####

if(!require("recommnederlab")) install.packages("recommenderlab"); library("recommenderlab")

##########      Popularity Recommender System     #######

md.pop <- Recommender(data = movies.ss[1:560], method = "POPULAR")

names(getModel(md.pop))
getModel(md.pop)$topN

pr.pop <- predict(md.pop, movies.ss[561:563], n = 10 ) 

top5Pop <- bestN(pr.pop, n = 5)
as(top5Pop, Class = 'list')

############     Collaborative Filtering    ##############

# create validation/evaluation set
e <- evaluationScheme(movies.ss, method = 'split', train = 0.75, given = 10, goodRating = 3)

#User based collaborative filtering
md.ubcf <- Recommender(getData(e,'train'), 'UBCF')
pr.ubcf <- predict(md.ubcf, getData(e, 'known'), type = 'ratings')

#Item based collaborative filtering
md.ibcf <- Recommender(getData(e,'train'), 'IBCF')
pr.ibcf <- predict(md.ibcf, getData(e, 'known'), type = 'ratings')

error <- rbind(UBCF = calcPredictionAccuracy(pr.ubcf, getData(e, 'unknown')), IBCF = calcPredictionAccuracy(pr.ibcf,getData(e, 'unknown'))
      )
error

#### Top K-reccomendations
reco.ibcf <- predict(md.ibcf, getData(e, "known"), type = 'topNList')
Top10List <- as(bestN(reco.ibcf, n = 10),'list')
print(Top10List)

ev <- evaluate(e, "UBCF", n=10)
avg(ev)

#evaluating several algos

scheme <- evaluationScheme(movies.ss, method ='split', train =0.75, given =-5, k =1, goodRating =3)

algorithms <- list('POPULAR' = list(name = 'POPULAR', param = NULL),                   'ICBF' = list(name = 'ICBF', param =list(k=50)))
results <- evaluate(scheme, algorithms,type ='topNList', n = c(5,10,15))
help('evaluate')
