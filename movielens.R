
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# # MovieLens 10M dataset:
# # https://grouplens.org/datasets/movielens/10m/
# # http://files.grouplens.org/datasets/movielens/ml-10m.zip
# 
# dl <- tempfile()
# download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
# 
# ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                  col.names = c("userId", "movieId", "rating", "timestamp"))
# 
# movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
# colnames(movies) <- c("movieId", "title", "genres")
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# 
# movielens <- left_join(ratings, movies, by = "movieId")
# 
# # Validation set will be 10% of MovieLens data
# set.seed(1, sample.kind="Rounding")
# # if using R 3.5 or earlier, use `set.seed(1)` instead
# test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
# edx <- movielens[-test_index,]
# temp <- movielens[test_index,]
# 
# # Make sure userId and movieId in validation set are also in edx set
# validation <- temp %>% 
#   semi_join(edx, by = "movieId") %>%
#   semi_join(edx, by = "userId")
# 
# # Add rows removed from validation set back into edx set
# removed <- anti_join(temp, validation)
# edx <- rbind(edx, removed)
# 
# rm(dl, ratings, movies, test_index, temp, movielens, removed)
#save(edx,file = "edx.RData")
#load("/Users/user/edx.RData")
edx<-readRDS("edx.rds")
validation<-readRDS("validation.rds")
#### data exploration 

# structure of edx dataset
str(edx)
# features of edx dataset
head(edx) 

# edx summary statistics
summary(edx)

#number of movies by rating
sum(edx$rating==0)# check first if any movie has been rated 0
rate_distribution<-edx%>% group_by(rating)%>%summarize(n=n())
rate_distribution%>% knitr::kable(col.names = c("Rating","Number of rates"))

# histogram showing the distribution of ratings
edx%>% group_by(rating)%>%ggplot(aes(x=rating))+geom_histogram()
#### number of movie rating for each single movie
edx%>% group_by(movieId)%>%summarize(number_movies=n())%>%ggplot(aes(number_movies))+geom_histogram(bins=30,color="grey")+scale_x_log10()+ggtitle("Distribution of movies")+xlab("Number of movies")+ylab("Number of ratings")
#number of unique users and unique movies in the ed dataset
edx %>% summarize(n_users=n_distinct(userId),n_movies= n_distinct(movieId))%>% knitr::kable(col.names = c("Number of users","Number of Movies"))

# movie ratings in each  genre 
genre<- edx%>% separate_rows(genres, sep = "\\|")%>% group_by(genres) %>% summarize(count=n()) %>% arrange(desc(count))
genre%>%ggplot(aes(x=reorder(genres,count),y=count))+geom_bar(stat = "identity", fill="lightgrey")+labs(x="",y="Number of ratings")+coord_flip()+geom_text(aes(label=count),hjust=-0.1,size=3)+labs(title = "Number of ratings by movie genre")
### Average ratttting per genres
genre_rating<- edx%>% separate_rows(genres, sep = "\\|")%>% group_by(genres)%>%summarize(n=mean(rating))%>%arrange(desc(n))

genre_rating  %>%ggplot(aes(x=n,y=reorder(genres,n)))+geom_bar(stat = "identity", fill="lightgrey")+labs(x=" Average ratings per Genres",y="Genres")+labs(title = "Average of ratings by movie genre")

#### which movie has greatest number of ratings
edx%>% group_by(title,genres)%>% summarize(n=n())%>%top_n(10,n)%>%arrange(desc(n))%>%knitr::kable(col.names = c("Title","Genre","Number of ratings"))

## top 10 movies based on number of ratings
top_10<-edx%>% group_by(title,genres)%>% summarize(n=n())%>%arrange(desc(n))

top_10[1:10,]%>%ggplot(aes(x=reorder(title,n),y=n))+geom_bar(stat = "identity", fill="lightgrey")+labs(x="",y="Number of ratings")   +coord_flip()+geom_text(aes(label=n),color="darkgrey")+labs(title = "Top 10 movies based on number of ratings")

#### top 10 movies average of rating
ratebygenre<-edx%>% group_by(genres)%>% summarize(n=n(),mean_rate=mean(rating))%>%arrange(desc(n))
  
#### RATING DISTRIBUTION BASED ON DAYS
edx$timestamp<-as_datetime(as.POSIXct(edx$timestamp, origin= "1970-01-01"))
edx_d<-edx%>%mutate(days=weekdays(edx$timestamp),monthss=months(edx$timestamp),years=year(edx$timestamp))
edx_d$days<- factor(edx_d$days,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))## ordering days
edx_d$monthss <- factor(edx_d$monthss,levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))##ordering months
edx_d%>%group_by(monthss)%>%summarize(n=n())%>%arrange(desc(n))%>%knitr::kable(col.names = c("Month","Number of ratings"))## rating distribution based on months
edx_d%>%group_by(days)%>%summarize(n=n())%>%arrange(desc(n))%>%knitr::kable(col.names = c("Days","Number of ratings"))##rating
edx_d%>%group_by(years)%>%summarize(n=n())%>%arrange(desc(n))%>%knitr::kable(col.names = c("Years","Number of ratings"))##rating

#### comparison based on weekdays
edx_d%>%group_by(days,rating)%>%summarize(n=n())%>%ggplot(aes(days,n))+geom_point(stat="identity",aes(color=rating,size=rating))+scale_color_gradient(low = "yellow",high = "blue")+labs(x="Weekday",y="Number of ratings",title = "Weekday rating comparison" ) 
## comparison based on months
edx_d%>%group_by(monthss,rating)%>%summarize(n=n())%>%ggplot(aes(months,n,fill=monthss))+geom_bar(stat="identity")+labs(x="Months",y="Number of ratings",title = "Months rating comparison" )

#number of ratings per weekdays
edx_d%>%group_by(days,monthss)%>%summarize(n=n())%>%ggplot(aes(monthss,n))+geom_bar(stat = "identity",aes(fill=days),position = "dodge")+labs(x="Month",y="Number of ratings",title = "Weekday and month rating comparison" )
###Dialy and monthly rating comparison
edx_d%>%group_by(days,monthss,rating)%>%summarize(n=n())%>%ggplot(aes(days,n))+geom_jitter(aes(color=rating,size=rating),alpha=0.55)+scale_color_gradient(low = "yellow",high = "blue") + theme(axis.text.x = element_text(angle = 45,hjust = 1))+facet_wrap(vars(monthss))

## year of release
release<-regmatches(edx$title, gregexpr( "(?<=\\().+?(?=\\))", edx$title, perl = T))
edx<-cbind(edx,release)



##edx data set will splited into training (90%) and test set.
set.seed(1, sample.kind="Rounding")
edx_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
 train_edx <- edx[-edx_index,]
 temp<- edx[edx_index,]

 
 ##make sure userID and movie Id in test set and in train set
test_edx<-temp%>% semi_join(train_edx, by="movieId")%>%
   semi_join(train_edx,by="userId")
 ## add rows from test set back intor train set
 removed<-anti_join(temp,test_edx)
 train_edx<-rbind(train_edx,removed)
 rm(edx_index,temp,removed)

 ## clean data
 train_edx<-train_edx%>%select(userId,movieId,rating,title)
 test_edx<-test_edx%>%select(userId,movieId,rating,title)
### Loss function RMSE

RMSE <- function (true_ratings, predicted_ratings) {
  sqrt(mean ((true_ratings-predicted_ratings)^2))
}

### Model 1 : Baseline
mu_hat<-mean(train_edx$rating)
baseline_RMSE<-RMSE(test_edx$rating,mu_hat)

RMSE_results<-data.frame(method="Average rating movie model", RMSE=baseline_RMSE)
RMSE_results%>% knitr::kable()

### Model 2 : adding movie effect  b_v
movie_avgs<-train_edx%>%group_by(movieId)%>% summarize(b_v = mean(rating-mu_hat))
head(movie_avgs)
# predict the rating with b_v
y_hat_movie_avgs<-mu_hat+test_edx%>%left_join(movie_avgs,by="movieId")%>% pull(b_v)
rmse_b_v<-RMSE(test_edx$rating,y_hat_movie_avgs)
## add the results to RMSE results
RMSE_results<-rbind(RMSE_results, data.frame( method="Average rating movie + b_v ", RMSE=rmse_b_v))

## model 3 include user effect b_u

movie_users<-train_edx%>%left_join(movie_avgs,by="movieId")%>% group_by(userId)%>% summarize(b_u=mean(rating-mu_hat-b_v))
head(movie_users)
# predict the rating with b_i
y_hat_model<-test_edx%>%left_join(movie_avgs,by="movieId")%>%left_join(movie_users,by="userId")%>%mutate(pred=mu_hat+b_v+b_u)%>% pull(pred)
rmse_b_v_b_u<-RMSE(test_edx$rating,y_hat_model)
## add the results to RMSE results
RMSE_results<-rbind(RMSE_results, data.frame( method="Average rating movie + b_v +b_u", RMSE=rmse_b_v_b_u))

##### Evaluating the model
movie_titles<-train_edx%>% select(movieId,title)%>%distinct()
#10 best movie according our model based on b_k
train_edx%>%count(movieId)%>%left_join(movie_avgs)%>%left_join(movie_titles,by="movieId")%>%arrange(desc(b_v))%>%select(title,b_v,n)%>% slice(1:10)
# 10 worst movies according our modek based on b_k
train_edx%>%count(movieId)%>%left_join(movie_avgs)%>%left_join(movie_titles,by="movieId")%>%arrange(b_v)%>%select(title,b_v,n)%>% slice(1:10)
### regularization 
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_edx$rating)
  b_i <- train_edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_edx %>%
    left_join(b_i, by="movieId") %>% group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    test_edx %>%
    left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)
  return(RMSE(predicted_ratings, test_edx$rating))
})
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)] 
lambda
RMSE_results <- rbind(RMSE_results,
                          data.frame(method="Regularized Movie + User Effect Model",RMSE=min(rmses)))
                                 knitr::kable(RMSE_results)
###validation 
                                 mu_edx <- mean(edx$rating)
                                 bi_edx <- edx %>%
                                   group_by(movieId) %>%
                                   summarize(b_i = sum(rating - mu_edx)/(n()+lambda))
                                 
                                 bu_edx <-edx %>%left_join(bi_edx, by="movieId") %>% group_by(userId) %>%
                                   summarize(b_u = sum(rating - b_i - mu_edx)/(n()+lambda))
                                 predicted_ratings <-validation%>%left_join(bi_edx, by = "movieId") %>% left_join(bu_edx, by = "userId") %>% mutate(pred = mu_edx + b_i + b_u) %>% pull(pred)
                                 val_pred<-RMSE(validation$rating,predicted_ratings)
                                 
                                 
                                 RMSE_results <- rbind(RMSE_results,data.frame(Method="Validation of edx data set using Movie and User Effect Model",RMSE=val_pred))
                                 knitr::kable(RMSE_results)     
                                 ```                                 
                                 