library(dplyr)
library(readxl)
library(ggplot2)

movie <- read_excel("MovieData-1xl.xlsx")
### Data cleaning ####

## Removed duplicate records


movie_no_dupl <- read.csv("movie_a.csv")

head(movie_no_dupl[,29])
head(movie_no_dupl[,30])


for(i in 29:56){
  movie_no_dupl[,i] = as.factor(movie_no_dupl[,i])
}

movie_no_dupl[,57] = as.numeric(movie_no_dupl[,57]) 

write.csv(movie_no_dupl, "movie_data_types.csv", row.names = FALSE)

write.csv(movie_no_dupl, "movie_data_types_basicEDA.csv", row.names = FALSE)


mystats <- function(y, na.omit=TRUE) {
  nmiss <- sum(is.na(y))
  x <- y[!is.na(y)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  min <- min(x)
  max <- max(x)
  p1 <- quantile(x, 0.01)
  p5 <- quantile(x, 0.05)
  q1 <- quantile(x, 0.25)
  q2 <- quantile(x, 0.50)
  q3 <- quantile(x, 0.75)
  p95 <- quantile(x, 0.95)
  p99 <- quantile(x, 0.99)
  UC <- q2+3*s               # replaced m with q2...to reduce contamination level.
  LC <- q2-3*s
  outlier_flag <- max>UC | min<LC
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, max=max, outlier_flag=outlier_flag, p1=p1, p5=p5, q1=q1, q2=q2, q3=q3, p95=p95,p99=p99,UC=UC, LC=LC ))
}

colnames(movie_no_dupl)

vars <- c( "duration","aspect_ratio","facenumber_in_poster", "movie_facebook_likes",
           "actor_1_facebook_likes","actor_2_facebook_likes","actor_3_facebook_likes",
           "director_facebook_likes","cast_total_facebook_likes",
           "num_critic_for_reviews","num_voted_users","num_user_for_reviews",
           "budget","imdb_score","gross")                               

stats<-t(data.frame(apply(movie_no_dupl[vars], 2, mystats)))

# Writing Summary stats to external file
write.csv(stats, file = "diag_stats.csv")


for(i in 1 : 2){
str(movie_no_dupl[i])
populated = ((4917 - sum(is.na(movie_no_dupl[i])))/ 4917)*100
print(populated)
print(summary(movie_no_dupl[i]))
}

summary(movie_no_dupl$imdb_score)

summary(movie_no_dupl)


# FOR SUCCESS\FAILURE CLASSIFICATION MODEL
movie_gross <- movie_no_dupl %>% filter(!is.na(movie_no_dupl$gross))

write.csv(movie_gross,"movie_gross.csv")

movie_gross <- read.csv("movie_gross_years_fixed.csv")

summary(movie_gross)
sum(is.na(movie_gross))

For_Budget = movie_gross %>% filter(!is.na(movie_gross$budget))
summary(For_Budget)

median(For_Budget$ROI)




# Missing value treatment
# num_critic_for_reviews has 3 NA's
movie_gross$num_critic_for_reviews[is.na(movie_gross$num_critic_for_reviews)] <- median(movie_gross$num_critic_for_reviews, na.rm = TRUE)

# num_critic_for_reviews has 2 NA's
movie_gross$duration[is.na(movie_gross$duration)] <- median(movie_gross$duration, na.rm = TRUE)

# director_facebook_likes has 2 NA's   (SKEWED DISTRIBUTIONS)
movie_gross$director_facebook_likes[is.na(movie_gross$director_facebook_likes)] <- median(movie_gross$director_facebook_likes, na.rm = TRUE)

# actor_3_facebook_likes has 13 NA's   (SKEWED DISTRIBUTIONS)
movie_gross$actor_3_facebook_likes[is.na(movie_gross$actor_3_facebook_likes)] <- median(movie_gross$actor_3_facebook_likes, na.rm = TRUE)

# actor_2_facebook_likes has 6 NA's   (SKEWED DISTRIBUTIONS)
movie_gross$actor_2_facebook_likes[is.na(movie_gross$actor_2_facebook_likes)] <- median(movie_gross$actor_2_facebook_likes, na.rm = TRUE)

# actor_1_facebook_likes has 4 NA's   (SKEWED DISTRIBUTIONS)
movie_gross$actor_1_facebook_likes[is.na(movie_gross$actor_1_facebook_likes)] <- median(movie_gross$actor_1_facebook_likes, na.rm = TRUE)

# facenumber_in_poster has 7 NA's   (SKEWED DISTRIBUTIONS)
movie_gross$facenumber_in_poster[is.na(movie_gross$facenumber_in_poster)] <- median(movie_gross$facenumber_in_poster, na.rm = TRUE)

# num_user_for_reviews has 1 NA's
movie_gross$num_user_for_reviews[is.na(movie_gross$num_user_for_reviews)] <- median(movie_gross$num_user_for_reviews, na.rm = TRUE)

# aspect_ratio has 104 NA's
movie_gross$aspect_ratio[is.na(movie_gross$aspect_ratio)] <- median(movie_gross$aspect_ratio, na.rm = TRUE)

# budget has 265 NA's  (Imputing by 1.065 times of gross)
movie_gross$budget_ROI =  movie_gross$gross/1.065
movie_gross$budget[is.na(movie_gross$budget)] <- movie_gross$budget_ROI

write.csv(movie_gross,"movie_gross.csv")


library(dplyr)

# Fixed Budget
movie_gross <- read.csv("movie_gross.csv")
nrow(movie_gross)

movie_gross = movie_gross %>% filter(!(content_rating == "GP" | content_rating == "TV-MA"))  

#GP
#TV-MA
movie_gross[,22] = as.factor(movie_gross[,22])

for(i in 29:54){
  movie_gross[,i] = as.factor(movie_gross[,i])
}

movie_gross[,59] = as.factor(movie_gross[,59])

summary(movie_gross)

movie_gross$Game.Show=NULL
movie_gross$Reality.TV=NULL

library(mlr)

set.seed(1)
sample <- sample.int(n = nrow(movie_gross), size = floor(.6*nrow(movie_gross)), replace = F)
train <- movie_gross[sample, ]
test  <- movie_gross[-sample, ]

nrow(train)
nrow(test)

full_data = rbind(train, test)

write.csv(train,"train.csv",row.names = FALSE)
write.csv(test,"test.csv", row.names = FALSE)

summary(train$content_rating)
levels(train$content_rating)

summary(test$content_rating)
levels(test$content_rating)


write.csv(full_data,"full_data.csv", row.names = FALSE)

glm_fit <- glm(Success~
                 #color+                 
                 #num_critic_for_reviews+      
                 #duration+                    
                 #director_facebook_likes+   
                 actor_3_facebook_likes+    
                 actor_1_facebook_likes+    
                 num_voted_users+           
                 cast_total_facebook_likes+ 
                 #facenumber_in_poster+      
                 #num_user_for_reviews+        
                 content_rating+          	
                 budget+                   	
                 title_year+                
                 actor_2_facebook_likes+    
                 #imdb_score+                
                 #aspect_ratio+              	
                 #movie_facebook_likes+      	
                 #Film.Noir+                 
                 #Action+                    	
                 #War+                       	
                 #History+                   	
                 #Western+                  	
                 #Documentary+               	
                 #Sport+                     	
                 #Thriller+                  	
                 #News+                      	
                 #Biography+                	
                 Comedy+                   	
                 #Mystery+                  	
                 #Musical+                  	
                 #Short+                     
                 #Adventure+                   
                 Horror+                      
                 Romance+                     
                 #Sci.Fi+                      
                 #Drama+                       
                 #Music+                       
                 #Game.Show+                   
                 #Crime+                       
                 #Fantasy+                     
                 #Animation+                   
                 Family+    
               #Reality.TV+
               No_Of_Genres
               #No_Of_Keywords
               , data = train, family = binomial(logit))


summary(glm_fit)



fitted.results <- predict(glm_fit,newdata=test,select=(c(3,5,6,8,13,14,22,23,24,25,30,34,39,40,44,45,52,53)),type='response',se.fit=FALSE)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
table(fitted.results,test$Success)
misClasificError <- mean(fitted.results != test$Success)
print(paste('Accuracy',1-misClasificError))

fitted.results <- predict(glm_fit,newdata=test,select=(c(3,5,6,8,13,14,22,23,24,25,30,34,39,40,44,45,52,53)),type='response',se.fit=FALSE)
fitted.results <- ifelse(fitted.results > 0.48,1,0)
ConfusionMatrix <- fitted.results
table(ConfusionMatrix,test$Success)
misClasificError <- mean(fitted.results != test$Success)
print(misClasificError)
print(paste('Accuracy',1-misClasificError))


library(ROCR)
pr <- prediction(fitted.results, test$Success)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)




library(randomForest)

rf_fit <- randomForest(Success~
                 color+                 
                 num_critic_for_reviews+      
                 duration+                    
                 director_facebook_likes+   
                 actor_3_facebook_likes+    
                 actor_1_facebook_likes+    
                 num_voted_users+           
                 cast_total_facebook_likes+ 
                 facenumber_in_poster+      
                 num_user_for_reviews+        
                 content_rating+          	
                 budget+                   	
                 title_year+                
                 actor_2_facebook_likes+    
                 #imdb_score+                
                 aspect_ratio+              	
                 movie_facebook_likes+      	
                 Film.Noir+                 
                 Action+                    	
                 War+                       	
                 History+                   	
                 Western+                  	
                 Documentary+               	
                 Sport+                     	
                 Thriller+                  	
                 News+                      	
                 Biography+                	
                 Comedy+                   	
                 Mystery+                  	
                 Musical+                  	
                 Short+                     
                 Adventure+                   
                 Horror+                      
                 Romance+                     
                 Sci.Fi+                      
                 Drama+                       
                 Music+                       
                 #Game.Show+                   
                 Crime+                       
                 Fantasy+                     
                 Animation+                   
                 Family+    
               #Reality.TV+
               No_Of_Genres+
               No_Of_Keywords
               , train,ntree=500)

summary(rf_fit)
rf_fit
predicted= predict(rf_fit,test)

table(predicted,test$Success)
misClasificError <- mean(predicted != test$Success)
print(paste('Accuracy',1-misClasificError))

rf_fit2 <- randomForest(Success~
                 #color+                 
                 #num_critic_for_reviews+      
                 #duration+                    
                 #director_facebook_likes+   
                 actor_3_facebook_likes+    
                 actor_1_facebook_likes+    
                 num_voted_users+           
                 cast_total_facebook_likes+ 
                 #facenumber_in_poster+      
                 #num_user_for_reviews+        
                 content_rating+          	
                 budget+                   	
                 title_year+                
                 actor_2_facebook_likes+    
                 #imdb_score+                
                 #aspect_ratio+              	
                 #movie_facebook_likes+      	
                 #Film.Noir+                 
                 #Action+                    	
                 #War+                       	
                 #History+                   	
                 #Western+                  	
                 #Documentary+               	
                 #Sport+                     	
                 #Thriller+                  	
               #News+                      	
               #Biography+                	
               Comedy+                   	
                 #Mystery+                  	
                 #Musical+                  	
                 #Short+                     
                 #Adventure+                   
                 Horror+                      
                 Romance+                     
                 #Sci.Fi+                      
                 #Drama+                       
                 #Music+                       
                 #Game.Show+                   
                 #Crime+                       
                 #Fantasy+                     
                 #Animation+                   
                 Family+    
                 #Reality.TV+
                 No_Of_Genres
               #No_Of_Keywords
               , train,ntree=500)

summary(rf_fit2)
rf_fit2
predicted= predict(rf_fit2,test)

table(predicted,test$Success)
misClasificError <- mean(predicted != test$Success)
print(paste('Accuracy',1-misClasificError))


## Linear Regression...predicting IMDB Score
lm_fit <- lm(imdb_score ~ 
                  #color+                 
                  num_critic_for_reviews+      
                  duration+                    
                  #director_facebook_likes+   
                  actor_3_facebook_likes+    
                  actor_1_facebook_likes+    
                  num_voted_users+           
                  cast_total_facebook_likes+ 
                  facenumber_in_poster+      
                  num_user_for_reviews+        
                  content_rating+          	
                  #budget+                   	
                  title_year+                
                  actor_2_facebook_likes+    
                  #imdb_score+                
                  #aspect_ratio+              	
                  movie_facebook_likes+      	
                  #Film.Noir+                 
                  Action+                    	
                  War+                       	
                  History+                   	
                  Western+                  	
                  Documentary+               	
                  Sport+                     	
                  Thriller+                  	
                  #News+                      	
                  Biography+                	
                  Comedy+                   	
                  Mystery+                  	
                  #Musical+                  	
                  #Short+                     
                  Adventure+                   
                  #Horror+                      
                  Romance+                     
                  Sci.Fi+                      
                  Drama+                       
                  Music+                       
                  Crime+                       
                  Fantasy+                     
                  Animation+                   
                  #Family+    
                  No_Of_Genres
                  #No_Of_Keywords
                 , data = full_data)

summary(lm_fit)

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lm_fit)

# Other useful functions 
coefficients(lm_fit) # model coefficients
confint(lm_fit, level=0.95) # CIs for model parameters 
fitted(lm_fit) # predicted values
residuals(lm_fit) # residuals
anova(lm_fit) # anova table 
vcov(lm_fit) # covariance matrix for model parameters 
influence(lm_fit) # regression diagnostics



# Function to remove Ã, leading and trailing whitespace from movies$movie_title
movie_title_processing <- function(str){
  str <- sub(pattern = "Ã", replacement = "", str)
  #str <- sub(pattern = "^//s+|//s+$", replacement ="", str)
}

# Apply previous function
movies$movie_title <- sapply(movies$movie_title, FUN = movie_title_processing)


movies <- movie %>% 
                filter(!is.na(movie$gross) & !is.na(movie$budget))

movies$News = NULL
movies$`Game-Show`=NULL
movies$`Reality-TV`=NULL

str(movies)

write.csv(movies, "NoNA_Budget_Gross.csv",row.names = FALSE)

df <- movies  # copy of original dataset

movies %>%
        summarise(
          dest_100 = nth(Dest,100))

head(movies[29])
head(movies[51])

df_mean_ROI  <- as.data.frame(matrix(0, ncol = 2, nrow = 23))

for(i in 29:51){
df_mean_ROI[i-28,1]= colnames(movies)[i] 
df_mean_ROI[i-28,2]= movies%>%filter(movies[i] == 1)%>%summarise(mean_ROI = mean(ROI, na.rm = TRUE)

df_mean_ROI[i-28,3]= movies%>%filter(movies[i] == 1)%>%summarise(median_ROI = median(ROI, na.rm = TRUE)
                                                                                                                                  
                    ,
                    min_ROI = min(ROI, na.rm = TRUE),
                    max_ROI = max(ROI, na.rm = TRUE)) 
}
write.csv(df_mean_ROI, "df_mean_ROI.csv",row.names = FALSE)


df_gross  <- as.data.frame(matrix(0, ncol = 2, nrow = 23), col.names = c("Genre","Gross"))

?as.data.frame

for(i in 29:51){
  df_gross[i-28,1]= colnames(movies)[i] 
  df_gross[i-28,2]= movies%>%filter(movies[i] == 1)%>%  summarise(total = sum(gross)) 
}
write.csv(df_gross, "df_gross.csv",row.names = FALSE)

ggplot(df_gross, aes(x=V1, y=V2)) +
       geom_bar(stat=identity())



action = movies %>%filter(Action == 1)%>% summarise(total = mean(ROI)) 


summary(movies$color)

movies$color = as.factor(movies$color)

as.factor(movies$director_name)


summary(movie$gross)
summary(movie$budget)

movie<-read.csv("movie.csv",header = T)

movie$ROI=(movie$gross-movie$budget)/movie$budget
movie<-movie[!duplicated(movie$movie_title),]

movie$Success = 
  


colnames(movie)



vars <- 


dupti<-movie[duplicated(movie$movie_title),]$movie_title

duplicates<-movie[movie$movie_title%in%dupti,c(12,1:11,13:28)]
duplicates<-arrange(duplicates,desc(movie_title))

dupti1<-movie[duplicated(duplicates$movie_title),]$movie_title
duplicates1<-duplicates[duplicates$movie_title%in%dupti1==FALSE,c(12,1:11,13:28)]
duplicates1<-arrange(duplicates1,desc(movie_title))

write.csv(movie,"stem.csv")

df <- read.csv("stem-new.csv", header = T)

independent <- df[c(1:6,8:11,14:47,49)]
movies <- independent[complete.cases(independent), ]

write.csv(movies,"movies_No_NA.csv")


glm_fit <- glm(Success~., data = independent, family = binomial(logit))




summary(glm_fit)

glm_fit2 <- glm(Success~
                  color+                 
                  num_critic_for_reviews+      
                  duration+                    
                  director_facebook_likes+   
                  actor_3_facebook_likes+    
                  actor_1_facebook_likes+    
                  num_voted_users+           
                  cast_total_facebook_likes+ 
                  facenumber_in_poster+      
                  num_user_for_reviews+        
                  content_rating+          	
                  budget+                   	
                  title_year+                
                  actor_2_facebook_likes+    
                  imdb_score+                
                  aspect_ratio+              	
                  movie_facebook_likes+      	
                  Film.Noir+                 
                  Action+                    	
                  War+                       	
                  History+                   	
                  Western+                  	
                  Documentary+               	
                  Sport+                     	
                  Thriller+                  	
                  News+                      	
                  Biography+                	
                  Comedy+                   	
                  Mystery+                  	
                  Musical+                  	
                  Short+                     
                  Adventure+                   
                  Horror+                      
                  Romance+                     
                  Sci.Fi+                      
                  Drama+                       
                  Music+                       
                  Game.Show+                   
                  Crime+                       
                  Fantasy+                     
                  Animation+                   
                  Family+    
                  Reality.TV+
                  No_Of_Genres, data = independent, family = binomial(logit))

summary(glm_fit2)


color+num_critic_for_reviews+duration+director_facebook_likes+actor_3_facebook_likes+
actor_1_facebook_likes+num_voted_users+cast_total_facebook_likes+content_rating+
budget+title_year+actor2_facebook_likes+movie_facebook_likes+aspect_ratio+Sci.Fi+Musical+
num_critic_for_reviews 




dupli<-movie[duplicated(movie),]$movie_title
length(dupti)
length(dupli)

# Remove instances which have at least one NA variable
movies <- movies[complete.cases(movies), ]

# Remove instances which are duplicated (duplicated based on title)
movies <- movies[!duplicated(movies$movie_title),]


movies <- movies[, c("movie_title", "gross", "imdb_score", "plot_keywords")]
# Function to remove Ã, leading and trailing whitespace from movies$movie_title
movie_title_processing <- function(str){
  str <- sub(pattern = "Ã", replacement = "", str)
  str <- sub(pattern = "^//s+|//s+$", replacement ="", str)
}

# Apply previous function
movies$movie_title <- sapply(movies$movie_title, FUN = movie_title_processing)
