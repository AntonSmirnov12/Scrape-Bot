library(RCurl)
library(dplyr)
library(tidyr)
library("stringr")
library(PlayerRatings)

start_year <- 1968
end_year <- 2022

url_1 <-"https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_"
# url_2 <- "https://github.com/JeffSackmann/tennis_wta/blob/master/wta_matches_" #women's


for (i in start_year:end_year){
  urlfile <- paste(url_1,i, sep = "")
  urlfile <- paste(urlfile,".csv",sep="")
  url_name <- str_extract(url_1,"\\w+$") 
  nam <- paste(url_name,i,sep="")
  assign(nam, (read.csv(urlfile)))
}
tennis_data.pattern <- paste(url_name, start_year:end_year, sep="")
tennis_data_list <- lapply(tennis_data.pattern,get)

All_matches <- Reduce(function(x,y) rbind(x,y), tennis_data_list)
All_matches <- All_matches %>% distinct(tourney_date, winner_name,loser_name, .keep_all = TRUE) %>% arrange(tourney_date)

All_matches_reduced_points <- All_matches %>% select(surface,tourney_date,winner_name,loser_name,w_svpt,w_1stWon,w_2ndWon,l_svpt,l_1stWon,l_2ndWon)
All_matches_reduced_points$win_serve_prob <- (All_matches_reduced_points$w_1stWon+All_matches_reduced_points$w_2ndWon)/All_matches_reduced_points$w_svpt
All_matches_reduced_points$win_receive_prob <- (All_matches_reduced_points$l_1stWon+All_matches_reduced_points$l_2ndWon)/All_matches_reduced_points$l_svpt
All_matches_reduced_points <- All_matches_reduced_points %>% select (surface,tourney_date,winner_name,loser_name, win_serve_prob,win_receive_prob)
complete_probs <- na.omit(All_matches_reduced_points) 

complete_probs <- complete_probs %>% filter(surface=="Hard")%>% select(!surface) %>%
  mutate(tourney_date = floor(tourney_date/100))

lose_probs <- complete_probs
lose_probs$random <- lose_probs$winner_name
lose_probs$winner_name <- lose_probs$loser_name
lose_probs$loser_name <- lose_probs$random
lose_probs$win_serve_prob <- lose_probs$win_receive_prob
lose_probs <- lose_probs %>% select(!random)

final_probs <- rbind(complete_probs,lose_probs) %>% select(!win_receive_prob)%>%arrange(tourney_date)
colnames(final_probs) <- c("tourney_date","server","receiver","server_win_prob")

final_probs$server <- lapply(final_probs$server, function(x) paste(x,'serve'))
final_probs$receiver <- lapply(final_probs$receiver, function(x) paste(x,'receive'))
final_probs <- final_probs %>% mutate(server = sapply(server, toString),receiver = sapply(receiver, toString))

Hard_points_serve_ratings <- glicko(final_probs)
Hard_points_serve_ratings_df <- Hard_points_serve_ratings[["ratings"]]
# Hard_points_serve_ratings_df <- Hard_points_serve_ratings_df %>% filter(Lag<10 & Games > 50)

# find the serve and receive ratings of a particular player

Player_1 <- "Aliassime"
Player_2 <- "Rublev"

Hard_points_serve_ratings_df %>% filter(str_detect(Player, Player_1))
Hard_points_serve_ratings_df %>% filter(str_detect(Player, Player_2))

P1_serve <- (Hard_points_serve_ratings_df %>% filter(str_detect(Player, Player_1)) %>% select("Rating"))[1,1]
P1_receive <- (Hard_points_serve_ratings_df %>% filter(str_detect(Player, Player_1)) %>% select("Rating"))[2,1]

P2_serve <- (Hard_points_serve_ratings_df %>% filter(str_detect(Player, Player_2)) %>% select("Rating"))[1,1]
P2_receive <- (Hard_points_serve_ratings_df %>% filter(str_detect(Player, Player_2)) %>% select("Rating"))[2,1]

p_1 <-  1/(1+10^((P2_receive-P1_serve)/400))
p_2 <- 1/(1+10^((P2_serve-P1_receive)/400))

# calculating implied p_serve and p_receive from Sportsbet set odds e.g. Aliassime vs Rublev

odds = c(81,19,11,6.5,5.5,13,7.5,151,26,15,8,7,15,8.5)
vec <- c("6-0","6-1","6-2","6-3","6-4","7-5","7-6","0-6","1-6","2-6","3-6","4-6","5-7","6-7")

error_func <- function(p){
  p_1 <- p[1]
  p_2 <- p[2]
  probabilities_finder <- rep(0,length(odds))
  for (i in 1:length(odds)){
    first_num <- as.numeric(substr(vec[i],1,1))
    second_num <- as.numeric(substr(vec[i],3,3))
    score <- c(first_num,second_num)
    probabilities_finder[i] <- prob_score(score,p_1,p_2)
  }
  # sum_of_errors <- sum(((probabilities_finder-(1/odds))^2)*odds)
  sum_of_errors <- sum((probabilities_finder-(1/odds))^2)
  sum_of_errors
}

result <- optim(par = c(0.3,0.1), fn = error_func)
variables <- result[1]
my_result <- unlist(variables)
my_result <- unname(my_result)
p_1 <- my_result[1]
p_2 <- my_result[2]

implied_probs <- rep(0,length(odds))

for (i in 0:6){
  implied_probs[(i+1)] <- prob_score(c(7,i),p_1,p_2)
  implied_probs[(8+i)] <- prob_score(c(i,7),p_1,p_2)
}

points_model_probs <- rep(0,length(odds))

for (i in 0:6){
  points_model_probs[(i+1)] <- prob_score(c(7,i),serve_prob,receive_prob)
  points_model_probs[(8+i)] <- prob_score(c(i,7),serve_prob,receive_prob)
}

# plotting:
x <- seq(1,length(odds))
plot(1/odds,xaxt="n",
     main="Graphing set score odds",
     xlab = "Score",
     ylab="Prob",
     type="l",
     col="blue")
lines(implied_probs, col="red")
lines(points_model_probs,col="green")
legend(0.8,0.18,legend=c("Sportsbet odds","implied Sportsbet odds","model odds"),fill=c("blue","red","green"))
axis(1, at=1:length(odds), labels=vec)
print(p_1)
print(p_2)
print(1/implied_probs)


total_sum <- 0
for (i in 1:7){
  total_sum <- total_sum +implied_probs[i]  
}
print(total_sum)

my_df <- data.frame(vec,odds,1/implied_probs,1/points_model_probs)
colnames(my_df) <- c("score","odds","implied odds","model_odds")
my_df


