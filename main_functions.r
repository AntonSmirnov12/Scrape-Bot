win_game <- function(p){
  sum <- 0
  for (i in 4:6){
    sum <- sum + p^i*(1-p)^(6-i)*choose(6,i)
  }
  i <- 3
  sum <- sum + p^i*(1-p)^(6-i)*choose(6,i)*(p^2)/((p^2)+(1-p)^2)
  return(sum)
}

tiebreak_win_func <- function(serve_prob,receive_prob, n=7){
  p_1 <- serve_prob
  p_2 <- receive_prob
  
  sum <- 0
  
  for (i in 0:n){
    for (j in (n-i):n){
      if (i+j >n){
        sum <- sum + p_1^i*(1-p_1)^(n-i)*choose(n,i)*(p_2)^j*(1-p_2)^(n-j)*choose(n,j)
      }
      else{
        sum <- sum + p_1^i*(1-p_1)^(n-i)*choose(n,i)*(p_2)^j*(1-p_2)^(n-j)*choose(n,j)*(p_1*p_2)/((p_1*p_2)+(1-p_1)*(1-p_2))
      }
    }
  }
  return(sum)
}

prob_score <- function(score,serve_prob,receive_prob){
  stable_score <- score
  p_1 <- win_game(serve_prob)
  p_2 <- win_game(receive_prob)
  tie_break_win <- tiebreak_win_func(serve_prob,receive_prob)
  
  if (stable_score[1]<stable_score[2]){
    p_1 <- 1-p_1
    p_2 <- 1-p_2
    tie_break_win <- 1-tie_break_win
    score[1] <- stable_score[2]
    score[2] <- stable_score[1]
  }
  
  p_55 <- sum(p_1^5*(1-p_2)^5+p_1^4*(1-p_1)*p_2*(1-p_2)^4*5*5+p_1^3*(1-p_1)^2*p_2^2*(1-p_2)^3*10*10,
              p_2^5*(1-p_1)^5+p_2^4*(1-p_2)*p_1*(1-p_1)^4*5*5+p_2^3*(1-p_2)^2*p_1^2*(1-p_1)^3*10*10)
  
  if (score[2] == 0){
    prob <- p_1^3*p_2^3
  } else if(score[2]==1){
    prob <- (p_1^3*p_2^2*(1-p_2)*3+p_1^2*(1-p_1)*p_2^3*3)*(p_1+p_2)/2
  } else if(score[2]==2){
    prob <- sum((p_1^4*p_2*(1-p_2)^2*3+p_1^3*(1-p_1)*p_2^2*(1-p_2)*4*3+p_1^2*(1-p_1)^2*p_2^3*6)*p_2,
                (p_2^4*p_1*(1-p_1)^2*3+p_2^3*(1-p_2)*p_1^2*(1-p_1)*4*3+p_2^2*(1-p_2)^2*p_1^3*6)*p_1)/2  
  } else if (score[2]==3){
    prob <- sum((p_1^4*p_2*(1-p_2)^3*4+p_1^3*(1-p_1)*p_2^2*(1-p_2)^2*4*6+p_1^2*(1-p_1)^2*p_2^3*(1-p_2)*6*4),
                p_1*(1-p_1)^3*p_2^4*4)*(p_1+p_2)/2
  } else if (score[2]==4){
    prob <- sum(sum(p_1^5*(1-p_2)^4+p_1^4*(1-p_1)*p_2*(1-p_2)^3*5*4+p_1^3*(1-p_1)^2*p_2^2*(1-p_2)^2*10*6,
                    p_1^2*(1-p_1)^3*p_2^3*(1-p_2)*10*4+p_1*(1-p_1)^4*p_2^4*5)*p_2,
                sum(p_2^5*(1-p_1)^4+p_2^4*(1-p_2)*p_1*(1-p_1)^3*5*4+p_2^3*(1-p_2)^2*p_1^2*(1-p_1)^2*10*6,
                    p_2^2*(1-p_2)^3*p_1^3*(1-p_1)*10*4+p_2*(1-p_2)^4*p_1^4*5)*p_1)/2
  } else if (score[2]==5){
    prob <- p_1*p_2*p_55
  } else if (score[2]==6){
    prob <- (p_1*(1-p_2)+(1-p_1)*p_2)*p_55*tie_break_win
  }
  
  prob
}
