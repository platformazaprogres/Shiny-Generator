######funkcija za workoute
random2 <- function(exercises_in_round, number_exercise, hardness, type1) {
  library(dplyr)
  print(type1)
  t1 <- type1[1]
  t2 <- type1[2]
  t3 <- type1[3]
  exercises_in_round["hardness"] <- hardness
  
  exercises_in_round%>%
    filter(V3 == t1 | V3 == t2 | V3 == t3) -> exercises_in_round
  
  exercises_in_round[sample(nrow(exercises_in_round), number_exercise, replace = T), ] -> exercises_in_round
  
  as.data.frame(exercises_in_round) -> exercises_in_round
  
  ifelse(exercises_in_round$level == "H" & hardness == "Beginner", sample(seq(from = 5, to = 15, by= 5), 1), 
         ifelse(exercises_in_round$level == "H" & hardness == "Intermediate", yes = sample(seq(from = 15, to = 30, by= 5), 1),
                ifelse(exercises_in_round$level == "H" & hardness == "Advanced", yes = sample(seq(from = 30, to = 50, by= 5), 1),
                       ifelse(exercises_in_round$level == "M" & hardness == "Beginner", yes = sample(seq(from = 10, to = 25, by= 5), 1),
                              ifelse(exercises_in_round$level == "M" & hardness == "Intermediate", yes = sample(seq(from = 40, to = 60, by= 5), 1),
                                     ifelse(exercises_in_round$level == "M" & hardness == "Advanced", yes = sample(seq(from = 55, to = 75, by= 5), 1),
                                            ifelse(exercises_in_round$level == "E" & hardness == "Beginner", yes = sample(seq(from = 15, to = 45, by= 5), 1),
                                                   ifelse(exercises_in_round$level == "E" & hardness == "Intermediate", yes = sample(seq(from = 50, to = 60, by= 5), 1),
                                                          no = sample(seq(from = 60, to = 75, by= 5)))))))))) -> exercises_in_round$time
  
  
  ifelse(exercises_in_round$level == "H" & exercises_in_round$time <= 50, sample(seq(from = 20, to = 30, by= 5), 1), 
         ifelse(exercises_in_round$level == "M" & exercises_in_round$time <= 50, sample(seq(from = 25, to = 40, by = 5), 1), sample(seq(from = 10, to = 35, by = 5), 1))) -> exercises_in_round$rest
  
  
  ifelse(exercises_in_round$level == "H" & hardness == "Beginner", sample(seq(from = 5, to = 15, by= 5), 1), 
         ifelse(exercises_in_round$level == "H" & hardness == "Intermediate", yes = sample(seq(from = 10, to = 20, by= 5), 1),
                ifelse(exercises_in_round$level == "H" & hardness == "Advanced", yes = sample(seq(from = 15, to = 30, by= 5), 1),
                       ifelse(exercises_in_round$level == "M" & hardness == "Beginner", yes = sample(seq(from = 10, to = 25, by= 5), 1),
                              ifelse(exercises_in_round$level == "M" & hardness == "Intermediate", yes = sample(seq(from = 25, to = 40, by= 5), 1),
                                     ifelse(exercises_in_round$level == "M" & hardness == "Advanced", yes = sample(seq(from = 30, to = 50, by= 5), 1),
                                            ifelse(exercises_in_round$level == "E" & hardness == "Beginner", yes = sample(seq(from = 15, to = 30, by= 5), 1),
                                                   ifelse(exercises_in_round$level == "E" & hardness == "Intermediate", yes = sample(seq(from = 35, to = 50, by= 5), 1),
                                                          no = sample(seq(from = 40, to = 75, by= 5)))))))))) -> exercises_in_round$reps
  
  
  sum(exercises_in_round$time) -> sum.time
  sum(exercises_in_round$rest) -> sum.rest
  sum(sum.time, sum.rest) -> sum.sum
  
  ifelse(sum.sum <= 250, 30, ifelse(sum.sum <= 350, 45, ifelse(sum.sum <= 200, 20, 60))) -> exercises_in_round$rest_round[number_exercise]
  
  return(exercises_in_round)
}

