load("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BHP-New/BHP-2017-roundData/combined-Jan16-2018.RData")
final <- subset(final, PH_or_S8 == "PH")
final <- subset(final, !is.na(final$From_15) & !is.na(final$From_17))



final$Q10_1_17[final$Q10_1_17=="Not Applicable"] <- NA
final$Q10_2_17[final$Q10_2_17=="Not Applicable"] <- NA
final$Q10_3_17[final$Q10_3_17=="Not Applicable"] <- NA
final$Q10_4_17[final$Q10_4_17=="Not Applicable"] <- NA
final$Q10_5_17[final$Q10_5_17=="Not Applicable"] <- NA
final$Q10_6_17[final$Q10_6_17=="Not Applicable"] <- NA
final$Q10_7_17[final$Q10_7_17=="Not Applicable"] <- NA
final$Q10_8_17[final$Q10_8_17=="Not Applicable"] <- NA

final$Q10_1_17 <- ordered(final$Q10_1_17, 
                          levels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"),
                          labels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"))
final$Q10_2_17 <- ordered(final$Q10_1_17, 
                          levels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"),
                          labels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"))
final$Q10_3_17 <- ordered(final$Q10_1_17, 
                          levels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"),
                          labels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"))
final$Q10_4_17 <- ordered(final$Q10_1_17, 
                          levels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"),
                          labels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"))
final$Q10_5_17 <- ordered(final$Q10_1_17, 
                          levels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"),
                          labels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"))
final$Q10_6_17 <- ordered(final$Q10_1_17, 
                          levels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"),
                          labels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"))
final$Q10_7_17 <- ordered(final$Q10_1_17, 
                          levels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"),
                          labels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"))
final$Q10_8_17 <- ordered(final$Q10_1_17, 
                          levels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"),
                          labels = c("1. Very Difficult", "2. Somewhat Difficult", "3. Neutral", "4. Somewhat Easy", "5. Somewhat Easy"))




#######################################################################################################
#Income barriers Q10:1-8
### METHOD 3: the count of the number of 2015 "Yes" responses that changed to "No" in 2017
#######################################################################################################

aggregateScore <- function(j) {
  total <- 0
  count <- 0
  for(i in 1:8) {
    response_15 <- (eval(parse(text = paste0("final$Q10_", as.character(i), "_15", sep="")))[j])
    response_17 <- (eval(parse(text = paste0("final$Q10_", as.character(i), "_17", sep="")))[j])
    if(!is.na(response_15) & 
       (as.integer(response_15) == 2 | as.integer(response_15) == 1) & 
       is.na(response_17)) response_17 <- "3. Neutral"
    if(!is.na(response_15) & !is.na(response_17) & (as.integer(response_15) == 2 | as.integer(response_15) == 1)) {
      count <- count + 1
      if(!is.na(response_17) & (as.integer(response_17) == 3 | as.integer(response_17) == 4 | as.integer(response_17) == 5)) total <- total + as.integer(response_17) - as.integer(response_15)
    }
  }
  return(c(total, count))
}

for(j in 1:length(final$TCode)) final$income_aggregateScore[j] <- aggregateScore(j)[1]
c <- 0
for(j in 1:length(final$TCode)) if(aggregateScore(j)[2]>=1) c <- c + 1

summary(as.factor(final$income_aggregateScore))


#######################################################################################################
#Food barriers Q15:1-4
### METHOD 3: the count of the number of 2015 "Yes" responses that changed to "No" in 2017
#######################################################################################################

aggregateScore <- function(j) {
  total <- 0
  count <- 0
  for(i in 1:4) {
    response_15 <- (eval(parse(text = paste0("final$Q15_", as.character(i), "_15", sep="")))[j])
    response_17 <- (eval(parse(text = paste0("final$Q15_", as.character(i), "_17", sep="")))[j])
    if(!is.na(response_15) & !is.na(response_17) & (as.integer(response_15) == 2)) {
      count <- count + 1
      if(is.na(response_17) | (!is.na(response_17) & as.integer(response_17) == 1)) {
        print(c(response_15, response_17)) 
        total <- total + as.integer(response_17) - as.integer(response_15)
      }
    }
  }
  return(c(total, count))
}

for(j in 1:length(final$TCode)) final$food_aggregateScore[j] <- aggregateScore(j)[1]
c <- 0
for(j in 1:length(final$TCode)) if(aggregateScore(j)[2]>=1) c <- c + 1


summary(as.factor(final$food_aggregateScore))


#######################################################################################################
#Transportation barriers Q18:1-10
### METHOD 3: the count of the number of 2015 "Yes" responses that changed to "No" in 2017
#######################################################################################################

aggregateScore <- function(j) {
  total <- 0
  count <- 0
  for(i in 1:10) {
    response_15 <- (eval(parse(text = paste0("final$Q18_", as.character(i), "_15", sep="")))[j])
    response_17 <- (eval(parse(text = paste0("final$Q18_", as.character(i), "_17", sep="")))[j])
    if(!is.na(response_15) & !is.na(response_17) & (as.integer(response_15) == 2)) {
      count <- count + 1
      if(is.na(response_17) | (!is.na(response_17) & as.integer(response_17) == 1)) {
        print(c(response_15, response_17)) 
        total <- total + as.integer(response_17) - as.integer(response_15)
      }
    }
  }
  return(c(total, count))
}

for(j in 1:length(final$TCode)) final$transportation_aggregateScore[j] <- aggregateScore(j)[1]
c <- 0
for(j in 1:length(final$TCode)) if(aggregateScore(j)[2]>=1) c <- c + 1


summary(as.factor(final$transportation_aggregateScore))


#######################################################################################################
#Childcare barriers Q22:1-3
### METHOD 3: the count of the number of 2015 "Yes" responses that changed to "No" in 2017
#######################################################################################################

aggregateScore <- function(j) {
  total <- 0
  count <- 0
  for(i in 1:3) {
    response_15 <- (eval(parse(text = paste0("final$Q22_", as.character(i), "_15", sep="")))[j])
    response_17 <- (eval(parse(text = paste0("final$Q22_", as.character(i), "_17", sep="")))[j])
    if(!is.na(response_15) & !is.na(response_17) & (as.integer(response_15) == 2)) {
      count <- count + 1
      if(is.na(response_17) | (!is.na(response_17) & as.integer(response_17) == 1)) {
        print(c(response_15, response_17)) 
        total <- total + as.integer(response_17) - as.integer(response_15)
      }
    }
  }
  return(c(total, count))
}

for(j in 1:length(final$TCode)) final$childcare_aggregateScore[j] <- aggregateScore(j)[1]
c <- 0
for(j in 1:length(final$TCode)) if(aggregateScore(j)[2]>=1) c <- c + 1


summary(as.factor(final$childcare_aggregateScore))


#######################################################################################################
#Network barriers Q24:1-6
### METHOD 3: the count of the number of 2015 "Yes" responses that changed to "No" in 2017
#######################################################################################################

aggregateScore <- function(j) {
  total <- 0
  count <- 0
  for(i in 1:6) {
    response_15 <- (eval(parse(text = paste0("final$Q24_", as.character(i), "_15", sep="")))[j])
    response_17 <- (eval(parse(text = paste0("final$Q24_", as.character(i), "_17", sep="")))[j])
    if(!is.na(response_15) & !is.na(response_17) & (as.integer(response_15) == 2)) {
      count <- count + 1
      if(is.na(response_17) | (!is.na(response_17) & as.integer(response_17) == 1)) {
        print(c(response_15, response_17)) 
        total <- total + as.integer(response_17) - as.integer(response_15)
      }
    }
  }
  return(c(total, count))
}

for(j in 1:length(final$TCode)) final$networks_aggregateScore[j] <- aggregateScore(j)[1]
c <- 0
for(j in 1:length(final$TCode)) if(aggregateScore(j)[2]>=1) c <- c + 1


summary(as.factor(final$networks_aggregateScore))



#######################################################################################################
#Legal barriers Q27: 1-4
### METHOD 3: the count of the number of 2015 "Yes" responses that changed to "No" in 2017
#######################################################################################################

aggregateScore <- function(j) {
  total <- 0
  count <- 0
  for(i in 1:4) {
    response_15 <- (eval(parse(text = paste0("final$Q27_", as.character(i), "_15", sep="")))[j])
    response_17 <- (eval(parse(text = paste0("final$Q27_", as.character(i), "_17", sep="")))[j])
    if(!is.na(response_15) & !is.na(response_17) & (as.integer(response_15) == 2)) {
      count <- count + 1
      if(is.na(response_17) | (!is.na(response_17) & as.integer(response_17) == 1)) {
        print(c(response_15, response_17)) 
        total <- total + as.integer(response_17) - as.integer(response_15)
      }
    }
  }
  return(c(total, count))
}

for(j in 1:length(final$TCode)) final$legal_aggregateScore[j] <- aggregateScore(j)[1]
c <- 0
for(j in 1:length(final$TCode)) if(aggregateScore(j)[2]>=1) c <- c + 1


summary(as.factor(final$legal_aggregateScore))
