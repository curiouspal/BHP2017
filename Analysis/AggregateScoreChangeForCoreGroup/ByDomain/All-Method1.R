load("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BHP-New/BHP-2017-roundData/combined-Jan16-2018.RData")
final <- subset(final, PH_or_S8 == "PH")
final <- subset(final, !is.na(final$From_17) & !is.na(final$Surveyed_15))

for(i in 1:length(final$TCode)) {
  for(j in grep("Q8_", names(final), value = FALSE)) {
    if(!is.na(final[i, j]) & final[i, j]=="Not Applicable") final[i, j] <- NA
  }
}

createTable <- function(j, ques, number_of_subques) {
  temp <- data.frame(Q = c(1:number_of_subques), y15=NA, y17=NA, diff=NA)
  for(i in 1:number_of_subques) {
    temp$y15[i] <- (eval(parse(text = paste0("final$Q", ques, "_", as.character(i), "_15", sep="")))[j])
    temp$y17[i] <- (eval(parse(text = paste0("final$Q", ques, "_", as.character(i), "_17", sep="")))[j])
    print(c(temp$y15[i], temp$y17[i]))
  }
  temp$diff <- as.integer(temp$y17) - as.integer(temp$y15)
  return(temp)
}
#temp1 <- createTable(1, "8", 14)
#final$Q8_2_17[1]

# Note that "Yes" is coded as 2 and "No" is coded as 1.

#########################################################################################################################
##### THIS CODE CREATES A NEW COLUMN IN THE "FINAL" DATA FRAME WITH AN AGGREGATE SCORE OF EACH DOMAIN'S BARRIER CHANGE  #######
#########################################################################################################################


##### METHOD 1: response_2017 minus response_2015 and then adding up the differences for each TCode.



method1 <- function(j, ques, number_of_subques) {
  counted <- 0 
  x <- createTable(j, ques, number_of_subques)
  if(sum(as.integer(!is.na(x$diff)))>=1) counted <- 1  # counted is value 1 if the sum of x$diff is >= 1. Here, "counted" is 1 when at least one pair of 
                                                       # response_2015 - response_2017 has non-NA value.
  if(counted == 1) out <- sum(x$diff, na.rm = TRUE) else out <- NA 
  return(c(out, counted))
}
#temp <- method1(1, "8", 14) 
#sum(as.integer(!is.na(temp1$diff)))
##################################################################################################

### Employment
for(j in 1:length(final$TCode)) final$employment_aggregateScore[j] <- method1(j, "8", 14)[1] 
employment_N <- 0
for(j in 1:length(final$TCode)) {
  if(method1(j, "8", 14)[2]==1) employment_N <- employment_N +1 # If atleast one pair of response_15 - response_17 is non-NA for a TCode, count that TCode in total N.
}
summary(as.factor(final$employment_aggregateScore))
final$employment_aggregateScore1[is.na(final$employment_aggregateScore)] <- NA
final$employment_aggregateScore1[final$employment_aggregateScore>0] <- "Negative change in employment situation"
final$employment_aggregateScore1[final$employment_aggregateScore<0] <- "Positive change in employment situation"
final$employment_aggregateScore1[final$employment_aggregateScore==0] <- "No change in employment barrier"
final$employment_aggregateScore1 <- as.factor(final$employment_aggregateScore1)
summary(final$employment_aggregateScore1)
employment_N



### Education
for(j in 1:length(final$TCode)) final$education_aggregateScore[j] <- method1(j, "35", 12)[1] 
education_N <- 0
for(j in 1:length(final$TCode)) {
  if(method1(j, "35", 12)[2]==1) education_N <- education_N +1
}
summary(as.factor(final$education_aggregateScore))
final$education_aggregateScore1[is.na(final$education_aggregateScore)] <- NA
final$education_aggregateScore1[final$education_aggregateScore>0] <- "Negative change in education situation"
final$education_aggregateScore1[final$education_aggregateScore<0] <- "Positive change in education situation"
final$education_aggregateScore1[final$education_aggregateScore==0] <- "No change in education barrier"
final$education_aggregateScore1 <- as.factor(final$education_aggregateScore1)
summary(final$education_aggregateScore1)
education_N




### Housing
for(j in 1:length(final$TCode)) final$housing_aggregateScore[j] <- method1(j, "3", 9)[1] 
housing_N <- 0
for(j in 1:length(final$TCode)) {
  if(method1(j, "3", 9)[2]==1) housing_N <- housing_N +1
}
summary(as.factor(final$housing_aggregateScore))
final$housing_aggregateScore1[is.na(final$housing_aggregateScore)] <- NA
final$housing_aggregateScore1[final$housing_aggregateScore>0] <- "Negative change in housing situation"
final$housing_aggregateScore1[final$housing_aggregateScore<0] <- "Positive change in housing situation"
final$housing_aggregateScore1[final$housing_aggregateScore==0] <- "No change in housing barrier"
final$housing_aggregateScore1 <- as.factor(final$housing_aggregateScore1)
summary(final$housing_aggregateScore1)
housing_N



### Support networks
for(j in 1:length(final$TCode)) final$networks_aggregateScore[j] <- method1(j, "24", 6)[1] 
networks_N <- 0
for(j in 1:length(final$TCode)) {
  if(method1(j, "24", 6)[2]==1) networks_N <- networks_N +1
}
summary(as.factor(final$networks_aggregateScore))
final$networks_aggregateScore1[is.na(final$networks_aggregateScore)] <- NA
final$networks_aggregateScore1[final$networks_aggregateScore>0] <- "Negative change in networks situation"
final$networks_aggregateScore1[final$networks_aggregateScore<0] <- "Positive change in networks situation"
final$networks_aggregateScore1[final$networks_aggregateScore==0] <- "No change in networks barrier"
final$networks_aggregateScore1 <- as.factor(final$networks_aggregateScore1)
summary(final$networks_aggregateScore1)
networks_N



### Transportation
for(j in 1:length(final$TCode)) final$transportation_aggregateScore[j] <- method1(j, "18", 10)[1] 
transportation_N <- 0
for(j in 1:length(final$TCode)) {
  if(method1(j, "18", 10)[2]==1) transportation_N <- transportation_N +1
}
summary(as.factor(final$transportation_aggregateScore))
final$transportation_aggregateScore1[is.na(final$transportation_aggregateScore)] <- NA
final$transportation_aggregateScore1[final$transportation_aggregateScore>0] <- "Negative change in transportation situation"
final$transportation_aggregateScore1[final$transportation_aggregateScore<0] <- "Positive change in transportation situation"
final$transportation_aggregateScore1[final$transportation_aggregateScore==0] <- "No change in transportation barrier"
final$transportation_aggregateScore1 <- as.factor(final$transportation_aggregateScore1)
summary(final$transportation_aggregateScore1)
transportation_N


### Food
for(j in 1:length(final$TCode)) final$food_aggregateScore[j] <- method1(j, "15", 4)[1] 
food_N <- 0
for(j in 1:length(final$TCode)) {
  if(method1(j, "15", 4)[2]==1) food_N <- food_N +1
}
summary(as.factor(final$food_aggregateScore))
final$food_aggregateScore1[is.na(final$food_aggregateScore)] <- NA
final$food_aggregateScore1[final$food_aggregateScore>0] <- "Negative change in food situation"
final$food_aggregateScore1[final$food_aggregateScore<0] <- "Positive change in food situation"
final$food_aggregateScore1[final$food_aggregateScore==0] <- "No change in food barrier"
final$food_aggregateScore1 <- as.factor(final$food_aggregateScore1)
summary(final$food_aggregateScore1)
food_N



### Legal
for(j in 1:length(final$TCode)) final$legal_aggregateScore[j] <- method1(j, "27", 4)[1] 
legal_N <- 0
for(j in 1:length(final$TCode)) {
  if(method1(j, "27", 4)[2]==1) legal_N <- legal_N +1
}
summary(as.factor(final$legal_aggregateScore))
final$legal_aggregateScore1[is.na(final$legal_aggregateScore)] <- NA
final$legal_aggregateScore1[final$legal_aggregateScore>0] <- "Negative change in legal situation"
final$legal_aggregateScore1[final$legal_aggregateScore<0] <- "Positive change in legal situation"
final$legal_aggregateScore1[final$legal_aggregateScore==0] <- "No change in legal barrier"
final$legal_aggregateScore1 <- as.factor(final$legal_aggregateScore1)
summary(final$legal_aggregateScore1)
legal_N



### Income



for(j in 1:length(final$TCode)) final$income_aggregateScore[j] <- method1(j, "10", 8)[1] 
income_N <- 0
for(j in 1:length(final$TCode)) {
  if(method1(j, "10", 8)[2]==1) income_N <- income_N +1
}
summary(as.factor(final$income_aggregateScore))
final$income_aggregateScore1[is.na(final$income_aggregateScore)] <- NA
final$income_aggregateScore1[final$income_aggregateScore<0] <- "Negative change in income situation"
final$income_aggregateScore1[final$income_aggregateScore>0] <- "Positive change in income situation"
final$income_aggregateScore1[final$income_aggregateScore==0] <- "No change in income barrier"
final$income_aggregateScore1 <- as.factor(final$income_aggregateScore1)
summary(final$income_aggregateScore1)
income_N

temp <- createTable(3, "10", 8)
temp1 <- method1(3, "10", 8)

### Child care
for(j in 1:length(final$TCode)) final$childcare_aggregateScore[j] <- method1(j, "22", 3)[1] 
childcare_N <- 0
for(j in 1:length(final$TCode)) {
  if(method1(j, "22", 3)[2]==1) childcare_N <- childcare_N +1
}
summary(as.factor(final$childcare_aggregateScore))
final$childcare_aggregateScore1[is.na(final$childcare_aggregateScore)] <- NA
final$childcare_aggregateScore1[final$childcare_aggregateScore>0] <- "Negative change in childcare situation"
final$childcare_aggregateScore1[final$childcare_aggregateScore<0] <- "Positive change in childcare situation"
final$childcare_aggregateScore1[final$childcare_aggregateScore==0] <- "No change in childcare barrier"
final$childcare_aggregateScore1 <- as.factor(final$childcare_aggregateScore1)
summary(final$childcare_aggregateScore1)
childcare_N

################################################################################3
################################################################################
##### METHOD 2: Count of barriers in 2015 minus count of barriers in 2017 for each TCode.

method2 <- function(j, ques, number_of_subques) {
  include <- 1 
  x <- createTable(j, ques, number_of_subques)
  if(sum(as.integer(!is.na(x$y15)) + as.integer(!is.na(x$y17))) == 0) include <- 0
  if(include == 1) {
    out <- sum(as.integer(x$y17)==2, na.rm = TRUE) - sum(as.integer(x$y15)==2, na.rm = TRUE)
    avg15 <- sum(as.integer(x$y15)==2, na.rm = TRUE)
    avg17 <- sum(as.integer(x$y17)==2, na.rm = TRUE)
  }
  else {
    out <- NA
    avg15 <- NA
    avg17 <- NA
  }
  return(c(out, include, avg15, avg17))
}

method2income <- function(j, ques, number_of_subques) {
  include <- 1 
  x <- createTable(j, ques, number_of_subques)
  #if(sum(as.integer(!is.na(x$y15)) + as.integer(!is.na(x$y17))) == 0) include <- 0
  if(sum(as.integer(!is.na(x$y17))) == 0) include <- 0
  if(include == 1) {
    out <- sum((as.integer(x$y17)==1 | as.integer(x$y17)==2 ), na.rm = TRUE) - sum((as.integer(x$y15)==1 | as.integer(x$y15)==2 ), na.rm = TRUE)
    avg15 <- sum((as.integer(x$y15)==1 | as.integer(x$y15)==2 ), na.rm = TRUE)
    avg17 <- sum((as.integer(x$y17)==1 | as.integer(x$y17)==2 ), na.rm = TRUE)
  }
  else {
    out <- NA
    avg15 <- NA
    avg17 <- NA
  }
  return(c(out, include, avg15, avg17))
}

method2childcare <- function(j, ques, number_of_subques) {
  include <- 1 
  x <- createTable(j, ques, number_of_subques)
  if(sum(as.integer(!is.na(x$y15)) + as.integer(!is.na(x$y17))) == 0) include <- 0
  if(include == 1) {
    out <- sum((as.integer(x$y17)==4 | as.integer(x$y17)==5 ), na.rm = TRUE) - sum((as.integer(x$y15)==4 | as.integer(x$y15)==5 ), na.rm = TRUE)
    avg15 <- sum((as.integer(x$y15)==4 | as.integer(x$y15)==5 ), na.rm = TRUE)
    avg17 <- sum((as.integer(x$y17)==4 | as.integer(x$y17)==5 ), na.rm = TRUE)
  }
  else {
    out <- NA
    avg15 <- NA
    avg17 <- NA
  }
  return(c(out, include, avg15, avg17))
}

x <- createTable(5, "10", 8)


###########################################################################################################
###########################################################################################################

### Employment
for(j in 1:length(final$TCode)) final$employment_aggregateScore[j] <- method2(j, "8", 14)[1] 
employment_N <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "8", 14)[2]==1) employment_N <- employment_N +1
}

total15 <- 0
total17 <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "8", 14)[2]==1) {
    total15 <- total15 + method2(j, "8", 14)[3]
    total17 <- total17 + method2(j, "8", 14)[4]
  }
}
Average15 <- total15/employment_N
Average17 <- total17/employment_N

summary(as.factor(final$employment_aggregateScore))
final$employment_aggregateScore1[is.na(final$employment_aggregateScore)] <- NA
final$employment_aggregateScore1[final$employment_aggregateScore>0] <- "Negative change in employment situation"
final$employment_aggregateScore1[final$employment_aggregateScore<0] <- "Positive change in employment situation"
final$employment_aggregateScore1[final$employment_aggregateScore==0] <- "No change in employment barrier"
final$employment_aggregateScore1 <- as.factor(final$employment_aggregateScore1)
summary(final$employment_aggregateScore1)
employment_N
Average15
Average17



### Education
for(j in 1:length(final$TCode)) final$education_aggregateScore[j] <- method2(j, "35", 12)[1] 
education_N <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "35", 12)[2]==1) education_N <- education_N +1
}

total15 <- 0
total17 <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "35", 12)[2]==1) {
    total15 <- total15 + method2(j, "35", 12)[3]
    total17 <- total17 + method2(j, "35", 12)[4]
  }
}
Average15 <- total15/education_N
Average17 <- total17/education_N


summary(as.factor(final$education_aggregateScore))
final$education_aggregateScore1[is.na(final$education_aggregateScore)] <- NA
final$education_aggregateScore1[final$education_aggregateScore>0] <- "Negative change in education situation"
final$education_aggregateScore1[final$education_aggregateScore<0] <- "Positive change in education situation"
final$education_aggregateScore1[final$education_aggregateScore==0] <- "No change in education barrier"
final$education_aggregateScore1 <- as.factor(final$education_aggregateScore1)
summary(final$education_aggregateScore1)
education_N
Average15
Average17




### Housing
for(j in 1:length(final$TCode)) final$housing_aggregateScore[j] <- method2(j, "3", 9)[1] 
housing_N <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "3", 9)[2]==1) housing_N <- housing_N +1
}


total15 <- 0
total17 <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "3", 9)[2]==1) {
    total15 <- total15 + method2(j, "3", 9)[3]
    total17 <- total17 + method2(j, "3", 9)[4]
  }
}
Average15 <- total15/housing_N
Average17 <- total17/housing_N


summary(as.factor(final$housing_aggregateScore))
final$housing_aggregateScore1[is.na(final$housing_aggregateScore)] <- NA
final$housing_aggregateScore1[final$housing_aggregateScore>0] <- "Negative change in housing situation"
final$housing_aggregateScore1[final$housing_aggregateScore<0] <- "Positive change in housing situation"
final$housing_aggregateScore1[final$housing_aggregateScore==0] <- "No change in housing barrier"
final$housing_aggregateScore1 <- as.factor(final$housing_aggregateScore1)
summary(final$housing_aggregateScore1)
housing_N
Average15
Average17



### Support networks
for(j in 1:length(final$TCode)) final$networks_aggregateScore[j] <- method2(j, "24", 6)[1] 
networks_N <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "24", 6)[2]==1) networks_N <- networks_N +1
}

total15 <- 0
total17 <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "24", 6)[2]==1) {
    total15 <- total15 + method2(j, "24", 6)[3]
    total17 <- total17 + method2(j, "24", 6)[4]
  }
}
Average15 <- total15/networks_N
Average17 <- total17/networks_N


summary(as.factor(final$networks_aggregateScore))
final$networks_aggregateScore1[is.na(final$networks_aggregateScore)] <- NA
final$networks_aggregateScore1[final$networks_aggregateScore>0] <- "Negative change in networks situation"
final$networks_aggregateScore1[final$networks_aggregateScore<0] <- "Positive change in networks situation"
final$networks_aggregateScore1[final$networks_aggregateScore==0] <- "No change in networks barrier"
final$networks_aggregateScore1 <- as.factor(final$networks_aggregateScore1)
summary(final$networks_aggregateScore1)
networks_N
Average15
Average17



### Transportation
for(j in 1:length(final$TCode)) final$transportation_aggregateScore[j] <- method2(j, "18", 10)[1] 
transportation_N <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "18", 10)[2]==1) transportation_N <- transportation_N +1
}

total15 <- 0
total17 <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "18", 10)[2]==1) {
    total15 <- total15 + method2(j, "18", 10)[3]
    total17 <- total17 + method2(j, "18", 10)[4]
  }
}
Average15 <- total15/transportation_N
Average17 <- total17/transportation_N

summary(as.factor(final$transportation_aggregateScore))
final$transportation_aggregateScore1[is.na(final$transportation_aggregateScore)] <- NA
final$transportation_aggregateScore1[final$transportation_aggregateScore>0] <- "Negative change in transportation situation"
final$transportation_aggregateScore1[final$transportation_aggregateScore<0] <- "Positive change in transportation situation"
final$transportation_aggregateScore1[final$transportation_aggregateScore==0] <- "No change in transportation barrier"
final$transportation_aggregateScore1 <- as.factor(final$transportation_aggregateScore1)
summary(final$transportation_aggregateScore1)
transportation_N
Average15
Average17


### Food
for(j in 1:length(final$TCode)) final$food_aggregateScore[j] <- method2(j, "15", 4)[1] 
food_N <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "15", 4)[2]==1) food_N <- food_N +1
}


total15 <- 0
total17 <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "15", 4)[2]==1) {
    total15 <- total15 + method2(j, "15", 4)[3]
    total17 <- total17 + method2(j, "15", 4)[4]
  }
}
Average15 <- total15/food_N
Average17 <- total17/food_N

summary(as.factor(final$food_aggregateScore))
final$food_aggregateScore1[is.na(final$food_aggregateScore)] <- NA
final$food_aggregateScore1[final$food_aggregateScore>0] <- "Negative change in food situation"
final$food_aggregateScore1[final$food_aggregateScore<0] <- "Positive change in food situation"
final$food_aggregateScore1[final$food_aggregateScore==0] <- "No change in food barrier"
final$food_aggregateScore1 <- as.factor(final$food_aggregateScore1)
summary(final$food_aggregateScore1)
food_N
Average15
Average17



### Legal
for(j in 1:length(final$TCode)) final$legal_aggregateScore[j] <- method2(j, "27", 4)[1] 
legal_N <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "27", 4)[2]==1) legal_N <- legal_N +1
}


total15 <- 0
total17 <- 0
for(j in 1:length(final$TCode)) {
  if(method2(j, "27", 4)[2]==1) {
    total15 <- total15 + method2(j, "27", 4)[3]
    total17 <- total17 + method2(j, "27", 4)[4]
  }
}
Average15 <- total15/legal_N
Average17 <- total17/legal_N

summary(as.factor(final$legal_aggregateScore))
final$legal_aggregateScore1[is.na(final$legal_aggregateScore)] <- NA
final$legal_aggregateScore1[final$legal_aggregateScore>0] <- "Negative change in legal situation"
final$legal_aggregateScore1[final$legal_aggregateScore<0] <- "Positive change in legal situation"
final$legal_aggregateScore1[final$legal_aggregateScore==0] <- "No change in legal barrier"
final$legal_aggregateScore1 <- as.factor(final$legal_aggregateScore1)
summary(final$legal_aggregateScore1)
legal_N
Average15
Average17



### Income

for(j in 1:length(final$TCode)) final$income_aggregateScore[j] <- method2income(j, "10", 8)[1] 
income_N <- 0
for(j in 1:length(final$TCode)) {
  if(method2income(j, "10", 8)[2]==1) income_N <- income_N +1
}

total15 <- 0
total17 <- 0
for(j in 1:length(final$TCode)) {
  if(method2income(j, "10", 8)[2]==1) {
    total15 <- total15 + method2income(j, "10", 8)[3]
    total17 <- total17 + method2income(j, "10", 8)[4]
  }
}
Average15 <- total15/income_N
Average17 <- total17/income_N

summary(as.factor(final$income_aggregateScore))
final$income_aggregateScore1[is.na(final$income_aggregateScore)] <- NA
final$income_aggregateScore1[final$income_aggregateScore<0] <- "Negative change in income situation"
final$income_aggregateScore1[final$income_aggregateScore>0] <- "Positive change in income situation"
final$income_aggregateScore1[final$income_aggregateScore==0] <- "No change in income barrier"
final$income_aggregateScore1 <- as.factor(final$income_aggregateScore1)
summary(final$income_aggregateScore1)
income_N
Average15
Average17


### Child care
for(j in 1:length(final$TCode)) final$childcare_aggregateScore[j] <- method2childcare(j, "22", 3)[1] 
childcare_N <- 0
for(j in 1:length(final$TCode)) {
  if(method2childcare(j, "22", 3)[2]==1) childcare_N <- childcare_N +1
}

total15 <- 0
total17 <- 0
for(j in 1:length(final$TCode)) {
  if(method2childcare(j, "22", 3)[2]==1) {
    total15 <- total15 + method2childcare(j, "22", 3)[3]
    total17 <- total17 + method2childcare(j, "22", 3)[4]
  }
}
Average15 <- total15/childcare_N
Average17 <- total17/childcare_N


summary(as.factor(final$childcare_aggregateScore))
final$childcare_aggregateScore1[is.na(final$childcare_aggregateScore)] <- NA
final$childcare_aggregateScore1[final$childcare_aggregateScore>0] <- "Negative change in childcare situation"
final$childcare_aggregateScore1[final$childcare_aggregateScore<0] <- "Positive change in childcare situation"
final$childcare_aggregateScore1[final$childcare_aggregateScore==0] <- "No change in childcare barrier"
final$childcare_aggregateScore1 <- as.factor(final$childcare_aggregateScore1)
summary(final$childcare_aggregateScore1)
childcare_N
Average15
Average17

############################################################################################
################################################################################3
################################################################################
##### METHOD 3: In each of the domains, take ONLY all households that selected at least 1 barrier in 2015 (=N).  Then we see if they again selected the same barrier(s) in 2017.  In 2017, we count a nonresponse the same as a “no”.  E.g., if a household selected just one barrier in 2015 and then did not select it again in 2017 (no or nonresponse), we place that household in the “positive change” group.  If they selected it again in 2017, they fall in the “no change” group.  If they selected two or more barriers in 2015 and they selected at least one fewer of those same barriers, they fall in the “positive change” group. If they did not select any barrier in 2015, they are not part of the N for this analysis.

method3 <- function(j, ques, number_of_subques) {
  include <- 1 
  x <- createTable(j, ques, number_of_subques)
  if(sum(as.integer(is.na(x$y15))) == number_of_subques | 
     sum(as.integer(as.integer(x$y15)==2), na.rm = TRUE) == 0) include <- 0
  out <- NA
  if(include == 1) out <- "No change"
  for(i in 1:number_of_subques) if(!is.na(x$y15[i]) & x$y15[i]==2 & (is.na(x$y17[i]) | x$y17[i] < 2)) out <- "Positive change"
  return(c((out), include))
}

method3income <- function(j, ques, number_of_subques) {
  include <- 1 
  x <- createTable(j, ques, number_of_subques)
  if(sum(as.integer(is.na(x$y17))) == number_of_subques |
     #sum(as.integer(is.na(x$y15))) == number_of_subques & 
     sum(as.integer(as.integer(x$y15)<=2), na.rm = TRUE) == 0) include <- 0
  out <- NA
  if(include == 1) out <- "No change"
  for(i in 1:number_of_subques) if(include == 1 &
                                   !is.na(x$y15[i]) & 
                                   x$y15[i]<=2 & 
                                   (is.na(x$y17[i]) | x$y17[i] > x$y15[i])) out <- "Positive change"
  return(c((out), include))
}

method3childcare <- function(j, ques, number_of_subques) {
  include <- 1 
  x <- createTable(j, ques, number_of_subques)
  if(sum(as.integer(is.na(x$y15))) == number_of_subques | 
     sum(as.integer(as.integer(x$y15)>=4), na.rm = TRUE) == 0) include <- 0
  out <- NA
  if(include == 1) out <- "No change"
  for(i in 1:number_of_subques) if(!is.na(x$y15[i]) & x$y15[i]>=4 & (is.na(x$y17[i]) | x$y17[i] < x$y15[i])) out <- "Positive change"
  return(c((out), include))
}

method3(10, "35", 12)


###########################################################################################################
###########################################################################################################

### Employment
for(j in 1:length(final$TCode)) final$employment_aggregateScore[j] <- method3(j, "8", 14)[1] 
employment_N <- 0
for(j in 1:length(final$TCode)) {
  if(method3(j, "8", 14)[2]==1) employment_N <- employment_N +1
}
summary(as.factor(final$employment_aggregateScore))
employment_N



### Education
for(j in 1:length(final$TCode)) final$education_aggregateScore[j] <- method3(j, "35", 12)[1] 
education_N <- 0
for(j in 1:length(final$TCode)) {
  if(method3(j, "35", 12)[2]==1) education_N <- education_N +1
}
summary(as.factor(final$education_aggregateScore))
education_N




### Housing
for(j in 1:length(final$TCode)) final$housing_aggregateScore[j] <- method3(j, "3", 9)[1] 
housing_N <- 0
for(j in 1:length(final$TCode)) {
  if(method3(j, "3", 9)[2]==1) housing_N <- housing_N +1
}
summary(as.factor(final$housing_aggregateScore))
housing_N



### Support networks
for(j in 1:length(final$TCode)) final$networks_aggregateScore[j] <- method3(j, "24", 6)[1] 
networks_N <- 0
for(j in 1:length(final$TCode)) {
  if(method3(j, "24", 6)[2]==1) networks_N <- networks_N +1
}
summary(as.factor(final$networks_aggregateScore))
networks_N



### Transportation
for(j in 1:length(final$TCode)) final$transportation_aggregateScore[j] <- method3(j, "18", 10)[1] 
transportation_N <- 0
for(j in 1:length(final$TCode)) {
  if(method3(j, "18", 10)[2]==1) transportation_N <- transportation_N +1
}
summary(as.factor(final$transportation_aggregateScore))
transportation_N


### Food
for(j in 1:length(final$TCode)) final$food_aggregateScore[j] <- method3(j, "15", 4)[1] 
food_N <- 0
for(j in 1:length(final$TCode)) {
  if(method3(j, "15", 4)[2]==1) food_N <- food_N +1
}
summary(as.factor(final$food_aggregateScore))
food_N



### Legal
for(j in 1:length(final$TCode)) final$legal_aggregateScore[j] <- method3(j, "27", 4)[1] 
legal_N <- 0
for(j in 1:length(final$TCode)) {
  if(method3(j, "27", 4)[2]==1) legal_N <- legal_N +1
}
summary(as.factor(final$legal_aggregateScore))
legal_N



### Income

for(j in 1:length(final$TCode)) final$income_aggregateScore[j] <- method3income(j, "10", 8)[1] 
income_N <- 0
for(j in 1:length(final$TCode)) {
  if(method3income(j, "10", 8)[2]==1) income_N <- income_N +1
}
summary(as.factor(final$income_aggregateScore))
income_N


### Child care
for(j in 1:length(final$TCode)) final$childcare_aggregateScore[j] <- method3childcare(j, "22", 3)[1] 
childcare_N <- 0
for(j in 1:length(final$TCode)) {
  if(method3childcare(j, "22", 3)[2]==1) childcare_N <- childcare_N +1
}
summary(as.factor(final$childcare_aggregateScore))
childcare_N



