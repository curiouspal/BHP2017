load("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BHP-New/BHP-2017-roundData/combined-Jan16-2018.RData")
final <- subset(final, PH_or_S8 == "PH")
final <- subset(final, !is.na(final$From_17) & !is.na(final$Surveyed_15))

for(i in 1:length(final$TCode)) {
  for(j in grep("Q8_", names(final), value = FALSE)) {
    if(!is.na(final[i, j]) & final[i, j]=="Not Applicable") final[i, j] <- NA
  }
}

#######################################################################################################
## this index would measure for each subquestion of (only) legal domain, if a family moved between 2015 and 2017 from “very difficult “ to something 
## better (including “somewhat difficult”) or from “somewhat difficult” to something better.  Then add up the results for all 
## subquestions for the legal domain. This variation of Method 1 would indicate progress made by families on barriers that 
## were very or somewhat difficult for them in 2015.
#######################################################################################################



createTableImproved <- function(j, ques, number_of_subques) {
  temp <- data.frame(Q = c(1:number_of_subques), y15=NA, y17=NA, diff=NA)
  for(i in 1:number_of_subques) {
    temp$y15[i] <- (eval(parse(text = paste0("final$Q", ques, "_", as.character(i), "_15", sep="")))[j])
    temp$y17[i] <- (eval(parse(text = paste0("final$Q", ques, "_", as.character(i), "_17", sep="")))[j])
    print(c(temp$y15[i], temp$y17[i]))
  }
  temp$diff <- 0
  temp$diff[as.integer(temp$y15) %in% c(2) & (temp$y15) > (temp$y17)] <- 1
  print(temp)
  return(temp)
}

temp1 <- createTableImproved(7, "27", 3)

method1improved <- function(j, ques, number_of_subques) {
  counted <- 0 
  x <- createTableImproved(j, ques, number_of_subques)
  out <- sum(x$diff)
  return(out)
}



for(j in 1:length(final$TCode)) final$legal_aggregateScorePos[j] <- method1improved(j, "27", 3)

summary(as.factor(final$legal_aggregateScorePos))
plot((as.factor(final$legal_aggregateScorePos)))

progressed <- final$TCode[final$legal_aggregateScorePos!=0]
##################### Deteriorated

createTableWorsened <- function(j, ques, number_of_subques) {
  temp <- data.frame(Q = c(1:number_of_subques), y15=NA, y17=NA, diff=NA)
  for(i in 1:number_of_subques) {
    temp$y15[i] <- (eval(parse(text = paste0("final$Q", ques, "_", as.character(i), "_15", sep="")))[j])
    temp$y17[i] <- (eval(parse(text = paste0("final$Q", ques, "_", as.character(i), "_17", sep="")))[j])
    print(c(temp$y15[i], temp$y17[i]))
  }
  temp$diff <- 0
  temp$diff[as.integer(temp$y15) %in% c(1) & (temp$y15) < (temp$y17)] <- 1
  print(temp)
  return(temp)
}

temp1 <- createTable(7, "27", 3)

method1worsened <- function(j, ques, number_of_subques) {
  counted <- 0 
  x <- createTableWorsened(j, ques, number_of_subques)
  out <- sum(x$diff)
  return(out)
}



for(j in 1:length(final$TCode)) final$legal_aggregateScoreNeg[j] <- method1worsened(j, "27", 3)

summary(as.factor(final$legal_aggregateScoreNeg))
plot((as.factor(final$legal_aggregateScoreNeg)))

regressed <- final$TCode[final$legal_aggregateScoreNeg!=0]
#### (1) how many of the 39 who progressed are NOT ALSO in the group of 37 who regressed (i.e., these are the ones who in 2017 did better on the barriers that in 2015 were very or somewhat difficult w/o falling into either of the two worst categories)
length(setdiff(progressed, regressed))

#### (2) how many fell in 2017 into the very or somewhat difficult category from a better 2015 position w/o ALSO during this time moving out of either one of the two worst categories.
length(setdiff(regressed, progressed))


################################################################################3
################################################################################
##### As a variation of Method 2, another index would look for each Q10 subquestion whether in 2015 a family was in a very or somewhat difficult situation, aggregate the results for the legal domain in 2015 and also in 2017 and then see for each family if they have a higher, lower, or same aggregate “very or somewhat difficult” legal score in 2017 compared with 2015.
##### METHOD 2: Count of barriers in 2015 minus count of barriers in 2017 for each TCode.
################################################################################

method2legal <- function(j, ques, number_of_subques) {
  include <- 1 
  x <- createTable(j, ques, number_of_subques)
  count <- 0
  for(i in 1:length(x$y15)) if(x$y15[i] %in% c(1,2)) count <- 1 
  out15 <- sum(x$y15, na.rm = TRUE)
  out17 <- sum(x$y17, na.rm = TRUE)
  
  return(c(out15, out17, count))
}

temp1 <- method2legal(5, "27", 3)
final$AggregatelegalMethod2 <- NA
for(i in 1:length(final$TCode)) if(method2legal(i, "27", 3)[3]==1) final$AggregatelegalMethod2[i] <- method2legal(i, "27", 3)[2] - method2legal(i, "27", 3)[1]

summary(as.factor(final$AggregatelegalMethod2))
final$temp[final$AggregatelegalMethod2>0] <- "Positive"
final$temp[final$AggregatelegalMethod2==0] <- "Neutral"
final$temp[final$AggregatelegalMethod2<0] <- "Negative"

summary(as.factor(final$temp))


###########################################################################################################
###########################################################################################################


## Analogous to the variation on Method1, a variation of Method 3 could just focus on those that selected “urgent” or “vulnerable”  
## and see if families who selected “urgent” in 2015 chose something better in 2017 (including “vulnerable”) or if families who selected
## “vulnerable” in 2015, chose something better in 2017 (those will be positive changes).  Negative changes will be when a family chose
## something better than “vulnerable” in 2015, and regressed to either “vulnerable” or “urgent” in 2017, or regressed from “vulnerable” to “urgent.”

summary(final$Q36_3_15)
summary(final$Q36_3_17)
final$temp <- NA
for(i in 1:length(final$TCode)) {
  if(!is.na(final$Q36_3_15[i]) & final$Q36_3_15[i] %in% c("1. Urgent situation, currently in crisis", "2. Vulnerable, need support to move forward")) {
    if(!is.na(final$Q36_3_15[i]) & !is.na(final$Q36_3_17[i]) & as.integer(final$Q36_3_17[i]) > as.integer(final$Q36_3_15[i])) final$temp[i] <- "Positive" 
  }
}
for(i in 1:length(final$TCode)) {
  if(!is.na(final$Q36_3_17[i]) & final$Q36_3_17[i] %in% c("1. Urgent situation, currently in crisis", "2. Vulnerable, need support to move forward")) {
    if(!is.na(final$Q36_3_15[i]) & !is.na(final$Q36_3_17[i]) & as.integer(final$Q36_3_17[i]) < as.integer(final$Q36_3_15[i])) final$temp[i] <- "Negative" 
  }
}

summary(as.factor(final$temp))