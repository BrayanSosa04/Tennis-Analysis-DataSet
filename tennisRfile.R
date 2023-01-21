library(caret) 
library(e1071) #for svm() and tune() function
library(class) #for knn()

setwd("C:/Users/melis/OneDrive/Documents/Data Science & Stats Project") #the directory is set to change depending on person
tennis = read.csv("data.csv", header = T) #make sure header is set to true since we have column names
#the above two lines are to load the data set into R

#Bet365 holds bets before 

#goal is predicting who will win and compare if the results matched up
#do this by finding the difference for a bunch of variables
#the result variable should be 1 for they win 0 for lose

#for all of the W# and L# if it says NA just set it equal to 0

#16-25
tennis$W1 = ifelse(is.na(tennis$W1), 0, tennis$W1)
tennis$L1 = ifelse(is.na(tennis$L1), 0, tennis$L1)
tennis$W2 = ifelse(is.na(tennis$W2), 0, tennis$W2)
tennis$L2 = ifelse(is.na(tennis$L2), 0, tennis$L2)
tennis$W3 = ifelse(is.na(tennis$W3), 0, tennis$W3)
tennis$L3 = ifelse(is.na(tennis$L3), 0, tennis$L3)
tennis$W4 = ifelse(is.na(tennis$W4), 0, tennis$W4)
tennis$L4 = ifelse(is.na(tennis$L4), 0, tennis$L4)
tennis$W5 = ifelse(is.na(tennis$W5), 0, tennis$W5)
tennis$L5 = ifelse(is.na(tennis$L5), 0, tennis$L5)
tennis$Wsets = ifelse(is.na(tennis$Wsets), 0, tennis$Wsets)
tennis$Lsets = ifelse(is.na(tennis$Lsets), 0, tennis$Lsets)
tennis$B365W = ifelse(is.na(tennis$B365W), 0, tennis$B365W)
tennis$B365L = ifelse(is.na(tennis$B365L), 0, tennis$B365L)
tennis$PSW = ifelse(is.na(tennis$PSW), 0, tennis$PSW) 
tennis$PSL = ifelse(is.na(tennis$PSL), 0, tennis$PSL)
tennis$MaxW = ifelse(is.na(tennis$MaxW), 0, tennis$MaxW)
tennis$MaxL = ifelse(is.na(tennis$MaxL), 0, tennis$MaxL)
tennis$AvgW = ifelse(is.na(tennis$AvgW), 0, tennis$AvgW) 
tennis$AvgL = ifelse(is.na(tennis$AvgL), 0, tennis$AvgL) #if any value is missing for any of these just fill it in with 0

tennis$WRank = ifelse(tennis$WRank == "N/A", 0, tennis$WRank)
tennis$LRank = ifelse(tennis$LRank == "N/A", 0, tennis$LRank)
tennis$WPts = ifelse(tennis$WPts == "N/A", 0, tennis$WPts)
tennis$LPts = ifelse(tennis$LPts == "N/A", 0, tennis$LPts) #if any value in these says "N/A" fill in a 0

tennis$Location = NULL
tennis$Tournament = NULL
tennis$Date = NULL
tennis$Series = NULL
tennis$Court = NULL
tennis$Surface = NULL
tennis$Best.of = NULL
tennis$Comment = NULL
tennis$Round = NULL
tennis$ATP = NULL

tennis$rankDiff = tennis$WRank - as.numeric(tennis$LRank) #WRank - LRank
tennis$ptsDiff = tennis$WPts - as.numeric(tennis$LPts) #winnerpts - loserspts
tennis$set1Diff = tennis$W1 - tennis$L1 #set1wins - set1loser
tennis$set2Diff = tennis$W2 - tennis$L2 #set2wins - set2loser
tennis$set3Diff = tennis$W3 - tennis$L3 #set3wins - set3loser 
tennis$set4Diff = tennis$W4 - tennis$L4 #set4wins - set4loser
tennis$set5Diff = tennis$W5 - tennis$L5 #set5wins - set5loser
tennis$numSetsDiff = as.numeric(tennis$Wsets) - as.numeric(tennis$Lsets) #number of winners sets - number of losers sets
tennis$B365Diff = as.numeric(tennis$B365W) - as.numeric(tennis$B365L)
tennis$PSDiff = as.numeric(tennis$PSW) - as.numeric(tennis$PSL)
tennis$MaxDiff = as.numeric(tennis$MaxW) - as.numeric(tennis$MaxL)
tennis$AvgDiff = as.numeric(tennis$AvgW) - as.numeric(tennis$AvgL)

posCounter = 0
negCounter = 0
for(x in 1:2222){
  if (as.numeric(tennis$rankDiff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$ptsDiff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$set1Diff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$set2Diff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$set3Diff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$set4Diff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$set5Diff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$numSetsDiff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$B365Diff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$PSDiff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$MaxDiff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$PSDiff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (as.numeric(tennis$AvgDiff[x]) < 0){
    negCounter = negCounter + 1
  }
  else{
    posCounter = posCounter + 1
  }
  
  if (posCounter > negCounter){
    tennis$results[x] = 1
  }
  else{
    tennis$results[x] = 0
  }
  posCounter = 0
  negCounter = 0
}

tennis$WRank = as.numeric(as.factor(tennis$WRank))
tennis$LRank = as.numeric(as.factor(tennis$LRank))
tennis$WPts = as.numeric(as.factor(tennis$WPts))
tennis$LPts = as.numeric(as.factor(tennis$LPts))
tennis$W1 = as.numeric(as.factor(tennis$W1))
tennis$L1 = as.numeric(as.factor(tennis$L1))
tennis$W2 = as.numeric(as.factor(tennis$W2))
tennis$L2 = as.numeric(as.factor(tennis$L2))
tennis$W3 = as.numeric(as.factor(tennis$W3))
tennis$L3 = as.numeric(as.factor(tennis$L3))
tennis$W4 = as.numeric(as.factor(tennis$W4))
tennis$L4 = as.numeric(as.factor(tennis$L4))
tennis$W5 = as.numeric(as.factor(tennis$W5))
tennis$L5 = as.numeric(as.factor(tennis$L5))
tennis$B365W = as.numeric(as.factor(tennis$B365W))
tennis$B365L = as.numeric(as.factor(tennis$B365L))
tennis$PSW = as.numeric(as.factor(tennis$PSW))
tennis$PSL = as.numeric(as.factor(tennis$PSL))
tennis$MaxW = as.numeric(as.factor(tennis$MaxW))
tennis$MaxL = as.numeric(as.factor(tennis$MaxL))
tennis$AvgW = as.numeric(as.factor(tennis$AvgW))
tennis$AvgL = as.numeric(as.factor(tennis$AvgL))
tennis$rankDiff = as.numeric(as.factor(tennis$rankDiff))
tennis$ptsDiff = as.numeric(as.factor(tennis$ptsDiff))
tennis$set1Diff = as.numeric(as.factor(tennis$set1Diff))
tennis$set2Diff = as.numeric(as.factor(tennis$set2Diff))
tennis$set3Diff = as.numeric(as.factor(tennis$set3Diff))
tennis$set4Diff = as.numeric(as.factor(tennis$set4Diff))
tennis$set5Diff = as.numeric(as.factor(tennis$set5Diff))
tennis$numSetsDiff = as.numeric(as.factor(tennis$numSetsDiff))
tennis$B365Diff = as.numeric(as.factor(tennis$B365Diff))
tennis$PSDiff = as.numeric(as.factor(tennis$PSDiff))
tennis$MaxDiff = as.numeric(as.factor(tennis$MaxDiff))
tennis$AvgDiff = as.numeric(as.factor(tennis$AvgDiff))
tennis$results = as.numeric(as.factor(tennis$results))

##################################the following will be the knn() section########################################
tennis$results = as.factor(tennis$results)

knnTennis = tennis[, c(3:39)]
#dim(knnTennis)

n = nrow(knnTennis)
set.seed(1)
dt = sample(1:n, n*.8)

XTrain = scale(knnTennis[dt, c(1:36)])
t = knnTennis[-dt, c(1:36)]
XTest = scale(t, center=attr(XTrain,"scaled:center"), scale=attr(XTrain,"scaled:scale"))

YTrain = knnTennis[dt, 37]
YTest = knnTennis[-dt, 37]

meansKnn = vector()
set.seed(1)
for(i in 1:20){
  set.seed(1)
  knnPred = knn(XTrain, XTest, YTrain, i)
  meansKnn[i] = mean(knnPred != YTest)
}

bestK = which.min(meansKnn) #choosing the best k: 10
bestK
knnPred = knn(XTrain, XTest, YTrain, bestK) 
#knnPred
mean(knnPred != YTest) #.0112359

#################################the following will be the svm() section#########################################
tennis$results = as.factor(tennis$results)

svmTennis = tennis[, c(3:39)] #this will cut off the first two columns so that we have only numerical values

dt = createDataPartition(y = svmTennis$results, p = .8, list = FALSE)
train = svmTennis[dt, ]
test = svmTennis[-dt, ]

#dim(train)
#dim(test)

train[["results"]] = factor(train[["results"]])

######################the following svm sequence will be for a linear svm
set.seed(1)
Ltuner = tune(svm, results~., data = svmTennis, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 100))) 
summary(Ltuner)
bestCost = Ltuner$best.parameters$cost #bests cost was 1

svmfitL = svm(results~., data = train, kernel = "linear", cost = bestCost, scale = TRUE)

svmTrainL.pred = predict(svmfitL, train)
table(train$results, svmTrainL.pred) #2 is yes they won, 1 is no they lost; training error: 9/1769 or .00508

svmTestL.pred = predict(svmfitL, test)
table(test$results, svmTestL.pred) #2 is yes they won, 1 is no they lost; test error: 1/73 or 0.01369

#############################the following svm sequence will be for the radial svm
set.seed(1)
Rtuner = tune(svm, results~., data = svmTennis, kernel = "radial", ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 100), gamma = c(.001, .01, .05, .1, 1, 5, 10)))
summary(Rtuner)
bestCost = Rtuner$best.parameters$cost #best cost was 100
bestGam = Rtuner$best.parameters$gamma #best gamma was .001

svmfitR = svm(results~., data = train, kernel = "radial", cost = bestCost, gamma = bestGam, scale = TRUE)

svmTrainR.pred = predict(svmfitR, train)
table(train$results, svmTrainR.pred) #2 is yes they won, 1 is no they lost; training error: 9/1769 or .00508

svmTestR.pred = predict(svmfitR, test)
table(test$results, svmTestR.pred) #2 is yes they won, 1 is no they lost; test error: 5/439 or .0113895

###########################the following svm sequence is for polynomial svm
set.seed(1)
Ptuner = tune(svm, results~., data = svmTennis, kernel = "polynomial", ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 100), degree = c(1:10)))
summary(Ptuner)
bestCost = Ptuner$best.parameters$cost #best cost was 10
bestDegree = Ptuner$best.parameters$degree #best degree was 1

svmfitP = svm(results~., data = train, kernel = "polynomial", cost = bestCost, degree = bestDegree, scale = TRUE)

svmTrainP.pred = predict(svmfitP, train)
table(train$results, svmTrainP.pred) #2 is yes they won, 1 is no they lost; training error: 5/884 or .00565

svmTestP.pred = predict(svmfitP, test) 
table(test$results, svmTestP.pred)#2 is yes they won, 1 is no they lost; test error: 5/439 or .0113895
