#' Name: James Quacinella

#' Plot the Monthly total of people on unemployed benefits in Australia (January 1956–July 1992).  
#' 
#' NOTES: The following code produces two graphs of the pre and post transformation of the monthly 
#' total of people on unemployed benefits in Australia. The data looks skewed to me, and by 
#' recommendation of the first textbook, I would apply this transformation. Note that the 
#' y-scale is a lot smaller with the transform, which is true across all problems in
#' this question.
question_2.1a <- function() {
  library("fma")
  lambda.dole <- BoxCox.lambda(dole)  # 0.3290922
  plot(dole, main="Monthly total of people on unemployed benefits in Australia", sub="Pre-Transformation")
  plot(BoxCox(dole, lambda.dole), sub="Post-Transformation")
}

#' Monthly total of accidental deaths in the United States (January 1973–December 1978).
#' 
#' NOTES: The lambda value is small, and according to the text, since the seasonality already looks
#' to be the same across the data, this transformation is probably not needed ("A good value oflambda 
#' is one which makes the size of the seasonal variation about the same across the whole series").
#' I have plotted both either way.
question_2.1b <- function() {
  lambda.usdeaths <- BoxCox.lambda(usdeaths)  # -0.03363775
  plot(usdeaths, main="Monthly total of accidental deaths in the United States", sub="Pre-Transformation")
  plot(BoxCox(usdeaths, lambda.usdeaths), main="Monthly total of accidental deaths in the United States", sub="Post-Transformation")
}

#' Quarterly production of bricks (in millions of units) at Portland, Australia (March 1956–September 1994). 
#' 
#' NOTES: The following code produces two graphs of the pre and post transformation of the Quarterly production 
#' of bricks. The shape of rhe data does not change signicificantly, but like part b), the y-scale is smaller.
question_2.1c <- function() {
  lambda.bricksq <- BoxCox.lambda(bricksq)  # 0.2548929
  plot(bricksq, main="Quarterly production of bricks", sub="Pre-transformation")
  plot(BoxCox(bricksq, lambda.bricksq), sub="Post-transformation")
}




#' Consider the daily closing IBM stock prices (data set ibmclose).
#' Produce some plots of the data in order to become familiar with it.
question_2.3a <- function() {
  plot(ibmclose, main="Closing Price of IBM Stock")
  lines(rollmean(ibmclose, k=30, fill=NA), col="red")
  lines(rollmean(ibmclose, k=60, fill=NA), col="blue")
  legend("topright", lty=1, col=c("red", "blue"), legend=c("30 day rolling average","60 day rolling average"))
}

#' Split the data into a training set of 300 observations and a test set of 69 observations.
question_2.3b <- function() {
  ibmclose.training <- window(ibmclose, start=1, end=300)
  ibmclose.test <- window(ibmclose, start=301)
}

#' Try various benchmark methods to forecast the training set and compare the results on the test set. 
#' Which method did best? 
#' 
#' NOTES: Note that the blue prediction is for the mean predicition, and looks different due to using 
#' plot() versus lines(). Also note that both naive methods overlap their predicitions. 
#' Visually inspecting the predictions, drift seems to ve the most accurate; the mean is so far away 
#' as it does not account well for the bug dip in value around t=250.
#' 
#' The various error rates are printed as well for each prediction scheme.  
#' Results for mean, naive, seasonal naive and drift, respectively:
#'
#'                         ME      RMSE       MAE        MPE     MAPE     MASE      ACF1 Theil's U
#' Test set     -1.306180e+02 132.12557 130.61797 -35.478819 35.47882 25.62649 0.9314689  19.05515
#' Test set     -3.7246377    20.248099 17.02899 -1.29391743 4.668186 3.340989 0.9314689  2.973486
#' Test set     -3.7246377    20.248099 17.02899 -1.29391743 4.668186 3.340989 0.9314689  2.973486
#' Test set     6.108138e+00  17.066963 13.974747  1.41920066 3.707888 2.741765 0.9045875  2.361092
#' 
#' Its clear that the mean error is too high compared to the rest. It seems that drift overall tends
#' to have lower error values that the naive methods, but are generally pretty close. As said above,
#' I think my tendency would be for the drift method.
question_2.3c <- function() {
  ibmclose.prediction_horizon <- length(ibmclose) - 300;
  
  # Calc mean, naive, seasonal naive, drift predictions
  ibmclose.fit.mean <- meanf(ibmclose.training, h=ibmclose.prediction_horizon)
  ibmclose.fit.naive <- naive(ibmclose.training, h=ibmclose.prediction_horizon)
  ibmclose.fit.seasonalNaive <- snaive(ibmclose.training, h=ibmclose.prediction_horizon)
  ibmclose.fit.drift <- rwf(ibmclose.training, drift=TRUE, h=ibmclose.prediction_horizon)
  
  # Plot data with predictions
  plot(ibmclose.training, main="Closing Price of IBM Stock")
  plot(ibmclose.fit.mean, plot.conf=FALSE, main="Closing Price of IBM Stock")
  lines(ibmclose.fit.naive$mean, col=2)
  lines(ibmclose.fit.seasonalNaive$mean, col=3)
  lines(ibmclose.fit.drift$mean, col=1)
  lines(ibmclose)
  legend("topright", lty=1, col=c(4,2,3,1), legend=c("Mean method","Naive method","Seasonal naive method", "Drift"))
  
  # Look at error values from our predictions
  accuracy(ibmclose.fit.mean, ibmclose.test)
  accuracy(ibmclose.fit.naive, ibmclose.test)
  accuracy(ibmclose.fit.seasonalNaive, ibmclose.test)
  accuracy(ibmclose.fit.drift, ibmclose.test)
}



#' Consider the sales of new one-family houses in the USA, Jan 1973 – Nov 1995 (data set hsales).
#' Produce some plots of the data in order to become familiar with it.
#' 
#' Comments: Here a simple ts plot, with a few moving averages is plotted, as well as a month and season
#' plot, since the data is the right format (versus previous question)
question_2.4a <- function() {
  # Plot time series with some moving averages
  plot(hsales, main="Sales of new one-family houses in the USA")
  lines(rollmean(hsales, k=30, fill=NA), col="red")
  lines(rollmean(hsales, k=60, fill=NA), col="blue")
  
  # Show seasonal plot, which should a general downward trend from March onwards
  seasonplot(hsales, col=1:20, pch=19, year.labels=TRUE, year.labels.left=TRUE)
  
  #' Show sales by month, which emphasizes that there is a ramp up in sales
  #' from Jan to Mar, but declines for the rest of the year
  monthplot(hsales,ylab="$ million",xlab="Month",xaxt="n", main="Sales of new one-family houses in the USA")
  axis(1,at=1:12,labels=month.abb,cex=0.8)
}

#' Split the hsales data set into a training set and a test set, where the test set is the last two years of data.
question_2.4b <- function() {
  hsales.training <- window(hsales, start=c(1973, 1), end=c(1993, 12)) 
  hsales.test <- window(hsales, start=c(1994, 1), end=c(1995, 11)) 
}

#' Try various benchmark methods to forecast the training set and compare the results on the test set. 
#' Which method did best? 
#' 
#' Comments: Visual inspection shows that the seasonal naive method looks pretty accurate
#' across the 23months of prediction, though the predicted value in 23months looks
#' pretty off. None of the methods seem to predict the end drop-off in value towards
#' the end of 1995. Here are the error values for the mean, naive, seasonal naive and
#' drift methods, respectively:
#' 
#'                      ME      RMSE      MAE       MPE     MAPE     MASE      ACF1 Theil's U
#' Test set     4.051587e+00  9.216133 7.850759  5.074990 13.75973 0.924979 0.5095178   1.13105
#' Test set      5.00000000 9.670664 8.304348   6.8080182 14.381673 0.9784210 0.5095178 1.179633
#' Test set     0.3043478   6.160886 5.0000     -0.7312374  9.12828 0.5891016 0.224307  0.8031005
#' Test set     5.191235e+00 9.761548 8.393037  7.1599507 14.50303 0.9888703 0.5083059  1.188562
#' 
#' The seasonal naive is aclear winner here. Visually, the seaonal naive method overlaps the 
#' test data very well, and generally has the least amount of error for any error metric.
question_2.4c <- function() {
  hsales.prediction_horizon <- 23;
  
  # Calculate the mean, naive, seasonal naive, drift predicitons
  hsales.fit.mean <- meanf(hsales.training, h=hsales.prediction_horizon)
  hsales.fit.naive <- naive(hsales.training, h=hsales.prediction_horizon)
  hsales.fit.seasonalNaive <- snaive(hsales.training, h=hsales.prediction_horizon)
  hsales.fit.drift <- rwf(hsales.training, drift=TRUE, h=hsales.prediction_horizon)
  
  # Plot the data with our predicitons
  plot(hsales.training, main="Sales of new one-family houses in the USA w/ Forecasts")
  plot(hsales.fit.mean, plot.conf=FALSE, main="Sales of new one-family houses in the USA")
  lines(hsales.fit.naive$mean, col=2)
  lines(hsales.fit.seasonalNaive$mean, col=3)
  lines(hsales.fit.drift$mean, col=1)
  lines(hsales)
  legend("topright", lty=1, col=c(4,2,3,1), legend=c("Mean method","Naive method","Seasonal naive method", "Drift"))

  # Look at error values from our predictions
  accuracy(hsales.fit.mean, hsales.test)
  accuracy(hsales.fit.naive, hsales.test)
  accuracy(hsales.fit.seasonalNaive, hsales.test)
  accuracy(hsales.fit.drift, hsales.test)
}



#' (a) Investigate the frequency distributions for the categorical predictors. Are
#' any of the distributions degenerate in the ways discussed earlier in this
#' chapter?
#' 
#' With regards to being degenerate, I will refer to Section 3.5 about Removing Predictors,
#' since I assume that degeneracy means 'detecting that this predictor should probably be 
#' removed'. The rule of thumb is predictors are 'degenerate' if the frequency of the first 
#' and second most common values for that predictor is > 20, while the fraction of unique
#' values as compared to the number of samples is low.
#' 
#' First, the fraction of unique values over the sample size is low for all columns, 
#' so we just need to check the ratio of first and second most frequent value per 
#' predictor. However, there is some ambiguity here as to whether or not we should
#' be including NAs as a value. I have done it both ways, since the results are a little
#' different.
#' 
#' With NAs counted, the columns that are close to the 20 threshold are
#' mycelium, int.discolor, and sclerotia. However, with NAs not counted,
#' the columns are mycelium, scelortia amd leaf.mild.
#' 
#' With NAs counted, the columns that are close to the 20 threshold are mycelium, 
#' int.discolor, and sclerotia. However, with NAs not counted, the columns are 
#' mycelium, scelortia amd leaf.mild. My intuition says that we should not count 
#' the NAs (since the histogram here is very clear that some columns are over the threshold)
#' Therefore, mycelium, scelortia amd leaf.mild should be considered degenerate.
question_3.2a <- function() {
  library(mlbench)
  data(Soybean)
  library(plyr)
  
  nrows <- nrow(Soybean);
  
  #' Lets check our rule of thumb by calculating ratio of 1st and 2nd highest freq
  i <- 1;
  ratiosWithoutNAs <- c()
  ratiosWithNAs <- c()
  for (column in colnames(Soybean)[2:length(colnames(Soybean))]) {
    # Do the calculation using count(), which includes NAs
    t <- count(Soybean, column)
    sorted_t <- t[order(-t['freq']), 'freq']
    ratiosWithNAs[i] <- (sorted_t[1] / sorted_t[2]);
    
    # Do the claculation using table() which does not include NAs
    t <- table(Soybean[ , column])
    sorted_t <- sort(as.numeric(t), decreasing=TRUE)
    ratiosWithoutNAs[i] <- (sorted_t[1] / sorted_t[2]);
    
    # Increment the idx for ratio lists
    i <- i + 1;
  }
  
  # Display the histogram when accounting for NAs
  library(ggplot2)
  ratiosWithNAs.dist <- data.frame(y=ratiosWithNAs, x=colnames(Soybean)[2:length(colnames(Soybean))])
  ratiosWithNAs.dist$x <-reorder(ratiosWithNAs.dist$x, -ratiosWithNAs.dist$y)
  ggplot(ratiosWithNAs.dist) + 
    geom_bar(aes(x=x, y=y), stat="identity") + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    ggtitle("Distribution of the Ratios of Most Frequently Occuring to \n 2nd Most Frequently Occuring (including NAs)") + 
    ylab("Ratio") + xlab("Column")
  
  # Display the histogram when not accounting for NAs
  ratiosWithoutNAs.dist <- data.frame(y=ratiosWithoutNAs, x=colnames(Soybean)[2:length(colnames(Soybean))])
  ratiosWithoutNAs.dist$x <-reorder(ratiosWithoutNAs.dist$x, -ratiosWithoutNAs.dist$y)
  ggplot(ratiosWithoutNAs.dist) + 
    geom_bar(aes(x=x, y=y), stat="identity") + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    ggtitle("Distribution of the Ratios of Most Frequently Occuring to \n 2nd Most Frequently Occuring (not including NAs)") + 
    ylab("Ratio") + xlab("Column")
}


#' (b) Roughly 18 % of the data are missing. Are there particular predictors that
#' are more likely to be missing? Is the pattern of missing data related to
#' the classes?
#' 
#' NOTES: Looking at the histogram, it seems that quite a few predictors
#' have signifcant number of NAs. There is a dropoff in NA level from around 12%
#' to 5% (highest being almost 20% missing data).
#' 
#' Also, the columns herbicide-injury, cyst-nematode, 2-4-d-injury, diaporthe-pod-&-stem-blight
#' have no instances / rows that do not have a NA in them. The class phytophthora-rot has 68 NAs, 
#' but all other classes have complete cases
question_3.2b <- function() {
  i <- 1;
  nas <- c()
  for (column in colnames(Soybean)[2:length(colnames(Soybean))]) {
    t <- count(Soybean, column)
    nas[i] <- ifelse(length(t[is.na(t[column]), 'freq']), t[is.na(t[column]), 'freq'] * 100 / nrows, 0)
    i <- i + 1;
  }

  # Display histogram of distrubtion of NAs
  nas.dist <- data.frame(y=nas, x=colnames(Soybean)[2:length(colnames(Soybean))])
  nas.dist$x <-reorder(nas.dist$x,-nas.dist$y)
  ggplot(nas.dist) + 
    geom_bar(aes(x=x, y=y), stat="identity") + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    ggtitle("Distribution of Missing Data") + 
    ylab("% Rows w/ NAs") + xlab("Column")
  

  # The columns herbicide-injury, cyst-nematode, 2-4-d-injury, diaporthe-pod-&-stem-blight
  # have no instances / rows that do not have a NA in them
  # The Class phytophthora-rot has 68  NAs, but all other classes have none
  withnas <- count(Soybean$Class)
  withoutnas <- count(Soybean[complete.cases(Soybean), 'Class'])
  for (i in 1:nrow(withoutnas)) { 
    classval <- withoutnas[i, 'x']
    diff <- withnas[withnas$x == classval, 'freq'] - withoutnas[withoutnas$x == classval, 'freq']
    if(diff > 0) {
      print(paste("Class", classval, "has", diff, " NAs"))
    }
  }
}


#' (c) Develop a strategy for handling missing data, either by eliminating
#' predictors or imputation.
#' 
#' NOTES: We should start by investigating the nature of the missing data for predictors with a large
#' number of missing values as well as why some classes have a high occurance of NAs. Our predictive
#' model is going to be biased if we do not have good observations for 4 or 5 of the 19 outcome
#' classes.
#' 
#' For each predictor that has a high number of NAs (starting from the left in the question_3.2b histogram)
#' We can calculate if we can remove the predictor by checking if it has a high collinearity with any other predictor. If so,
#' we can remove that preidctor and simply use the collinear predictor (presuming it has less instances of NAs)
#' 
#' Imputing values would be difficult, since these are all categorical variables, and averaging
#' those values would make no sense. We could impute a value using K-nearest neighbors, which
#' would find the K rows that are closest to it by some definition of 'close', and then take
#' a majority vote on what the categorical value should be.




#' 4.4. snip 
#' (a) Use the sample function in base R to create a completely random sample
#' of 60 oils. How closely do the frequencies of the random sample match
#' the original samples? Repeat this procedure several times of understand
#' the variation in the sampling process.
question_4.4a <- function() {
  set.seed(1)
  library(caret)
  library(descr)
  data(oil)
  
  print("Frequency from Data")
  print(freq(oilType, plot = FALSE))
  
  cat("\n")
  print("Frequency from Sampling 60 items")
  for (i in 1:10) {
    print(freq(sample(oilType, 60), plot = FALSE))
  }
}

#' (b) Use the caret package function createDataPartition to create a stratified
#' random sample. How does this compare to the completely random samples?
#' the variation in the sampling process.
#' 
#' NOTES: the createDataPartition is indeed giving back different numbers every time
#' but table() output shows how it is very consistent in how it distributes samples
#' across class values, and is very close to the ideal from the data itself. Much
#' better than the previous approach
question_4.4b <- function() {
  print("Frequency from Data")
  print(freq(oilType, plot = FALSE))
  
  cat("\n")
  print("Frequency from Sampling 60 items")
  for (i in 1:10) {
    print(freq(oilType[createDataPartition(oilType, p=60 / length(oilType), list=FALSE)], plot = FALSE))
  }
}



#' (c) With such a small samples size, what are the options for determining
#' performance of the model? Should a test set be used?
#' 
#' NOTES: When the number of samples is not large, a single test set should be avoided 
#' because we may need every sample during model building. We should use some 
#' kind of resampling technique, like k-fold cross-validation. The book recommends
#' "If the samples size is small, we recommend repeated 10-fold cross-validation for several 
#' reasons: the bias and variance properties are good and, given the sample size, the 
#' computational costs are not large.


#' (d) Snip...
#' In this case, the width of the 95 % confidence interval is 37.9 %. Try
#' different samples sizes and accuracy rates to understand the trade-off
#' between the uncertainty in the results, the model performance, and the
#' test set size.
#' 
#' NOTES: By keeping the model performance steady, while changing the test 
#' sample size leads to a relationship where higher sample size means less 
#' uncertainity. This makes sense: we would expect us to be more certain 
#' if we have more samples to evaluate the model performance with.
#' 
#' If you keep the sample size steady, and vary the model performance leads 
#' to a relationship where:
#'   - the uncertainity maxes when performance is 50% (which makes sense 
#'      since its binomial probability).
#'   - the uncertainity is less when model performace is near either extreme 
#'      (partially due to clipping of the CI by the boundry)
function question_4.4d <- function() {
  spread <- function(obj) { conf <- obj$conf.int; return(conf[2] - conf[1]);}
  spread(binom.test(16, 20)) # 0.37928
  spread(binom.test(32, 40)) # 0.2659556
  spread(binom.test(30, 40)) # 0.2850472
  spread(binom.test(10, 40)) # 0.2850472
  spread(binom.test(15, 40)) # 0.3147225
  spread(binom.test(20, 40)) # 0.3239644
  spread(binom.test(25, 40)) # 0.3147225
  spread(binom.test(40, 40)) # 0.0880973
  spread(binom.test(20, 40)) # 0.3239644
  spread(binom.test(21, 40)) # 0.3236002
  spread(binom.test(19, 40)) # 0.3236002
  spread(binom.test(19, 20)) # 0.2474677
  spread(binom.test(20, 20)) # 0.1684335
  spread(binom.test(10, 20)) # 0.4560843
  spread(binom.test(1, 20))  # 0.2474677
}