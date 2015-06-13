#' Name: James Quacinella

#' Forecasting results are relatively insensitive to the value of λ.
#' Often no transformation is needed.
#' Transformations sometimes make little difference to the forecasts but have a large effect on prediction intervals.
 
#' Plot the Monthly total of people on unemployed benefits in Australia (January 1956–July 1992).  
question_2.1a <- function() {
  library("fma")
  lambda.dole <- BoxCox.lambda(dole)  # -0.03363775
  plot(dole, main="Monthly total of people on unemployed benefits in Australia", sub="Pre-Transformation")
  plot(BoxCox(dole, lambda.dole), main="Monthly total of people on unemployed benefits in Australia", sub="Post-Transformation")
}

#' Monthly total of accidental deaths in the United States (January 1973–December 1978).
#' The lambda value is small, but since the plot shows seasonality, I think this transformation
#' ise useful. Also, the scale post-transformaion is a lot smaller, which is sometimes helpful
#' in machine learning / predicitive tasks
question_2.1b <- function() {
  lambda.usdeaths <- BoxCox.lambda(usdeaths)  # -0.03363775
  plot(usdeaths, main="Monthly total of accidental deaths in the United States", sub="Pre-Transformation")
  plot(BoxCox(usdeaths, lambda.usdeaths), main="Monthly total of accidental deaths in the United States", sub="Post-Transformation")
}

#' Quarterly production of bricks (in millions of units) at Portland, Australia (March 1956–September 1994). 
#' 
question_2.1c <- function() {
  lambda.bricksq <- BoxCox.lambda(bricksq)  # 0.2548929
  plot(bricksq, main="Quarterly production of bricks", sub="Pre-transformation")
  plot(BoxCox(bricksq, lambda.bricksq), main="Quarterly production of bricks", sub="Post-transformation")
}




#' Consider the daily closing IBM stock prices (data set ibmclose).
#' Produce some plots of the data in order to become familiar with it.
question_2.3a <- function() {
  plot(ibmclose, main="Closing Price of IBM Stock")
  lines(rollmean(ibmclose, k=30, fill=NA), col="red")
  lines(rollmean(ibmclose, k=60, fill=NA), col="blue")
}

#' Split the data into a training set of 300 observations and a test set of 69 observations.
question_2.3b <- function() {
  ibmclose.training <- window(ibmclose, start=1, end=300)
  ibmclose.test <- window(ibmclose, start=301)
}

#' Try various benchmark methods to forecast the training set and compare the results on the test set. 
#' Which method did best? 
#' 
#' Comments: Note that the blue prediction is for the mean predicition, and looks different due to using 
#' plot() versus lines(). Also note that both naive methods overlap their predicitions. 
#' Visually inspecting the predictions, drift is clearly the most accurate; the mean is so far away as it does
#' not account well for the bug dip in value around t=250.
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
  accuracy(ibmclose.fit.mean, ibmclose)
  accuracy(ibmclose.fit.naive, ibmclose)
  accuracy(ibmclose.fit.seasonalNaive, ibmclose)
  accuracy(ibmclose.fit.drift, ibmclose)
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
#' the end of 1995
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
  accuracy(hsales.fit.mean, hsales)
  accuracy(hsales.fit.naive, hsales)
  accuracy(hsales.fit.seasonalNaive, hsales)
  accuracy(hsales.fit.drift, hsales)
}





#' The soybean data can also be found at the UC Irvine Machine Learning
#' Repository. Data were collected to predict disease in 683 soybeans. The 35
#' predictors are mostly categorical and include information on the environmental 
#' conditions (e.g., temperature, precipitation) and plant conditions 
#' (e.g., left spots, mold growth). The outcome labels consist of 19 distinct classes.
#' See ?Soybean for details



#' (a) Investigate the frequency distributions for the categorical predictors. Are
#' any of the distributions degenerate in the ways discussed earlier in this
#' chapter?
#' 
#' It seems that only three columns come close (but don't quite hit the threshold)
#' to matching the rules of thumb for degenerate categorical data. Those columns are
#' mycelium, int.discolor, and sclerotia (see output)
question_3.2a <- function() {
  library(mlbench)
  data(Soybean)
  library(plyr)
  
  nrows <- nrow(Soybean);
  
  #' Lets check our rule of thumb
  #' NOTE: The fraction of unique values over the sample size is low for all columns
  #' NOTE: only three columns have a ratio of the frequency of the most prevalent value to 
  #' the frequency of the second most prevalent value close to 20. These columns are:
  #' mycelium, scleroria and int.discolor
  i <- 1;
  ratios <- c()
  for (column in colnames(Soybean)[2:length(colnames(Soybean))]) {
    t <- count(Soybean, column)
    sorted_t <- t[order(-t['freq']), 'freq']
    
    ratios[i] <- (sorted_t[1] / sorted_t[2]);
    i <- i + 1;
  }
  
  library(gpglot2)
  ratios.dist <- data.frame(y=ratios, x=colnames(Soybean)[2:length(colnames(Soybean))])
  ratios.dist$x <-reorder(ratios.dist$x,-ratios.dist$y)
  ggplot(ratios.dist) + 
    geom_bar(aes(x=x, y=y), stat="identity") + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    ggtitle("Distribution of the Ratios of Most Frequently Occuring to 2nd Most Frequently Occuring") + 
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
#' We should start by investigating the nature of the missing data for predictors with a large
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
