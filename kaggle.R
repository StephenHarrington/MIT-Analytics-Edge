{
 "cells": [
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## Test your analytics skills by predicting which New York Times blog articles will be the most popular.\n",
    "\n",
    "Newspapers and online news aggregators like Google News need to understand which news articles will be the most popular, so that they can prioritize the order in which stories appear. In this competition, you will predict the popularity of a set of New York Times blog articles from the time period September 2014-December 2014.\n",
    "\n",
    "The following screenshot shows an example of the New York Times technology blog \"Bits\" homepage:\n",
    "\n",
    "![](https://kaggle2.blob.core.windows.net/competitions/kaggle/4347/media/NYBlogScreenshot_031715.png)\n",
    "\n",
    "Many blog articles are published each day, and the New York Times has to decide which articles should be featured. In this competition, we challenge you to develop an analytics model that will help the New York Times understand the features of a blog post that make it popular."
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "#Read in training and testing sets\n",
    "\n",
    "Originally these data files could be found at this [link](https://www.kaggle.com/c/15-071x-the-analytics-edge-competition-spring-2015/download/NYTimesBlogTest.csv), but the Jupyter R kernal does not support direct read.csv from a url.\n",
    "\n",
    "The codebook for the two datatsets are nearly identical with only the \"Popular\" feature missing from the testing set.\n",
    "\n",
    "##File Descriptions\n",
    "\n",
    "The data provided for this competition is split into two files:\n",
    "\n",
    "NYTimesBlogTrain.csv = the training data set. It consists of 6532 articles.\n",
    "NYTimesBlogTest.csv = the testing data set. It consists of 1870 articles.  \n",
    "\n",
    "We have also provided a sample submission file, SampleSubmission.csv. This file gives an example of the format of submission files (see the Evaluation page for more information). The data for this competition comes from the New York Times website.\n",
    "\n",
    "###Variable Descriptions\n",
    "\n",
    "The dependent variable in this problem is the variable Popular, which labels if an article had 25 or more comments in its online comment section (equal to 1 if it did, and 0 if it did not). The dependent variable is provided in the training data set, but not the testing dataset. This is an important difference from what you are used to - you will not be able to see how well your model does on the test set until you make a submission on Kaggle.\n",
    "\n",
    "The independent variables consist of 8 pieces of article data available at the time of publication, and a unique identifier:\n",
    "\n",
    "NewsDesk = the New York Times desk that produced the story (Business, Culture, Foreign, etc.)\n",
    "SectionName = the section the article appeared in (Opinion, Arts, Technology, etc.)\n",
    "SubsectionName = the subsection the article appeared in (Education, Small Business, Room for Debate, etc.)\n",
    "Headline = the title of the article\n",
    "Snippet = a small portion of the article text\n",
    "Abstract = a summary of the blog article, written by the New York Times\n",
    "WordCount = the number of words in the article\n",
    "PubDate = the publication date, in the format \"Year-Month-Day Hour:Minute:Second\"\n",
    "UniqueID = a unique identifier for each article"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "nytTrain = read.csv( \"NYTimesBlogTrain.csv\", stringsAsFactors = FALSE )\n",
    "nytTest  = read.csv( \"NYTimesBlogTest.csv\",  stringsAsFactors = FALSE )\n",
    "nytTest$Popular = rep( 2 , dim( nytTest )[ 1 ] )  ##make the Train and Test sets conformable by adding column to test set with value of '2' as unknonw"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": []
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "str(nytTrain)\n",
    "summary(nytTrain)\n",
    "\n",
    "nytTrain$Headline = gsub(\"New York\",\"\",nytTrain$Headline)\n",
    "nytTrain$Snippet = gsub(\"New York\",\"\",nytTrain$Headline)\n",
    "nytTrain$Abstract = gsub(\"New York\",\"\",nytTrain$Headline)\n",
    "\n",
    "nytTrain$PubDate=strptime(nytTrain$PubDate,'%Y-%m-%d %H:%M:%S')\n",
    "nytTrain$hour = format(nytTrain$PubDate,'%H')\n",
    "nytTrain$dow = format(nytTrain$PubDate,'%A')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "##emails$spam = as.factor(emails$spam)\n",
    "corpus  = Corpus(VectorSource(c(nytTrain$Headline,nytTrain$Snippet,nytTrain$Abstract,nytUNK$Headline,nytUNK$Snippet,nytUNK$Abstract)))\n",
    "corpus = tm_map(corpus, PlainTextDocument)\n",
    "corpus = tm_map(corpus, tolower)\n",
    "corpus = tm_map(corpus, PlainTextDocument)\n",
    "corpus = tm_map(corpus, removePunctuation)\n",
    "corpus = tm_map(corpus, removeWords, stopwords(\"english\"))\n",
    "corpus = tm_map(corpus, stemDocument)\n",
    "dtm = DocumentTermMatrix(corpus)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "spdtm = removeSparseTerms(dtm, 0.99)\n",
    "spdtm\n",
    "nytSparse     = as.data.frame(as.matrix(spdtm))\n",
    "colnames(nytSparse) = make.names(colnames(nytSparse))\n",
    "\n",
    "rev(sort(colSums(nytSparse)))[1]\n",
    "nytSparse$Popular = c(nytTrain$Popular,nytUNK$Popular)\n",
    "\n",
    "nytSparse$Popular = as.factor(nytSparse$Popular)\n",
    "nytSparse$WordCount    = nytTrain$WordCount\n",
    "nytSparse$hour    = as.factor(nytTrain$hour)\n",
    "nytSparse$dow     = as.factor(nytTrain$dow)\n",
    "\n",
    "nytTESTset = subset(nytSparse,Popular=='2')\n",
    "nytSparse = subset(nytSparse,Popular %in% c('0','1'))"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "set.seed(123)\n",
    "split = sample.split(nytSparse$Popular, SplitRatio = 0.7)\n",
    "\n",
    "train = subset(nytSparse, split==TRUE)\n",
    "train$Popular = droplevels(train$Popular)\n",
    "test  = subset(nytSparse, split==FALSE)\n",
    "test$Popular = droplevels(test$Popular)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "nytLog = glm(Popular ~ .,data=train,family=binomial)\n",
    "\n",
    "\n",
    "predPopularLog = predict(nytLog, type=\"response\")\n",
    "x=table(train$Popular, predPopularLog >= 0.5)\n",
    "sum(diag(x))/nrow(train)\n",
    "\n",
    "\n",
    "ROCRpred = prediction(predPopularLog, train$Popular)\n",
    "as.numeric(performance(ROCRpred, \"auc\")@y.values)\n",
    "\n",
    "\n",
    "\n",
    "##test set\n",
    "predPopularLog = predict(nytLog, newdata=test, type=\"response\")\n",
    "x<-table(test$Popular, predPopularLog >= 0.5)\n",
    "sum(diag(x))/nrow(test)\n",
    "ROCRpred = prediction(predPopularLog, test$Popular)\n",
    "as.numeric(performance(ROCRpred, \"auc\")@y.values)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "nytCART = rpart(Popular ~ ., data=train, method=\"class\")\n",
    "prp(nytCART)\n",
    "\n",
    "\n",
    "predictCART = predict(nytCART, newdata=train, type=\"class\")\n",
    "table(train$Popular, predictCART)\n",
    "# Compute accuracy\n",
    "sum(diag(table(train$Popular, predictCART)))/nrow(train)\n",
    "\n",
    "predictCART = predict(nytCART, newdata=train)[,2]\n",
    "ROCRpred = prediction(predictCART, train$Popular)\n",
    "as.numeric(performance(ROCRpred, \"auc\")@y.values)\n",
    "\n",
    "\n",
    "##test set\n",
    "predictCART = predict(nytCART, newdata=test, type=\"class\")\n",
    "table(test$Popular, predictCART)\n",
    "# Compute accuracy\n",
    "sum(diag(table(test$Popular, predictCART)))/nrow(test)\n",
    "\n",
    "predictCART = predict(nytCART, newdata=test)[,2]\n",
    "ROCRpred = prediction(predictCART, test$Popular)\n",
    "as.numeric(performance(ROCRpred, \"auc\")@y.values)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "library(randomForest)\n",
    "set.seed(123)\n",
    "nytRF = randomForest(Popular ~ ., data=train )\n",
    "\n",
    "predictRF = predict(nytRF, newdata=train, type=\"class\")\n",
    "table(train$Popular, predictRF)\n",
    "# Compute accuracy\n",
    "sum(diag(table(train$Popular, predictRF)))/nrow(train)\n",
    "\n",
    "predictRF = predict(nytRF, newdata=train, type='prob')[,2]\n",
    "ROCRpred = prediction(predictRF, train$Popular)\n",
    "as.numeric(performance(ROCRpred, \"auc\")@y.values)\n",
    "\n",
    "\n",
    "##test\n",
    "predictRF = predict(nytRF, newdata=test, type=\"class\")\n",
    "table(test$Popular, predictRF)\n",
    "# Compute accuracy\n",
    "sum(diag(table(test$Popular, predictRF)))/nrow(test)\n",
    "\n",
    "predictRF = predict(nytRF, newdata=test, type='prob')[,2]\n",
    "ROCRpred = prediction(predictRF, test$Popular)\n",
    "as.numeric(performance(ROCRpred, \"auc\")@y.values)"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "##Evaluation\n",
    "\n",
    "The evaluation metric for this competition is AUC. The AUC, which we described in Unit 3 when we taught logistic regression, is a commonly used evaluation metric for binary classification problems like this one. The interpretation is that given a random positive observation and negative observation, the AUC gives the proportion of the time you guess which is which correctly. It is less affected by sample balance than accuracy. A perfect model will score an AUC of 1, while random guessing will score an AUC of around 0.5.\n",
    "\n",
    "##Submission File\n",
    "\n",
    "For every observation in the test set, submission files should contain two columns: UniqueID and Probability1. The submission should be a csv file. The UniqueID should just be the corresponding UniqueID column from the test dataset. The Probability1 column should be the predicted probability of the outcome 1 according to your model, for that UniqueID. We have provided an example of a submission file, SampleSubmission.csv, which can be found in the Data section on Kaggle.\n",
    "\n",
    "As an example of how to generate a submission file in R, suppose that your test set probability predictions are called \"testPred\" and your test data set is called \"test\". Then you can generate a submission file called \"submission.csv\" by running the following two lines of code in R (if you copy and paste these lines of code into R, the quotes around submission.csv might not read properly - please delete and re-type the quotes if you get an error):\n",
    "\n",
    "submission = data.frame(UniqueID = test$UniqueID, Probability1 = testPred)\n",
    "write.csv(submission, â€œsubmission.csvâ€, row.names=FALSE)\n",
    "\n",
    "You should then submit the file \"submission.csv\" by clicking on \"Make a Submission\" on the Kaggle website.\n",
    "\n",
    "If you take a look at the file \"submission.csv\", you should see that the file contains a header and has the following format:\n",
    "\n",
    "UniqueID,Probability1\n",
    "6533,0.279672578 \n",
    "6534,0.695794648 \n",
    "6535,0.695794648 \n",
    "6536,0.279672578 \n",
    "6537,0.554216867 \n",
    "6538,0.640816327 \n",
    "6539,0.695794648\n",
    "etc."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "PredTest = predict(nytRF, newdata=nytUNKSparse, type=\"response\")\n",
    "MySubmission = data.frame(UniqueID = nytUNK$UniqueID, Probability1 = PredTest)\n",
    "write.csv(MySubmission, \"Submission0.csv\", row.names=FALSE)"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "R",
   "language": "R",
   "name": "ir"
  },
  "language_info": {
   "codemirror_mode": "r",
   "file_extension": ".r",
   "mimetype": "text/x-r-source",
   "name": "R",
   "pygments_lexer": "r",
   "version": "3.1.3"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 0
}
