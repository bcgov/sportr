#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector gs2gw(NumericVector x, double a, double b){
  int len = x.length();
  NumericVector out(len);
  for(int i = 0; i < len; i++){
    out[i] = a*exp(x[i]*b);
  }
  return(out);
}


// [[Rcpp::export]]
NumericVector SimGrowth_Climate(DataFrame DF, double cmdMin, 
                        double cmdMax, double tempMin, double tempMax, double climLoss){
  NumericVector Growth = DF["Growth"]; //convert to vectors
  NumericVector ProbEvent  = DF["ProbEvent"];
  NumericVector MeanDead = DF["MeanDead"];
  NumericVector Ruin = DF["Suit"];//think about this one
  NumericVector climCMD = DF["CMD"];
  NumericVector climMax = DF["Tmax_sm"];
  NumericVector climMin = DF["Tmin_sp"];
  
  int numYears = Growth.length();
  NumericVector Returns(numYears);
  double height, percentDead, percentRuin, climDead, climDiff, diffProp;
  int prevTrees, numDead, i;
  int nTrees = 100;
  for(i = 0; i < numYears; i++){
    height = sum(Growth[Rcpp::Range(0,i)]);
    Returns[i] = nTrees*height;
    climDead = 0;
    if(climCMD[i] > cmdMax){ // CMD max 
      climDiff = climCMD[i] - cmdMax;
      diffProp = (climDiff*100)/(cmdMax - cmdMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too dry \n";
    }
    if(climCMD[i] < cmdMin){
      climDiff = cmdMin - climCMD[i];
      diffProp = (climDiff*100)/(cmdMax - cmdMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too wet \n";
    }
    if(climMax[i] > tempMax){
      climDiff = climMax[i] - tempMax;
      diffProp = (climDiff*100)/(tempMax - tempMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too hot \n";
    }
    if(climMin[i] < tempMin){
      climDiff = tempMin - climMin[i];
      diffProp = (climDiff*100)/(cmdMax - cmdMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too cold \n";
    }
    if(climDead > nTrees){
      climDead = nTrees;
    }
    nTrees = nTrees - climDead;
    if(Rcpp::runif(1,0,100)[0] > ProbEvent[i]){//regular environmental loss
      percentDead = Rcpp::rgamma(1, 2, MeanDead[i])[0];
      numDead = (percentDead/100)*prevTrees;
      nTrees = nTrees - numDead;
    }
  }
  return(Returns);
}

// [[Rcpp::export]]
NumericVector SimGrowth_Regular(DataFrame DF){
  NumericVector Growth = DF["SI"]; //convert to vectors
  NumericVector ProbEvent  = DF["ProbEvent"];
  NumericVector MeanDead = DF["Prop_Feas"];
  NumericVector Ruin = DF["FeasDiff"];//think about this one
  
  int numYears = Growth.length();
  NumericVector Returns(numYears);
  double height, percentDead, percentRuin, climDead, climDiff, diffProp;
  int prevTrees, numDead, i;
  int nTrees = 100;
  for(i = 0; i < numYears; i++){
    height = sum(Growth[Rcpp::Range(0,i)]);
    Returns[i] = nTrees*height;
    
    if(Rcpp::runif(1,0,100)[0] > ProbEvent[i]){//regular environmental loss
      Rcout << "Killing trees \n";
      percentDead = Rcpp::rgamma(1, 2, MeanDead[i])[0];
      numDead = (percentDead/100)*prevTrees;
      nTrees = nTrees - numDead;
    }
    if(Ruin[i] >= 2){
      percentDead = Rcpp::rgamma(1, 1.5, 25)[0];
      numDead = (percentDead/100)*prevTrees;
      nTrees = nTrees - numDead;
    }
  }
  return(Returns);
}

// [[Rcpp::export]]
NumericVector simGrowthCpp(DataFrame DF){
  int nTrees = 100;
  NumericVector Growth = DF["SI"]; //convert to vectors
  NumericVector ProbEvent  = DF["ProbEvent"];
// NumericVector ProbLoss = DF["ProbLoss"];
  NumericVector gamma_shp = DF["gamma_shp"];
  NumericVector gamma_rate = DF["gamma_rate"];
  NumericVector Ruin = DF["FeasDiff"];//think about this one
  
  int numYears = Growth.length();
  NumericVector Returns(numYears);
  //NumericVector Livetrees(numYears);
  double height, percentDead;
  int prevTrees, numDead, i;
  for(i = 0; i < numYears; i++){
    height = sum(Growth[Rcpp::Range(0,i)]);
    Returns[i] = nTrees*height;
    //Livetrees[i] = nTrees;
    if(Rcpp::runif(1,0,100)[0] > ProbEvent[i]){//regular environmental loss
      prevTrees = nTrees;
      percentDead = Rcpp::rgamma(1, gamma_shp[i], gamma_rate[i])[0];
      //Rcout << "Percent Dead:" << percentDead << "\n";
      numDead = (percentDead/100)*prevTrees;
      nTrees = prevTrees - numDead;
    }
    // if(Ruin[i] >= 3){
    //   prevTrees = nTrees;
    //   percentDead = Rcpp::rgamma(1, 1.5, 25)[0];
    //   numDead = (percentDead/100)*prevTrees;
    //   nTrees = nTrees - numDead;
    // }
  }
 return(Returns);
}

// [[Rcpp::export]]
NumericVector simTreesCpp(DataFrame DF){
  int nTrees = 100;
  NumericVector Growth = DF["SI"]; //convert to vectors
  NumericVector ProbEvent  = DF["ProbEvent"];
  // NumericVector ProbLoss = DF["ProbLoss"];
  NumericVector gamma_shp = DF["gamma_shp"];
  NumericVector gamma_rate = DF["gamma_rate"];
  NumericVector Ruin = DF["FeasDiff"];//think about this one
  
  int numYears = Growth.length();
  //NumericVector Returns(numYears);
  NumericVector Livetrees(numYears);
  double height, percentDead;
  int prevTrees, numDead, i;
  for(i = 0; i < numYears; i++){
    height = sum(Growth[Rcpp::Range(0,i)]);
   //Returns[i] = nTrees*height;
    Livetrees[i] = nTrees;
    if(Rcpp::runif(1,0,100)[0] > ProbEvent[i]){//regular environmental loss
      prevTrees = nTrees;
      percentDead = Rcpp::rgamma(1, gamma_shp[i], gamma_rate[i])[0];
      //Rcout << "Percent Dead:" << percentDead << "\n";
      numDead = (percentDead/100)*prevTrees;
      nTrees = prevTrees - numDead;
    }
    // if(Ruin[i] >= 3){
    //   prevTrees = nTrees;
    //   percentDead = Rcpp::rgamma(1, 1.5, 25)[0];
    //   numDead = (percentDead/100)*prevTrees;
    //   nTrees = nTrees - numDead;
    // }
  }
  return(Livetrees);
}