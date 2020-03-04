fast_table <- function(x, size = 5L) {
  .Call("_lidR_fast_table", x, size)
}

fast_countover <- function(x, t) {
  .Call("_lidR_fast_countover", x, t)
}

stdmetrics_z2 = function(z, dz = 1, th = 2)
{
  ###############
  
  ctv = function(z){quantile(z[z>=th],probs=0.95,type=2)}
  
  DensityMetrics <- function(z){  
    if(is.na(ctv(z))==FALSE){
      k<-vector()
      for (i in 0:9){
        k<-cbind(k,(length(z[z >= th+((ctv(z)-th)/10)*i])/length(z))) 
      }
      k<-as.numeric(k)
    }
    else
    {
      k <- rep(0,10)
    }
  }
  
  d <- DensityMetrics(z)
  names(d) = paste0("zpcum", 0:9)
  
  ###############
  z <- z[z>=th]
  
  probs = seq(0.1,0.9,0.1)
  zq 	  = as.list(stats::quantile(z, probs))
  names(zq) = paste0(distpre, probs*100)
  
  skewness<-function(z){ 
    m3<-sum((z-mean(z))^3)/length(z) 
    s3<-sqrt(var(z))^3 
    m3/s3 
  } 
  kurtosis<-function(z){ 
    m4<-sum((z-mean(z))^4)/length(z) 
    s4<-var(z)^2 
    m4/s4 - 3 
  } 
  coefvar<-function(z){
    sd(z)/mean(z)
  }
  
  metrics = list(
    max  = max(z),
    mean = mean(z),
    sd   = stats::sd(z),
    cv   = coefvar(z),
    kurt = kurtosis (z),
    skewness = skewness(z)
  )
  names(metrics) <- paste0(distpre,gsub("[%]","",names(metrics)))
  
  metrics = c(metrics, zq, d)
  
  return(metrics)
}


# How to call it
# metrics <- grid_metrics(ctg1, ~stdmetrics_z2(Z) , sqrt(250))
# 
# How to call it for catalog_apply
# f <- function(cluster) {
#   las <- readLAS(cluster)
#   if (is.empty(las)) return(NULL)
# 
#   metrics <- cloud_metrics(las, ~stdmetrics_z2(Z))
#   return(metrics)
# }
# plots_metrics <- catalog_apply(plots_ALS_clipped_ID, f)
