fast_table <- function(x, size = 5L) {
  .Call("_lidR_fast_table", x, size)
}

fast_countover <- function(x, t) {
  .Call("_lidR_fast_countover", x, t)
}

stdmetrics_z2 = function(z, dz = 1, th = 2)
{
  n = length(z)
  zmax  = max(z)
  zmean = mean(z)
  
  probs = seq(0.05, 0.95, 0.05)
  zq 	  = as.list(stats::quantile(z, probs))
  names(zq) = paste0("zq", probs*100)
  
  pzabovex = lapply(th, function(x) { fast_countover(z, x) / n * 100 })
  names(pzabovex) = paste0("pzabove", th)
  
  pzabovemean = fast_countover(z, zmean) / n * 100
  
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
  
  metrics = list(
    zmax  = zmax,
    zmean = zmean,
    zsd   = stats::sd(z),
    zskew = (sum((z - zmean)^3)/n)/(sum((z - zmean)^2)/n)^(3/2),
    zkurt = n * sum((z - zmean)^4)/(sum((z - zmean)^2)^2),
    zentropy  = entropy(z, dz),
    pzabovezmean = pzabovemean
  )
  
  metrics = c(metrics, pzabovex, zq, d)
  
  return(metrics)
}
