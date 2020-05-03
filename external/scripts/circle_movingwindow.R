make_circ_filter <- function(radius,res){
  circ_filter<-matrix(NA, nrow=1+(2*radius/res), ncol=1+(2*radius/res))
  dimnames(circ_filter)[[1]]<-seq(-radius, radius, by=res)
  dimnames(circ_filter)[[2]]<-seq(-radius, radius, by=res)
  sweeper<-function(mat){
    for(row in 1:nrow(mat)){
      for(col in 1:ncol(mat)){
        dist<-sqrt((as.numeric(dimnames(mat)[[1]])[row])^2 +
                     (as.numeric(dimnames(mat)[[1]])[col])^2)
        if(dist<=radius) {mat[row, col]<-1}
      }
    }
    return(mat)
  }
  out<-sweeper(circ_filter)
  return(out)
}
