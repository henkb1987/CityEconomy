PlotPriceHistory <- function(world, price.history, i, n.iter){
  plot(price.history[1,1:i],type="l",col=rainbow(26)[1],
       xlim=c(0,n.iter),ylim=c(min(price.history,na.rm = T),max(price.history,na.rm = T)),
       lty=world$resource.information$type[1]+1)
  for(j in 2:26){
    lines(price.history[j,1:i],type="l",col=rainbow(26)[j], lty=world$resource.information$type[j]+1)
  }
  text(i, world$resource.information$price,LETTERS)
}
