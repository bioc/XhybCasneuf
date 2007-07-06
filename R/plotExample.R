
setClass("XhybExamples", representation(X = "character", Y = "character", 
   IVX = "numeric", IVY = "numeric", IVXi = "matrix", ai = "numeric"))


setGeneric("plotExample", function(ex, ...) standardGeneric("plotExample"))
setMethod("plotExample", "XhybExamples", function(ex){
  par(mfrow=c(2,2))
#  par(mfrow=c(1,3))
  ymin = 2; ymax = 13; ylab = "Intensity"; xlab = "Arrays"; nbbreaks = 8
  breaks = round(seq(1,125, length.out=nbbreaks ))
  spectrum <- brewer.pal(9, name = "YlOrRd")[- seq(1,9-(nbbreaks -1))]
  linecolors = spectrum[findInterval(ex@ai, vec = breaks, rightmost.closed= TRUE, all.inside = TRUE)]

## plot X and Y's expression values
  matplot(c(ex@IVX), main = "A", type = "l", col = "blue", ylim = c(ymin, ymax), ylab = ylab, xlab = xlab)
  lines(ex@IVY, col = "orange")
   legend(1,13, col = c("blue","orange"), legend = paste(c("X: ", "Y: "), c(ex@X, ex@Y), sep = ""), lty =1)

 ## plot X's reporters' expression patterns
   matplot(t(ex@IVXi), type = "l", col = linecolors, ylim=c(ymin, ymax), ylab = ylab, xlab = xlab, lty = 1, main = "B")
   mylegend = paste("(", breaks[1:(length(breaks)-1)],"-" ,breaks[2:length(breaks)] , "]", sep = "")
#     legend(1, ymax, legend = mylegend[1:round((length(mylegend)/2))], col = spectrum[1:round(length(spectrum)/2)], lty = 1)
#     legend(6, ymax, legend = mylegend[(round(length(mylegend)/2)+1):length(mylegend)], col = spectrum[(round(length(mylegend)/2)+1):length(mylegend)], lty = 1)

## plot rho[Xi,Y]~Ai
 rhoXiY <- apply(ex@IVXi,1, function(r){cor(r, ex@IVY , method = "pearson") })
  plot(rhoXiY~ex@ai, pch = 17, col = linecolors, xlim = c(15, 127), ylab = expression(rho[x[i]*Y]),xlab=expression(a[i]), lwd=4, ylim=c(-1,1), main = "C")
  abline(0,0, lty = 2, col = "grey")
  abline(0.5,0, lty = 2, col = "grey")
  abline(-0.5,0, lty = 2, col = "grey")

 xx <- 1:10
 yy <- 1:10
 plot(yy~xx, type = "n", axes = FALSE, main ="LEGEND to B and C", xlab = "", ylab = "")
 legend(3, 10, legend = mylegend[1:length(mylegend)], col = spectrum, lty = 1)

}
)
