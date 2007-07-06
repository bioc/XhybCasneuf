runSimulation  <- function(){
set.seed(0xbeef7)

 ourmedianpolish = function (x, ...) {
    tmp <- medpolish(log2(x), trace.iter = FALSE, ...)
    list(exprs = tmp$overall + tmp$col, row=tmp$row, residuals=tmp$residuals)
 }

 nrpps  <- 11  ## number of reporters per probeset 
 nrarray <- 14 ## number of arrays
 nrpxhyb <- 2  ## number of reporters that cross-hybe

 linecolors = hsv(rev(seq(0, 1, length=nrpps+1)[-1]), s=0.8, v=.7)
 fillcolors = hsv(rev(seq(0, 1, length=nrpps+1)[-1]), s=0.4, v=.7)

 n <- nrpps*nrarray
 x <- array(as.numeric(NA), dim=c(nrpps, nrarray, 3))

 delta = 0.1   ## multiplicative noise
 alpha = 30      ## additive noise
 crosshyb = 0.2  ## strength of cross-hyb

## 1st probe set: sinusoid expression pattern
 x[,,1] <- (1000 + 500*sin(seq(0, 2*pi, length=n))) * exp(delta*rnorm(n)) + alpha*rnorm(n)
## 2nd probe set: white noise
 x[,,2] <- 200 + alpha*rnorm(n)
 x[,,3] <- rbind(200+x[1:nrpxhyb,,1]*crosshyb, x[(nrpxhyb+1):nrpps,,2])

 layout(rbind(c(1,2), c(3,4)))
 par(mfrow=c(2,2))
 xlab = "Arrays"
 ylab = expression(log[2]~Intensity)
 ylim = log2(range(x))
 ytxt = ylim[2]+(diff(ylim)/4.5)
 xtxt = -0.8

## A-C) Reporter level data

  cat("Plot background-corrected, normalised expression data of reporters:  \n")
 for(j in 1:3) {
  cat("   - of probe set ", LETTERS[j], " -> plot ", LETTERS[j] ,"\n")
   matplot(t(log2(x[,,j])), type = "l", ylab=ylab, xlab=xlab, ylim=ylim, lty=1, col=linecolors, main = paste("probe set ", LETTERS[j], sep = " "))
 }

 omedp  = apply(x, 3, ourmedianpolish) ## Our Own MedianPolish
 omas   = apply(x, 3, generateExprVal.method.mas)
 oliw   = apply(x, 3, generateExprVal.method.liwong)

 smedp = sapply(omedp, "[[", "exprs")
 smas  = sapply(omas,   "[[", "exprs")
 sliw  = sapply(oliw,   "[[", "exprs")

## Just to double-check - compare with the one from affy::rma
 mycdf = new.env()
 exprm = NULL
 for(j in 1:dim(x)[3]) {
   exprm = rbind(exprm, x[,,j])
   assign(sprintf("Probe%03d", j), cbind(1:nrpps+(j-1)*nrpps, rep(as.integer(NA), nrpps)), envir=mycdf)
 }

## D) Plot Summarized Values
 lincol = c("black", "blue", "orange")
 matplot(smedp, type = "l", ylab=ylab, xlab=xlab, ylim=ylim,, col = lincol, lty=c(2,4,1), main = "D")
  cat("Plot median-polish summarised expression data of the three probe sets -> plot D\n")
 
 legend(1, 9.5, legend=paste("probe set", LETTERS[1:3]), col=lincol, lty=c(2,4,1))

 corrs = cor(smedp)
 cmas = cor(smas)
 cliw = cor(sliw)

cat("\n\n***  Pearson correlation coefficients between expression values summarised with:\n")
 myPrint = function(cr, nm) {
   cat("***", nm, "***\n")
   cat("corr.: probe sets A and B: ", signif(cr[1,2],3), "\n")
   cat("corr.: probe sets A and C: ", signif(cr[1,3],3), "\n\n")
 }
 myPrint(corrs, "median polish")
 myPrint(cmas, "tuckey's biweight")
 myPrint(cliw, "dChip")
}
