#' ---
#' title: "Optimal timing"
#' author: "A E Camaclang"
#' date: "2021-10-14"
#' output: github_document
#' ---
#'
#' Creates figures for the manuscript:
#' Camaclang, AE, Chades, I, Martin, TG, and Possingham, HP. (2021)
#' "Predicting the optimal amount of time to spend learning before
#' designating protected habitat for threatened species."
#'
#'

# library(opttiming)
# library(here)
# path <- here()

#' Set parameters
mlin<-9/50

bhyp1<-9.25
mhyp1<-1

bhyp2<-10
mhyp2<-5

b1<-10
m1<-0.15

b2<-10.75
m2<-0.1

a0 <- 1 # accuracy at time = 0 (y-intercept of learning curves)

B1 <- 0.2 # B = threshold false positive rate, beta
B2 <- 0.5
l1 <- 0.01 # l = rate of habitat loss, lambda (or negative pop'n growth rate, r)
l2 <- 0.02

#' ROC curve function
ROC <- function(a, x) x^(1/a)

#' Figure 1 plots
# ROC curves (true positive rate vs. false positive rate)
pdf(paste(path, "/analysis/ROCcurves.pdf", sep = ""), 4, 4)
par(mar=c(4.1,4.1,2.1,1.1))
curve(ROC(1,x), from = 0, to = 1, ylim = c(0,1),
      xaxs = "i", yaxs = "i", bty = "n",
      cex.lab = 1.5, mgp = c(2.5,1,0),
      xlab = expression(paste("False positive rate, ", beta)),
      ylab = expression(paste("True positive rate, ", delta)))
curve(ROC(3,x), from = 0, to = 1, add = TRUE)
curve(ROC(10,x),from = 0, to = 1, add = TRUE)
text(0.5, 0.3, "accuracy=1", cex = 1.2)
text(0.35, 0.55, "accuracy=3", cex = 1.2)
text(0.2, 0.92, "accuracy=10", cex = 1.2)

dev.off()

# Proportion of habitat designated vs. time spent learning, no habitat loss
# given beta = 0.2 (B1) and linear learning curve
pdf(paste(path, "/analysis/learning.pdf", sep = ""), 4, 4)
curve(B1^(1/(linear(m = mlin, yint = a0, x = x))),
      xlim = c (0,50), ylim = c(0,1), xaxs = "i", yaxs = "i", bty = "n",
      cex.lab = 1.5, mgp = c(2.5, 1, 0),
      xlab = "Time spent learning", ylab = "Proportion of habitat designated")
dev.off()

# Proportion of habitat remaining over time, given lambda = 0.02 (l2)
pdf(paste(path, "/analysis/habitatloss.pdf", sep = ""), 4, 4)
curve(exp(-l2 * x), xlim = c(0,50), ylim = c(0,1), xaxs = "i", yaxs = "i", bty = "n",
      cex.lab = 1.5, mgp = c(2.5, 1, 0),
      xlab = "Time", ylab = "Proportion of habitat remaining")
dev.off()

# Proportion of habitat designated vs. time spent learning, with habitat loss
pdf(paste(path, "/analysis/optimal.pdf", sep = ""),4,4)
curve(exp(-l2 * x) * B1^(1/(linear(m = mlin, yint = a0, x = x))),
      xlim = c(0,50), ylim = c(0,1), xaxs = "i", yaxs = "i", bty = "n",
      cex.lab = 1.5, mgp = c(2.5,1,0),
      xlab = "Time spent learning", ylab = "Proportion of initial habitat area")

# Optimal time (t*) and proportion protected at t*
lowlin <- ((sqrt(-(log(B1) * mlin)/l2)) - 1)/0.18 # t*
area <- B1^(1/(mlin * lowlin + 1)) * exp(-l2 * lowlin) # max prop designated
segments(x0 = lowlin, y0 = 0, x1 = lowlin, y1 = area, lty = "dashed")
mtext("optimal\ntime", side = 1, at = c(lowlin), line = 0.5, cex = 0.7)
segments(x0 = 0, y0 = area, x1 = lowlin, y1 = area, lty = "dashed")
mtext("max\narea", side = 2, at = c(area+0.03), line = 0.1, cex = 0.7, las = 1)

dev.off()

#' Figure 2. Hypothetical curves modelling (a) the increase in the accuracy, a,
#' of habitat identification over time as learning occurs, and (b) the rate of
#' true positives, delta, relative to the rate of false positives, beta, for
#' different values of a, based on ROC curves of the form delta = beta^(1/a)

pdf(paste(path,"/analysis/Fig2.pdf", sep = ""), 3.15, 7)
# tiff(paste(path,"/analysis/Fig2.pdf", sep = ""), 3.15, 7 ,units="in", res=600)
par(mfrow = c(2, 1))
par(mar = c(4.1, 4.1, 2.1, 1.1))

# Learning curves
curve(linear(m = mlin, yint = a0, x = x),
      from = 0, to = 50, ylim = c(0,10),
      xaxs = "i", yaxs = "i", bty = "n", mgp = c(2.5, 1, 0),
      xlab = "Time, t", ylab = "Accuracy, a")
curve(hyperb(b = bhyp1, m = mhyp1, yint = a0, x = x),
      from = 0, to = 50, col = "blue", add = TRUE)
curve(hyperb(b = bhyp2, m = mhyp2, yint = a0, x = x),
      from = 0, to = 50, col = "blue", lty = "dashed", add = TRUE)
curve(sigmoid(b = b1, m = m1, yint = a0, x = x),
      from = 0, to = 50, col = "red", add = TRUE)
curve(sigmoid(b = b2, m = m2, yint = a0, x = x),
      from = 0, to = 50, col = "red", lty = "dashed", add = TRUE)
legend(x = "bottomright", inset = 0.05, cex = 0.75,
       legend = c("linear", "hyperbolic 1", "hyperbolic 2", "sigmoid 1", "sigmoid 2"),
       col = c("black","blue","blue","red","red"),
       lty = c(1,1,2,1,2), bty = "n",
       title = "Learning curves")
mtext("(a)", side = 3, adj = 0, line = 1)

# ROC curves
curve(ROC(1,x), from = 0, to = 1, ylim = c(0,1),
      xaxs = "i", yaxs = "i", bty = "n", mgp = c(2.5, 1, 0),
      xlab = expression(paste("False positive rate, ", beta)),
      ylab = expression(paste("True positive rate, ", delta)))
curve(ROC(2,x), from = 0, to = 1, add = TRUE)
curve(ROC(4,x), from = 0, to = 1, add = TRUE)
curve(ROC(6,x), from = 0, to = 1, add = TRUE)
curve(ROC(8,x), from = 0, to = 1, add = TRUE)
curve(ROC(10,x),from = 0, to = 1, add = TRUE)
text(0.4, 0.3, "a=1", cex=0.75)
text(0.1, 0.9, "a=10", cex=0.75)
mtext("(b)", side = 3, adj = 0, line = 1)

dev.off()

#' Figure 3. Proportion of initial habitat area correctly identified
#' and protected over time when no habitat loss is occurring (black solid line)
#' and when habitat area is decreasing by lambda (l2) = 0.02 (red solid line), assuming a
#' linear learning curve and false positive rate beta (B2) = 0.5.

pdf(paste(path,"/analysis/Fig3.pdf", sep = ""),3.15,3.15)
# tiff(paste(path,"/analysis/Fig3.tiff", sep = ""),width = 4, height = 4, units = "in",res=600)
par(mar = c(4.1,4.1,1.1,1.1))

# Proportion correctly identified learning over time with learning, and without habitat loss
curve(B2^(1/(linear(m = mlin, yint = a0, x = x))), xlim = c(0,50), ylim = c(0,1),
      xaxs = "i", yaxs = "i", bty = "n",
      cex.lab = 1, cex.axis = 1, mgp = c(2.5,1,0),
      xlab = "Time", ylab = "Proportion of initial habitat area")

# Proportion of initial habitat correctly identified over time with learning AND with habitat loss
curve(exp(-l2 * x) * B2^(1/(linear(m = mlin, yint = a0, x = x))), xlim = c(0,50), ylim = c(0,1),
      xaxs = "i", yaxs = "i",
      add = TRUE, col = "red")

# Proportion of habitat remaining over time with habitat loss
curve(exp(-l2 * x), xlim=c(0,50), ylim=c(0,1), # available, with habitat loss
      xaxs = "i",yaxs = "i",
      add = TRUE, col = "red",lty = "dotdash")

# Proportion of habitat available without habitat loss (= 1)
segments(x0 = 0, y0 = 1, x1 = 50, y1 = 1, lty = "dotdash")

# Proportion correctly identified without time spent learning (= beta)
segments(x0 = 0, y0 = B2, x1 = 50, y1 = B2, col = "blue")

# Optimal time and proportion correctly identified at the optimal time
lowlin <- ((sqrt(-(log(B2) * mlin)/l2)) - 1)/0.18 # optimal time, t*
area <- B2^(1/(mlin * lowlin + 1)) * exp(-l2 * lowlin) # max prop. correctly identified
segments(x0 = lowlin, y0 = 0, x1 = lowlin, y1 = area, lty = "dotted", col = "grey50")
mtext("t*", side = 1, at = c(lowlin), cex = 0.6)
# segments(x0 = -2, y0 = area, x1 = lowlin, y1 = area,lty = "dotted", col = "grey50")

## Maximum amount of time (tmax) to spend learning; beyond this,
## a smaller proportion of initial habitat area is correctly designated
## than when designation occurs immediately (i.e., without learning)
# maxt <- (((-mlin * log(B2))/l2) - 1)/mlin
# segments(x0 = maxt, y0 = -0.4 , x1 = maxt, y1 = B2, lty = "dotted", col = "grey50") # value of tmax
# mtext("tmax", side = 1, at = c(maxt), cex = 0.6)

# Manual legend
legend(x = "bottomright", legend = c("available, no loss",
                                     "available, with loss",
                                     "protected with learning, no loss",
                                     "protected with learning, with loss",
                                     "protected with no learning"),
       col = c("black","red","black","red","blue"),
       lty = c(4,4,1,1,1),
       bty = "n", cex = 0.55)
# , inset = c(0,0.05))

dev.off()

#' Figure 4. (a) Optimal amount of time to spend learning before protecting habitat
#' for the koala at different rates of habitat loss for five different learning curves
#' and for false positive beta = 0.5 and beta = 0.2, and (b) the corresponding proportion
#' of the initial habitat area protected when the optimal amount of time is spent learning
#' before designation.
#'
#' For habitat loss rates ranging from lambda = 0 to lambda = 0.01
#'
pdf(paste(path,"/analysis/Fig4.pdf", sep = ""), 7, 7) # Figure 6
# tiff(paste(path, "/analysis/Fig4.tiff", sep = ""), 7, 7, units = "in", res = 600)
par(mfrow = c(2,2))
par(mar = c(5.1,4.1,2.1,1.1)) #adjusts plot margins

# Optimal time to designate vs. habitat loss rate

# beta = 0.5 (B2)
curve(optlin(B = B2, m = mlin, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), xaxs = "i", yaxs = "i", bty = "n",
      xlab = expression(paste("Proportion of habitat area lost , ", lambda)),
      ylab = "Optimal time to spend learning (years)")
curve(opthyp(B = B2, b = bhyp1, m = mhyp1, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), add = TRUE, col = "blue")
curve(opthyp(B = B2, b = bhyp2, m = mhyp2, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), add = TRUE, col = "blue", lty = "dashed")
curve(optsig(B = B2, b = b1, m = m1, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), add = TRUE, col = "red")
curve(optsig(B = B2, b = b2, m = m2, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), add = TRUE, col = "red",lty = "dashed")

mtext("(a)", side = 3, line = 1, adj = 0)

mtext(expression(paste(beta, " = 0.5")), side = 3, line = 0, adj = 0.5)

# beta = 0.2 (B1)
curve(optlin(B = B1, m = mlin, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), xaxs = "i", yaxs = "i", bty = "n",
      xlab = expression(paste("Proportion of habitat area lost, ", lambda)),
      ylab = "Optimal time to spend learning (years)")
curve(opthyp(B = B1, b = bhyp1, m = mhyp1, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), add = TRUE, col = "blue")
curve(opthyp(B = B1, b = bhyp2, m = mhyp2, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), add = TRUE, col = "blue", lty = "dashed")
curve(optsig(B = B1, b = b1, m = m1, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), add = TRUE, col = "red")
curve(optsig(B = B1, b = b2, m = m2, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,50), add = TRUE, col = "red", lty = "dashed")

mtext(expression(paste(beta, " = 0.2")), side = 3, line = 0, adj = 0.5)

# Prop of habitat designated at topt vs. habitat loss rate
# beta = 0.5 (B2)
curve(proplin(B = B2, m = mlin, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), xaxs = "i", yaxs = "i", bty = "n",
      ylab = "Proportion of habitat protected at optimal time",
      xlab = expression(paste("Proportion of habitat area lost, ", lambda)))
curve(prophyp(B = B2, b = bhyp1, m = mhyp1, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), add = TRUE, col = "blue")
curve(prophyp(B = B2, b = bhyp2, m = mhyp2, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), add = TRUE, col = "blue", lty = "dashed")
curve(propsig(B = B2, b = b1, m = m1, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), add = TRUE, col = "red")
curve(propsig(B = B2, b = b2, m = m2, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), add = TRUE, col = "red", lty = "dashed")

mtext("(b)", side = 3, line = 1, adj = 0)

# beta = 0.2 (B1)
curve(proplin(B = B1, m = mlin, yint= a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), xaxs = "i", yaxs = "i", bty = "n",
      ylab = "Proportion of habitat protected at optimal time",
      xlab = expression(paste("Proportion of habitat area lost, ", lambda)))
curve(prophyp(B = B1, b = bhyp1, m = mhyp1, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), add = TRUE, col = "blue")
curve(prophyp(B = B1, b = bhyp2, m = mhyp2, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), add = TRUE, col = "blue", lty = "dashed")
curve(propsig(B = B1, b = b1, m = m1, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), add = TRUE, col = "red")
curve(propsig(B = B1, b = b2, m = m2, yint = a0, q = 1, x = x),
      xlim = c(0,0.01), ylim = c(0,1), add = TRUE, col = "red", lty = "dashed")

legend(x = "bottomright", inset = 0.05, bty = "n",
       legend = c("linear", "hyp 1", "hyp 2", "sig 1", "sig 2"),
       col = c("black","blue","blue","red","red"),
       lty = c(1,1,2,1,2),
       title = "Learning curve")

dev.off()

#' Figure 5. (a) Optimal amount of time to spend learning before protecting habitat
#' for the northern abalone at different rates of poaching for five different
#' learning curves and for false positive rate beta = 0.5 and beta = 0.2, and
#' (b) the corresponding proportion of the initial habitat area protected when
#' the optimal amount of time is spent learning before designation.
#'
#' For poaching rates ranging from lambda = 0 to lambda = 0.5
#'
pdf(paste(path, "/analysis/Fig5.pdf", sep = ""), 7, 7) # Figure 7
# tiff(paste(path, "/analysis/Fig5.tiff", sep = ""), 7, 7, units = "in",res = 600)
par(mfrow = c(2,2))
par(mar = c(5.1,4.1,2.1,1.1)) #adjusts plot margins

# Optimal time to designate vs. poaching rate
# beta = 0.5 (B2)
curve(optlin(B = B2, m = mlin, yint = a0, q = 1, x = x),
      from = NULL, to = -log(B2) * mlin,
      xlim = c(0,0.5), ylim = c(0,50), xaxs = "i", yaxs = "i", bty = "n",
      xlab = expression(paste("Proportion of habitat area poached, ", lambda)),
      ylab = "Optimal time to spend learning (years)")
curve(opthyp(B = B2, b = bhyp1, m = mhyp1, yint = a0, q = 1, x = x),
      xlim = c(0,0.5), ylim = c(0,50), add = TRUE, col = "blue")
curve(opthyp(B = B2, b = bhyp2, m = mhyp2, yint = a0, q = 1, x = x),
      xlim = c(0,0.5), ylim = c(0,50), add = TRUE, col = "blue", lty = "dashed")
curve(optsig(B = B2, b = b1, m = m1, yint = a0, q = 1, x = x),
      from = NULL, to = -(b1 - a0) * m1 * log(B2)/b1 * a0,
      xlim = c(0,0.5), ylim = c(0,50), add = TRUE, col = "red")
curve(optsig(B = B2, b = b2, m = m2, yint = a0, q = 1, x = x),
      from = NULL, to = -(b2 - a0) * m2 * log(B2)/b2 * a0,
      xlim = c(0,0.5), ylim = c(0,50), add = TRUE, col = "red", lty = "dashed")

mtext("(a)", side = 3, line = 1, adj = 0)

mtext(expression(paste(beta, " = 0.5")), side = 3, line = 0, adj = 0.5)

# beta = 0.2 (B1)
curve(optlin(B = B1, m = mlin, yint = a0, q = 1, x = x),
      from = NULL, to = -log(B1) * mlin,
      xlim = c(0,0.5), ylim = c(0,50), xaxs = "i", yaxs = "i", bty = "n",
      xlab = expression(paste("Proportion of habitat area poached, ", lambda)),
      ylab = "Optimal time to spend learning (years)")
curve(opthyp(B = B1, b = bhyp1, m = mhyp1, yint = a0, q = 1, x = x),
      xlim = c(0,0.5), ylim = c(0,50), add = TRUE, col = "blue")
curve(opthyp(B = B1, b = bhyp2, m = mhyp2, yint = a0, q = 1, x = x),
      xlim = c(0,0.5), ylim = c(0,50), add = TRUE, col = "blue", lty = "dashed")
curve(optsig(B = B1, b = b1, m = m1, yint = a0, q = 1, x = x),
      from = NULL, to = -(b1 - a0) * m1 * log(B1)/b1 * a0,
      xlim = c(0,0.5), ylim = c(0,50), add = TRUE, col = "red")
curve(optsig(B = B1, b = b2, m = m2, yint = a0, q = 1, x = x),
      from = NULL, to = -(b2 - a0) * m2 * log(B1)/b2 * a0,
      xlim = c(0,0.5), ylim = c(0,50), add = TRUE, col = "red", lty = "dashed")

mtext(expression(paste(beta, " = 0.2")), side = 3, line = 0, adj = 0.5)

legend(x = "topright", inset = 0.05, bty = "n",
       legend = c("linear", "hyp 1", "hyp 2", "sig 1", "sig 2"),
       col = c("black","blue","blue","red","red"),
       lty = c(1,1,2,1,2),
       title = "Learning curves")

# Prop of habitat designated at topt vs. poaching rate
# beta = 0.5 (B2)
curve(proplin(B = B2, m = mlin, yint = a0, q = 1, x = x),
      from = NULL, to = -log(B2) * mlin,
      xlim = c(0,0.5), ylim = c(0,1), xaxs = "i", yaxs = "i", bty = "n",
      ylab = "Proportion of habitat protected at optimal time",
      xlab = expression(paste("Proportion of habitat area poached, ", lambda)))
segments(x0 = -log(B2) * mlin, y0 = B2, x1 = 0.5, y1 = B2)
curve(prophyp(B = B2, b = bhyp1, m = mhyp1, yint = a0, q = 1, x = x),
      xlim = c(0,0.5), ylim = c(0,1), add = TRUE, col = "blue")
curve(prophyp(B = B2, b = bhyp2, m = mhyp2, yint = a0, q = 1, x = x),
      xlim = c(0,0.5), ylim = c(0,1), add = TRUE, col = "blue", lty = "dashed")
curve(propsig(B = B2, b = b1, m = m1, yint = a0, q = 1, x = x),
      from = NULL, to = -(b1 - a0) * m1 * log(B2)/b1 * a0,
      xlim = c(0,0.5), ylim = c(0,1), add = TRUE, col = "red")
segments(x0 = -(b1 - a0) * m1 * log(B2)/b1 * a0, y0 = B2,
         x1 = 0.5, y1 = B2, col = "red")
curve(propsig(B = B2, b = b2, m = m2, yint = a0, q = 1, x = x),
      from = NULL, to = -(b2 - a0) * m2 * log(B2)/b2 * a0,
      xlim = c(0,0.5), ylim = c(0,1), add = TRUE, col = "red", lty = "dashed")
segments(x0 = -(b2 - a0) * m2 * log(B2)/b2 * a0, y0 = B2,
         x1 = 0.5, y1 = B2, col = "red", lty = "dashed")

mtext("(b)", side = 3, line = 1, adj = 0)

# beta = 0.2 (B1)
curve(proplin(B = B1, m = mlin, yint= a0, q = 1, x = x),
      from = NULL, to = -log(B1) * mlin,
      xlim = c(0,0.5), ylim = c(0,1), xaxs = "i", yaxs = "i", bty = "n",
      ylab = "Proportion of habitat protected at optimal time",
      xlab = expression(paste("Proportion of habitat area poached, ", lambda)))
segments(x0 = -log(B1) * mlin, y0 = B1, x1 = 0.5, y1 = B1)
curve(prophyp(B = B1, b = bhyp1, m = mhyp1, yint = a0, q = 1, x = x),
      xlim = c(0,0.5), ylim = c(0,1), add = TRUE, col = "blue")
curve(prophyp(B = B1, b = bhyp2, m = mhyp2, yint = a0, q = 1, x = x),
      xlim = c(0,0.5), ylim = c(0,1), add = TRUE, col = "blue", lty = "dashed")
curve(propsig(B = B1, b = b1, m = m1, yint = a0, q = 1, x = x),
      from = NULL, to = -(b1 - a0) * m1 * log(B1)/b1 * a0,
      xlim = c(0,0.5), ylim = c(0,1), add = TRUE,col = "red")
segments(x0 = -(b1 - a0) * m1 * log(B1)/b1 * a0, y0 = B1,
         x1 = 0.5, y1 = B1, col = "red")
curve(propsig(B = B1, b = b2, m = m2, yint = a0, q = 1, x = x),
      from = NULL, to = -(b2 - a0) * m2 * log(B1)/b2 * a0,
      xlim = c(0,0.5), ylim = c(0,1), add = TRUE, col = "red", lty = "dashed")
segments(x0 = -(b2 - a0) * m2 * log(B1)/b2 * a0, y0 = B1,
         x1 = 0.5, y1 = B1, col = "red", lty = "dashed")

dev.off()
