#' ---
#' title: "Minimax regret analysis"
#' author: "A E Camaclang"
#' date: "2021-10-14"
#' output: github_document
#' ---
#'
#' Calculates values for Table 2 of manuscript:
#' Camaclang, AE, Chades, I, Martin, TG, and Possingham, HP. (2021)
#' "Predicting the optimal amount of time to spend learning before
#' designating protected habitat for threatened species."

# library(here)
# path <- here()

#' Set learning curve parameters
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

B <- 0.2 # B = threshold false positive rate, beta

#' Set habitat loss rate, lambda (or negative population growth rate, r)
l <- 0.01 # for Table 2
# l <- 0.1 # for Table 2, and low poaching rate for northern abalone case study
# l <- 0.008 # habitat loss rate for koala case study
# l <- 0.3 # moderate poaching rate for northern abalone case study
# l <- 0.5 # high poaching rate for northern abalone case study

#' Determine optimal time to designate for each curve
OT_H1 <- opthyp(B, b = bhyp1, m = mhyp1, yint = a0, x = l)
OT_H2 <- opthyp(B, b = bhyp2, m = mhyp2, yint = a0, x = l)
OT_S1 <- optsig(B, b = b1, m = m1, yint = a0, x = l)
OT_S2 <- optsig(B, b = b2, m = m2, yint = a0, x = l)
OT_lin <- optlin(B, m = mlin, yint = a0, x = l)
OT <- c(OT_H1, OT_H2, OT_S1, OT_S2, OT_lin)

#' Determine accuracy at optimal time for each curve
a_h1 <- hyperb(b = bhyp1, m = mhyp1, yint = a0, x = OT_H1)
a_h2 <- hyperb(b = bhyp2, m = mhyp2, yint = a0, x = OT_H2)
a_s1 <- sigmoid(b = b1, m = m1, yint = a0, x = OT_S1)
a_s2 <- sigmoid(b = b2, m = m2, yint = a0, x = OT_S2)
a_lin <- linear(m = mlin, yint = a0, x = OT_lin)

#' Determine the proportion correctly identified with PERFECT INFO,
#' i.e., if designation occurs at optimal time for the true learning curve
h1 <- prophyp(B, b = bhyp1, m = mhyp1, a = a_h1, t = OT_H1, x = l)
h2 <- prophyp(B, b = bhyp2, m = mhyp2, a = a_h2, t = OT_H2, x = l)
s1 <- propsig(B, b = b1, m = m1, a = a_s1, t = OT_S1, x = l)
s2 <- propsig(B, b = b2, m = m2, a = a_s2, t = OT_S2, x = l)
lin <- proplin(B, m = mlin, a = a_lin, t = OT_lin, x = l)
PR <- rbind(h1, h2, s1, s2, lin) # values along diagonal in Table 2

#' Determine the proportion correctly identified when designating at
#' the optimal time for different ASSUMED learning curves (columns),
#' given that the TRUE curve is:
# Hyp 1
Pr_Hyp1 <- vector()
for (s in 1:length(OT)) {
  learntime <- OT[s]
  a <- hyperb(b = bhyp1, m = mhyp1, yint = a0, x = learntime)
  Pr_Hyp1[s] <- prophyp(B, b = bhyp1, m = mhyp1, a = a, t = learntime, x = l)
}

# Hyp 2
Pr_Hyp2 <- vector()
for (s in 1:length(OT)) {
  learntime <- OT[s]
  a <- hyperb(b = bhyp2, m = mhyp2, yint = a0, x = learntime)
  Pr_Hyp2[s] <- prophyp(B, b = bhyp2, m = mhyp2, a = a, t = learntime, x = l)
}

# Sig 1
Pr_Sig1 <- vector()
for (s in 1:length(OT)) {
  learntime <- OT[s]
  a <- sigmoid(b = b1, m = m1, yint = a0, x = OT[s])
  Pr_Sig1[s] <- propsig(B, b = b1, m = m1, a = a, t = learntime, x = l)
}

# Sig 2
Pr_Sig2 <- vector()
for (s in 1:length(OT)) {
  learntime <- OT[s]
  a <- sigmoid(b = b2, m = m2, yint = a0, x = OT[s])
  Pr_Sig2[s] <- propsig(B, b = b2, m = m2, a = a, t = learntime, x = l)
}

# Linear
Pr_Linear <- vector()
for (s in 1:length(OT)) {
  learntime <- OT[s]
  a <- linear(m = mlin, yint = a0, x = OT[s])
  Pr_Linear[s] <- proplin(B, m = mlin, a = a, t = OT[s], x = l)
}

Pr_Correct <- rbind(Pr_Hyp1, Pr_Hyp2, Pr_Sig1, Pr_Sig2, Pr_Linear) # rows = 'TRUE', cols = 'ASSUMED'
colnames(Pr_Correct) <- rownames(Pr_Correct)

#' Calculate the proportion 'missed' when the wrong learning curve is assumed (Table 2)
diff <- matrix(NA,5,5)
for (s in 1:length(OT)) {
  diff[,s] <- PR-Pr_Correct[,s]
}
rownames(diff) <- rownames(Pr_Correct)
colnames(diff) <- rownames(Pr_Correct)

#' Save results to .csv
# write.csv(Pr_Correct, paste(path,"/analysis/PropCorrect_b", B, "_l", l, ".csv", sep = ""))
# write.csv(diff, paste(path, "/analysis/MinMax_b", B, "_l", l, ".csv", sep = ""))


