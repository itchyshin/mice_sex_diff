#### Calculate metanalaysis statistics

#### Created by A M Senior @ the University of Otago NZ 03/01/2014

#### Below are funcitons for calculating effect sizes for meta-analysis of variance. 
#### All functions take the mean, sd and n from the control and experimental groups.

#### The first function, Cal.lnCVR, calculates the the log repsonse-ratio of the coefficient of variance (lnCVR) - see Nakagawa et al 2015.

#### The second function calculates the measuremnt error variance for lnCVR. As well as the aforementioned parameters, this function also takes
#### Equal.E.C.Corr (default = T), which must be True or False. If true, the funciton assumes that the correlaiton between mean and sd (Taylor's Law) 
#### is equal for the mean and control groups, and, thus these data are pooled. If False the mean-SD correlation for the experimental and control groups
#### are calculated seperatley from one another.

### Similar functions are then implemented for lnVR (for comparison of standard deviations) and ln RR  (for comparison of means) 

Calc.lnCVR <- function(CMean, CSD, CN, EMean, ESD, EN){
  log(ESD) - log(EMean) + 1 / (2*(EN - 1)) - (log(CSD) - log(CMean) + 1 / (2*(CN - 1)))
}

Calc.var.lnCVR <- function(CMean, CSD, CN, EMean, ESD, EN, Equal.E.C.Corr=T) {
  if(Equal.E.C.Corr==T){
    mvcorr <- cor.test(log(c(CMean, EMean)), log(c(CSD, ESD)))$estimate
    S2 <- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 2 * mvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 2 * mvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))
  }
  else{
    Cmvcorr <- cor.test(log(CMean), log(CSD))$estimate
    Emvcorr <- cor.test(log(EMean), (ESD))$estimate
    S2 <- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 2 * Cmvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 2 * Emvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))		
  }
  S2
}

Calc.lnVR <- function(CSD, CN, ESD, EN){
  log(ESD) - log(CSD) + 1 / (2*(EN - 1)) - 1 / (2*(CN - 1))
}

Calc.var.lnVR <- function( CN,  EN) {
  1 / (2*(EN - 1)) + 1 / (2*(CN - 1))
}

Calc.lnRR <- function(CMean, CSD, CN, EMean, ESD, EN) {
  log(EMean) - log(CMean)
}

Calc.var.lnRR <- function(CMean, CSD, CN, EMean, ESD, EN) {
  CSD^2/(CN * CMean^2) + ESD^2/(EN * EMean^2)
}