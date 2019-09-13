library(tdmore)
library(shinytdmore)
library(magrittr)

#' Build 'bergmann2014_mixture' model.
#' Bergmann, Troels K., et al. "Population pharmacokinetics of tacrolimus in adult kidney transplant patients:
#' impact of CYP3A5 genotype on starting dose." Therapeutic drug monitoring 36.1 (2014): 62-70.
#' 
#' @return a tdmore_mixture model
#'
buildModel <- function() {
  omega <- c(ECL=0.486, EV1=1.136, EV2=0.914, EKA=0.549)**2
  rxodeModel <- "
TVCL = 23.9
TVV1 = 145 #L
TVQ = 101
TVV2 = 1500 #L
TVKA = 0.51
TLAG = 0.30

CL = TVCL * 1.95^(CYP3A5) * exp(ECL)
V1 = TVV1 * exp(EV1)
Q = TVQ
V2 = TVV2*exp(EV2)
KA = TVKA * exp(EKA)

K12 = Q/V1
K21 = Q/V2

d/dt(depot) = -KA*depot
alag(depot) = TLAG
d/dt(center) = KA*depot - CL/V1 * center - K12*center + K21 * periph
d/dt(periph) = K12*center - K21 * periph

CONC = center / V1 * 1000
  "
  outputM <- output(name="CONC", label="Tacrolimus concentration", unit="ng/mL", default_value=5)
  form1 <- formulation(name="Prograft",unit="mg", dosing_interval=12, default_value=5, round_function=function(x){round(x/0.5)*0.5})
  form2 <- formulation(name="Advagraf",unit="mg", dosing_interval=24, default_value=5, round_function=function(x){round(x/0.5)*0.5})
  targetM <- target(min=12, max=15)
  observedM <- observed_variables(c("CL", "V1", "V2", "KA"))
  
  fastMetaboliser <- RxODE::RxODE(gsub("CYP3A5", "0", rxodeModel)) %>%
    tdmore(parameters=names(omega), omega=omega, res_var=list(errorModel(prop=0.295))) %>%
    metadata(outputM, form1, form2, targetM, observedM)
  
  slowMetaboliser <- RxODE::RxODE(gsub("CYP3A5", "1", rxodeModel)) %>%
    tdmore(parameters=names(omega), omega=omega, res_var=list(errorModel(prop=0.295))) %>%
    metadata(outputM, form1, form2, targetM, observedM)
  
  regimen <- data.frame(
    TIME=c(0,24),
    AMT=10
  )
  # plot(fastMetaboliser, regimen)
  # plot(slowMetaboliser, regimen)
  mixture <- tdmore_mixture(fastMetaboliser, slowMetaboliser, probs = c(0.13, 0.87))
}
buildModel()