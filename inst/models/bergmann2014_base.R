library(tdmore)
library(shinytdmore)
library(magrittr)

#' Build 'bergmann2014_base' model.
#' Bergmann, Troels K., et al. "Population pharmacokinetics of tacrolimus in adult kidney transplant patients:
#' impact of CYP3A5 genotype on starting dose." Therapeutic drug monitoring 36.1 (2014): 62-70.
#' 
#' @return a tdmore model
#'
buildModel <- function() {
  omega <- c(ECL=0.486, EV1=1.136, EV2=0.914, EKA=0.549)**2
  
  RxODE::RxODE("
TVCL = 23.9
CL_CYP3A5 = 1.95
TVV1 = 145 #L
TVQ = 101
TVV2 = 1500 #L
TVKA = 0.51
TLAG = 0.30

CL = TVCL * CL_CYP3A5^(CYP3A5) * exp(ECL)
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
  ") %>% tdmore(
    parameters=names(omega),
    omega=omega,
    res_var=list(errorModel(prop=0.295))
  ) %>% metadata(covariate("CYP3A5", label="CYP3A5 expressor", choices=list(Fast=0, Slow=1))) %>%
    metadata(output(name="CONC", label="Tacrolimus concentration", unit="ng/mL", default_value=5)) %>%
    metadata(formulation(name="Prograft", unit="mg", dosing_interval=12, default_value=5, round_function=function(x){round(x/0.5)*0.5})) %>%
    metadata(formulation(name="Advagraf", unit="mg", dosing_interval=24, default_value=5, round_function=function(x){round(x/0.5)*0.5})) %>%
    metadata(target(min=12, max=15)) %>%
    metadata(observed_variables(c("CL", "V1", "V2", "KA")))
}
buildModel()