##
## Script name: 
##
## Purpose of script:
## List standard models for Tacrolimus
##
## Author: Ruben Faelens
##
## Date Created: Thu Jan 03 15:20:05 2019
##
## Copyright (c) Ruben Faelens, 2019
## Email: ruben.faelens@gmail.com
##
## ---------------------------
##
## Notes:
## Models were all selected based on a review paper by Brooks et al. 2016
##
## ---------------------------

library(nlmixr)
library(tdmore)

#Bergmann, Troels K., et al. "Population pharmacokinetics of tacrolimus in adult kidney transplant patients: impact of CYP3A5 genotype on starting dose." Therapeutic drug monitoring 36.1 (2014): 62-70.
omega <- c(ECL=0.486, EV1=1.136, EV2=0.914, EKA=0.549)**2
bergmann2014_base <- RxODE::RxODE("
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

CONC = center / V1
") %>% tdmore(
  parameters=names(omega),
  omega=omega,
  res_var=list(errorModel(prop=0.295))
) %>% metadata(covariate("CYP3A5", label="CYP3A5 expressor", choices=list(Fast=0, Slow=1))) %>%
  metadata(output(name="CONC", label="Tacrolimus concentration", unit="ng/mL", default_value=5)) %>%
  metadata(dose(unit="ug", dosing_interval=12, default_value=8000)) %>%
  metadata(target(min=10, max=15))
usethis::use_data(bergmann2014_base, overwrite=TRUE)

meropenem <- nlmixrUI(function(){
  ini({
    TVV1 <- 24.4;
    TVV2 <- 7.01;
    TVQ <- 4.97;
    TVCL <- 9.87;
    ECL ~ 0.194 # This value corresponds to OMEGA_CL (44% SD)
    EV1 ~ 0.287 # This value corresponds to OMEGA_V1 (54% SD)
    EPS_PROP <- 0.371 # Proportional error (37% SD)
  })
  model({
    CL <- TVCL * (WT/70)^0.75 * exp(ECL)
    V1 <- TVV1 * (WT/70) * exp(EV1)
    V2 <- TVV2 * (WT/70)
    Q <- TVQ * (WT/70)^0.75
    K12 <- Q/V1
    K21 <- Q/V2
    
    d/dt(center) = - CL/V1 * center - K12*center + K21 * periph
    d/dt(periph) = K12*center - K21 * periph
    
    CONC = center / V1
    CONC ~ prop(EPS_PROP) # Proportional error linked to the PK model
  })
}) %>% tdmore() %>% 
  metadata(covariate(name="WT", label="Weight", unit="kg", min=20, max=150)) %>%
  metadata(output(name="CONC", label="Meropenem concentration", unit="ng/mL", default_value=1)) %>%
  metadata(dose(unit="ug", dosing_interval=8, default_value=1000)) %>%
  metadata(target(min=10, max=15))
usethis::use_data(meropenem, overwrite=TRUE)

rxModel <- RxODE::RxODE('
#Holford model
# TODO: covariate effects

KA = 1.01;
CL= 16.1*exp(ECL);
V1= 125 * exp(EV1);
Q=23.8 * exp(EQ);
V2=636;
TLag=0.41;   # no IOV or IIV; TODO needs to be included
F=1 * exp(EFDay2); # TODO: code "increase after day 2"
# TODO: include IIV correlations CL-V1 of 0.43, CL-Q of 0.62
# TODO: include IOV of 23% on F, 120% on Ka
# TODO: covariate effects

Ke=CL/V1;
K12=Q/V1;
K21=Q/V2;

CONC=CENTR/V1 * 1000;

d/dt(ABS) = -KA*ABS;
d/dt(CENTR) = KA*ABS - K12*CENTR + K21*PERIP - Ke*CENTR;
d/dt(PERIP) = K12*CENTR - K21*PERIP;
')
omegas=c(ECL=0.40^2, EV1=0.54^2, EQ=0.63^2, EFDay2=0.57^2)
tacrolimuskidney <- tdmore(rxModel, omega=omegas, res_var=list(errorModel("CONC", prop=0.149))) %>% 
  metadata(output(name="CONC", label="Tacrolimus concentration", unit="ng/mL", default_value=5)) %>%
  metadata(dose(unit="ug", dosing_interval=12, default_value=8)) %>%
  metadata(target(min=10, max=15))
usethis::use_data(tacrolimuskidney, overwrite=TRUE)