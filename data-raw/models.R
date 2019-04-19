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
)
usethis::use_data(bergmann2014_base, overwrite=TRUE)


## Furthermore, it uses the following covariates:
## CYP3A5 (1 for heterozygote and homozygote expressers, 0 for non-expressers)
## HEM (hematocrit fraction)
## POD (time since transplant)
## PRED_CmaxFree (value of free prednisolone nmol/L)

OCL = 0.295^2 #29.5%
OV1 = 0.468^2 #46.8%
OV2 = 0.894^2 #89.4%
OKA = 0.476^2 #47.6%

CorrV1Ka = 0.677
CorrV1V2 = -0.049
CorrKaV2 = -0.013

OV1Ka = CorrV1Ka*sqrt(OV1)*sqrt(OKA)
OV1V2 = CorrV1V2*sqrt(OV1)*sqrt(OV2)
OKaV2 = CorrKaV2*sqrt(OKA)*sqrt(OV2)

OCL_IOV = 0.299^2 #29.9%
OV1_IOV = 1.265^2 #126.5%

omega <- c(ECL=0.295, EV1=0.468, EV2=0.894, EKA=0.476, ECL_IOV=0.299, EV1_IOV=1.265)**2
omegaMatrix <- diag(omega)
colnames(omegaMatrix) <- names(omega)
rownames(omegaMatrix) <- names(omega)
omega <- omegaMatrix
omega["EV1", "EKA"] <- omega["EKA", "EV1"] <- OV1Ka
omega["EV1", "EV2"] <- omega["EV2", "EV1"] <- OV1V2
omega["EV2", "EKA"] <- omega["EKA", "EV2"] <- OKaV2

bergmann2014_full = RxODE::RxODE("
TVCL = 25.5
CYP3A5_CL = 1.60
HEM_CL = -1.01
POD_CL = -0.0021

TVV1 = 113.0
PRED_V1 = -0.0028

TVQ = 67.9
TVV2 = 1060
TVKA = 0.35
TLAG = 0.44

CL = TVCL * CYP3A5_CL^CYP3A5 *(1+HEM_CL*(HEM-0.33)) * (WT/70)^0.75 * (1+POD_CL*(POD-22.7)) *
    exp(ECL + ECL_IOV)
V1 = TVV1*(1+PRED_V1*(PRED_CMaxFree-155.5)) *
    exp(EV1 + EV1_IOV)
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
    parameters=rownames(omega),
    omega=omega,
    iov=c("ECL_IOV", "EV1_IOV"),
    res_var=list(errorModel(var="CONC", prop=0.183))
)
usethis::use_data(bergmann2014_full, overwrite=TRUE)


omega <- c(ECL=1, EV1=1, EV2=1, EKA=1)**2
bergmann2014_to_delete <- RxODE::RxODE("
TVCL = 23.9;
CL_CYP3A5 = 1.95;
TVV1 = 145; #L
TVQ = 101;
TVV2 = 1500; #L
TVKA = 0.51;

CL = TVCL * CL_CYP3A5^(CYP3A5) * exp(ECL);
V1 = TVV1 * exp(EV1);
Q = TVQ;
V2 = TVV2*exp(EV2);
KA = TVKA * exp(EKA);

K12 = Q/V1;
K21 = Q/V2;

d/dt(depot) = -KA*depot;
d/dt(center) = KA*depot - CL/V1 * center - K12*center + K21 * periph;
d/dt(periph) = K12*center - K21 * periph;

CONC = (center / V1)*1000;
") %>% tdmore(
  parameters=names(omega),
  omega=omega,
  res_var=list(errorModel(prop=0.295))
)
usethis::use_data(bergmann2014_to_delete, overwrite=TRUE)


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
tacrolimuskidney <- tdmore(rxModel, omega=omegas, res_var=list(errorModel("CONC", prop=0.149)))
usethis::use_data(tacrolimuskidney, overwrite=TRUE)