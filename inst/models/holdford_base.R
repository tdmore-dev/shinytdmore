library(tdmore)
library(shinytdmore)
library(RxODE)

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
holdford_base <- tdmore(rxModel, omega=omegas, res_var=list(errorModel("CONC", prop=0.149))) %>% 
  metadata(output(name="CONC", label="Tacrolimus concentration", unit="ng/mL", default_value=5)) %>%
  metadata(formulation(name="Default",unit="mg", dosing_interval=12, default_value=8)) %>%
  metadata(target(min=12, max=15))