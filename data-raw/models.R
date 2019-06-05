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

#________________________________________________________________________________________
#----                         Model: bergmann2014_base                               ----
#________________________________________________________________________________________
# Bergmann, Troels K., et al. "Population pharmacokinetics of tacrolimus in adult kidney transplant patients:
# impact of CYP3A5 genotype on starting dose." Therapeutic drug monitoring 36.1 (2014): 62-70.

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

#________________________________________________________________________________________
#----                        Model: meropenem_alloWT                                 ----
#________________________________________________________________________________________


meropenem_alloWT <- nlmixrUI(function(){
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
  metadata(target(min=2, max=5))
usethis::use_data(meropenem_alloWT, overwrite=TRUE)

#________________________________________________________________________________________
#----                             Model: holford_base                                ----
#________________________________________________________________________________________

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
  metadata(dose(unit="ug", dosing_interval=12, default_value=8)) %>%
  metadata(target(min=10, max=15))
usethis::use_data(holdford_base, overwrite=TRUE)

#________________________________________________________________________________________
#----                         Model: default_alloWT_MPC                              ----
#________________________________________________________________________________________

default_alloWT_MPC <- nlmixrUI(function(){
  ini({
    TVKA <- 3.7
    TVQ <- 10
    ECL ~ 0.0784 #ETA1, 28%
    EV1 ~ 0.0361 #ETA2, 19%
    EPS_PROP <- 0.23 # Proportional error, 23% SD
  })
  model({
    TVV1_next <- TVV1 * exp(EV1)
    TVCL_next <- TVCL * exp(ECL)
    
    KA <- TVKA
    CL <- TVCL_next * (WT/70)^0.75
    V1 <- TVV1_next
    V2 <- V1
    Q <- TVQ
    K12 <- Q/V1
    K21 <- Q/V2
    
    d/dt(depot) = -KA*depot
    d/dt(center) = KA*depot - CL/V1 * center - K12*center + K21 * periph
    d/dt(periph) = K12*center - K21 * periph
    
    CONC = (center / V1) * 100
    CONC ~ prop(EPS_PROP)
  })
}) %>% tdmore(iov=c("EV1", "ECL")) %>% 
  metadata(output(name="CONC", label="Drug concentration", unit="mg/mL", default_value=5)) %>%
  metadata(dose(unit="mg", dosing_interval=24, default_value=5)) %>%
  metadata(target(min=4, max=8)) %>% 
  mpc(theta=c(TVCL=3.7, TVV1=61), suffix="_next")
usethis::use_data(default_alloWT_MPC, overwrite=TRUE)


#________________________________________________________________________________________
#----                     Model: D7_AUC_2cpt_Tlag_CYP3A5_alloWT                      ----
#________________________________________________________________________________________

populationParameters <- "parameter,value,se_sa,rse_sa,pvalue_sa
Tlag_pop,0.287576060336467,0.0294240188688738,10.2317344616403,
ka_pop,1.7752875182154,0.317880614766599,17.9058665993522,
Cl_pop,0.0240068493206146,0.000388924978411163,1.62005839757237,
beta_Cl_CYP3A5_2,0.325552162617426,0.0385603125855108,11.8445880609385,<2.2e-16
beta_Cl_allo_WT,0.75,,,
V1_pop,0.214514498353131,0.0199219616207925,9.28700007399836,
beta_V1_allo_WT,1,,,
Q_pop,0.0871967843605594,0.00859450530650489,9.85644753935712,
beta_Q_allo_WT,0.75,,,
V2_pop,0.416644609384249,0.0475150113604173,11.4042064364252,
beta_V2_allo_WT,1,,,
F_pop,1,,,
omega_Tlag,0.614205727288474,0.0893784206646036,14.5518702762967,
omega_ka,1.06884424833521,0.144152018832791,13.486718860799,
omega_Cl,0.428158333328364,0.0326506400858816,7.62583314262884,
omega_V1,0.380154327035001,0.0525499210073657,13.8233126049694,
omega_Q,0.394276490571021,0.0904715373427836,22.9462165526927,
omega_V2,0.25591227559465,0.0972711628460751,38.0095728585318,
omega_F,0.456255314817847,0.0375146861501613,8.2223011835245,
corr1_F_Cl,-0.999,0.00131901306663682,0.132033340003686,
a,0.091576598147651,0.0980750674290549,107.096211710033,
b,0.104066412486623,0.00850598873968704,8.17361580594546,"

monolixValues <-
  read.csv(text = populationParameters) %>%
  mutate(
    OMEGA=str_detect(parameter,"^omega"),
    GAMMA=str_detect(parameter, "^gamma"),
    CORR=str_detect(parameter, "^corr"),
    THETA=str_detect(parameter, "_pop$"),
    BETA=str_detect(parameter, "^beta_"),
    RE= (parameter %in% c("a", "b"))
  )

thetaValues = monolixValues %>%
  filter(BETA) %>%
  mutate(text=paste0(parameter, "=", value)) %>%
  pull(text) %>%
  paste(collapse="\n")

m1Code <- thetaValues %>% paste("
allo_WT = log( Weight / 70 );

CYP3A5Expressor=0;
if(CYP3A5 > 1) {
CYP3A5Expressor=1;
}

TVTlag = 0.28757606
TVka = 1.77528752
TVCl = 0.02400685
TVV1 = 0.21451450
TVQ = 0.08719678
TVV2 = 0.41664461
TVF = 1

TVTlag_next = exp(log(TVTlag) + eta_ID_Tlag)
TVka_next = exp(log(TVka) + eta_ID_ka)
TVCl_next = exp(log(TVCl) + eta_ID_Cl)
TVV1_next = exp(log(TVV1) + eta_ID_V1)
TVQ_next = exp(log(TVQ) + eta_ID_Q)
TVV2_next = exp(log(TVV2) + eta_ID_V2)
TVF_next = exp(log(TVF) + eta_ID_F)


Tlag = TVTlag_next
ka = TVka_next
Cl = TVCl_next * exp( beta_Cl_allo_WT*allo_WT + beta_Cl_CYP3A5_2*CYP3A5Expressor)
V1 = TVV1_next * exp(beta_V1_allo_WT*allo_WT)
Q = TVQ_next * exp(beta_Q_allo_WT*allo_WT)
V2 = TVV2_next * exp(beta_V2_allo_WT*allo_WT)
F = TVF_next

V = V1/F
k = Cl/V
k12 = Q/V
k21 = Q/(V2/F)

d/dt(A0) = -ka*A0;
alag(A0) = Tlag;
d/dt(A1) = ka*A0 - k*A1 - k12*A1 +k21*A2;
d/dt(A2) = k12*A1 - k21*A2;

Cwb = A1 / V;")
message("Loading OMEGA matrix")
omegaMonolix <- monolixValues %>% filter(OMEGA | GAMMA) %>% mutate(
  parameterName = substr(parameter, 7, 999),
  etaName = ifelse(OMEGA,
                   paste0("eta_ID_", parameterName),
                   paste0("eta_OCC_", parameterName)
  )
)
omega <- diag( omegaMonolix$value**2 )
colnames(omega) <- omegaMonolix$etaName
rownames(omega) <- omegaMonolix$etaName

message("Loading Correlations")
corrMonolix <- monolixValues %>% filter(CORR) %>%
  tidyr::extract(parameter, regex="^corr(\\d*)_(.*)_(.*)$", into=c("i", "par1", "par2"))
for(i in seq_len(nrow(corrMonolix))) {
  par1 <- omegaMonolix %>% filter(parameterName == corrMonolix$par1[i] ) %>%
    filter(row_number() == corrMonolix$i[i] )
  par2 <- omegaMonolix %>% filter(parameterName == corrMonolix$par2[i] ) %>%
    filter(row_number() == corrMonolix$i[i] )
  covValue <- corrMonolix$value[i] * par1$value * par2$value
  omega[ par1$etaName, par2$etaName ] <- covValue
  omega[ par2$etaName, par1$etaName ] <- covValue
}

iov = omegaMonolix %>% filter(GAMMA) %>% pull(etaName)
if(length(iov)==0) iov <- NULL

thetaDf <- monolixValues %>% dplyr::filter(THETA)
theta <- thetaDf$value
names(theta) <- paste0("TV", thetaDf$parameter %>% str_match("(.*)_pop$") %>% .[,2])

D7_AUC_2cpt_Tlag_CYP3A5_alloWT <- RxODE::RxODE(m1Code) %>% tdmore(
  parameters=rownames(omega),
  omega=omega,
  iov=omegaMonolix %>% dplyr::filter(OMEGA | GAMMA) %>% pull(etaName),
  res_var=list( errorModel(var="Cwb",
                           add=monolixValues %>% dplyr::filter(parameter=="a") %>% pull(value),
                           prop=monolixValues %>% dplyr::filter(parameter=="b") %>% pull(value)) )
) %>% metadata(covariate("CYP3A5", label="CYP3A5 expressor", choices=list(Fast=0, Slow=1))) %>%
  metadata(output(name="CONC", label="Tacrolimus concentration", unit="ng/mL", default_value=5)) %>%
  metadata(dose(unit="mg", dosing_interval=12, default_value=8)) %>%
  metadata(target(min=10, max=15))
usethis::use_data(D7_AUC_2cpt_Tlag_CYP3A5_alloWT, overwrite=TRUE)

#________________________________________________________________________________________
#----                    Model: D7_AUC_2cpt_Tlag_CYP3A5_alloWT_MPC                   ----
#________________________________________________________________________________________


populationParameters <- "parameter,value,se_sa,rse_sa,pvalue_sa
Tlag_pop,0.287576060336467,0.0294240188688738,10.2317344616403,
ka_pop,1.7752875182154,0.317880614766599,17.9058665993522,
Cl_pop,0.0240068493206146,0.000388924978411163,1.62005839757237,
beta_Cl_CYP3A5_2,0.325552162617426,0.0385603125855108,11.8445880609385,<2.2e-16
beta_Cl_allo_WT,0.75,,,
V1_pop,0.214514498353131,0.0199219616207925,9.28700007399836,
beta_V1_allo_WT,1,,,
Q_pop,0.0871967843605594,0.00859450530650489,9.85644753935712,
beta_Q_allo_WT,0.75,,,
V2_pop,0.416644609384249,0.0475150113604173,11.4042064364252,
beta_V2_allo_WT,1,,,
F_pop,1,,,
omega_Tlag,0.614205727288474,0.0893784206646036,14.5518702762967,
omega_ka,1.06884424833521,0.144152018832791,13.486718860799,
omega_Cl,0.428158333328364,0.0326506400858816,7.62583314262884,
omega_V1,0.380154327035001,0.0525499210073657,13.8233126049694,
omega_Q,0.394276490571021,0.0904715373427836,22.9462165526927,
omega_V2,0.25591227559465,0.0972711628460751,38.0095728585318,
omega_F,0.456255314817847,0.0375146861501613,8.2223011835245,
corr1_F_Cl,-0.999,0.00131901306663682,0.132033340003686,
a,0.091576598147651,0.0980750674290549,107.096211710033,
b,0.104066412486623,0.00850598873968704,8.17361580594546,"

monolixValues <-
  read.csv(text = populationParameters) %>%
  mutate(
    OMEGA=str_detect(parameter,"^omega"),
    GAMMA=str_detect(parameter, "^gamma"),
    CORR=str_detect(parameter, "^corr"),
    THETA=str_detect(parameter, "_pop$"),
    BETA=str_detect(parameter, "^beta_"),
    RE= (parameter %in% c("a", "b"))
  )

thetaValues = monolixValues %>%
  filter(BETA) %>%
  mutate(text=paste0(parameter, "=", value)) %>%
  pull(text) %>%
  paste(collapse="\n")

m1Code <- thetaValues %>% paste("
allo_WT = log( Weight / 70 );

CYP3A5Expressor=0;
if(CYP3A5 > 1) {
CYP3A5Expressor=1;
}


TVTlag_next = exp(log(TVTlag) + eta_ID_Tlag)
TVka_next = exp(log(TVka) + eta_ID_ka)
TVCl_next = exp(log(TVCl) + eta_ID_Cl)
TVV1_next = exp(log(TVV1) + eta_ID_V1)
TVQ_next = exp(log(TVQ) + eta_ID_Q)
TVV2_next = exp(log(TVV2) + eta_ID_V2)
TVF_next = exp(log(TVF) + eta_ID_F)


Tlag = TVTlag_next
ka = TVka_next
Cl = TVCl_next * exp( beta_Cl_allo_WT*allo_WT + beta_Cl_CYP3A5_2*CYP3A5Expressor)
V1 = TVV1_next * exp(beta_V1_allo_WT*allo_WT)
Q = TVQ_next * exp(beta_Q_allo_WT*allo_WT)
V2 = TVV2_next * exp(beta_V2_allo_WT*allo_WT)
F = TVF_next

V = V1/F
k = Cl/V
k12 = Q/V
k21 = Q/(V2/F)

d/dt(A0) = -ka*A0;
alag(A0) = Tlag;
d/dt(A1) = ka*A0 - k*A1 - k12*A1 +k21*A2;
d/dt(A2) = k12*A1 - k21*A2;

Cwb = A1 / V;")
message("Loading OMEGA matrix")
omegaMonolix <- monolixValues %>% filter(OMEGA | GAMMA) %>% mutate(
  parameterName = substr(parameter, 7, 999),
  etaName = ifelse(OMEGA,
                   paste0("eta_ID_", parameterName),
                   paste0("eta_OCC_", parameterName)
  )
)
omega <- diag( omegaMonolix$value**2 )
colnames(omega) <- omegaMonolix$etaName
rownames(omega) <- omegaMonolix$etaName

message("Loading Correlations")
corrMonolix <- monolixValues %>% filter(CORR) %>%
  tidyr::extract(parameter, regex="^corr(\\d*)_(.*)_(.*)$", into=c("i", "par1", "par2"))
for(i in seq_len(nrow(corrMonolix))) {
  par1 <- omegaMonolix %>% filter(parameterName == corrMonolix$par1[i] ) %>%
    filter(row_number() == corrMonolix$i[i] )
  par2 <- omegaMonolix %>% filter(parameterName == corrMonolix$par2[i] ) %>%
    filter(row_number() == corrMonolix$i[i] )
  covValue <- corrMonolix$value[i] * par1$value * par2$value
  omega[ par1$etaName, par2$etaName ] <- covValue
  omega[ par2$etaName, par1$etaName ] <- covValue
}

iov = omegaMonolix %>% filter(GAMMA) %>% pull(etaName)
if(length(iov)==0) iov <- NULL

thetaDf <- monolixValues %>% dplyr::filter(THETA)
theta <- thetaDf$value
names(theta) <- paste0("TV", thetaDf$parameter %>% str_match("(.*)_pop$") %>% .[,2])

tacro_2cpt <- RxODE::RxODE(m1Code) %>% tdmore(
  parameters=rownames(omega),
  omega=omega,
  iov=omegaMonolix %>% dplyr::filter(OMEGA | GAMMA) %>% pull(etaName),
  res_var=list( errorModel(var="Cwb",
                           add=monolixValues %>% dplyr::filter(parameter=="a") %>% pull(value),
                           prop=monolixValues %>% dplyr::filter(parameter=="b") %>% pull(value)) )
) %>% metadata(covariate("CYP3A5", label="CYP3A5 expressor", choices=list(Fast=0, Slow=1))) %>%
  metadata(output(name="CONC", label="Tacrolimus concentration", unit="ng/mL", default_value=5)) %>%
  metadata(dose(unit="mg", dosing_interval=12, default_value=8)) %>%
  metadata(target(min=10, max=15))
D7_AUC_2cpt_Tlag_CYP3A5_alloWT_MPC <- tacro_2cpt %>% mpc(theta=theta, suffix="_next")
usethis::use_data(D7_AUC_2cpt_Tlag_CYP3A5_alloWT_MPC, overwrite=TRUE)