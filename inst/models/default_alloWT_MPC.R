library(tdmore)
library(shinytdmore)
library(nlmixr)

nlmixrUI(function(){
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
  metadata(covariate(name="WT", label="Weight", unit="kg", min=20, max=150)) %>%
  metadata(output(name="CONC", label="Drug concentration", unit="mg/mL", default_value=5)) %>%
  metadata(formulation(name="Default",unit="mg", dosing_interval=24, default_value=5)) %>%
  metadata(target(min=4, max=8)) %>% 
  mpc(theta=c(TVCL=3.7, TVV1=61), suffix="_next") %>%
  metadata(observed_variables(c("CL", "V1")))