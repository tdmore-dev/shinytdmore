library(tdmore)
library(nlmixr)

nlmixrUI(function(){
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