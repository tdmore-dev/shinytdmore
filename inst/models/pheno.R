# This model was copied verbatim from the nlmixr guide,
# and is reproduced here as an example model
# for use with tdmore.
# 
# The nlmixr package is licensed under GPLv2, 
# 
# See https://nlmixrdevelopment.github.io/nlmixr/articles/addingCovariances.html
pheno <- function() {
  ini({
    tcl <- log(0.008) # typical value of clearance
    tv <-  log(0.6)   # typical value of volume
    ## var(eta.cl)
    eta.cl + eta.v ~ c(1, 
                       0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
    # interindividual variability on clearance and volume
    add.err <- 0.1    # residual variability
  })
  model({
    cl <- exp(tcl + eta.cl) # individual value of clearance
    v <- exp(tv + eta.v)    # individual value of volume
    ke <- cl / v            # elimination rate constant
    d/dt(A1) = - ke * A1    # model differential equation
    cp = A1 / v             # concentration in plasma
    cp ~ add(add.err)       # define error model
  })
}

nlmixr::nlmixr(pheno) %>% tdmore::tdmore() %>%
  tdmore::metadata(
    tdmore::output(name="cp", label="Concentration", unit="mg/L"),
                   tdmore::formulation(name="IV", unit="mg", default_value=130, dosing_interval=8),
                   tdmore::formulation(name="intramusc", unit="mg", default_value=65, dosing_interval=8),
                   tdmore::formulation(name="per os", unit="mg", default_value=15, dosing_interval=24)
                   )
