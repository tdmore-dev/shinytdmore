#' Base model of Bergmann 2014
#' 2-compartment model with oral absorption and lag time.
#' CYP3A5 effect on CL (CYP3A5=1 for homozygote and heterozygote, CYP3A5=0 for non-expressors)
#' Prograf administration (Janssen-Cilag, MacQuarie Park, Australia)
#' 
#' Based on data from two studies in Brisbane, Australia.
#' <quote>
#' In the first study, a full concentration–time profile characterizing 
#' tacrolimus and prednisolone therapy was collected from 20 patients 
#' with blood samples taken predose and 0.25, 0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 6, 9,
#'  and 12 hours postdose. Half of the patients were sampled in their first posttransplant week, 
#'  and the other half were >90 days posttransplantation 
#'  at the time of sampling (range: 90 days to 6.5 years). 
#' In the second study, a limited concentration–time profile characterizing tacrolimus 
#' and prednisolone therapy was collected from 153 patients with blood samples taken 
#' predose and 1, 2, and 4 hours postdose on at least one occasion between day 4 and 
#' 12 months posttransplantation.
#' </quote>
#' 
#' Bergmann, Troels K., et al. "Population pharmacokinetics of tacrolimus in adult kidney transplant patients: impact of CYP3A5 genotype on starting dose." Therapeutic drug monitoring 36.1 (2014): 62-70.
#' 
#' @format RxODE model
"bergmann2014_base"



#' Full model of Bergmann 2014
#' 2-compartment model with oral absorption and lag time.
#' CYP3A5 effect on CL (CYP3A5=1 for homozygote and heterozygote, CYP3A5=0 for non-expressors)
#' 
#' 
#' Based on data from two studies.
#' <quote>
#' In the first study, a full concentration–time profile characterizing 
#' tacrolimus and prednisolone therapy was collected from 20 patients 
#' with blood samples taken predose and 0.25, 0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 6, 9,
#'  and 12 hours postdose. Half of the patients were sampled in their first posttransplant week, 
#'  and the other half were >90 days posttransplantation 
#'  at the time of sampling (range: 90 days to 6.5 years). 
#' In the second study, a limited concentration–time profile characterizing tacrolimus 
#' and prednisolone therapy was collected from 153 patients with blood samples taken 
#' predose and 1, 2, and 4 hours postdose on at least one occasion between day 4 and 
#' 12 months posttransplantation.
#' </quote>
#' 
#' Bergmann, Troels K., et al. "Population pharmacokinetics of tacrolimus in adult kidney transplant patients: impact of CYP3A5 genotype on starting dose." Therapeutic drug monitoring 36.1 (2014): 62-70.
#' 
#' @format RxODE model
"bergmann2014_full"

#' Tacrolimus kidney model.
#'
#' @format RxODE model
"tacrolimuskidney"

#' Temporary model.
#' Will be deleted.
#'
#' @format RxODE model
"bergmann2014_to_delete"

