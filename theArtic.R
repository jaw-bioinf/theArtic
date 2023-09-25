# intuition: Mendelian Randomization approximates randomized clinical trials by using alleles as instrumental variables.
# Mendel's 2nd law -> random assortment takes care of randomization from birth; then if the alleles/SNPs associate only
# with an intervention and not an outcome, it provides evidence for causation. This is cool, and I use it a lot in my work.
# The effects are of a lifetime predispostion to the exposure influencing the outcome.
# Classic example: observational studies report an inverse association between HDL cholesterol and coronary heart disease. However,
# MR studies don't show this association, and later RCTs of pharma interventions echo this. 
# These data below are from high throughput studies conducted widely but POORLY --- good for hypothesis generation but not to be 
# totally trusted since various assumptions may be violated --- but maybe something will make sense! 
# Anyway good luck -- could be food for thought, or may be boring enough to send you to dream land.
# source: https://www.epigraphdb.org/mr
# MR theory if you're bored: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7384151/ && https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7614635/ 
#### this is NOT medical advice and is an example of some scientific data wrangling for educational purposes

### Instructions:
# 1. Look at the "exposure.trait" traits
# 2. mr.b is the beta, mr.pval is the pvalue, mr.se is the standard error just FYI


dir.create("theArtic")
setwd("theArtic/")
install.packages(c("devtools","tidyverse"))
devtools::install_github("MRCIEU/epigraphdb-r")
library(epigraphdb)
library(tidyverse)
# sleep disorders as outcome --- intution small beta would be things to look at getting more of; large beta would be avoidances
sleep_disorders_try = query_epigraphdb(
  route="/mr",
  params=list(exposure_trait=NULL, outcome_trait="Diagnoses - main ICD10: G47 Sleep disorders", pval_threshold=1e-08),
  mode="table"
) %>% dplyr::filter(mr.b < 0) %>% dplyr::arrange((mr.b)) %>% dplyr::filter(!grepl("sleep disorders", exposure.trait,ignore.case = TRUE))

# sleep duration as exposures --- big betas good here
sleep_duration_try = query_epigraphdb(
  route="/mr",
  params=list(exposure_trait=NULL, outcome_trait="Sleep duration", pval_threshold=1e-08),
  mode="table"
) %>% dplyr::filter(mr.b > 0) %>% dplyr::arrange((mr.b)) %>% dplyr::filter(!grepl("sleep duration", exposure.trait,ignore.case = TRUE))



# chronotype as exposure; specifically evening chronotype so since you're morning we want to do the inverse, so small betas good here

chronotype_try = query_epigraphdb(
  route="/mr",
  params=list(exposure_trait=NULL, outcome_trait="Morning/evening person (chronotype)", pval_threshold=1e-08),
  mode="table"
) %>% dplyr::filter(mr.b < 0) %>% dplyr::arrange((mr.b)) %>% dplyr::filter(!grepl("chronotype", exposure.trait,ignore.case = TRUE))



# getting up in the morning -- I guess large betas good?

get_up_morning_try = query_epigraphdb(
  route="/mr",
  params=list(exposure_trait=NULL, outcome_trait="Getting up in morning", pval_threshold=1e-08),
  mode="table"
) %>% dplyr::filter(mr.b > 0) %>% dplyr::arrange(desc(mr.b)) %>% dplyr::filter(!grepl("morning", exposure.trait,ignore.case = TRUE))


#### and the reverse for things to avoid

sleep_disorders_avoid = query_epigraphdb(
  route="/mr",
  params=list(exposure_trait=NULL, outcome_trait="Diagnoses - main ICD10: G47 Sleep disorders", pval_threshold=1e-08),
  mode="table"
) %>% dplyr::filter(mr.b > 0) %>% dplyr::arrange((mr.b)) %>% dplyr::filter(!grepl("sleep disorders", exposure.trait,ignore.case = TRUE))

sleep_duration_avoid = query_epigraphdb(
  route="/mr",
  params=list(exposure_trait=NULL, outcome_trait="Sleep duration", pval_threshold=1e-08),
  mode="table"
) %>% dplyr::filter(mr.b < 0) %>% dplyr::arrange((mr.b)) %>% dplyr::filter(!grepl("sleep duration", exposure.trait,ignore.case = TRUE))


chronotype_avoid = query_epigraphdb(
  route="/mr",
  params=list(exposure_trait=NULL, outcome_trait="Morning/evening person (chronotype)", pval_threshold=1e-08),
  mode="table"
) %>% dplyr::filter(mr.b > 0) %>% dplyr::arrange((mr.b)) %>% dplyr::filter(!grepl("chronotype", exposure.trait,ignore.case = TRUE))


get_up_morning_avoid = query_epigraphdb(
  route="/mr",
  params=list(exposure_trait=NULL, outcome_trait="Getting up in morning", pval_threshold=1e-08),
  mode="table"
) %>% dplyr::filter(mr.b < 0) %>% dplyr::arrange(desc(mr.b)) %>% dplyr::filter(!grepl("morning", exposure.trait,ignore.case = TRUE))

avoid_df <- grep("avoid",names(.GlobalEnv),value=TRUE)
avoid_df <- do.call(rbind,do.call("list",mget(avoid_df)))
avoid_df <- avoid_df %>% dplyr::select(exposure.trait,mr.b,mr.se,mr.pval)

try_df <- grep("try",names(.GlobalEnv),value=TRUE)
try_df <- do.call(rbind,do.call("list",mget(try_df)))
try_df <- try_df %>% dplyr::select(exposure.trait,mr.b,mr.se,mr.pval)

write.table(try_df,"to_try.tsv",sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(avoid_df,"to_avoid.tsv",sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
