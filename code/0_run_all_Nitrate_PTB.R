## Nitrate in drinking water and spontaneous preterm birth:## 
## A within-mother retrospective analysis in California ##

  # Author: Allison Sherris, PhD Candidate, E-IPER, Stanford
  # Co-authors: Gary Shaw, Wei Yang, Mike Baiocchi, Scott Fendorf, Steve Luby
  # Start date: June 23, 2019
  # Latest version date: February 4, 2021

  # Project summary: 
    # Estimate tap water during pregnancy for California sibling births, 2000-2011, and
    # define associations between nitrate in drinking water and risk of preterm birth
    # using a within-mother design and conditional logistic regression.
    # Code is customizable to other contaminants and outcomes of interest. 

### Configure: loads libraries and universal objects and functions

  source('code/0_config.R')

### 1. Import clean data 
  
  source("code/1_data_import/1_import_raw_data.R")  
  
### 2. Exposure assessment 
  
  ## 2.1 Load functions to link births to public water systems based on maternal residence
     source("code/2_exposure_assessment/1_assign_maternal_PWS.R")
  
  ## 2.2 Load functions to assign gestational water quality based on public water systems
     source("code/2_exposure_assessment/2_assign_gestational_exposure.R")
  
  ## 2.3 Run functions to link births to public water systems 
     source("code/2_exposure_assessment/3_run_maternal_PWS.R")
    
  ## 2.4 Run functions to assign gestational water quality 
     source("code/2_exposure_assessment/4_run_gestational_exposure.R")

### 3. Analysis
    
  ## 3.1 Load functions to run models and generate results tables
    
    # Conditional logistic regression models for sibling analysis
      source("code/3_analysis/1_define_model_fxns/1_clogit_fxns.R")
  
    # Mixed models for individual-level analysis
      source("code/3_analysis/1_define_model_fxns/2_glmer_fxns.R")
    
    # Clean model output to generate results tables
      source("code/3_analysis/1_define_model_fxns/3_results_table_fxns.R")
    
  ## 3.2 Prepare data for analysis
    
    # Generate sample for individual-level analysis
      source("code/3_analysis/2_prep_data_for_analysis/1_individual_sample.R")
  
    # Generate sample for sibling analysis
      source("code/3_analysis/2_prep_data_for_analysis/2_sibling_sample.R")
  
  ## 3.3 Primary analysis
  
    source("code/3_analysis/3_primary_analysis/sibling_analysis.R")
  
  ## 3.4 Secondary analysis
    
    # Siblings within the same water system
      source("code/3_analysis/4_secondary_analysis/1_siblings_same_pws.R")    
  
    # Individual-level (case-control) analysis
      source("code/3_analysis/4_secondary_analysis/2_case_control.R")
  
    # Primary and secondary models with continuous exposure
      source("code/3_analysis/4_secondary_analysis/3_continuous_exposure.R")
  
  ## 3.5 Sensitivity analyses
  
    # Contact Allison Sherris at asherris@stanford.edu for sensitivity analyses code
    
### 4. Visualization
  
  # 4.1 Manuscript tables and figures
    
    # Run functions to prepare tables
      source("code/4_tables/1_manuscript/0_Table_fxns.R")
  
    # Prepare manuscript Tables 1, 2, and 3
      source("code/4_tables/1_manuscript/1_Table1.R")
      source("code/4_tables/1_manuscript/2_Table2.R")
      source("code/4_tables/1_manuscript/3_Table3.R")
  
  # 4.2 Supplement tables and figures
  
    # Contact Allison Sherris at asherris@stanford.edu for ensitivity analyses code
  
    
# End