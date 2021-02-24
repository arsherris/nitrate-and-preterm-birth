# nitrate-and-preterm-birth
##  Gestational exposure to nitrate in drinking water and preterm birth:  
A retrospective within-mother analysis

 Authors: Allison R Sherris, Mike Baiocchi, Scott Fendorf, Steve Luby, Wei Yang, Gary Shaw  
 **Currently in review / draft form**  
 Contact asherris@stanford.edu with questions and for details on data access 

## Project summary 
 Estimate tap water during pregnancy for California sibling births, 2000-2011, and define associations between nitrate in drinking water and risk of preterm birth using a within-mother design and conditional logistic regression. Code is customizable to other contaminants and outcomes of interest. 

## Data sources

## Code

*code/0_config.R* loads libraries and universal objects and functions

1. Import data  
* *code/1_data_import/1_import_raw_data.R* Imports data    
2. Clean data  
* *code/2_data_cleaning/1_clean_pws_data.R* Loads functions to clean public water system data  
* *code/2_data_cleaning/2_clean_wq_data.R* Loads functions to clean water quality data  
* *code/2_data_cleaning/3_clean_birth_data.R* Loads functions to clean births data
* *code/2_data_cleaning/4_inclusion_criteria.R* Loads functions to restrict births to study population
* *code/2_data_cleaning/5_run_data_cleaning.R* Runs functions to clean data and restrict to study population
3. Exposure assessment 
* *code/3_exposure_assessment/1_assign_maternal_PWS.R* Loads functions to link births to public water systems based on maternal residence
    
  
  ## 3.2 Load functions to assign gestational water quality based on public water systems
     source("code/3_exposure_assessment/2_assign_gestational_exposure.R")
  
  ## 3.3 Run functions to link births to public water systems 
     source("code/3_exposure_assessment/3_run_maternal_PWS.R")
    
  ## 3.4 Run functions to assign gestational water quality 
     source("code/3_exposure_assessment/4_run_gestational_exposure.R")

### 4. Analysis
    
  ## 4.1 Load functions to run models and generate results tables
    
    # Conditional logistic regression models for sibling analysis
      source("code/4_analysis/1_define_model_fxns/1_clogit_fxns.R")
  
    # Mixed models for individual-level analysis
      source("code/4_analysis/1_define_model_fxns/2_glmer_fxns.R")
    
    # Clean model output to generate results tables
      source("code/4_analysis/1_define_model_fxns/3_results_table_fxns.R")
    
  ## 4.2 Prepare data for analysis
    
    # Generate sample for individual-level analysis
      source("code/4_analysis/2_prep_data_for_analysis/1_individual_sample.R")
  
    # Generate sample for sibling analysis
      source("code/4_analysis/2_prep_data_for_analysis/2_sibling_sample.R")
  
  ## 4.3 Primary analysis
  
    source("code/4_analysis/3_primary_analysis/sibling_analysis.R")
  
  ## 4.4 Secondary analysis
    
    # Siblings within the same water system
      source("code/4_analysis/4_secondary_analysis/1_siblings_same_pws.R")    
  
    # Individual-level (case-control) analysis
      source("code/4_analysis/4_secondary_analysis/2_case_control.R")
  
    # Primary and secondary models with continuous exposure
      source("code/4_analysis/4_secondary_analysis/3_continuous_exposure.R")
  
  ## 4.5 Sensitivity analyses
  
    # Contact Allison Sherris at asherris@stanford.edu for sensitivity analyses code
    
### 5. Visualization
  
  # 5.1 Manuscript tables and figures
    
    # Run functions to prepare tables
      source("code/5_visualization/1_manuscript/0_Table_fxns.R")
  
    # Prepare manuscript Tables 1, 2, and 3
      source("code/5_visualization/1_manuscript/1_Table1.R")
      source("code/5_visualization/1_manuscript/2_Table2.R")
      source("code/5_visualization/1_manuscript/3_Table3.R")
  
    # Prepare inclusion/exclusion numbers for Figure 1
      source("code/5_visualization/1_manuscript/4_Fig1_inclusion_flowchart.R")
  
  # 5.2 Supplement tables and figures
  
    # Contact Allison Sherris at asherris@stanford.edu for sensitivity analyses code
  
    
# End
