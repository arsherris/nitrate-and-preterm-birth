# nitrate-and-preterm-birth
##  Gestational exposure to nitrate in drinking water and preterm birth:  
A retrospective within-mother analysis

 Authors: Allison R Sherris, Mike Baiocchi, Scott Fendorf, Steve Luby, Wei Yang, Gary Shaw  
 **Currently in review / draft form**  
 Contact asherris@stanford.edu with questions and for details on data access 

## Project summary 
 Estimate tap water during pregnancy for California sibling births, 2000-2011, and define associations between nitrate in drinking water and risk of preterm birth using a within-mother design and conditional logistic regression. Code is customizable to other contaminants and outcomes of interest. 

## Data sources
TBD

## Code

**Execute project:**
*code/run_all_Nitrate_PTB.R* describes and executes all scripts

0. Configure workspace
* *code/0_config.R* loads libraries and universal objects and functions

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
* *code/3_exposure_assessment/2_assign_gestational_exposure.R* Loads functions to assign gestational water quality based on public water systems
* *code/3_exposure_assessment/3_Runs_maternal_PWS.R* Runs functions to link births to public water systems 
* *code/3_exposure_assessment/4_Runs_gestational_exposure.R* Runs functions to assign gestational water quality 

4. Analysis
4.1 Functions to run models and generate results tables
* *code/4_analysis/1_define_model_fxns/1_clogit_fxns.R* Functions for conditional logistic regression models for sibling analysis
* *code/4_analysis/1_define_model_fxns/2_glmer_fxns.R* Functions for mixed models for individual-level analysis
* *code/4_analysis/1_define_model_fxns/3_results_table_fxns.R* Functions to clean model output and generate results tables
4.2 Prepare data for analysis
* *code/4_analysis/2_prep_data_for_analysis/1_individual_sample.R* Generates sample for individual-level analysis
* *code/4_analysis/2_prep_data_for_analysis/2_sibling_sample.R* Generates sample for sibling analysis
4.3 Primary analysis
* *code/4_analysis/3_primary_analysis/sibling_analysis.R* Runs sibling-matched analysis
4.4 Secondary analyses
* *code/4_analysis/4_secondary_analysis/1_siblings_same_pws.R* Secondary sibling-matched analysis: siblings within the same water system
* *code/4_analysis/4_secondary_analysis/2_case_control.R* Secondary individual-level (case-control) analysis
* *code/4_analysis/4_secondary_analysis/3_continuous_exposure.R* Primary and secondary models with continuous exposure
      
Contact Allison Sherris at asherris@stanford.edu for sensitivity analyses code
  
    
# End
