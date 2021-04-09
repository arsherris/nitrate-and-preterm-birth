# nitrate-and-preterm-birth
##  Gestational exposure to nitrate in drinking water and preterm birth:  A retrospective within-mother analysis

 Authors: Allison R Sherris, Mike Baiocchi, Scott Fendorf, Steve Luby, Wei Yang, Gary Shaw  
 **Currently in press at Environmental Health Perspectives**  
 
 For comments, questions, concerns regarding code: Allison Sherris, asherris@stanford.edu

## Project summary 
This study estimated exposure to nitrate in tap water during pregnancy for a large cohort of California births from 2000-2011. The primary analysis uses a within-mother design to evaluate associations with nitrate in drinking water and odds of spontaneous preterm preterm. Code is customizable to other contaminants and outcomes of interest. 

## Data sources
**Nitrate monitoring records for public water systems**: State Water Resources Control Board, EDT Library and Water Quality Analyses Data and Download Page. <https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/EDTlibrary.html> Downloaded November 22, 2019.

**Community water system service area boundaries**: Tracking California, Water Boundary Tool. <https://trackingcalifornia.org/water-boundary-tool/water-boundary-tool-landing/> Downloaded May 19, 2019.

**Community water system flow paths**: Accessed by request from Paul Williams, State Water Resources Control Board (Paul.Williams@waterboards.ca.gov), August 18, 2020. 

**Birth certificate and hospital discharge records**: The data employed in this study were made available by California OSHPD and Vital Records. Use of such data is possible to researchers who apply for their use and follow all procedures for their use as stipulated by IRB protocols, including stringent measures to ensure participant confidentiality and privacy. 

Contact Allison Sherris, asherris@stanford.edu, for details and code to produce cleaned datasets.

## Code

**Execute project:**
*code/run_all_Nitrate_PTB.R* describes and executes all scripts

**Individual scripts:**
0. Configure workspace
* *code/0_config.R* loads libraries and universal objects and functions

1. Import data  
* *code/1_data_import/1_import_raw_data.R* Imports data    

2. Exposure assessment 
* *code/2_exposure_assessment/1_assign_maternal_PWS.R*   Loads functions to link births to public water systems based on maternal residence
* *code/2_exposure_assessment/2_assign_gestational_exposure.R* Loads functions to assign gestational water quality based on public water systems
* *code/2_exposure_assessment/3_Runs_maternal_PWS.R* Runs functions to link births to public water systems 
* *code/2_exposure_assessment/4_Runs_gestational_exposure.R* Runs functions to assign gestational water quality 

3. Analysis  
3.1 Functions to run models and generate results tables   
* *code/3_analysis/1_define_model_fxns/1_clogit_fxns.R* Functions for conditional logistic regression models for sibling analysis
* *code/3_analysis/1_define_model_fxns/2_glmer_fxns.R* Functions for mixed models for individual-level analysis
* *code/3_analysis/1_define_model_fxns/3_results_table_fxns.R* Functions to clean model output and generate results tables  
3.2 Prepare data for analysis  
* *code/3_analysis/2_prep_data_for_analysis/1_individual_sample.R* Generates sample for individual-level analysis  
* *code/3_analysis/2_prep_data_for_analysis/2_sibling_sample.R* Generates sample for sibling analysis  
3.3 Primary analysis  
* *code/3_analysis/3_primary_analysis/sibling_analysis.R* Runs sibling-matched analysis  
3.4 Secondary analyses  
* *code/3_analysis/4_secondary_analysis/1_siblings_same_pws.R* Secondary sibling-matched analysis: siblings within the same water system  
* *code/3_analysis/4_secondary_analysis/2_case_control.R* Secondary individual-level (case-control) analysis  
* *code/3_analysis/4_secondary_analysis/3_continuous_exposure.R* Primary and secondary models with continuous exposure  

4. Visualization: Produces manuscript Tables 1-3 
        
Contact Allison Sherris at asherris@stanford.edu for code to run sensitivity analyses and supplementary tables/figures
  
# End
