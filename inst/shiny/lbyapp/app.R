library(shiny)
library(ruODK)
library(hash)
library(psych)
library(dplyr )

##Change this as soon as format_timci_data.R is in the newest package
source("R/format_data.R")

server <- function(input, output) {

  #######################################
  # ODK CENTRAL SERVER CONNECTION SETUP #
  #######################################
# Specify the path to your environmental variable file
  current_directory <- getwd()
  env_file_name <- "inst/shiny/lbyapp/.env"
  env_file <- file.path(current_directory, env_file_name)

  # Read the environmental variables from the file
  env_vars <- readLines(env_file)

  # Set the environmental variables
  for (env_var_line in env_vars) {
    parts <- strsplit(env_var_line, "=")
    if (length(parts[[1]]) == 2) {
      var_name <- parts[[1]][1]
      var_value <- parts[[1]][2]
      Sys.setenv(var_name = var_value)
    } else {
      cat("Skipping line:", env_var_line, "\n")
    }
  }
  print(Sys.getenv())
  print(Sys.getenv("ODKC_SVC"))
  print(Sys.getenv("ODKC_UN"))
  print(Sys.getenv("ODKC_PW"))
  print(Sys.getenv("TZ"))

  # Connect to the ODK Central server using ruODK
  ruODK::ru_setup(
    svc = Sys.getenv("ODKC_SVC"),
    un = Sys.getenv("ODKC_UN"),
    pw = Sys.getenv("ODKC_PW"),
    tz = Sys.getenv("TZ"),
    verbose = TRUE # Can be switched to TRUE for demo or debugging
  )
  
  ################
  # RAW ODK DATA #
  ################

  # Load ODK forms that have at least 1 submission
  odk_form_list <- ruODK::form_list()
  valid_odk_forms <- subset(odk_form_list, submissions > 0, select = c(fid))
  valid_odk_form_list <- valid_odk_forms$fid

  # Load ODK data
  odk_data <- hash::hash()
  for (form in valid_odk_form_list) {
    odk_data[[form]] <- ruODK::odata_submission_get(fid = form)
  }

  ## odk_data is a hash table with all the forms in the given ODK project
  ## extract only the libyan baseline questionnaire
  baseline_libya = odk_data[["baseline_questionnaire_libya_main_survey"]]
  dplyr::glimpse(baseline_libya)
  ## Drop unnecessary columns from the dataframe
  baseline_libya <- baseline_libya %>%
      dplyr::select(-c(g1_label_permission, g2_label_intro, g2_label_patient_intro,
      g3_label_start, g4_label_clinical_history, g4_label_dangersigns,
      g4_label_symptomreported_cargiver,g4_label_symptomchecked_provider,
      g4_label_physical_exam, g4_label_other_clinical_assessment, 
      g4_label_labtests, g4_label_counseling, g4_label_counseling, g4_label_test_outcome, 
      g4_label_diagnose, g4_label_treatment, g4_label_end ))

  ## Convert the submission_date from string to an actual Date
  baseline_libya$system_submission_date <- as.Date(baseline_libya$system_submission_date)

  ## Drop submissions that were done just for testing
  baseline_libya <- baseline_libya %>%
    dplyr::filter(meta_instance_id != 'uuid:aa7ca405-9b7b-4a96-91da-6c78c541cfc6' &
    meta_instance_id != 'uuid:69497c23-06be-489c-9821-dcb644cc657e' & 
    meta_instance_id != 'uuid:27a1dc99-2e0c-4f1b-81b1-a72429866612' &
    meta_instance_id != 'uuid:22bbd5d6-cc79-44d8-8700-6385de32dd31' &
    system_submitter_name != 'Dr. Alfaqeh' & baseline_libya$g2_fac == 'alsabri' &
    (system_review_state != 'rejected' | is.na(system_review_state))) #if not explicitly said that you also want NAs they will be removed

  #baseline_libya <- baseline_libya %>%
  #  mutate_at(vars(-g2_date_observation), ~ifelse(grepl("^\\d+$", .), as.numeric(.), ifelse(is.na(.), 0, ifelse(.=="", 0, .))))
  #baseline_libya <- convert_numeric_strings(baseline_libya, "g2_date_observation")
  baseline_libya <- baseline_libya %>% 
    mutate_at(vars(-g2_date_observation), ~{
    ifelse(!is.na(.) & grepl("^\\d+$", .), as.numeric(.),
           ifelse(!is.na(.) & . != "", ., 0))
  })


  baseline_libya$system_submission_date <- as.Date(baseline_libya$system_submission_date, format = "%Y-%m-%d %H:%M:%S")
  baseline_libya <- baseline_libya[order(baseline_libya$system_submission_date), ]

  #baseline_libya <- baseline_libya %>%
  #  dplyr::mutate_if(is.character, convert_to_numeric_safely)
  baseline_libya <- convert_to_numeric_safely(baseline_libya,  skip_columns = c("g2_date_observation"))
  
  
  print("-----------------")
  print("Creating permission table")
  baseline_libya$g1_permission_granted <- factor(baseline_libya$g1_permission_granted, levels = c(0, 1, 2),
                                 labels = c("No consent", "Yes, oral", "Yes, signed"))

  print('--------------------------------')
  

  permission_table <- create_basic_table(baseline_libya, 
    baseline_libya$g1_permission_granted,"pre almanach")
  print("Creating facility table")
  print(baseline_libya$g1_permission_granted)
  baseline_libya <- baseline_libya[baseline_libya$g1_permission_granted == "Yes, oral" | 
    baseline_libya$g1_permission_granted == "Yes, signed", ]

  baseline_libya$g2_fac <- ifelse(baseline_libya$g2_fac == "sk", 1, baseline_libya$g2_fac)

  baseline_libya$g2_fac <- factor(baseline_libya$g2_fac, levels = c(1),
                                 labels = c("Suleiman Khater"))
    
  fac_table <- create_basic_table(baseline_libya, baseline_libya$g2_fac,"pre almanach")
  print('---------------------------')
  print('Creating health worker category')
  baseline_libya$g2_hw <- ifelse(baseline_libya$g2_hw == 'pediatrician', 1, 
    ifelse(baseline_libya$g2_hw== 'nurse_reg', 2,
    ifelse(baseline_libya$g2_hw== 'nurse', 3, 
    ifelse(baseline_libya$g2_hw == 'midwife', 4,
    ifelse(baseline_libya$g2_hw == 'gp', 5, 99)))))
  baseline_libya$g2_hw <- factor(baseline_libya$g2_hw, levels = c(1, 2, 3, 4, 5, 99),
                                 labels = c("Pediatrician", "Registered Nurse", "Nurse", "Midwife", "General Practitioner", "Other"))
  
  hw_table <- create_basic_table(baseline_libya, baseline_libya$g2_hw,"pre almanach")
  
  print('---------------------------')
  print('Creating gender graph')
  baseline_libya$g2_p_sex <- ifelse(baseline_libya$g2_p_sex == 'female', 1, 
    ifelse(baseline_libya$g2_p_sex == 'male', 2, 99))
  baseline_libya$g2_p_sex <- factor(baseline_libya$g2_p_sex, levels = c(1, 2, 99),
                                 labels = c("Girls", "Boys", "Other"))
  
  gender_table <- create_basic_table(baseline_libya, baseline_libya$g2_p_sex,"pre almanach")
  print('---------------------------')
  print('Creating age graph')

  baseline_libya$g2_p_age <- ifelse(baseline_libya$g2_p_age == '2-11', 1, 
    ifelse(baseline_libya$g2_p_age== '12-23', 2,
    ifelse(baseline_libya$g2_p_age== '24-35', 3, 
    ifelse(baseline_libya$g2_p_age == '36-47', 4,
    ifelse(baseline_libya$g2_p_age == '48-59', 5, 99)))))
  baseline_libya$g2_p_age <- factor(baseline_libya$g2_p_age, levels = c(1, 2, 3, 4, 5, 99),
                                 labels = c("2-11 mo", "12-23 mo", "2-<3y", "3-<4y", "4-<5y", "Other"))

  age_w_months_table <- create_2way_table(baseline_libya$g2_p_age_months, baseline_libya$g2_p_age, c('Age in months','Age'))
  age_table <- create_basic_table(baseline_libya, baseline_libya$g2_p_age,"pre almanach")
  print('---------------------------')
  print('Creating muac graph')
  baseline_libya$g4_muac_color <- baseline_libya$g4_muac 

  baseline_libya$g4_muac <- ifelse(baseline_libya$g4_muac == 'none', 0, 
    ifelse(baseline_libya$g4_muac== 'red', 1,
    ifelse(baseline_libya$g4_muac== 'yellow', 2, 
    ifelse(baseline_libya$g4_muac == 'green', 3, 99))))
  baseline_libya$g4_muac <- factor(baseline_libya$g4_muac, levels = c(0, 1, 2, 3, 99),
                                 labels = c("none", "red", "yellow", "green", "Other"))
  
  muac_table <- create_basic_table(baseline_libya, baseline_libya$g4_muac,"pre almanach")
  print('---------------------------')
  print('Creating referral graph')
  baseline_libya$g4_referral <- ifelse(baseline_libya$g4_referral == 'home', 1, 
    ifelse(baseline_libya$g4_referral== 'refer_urgent', 2,
    ifelse(baseline_libya$g4_referral == 'refer_nonurgent', 3, 99)))
  baseline_libya$g4_referral <- factor(baseline_libya$g4_referral, levels = c(1, 2, 3, 99),
                                 labels = c("Child treated and sent home", "Urgent referral to hospital/ Stabilization center recommended"
                                 , "Non urgent referral for other care/ assessment recommended", "Not available"))
  
  referral_table <- create_basic_table(baseline_libya, baseline_libya$g4_referral,"pre almanach")

  print('---------------------------')
  print('Creating vaccination graph')
  baseline_libya$g4_referral_vacc_reason <- ifelse(baseline_libya$g4_referral_vacc_reason == 'complete', 1, 
    ifelse(baseline_libya$g4_referral_vacc_reason== 'vaccine_stockout', 2, 99))
  baseline_libya$g4_referral_vacc_reason <- factor(baseline_libya$g4_referral_vacc_reason, levels = c(1, 2, 99),
                                 labels = c("informed but not due/ completed", 
                                 "informed but vaccines not available",
                                  "Not available"))
  
  referral_vaccination_table <- create_basic_table(baseline_libya, baseline_libya$g4_referral_vacc_reason,"pre almanach")
  print('---------------------------')
  print('Creating Hemo results graph')
  #in ODK none and unavailable are mixed up with the labels, somebody filling in does not realize that if 
  # he checks "test not ordered" that it codes "unavailable" in the data. The recoding takes into account for this mistake
  baseline_libya$g4_hemo_result <- ifelse(baseline_libya$g4_hemo_result == 'none', 0, 
    ifelse(baseline_libya$g4_hemo_result== 'below7', 1,
    ifelse(baseline_libya$g4_hemo_result== 'between7_11', 2, 
    ifelse(baseline_libya$g4_hemo_result == 'above_11', 3,
    ifelse(baseline_libya$g4_hemo_result == 'unavailable', 4, 99)))))
  baseline_libya$g4_hemo_result <- factor(baseline_libya$g4_hemo_result, levels = c(1, 2, 3, 4, 99),
                                 labels = c("Hb <7 g/dl", 
                                 "Hb between 7-<11g/dl",
                                  "Hb>= 11g/dl",
                                  "test not ordered",
                                  "test not available"))
  
  referral_hemo_table <- create_basic_table(baseline_libya, baseline_libya$g4_hemo_result,"pre almanach")
  print('---------------------------')
  print('Creating Dehydration results graph')
  baseline_libya$g4_d_dehydration_type <- ifelse(baseline_libya$g4_d_dehydration_type == 'dehydration_no', 0, 
    ifelse(baseline_libya$g4_d_dehydration_type== 'dehydration_moderate', 1,
    ifelse(baseline_libya$g4_d_dehydration_type== 'dehydration_severe', 2, 99)))
  baseline_libya$g4_d_dehydration_type <- factor(baseline_libya$g4_d_dehydration_type, levels = c(1, 2, 3, 99),
                                 labels = c("Moderate dehydration", 
                                 "Severe dehydration",
                                  "No dehydration", "Other"))
  #new
  tests_table <- create_basic_table(baseline_libya, baseline_libya$g4_is_test,"pre almanach")
  dehydration_type_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_dehydration_type,"pre almanach")

  print('---------------------------')
  print('Creating Diarrhoea results graph')
  label_values <- list(
  list(value = 'awd', label = "Acute Watery Diarrhea/Diarrhea"),
  list(value = 'dysentery', label = "Dysentery"),
  list(value = 'parasitosis', label = "Intestinal Parasitosis"),
  list(value = 'persistent_diarr', label = "Persistent diarrhea"),
  list(value = 'other', label = "Other digestive disease")  # Add more labels as needed
  )
  baseline_libya <- recode_data(baseline_libya, "g4_d_diarrhoea_type", label_values)
  diarrhea_type_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_diarrhoea_type,"pre almanach")
  print('---------------------------')
  print('Creating malnutrition results graph')
  label_values <- list(
  list(value = 'sam', label = "Severe Acute Malnutrition (SAM)"),
  list(value = 'mam', label = "Moderate Acute Malnutrition (MAM)"),
  list(value = 'feeding_problem_or_low_weight', label = "Feeding problem or low weight for age suspected"),
  list(value = 'other', label = "Other Malnutrition problem")
  )
  baseline_libya <- recode_data(baseline_libya, "g4_d_malnutrition_type", label_values)
  malnutrition_type_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_malnutrition_type,"pre almanach")

  malnutrition_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_malnutrition,"pre almanach")

  print('---------------------------')
  print('Creating Fever results graph')
  label_values <- list(
  list(value = 'measles', label = "Measles"),
  list(value = 'fever_vsfd', label = "Very severe febrile disease"),
  list(value = 'other', label = "Other febrile disease")  # Add more labels as needed
  )
  baseline_libya <- recode_data(baseline_libya, "g4_d_fever_type", label_values)
  fever_type_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_fever_type,"pre almanach")

  print('---------------------------')
  print('Creating Ear results graph')
  label_values <- list(
  list(value = 'mastoiditis', label = "Mastoiditis"),
  list(value = 'aei', label = "Acute Ear Infection"),
  list(value = 'other', label = "Other Ear disease")  # Add more labels as needed
  )
  baseline_libya <- recode_data(baseline_libya, "g4_d_ear_type", label_values)
  ear_type_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_ear_type,"pre almanach")

  print('---------------------------')
  print('Creating throat results graph')
  print(baseline_libya$g4_d_throat_type)
  label_values <- list(
  list(value = 'viral_sore_throat', label = "Sore Throat (Viral Sore Throat)"),
  list(value = 'possible_streptococcal_sore_throat', label = "Possible Streptococcus sore throat"),
  list(value = 'streptococcal_sore_throat', label = "Streptococcus sore throat"),
  list(value = '0', label = "No Throat problem"),
  list(value = 'other', label = "Other throat disease")  # Add more labels as needed
  )
  baseline_libya <- recode_data(baseline_libya, "g4_d_throat_type", label_values)
  throat_type_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_throat_type,"pre almanach")

  print('---------------------------')
  print('Creating Respiratory results table')
  label_values <- list(
  list(value = 'severe_pneumonia_or_vsrd', label = "Severe pneumonia or Very severe respiratory disease"),
  list(value = 'suspected_vsrd', label = "Suspected very severe respiratory disease"),
  list(value = 'persistent_cough', label = "Persistent cough"),
  list(value = 'pneumonia', label = "Pneumonia"),
  list(value = 'urti', label = "Cough or cold (Upper repiratory tract infection)"),
  list(value = 'wheezing', label = "Wheezing"),
  list(value = '0', label = "No respiratory disease"),
  list(value = 'other', label = "Other respiratory disease")  # Add more labels as needed
  )
  print(baseline_libya$g4_d_respiratory_type)

  respiratory_type_values <- split_concatenated_strings(baseline_libya$g4_d_respiratory_type)
  print(respiratory_type_values)
  respiratory_type_values <- recode_column(respiratory_type_values,
                          label_values)
  print(respiratory_type_values)
  #need to change this to get the respiratory_type_values in a table
  #respiratory_type_table <- create_basic_table_col(respiratory_type_values)
  respiratory_type_table <- create_basic_table(baseline_libya, respiratory_type_values,"pre almanach")
  
  print(respiratory_type_table)
  baseline_libya$d_pneumonia <- ifelse(
    grepl('pneumonia', baseline_libya$g4_d_respiratory_type, fixed = TRUE), 1,0)
  pneumonia_table <- create_basic_table(baseline_libya, baseline_libya$d_pneumonia,"pre almanach")

  baseline_libya$d_urti <- ifelse(
    grepl('urti', baseline_libya$g4_d_respiratory_type, fixed = TRUE), 1,0)
  urti_table <- create_basic_table(baseline_libya, baseline_libya$d_urti,"pre almanach")


  reported_cough_table <- create_basic_table(baseline_libya, baseline_libya$g4_reported_cough,"pre almanach")
  checked_cough_table <- create_basic_table(baseline_libya, baseline_libya$g4_checked_cough,"pre almanach")
  present_cough_table <- create_basic_table(baseline_libya, baseline_libya$g4_cough_present,"pre almanach")

  respiratory_rate_assessed_table <- create_basic_table(baseline_libya, baseline_libya$g4_respiratory_rate,"pre almanach")
  
  respiratoryrate_cough_reported_table <- create_2way_table(baseline_libya$g4_respiratory_rate, baseline_libya$g4_reported_cough, c('Checked respiratory rate','Reported cough'))
  respiratoryrate_cough_present_table <- create_2way_table(baseline_libya$g4_respiratory_rate, baseline_libya$g4_cough_present, c('Checked respiratory rate','Present cough'))
  respiratoryrate_pneumonia_table <- create_2way_table(baseline_libya$g4_respiratory_rate, baseline_libya$d_pneumonia, c('Checked respiratory rate','Pneumonia present'))
  cough_reported_fever_present_table <- create_2way_table(baseline_libya$g4_reported_cough, baseline_libya$g4_fever_present, c('Reported cough','Fever present'))
  respiratoryrate_fever_present_table <- create_2way_table(baseline_libya$g4_respiratory_rate, baseline_libya$g4_fever_present, c('Checked respiratory rate','Fever present'))

  
  pneutable <- create_basic_table(baseline_libya, baseline_libya$g4_checked_cough,"pre almanach")
  present_cough_table <- create_basic_table(baseline_libya, baseline_libya$g4_cough_present,"pre almanach")



  baseline_libya$d_wheezing <- ifelse(baseline_libya$g4_d_respiratory_type == 'Wheezing', 1,0)
  wheezing_table <- create_basic_table(baseline_libya, baseline_libya$d_wheezing,"pre almanach")

  baseline_libya$d_resp_other <- ifelse(baseline_libya$g4_d_respiratory_type == 'Other respiratory disease', 1,0)
  resp_other_table <- create_basic_table(baseline_libya, baseline_libya$d_resp_other,"pre almanach")

  print('---------------------------')
  print('Creating anemia results graph')
  label_values <- list(
  list(value = 'anemia_moderate', label = "Moderate Anaemia"),
  list(value = 'anemia_severe', label = "Severe Anaemia"),
  list(value = 'other', label = "Other")  # Add more labels as needed
  )
  baseline_libya <- recode_data(baseline_libya, "g4_d_anemia_type", label_values)
  anemia_type_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_anemia_type,"pre almanach")
  print('---------------------------')
  print('Creating UTI results graph')
  uti_type_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_uti,"pre almanach")

  print('---------------------------')
  print('Calculate consultation time')

  time_cols = c('g3_t1', 'g4_t2', 'g4_t3', 'g4_t4')
  print(baseline_libya[['g3_t1']])
  for (col in time_cols) {
        baseline_libya[[col]] <- lubridate::ymd_hms(substr(baseline_libya[[col]], 1, 19))
  }
  # Replace missing values with 0 in ct2 and ct3
  baseline_libya$g4_t2[is.na(baseline_libya$g4_t2)] <- baseline_libya$g4_t4[is.na(baseline_libya$g4_t2)] 
  baseline_libya$g4_t3[is.na(baseline_libya$g4_t3)] <- baseline_libya$g4_t4[is.na(baseline_libya$g4_t3)] 

  # Calculate consultation_length in minutes
  diff1 <- baseline_libya$g4_t2 - baseline_libya$g3_t1
  diff2 <- baseline_libya$g4_t4 - baseline_libya$g4_t3
  baseline_libya$consultation_length <- (diff1 + diff2) / (60)
  
  baseline_libya$consultation_time <- trunc(baseline_libya$consultation_length)

  baseline_libya$consultation_time <- ifelse(baseline_libya$consultation_time > 12, 12,baseline_libya$consultation_time)
  label_values <- list(
  list(value = 0, label = "0-<1 min"),
  list(value = 1, label = "1-<2 min"),
  list(value = 2, label = "2-<3 min"), 
  list(value = 3, label = "3-<4 min"),
  list(value = 4, label = "4-<5 min"), 
  list(value = 5, label = "5-<6 min"),
  list(value = 6, label = "6-<7 min"),
  list(value = 7, label = "7-<8 min"), 
  list(value = 8, label = "8-<9 min"),
  list(value = 9, label = "9-<10 min"), 
  list(value = 10, label = "10-<11 min"),
  list(value = 11, label = "11-<12 min"),
  list(value = 12, label = ">=12 min")
  )
  baseline_libya <- recode_data(baseline_libya, "consultation_time", label_values)
  consultation_time_table <- create_basic_table(baseline_libya, baseline_libya$consultation_time,"pre almanach")
  consultation_time_summary <- (psych::describe(as.numeric(baseline_libya$consultation_length), skew=FALSE))
  print('---------------------------')
  print('Creating Danger signs results graph')
  
  # Replace 'dangersign_observed' with 1 if any of the specified conditions are met
  baseline_libya$dangersign_observed <- ifelse(baseline_libya$g4_unconscious_lethargic == 1 | baseline_libya$g4_convulsing == 1, 1, 0)
  dangersign_observed_table <- create_basic_table(baseline_libya, baseline_libya$dangersign_observed,"pre almanach")
  convulsions_observed<- create_basic_table(baseline_libya,baseline_libya$g4_convulsing, "pre almanach")
  convulsions_present<- create_basic_table(baseline_libya,baseline_libya$g4_conv_present, "pre almanach")

  # Replace 'dangersign_reported' with 1 if any of the specified conditions are met
  baseline_libya$dangersign_reported <- ifelse(baseline_libya$g4_reported_convulsions == 1 | baseline_libya$g4_reported_unable_to_drink == 1 | baseline_libya$g4_reported_vomitting_everything == 1, 1, 0)
  dangersign_reported_table <- create_basic_table(baseline_libya, baseline_libya$dangersign_reported,"pre almanach")
  convulsions_reported<- create_basic_table(baseline_libya,baseline_libya$g4_reported_convulsions, "pre almanach")
  vomitting_evr_reported_table <- create_basic_table(baseline_libya,baseline_libya$g4_reported_vomitting_everything, "pre almanach")
  unable_to_drink_reported_table <- create_basic_table(baseline_libya,baseline_libya$g4_reported_unable_to_drink, "pre almanach")


  baseline_libya$dangersign_checked <- ifelse(baseline_libya$g4_checked_convulsions == 1 | baseline_libya$g4_checked_unable_to_drink == 1 | baseline_libya$g4_checked_vomitting_everything == 1, 1, 0)
  dangersign_checked_table <- create_basic_table(baseline_libya, baseline_libya$dangersign_checked,"pre almanach")
  convulsions_checked<- create_basic_table(baseline_libya,baseline_libya$g4_checked_convulsions, "pre almanach")
  vomitting_evr_checked_table <- create_basic_table(baseline_libya,baseline_libya$g4_checked_vomitting_everything, "pre almanach")
  vomitting_evr_present_table <- create_basic_table(baseline_libya,baseline_libya$g4_vomitting_everything_present, "pre almanach")
  unable_to_drink_checked_table <- create_basic_table(baseline_libya,baseline_libya$g4_checked_unable_to_drink, "pre almanach")
  unable_to_drink_present_table <- create_basic_table(baseline_libya,baseline_libya$g4_unable_drink_present, "pre almanach")
  offered_drink_checked_vomit_table <- create_2way_table(baseline_libya$g4_drink_offered, baseline_libya$g4_checked_vomitting_everything, c('Drink offered','Checked if vomitting everything'))
  offered_drink_checked_vomit_present_table <- create_2way_table(baseline_libya$g4_drink_offered, baseline_libya$g4_vomitting_everything_present, c('Drink offered','Checked if vomitting present'))  
  
  baseline_libya$dehydration <- ifelse(baseline_libya$g4_d_dehydration_type == 'Moderate dehydration' | baseline_libya$g4_d_dehydration_type == 'Severe dehydration', 1,0)
  dehydration_table <- create_basic_table(baseline_libya, baseline_libya$dehydration,"pre almanach")

  ## Create respiratory feature 
  respiratory_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_respiratory,"pre almanach")
  ## Create diarrhoea feature
  diarrhoea_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_diarrhoea,"pre almanach")
  checked_diarrhoea_table <- create_basic_table(baseline_libya, baseline_libya$g4_checked_diarrhoea,"pre almanach")
  reported_diarrhoea_table <- create_basic_table(baseline_libya, baseline_libya$g4_reported_diarrhoea,"pre almanach")
  reported_diagnosed_diarrhoea_table <- create_2way_table(baseline_libya$g4_reported_diarrhoea, baseline_libya$g4_d_diarrhoea, c('Reported Diarrhoea','Diagnosed Diarrhoea'))
  present_diarrhoea_table <- create_basic_table(baseline_libya, baseline_libya$g4_diarrhoea_present,"pre almanach")

  ## Skin issues
  skin_pinch_table <- create_basic_table(baseline_libya, baseline_libya$g4_skin_pinch,"pre almanach")
  skin_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_skin,"pre almanach")
  skin_pinch_diarrhoea_reported_table <- create_2way_table(baseline_libya$g4_skin_pinch, baseline_libya$g4_reported_diarrhoea, c('Skin pinch','Reported diarrhoea'))
  skin_pinch_diarrhoea_present_table <- create_2way_table(baseline_libya$g4_skin_pinch, baseline_libya$g4_diarrhoea_present, c('Skin pinch','Diarrhoea present'))
  drink_offered_reported_diarrhoea_table <- create_2way_table(baseline_libya$g4_drink_offered, baseline_libya$g4_reported_diarrhoea, c('Drink offered','Reported diarrhoea'))
  drink_offered_present_diarrhoea_table <- create_2way_table(baseline_libya$g4_drink_offered, baseline_libya$g4_diarrhoea_present, c('Drink offered','Diarrhoea present'))
  baseline_libya$antibiotic_inj[!is.na(baseline_libya$g4_tt_general) & baseline_libya$g4_tt_general != 0] <- 1
  baseline_libya$antibiotic_inj[is.na(baseline_libya$antibiotic_inj)] <- 0
  print(baseline_libya$antibiotic_inj)
  antibiotic_inj_table <- create_basic_table(baseline_libya, baseline_libya$antibiotic_inj, "pre almanach")
  baseline_libya$antibiotic_oral[!is.na(baseline_libya$g4_tt_antibio_oral) & baseline_libya$g4_tt_antibio_oral != 0] <- 1
  antibiotic_oral_table <- create_basic_table(baseline_libya, baseline_libya$antibiotic_oral, "pre almanach")
  baseline_libya$antibiotics[(baseline_libya$antibiotic_oral != '' & baseline_libya$antibiotic_oral == 1) | 
                (baseline_libya$antibiotic_inj != '' & baseline_libya$antibiotic_inj == 1)] <- 1
  baseline_libya$antibiotics[is.na(baseline_libya$antibiotics)] <- 0
  # antibiotics injectables 
  baseline_libya$ampi_inj[baseline_libya$g4_tt_general == 'ampicillin'] <- 1

  baseline_libya$ceftriaxone_inj[baseline_libya$g4_tt_general == 'ceftriaxone'] <- 1

  baseline_libya$genta_inj[baseline_libya$g4_tt_general == 'genta'] <- 1

  
  ampi_inj_table <- create_basic_table(baseline_libya, baseline_libya$ampi_inj, "pre almanach")

  ceftriaxone_inj_table <- create_basic_table(baseline_libya, baseline_libya$ceftriaxone_inj, "pre almanach")
  genta_inj_table <- create_basic_table(baseline_libya, baseline_libya$genta_inj, "pre almanach")
  # antibiotics oral

  baseline_libya$amoxi_tab[grepl('amoxi_tab', baseline_libya$antibiotic_oral)] <- 1

  baseline_libya$amoxi_ml[grepl('amoxi_ml', baseline_libya$antibiotic_oral)] <- 1
  baseline_libya$amoxiclav_ml[grepl('amoxiclav_ml', baseline_libya$antibiotic_oral)] <- 1
  baseline_libya$ciprofloxacin_tab[grepl('ciprofloxacin_tab', baseline_libya$antibiotic_oral)] <- 1
  baseline_libya$erythromycin_ml[grepl('erythromycin_ml', baseline_libya$antibiotic_oral)] <- 1
  baseline_libya$erythromycin_tab[grepl('erythromycin_tab', baseline_libya$antibiotic_oral)] <- 1
  baseline_libya$metro_ml[grepl('metro_ml', baseline_libya$antibiotic_oral)] <- 1
  baseline_libya$metro_tab[grepl('metro_tab', baseline_libya$antibiotic_oral)] <- 1
  baseline_libya$penicillin_v[grepl('penicillin_v', baseline_libya$antibiotic_oral)] <- 1

  #TODO
  amoxi_tab_table <- create_basic_table(baseline_libya, baseline_libya$amoxi_tab, "pre almanach")
  amoxi_ml_table <- create_basic_table(baseline_libya, baseline_libya$amoxi_ml, "pre almanach")
  amoxiclav_ml_table <- create_basic_table(baseline_libya, baseline_libya$amoxiclav_ml, "pre almanach")
  ciprofloxacin_tab_table <- create_basic_table(baseline_libya, baseline_libya$ciprofloxacin_tab, "pre almanach")
  erythromycin_ml_table <- create_basic_table(baseline_libya, baseline_libya$erythromycin_ml, "pre almanach")
  erythromycin_tab_table <- create_basic_table(baseline_libya, baseline_libya$erythromycin_tab, "pre almanach")
  metro_ml_table <- create_basic_table(baseline_libya, baseline_libya$metro_ml, "pre almanach")
  metro_tab_table <- create_basic_table(baseline_libya, baseline_libya$metro_tab, "pre almanach")
  penicillin_v_table <- create_basic_table(baseline_libya, baseline_libya$penicillin_v, "pre almanach")

  antibiotics_table <- create_basic_table(baseline_libya, baseline_libya$antibiotics, "pre almanach")
  
  baseline_libya$parasitosis <- ifelse(baseline_libya$g4_d_diarrhoea_type == 'Intestinal Parasitosis', 1,0)
  parasitosis_atb_table <- create_2way_table(baseline_libya$parasitosis, baseline_libya$antibiotics, c('Parasitosis','Antibiotics'))

  other_diagnostic_table <- create_basic_table(baseline_libya, baseline_libya$g4_str_d_other,"pre almanach")
  
  parasitosis_table <- create_basic_table(baseline_libya, baseline_libya$parasitosis,"pre almanach")
  
  baseline_libya$awd <- ifelse(baseline_libya$g4_d_diarrhoea_type == 'Acute Watery Diarrhea/Diarrhea', 1,0)
  awd_table <- create_basic_table(baseline_libya, baseline_libya$awd,"pre almanach")
  baseline_libya$ors[baseline_libya$g4_medication_other == 'orsa' | baseline_libya$g4_medication_other == 'orsb'] <- 1
  baseline_libya$zinc[baseline_libya$g4_medication_other == 'zinc'] <- 1
  
  awd_atb_table <- create_2way_table(baseline_libya$awd, baseline_libya$antibiotics, c('Awd','Antibiotics'))
  
  awd_ors_table <- create_2way_table(baseline_libya$awd, baseline_libya$ors, c('Awd','ORS treatment'))
  awd_zinc_table <- create_2way_table(baseline_libya$awd, baseline_libya$zinc, c('Awd','Zinc'))

  fever_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_fever,"pre almanach")
  fever_reported_table <- create_basic_table(baseline_libya, baseline_libya$g4_reported_fever,"pre almanach")
  fever_checked_table <- create_basic_table(baseline_libya, baseline_libya$g4_checked_fever,"pre almanach")
  fever_present_table <- create_basic_table(baseline_libya, baseline_libya$g4_fever_present,"pre almanach")
  fever_present_and_checked_table <- create_2way_table(baseline_libya$g4_fever_present, baseline_libya$g4_checked_fever, c('Fever present','Fever checked'))


  ear_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_ear,"pre almanach")
  reported_ear_table <- create_basic_table(baseline_libya, baseline_libya$g4_reported_eaer,"pre almanach")
  checked_ear_table <- create_basic_table(baseline_libya, baseline_libya$g4_checked_ear,"pre almanach")
  present_ear_table <- create_basic_table(baseline_libya, baseline_libya$g4_ear_present,"pre almanach")
  d_ear_and_ear_reported_table <- create_2way_table(baseline_libya$g4_d_ear, baseline_libya$g4_reported_eaer, c('Ear problem diagnosed','Ear problem reported'))


  throat_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_throat,"pre almanach")

  anemia_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_anemia,"pre almanach")
  anemia_palm_pallor_table <- create_basic_table(baseline_libya, baseline_libya$g4_palm_pallor,"pre almanach")
  palm_pallor_anemia_diagnosed_table <- create_2way_table(baseline_libya$g4_palm_pallor, baseline_libya$g4_d_anemia, c('Palm pallor','Anemia diagnosed'))

  uti_table <- create_basic_table(baseline_libya, baseline_libya$g4_d_uti,"pre almanach")
  
  user_table <- create_basic_table(baseline_libya, baseline_libya$g2_int_user,"pre almanach")


  baseline_libya_numeric <- baseline_libya
  baseline_libya_numeric <- data.frame(sapply(baseline_libya_numeric, as.numeric))
  baseline_libya_numeric[is.na(baseline_libya_numeric)] <- 0
  baseline_libya$comorbidity <- as.numeric(baseline_libya_numeric$g4_d_diarrhoea) + as.numeric(baseline_libya_numeric$g4_d_dehydration) + 
                                  as.numeric(baseline_libya_numeric$g4_d_respiratory) + as.numeric(baseline_libya_numeric$g4_d_fever) +
                                  as.numeric(baseline_libya_numeric$g4_d_ear) + as.numeric(baseline_libya_numeric$g4_d_throat) + 
                                  as.numeric(baseline_libya_numeric$g4_d_anemia) + as.numeric(baseline_libya_numeric$g4_d_uti)
  baseline_libya$antibio_oral_cat <- ifelse(grepl('amoxi_tab', baseline_libya$antibiotic_oral) | grepl('amoxi_ml', baseline_libya$antibiotic_oral), 'amoxi', 
    ifelse(grepl('metro_tab', baseline_libya$antibiotic_oral) | grepl('metro_ml', baseline_libya$antibiotic_oral), 'metro',
    ifelse(grepl('erythromycin_tab', baseline_libya$antibiotic_oral) | grepl('erythromycin_tab', baseline_libya$antibiotic_oral), 'erythromycin', 
    ifelse(grepl('amoxiclav_ml', baseline_libya$antibiotic_oral), 'amoxiclav',
    ifelse(grepl('penicillin_v', baseline_libya$antibiotic_oral), 'penicillin',
    ifelse(grepl('ciprofloxacin_tab', baseline_libya$antibiotic_oral), 'cipro', ''))))))
  antibio_oral_category_table <- create_basic_table(baseline_libya, baseline_libya$antibio_oral_cat, "pre almanach")
  
  ## Creating tables with multiple diagnosis
  comorbidity_table <- create_basic_table(baseline_libya, baseline_libya$comorbidity, "pre almanach")
  twocomorbidity_table <- baseline_libya[as.numeric(baseline_libya$comorbidity) == 2, c('g4_d_diarrhoea', 'g4_d_dehydration',
                    'g4_d_respiratory', 'g4_d_fever', 'g4_d_ear', 'g4_d_throat', 'g4_d_anemia', 'g4_d_uti')]
  threecomorbidity_table <- baseline_libya[as.numeric(baseline_libya$comorbidity) == 3, c('g4_d_diarrhoea', 'g4_d_dehydration',
                    'g4_d_respiratory', 'g4_d_fever', 'g4_d_ear', 'g4_d_throat', 'g4_d_anemia', 'g4_d_uti')]
  fourcomorbidity_table <- baseline_libya[as.numeric(baseline_libya$comorbidity) == 4, c('g4_d_diarrhoea', 'g4_d_dehydration',
                    'g4_d_respiratory', 'g4_d_fever', 'g4_d_ear', 'g4_d_throat', 'g4_d_anemia', 'g4_d_uti')]
  #Other clinical exams
  checked_oedema <- create_basic_table(baseline_libya, baseline_libya$g4_feet_oedema, "pre almanach")
  checked_stiff_neck <- create_basic_table(baseline_libya, baseline_libya$g4_stiff_neck, "pre almanach")
  baseline_libya$age_gt_1 <- baseline_libya$g2_p_age > 1 & !is.na(baseline_libya$g2_p_age)
  stiffneck_age_table <- create_2way_table(baseline_libya$g4_stiff_neck, baseline_libya$g2_p_age, c('Stiff neck','Age'))
  stiffneck_age_gt_1_table <- create_2way_table(baseline_libya$g4_stiff_neck, baseline_libya$age_gt_1, c('Stiff neck','Age greater than one'))
  checked_mouth <- create_basic_table(baseline_libya, baseline_libya$g4_mouth, "pre almanach")
  checked_ear <- create_basic_table(baseline_libya, baseline_libya$g4_ear, "pre almanach")
  drink_offered_table <- create_basic_table(baseline_libya, baseline_libya$g4_drink_offered, "pre almanach")
  feeding_normal_table <- create_basic_table(baseline_libya, baseline_libya$g4_feeding_normal, "pre almanach")
  breastfeeding_table <- create_basic_table(baseline_libya, baseline_libya$g4_breastfeeding, "pre almanach")
  immunization_checked_table <- create_basic_table(baseline_libya, baseline_libya$g4_health_card, "pre almanach")
  muac_mentioned_table <- create_basic_table(baseline_libya, baseline_libya$g4_health_card_muac, "pre almanach")
  weight_mentioned_table <- create_basic_table(baseline_libya,baseline_libya$g4_health_card, "pre almanach")

  


  #We don't have this variable in Libya
  #baseline_libya$tt_ORS[baseline_libya$g4_tt_ORSa == 1 | baseline_libya$g4_tt_ORSb == 1] <- 1 
  #tt_ors_table <- create_basic_table(baseline_libya, "tt_ORS", "pre almanach")
  urti_antibiotics_table <- create_2way_table(baseline_libya$d_urti, baseline_libya$antibiotics, c('URTI','Antibiotics'))
  pneumonia_antibiotics_table <- create_2way_table(baseline_libya$d_pneumonia, baseline_libya$antibiotics, c('Pneumonia','Antibiotics'))
  # We are missing the respiratory one that will come with v2 
  
  baseline_libya$urti_no_atb <- with(baseline_libya, ifelse('g4_respiratory' == 'urti' & 
              'antibiotics' == 1 & 
              ('respiratory' != 'pneumonia' | 'g4_d_ear_type' != 'Acute ear infection with discharge' | 
              'g4_d_throat_type' != 'Possible Streptococcus sore throat' |
              'g4_d_diarrhoea_type' != 'Severe Persistent Diarrhoea' |
              'g4_d_uti' == 1 | ('parasitosis'!=1 & 
              ('g4_antibio_oral' !='metro_ml' | 'g4_antibio_oral' !='metro_tab' ))), 1, 0))
  
  urti_unnecessary_atb_table <- create_basic_table(baseline_libya, baseline_libya$urti_no_atb, 'pre almanach')
  baseline_libya$d_urti[is.na(baseline_libya$antibiotics)] <- 0
  baseline_libya$d_urti[is.na(baseline_libya$antibiotics)] <- 0

  diagnosis_2_urti_atb <- baseline_libya[as.numeric(baseline_libya$comorbidity) == 2 & as.numeric(baseline_libya$d_urti)==1
                      & as.numeric(baseline_libya$antibiotics)==1, c('g4_d_diarrhoea', 'g4_d_dehydration',
                    'g4_d_respiratory', 'g4_d_fever', 'g4_d_ear', 'g4_d_throat')]
  diagnosis_3_urti_atb <- baseline_libya[as.numeric(baseline_libya$comorbidity) == 3 & as.numeric(baseline_libya$d_urti)==1
                    & as.numeric(baseline_libya$antibiotics)==1, c('g4_d_diarrhoea', 'g4_d_dehydration',
                    'g4_d_respiratory', 'g4_d_fever', 'g4_d_ear', 'g4_d_throat')]
  diagnosis_4_urti_atb <- baseline_libya[as.numeric(baseline_libya$comorbidity) == 4 & as.numeric(baseline_libya$d_urti)==1
                    & as.numeric(baseline_libya$antibiotics)==1, c('g4_d_diarrhoea', 'g4_d_dehydration',
                    'g4_d_respiratory', 'g4_d_fever', 'g4_d_ear', 'g4_d_throat')]
  
  baseline_libya$muac_done <- ifelse((baseline_libya$g4_muac == 1 | baseline_libya$g4_muac == 2 | baseline_libya$g4_muac == 3) & (baseline_libya$g2_p_age_months >= 6 | baseline_libya$g2_p_age_months == ''), 1,0)
  muac_done_table <- create_basic_table(baseline_libya, baseline_libya$muac_done, 'pre almanach')
  baseline_libya$muac <- ifelse((baseline_libya$g2_p_age_months >= 6 | baseline_libya$g2_p_age_months == '' | is.na(baseline_libya$g2_p_age_months)), baseline_libya$g4_muac_color ,'too young')
  muac_table <- create_basic_table(baseline_libya, baseline_libya$muac, 'pre almanach')
  general_feeding_recoms_table <- create_basic_table(baseline_libya, baseline_libya$g4_general_feeding_recoms, 'pre almanach')
  informed_about_illness_table <- create_basic_table(baseline_libya, baseline_libya$g4_illness_informed, 'pre almanach')
  described_signs_to_return_table <- create_basic_table(baseline_libya, baseline_libya$g4_signto_comeback, 'pre almanach')
  
  advice_fluid_extra_table <- create_basic_table(baseline_libya, baseline_libya$g4_advice_fluid_extra, 'pre almanach')
  advice_feed_continuous_table <- create_basic_table(baseline_libya, baseline_libya$g4_advice_feed_continuous, 'pre almanach')
  medication_explained_table <- create_basic_table(baseline_libya, baseline_libya$g4_medication_explained, 'pre almanach')
  baseline_libya$counseling <- ifelse(baseline_libya$g4_illness_informed == 1 | baseline_libya$g4_medication_explained == 1 | baseline_libya$g4_general_feeding_recoms == 1 | baseline_libya$g4_advice_fluid_extra == 1 | baseline_libya$g4_advice_feed_continuous == 1 | baseline_libya$g4_signto_comeback == 1, 1,0)
  
  counseling_table <- create_basic_table(baseline_libya, baseline_libya$counseling,"pre almanach")
  labels = list('Age in months|Age group','URTI present|Antibiotics given',
  'Pneumonia present|Antibiotics given', 'Drink offered|checked if vomitting everything',
  'Drink offered|checked if vommiting everything','Drink offered | vomitting everything present',
  'Fever present|checked fever','Respiratory rate assessed|cough reported',
  'Respiratory rate assessed|cough present','Respiratory rate assessed|pneumonia present',
  'Cough reported|fever present','Respiratory rate assessed|fever present',
  'Reported diarrhoea vs Diarrhoea dignosed','Parasitosis|Antibiotics given',
  'Acute watery diarrhoea|Antibiotics given','ORS treatment|acute watery diarrhoea',
  'Zinc treatment|acute watery diarrhoea','Skin pinch|diarrhoea reported','Skin pinch|diarrhoea present',
  'Drink offered|diarrhoea reported','Drink offered|diarrhoea present','Ear diagnosed|Ear problem reported',
  'Palm pallor|Anemia diagnosed','Stiff neck checked|Age of the child','Stiff neck checked|Older than 1y'
  )
  captions = list('permission table','User table', 'facility table', 'healthworker table',
  'gender table', 'age table','Age in months (children < 2 yrs)','comorbidity table', 'Two diagnostics at same time', 
  'Three diagnostics at same time', 'Four diagnostics at same time', 'throat table', 
  'throat type table', 'respiratory table', 'respiratory type table', 'Pneumonia cases', 
  'URTI cases', 'URTI Cases with antibiotics', 'Pneumonia cases with antibiotics',
  'URTI treated with antibiotics without being necessary','Pilot - Many diagnosis =2 and urti and antibiotics=1',
  'Pilot - Many diagnosis =3 and urti and antibiotics=1', 'Pilot - Many diagnosis =4 and urti and antibiotics=1',
  'Wheezing cases', 'Other respiratory issues','Malnutrition table', 'Malnutrition type table', 'dehydration table',
  'dehydration type table', 'diarrhoea table', 'diarrhoea type table', 'fever table',
  'fever type table', 'ear table', 'ear type table', 'anemia table', 'anemia type table', 
  'uti table', 'Skin table', 'Other diagnosis table','consultation time discriptive statistics', 
  'consultation time table', 'Danger signs reported','Danger signs checked','Danger signs observed',
  'Convulsions reported','Convulsions checked','Convulsions observed', 'Convulsions present', 
  'Reported vomitting everything','Checked vomitting everything','Vomitting everything present',
  'Drink offered and checked if vomitting everything','Drink offered and vomitting everything present',
  'Reported unable to drink','Checked if unable to drink','Unable to drink present',
  'Fever reported','Fever checked','Fever present','Fever present and checked',
  'Cough reported','Cough checked','Cough present','Respiratory rate assessed',
  'Respiratory rate assessed when cough was reported','Respiratory rate assessed when cough present',
  'Respiratory rate assessed when pneumonia present', 'Couch reported when fever was present',
  'Respiratory rate assessed when fever was present', 'Reported diarrhoea','Reported diarrhoea vs Diagnosed diarrhoea' ,
  'Parasitosis vs antibiotics', 'Acute watery diarrhoea vs antibiotics', 'ORS treatment with acute watery diarrhoea', 
  'Zinc treatment with acute watery diarrhoea' , 'Checked diarrhoea',
  'Present diarrhoea', 'Skin pinch', 'Skin pinch and diarrhoea reported', 'Skin pinch and diarrhoea present', 
  'Drink offered and diarrhoea reported', 'Drink offered and diarrhoea present',
  'Reported ear problem','Checked ear problem','Present ear problem','Ear diagnosed and reported ear problem',
  'Palm pallor','Palm pallor and anemia diagnosed', 'Feet pressed to check oedema', 'Checked for neck stiffness',
  'Stiff neck checked against age','Stiff neck checked on older than 1ys old','Looked into mouth',
  'Felt behind child\'s ear','Offered something to drink','Asked about feeding habits when not ill',
  'Asked about breastfeeding habits when ill','Child\'s immunization card checked','Mentioned MUAC to caretaker', 
  'Mentioned weight to caretaker',
  'antibiotic injections table', 'Ampicillin injections','Ceftriaxone injections','Gentamycin injections', 
  'antibiotic oral table', 'Amoxicillin tablets','Amoxicillin syrup','Amoxiclav syrup','Ciprofloxacin tablets',
  'Erythromycin syrup','Erythromycin tablets','Metronidazole syrup','Metronidazole tablets','Penicillin V', 
  'Antibiotic oral category', 
  'referral table', 'Tests ordered ','referral vaccination table', 'referral hemo table',  
  'counseling table', 'parasitosis table', 'awd table', 'antibiotics table', 'MUAC measured', 'MUAC', 'General feeding recommendations',
  'Informed about illness', 'Described signs for which to come back', 'Told caretaker to give extra fluids to child',
  'Told the caretaker to continue feeding during this illness', 'Explained how to administer oral treatment', 'Total amount of counseling given')
  create_report('ALMANACH Libya', 'DHU', 'pdf', list(
    permission_table,user_table,fac_table,hw_table,gender_table,
    age_table, age_w_months_table, comorbidity_table, twocomorbidity_table,
    threecomorbidity_table, fourcomorbidity_table, throat_table, 
    throat_type_table, respiratory_table, respiratory_type_table, pneumonia_table,
    urti_table,urti_antibiotics_table, pneumonia_antibiotics_table,urti_unnecessary_atb_table, 
    diagnosis_2_urti_atb, diagnosis_3_urti_atb, diagnosis_4_urti_atb, wheezing_table, resp_other_table,
    malnutrition_table, malnutrition_type_table, dehydration_table, 
    dehydration_type_table, diarrhoea_table, 
    diarrhea_type_table, fever_table, fever_type_table, ear_table, ear_type_table, 
    anemia_table, anemia_type_table, uti_type_table, skin_table, other_diagnostic_table,
    consultation_time_summary, consultation_time_table, dangersign_reported_table, 
    dangersign_checked_table, dangersign_observed_table, 
    convulsions_reported, convulsions_checked, convulsions_observed, convulsions_present,
    vomitting_evr_reported_table, vomitting_evr_checked_table, vomitting_evr_present_table,
    offered_drink_checked_vomit_table, offered_drink_checked_vomit_present_table, 
    unable_to_drink_reported_table, unable_to_drink_checked_table, unable_to_drink_present_table,
    fever_reported_table, fever_checked_table, fever_present_table, fever_present_and_checked_table,
    reported_cough_table, checked_cough_table, present_cough_table, respiratory_rate_assessed_table,
    respiratoryrate_cough_reported_table, respiratoryrate_cough_present_table, respiratoryrate_pneumonia_table,
    cough_reported_fever_present_table, respiratoryrate_fever_present_table, reported_diarrhoea_table, 
    reported_diagnosed_diarrhoea_table, parasitosis_atb_table, awd_atb_table, awd_ors_table, awd_zinc_table,
    checked_diarrhoea_table, present_diarrhoea_table, skin_pinch_table, skin_pinch_diarrhoea_reported_table,
    skin_pinch_diarrhoea_present_table, drink_offered_reported_diarrhoea_table, drink_offered_present_diarrhoea_table,
    reported_ear_table, checked_ear_table, present_ear_table, d_ear_and_ear_reported_table, 
    anemia_palm_pallor_table, palm_pallor_anemia_diagnosed_table, checked_oedema, checked_stiff_neck,
    stiffneck_age_table, stiffneck_age_gt_1_table, checked_mouth, checked_ear, drink_offered_table,
    feeding_normal_table, breastfeeding_table, immunization_checked_table, muac_mentioned_table, weight_mentioned_table,
    antibiotic_inj_table, ampi_inj_table, ceftriaxone_inj_table, genta_inj_table, antibiotic_oral_table,
    amoxi_tab_table, amoxi_ml_table, amoxiclav_ml_table,ciprofloxacin_tab_table,erythromycin_ml_table,
    erythromycin_tab_table, metro_ml_table, metro_tab_table, penicillin_v_table, antibio_oral_category_table,
    referral_table, tests_table, referral_vaccination_table,
    referral_hemo_table, counseling_table, parasitosis_table, awd_table,
    antibiotics_table, muac_done_table, muac_table, general_feeding_recoms_table, informed_about_illness_table,
    described_signs_to_return_table, advice_fluid_extra_table, advice_feed_continuous_table, 
    medication_explained_table, counseling_table), captions)
  
  # Execute the form selection module
  current_odk_form <- timci::select_list_item_server("select_odk_form",
                                                     valid_odk_form_list,
                                                     odk_data)


  # Execute the info module about the raw ODK data
  timci::reactive_odk_data_info_server("raw_odk_info", current_odk_form)
  # Execute the table module that displays the raw ODK data
  timci::reactive_data_table_server("raw_odk_table", current_odk_form)
  # Execute the CSV and Excel download modules
  timci::reactive_csv_download_server("raw_odk_csv_export", current_odk_form)
  timci::reactive_xlsx_download_server("raw_odk_xlsx_export", current_odk_form)
}

ui <- shiny::fluidPage(

  shiny::navbarPage("TIMCI Dashboard", id="nav",

                    shiny::navbarMenu("Data",

                                      # Raw ODK data panel (will be hidden / access restricted)
                                      shiny::tabPanel("ODK data",

                                                      # Sidebar layout with input and output definitions
                                                      shiny::sidebarLayout(

                                                        # Sidebar panel for inputs
                                                        shiny::sidebarPanel(

                                                          # Help text
                                                          shiny::helpText("ODK form selection"),

                                                          # Display the list item selection module
                                                          timci::select_list_item_ui("select_odk_form"),

                                                          # Info text
                                                          shiny::p(shiny::strong("Database information")),

                                                          # Study data info
                                                          timci::odk_data_info_ui("raw_odk_info"),

                                                          # Export study data in *.csv format
                                                          timci::csv_download_ui("raw_odk_csv_export"),

                                                          # Export study data in *.xlsx format
                                                          timci::xlsx_download_ui("raw_odk_xlsx_export")),


                                                        # Display the table module
                                                        shiny::mainPanel(

                                                          shiny::fluidRow(
                                                            timci::data_table_ui("raw_odk_table"))))
                                      )
                    )

  )
)

shinyApp(ui,server)

