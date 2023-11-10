library(shiny)
library(ruODK)
library(hash)
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
    print(length(parts[[1]]))
    if (length(parts[[1]]) == 2) {
      var_name <- parts[[1]][1]
      var_value <- parts[[1]][2]
      Sys.setenv(var_name = var_value)
    } else {
      cat("Skipping line:", env_var_line, "\n")
    }
  }
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
  baseline_libya = odk_data[["baseline_questionnaire_libya"]]
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
    meta_instance_id != 'uuid:22bbd5d6-cc79-44d8-8700-6385de32dd31')
  print(baseline_libya$system_submission_date)
  baseline_libya$system_submission_date <- as.Date(baseline_libya$system_submission_date, format = "%Y-%m-%d %H:%M:%S")
  baseline_libya <- baseline_libya[order(baseline_libya$system_submission_date), ]

  baseline_libya <- baseline_libya %>%
    dplyr::mutate_if(is.character, convert_to_numeric_safely)
  print("-----------------")
  print("Creating permission table")

  baseline_libya$g1_permission_granted <- factor(baseline_libya$g1_permission_granted, levels = c(0, 1, 2),
                                 labels = c("No consent", "Yes, oral", "Yes, signed"))

  print('--------------------------------')
  

  permission_table <- create_basic_table(baseline_libya, "g1_permission_granted","pre almanach")
  print("Creating facility table")
  print(baseline_libya$g1_permission_granted)
  baseline_libya <- baseline_libya[baseline_libya$g1_permission_granted == "Yes, oral" | 
    baseline_libya$g1_permission_granted == "Yes, signed", ]

  baseline_libya$g2_fac <- ifelse(baseline_libya$g2_fac == "sk", 1, baseline_libya$g2_fac)


  baseline_libya$g2_fac <- factor(baseline_libya$g2_fac, levels = c(1),
                                 labels = c("Suleiman Khater"))
    
  fac_table <- create_basic_table(baseline_libya, "g2_fac","pre almanach")
  print('---------------------------')
  print('Creating health worker category')
  baseline_libya$g2_hw <- ifelse(baseline_libya$g2_hw == 'pediatrician', 1, 
    ifelse(baseline_libya$g2_hw== 'nurse_reg', 2,
    ifelse(baseline_libya$g2_hw== 'nurse', 3, 
    ifelse(baseline_libya$g2_hw == 'midwife', 4,
    ifelse(baseline_libya$g2_hw == 'gp', 5, 99)))))
  baseline_libya$g2_hw <- factor(baseline_libya$g2_hw, levels = c(1, 2, 3, 4, 5, 99),
                                 labels = c("Pediatrician", "Registered Nurse", "Nurse", "Midwife", "General Practitioner", "Other"))
  
  hw_table <- create_basic_table(baseline_libya, "g2_hw","pre almanach")
  print('---------------------------')
  print('Creating gender graph')
  baseline_libya$g2_p_sex <- ifelse(baseline_libya$g2_p_sex == 'female', 1, 
    ifelse(baseline_libya$g2_p_sex == 'male', 2, 99))
  baseline_libya$g2_p_sex <- factor(baseline_libya$g2_p_sex, levels = c(1, 2, 99),
                                 labels = c("Girls", "Boys", "Other"))
  
  gender_table <- create_basic_table(baseline_libya, "g2_p_sex","pre almanach")
  print('---------------------------')
  print('Creating age graph')

  baseline_libya$g2_p_age <- ifelse(baseline_libya$g2_p_age == '2-11', 1, 
    ifelse(baseline_libya$g2_p_age== '12-23', 2,
    ifelse(baseline_libya$g2_p_age== '24-35', 3, 
    ifelse(baseline_libya$g2_p_age == '36-47', 4,
    ifelse(baseline_libya$g2_p_age == '48-59', 5, 99)))))
  baseline_libya$g2_p_age <- factor(baseline_libya$g2_p_age, levels = c(1, 2, 3, 4, 5, 99),
                                 labels = c("2-11 mo", "12-23 mo", "2-<3y", "3-<4y", "4-<5y", "Other"))
  
  age_table <- create_basic_table(baseline_libya, "g2_p_age","pre almanach")
  print('---------------------------')
  print('Creating muac graph')
  baseline_libya$g4_muac <- ifelse(baseline_libya$g4_muac == 'none', 0, 
    ifelse(baseline_libya$g4_muac== 'red', 1,
    ifelse(baseline_libya$g4_muac== 'yellow', 2, 
    ifelse(baseline_libya$g4_muac == 'green', 3, 99))))
  baseline_libya$g4_muac <- factor(baseline_libya$g4_muac, levels = c(0, 1, 2, 3, 99),
                                 labels = c("none", "red", "yellow", "green", "Other"))
  
  muac_table <- create_basic_table(baseline_libya, "g4_muac","pre almanach")
  print('---------------------------')
  print('Creating referral graph')
  baseline_libya$g4_referral <- ifelse(baseline_libya$g4_referral == 'home', 1, 
    ifelse(baseline_libya$g4_referral== 'refer_urgent', 2,
    ifelse(baseline_libya$g4_referral == 'refer_nonurgent', 3, 99)))
  baseline_libya$g4_referral <- factor(baseline_libya$g4_referral, levels = c(1, 2, 3, 99),
                                 labels = c("Child treated and sent home", "Urgent referral to hospital/ Stabilization center recommended"
                                 , "Non urgent referral for other care/ assessment recommended", "Not available"))
  
  referral_table <- create_basic_table(baseline_libya, "g4_referral","pre almanach")

  print('---------------------------')
  print('Creating vaccination graph')
  baseline_libya$g4_referral_vacc_reason <- ifelse(baseline_libya$g4_referral_vacc_reason == 'complete', 1, 
    ifelse(baseline_libya$g4_referral_vacc_reason== 'vaccine_stockout', 2, 99))
  baseline_libya$g4_referral_vacc_reason <- factor(baseline_libya$g4_referral_vacc_reason, levels = c(1, 2, 99),
                                 labels = c("informed but not due/ completed", 
                                 "informed but vaccines not available",
                                  "Not available"))
  
  referral_vaccination_table <- create_basic_table(baseline_libya, "g4_referral_vacc_reason","pre almanach")
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
  
  referral_hemo_table <- create_basic_table(baseline_libya, "g4_hemo_result","pre almanach")
  print('---------------------------')
  print('Creating Dehydration results graph')
  baseline_libya$g4_d_dehydration_type <- ifelse(baseline_libya$g4_d_dehydration_type == 'dehydration_no', 0, 
    ifelse(baseline_libya$g4_d_dehydration_type== 'dehydration_moderate', 1,
    ifelse(baseline_libya$g4_d_dehydration_type== 'dehydration_severe', 2, 99)))
  baseline_libya$g4_d_dehydration_type <- factor(baseline_libya$g4_d_dehydration_type, levels = c(1, 2, 3, 99),
                                 labels = c("Moderate dehydration", 
                                 "Severe dehydration",
                                  "No dehydration", "Other"))
  
  dehydration_type_table <- create_basic_table(baseline_libya, "g4_d_dehydration_type","pre almanach")

  print('---------------------------')
  print('Creating Diarrhoea results graph')
  label_values <- list(
  list(value = 'awd', label = "Acute Watery Diarrhea/Diarrhea"),
  list(value = 'dysentery', label = "Dysentery"),
  list(value = 'parasitosis', label = "Intestinal Parasitosis"),
  list(value = 'persistent_diarr', label = "Persistent diarrhea"),
  list(value = 'other', label = "Other digestive disease")  # Add more labels as needed
  )
  baseline_libya <- recode_column(baseline_libya, "g4_d_diarrhoea_type", label_values)
  diarrhea_type_table <- create_basic_table(baseline_libya, "g4_d_diarrhoea_type","pre almanach")

  print('---------------------------')
  print('Creating Fever results graph')
  label_values <- list(
  list(value = 'measles', label = "Measles"),
  list(value = 'fever_vsfd', label = "Very severe febrile disease"),
  list(value = 'other', label = "Other febrile disease")  # Add more labels as needed
  )
  baseline_libya <- recode_column(baseline_libya, "g4_d_fever_type", label_values)
  fever_type_table <- create_basic_table(baseline_libya, "g4_d_fever_type","pre almanach")

  print('---------------------------')
  print('Creating Ear results graph')
  label_values <- list(
  list(value = 'mastoiditis', label = "Mastoiditis"),
  list(value = 'aei', label = "Acute Ear Infection"),
  list(value = 'other', label = "Other Ear disease")  # Add more labels as needed
  )
  baseline_libya <- recode_column(baseline_libya, "g4_d_ear_type", label_values)
  ear_type_table <- create_basic_table(baseline_libya, "g4_d_ear_type","pre almanach")

  print('---------------------------')
  print('Creating throat results graph')
  label_values <- list(
  list(value = 'sore_throat', label = "Sore Throat (Viral Sore Throat)"),
  list(value = 'throat_bacterial', label = "Possible Streptococcus sore throat"),
  list(value = 'throat_abscess', label = "Throat abscess"),
  list(value = 'other', label = "Other throat disease")  # Add more labels as needed
  )
  baseline_libya <- recode_column(baseline_libya, "g4_d_throat_type", label_values)
  throat_type_table <- create_basic_table(baseline_libya, "g4_d_throat_type","pre almanach")

  print('---------------------------')
  print('Creating anemia results graph')
  label_values <- list(
  list(value = 'anemia_moderate', label = "Moderate Anaemia"),
  list(value = 'anemia_severe', label = "Severe Anaemia"),
  list(value = 'other', label = "Other")  # Add more labels as needed
  )
  baseline_libya <- recode_column(baseline_libya, "g4_d_throat_type", label_values)
  anemia_type_table <- create_basic_table(baseline_libya, "g4_d_throat_type","pre almanach")
  print('---------------------------')
  print('Creating UTI results graph')
  label_values <- list(
  list(value = 'uti', label = "UTI"),
  list(value = 'none', label = "None")  # Add more labels as needed
  )
  baseline_libya <- recode_column(baseline_libya, "g4_d_uti", label_values)
  uti_type_table <- create_basic_table(baseline_libya, "g4_d_uti","pre almanach")

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
  print("the sum")
  print(diff1+diff2)
  baseline_libya$consultation_length <- (diff1 + diff2) / (60)
  
  print(baseline_libya$consultation_length)
  baseline_libya$consultation_time <- trunc(baseline_libya$consultation_length)
  print(baseline_libya$consultation_time)

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
  baseline_libya <- recode_column(baseline_libya, "consultation_time", label_values)
  consultation_time_table <- create_basic_table(baseline_libya, "consultation_time","pre almanach")
  print('---------------------------')
  print('Creating Danger signs results graph')
  
  # Replace 'dangersign_observed' with 1 if any of the specified conditions are met
  baseline_libya$dangersign_observed[baseline_libya$g4_unconscious == 1 | baseline_libya$g4_lethargic == 1 | baseline_libya$g4_convulsing == 1] <- 1

  # Replace 'dangersign_reported' with 1 if any of the specified conditions are met
  baseline_libya$dangersign_reported[baseline_libya$g4_reported_convulsions == 1 | baseline_libya$g4_reported_unable_to_drink == 1 | baseline_libya$g4_reported_vomitting_everything == 1] <- 1

  baseline_libya$dangersign_checked[baseline_libya$g4_checked_convulsions == 1 | baseline_libya$g4_checked_unable_to_drink == 1 | baseline_libya$g4_checked_vomitting_everything == 1] <- 1

  baseline_libya$muac_done[baseline_libya$g4_muac == 1 | baseline_libya$g4_muac == 2 | baseline_libya$g4_muac == 3] <- 1

  baseline_libya$counseling[baseline_libya$g4_illness_informed == 1 | baseline_libya$g4_medication_explained == 1 | baseline_libya$g4_general_feeding_recoms == 1 | baseline_libya$g4_advice_fluid_extra == 1 | baseline_libya$g4_advice_feed_continuous == 1 | baseline_libya$g4_signto_comeback == 1] <- 1
  
  counseling_table <- create_basic_table(baseline_libya, "counseling","pre almanach")
  
  baseline_libya$dehydration[baseline_libya$g4_d_dehydration_type == 'Moderate dehydration' | baseline_libya$g4_d_dehydration_type == 'Severe dehydration'] <- 1
  dehydration_table <- create_basic_table(baseline_libya, "dehydration","pre almanach")

  ## Create respiratory feature
  respiratory_table <- create_basic_table(baseline_libya, "g4_d_respiratory","pre almanach")
  ## Create respiratory feature
  diarrhoea_table <- create_basic_table(baseline_libya, "g4_d_diarrhoea","pre almanach")
  ## Create parasitosis feature
  
  baseline_libya$parasitosis[baseline_libya$g4_d_diarrhoea_type == 'Intestinal Parasitosis'] <- 1

  other_diarrhoea_table <- create_basic_table(baseline_libya, "g4_str_d_other_diarr","pre almanach")

  other_diagnostic_table <- create_basic_table(baseline_libya, "g4_str_d_other","pre almanach")
  
  parasitosis_table <- create_basic_table(baseline_libya, "parasitosis","pre almanach")
  
  baseline_libya$awd[baseline_libya$g4_d_diarrhoea_type == 'Acute Watery Diarrhea/Diarrhea'] <- 1
  awd_table <- create_basic_table(baseline_libya, "awd","pre almanach")

  fever_table <- create_basic_table(baseline_libya, "g4_d_fever","pre almanach")

  ear_table <- create_basic_table(baseline_libya, "g4_d_ear","pre almanach")

  throat_table <- create_basic_table(baseline_libya, "g4_d_throat","pre almanach")

  anemia_table <- create_basic_table(baseline_libya, "g4_d_anemia","pre almanach")

  uti_table <- create_basic_table(baseline_libya, "g4_d_uti","pre almanach")
  
  user_table <- create_basic_table(baseline_libya, "g2_int_user","pre almanach")

  
  # Add skin when using new version
  baseline_libya_numeric <- baseline_libya
  baseline_libya_numeric <- data.frame(sapply(baseline_libya_numeric, as.numeric))

  baseline_libya_numeric[is.na(baseline_libya_numeric)] <- 0
  baseline_libya$comorbidity <- as.numeric(baseline_libya_numeric$g4_d_diarrhoea) + as.numeric(baseline_libya_numeric$g4_d_dehydration) + 
                                  as.numeric(baseline_libya_numeric$g4_d_respiratory) + as.numeric(baseline_libya_numeric$g4_d_fever) +
                                  as.numeric(baseline_libya_numeric$g4_d_ear) + as.numeric(baseline_libya_numeric$g4_d_throat) + 
                                  as.numeric(baseline_libya_numeric$g4_d_anemia) + as.numeric(baseline_libya_numeric$g4_d_uti)
  
  comorbidity_table <- create_basic_table(baseline_libya, "comorbidity", "pre almanach")
  
  baseline_libya$antibiotic_inj[!is.na(baseline_libya$g4_tt_general) & baseline_libya$g4_tt_general != ""] <- 1

  antibiotic_inj_table <- create_basic_table(baseline_libya, "antibiotic_inj", "pre almanach")
  
  baseline_libya$antibiotic_oral[!is.na(baseline_libya$g4_tt_antibio_oral) & baseline_libya$g4_tt_antibio_oral != ""] <- 1
  antibiotic_oral_table <- create_basic_table(baseline_libya, "antibiotic_oral", "pre almanach")
  baseline_libya$antibiotics[baseline_libya$antibiotic_oral == 1 | baseline_libya$antibiotic_inj == 1] <- 1
  antibiotics_table <- create_basic_table(baseline_libya, "antibiotics", "pre almanach")
  
  #We don't have this variable in Libya
  #baseline_libya$tt_ORS[baseline_libya$g4_tt_ORSa == 1 | baseline_libya$g4_tt_ORSb == 1] <- 1 
  #tt_ors_table <- create_basic_table(baseline_libya, "tt_ORS", "pre almanach")

  # We are missing the respiratory one that will come with v2
  baseline_libya$urti_no_antibio[baseline_libya$g4_respiratory=='urti' &  baseline_libya$antibiotics ==1 & 
        (baseline_libya$respiratory == 'pneumonia' | baseline_libya$g4_d_ear_type == 'Acute ear infection with discharge' | 
        baseline_libya$g4_d_throat_type == 'Possible Streptococcus sore throat' |
        baseline_libya$g4_d_diarrhoea_type == 'Severe Persistent Diarrhoea' |
        baseline_libya$g4_d_uti == 1 | (baseline_libya$parasitosis==1 & 
        (baseline_libya$g4_antibio_oral =='metro_ml' | baseline_libya$g4_antibio_oral =='metro_tab' )))]
  ## THIS WILL COME WITH BASELINE V2
  ##skin_table <- create_basic_table(baseline_libya, "g4_d_skin","pre almanach")

  captions = list('User table', 'permission table', 'facility table', 'healthworker table',
  'gender table', 'age table', 'fererral table', 'referral vaccination table',
  'referral hemo table', 'dehydration type table', 'diarrhoea type table',
  'fever type table', 'ear type table', 'throat type table', 'anemia type table',
  'uti type table', 'consultation time table', 'counseling table', 'dehydration table',
  'respiratory table', 'diarrhoea table', 'other diarrhoea table', 'other diagnostic table',
  'parasitosis table', 'awd table', 'fever table', 'ear table', 'throat table',
  'anemia table', 'comorbidity table', 'antibiotic injections table', 
  'antibiotic_oral_table', 'antibiotics table')
  create_report('ALMANACH Libya', 'DHU', 'pdf', list(
    user_table, permission_table,fac_table,hw_table,gender_table,
    age_table, referral_table, referral_vaccination_table,
    referral_hemo_table, dehydration_type_table,
    diarrhea_type_table, fever_type_table, ear_type_table,
    throat_type_table, anemia_type_table, uti_type_table,
    consultation_time_table, counseling_table, dehydration_table,
    respiratory_table, diarrhoea_table, other_diarrhoea_table,
    other_diagnostic_table, parasitosis_table, awd_table, fever_table,
    ear_table, throat_table, anemia_table, comorbidity_table, antibiotic_inj_table,
    antibiotic_oral_table, antibiotics_table), captions)


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

