#### common script to process Canada REDCap data pull ####
# just need to update to most recent CSV file

#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv(
  './inst/extdata/Metformin-AllDataForDavis_DATA_2026-04-27_1052.csv'
)
#Setting Labels
data_labels <- list(
  fxs_sts_id = "FXS/STS ID #",
  redcap_event_name = "Event Name",
  redcap_data_access_group = "Data Access Group",
  rand_inclyn = "Does the participant meet inclusion/exclusion criteria?",
  rand_consent = "Participant provided informed consent?",
  sex = "Gender",
  demo_age = "Age at Visit",
  demo_age_month = "Age at Visit (in months)",
  rand_enroll = "Do you wish to enroll and randomize the participant?",
  rand_code = "Randomization ",
  race___1 = "Race (choice=Caucasian)",
  race___5 = "Race (choice=Native Hawaiian or Other Pacific Islander)",
  race___2 = "Race (choice=Black or African American)",
  race___3 = "Race (choice=Asian)",
  race___4 = "Race (choice=American Indian or Alaska Native)",
  race___8 = "Race (choice=Hispanic or Latino)",
  race___9 = "Race (choice=Not Hispanic or Latino)",
  race___6 = "Race (choice=Other)",
  race___7 = "Race (choice=Unknown / Not Reported)",
  race_spec = "Specify",
  ethn___1 = "Ethnicity (choice=Hispanic or Latino)",
  ethn___2 = "Ethnicity (choice=Chinese)",
  ethn___3 = "Ethnicity (choice=Indian)",
  ethn___4 = "Ethnicity (choice=Japanese)",
  ethn___5 = "Ethnicity (choice=Mixed Ethnicity (Specify))",
  ethn___6 = "Ethnicity (choice=Other)",
  ethn___7 = "Ethnicity (choice=Unknown / Not Reported)",
  ethn_spec = "Specify",
  lang = "Primary language spoken at home?",
  weight_kg = "Weight in kg",
  height_cm = "Height in cm",
  bmi = "Calculated BMI",
  cgis_date_2 = "Date of Visit",
  cgis_soi = "Clincal Global Impression-- Severity of Illness (CGIS_SOI)",
  cgi_date_2 = "Date of Visit",
  cgi_i = "Clincal Global Impression-- Improvement (CGI-I)",
  vas_dov = "Date of VAS",
  vas_tb1_beh = "Severity of Target 1 (Behavior Problems), in cm",
  vas_tb2_lang = "Severity of Target 2 (Language Abilities), in cm",
  vas_tb3_eat = "Severity of Target 3 (Eating Behavior), in cm",
  abc_date2 = "Date of Assessment",
  abc_comp_rev = "ABC Composite Score REVISED",
  ss_i_irrit_rev = "Total - Subscale I (Irritability) REVISED",
  ss_ii_leth_rev = "Total - Subscale II (Lethargy) REVISED",
  ss_iii_stereo_rev = "Total - Subscale III (Stereotypy) REVISED",
  ss_iv_hyper_rev = "Total - Subscale IV (Hyperactivity) REVISED",
  ss_v_inapp_rev = "Total - Subscale V (Inappropriate Speech) REVISED",
  ss_vi_socavoid_rev = "Total - Subscale VI (Social Avoidance) REVISED",
  cshq_date2 = "Date of CSHQ Assessment",
  cshq_1 = "1. Child goes to bed at the same time at night",
  cshq_2 = "2. Child falls asleep within 20 minutes after going to bed",
  cshq_3 = "3. Child falls asleep alone in own bed",
  cshq_4 = "4. Child falls asleep in parents or siblings bed",
  cshq_5 = "5. Child needs parent in the room to fall asleep",
  cshq_6 = "6. Child struggles at bedtime (cries, refuses to stay in bed, etc.)",
  cshq_7 = "7. Child is afraid of sleeping in the dark",
  cshq_8 = "8. Child is afraid of sleeping alone",
  cshq_9 = "9. Child sleeps too little",
  cshq_10 = "10. Child sleeps the right amount",
  cshq_11 = "11. Child sleeps about the same amount each day",
  cshq_12 = "12. Child wets the bed at night",
  cshq_13 = "13. Child talks during sleep",
  cshq_14 = "14. Child is restless and moves a lot during sleep",
  cshq_15 = "15. Child sleepwalks during the night",
  cshq_16 = "16. Child moves to someone elses bed during the night (parent, brother, sister, etc.)",
  cshq_17 = "17. Child grinds teeth during sleep (your dentist may have told you this)",
  cshq_18 = "18. Child snores loudly",
  cshq_19 = "19. Child seems to stop breathing during sleep",
  cshq_20 = "20. Child snorts and/or gasps during sleep",
  cshq_21 = "21. Child has trouble sleeping away from home (visiting relatives, vacation)",
  cshq_22 = "22. Child awakens during night screaming, sweating, and inconsolable",
  cshq_23 = "23. Child awakens alarmed by a frightening dream",
  cshq_24 = "24. Child awakes once during the night",
  cshq_25 = "25. Child awakes more than once during the night",
  cshq_26 = "26. Child wakes up by him/herself",
  cshq_27 = "27. Child wakes up in negative mood",
  cshq_28 = "28. Adults or siblings wake up child",
  cshq_29 = "29. Child has difficulty getting out of bed in the morning",
  cshq_30 = "30. Child takes a long time to become alert in the morning",
  cshq_31 = "31. Child seems tired",
  cshq_32 = "32. Watching TV",
  cshq_33 = "33. Riding a car",
  pql_date2 = "Date of PedsQL Assessment",
  pql_pf_tot = "Physical Functioning Total",
  pql_ef_tot = "Emotional Functioning Total",
  pql_socf_tot = "Social Functioning Total",
  pql_schf_tot = "School Functioning Total",
  q3 = "Age",
  qq1 = "1. Often fails to give close attention to details or makes careless mistakes in schoolwork or tasks",
  qq2 = "2. Often has difficulty sustaining attention in tasks or play activities",
  qq3 = "3. Often does not seem to listen when spoken to directly",
  qq4 = "4. Often does not follow through on instructions and fails to finish schoolwork, chores, or duties",
  qq5 = "5. Often has difficulty organizing tasks and activities",
  qq6 = "6. Often avoids, dislikes, or reluctantly engages in tasks requiring sustained mental effort",
  qq7 = "7. Often loses things necessary for activities (e.g., toys, school assignments, pencils, or books)",
  qq8 = "8. Often is distracted by extraneous stimuli",
  qq9 = "9. Often is forgetful in daily activities",
  qq11 = "11. Often fidgets with hands or feet or squirms in seat",
  qq12 = "12. Often leaves seat in classroom or in other situations in which remaining seated is expected",
  qq13 = "13. Often runs about or climbs excessively in situations in which it is inappropriate",
  qq14 = "14. Often has difficulty playing or engaging in leisure activities quietly",
  qq15 = "15. Often is on the go or often acts as if driven by a motor",
  qq16 = "16. Often talks excessively",
  qq17 = "17. Often blurts out answers before questions have been completed",
  qq18 = "18. Often has difficulty awaiting turn",
  qq19 = "19. Often interrupts or intrudes on others (e.g., butts into conversations/games)",
  qq21 = "21. Often loses temper",
  qq22 = "22. Often argues with adults",
  qq23 = "23. Often actively defies or refuses adult requests or rules",
  qq24 = "24. Often deliberately does things that annoy other people",
  qq25 = "25. Often blames others for his or her mistakes or misbehavior",
  qq26 = "26. Often touchy or easily annoyed by others",
  qq27 = "27 Often is angry and resentful",
  qq28 = "28. Often is spiteful or vindictive",
  p6 = "Age",
  man_hyp_tot = "Manic / Hyperactive Behavior total",
  depres_tot = "Depressed Mood total",
  soc_avoid_tot = "Social Avoidance total",
  gen_anx_tot = "General Anxiety total",
  ocb_tot = "Obsessive / compulsive behavior total",
  a2 = "Completion Date",
  z2 = "Social Affect (SA) Total",
  z3 = "Restricted and Repetitive Behaviour (RRB) Total",
  z4 = "Overall Total (SA + RRB)",
  z6 = "ADOS-2 Comparison Score",
  a2_v2 = "Completion Date",
  z2_v2 = "Social Affect (SA) Total",
  z3_v2 = "Restricted and Repetitive Behaviour (RRB) Total",
  z4_v2 = "Overall Total (SA + RRB)",
  z6_v2 = "ADOS-2 Comparison Score",
  a2_v2_v2 = "Completion Date",
  z2_v2_v2 = "Social Affect (SA) Total",
  z3_v2_v2 = "Restricted and Repetitive Behaviour (RRB) Total",
  z4_v2_v2 = "Overall Total (SA + RRB)",
  z6_v2_v2 = "ADOS-2 Comparison Score",
  a2_v2_v2_v2 = "Completion Date",
  z2_v2_v2_v2 = "Communication Total",
  z3_v2_v2_v2 = "Social Interaction Total",
  z4_v2_v2_v2 = "Communication + Social Interaction Total",
  z6_v2_v2_v2 = "Stereotyped Behaviours and Restricted Interests Total",
  leit3_date = "Date of Leiter-3 Administration",
  leit3_nv_iq = "Nonverbal IQ/Composite Score",
  vabs3_date = "Date Administered",
  vabs3_rc_rs = "Receptive Raw Score",
  vabs3_rc_ss = "Receptive v-Scale Score",
  vabs3_ec_rs = "Expressive Raw Score",
  vabs3_ec_ss = "Expressive Scale Score",
  vabs3_wc_rs = "Written  Raw Score",
  vabs3_wc_ss = "Written Scale Score",
  vabs3_pd_rs = "Personal Raw Score",
  vabs3_pd_ss = "Personal Scale Score",
  vabs3_dd_rs = "Domestic Raw Score",
  vabs3_dd_ss = "Domestic Scale Score",
  vabs3_cd_rs = "Community Raw Score",
  vabs3_cd_ss = "Community Scale Score",
  vabs3_is_rs = "Interpersonal Relationships Raw Score",
  vabs3_is_ss = "Interpersonal Relationships Scale Score",
  vabs3_ps_rs = "Play & Leisure Time Raw Score",
  vabs3_ps_ss = "Play & Leisure Time Scale Score",
  vabs3_cs_rs = "Coping Skills Raw Score",
  vabs3_cs_ss = "Coping Skills Scale Score",
  vabs3_gm_rs = "Gross Motor Raw Score",
  vabs3_gm_ss = "Gross Motor Scale Score",
  vabs3_fm_rs = "Fine Motor Raw Score",
  vabs3_fm_ss = "Fine Motor Scale Score",
  vabs3_ac_ss = "Adaptive Behavior Composite Standard Score",
  vabs3_c_ss = "Communication Standard Score",
  vabs3_dl_ss = "Daily Living Skills Standard Score",
  vabs3_s_ss = "Socialization Standard Score",
  vabs3_m_ss = "Motor Skills Scale Score",
  nar_doe = "Date of Evaluation",
  nar_order = "What was the order of administration of Conversation/Narrative Assessments?",
  nar_ndw50_a = "Number of Different Words 50 - Analysis Set",
  nar_mlum_a = "MLU Morphemes - Analysis Set",
  nar_ndw_a = "Number of Different Words - Analysis Set",
  nar_utpermin = "Utterances per Min",
  nar_perc_intellig_t = "% Intelligible - Total",
  nar_perc_unintelut_t = "% Unintelligible Utterances - Total",
  con_doe = "Date of Evaluation",
  con_order = "What was the order of administration of Conversation/Narrative Assessments?",
  con_ndw50_a = "Number of Different Words 50 - Analysis Set",
  con_ndw_a = "Number of Different Words - Analysis Set",
  con_utpermin = "Utterances per Min",
  con_perc_intellig_t = "% Intelligible - Total",
  con_perc_unintelut_t = "% Unintelligible Utterances - Total",
  tcb_admin_2 = "Date of administration",
  tcb_flnkr_valid = "NIH Toolbox Flanker Inhibitory Control and Attn Test valid?",
  tcb_vocb_valid = "NIH Toolbox Picture Vocabulary Test valid?",
  tcb_dccs_valid = "NIH Toolbox Dimensional Change Card Sort Test valid?",
  psm_form_v2 = "Which form version (A or B) was administered?",
  tcb_picseq_valid = "NIH Toolbox Picture Sequence Memory Test valid?",
  tcb_picseq_admin56 = "Was Picture Sequence Memory Age 5-6 administered after Age 3-4? ",
  tcb_picseq_form_admin1 = "Which version of Picture Sequence Memory was administered first?",
  psm_form_v2_56 = "Which form version (A or B) was administered?",
  tcb_picseq_valid56 = "NIH Toolbox Picture Sequence Memory Age 5-6 valid?",
  tcb_lstsrt_valid = "NIH Toolbox List Sorting Working Memory Test valid?",
  tcb_spdmtch_valid = "NIH Toolbox Speeded Matching Test valid?",
  tcb_ptrncomp_valid = "NIH Toolbox Pattern Comparison Processing Speed Test valid?",
  tcb_read_valid = "NIH Toolbox Oral Reading Recognition Test valid?",
  mem_game1 = "Participant Code",
  mem_game3 = "Date",
  mem_gametime = "Time",
  mem_game4 = "Paired Association Raw Score",
  mem_game5 = "Paired Association Reaction Time Average",
  mem_game6 = "Recognition Raw Score",
  mem_game7 = "Recognition Reaction Time Average",
  mem_game9 = "Day 2 Complete",
  mem_game10 = "Date",
  mem_gametime_2 = "Time",
  mem_gameincomplete_2 = "Incomplete Reason",
  mem_game11 = "Paired Association Raw Score",
  mem_game12 = "Paired Association Reaction Time Average",
  mem_game13 = "Recognition Raw Score",
  mem_game14 = "Recognition Reaction Time Average",
  mem_game15 = "Comments (if participant paid attention, got distracted, knew what they were supposed to do, or not, etc.)",
  mem_game16 = "Time between each session for the first administration of the game:",
  mem_game17 = "Has the data for both days been graphed?",
  mem_game18 = "If yes, where is the graph located and what is the document titled?",
  mem_game19 = "If no, why not?",
  mem_game1_v2 = "Participant Code",
  mem_game3_v2 = "Date",
  mem_gametime_v2 = "Time",
  mem_game4_v2 = "Paired Association Raw Score",
  mem_game5_v2 = "Paired Association Reaction Time Average",
  mem_game6_v2 = "Recognition Raw Score",
  mem_game7_v2 = "Recognition Reaction Time Average",
  mem_game9_v2 = "Day 2 Complete",
  mem_game10_v2 = "Date",
  mem_gametime_2_v2 = "Time",
  mem_gameincomplete_2_v2 = "Incomplete Reason",
  mem_game11_v2 = "Paired Association Raw Score",
  mem_game12_v2 = "Paired Association Reaction Time Average",
  mem_game13_v2 = "Recognition Raw Score",
  mem_game14_v2 = "Recognition Reaction Time Average",
  mem_game15_v2 = "Comments (if participant paid attention, got distracted, knew what they were supposed to do, or not, etc.)",
  mem_game16_v2 = "Time between each session for the first administration of the game:",
  mem_game17_v2 = "Has the data for both days been graphed?",
  mem_game18_v2 = "If yes, where is the graph located and what is the document titled?",
  mem_game19_v2 = "If no, why not?",
  mem_game20 = "Was the game completed twice, approximately 4 months apart (before and after Metformin treatment)?",
  mem_game21 = "If yes, time between each administration of the game:",
  mem_game22 = "If no, why not?",
  nar_perc_partintelut_t = "% Partially Unintellligible Utterances - Total",
  con_perc_partintelut_t = "% Partially Unintellligible Utterances - Total",
  pql_pf_1 = "1. Walking more than one block",
  pql_pf_2 = "2. Running",
  pql_pf_3 = "3. Participating in sports activity or exercise",
  pql_pf_4 = "4. Lifting something heavy",
  pql_pf_5 = "5. Taking a bath or shower by him or herself",
  pql_pf_7 = "7. Having hurts or aches",
  pql_pf_8 = "8. Low energy level",
  pql_ef_1 = "1. Feeling afraid or scared",
  pql_ef_2 = "2. Feeling sad or blue",
  pql_ef_3 = "3. Feeling angry",
  pql_ef_4 = "4. Trouble sleeping",
  pql_ef_5 = "5. Worrying about what will happen to him or her",
  pql_socf_1 = "1. Getting along with other children",
  pql_socf_2 = "2. Other kids not wanting to be his or her friend",
  pql_socf_3 = "3. Getting teased by other children",
  pql_socf_4 = "4. Not able to do things that other children his or her age can do",
  pql_socf_5 = "5. Keeping up when playing with other children",
  pql_schf_1 = "1. Paying attention in class",
  pql_schf_2 = "2. Forgetting things",
  pql_schf_3 = "3. Keeping up with schoolwork",
  pql_schf_4 = "4. Missing school because of not feeling well",
  pql_schf_5 = "5. Missing school to go to the doctor or hospital",
  nar_perc_utwmz_t = "% Utterances with Mazes - Total",
  con_perc_utwmz_t = "% Utterances with Mazes - Total",
  con_mlum_a = "MLU Morphemes - Analysis Set",
  nar_totnumut_a = "Total Number of Utterances - Analysis Set",
  nar_totnumut_t = "Total Number of Utterances - Total",
  nar_totverbalut_t = "Total Verbal Utterances - Total",
  nar_nonverbalut_t = "Total Nonverbal Utterances - Total",
  nar_perc_totverbalut_t = "% Verbal Utterances - Total",
  nar_perc_nonverbalut_t = "% Nonverbal Utterances - Total",
  con_totnumut_a = "Total Number of Utterances - Analysis Set",
  con_totnumut_t = "Total Number of Utterances - Total",
  con_totverbalut_t = "Total Verbal Utterances - Total",
  con_nonverbalut_t = "Total Nonverbal Utterances - Total",
  con_perc_totverbalut_t = "% Verbal Utterances - Total",
  con_perc_nonverbalut_t = "% Nonverbal Utterances - Total"
)


#Setting Factors
data$redcap_event_name = factor(
  data$redcap_event_name,
  levels=c(
    "baseline_visit_vis_arm_1",
    "week_8_visit_visit_arm_1",
    "week_16_visit_3ear_arm_1"
  )
)
data$rand_inclyn = factor(data$rand_inclyn,levels=c("1","0"))
data$sex = factor(data$sex,levels=c("1","2"))
data$race___1 = factor(data$race___1,levels=c("0","1"))
data$race___5 = factor(data$race___5,levels=c("0","1"))
data$race___2 = factor(data$race___2,levels=c("0","1"))
data$race___3 = factor(data$race___3,levels=c("0","1"))
data$race___4 = factor(data$race___4,levels=c("0","1"))
data$race___8 = factor(data$race___8,levels=c("0","1"))
data$race___9 = factor(data$race___9,levels=c("0","1"))
data$race___6 = factor(data$race___6,levels=c("0","1"))
data$race___7 = factor(data$race___7,levels=c("0","1"))
data$ethn___1 = factor(data$ethn___1,levels=c("0","1"))
data$ethn___2 = factor(data$ethn___2,levels=c("0","1"))
data$ethn___3 = factor(data$ethn___3,levels=c("0","1"))
data$ethn___4 = factor(data$ethn___4,levels=c("0","1"))
data$ethn___5 = factor(data$ethn___5,levels=c("0","1"))
data$ethn___6 = factor(data$ethn___6,levels=c("0","1"))
data$ethn___7 = factor(data$ethn___7,levels=c("0","1"))
data$cgi_i = factor(data$cgi_i,levels=c("1","2","3","4","5","6","7","999","888"))
data$cgis_soi = factor(data$cgis_soi,levels=c("0","1","2","3","4","5","6","7"))
data$mem_game9 = factor(data$mem_game9,levels=c("1","0"))
data$mem_game17 = factor(data$mem_game17,levels=c("1","0"))
data$mem_game9_v2 = factor(data$mem_game9_v2,levels=c("1","0"))
data$mem_game17_v2 = factor(data$mem_game17_v2,levels=c("1","0"))
data$mem_game20 = factor(data$mem_game20,levels=c("1","0"))

data$tcb_flnkr_valid = factor(data$tcb_flnkr_valid,levels=c("1","0","2"))
data$tcb_vocb_valid = factor(data$tcb_vocb_valid,levels=c("1","0","2"))
data$tcb_dccs_valid = factor(data$tcb_dccs_valid,levels=c("1","0","2"))
data$tcb_picseq_valid = factor(data$tcb_picseq_valid,levels=c("1","0","2"))
data$tcb_picseq_valid56 = factor(data$tcb_picseq_valid56,levels=c("1","2","3"))
data$tcb_lstsrt_valid = factor(data$tcb_lstsrt_valid,levels=c("1","0","2"))
data$tcb_spdmtch_valid = factor(data$tcb_spdmtch_valid,levels=c("1","0","2"))
data$tcb_ptrncomp_valid = factor(data$tcb_ptrncomp_valid,levels=c("1","0","2"))
data$tcb_read_valid = factor(data$tcb_read_valid,levels=c("1","0","2"))

data$tcb_picseq_form_admin1 = factor(data$tcb_picseq_form_admin1,levels=c("1","2","3"))
data$tcb_picseq_admin56 = factor(data$tcb_picseq_admin56,levels=c("1","0"))

levels(data$redcap_event_name) = c(
  "Baseline Visit (Visit 1)",
  "Week 8 Visit (Visit 2)",
  "Week 16 (Visit 3/Early Term)"
)
levels(data$rand_inclyn) = c("Yes","No")
levels(data$sex) = c("Male","Female")
levels(data$race___1) = c("Unchecked","Checked")
levels(data$race___5) = c("Unchecked","Checked")
levels(data$race___2) = c("Unchecked","Checked")
levels(data$race___3) = c("Unchecked","Checked")
levels(data$race___4) = c("Unchecked","Checked")
levels(data$race___8) = c("Unchecked","Checked")
levels(data$race___9) = c("Unchecked","Checked")
levels(data$race___6) = c("Unchecked","Checked")
levels(data$race___7) = c("Unchecked","Checked")
levels(data$ethn___1) = c("Unchecked","Checked")
levels(data$ethn___2) = c("Unchecked","Checked")
levels(data$ethn___3) = c("Unchecked","Checked")
levels(data$ethn___4) = c("Unchecked","Checked")
levels(data$ethn___5) = c("Unchecked","Checked")
levels(data$ethn___6) = c("Unchecked","Checked")
levels(data$ethn___7) = c("Unchecked","Checked")
levels(data$cgi_i) = c("1=Very Much Improved","2=Much Improved","3=Minimally Improved","4=No Change","5=Minimally Worse","6=Much Worse","7=Very Much Worse","999=No Data","888=NA")
levels(data$cgis_soi) = c("0 = Not Assessed","1 = Normal","2 = Borderline Ill","3 = Mildly Ill","4 = Moderately Ill","5 = Markedly Ill","6 = Severely Ill","7 = Among the Most Extremely Ill Patients")
levels(data$mem_game9) = c("Yes","No")
levels(data$mem_game17) = c("Yes","No")
levels(data$mem_game9_v2) = c("Yes","No")
levels(data$mem_game17_v2) = c("Yes","No")
levels(data$mem_game20) = c("Yes","No")

levels(data$tcb_flnkr_valid) = c("Yes","No","?")
levels(data$tcb_vocb_valid) = c("Yes","No","?")
levels(data$tcb_dccs_valid) = c("Yes","No","?")
levels(data$tcb_picseq_valid) = c("Yes","No","?")
levels(data$tcb_picseq_valid56) = c("Yes","No","?")
levels(data$tcb_lstsrt_valid) = c("Yes","No","?")
levels(data$tcb_spdmtch_valid) = c("Yes","No","?")
levels(data$tcb_ptrncomp_valid) = c("Yes","No","?")
levels(data$tcb_read_valid) = c("Yes","No","?")

levels(data$tcb_picseq_form_admin1) = c("Age 3-4","Age 7","Age 8+")
levels(data$tcb_picseq_admin56) = c("Yes","No")
