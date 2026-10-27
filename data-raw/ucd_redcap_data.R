#### common script to process REDCap data pull ####
# just need to update to most recent CSV file

#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv(
  './inst/extdata/CTSC4328ADoubleBlind-METUCDJan2025Copy_DATA_2026-04-27_0949.csv'
)
#Setting Labels

data_labels <- list(
  fxs_sts_id = "FXS/STS ID #",
  redcap_event_name = "Event Name",
  screen_fail = "Did participant screen fail",
  childstudyid = "Participant ID #",
  demo_age = "Age at Visit",
  demo_age_month = "Age at Visit (in months)",
  sex = "Gender",
  race___1 = "Race (choice=Caucasian)",
  race___2 = "Race (choice=Black or African American)",
  race___3 = "Race (choice=Asian)",
  race___4 = "Race (choice=Native American/Alaska Native)",
  race___5 = "Race (choice=Native Hawaiian or Other Pacific Islander)",
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
  profile_studyid = "Participant ID",
  profile_language = "Primary language spoken at home?",
  cgi_init = "Raters Initials",
  cgi_i = "Clincal Global Impression-- Improvement (CGI-I)",
  cgis_init = "Raters Initials",
  cgis_soi = "Clincal Global Impression-- Severity of Illness (CGIS_SOI)",
  pql_pf_tot = "Physical Functioning Total",
  pql_ef_tot = "Emotional Functioning Total",
  pql_socf_tot = "Social Functioning Total",
  pql_schf_tot = "School Functioning Total",
  pql_pf_tot_cor = "Physical Functioning Total Corrected",
  pql_ef_tot_cor = "Emotional functioning",
  pql_socf_tot_cor = "Social Functioning Total Corrected",
  pql_schf_tot_cor = "School functioning Total Corrected",
  weight_kg = "Weight in kg",
  height_cm = "Height in cm",
  bmi = "Calculated BMI",
  snap_adhd_combtot = "ADHD Combined Total",
  snap_odd_tot = "ODD Total",
  abc_comp_rev = "ABC Composite Score-- REVISED",
  ss_i_irrit_rev = "Subscale I (Irritability)",
  ss_ii_leth_rev = "Subscale II (Socially Unresponsive/Lethargic)",
  ss_iv_hyper_rev = "Subscale IV (Hyperactivity)",
  ss_iii_stereo_rev = "Subscale III (Stereotypy)",
  ss_v_inapp_rev = "Subscale V (Inappropriate Speech)",
  ss_vi_socavoid_rev = "Subscale VI (Social Avoidance)",
  adams_manic = "Manic/Hyperactive Behavior Total",
  adams_depress = "Depressed Mood Total",
  adams_avoid = "Social Avoidance Total",
  adams_anxious = "General Anxiety Total",
  adams_obsess = "Obsessive/Compulsive Behavior Total",
  ados_age = "Age at Visit",
  a2_cs_as = "ADOS Severity",
  a2_cs_acs = "ADOS Comparison Score",
  a2_m1_satotal = "Social Affect Total",
  a2_m1_rrbtotal = "Restricted and Repetitive Behaviors Total",
  a2_m1_total = "Social Affect + Restricted and Repetitive Behaviors Total",
  a2_m2_satotal = "Social Affect Total",
  a2_m2_rrbtotal = "Restricted and Repetitive Behaviors Total",
  a2_m2_total = "Social Affect + Restricted and Repetitive Behaviors Total",
  a2_m3_satotal = "Social Affect Total",
  a2_m3_rrbtotal = "Restricted and Repetitive Behavior Total",
  a2_m3_total = "Social Affect + Restricted and Repetitive Behaviors Total",
  a2_m4_sbri_total = "Stereotyped Behavior and Restricted Interests Total",
  a2_m4_comtotal = "Communication Total",
  a2_m4_rsi_total = "Social Interaction Total",
  a2_m4_csi_total = "Communication + Social Interaction Total",
  leit3_fg_rs = "Figure Ground Raw Score",
  leit3_fc_raw1_8 = "FC1-FC8 Raw Score",
  leit3_fc_raw9_15 = "FC9-FC15 Raw Score",
  leit3_fc_rstotal = "Total Form Completion Raw Score",
  leit3_ca_rs1_7 = "CA1-CA7 Raw Score",
  leit3_ca_rs8_17 = "CA8-CA17 Raw Score",
  leit3_so_rs = "Sequential Order Total Raw Score",
  leit3_vp_rs = "VP1-VP10 Raw Score",
  leit3_fg_sc_rs = "Figure Ground Raw Score",
  leit3_fc_sc_rs = "Form Completion Raw Score",
  leit3_ca_sc_rs = "Classification and Analogies Raw Score",
  leit3_so_sc_rs = "Sequential Order Raw Score",
  leit3_vp_sc_rs = "Visual Patterns Raw Score (optional)",
  leit3_fg_sc_ss = "Figure Ground Scaled Score",
  leit3_fc_sc_ss = "Form Completion Scaled Score",
  leit3_ca_sc_ss = "Classification and Analogies Scaled Score",
  leit3_so_sc_ss = "Sequential Order Scaled Score",
  leit3_vp_sc_ss = "Visual Patterns Scaled Score (optional)",
  leit3_sum_sc_ss = "Sum of (4) Scaled Scores",
  leit3_nv_iq = "Nonverbal IQ/Composite Score",
  vabs3_rc_rs = "Receptive Raw Score",
  vabs3_rc_ss = "Receptive Scale Score",
  vabs3_ec_rs = "Expressive Raw Score",
  vabs3_ec_ss = "Expressive Scale Score",
  vabs3_wc_rs = "Written  Raw Score",
  vabs3_wc_ss = "Written Scale Score",
  vabs3_c_ss = "Communication Standard Score",
  vabs3_pd_rs = "Personal Raw Score",
  vabs3_pd_ss = "Personal Scale Score",
  vabs3_dd_rs = "Domestic Raw Score",
  vabs3_dd_ss = "Domestic Scale Score",
  vabs3_cd_rs = "Community Raw Score",
  vabs3_cd_ss = "Community Scale Score",
  vabs3_dl_ss = "Daily Living Skills Standard Score",
  vabs3_is_rs = "Interpersonal Relationships Raw Score",
  vabs3_is_ss = "Interpersonal Relationships Scale Score",
  vabs3_ps_rs = "Play & Leisure Time Raw Score",
  vabs3_ps_ss = "Play & Leisure Time Scale Score",
  vabs3_cs_rs = "Coping Skills Raw Score",
  vabs3_cs_ss = "Coping Skills Scale Score",
  vabs3_s_ss = "Socialization Standard Score",
  vabs3_gm_rs = "Gross Motor Raw Score",
  vabs3_gm_ss = "Gross Motor Scale Score",
  vabs3_fm_rs = "Fine Motor Raw Score",
  vabs3_fm_ss = "Fine Motor Scale Score",
  vabs3_m_ss = "Motor Skills Scale Score",
  vabs3_sa_rs = "Sum of Domain Scale Scores",
  vabs3_ac_ss = "Adaptive Behavior Composite Standard Score",
  tassone_cat = "Molecular category",
  tassone_cgg = "CGG repeat number",
  tassone_meth = "% Methylation",
  tassone_ar = "Activation Ratio",
  tassone_fmr1 = "FMR1 mRNA levels",
  tassone_fmr1_se = "FMR1 mRNA Standard Error",
  tassone_cyfip1 = "CYFIP1 mRNA levels",
  tassone_cyfip1_se = "CYFIP1 mRNA Standard Error",
  tassone_cyp450_1 = "CYP450 allelic variant 1",
  tassone_cyp450_2 = "CYP450 allelic variant 2",
  tassone_cyp450_3 = "CYP450 allelic variant 3",
  tassone_mmp9 = "MMP9 normalized ratio",
  tassone_s6k = "S6K ratio",
  tassone_eif4e = "EIF 4e ratio",
  fmrp_fxsid = "STS/ FXSID",
  fmrp_rhmc = "FMRP (relative to history mean of controls)",
  fmrp_std_rhmc = "FMRP STD (relative to historic mean of controls)",
  fmrp_se_rhmc = "FMRP Standard Error (relative to historic mean of controls)",
  fmrp_cv = "FMRP CV",
  vas_init = "Rater initials",
  vas_tb1_beh = "Severity of Target 1 (Behavior Problems), in cm",
  vas_tb2_lang = "Severity of Target 2 (Language Abilities), in cm",
  vas_tb3_eat = "Severity of Target 3 (Eating Behavior), in cm",
  cshq_1 = "1. Child goes to bed at the same time at night (R)",
  cshq_2 = "2. Child falls asleep within 20 minutes after going to bed (R)",
  cshq_3 = "3. Child falls asleep alone in own bed (R)",
  cshq_4 = "4. Child falls asleep in parents or siblings bed",
  cshq_5 = "5. Child needs parent in the room to fall asleep",
  cshq_6 = "6. Child struggles at bedtime (cries, refuses to stay in bed, etc.)",
  cshq_7 = "7. Child is afraid of sleeping in the dark",
  cshq_8 = "8. Child is afraid of sleeping alone",
  cshq_9 = "9. Child sleeps too little",
  cshq_10 = "10. Child sleeps the right amount (R)",
  cshq_11 = "11. Child sleeps about the same amount each day (R)",
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
  cshq_26 = "26. Child wakes up by him/herself (R)",
  cshq_27 = "27. Child wakes up in negative mood",
  cshq_28 = "28. Adults or siblings wake up child",
  cshq_29 = "29. Child has difficulty getting out of bed in the morning",
  cshq_30 = "30. Child takes a long time to become alert in the morning",
  cshq_31 = "31. Child seems tired",
  cshq_32 = "32. Watching TV",
  cshq_33 = "33. Riding a car",
  cshq_ts = "CSHQ Total score",
  nar_doe = "Date of Evaluation - Narrative",
  nar_order = "What was the order of administration of Conversation/Narrative Assessments?",
  nar_ndw50_a = "Number of Different Words 50 - Narrative Analysis Set",
  nar_ndw_a = "Number of Different Words - Narrative Analysis Set",
  nar_utpermin = "Utterances per Min - Narrative",
  nar_mlum_a = "MLU Morphemes - Narrative Analysis Set",
  nar_perc_intellig_t = "% Intelligible - Narrative Total",
  nar_perc_utwmz_t = "% Utterances with Mazes - Narrative Total",
  con_doe = "Date of Evaluation - Conversation",
  con_order = "What was the order of administration of Conversation/Narrative Assessments?",
  con_ndw50_a = "Number of Different Words 50 - Conversation Analysis Set",
  con_ndw_a = "Number of Different Words - Conversation Analysis Set",
  con_utpermin = "Utterances per Min - Conversation",
  con_mlum_a = "MLU Morphemes - Conversation Analysis Set",
  con_perc_intellig_t = "% Intelligible - Conversation Total",
  con_perc_utwmz_t = "% Utterances with Mazes - Conversation Total",
  dob = "Date of birth",
  vit_date = "Date of collection",
  cgi_date_2 = "Date of Visit",
  cgis_date_2 = "Date of Visit",
  vas_dov = "Date of VAS",
  cshq_date2 = "Date of CSHQ Assessment",
  pql_date2 = "Date of PedsQL Assessment",
  snap_date_2 = "SNAP-IV Date",
  abc_date2 = "Date of Assessment",
  adams_date2 = "Date of Assessment",
  leit3_date = "Date of Leiter-3 Administration",
  vabs3_date = "Date Administered",
  mem_game1 = "Participant Code",
  mem_game2 = "Day 1 Complete",
  mem_game3 = "Date",
  mem_gametime = "Time",
  mem_gameincomplete = "Incomplete Reason",
  mem_game4 = "Paired Association Raw Score",
  mem_game5 = "Paired Association Reaction Time Average",
  mem_game6 = "Recognition Raw Score",
  mem_game7 = "Recognition Reaction Time Average",
  mem_game8 = "Comments (if participant paid attention, got distracted, knew what they were supposed to do, or not, etc.)",
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
  memory_game_visit_1_complete = "Complete?",
  mem_game1_v2 = "Participant Code",
  mem_game2_v2 = "Day 1 Complete",
  mem_game3_v2 = "Date",
  mem_gametime_v2 = "Time",
  mem_gameincomplete_v2 = "Incomplete Reason",
  mem_game4_v2 = "Paired Association Raw Score",
  mem_game5_v2 = "Paired Association Reaction Time Average",
  mem_game6_v2 = "Recognition Raw Score",
  mem_game7_v2 = "Recognition Reaction Time Average",
  mem_game8_v2 = "Comments (if participant paid attention, got distracted, knew what they were supposed to do, or not, etc.)",
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
  memory_game_visit_3_complete = "Complete?",
  nar_perc_unintelut_t = "% Unintelligible Utterances - Total",
  nar_perc_partintelut_t = "% Partially Unintellligible Utterances - Total",
  con_perc_unintelut_t = "% Unintelligible Utterances - Total",
  con_perc_partintelut_t = "% Partially Unintellligible Utterances - Total",
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
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_event_name = factor(
  data$redcap_event_name,
  levels=c("baseline_visit_vis_arm_1", "week_8_visit_visit_arm_1",
           "week_16_visit_3ear_arm_1")
)
data$screen_fail = factor(data$screen_fail,levels=c("1","0"))
data$sex = factor(data$sex,levels=c("1","2"))
data$race___1 = factor(data$race___1,levels=c("0","1"))
data$race___2 = factor(data$race___2,levels=c("0","1"))
data$race___3 = factor(data$race___3,levels=c("0","1"))
data$race___4 = factor(data$race___4,levels=c("0","1"))
data$race___5 = factor(data$race___5,levels=c("0","1"))
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
data$a2_cs_as = factor(data$a2_cs_as,levels=c("0","1","2"))
data$mem_game2 = factor(data$mem_game2,levels=c("1","0"))
data$mem_game9 = factor(data$mem_game9,levels=c("1","0"))
data$mem_game17 = factor(data$mem_game17,levels=c("1","0"))
data$memory_game_visit_1_complete = factor(data$memory_game_visit_1_complete,levels=c("0","1","2"))
data$mem_game2_v2 = factor(data$mem_game2_v2,levels=c("1","0"))
data$mem_game9_v2 = factor(data$mem_game9_v2,levels=c("1","0"))
data$mem_game17_v2 = factor(data$mem_game17_v2,levels=c("1","0"))
data$mem_game20 = factor(data$mem_game20,levels=c("1","0"))
data$memory_game_visit_3_complete = factor(data$memory_game_visit_3_complete,levels=c("0","1","2"))

levels(data$redcap_event_name)=c("Baseline Visit (Visit 1)","Week 8 Visit (Visit 2)","Week 16 (Visit 3/Early Term)")
levels(data$screen_fail)=c("Yes","No")
levels(data$sex)=c("Male","Female")
levels(data$race___1)=c("Unchecked","Checked")
levels(data$race___2)=c("Unchecked","Checked")
levels(data$race___3)=c("Unchecked","Checked")
levels(data$race___4)=c("Unchecked","Checked")
levels(data$race___5)=c("Unchecked","Checked")
levels(data$race___6)=c("Unchecked","Checked")
levels(data$race___7)=c("Unchecked","Checked")
levels(data$ethn___1)=c("Unchecked","Checked")
levels(data$ethn___2)=c("Unchecked","Checked")
levels(data$ethn___3)=c("Unchecked","Checked")
levels(data$ethn___4)=c("Unchecked","Checked")
levels(data$ethn___5)=c("Unchecked","Checked")
levels(data$ethn___6)=c("Unchecked","Checked")
levels(data$ethn___7)=c("Unchecked","Checked")
levels(data$cgi_i)=c("1=Very Much Improved","2=Much Improved","3=Minimally Improved","4=No Change","5=Minimally Worse","6=Much Worse","7=Very Much Worse","999=No Data","888=NA")
levels(data$cgis_soi)=c("0 = Not Assessed","1 = Normal","2 = Borderline Ill","3 = Mildly Ill","4 = Moderately Ill","5 = Markedly Ill","6 = Severely Ill","7 = Among the Most Extremely Ill Patients")
levels(data$a2_cs_as)=c("No/mild","Moderate","High")
levels(data$mem_game2)=c("Yes","No")
levels(data$mem_game9)=c("Yes","No")
levels(data$mem_game17)=c("Yes","No")
levels(data$memory_game_visit_1_complete)=c("Incomplete","Unverified","Complete")
levels(data$mem_game2_v2)=c("Yes","No")
levels(data$mem_game9_v2)=c("Yes","No")
levels(data$mem_game17_v2)=c("Yes","No")
levels(data$mem_game20)=c("Yes","No")
levels(data$memory_game_visit_3_complete)=c("Incomplete","Unverified","Complete")
