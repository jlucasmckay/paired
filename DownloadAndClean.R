# DownloadAndClean.R
library(motionTools)
library(tidyverse)


# The project redcap has been configured with three "Arms," we are interested in the randomized phase.
redcap.arms = c("Arm 1: Focus Group","Arm 2: Screening and Informed Consent","Arm 3: Randomized Phase")

# The project redcap has been configured so that there are four possible participant codes.
participant.codes = c("PRR","SF","PID","FG")

# The project has multiple unique events; select only those in the Randomized phase.
event.names = c("Baseline (Arm 3: Randomized Phase)",
                "Randomization (Arm 3: Randomized Phase)",
                "Intervention (Arm 3: Randomized Phase)",
                "3-month (Arm 3: Randomized Phase)",
                "Monthly Check-in (Arm 3: Randomized Phase)",
                "7-8 Month Screening (Arm 3: Randomized Phase)",
                "12-month (Arm 3: Randomized Phase)")

# Function to download Redcap report
DownloadReport = function(report_id='30166',rawOrLabelHeaders='label',rawOrLabel='label'){
  token = Sys.getenv("partner_redcap_token")
  url = "https://redcap.emory.edu/api/"
  
  formData = list("token"=token,
                  content='report',
                  format='csv',
                  csvDelimiter='',
                  rawOrLabel=rawOrLabel,
                  rawOrLabelHeaders=rawOrLabelHeaders,
                  exportCheckboxLabel='true',
                  returnFormat='json',
                  report_id=report_id
  )
  httr::POST(url, body = formData, encode = "form") |> httr::content()
}

# Function to download demographics data
get.demographics.data = function(){
  # Download report "DSMB Table" - this includes data for all study arms.
  # Omit some redundant redcap instruments; anonymize treatment arm.
  # Note that the email field and previous.id fields are critical here to establish unique keys.
  # This should probably be corrected.
  demographics.data = DownloadReport('30166') |> 
    arrange(`Participant ID`,`Event Name`) |> 
    group_by(`Participant ID`) |> 
    fill(`Email Field`,.direction="downup")
  
  # Add an identifier for missing email for PRR016 to enable linkage.
  demographics.data$`Email Field`[demographics.data$`Participant ID`=="PRR016"] = "PRR016 does not use email"
  demographics.data$`Email Field` = factor(demographics.data$`Email Field`)
  # Anonymize
  demographics.data$unique.id = forcats::fct_anon(demographics.data$`Email Field`)
  
  # Anonymize treatment arm
  demographics.data$Arm = factor(demographics.data$`Treatment Arm`)
  demographics.data$Arm = forcats::fct_anon(demographics.data$Arm)
  demographics.data = demographics.data |> select(-`Treatment Arm`)
  
  # Age and sex
  demographics.data$Age = as.numeric(round((demographics.data$`Assessment Date`-demographics.data$`Date of Birth`)/365.25,1))
  demographics.data$Sex = NA_character_
  demographics.data$Sex[demographics.data$`Gender (choice=Male)` == "Male"] = "Male"
  demographics.data$Sex[demographics.data$`Gender (choice=Female)` == "Female"] = "Female"
  
  # Rename
  demographics.data = demographics.data |>
    rename(MoCA = `Total points for MOCA`) |> 
    select(unique.id,`Participant ID`,`Event Name`,Age,Sex,MoCA,Arm) |> 
    filter(`Event Name` %in% c("Baseline (Arm 3: Randomized Phase)",
                               "Randomization (Arm 3: Randomized Phase)"))
  
  # Group and fill
  demographics.data = demographics.data |> 
    select(-unique.id) |> 
    arrange(`Participant ID`,`Event Name`) |> 
    ungroup() |> 
    group_by(`Participant ID`) |> 
    fill(-c(`Participant ID`,`Event Name`),.direction="downup") |> 
    ungroup() |> 
    filter(`Event Name` == "Baseline (Arm 3: Randomized Phase)") |> 
    select(-`Event Name`)
  
  demographics.data
}


# Function to download intervention data
get.intervention.data = function(){
  intervention.data = DownloadReport('30167') |> 
    select(`Participant ID`,`Event Name`,`Repeat Instrument`,`Date and Time`:`Date and Time (PWV)`) |> 
    filter(`Event Name` == "Intervention (Arm 3: Randomized Phase)")
}

# Function to download outcomes data
get.outcomes.data = function(){
  outcomes.data = DownloadReport('30171',rawOrLabelHeaders='raw') |> 
    rename(TICS = total_score)
  
  smq.vars = c("memory_decline","repeat_questions","misplacing_things","written_reminders","appointments","recalling","driving","money","socializing","work","news_books_tv","hobbies","lost","household_appliances")
  SMQ.char = outcomes.data |> select(any_of(smq.vars)) |> as.data.frame()
  SMQ.num = matrix(nrow = nrow(SMQ.char), ncol = ncol(SMQ.char))
  SMQ.num[SMQ.char=="Yes"] = 2
  SMQ.num[SMQ.char=="Maybe"] = 1
  SMQ.num[SMQ.char=="No"] = 0
  SMQ = SMQ.num |> apply(1,sum)
  
  outcomes.data$SMQ = SMQ
  outcomes.data = outcomes.data |> select(-any_of(smq.vars))
  
  outcomes.data$wmslm.immediate = outcomes.data |> select(starts_with("immediate_")) |> apply(1,sum)
  outcomes.data$wmslm.delay = outcomes.data |> select(starts_with("delay_")) |> apply(1,sum)
  
  outcomes.data = outcomes.data |> select(-starts_with("immediate_")) |> select(-starts_with("delay_"))
  
  outcomes.data$IADL = outcomes.data |> select(starts_with("iadl")) |> apply(1,function(x) sum(x,na.rm=T))
  outcomes.data = outcomes.data |> select(-starts_with("iadl_"))
  
  LSQ.char = outcomes.data |> select(starts_with("lsq")) |> as.data.frame()
  LSQ.num = matrix(nrow = nrow(LSQ.char), ncol = ncol(LSQ.char))
  LSQ.num[LSQ.char=="Yes"]=1
  LSQ.num[LSQ.char=="No"]=0
  
  outcomes.data = outcomes.data |> select(-starts_with("lsq"))
  outcomes.data$LSQ = apply(LSQ.num,1,sum)
  
  outcomes.data$ravlt.composite = outcomes.data |> select(starts_with("list_")) |> apply(1,sum)
  outcomes.data = outcomes.data |> select(-starts_with("list_"))
  
  # Need to finish the rest of these, but for now, rename
  outcomes.data = outcomes.data |> rename(
    `TICS`=TICS,
    FAQ=faq_score,
    `Percent Handedness`=pct_handed,
    `ACCESS score:`=access_score,
    `SKILLS score:`=skills_score,
    `CPF Score:`=cpf_score,
    `Blind MoCA Total`=blindmoca_total,
    `Previous ID`=screening_id,
    `MCI Duration`=years_since_mci_dx,
    `# Falls in the past 6 months`=assess_falls,
    `Tele-eligible?`=tele_eligible,
    `Wechsler Memory Scale Logical Memory (Immediate)`=wmslm.immediate,
    `Wechsler Memory Scale Logical Memory (Delayed)`=wmslm.delay,
    `Lawton-Brody Instrumental Activities of Daily Living Scale`=IADL,
    `BDI Total Score`=bdi_total_score,
    `PASE Total Score`=pase_score_total,
    `SF12_PCS (Physical Component Summary)`=sf12_pcs,
    `SF12_MCS (Mental Component Summary)`=sf12_mcs,
    `IPA`=ipa_tot,
    `ABC`=abc_avg,
    `AUA Symptom Score`=auasi_total_sc,
    `LSQ`=LSQ,
    `CES-D`=cesd_total,
    `MSPSS`=total_scale_sc,
    `PHQ-9`=phq9_total,
    `QOL-AD`=qol_ad_total,
    `MoCA`=moca_total,
    `Trail A`=trails_a,
    `Trail B`=trails_b,
    `RAVLT Composite Score`=ravlt.composite,
    `Brooks Spatial Memory Percent Correct`=bsm_prcntrt,
    `Benton's JLO`=bjlo_total_correct,
    `Number Span Forward`=fwd_correct,
    `Number Span Backward`=bkwd_correct,
    `D-KEFS Inhibition/Switching`=insw_err_scale,
    `Recognition Hits`=ravltr_correct,
    `Recognition False Positives`=ravltr_false_pos,
    `WRAT`=wordreading_rawscore,
    `BNT Correct`=bnt_total_correct,
    `BNT Incorrect`=btn_total_incorrect,
    `Corsi Blocks Product Score (Trials x Span)`=corsi_prod,
    `D-KEFS TOL`=totmoves_count,
    `Average Forward Gait Speed`=gaitfwd_speed,
    `30 Second Chair Stand`=chair_stand_num,
    `FSST Best Time`=fsst_best_05f206,
    `6MWT (m)`=six_mwt_dist_m,
    `TUG simple (baseline) (s)`=tugs_baseline_s_9bc056,
    `Cognitive Dual Task (s)`=tug_cog_s,
    `Manual Dual Task (s)`=tug_man_s,
    `R: Time to turn (s)`=turn_test_time_s_right,
    `L: Time to turn (s)`=turn_test_time_s_left,
    `Tandem Walk number of interruptions`=tandem_walk_interrupt,
    `Jump Distance (m)`=jump_distance_m,
    `BPST Product`=bpst_prod,
    `Mini-BESTest Total Score`=minibest_total,
    `DGI (/24)`=dgitot,
    `MDS-UPDRS-I`=updrs_1_sum_917709,
    `MDS-UPDRS-II`=updrs_2_sum_393842,
    `MDS-UPDRS-III`=updrs_3_sum_10e4e9,
    `MDS-UPDRS-IV`=updrs_4_sum_de2ba3,
    `MDS-UPDRS-III (Video)`=updrs_3_sum_video_727ef0,
    `FAB (/40)`=fabtot) |> 
    mutate(`Tandem Stance (L)` = max(tandem_stance_trial1_l,tandem_stance_trial2_l,na.rm = T)) |> 
    mutate(`Tandem Stance (R)` = max(tandem_stance_trial1_r,tandem_stance_trial2_r,na.rm = T)) |>   
    mutate(`OLS (L)` = max(one_leg_stance_trial1_l,one_leg_stance_trial2_l,na.rm = T)) |> 
    mutate(`OLS (R)` = max(one_leg_stance_trial1_r,one_leg_stance_trial2_r,na.rm = T))
  
  outcomes.data = outcomes.data |> select(participants,starts_with("redcap"),everything())
}

# download
demographics.data = get.demographics.data()

# download
intervention.data = get.intervention.data()

# download
outcomes.data = get.outcomes.data()
