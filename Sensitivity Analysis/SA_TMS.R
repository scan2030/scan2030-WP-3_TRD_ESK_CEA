#SA - TMS VS ESK

install.packages("heemod")
library(heemod)
pacman::p_load(data.table, dplyr)



par_mod <- define_parameters(
  c_3rdOP = 1165,
  c_4thOP = 1341,
  c_5thOP = 1686,
  c_3rdIP = 405,
  c_4thIP = 405,
  c_5thIP = 572,
  c_SOPC_TMS1 = 23800,
  c_SOPC_TMS2 = 11900,
  c_AD = 12.5,
  c_rTMS1 =	60000,
  c_rTMS2 =	30000,
  c_ESK_SOPC1 = 9520,
  c_ESK_SOPC2 = 4760,
  c_ESK_PDH1 = 10080,
  c_ESK_PDH2 = 5040,
  c_ESK_TX1	= 31296,
  c_ESK_TX2	= 15648,
  u_remission = 0.85,
  u_response = 0.72,
  u_nresponse = 0.58,
  u_tx = 0.58,
  p_3rdTX2RES = 0.186,
  p_3rdTX2REM = 0.198,
  p_3rdRES2REL = 0.281,
  p_3rdREM2REL = 0.125,
  p_4thTX2RES = 0.109,
  p_4thTX2REM = 0.1,
  p_4thRES2REL = 0.359,
  p_4thREM2REL = 0.126,
  p_5thTX2RES = 0.081,
  p_5thTX2REM = 0.073,
  p_5thRES2REL = 0.38,
  p_5thREM2REL = 0.228,
  rr_TMS_remission =	1.83,
  rr_TMS_response =	2.76,
  rr_TMS_rem2rel =	0.82,
  rr_TMS_res2rel =	0.66,
  rr_ESK_response = 1.35,
  rr_ESK_remission = 1.43,
  rr_ESK_rem2rel = 0.49,
  rr_ESK_res2rel = 0.3,
  d_rate = 0.03,
  p_death1 = 0.001772008,  
  p_death2 = 0.000921875,  
  p_death3 = 0.000706614,  
  p_death4 = 0.000590032, 
  p_death5 = 0.000513965,   
  
  p_death = case_when(
    model_time <= 13 ~ p_death1,  
    model_time >= 14 & model_time <= 26 ~ p_death2, 
    model_time >= 27 & model_time <= 39 ~ p_death3, 
    model_time >= 40 & model_time <= 52 ~ p_death4, 
    model_time >= 53 ~ p_death5    
  )
)



discount_cost <- function(value, rate, cycle) {
  year <- case_when(
    cycle <= 13 ~ 1, 
    cycle >= 14 & cycle <= 26 ~ 2, 
    cycle >= 27 & cycle <= 39 ~ 3, 
    cycle >= 40 & cycle <= 52 ~ 4,
    cycle >= 53 & cycle <= 65 ~ 5 
  )
  
  ifelse(year == 1,
         value,
         value / (1 + rate)^(year - 1) 
  )
}

discount_qaly <- function(value, rate, cycle) {

  year <- case_when(
    cycle <= 13 ~ 1,
    cycle >= 14 & cycle <= 26 ~ 2, 
    cycle >= 27 & cycle <= 39 ~ 3, 
    cycle >= 40 & cycle <= 52 ~ 4, 
    cycle >= 53 & cycle <= 65 ~ 5  
  )
  

  ifelse(year == 1,
         value/13, 
         value/ 13 / (1 + rate)^(year - 1) 
  )
}



mat_TMS <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  #0, p_3rdTX2RES*rr_TMS_response, p_3rdTX2REM*rr_TMS_remission, (1 - p_3rdTX2RES*rr_TMS_response - p_3rdTX2REM*rr_TMS_remission - p_death), 0, 0, 0, 0, 0, 0, 0, 0, p_death,
  0, 
  (p_3rdTX2RES * rr_TMS_response) / 
    (p_3rdTX2RES * rr_TMS_response + p_3rdTX2REM * rr_TMS_remission + max(0, 1 - p_3rdTX2RES * rr_TMS_response - p_3rdTX2REM * rr_TMS_remission - p_death) + p_death),
  (p_3rdTX2REM * rr_TMS_remission) / 
    (p_3rdTX2RES * rr_TMS_response + p_3rdTX2REM * rr_TMS_remission + max(0, 1 - p_3rdTX2RES * rr_TMS_response - p_3rdTX2REM * rr_TMS_remission - p_death) + p_death),
  max(0, 1 - p_3rdTX2RES * rr_TMS_response - p_3rdTX2REM * rr_TMS_remission - p_death) / 
    (p_3rdTX2RES * rr_TMS_response + p_3rdTX2REM * rr_TMS_remission + max(0, 1 - p_3rdTX2RES * rr_TMS_response - p_3rdTX2REM * rr_TMS_remission - p_death) + p_death),
  0, 0, 0, 0, 0, 0, 0, 0,
  p_death / 
    (p_3rdTX2RES * rr_TMS_response + p_3rdTX2REM * rr_TMS_remission + max(0, 1 - p_3rdTX2RES * rr_TMS_response - p_3rdTX2REM * rr_TMS_remission - p_death) + p_death),

  0, (1 - p_3rdRES2REL*rr_TMS_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_TMS_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,

  0, 0, (1 - p_3rdREM2REL*rr_TMS_rem2rel - p_death), 0, p_3rdREM2REL*rr_TMS_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,

  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,

  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,

  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,

  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,

  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)


mod_TMS <- define_strategy(
  transition = mat_TMS,
  
  "3rd_TX" = define_state(
    cost = discount_cost(c_SOPC_TMS1 + c_3rdOP + c_3rdIP + c_AD + c_rTMS1, d_rate, model_time),
    QALY = discount_qaly(u_tx, d_rate, model_time)
  ),
  
  "4th_TX" = define_state(
    cost = discount_cost(c_4thOP + c_4thIP, d_rate, model_time),
    QALY = discount_qaly(u_tx, d_rate, model_time)
  ),
  
  "5th_TX" = define_state(
    cost = discount_cost(c_5thOP + c_5thIP, d_rate, model_time),
    QALY = discount_qaly(u_tx, d_rate, model_time)
  ),
  
  "3rd_Response" = define_state(
    cost = discount_cost(
      ifelse(model_time == 1,
             c_SOPC_TMS1 + c_3rdOP + c_AD + c_rTMS1,
             ifelse(model_time == 2,
                    c_SOPC_TMS2 + c_3rdOP + c_AD + c_rTMS2,
                    c_3rdOP)
      ),
      d_rate,
      model_time
    ),
    QALY = discount_qaly(u_response, d_rate, model_time)
  ),
  
  "4th_Response" = define_state(
    cost = discount_cost(c_4thOP, d_rate, model_time),
    QALY = discount_qaly(u_response, d_rate, model_time)
  ),
  
  "5th_Response" = define_state(
    cost = discount_cost(c_5thOP, d_rate, model_time),
    QALY = discount_qaly(u_response, d_rate, model_time)
  ),
  
  "3rd_Remission" = define_state(
    cost = discount_cost(
      ifelse(model_time == 1,
             c_SOPC_TMS1 + c_3rdOP + c_AD + c_rTMS1,
             ifelse(model_time == 2,
                    c_SOPC_TMS2 + c_3rdOP + c_AD + c_rTMS2,
                    c_3rdOP)
      ),
      d_rate,
      model_time
    ),
    QALY = discount_qaly(u_remission, d_rate, model_time)
  ),
  
  "4th_Remission" = define_state(
    cost = discount_cost(c_4thOP, d_rate, model_time),
    QALY = discount_qaly(u_remission, d_rate, model_time)
  ),
  
  "5th_Remission" = define_state(
    cost = discount_cost(c_5thOP, d_rate, model_time),
    QALY = discount_qaly(u_remission, d_rate, model_time)
  ),
  
  "3rd_Nresponse" = define_state(
    cost = discount_cost(c_3rdOP + c_3rdIP, d_rate, model_time),
    QALY = discount_qaly(u_nresponse, d_rate, model_time)
  ),
  
  "4th_Nresponse" = define_state(
    cost = discount_cost(c_4thOP + c_4thIP, d_rate, model_time),
    QALY = discount_qaly(u_nresponse, d_rate, model_time)
  ),
  
  "5th_Nresponse" = define_state(
    cost = discount_cost(c_5thOP + c_5thIP, d_rate, model_time),
    QALY = discount_qaly(u_nresponse, d_rate, model_time)
  ),
  
  "Death" = define_state(
    cost = 0,
    QALY = 0
  )
)


mat_ESK <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  

  0, p_3rdTX2RES*rr_ESK_response, p_3rdTX2REM*rr_ESK_remission, (1 - p_3rdTX2RES*rr_ESK_response - p_3rdTX2REM*rr_ESK_remission - p_death), 0, 0, 0, 0, 0, 0, 0, 0, p_death,

  0, (1 - p_3rdRES2REL*rr_ESK_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_ESK_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,

  0, 0, (1 - p_3rdREM2REL*rr_ESK_rem2rel - p_death), 0, p_3rdREM2REL*rr_ESK_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,

  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,

  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,

  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,

  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,

  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,

  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)



mod_ESK <- define_strategy(
  transition = mat_ESK,
  
  "3rd_TX" = define_state(
    cost = discount_cost(c_3rdOP + c_3rdIP + c_AD + c_ESK_SOPC1 + c_ESK_PDH1 + c_ESK_TX1, d_rate, model_time),
    QALY = discount_qaly(u_tx, d_rate, model_time)
  ),
  
  "4th_TX" = define_state(
    cost = discount_cost(c_4thOP + c_4thIP, d_rate, model_time),
    QALY = discount_qaly(u_tx, d_rate, model_time)
  ),
  
  "5th_TX" = define_state(
    cost = discount_cost(c_5thOP + c_5thIP, d_rate, model_time),
    QALY = discount_qaly(u_tx, d_rate, model_time)
  ),
  
  "3rd_Response" = define_state(
    cost = discount_cost(
      ifelse(model_time == 1,
             c_3rdOP + c_AD + c_ESK_SOPC1 + c_ESK_PDH1 + c_ESK_TX1,
             ifelse(model_time >= 2 & model_time <= 6,
                    c_3rdOP + c_AD + c_ESK_SOPC2 + c_ESK_PDH2 + c_ESK_TX2,
                    c_3rdOP)
      ),
      d_rate,
      model_time
    ),
    QALY = discount_qaly(u_response, d_rate, model_time)
  ),
  
  "4th_Response" = define_state(
    cost = discount_cost(c_4thOP, d_rate, model_time),
    QALY = discount_qaly(u_response, d_rate, model_time)
  ),
  
  "5th_Response" = define_state(
    cost = discount_cost(c_5thOP, d_rate, model_time),
    QALY = discount_qaly(u_response, d_rate, model_time)
  ),
  
  "3rd_Remission" = define_state(
    cost = discount_cost(
      ifelse(model_time == 1,
             c_3rdOP + c_AD + c_ESK_SOPC1 + c_ESK_PDH1 + c_ESK_TX1,
             ifelse(model_time >= 2 & model_time <= 6,
                    c_3rdOP + c_AD + c_ESK_SOPC2 + c_ESK_PDH2 + c_ESK_TX2,
                    c_3rdOP)
      ),
      d_rate,
      model_time
    ),
    QALY = discount_qaly(u_remission, d_rate, model_time)
  ),
  
  "4th_Remission" = define_state(
    cost = discount_cost(c_4thOP, d_rate, model_time),
    QALY = discount_qaly(u_remission, d_rate, model_time)
  ),
  
  "5th_Remission" = define_state(
    cost = discount_cost(c_5thOP, d_rate, model_time),
    QALY = discount_qaly(u_remission, d_rate, model_time)
  ),
  
  "3rd_Nresponse" = define_state(
    cost = discount_cost(c_3rdOP + c_3rdIP, d_rate, model_time),
    QALY = discount_qaly(u_nresponse, d_rate, model_time)
  ),
  
  "4th_Nresponse" = define_state(
    cost = discount_cost(c_4thOP + c_4thIP, d_rate, model_time),
    QALY = discount_qaly(u_nresponse, d_rate, model_time)
  ),
  
  "5th_Nresponse" = define_state(
    cost = discount_cost(c_5thOP + c_5thIP, d_rate, model_time),
    QALY = discount_qaly(u_nresponse, d_rate, model_time)
  ),
  
  "Death" = define_state(
    cost = 0,
    QALY = 0
  )
)


compare_model <- run_model(
  TMS = mod_TMS,
  ESK = mod_ESK,
  parameters = par_mod,
  cycles = 65,
  cost= cost,
  effect = QALY,
  method = "beginning",
  init = c(1000,rep(0,12)))

a <- compare_model$eval_strategy_list$TMS$counts
b <- compare_model$eval_strategy_list$ESK$counts
plot(compare_model, type = "counts", panel = "by_state", free_y = TRUE)
plot(compare_model$eval_strategy_list$TMS$values$"cost")
plot(compare_model$eval_strategy_list$ESK$values$"QALY")




# DSA
dsa <- define_dsa(

  c_3rdOP, 1103, 1251,
  c_4thOP, 1249, 1445,
  c_5thOP, 1557, 1825,
  c_3rdIP, 226, 772,
  c_4thIP, 233, 740,
  c_5thIP, 233, 1335,
  c_ESK_TX1, 25037,37555,
  c_ESK_TX2, 12518,18778,
  c_rTMS1, 48000, 72000,
  c_rTMS2, 24000, 36000,

  u_remission, 0.83, 0.87,
  u_response, 0.69, 0.75,
  u_nresponse, 0.55, 0.61,
  u_tx, 0.55, 0.61,
  

  p_3rdTX2RES, 0.162, 0.209,
  p_3rdTX2REM, 0.174, 0.223,
  p_3rdRES2REL, 0.253, 0.308,
  p_3rdREM2REL, 0.106, 0.146,
  p_4thTX2RES, 0.090, 0.128,
  p_4thTX2REM, 0.082, 0.118,
  p_4thRES2REL, 0.328, 0.388,
  p_4thREM2REL, 0.104, 0.147,
  p_5thTX2RES, 0.064, 0.097,
  p_5thTX2REM, 0.057, 0.089,
  p_5thRES2REL, 0.352, 0.411,
  p_5thREM2REL, 0.202, 0.254,
  

  p_death1, 0.0010, 0.0029,
  p_death2, 0.0006, 0.0014,
  p_death3, 0.0005, 0.0010,
  p_death4, 0.0004, 0.0009,
  p_death5, 0.0003, 0.0007,
  

  d_rate, 0.0, 0.05,
  

  rr_TMS_remission, 0.94, 3.55, 
  rr_TMS_response, 1.26, 6.02, 
  rr_TMS_rem2rel, 0.59, 1.11,
  rr_TMS_res2rel, 0.45, 0.96,
  

  rr_ESK_remission, 1.04, 1.97,
  rr_ESK_response, 1.14, 1.61,
  rr_ESK_rem2rel, 0.29, 0.84,
  rr_ESK_res2rel, 0.16, 0.55
)


set.seed(10)
sc_dsa <- run_dsa(
  model=compare_model,
  dsa=dsa
)

dsa_summary = summary(sc_dsa)

# Extract full DSA result
library(dplyr)
library(ggplot2)
library(scales)


dsa_summary <- summary(sc_dsa, center = FALSE)$res_comp %>%
  mutate(.icer = .icer / 7.8) %>%  
  left_join(
    summary(compare_model, center = FALSE)$res_comp %>%
      mutate(.icer = .icer / 7.8) %>%  
      select(.strategy_names, .icer_ref = .icer),
    by = ".strategy_names"
  ) %>%
  mutate(
    .col_icer = ifelse(.icer > .icer_ref, ">", 
                       ifelse(.icer == .icer_ref, "=", "<")),
    dicer = .icer - .icer_ref
  ) %>%
  filter(.strategy_names == "ESK") %>%
  group_by(.par_names) %>%
  mutate(xiao = min(.icer, na.rm = TRUE),
         da = max(.icer, na.rm = TRUE),
         diff = da - xiao) %>%
  ungroup() %>%
  arrange(desc(diff)) %>%
  utils::head(20)


name_map <- c(
  "c_3rdOP" = "Cost: 3rd-line outpatient",
  "c_4thOP" = "Cost: 4th-line outpatient",
  "c_5thOP" = "Cost: 5th-line outpatient",
  "c_3rdIP" = "Cost: 3rd-line inpatient",
  "c_4thIP" = "Cost: 4th-line inpatient",
  "c_5thIP" = "Cost: 5th-line inpatient",
  "c_ESK_TX1" = "Cost: ESK Induction phase",
  "c_ESK_TX2" = "Cost: ESK Maintenance phase",
  "c_rTMS1" = "Cost: rTMS Induction phase",
  "c_rTMS2" = "Cost: rTMS Maintenance phase",
  "u_remission" = "Utility: Remission",
  "u_response" = "Utility: Response",
  "u_nresponse" = "Utility: No response",
  "u_tx" = "Utility: On treatment",
  "p_3rdTX2RES" = "Prob: 3rd-line to Response",
  "p_3rdTX2REM" = "Prob: 3rd-line to Remission",
  "p_3rdRES2REL" = "Prob: 3rd Response to Relapse",
  "p_3rdREM2REL" = "Prob: 3rd Remission to Relapse",
  "p_4thTX2RES" = "Prob: 4th-line to Response",
  "p_4thTX2REM" = "Prob: 4th-line to Remission",
  "p_4thRES2REL" = "Prob: 4th Response to Relapse",
  "p_4thREM2REL" = "Prob: 4th Remission to Relapse",
  "p_5thTX2RES" = "Prob: 5th-line to Response",
  "p_5thTX2REM" = "Prob: 5th-line to Remission",
  "p_5thRES2REL" = "Prob: 5th Response to Relapse",
  "p_5thREM2REL" = "Prob: 5th Remission to Relapse",
  "p_death1" = "Mortality: Year 1",
  "p_death2" = "Mortality: Year 2",
  "p_death3" = "Mortality: Year 3",
  "p_death4" = "Mortality: Year 4",
  "p_death5" = "Mortality: Year 5",
  "d_rate" = "Discount rate",
  "rr_TMS_remission" = "RR (TMS): Remission",
  "rr_TMS_response" = "RR (TMS): Response",
  "rr_TMS_rem2rel" = "RR (TMS): Remission to Relapse",
  "rr_TMS_res2rel" = "RR (TMS): Response to Relapse",
  "rr_ESK_remission" = "RR (ESK): Remission",
  "rr_ESK_response" = "RR (ESK): Response",
  "rr_ESK_rem2rel" = "RR (ESK): Remission to Relapse",
  "rr_ESK_res2rel" = "RR (ESK): Response to Relapse"
)

dsa_summary <- dsa_summary %>%
  mutate(.par_names_label = name_map[.par_names])


ggplot(dsa_summary, aes(x = .icer, y = reorder(.par_names_label, diff), color = .col_icer)) +
  geom_segment(aes(xend = .icer_ref, yend = reorder(.par_names_label, diff)), 
               linewidth = 5, alpha = 0.5) +
  geom_text(
    data = dsa_summary %>% group_by(.par_names) %>% slice_min(.icer),
    aes(label = round(.par_value_eval, 4), x = .icer, 
        y = reorder(.par_names_label, diff)),
    hjust = 1.1, color = "black", size = 3
  ) +
  geom_text(
    data = dsa_summary %>% group_by(.par_names) %>% slice_max(.icer),
    aes(label = round(.par_value_eval, 4), x = .icer, 
        y = reorder(.par_names_label, diff)),
    hjust = -0.1, color = "red", size = 3
  ) +
  geom_vline(xintercept = 50000, linetype = "dashed", color = "#011627", linewidth = 0.8) +
  geom_vline(xintercept = 150000, linetype = "dashed", color = "#011627", linewidth = 0.8) +
  annotate("text", x = 50000, y = 5, label = "WTP = USD 50,000/QALY", angle = 90, 
           vjust = -0.5, hjust = -0.1, size = 2, color = "#011627", fontface = "bold") +
  annotate("text", x = 150000, y = 5, label = "3xWTP = USD 150,000/QALY", angle = 90, 
           vjust = -0.5, hjust = -0.1, size = 2, color = "#011627", fontface = "bold") +
  scale_color_manual(values = c(">" = "#F0868C", "<" = "#70A5D9")) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Esketamine vs. Unilateral rTMS plus Oral AD",
    x = "ICER (USD/QALY)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.3),
    text = element_text(color = "black")
  )


ggsave(
  filename = "dsa_icer_plot_TMS.svg",  
  plot = last_plot(),             
  width = 10, height = 6, dpi = 300 
)

