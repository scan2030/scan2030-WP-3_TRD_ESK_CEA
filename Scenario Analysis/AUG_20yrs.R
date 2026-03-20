# AUG - Lifetime horizon 20 years (260 cycles of 4 weeks)

library(heemod)
pacman::p_load(data.table, dplyr)

par_mod_aug20 <- define_parameters(
  c_3rdOP = 1165,
  c_4thOP = 1341,
  c_5thOP = 1686,
  c_3rdIP = 405,
  c_4thIP = 405,
  c_5thIP = 572,
  c_SOPC  = 1190,
  c_AUG   = 109,
  
  u_remission = 0.85,
  u_response  = 0.72,
  u_nresponse = 0.58,
  u_tx        = 0.58,
  
  p_3rdTX2RES   = 0.186,
  p_3rdTX2REM   = 0.198,
  p_3rdRES2REL  = 0.281,
  p_3rdREM2REL  = 0.125,
  p_4thTX2RES   = 0.109,
  p_4thTX2REM   = 0.10,
  p_4thRES2REL  = 0.359,
  p_4thREM2REL  = 0.126,
  p_5thTX2RES   = 0.081,
  p_5thTX2REM   = 0.073,
  p_5thRES2REL  = 0.38,
  p_5thREM2REL  = 0.228,
  
  rr_AUG_remission = 1.28,
  rr_AUG_response  = 1.27,
  rr_AUG_rem2rel   = 1,
  rr_AUG_res2rel   = 1,
  
  d_rate = 0.03,
  
  p_death = case_when(
    # Year 1: cycles 1–13
    model_time <= 13 ~ 0.001772008,
    
    # Year 2: cycles 14–26
    model_time >= 14 & model_time <= 26 ~ 0.000921875,
    
    # Year 3: cycles 27–39
    model_time >= 27 & model_time <= 39 ~ 0.000706614,
    
    # Year 4: cycles 40–52
    model_time >= 40 & model_time <= 52 ~ 0.000590032,
    
    # Year 5: cycles 53–65
    model_time >= 53 & model_time <= 65 ~ 0.000513965,
    
    # Year 6: cycles 66–78
    model_time >= 66 & model_time <= 78 ~ 0.000459328,
    
    # Year 7: cycles 79–91
    model_time >= 79 & model_time <= 91 ~ 0.000417671,
    
    # Year 8: cycles 92–104
    model_time >= 92 & model_time <= 104 ~ 0.000384581,
    
    # Year 9: cycles 105–117
    model_time >= 105 & model_time <= 117 ~ 0.000357499,
    
    # Year 10: cycles 118–130
    model_time >= 118 & model_time <= 130 ~ 0.000334820,
    
    # Year 11: cycles 131–143
    model_time >= 131 & model_time <= 143 ~ 0.000315481,
    
    # Year 12: cycles 144–156
    model_time >= 144 & model_time <= 156 ~ 0.000298747,
    
    # Year 13: cycles 157–169
    model_time >= 157 & model_time <= 169 ~ 0.000284089,
    
    # Year 14: cycles 170–182
    model_time >= 170 & model_time <= 182 ~ 0.000271118,
    
    # Year 15: cycles 183–195
    model_time >= 183 & model_time <= 195 ~ 0.000259539,
    
    # Year 16: cycles 196–208
    model_time >= 196 & model_time <= 208 ~ 0.000249124,
    
    # Year 17: cycles 209–221
    model_time >= 209 & model_time <= 221 ~ 0.000239694,
    
    # Year 18: cycles 222–234
    model_time >= 222 & model_time <= 234 ~ 0.000231106,
    
    # Year 19: cycles 235–247
    model_time >= 235 & model_time <= 247 ~ 0.000223245,
    
    # Year 20: cycles 248–260
    model_time >= 248 ~ 0.000216016
  )
)


discount_cost_aug20 <- function(value, rate, cycle) {
  year <- ceiling(cycle / 13)
  ifelse(
    year == 1,
    value,
    value / (1 + rate)^(year - 1)
  )
}

discount_qaly_aug20 <- function(value, rate, cycle) {
  year <- ceiling(cycle / 13)
  ifelse(
    year == 1,
    value / 13,  
    value / 13 / (1 + rate)^(year - 1)
  )
}


mat_aug20 <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  # From 3rd_TX
  0, p_3rdTX2RES * rr_AUG_response, p_3rdTX2REM * rr_AUG_remission,
  (1 - p_3rdTX2RES * rr_AUG_response - p_3rdTX2REM * rr_AUG_remission - p_death),
  0, 0, 0, 0, 0, 0, 0, 0, p_death,
  
  # From 3rd_Response
  0,
  (1 - p_3rdRES2REL * rr_AUG_res2rel - p_death),
  0, 0, p_3rdRES2REL * rr_AUG_res2rel,
  0, 0, 0, 0, 0, 0, 0, p_death,
  
  # From 3rd_Remission
  0, 0,
  (1 - p_3rdREM2REL * rr_AUG_rem2rel - p_death),
  0, p_3rdREM2REL * rr_AUG_rem2rel,
  0, 0, 0, 0, 0, 0, 0, p_death,
  
  # From 3rd_Nresponse
  0, 0, 0, 0,
  (1 - p_death),
  0, 0, 0, 0, 0, 0, 0, p_death,
  
  # From 4th_TX
  0, 0, 0, 0, 0,
  p_4thTX2RES, p_4thTX2REM,
  (1 - p_4thTX2RES - p_4thTX2REM - p_death),
  0, 0, 0, 0, p_death,
  
  # From 4th_Response
  0, 0, 0, 0, 0,
  (1 - p_4thRES2REL - p_death),
  0, 0, p_4thRES2REL,
  0, 0, 0, p_death,
  
  # From 4th_Remission
  0, 0, 0, 0, 0, 0,
  (1 - p_4thREM2REL - p_death),
  0, p_4thREM2REL,
  0, 0, 0, p_death,
  
  # From 4th_Nresponse
  0, 0, 0, 0, 0, 0, 0,
  0, (1 - p_death),
  0, 0, 0, p_death,
  
  # From 5th_TX
  0, 0, 0, 0, 0, 0, 0, 0,
  0, p_5thTX2RES, p_5thTX2REM,
  (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,
  
  # From 5th_Response
  0, 0, 0, 0, 0, 0, 0, 0,
  p_5thRES2REL,
  (1 - p_5thRES2REL - p_death),
  0, 0, p_death,
  
  # From 5th_Remission
  0, 0, 0, 0, 0, 0, 0, 0,
  p_5thREM2REL, 0,
  (1 - p_5thREM2REL - p_death),
  0, p_death,
  
  # From 5th_Nresponse
  0, 0, 0, 0, 0, 0, 0, 0,
  (1 - p_death),
  0, 0, 0, p_death,
  
  # From Death
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
)


mod_aug20 <- define_strategy(
  transition = mat_aug20,
  
  "3rd_TX" = define_state(
    cost  = discount_cost_aug20(c_SOPC + c_3rdOP + c_3rdIP + c_AUG, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_tx, d_rate, model_time)
  ),
  "4th_TX" = define_state(
    cost  = discount_cost_aug20(c_4thOP + c_4thIP, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_tx, d_rate, model_time)
  ),
  "5th_TX" = define_state(
    cost  = discount_cost_aug20(c_5thOP + c_5thIP, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_tx, d_rate, model_time)
  ),
  
  "3rd_Response" = define_state(
    cost  = discount_cost_aug20(c_SOPC + c_3rdOP + c_AUG, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_response, d_rate, model_time)
  ),
  "4th_Response" = define_state(
    cost  = discount_cost_aug20(c_4thOP, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_response, d_rate, model_time)
  ),
  "5th_Response" = define_state(
    cost  = discount_cost_aug20(c_5thOP, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_response, d_rate, model_time)
  ),
  
  "3rd_Remission" = define_state(
    cost  = discount_cost_aug20(c_SOPC + c_3rdOP + c_AUG, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_remission, d_rate, model_time)
  ),
  "4th_Remission" = define_state(
    cost  = discount_cost_aug20(c_4thOP, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_remission, d_rate, model_time)
  ),
  "5th_Remission" = define_state(
    cost  = discount_cost_aug20(c_5thOP, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_remission, d_rate, model_time)
  ),
  
  "3rd_Nresponse" = define_state(
    cost  = discount_cost_aug20(c_3rdOP + c_3rdIP, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_nresponse, d_rate, model_time)
  ),
  "4th_Nresponse" = define_state(
    cost  = discount_cost_aug20(c_4thOP + c_4thIP, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_nresponse, d_rate, model_time)
  ),
  "5th_Nresponse" = define_state(
    cost  = discount_cost_aug20(c_5thOP + c_5thIP, d_rate, model_time),
    QALY  = discount_qaly_aug20(u_nresponse, d_rate, model_time)
  ),
  
  "Death" = define_state(
    cost = 0,
    QALY = 0
  )
)


result_aug20 <- run_model(
  mod   = mod_aug20,
  parameters = par_mod_aug20,
  cycles     = 260,                 # 20  * 13 cycles/year
  cost       = cost,
  effect     = QALY,
  method     = "beginning",
  init       = c(1000, rep(0, 12))
)


print(result_aug20$run_model$cost)
print(result_aug20$run_model$QALY)
