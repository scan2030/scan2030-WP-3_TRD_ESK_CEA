#Psychotherapies
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
  c_SOPC = 4760,
  c_PSY = 12600,
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
  rr_mPSY_remission =	0.79,
  rr_mPSY_response =	0.79,
  rr_mPSY_rem2rel =	1.02,
  rr_mPSY_res2rel =	1.02,
  d_rate = 0.03,
  

  p_death = case_when(
    model_time <= 13 ~ 0.001772008,  
    model_time >= 14 & model_time <= 26 ~ 0.000921875,  
    model_time >= 27 & model_time <= 39 ~ 0.000706614,  
    model_time >= 40 & model_time <= 52 ~ 0.000590032,  
    model_time >= 53 ~ 0.000513965    
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


mat_antidepressant <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  0, p_3rdTX2RES*rr_mPSY_response, p_3rdTX2REM*rr_mPSY_remission, (1 - p_3rdTX2RES*rr_mPSY_response - p_3rdTX2REM*rr_mPSY_remission - p_death), 0, 0, 0, 0, 0, 0, 0, 0, p_death,

  0, (1 - p_3rdRES2REL*rr_mPSY_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_mPSY_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,

  0, 0, (1 - p_3rdREM2REL*rr_mPSY_rem2rel - p_death), 0, p_3rdREM2REL*rr_mPSY_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,

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


mod_antidepressant <- define_strategy(
  transition = mat_antidepressant,
  
  "3rd_TX" = define_state(
    cost = discount_cost(c_SOPC + c_3rdOP + c_3rdIP + c_PSY, d_rate, model_time),
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
      ifelse(model_time <= 4,
             c_SOPC + c_3rdOP + c_PSY,
             c_3rdOP
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
      ifelse(model_time <= 4,
             c_SOPC + c_3rdOP + c_PSY,
             c_3rdOP
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


result <- run_model(
  mod = mod_antidepressant,
  parameters = par_mod,
  cycles = 65,
  cost = cost,
  effect = QALY,
  method = "beginning",
  init = c(1000, rep(0, 12)) 
)


state_counts <- get_counts(result)

print(state_counts, n=1000)


cost_by_cycle <- get_values(result)


print(cost_by_cycle)


print(cost_by_cycle)
print(qaly_by_cycle)


print(result$run_model$cost)
print(result$run_model$QALY)

