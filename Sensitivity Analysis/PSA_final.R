library(ggplot2)
library(heemod)
pacman::p_load(data.table, dplyr)

par_mod <- define_parameters(
  c_3rdOP = 1165,
  c_4thOP = 1341,
  c_5thOP = 1686,
  c_3rdIP = 405,
  c_4thIP = 405,
  c_5thIP = 572,
  c_AD = 12.5,
  c_COM = 25,
  c_AUG = 109,
  c_PSY = 12600,
  c_rTMS1 =	60000,
  c_rTMS2 =	30000,
  c_ECT1 =	77760,
  c_ECT2 =	38880,
  c_SOPC_PSY = 4760,
  c_SOPC_ECT = 65520,
  c_SOPC_TMS1 = 23800,
  c_SOPC_TMS2 = 11900,
  c_SOPC = 1190,
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
  rr_COM_remission =	1.54,
  rr_COM_response =	1.34,
  rr_COM_rem2rel =	1,
  rr_COM_res2rel =	1,
  rr_AUG_remission	= 1.28,
  rr_AUG_response	= 1.27,
  rr_AUG_rem2rel	= 1,
  rr_AUG_res2rel	= 1,
  rr_mPSY_remission =	0.79,
  rr_mPSY_response =	0.79,
  rr_mPSY_rem2rel =	1.02,
  rr_mPSY_res2rel =	1.02,
  rr_cPSY_remission =	1.92,
  rr_cPSY_response =	1.8,
  rr_cPSY_rem2rel =	0.85,
  rr_cPSY_res2rel =	0.85,
  rr_TMS_remission =	1.83,
  rr_TMS_response =	2.76,
  rr_TMS_rem2rel =	0.81,
  rr_TMS_res2rel =	0.66,
  rr_ESK_response = 1.35,
  rr_ESK_remission = 1.43,
  rr_ESK_rem2rel = 0.49,
  rr_ESK_res2rel = 0.3,
  rr_ECT_response = 1.82,
  rr_ECT_remission = 3.33,
  rr_ECT_rem2rel = 0.65,
  rr_ECT_res2rel = 0.65,
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


mat_COM <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  # 3rd_TX
  0, 
  p_3rdTX2RES * rr_COM_response / (p_3rdTX2RES * rr_COM_response + p_3rdTX2REM * rr_COM_remission + max(0, 1 - p_3rdTX2RES * rr_COM_response - p_3rdTX2REM * rr_COM_remission - p_death) + p_death),
  p_3rdTX2REM * rr_COM_remission / (p_3rdTX2RES * rr_COM_response + p_3rdTX2REM * rr_COM_remission + max(0, 1 - p_3rdTX2RES * rr_COM_response - p_3rdTX2REM * rr_COM_remission - p_death) + p_death),
  max(0, 1 - p_3rdTX2RES * rr_COM_response - p_3rdTX2REM * rr_COM_remission - p_death) / (p_3rdTX2RES * rr_COM_response + p_3rdTX2REM * rr_COM_remission + max(0, 1 - p_3rdTX2RES * rr_COM_response - p_3rdTX2REM * rr_COM_remission - p_death) + p_death),
  0, 0, 0, 0, 0, 0, 0, 0,
  p_death / (p_3rdTX2RES * rr_COM_response + p_3rdTX2REM * rr_COM_remission + max(0, 1 - p_3rdTX2RES * rr_COM_response - p_3rdTX2REM * rr_COM_remission - p_death) + p_death),
  # 3rd_Response
  0, (1 - p_3rdRES2REL*rr_COM_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_COM_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Remission 
  0, 0, (1 - p_3rdREM2REL*rr_COM_rem2rel - p_death), 0, p_3rdREM2REL*rr_COM_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Nresponse
  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,
  # 4th_TX
  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,
  # 4th_Response
  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,
  # 4th_Remission
  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,
  # 4th_Nresponse
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # 5th_TX
  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,
  # 5th_Response
  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,
  # 5th_Remission 
  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,
  # 5th_Nresponse 
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # Death
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

mod_COM <- define_strategy(
  transition = mat_COM,
  
  "3rd_TX" = define_state(
    cost = discount_cost(c_SOPC + c_3rdOP + c_3rdIP + c_COM, d_rate, model_time),
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
    cost = discount_cost(c_SOPC + c_3rdOP + c_COM, d_rate, model_time),
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
    cost = discount_cost(c_SOPC + c_3rdOP + c_COM, d_rate, model_time),
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


mat_AUG <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  # 3rd_TX 
  0, p_3rdTX2RES*rr_AUG_response, p_3rdTX2REM*rr_AUG_remission, (1 - p_3rdTX2RES*rr_AUG_response - p_3rdTX2REM*rr_AUG_remission - p_death), 0, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Response
  0, (1 - p_3rdRES2REL*rr_AUG_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_AUG_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Remission
  0, 0, (1 - p_3rdREM2REL*rr_AUG_rem2rel - p_death), 0, p_3rdREM2REL*rr_AUG_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Nresponse
  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,
  # 4th_TX
  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,
  # 4th_Response
  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,
  # 4th_Remission
  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,
  # 4th_Nresponse 
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # 5th_TX
  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,
  # 5th_Response
  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,
  # 5th_Remission
  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,
  # 5th_Nresponse
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # Death
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)


mod_AUG <- define_strategy(
  transition = mat_AUG,
  
  "3rd_TX" = define_state(
    cost = discount_cost(c_SOPC + c_3rdOP + c_3rdIP + c_AUG, d_rate, model_time),
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
    cost = discount_cost(c_SOPC + c_3rdOP + c_AUG, d_rate, model_time),
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
    cost = discount_cost(c_SOPC + c_3rdOP + c_AUG, d_rate, model_time),
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


mat_mPSY <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  # 3rd_TX
  0, p_3rdTX2RES*rr_mPSY_response, p_3rdTX2REM*rr_mPSY_remission, (1 - p_3rdTX2RES*rr_mPSY_response - p_3rdTX2REM*rr_mPSY_remission - p_death), 0, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Response
  0, (1 - p_3rdRES2REL*rr_mPSY_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_mPSY_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Remission
  0, 0, (1 - p_3rdREM2REL*rr_mPSY_rem2rel - p_death), 0, p_3rdREM2REL*rr_mPSY_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Nresponse
  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,
  # 4th_TX
  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,
  # 4th_Response
  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,
  # 4th_Remission
  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,
  # 4th_Nresponse
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # 5th_TX
  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,
  # 5th_Response
  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,
  # 5th_Remission
  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,
  # 5th_Nresponse
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # Death
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)


mod_mPSY <- define_strategy(
  transition = mat_mPSY,
  
  "3rd_TX" = define_state(
    cost = discount_cost(c_SOPC_PSY + c_3rdOP + c_3rdIP + c_PSY, d_rate, model_time),
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
             c_SOPC_PSY + c_3rdOP + c_PSY,
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
             c_SOPC_PSY + c_3rdOP + c_PSY,
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


mat_cPSY <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  # 3rd_TX
  #0, p_3rdTX2RES*rr_cPSY_response, p_3rdTX2REM*rr_cPSY_remission, (1 - p_3rdTX2RES*rr_cPSY_response - p_3rdTX2REM*rr_cPSY_remission - p_death), 0, 0, 0, 0, 0, 0, 0, 0, p_death,
  0, 
  (p_3rdTX2RES * rr_cPSY_response) / 
    (p_3rdTX2RES * rr_cPSY_response + p_3rdTX2REM * rr_cPSY_remission + max(0, 1 - p_3rdTX2RES * rr_cPSY_response - p_3rdTX2REM * rr_cPSY_remission - p_death) + p_death),
  (p_3rdTX2REM * rr_cPSY_remission) / 
    (p_3rdTX2RES * rr_cPSY_response + p_3rdTX2REM * rr_cPSY_remission + max(0, 1 - p_3rdTX2RES * rr_cPSY_response - p_3rdTX2REM * rr_cPSY_remission - p_death) + p_death),
  max(0, 1 - p_3rdTX2RES * rr_cPSY_response - p_3rdTX2REM * rr_cPSY_remission - p_death) / 
    (p_3rdTX2RES * rr_cPSY_response + p_3rdTX2REM * rr_cPSY_remission + max(0, 1 - p_3rdTX2RES * rr_cPSY_response - p_3rdTX2REM * rr_cPSY_remission - p_death) + p_death),
  0, 0, 0, 0, 0, 0, 0, 0,
  p_death / 
    (p_3rdTX2RES * rr_cPSY_response + p_3rdTX2REM * rr_cPSY_remission + max(0, 1 - p_3rdTX2RES * rr_cPSY_response - p_3rdTX2REM * rr_cPSY_remission - p_death) + p_death),
  # 3rd_Response
  0, (1 - p_3rdRES2REL*rr_cPSY_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_cPSY_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Remission
  0, 0, (1 - p_3rdREM2REL*rr_cPSY_rem2rel - p_death), 0, p_3rdREM2REL*rr_cPSY_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Nresponse 
  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,
  # 4th_TX 
  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,
  # 4th_Response 
  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,
  # 4th_Remission 
  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,
  # 4th_Nresponse 
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # 5th_TX 
  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,
  # 5th_Response
  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,
  # 5th_Remission 
  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,
  # 5th_Nresponse 
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # Death
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

mod_cPSY <- define_strategy(
  transition = mat_cPSY,
  
  "3rd_TX" = define_state(
    cost = discount_cost(c_SOPC_PSY + c_3rdOP + c_3rdIP + c_AD + c_PSY, d_rate, model_time),
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
             c_SOPC_PSY + c_3rdOP + c_PSY+ c_AD,
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
             c_SOPC_PSY + c_3rdOP + c_PSY+ c_AD,
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

mat_TMS <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  # 3rd_TX
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
  # 3rd_Response
  0, (1 - p_3rdRES2REL*rr_TMS_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_TMS_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Remission
  0, 0, (1 - p_3rdREM2REL*rr_TMS_rem2rel - p_death), 0, p_3rdREM2REL*rr_TMS_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Nresponse
  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,
  # 4th_TX
  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,
  # 4th_Response
  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,
  # 4th_Remission
  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,
  # 4th_Nresponse 
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # 5th_TX
  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,
  # 5th_Response 
  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,
  # 5th_Remission 
  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,
  # 5th_Nresponse 
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # Death
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

# 定义状态转移矩阵
mat_ECT <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  # 3rd_TX
  #0, p_3rdTX2RES*rr_TMS_response, p_3rdTX2REM*rr_TMS_remission, (1 - p_3rdTX2RES*rr_TMS_response - p_3rdTX2REM*rr_TMS_remission - p_death), 0, 0, 0, 0, 0, 0, 0, 0, p_death,
  0, 
  (p_3rdTX2RES * rr_ECT_response) / 
    (p_3rdTX2RES * rr_ECT_response + p_3rdTX2REM * rr_ECT_remission + max(0, 1 - p_3rdTX2RES * rr_ECT_response - p_3rdTX2REM * rr_ECT_remission - p_death) + p_death),
  (p_3rdTX2REM * rr_ECT_remission) / 
    (p_3rdTX2RES * rr_ECT_response + p_3rdTX2REM * rr_ECT_remission + max(0, 1 - p_3rdTX2RES * rr_ECT_response - p_3rdTX2REM * rr_ECT_remission - p_death) + p_death),
  max(0, 1 - p_3rdTX2RES * rr_ECT_response - p_3rdTX2REM * rr_ECT_remission - p_death) / 
    (p_3rdTX2RES * rr_ECT_response + p_3rdTX2REM * rr_ECT_remission + max(0, 1 - p_3rdTX2RES * rr_ECT_response - p_3rdTX2REM * rr_ECT_remission - p_death) + p_death),
  0, 0, 0, 0, 0, 0, 0, 0,
  p_death / 
    (p_3rdTX2RES * rr_ECT_response + p_3rdTX2REM * rr_ECT_remission + max(0, 1 - p_3rdTX2RES * rr_ECT_response - p_3rdTX2REM * rr_ECT_remission - p_death) + p_death),
  # 3rd_Response
  0, (1 - p_3rdRES2REL*rr_ECT_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_ECT_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Remission
  0, 0, (1 - p_3rdREM2REL*rr_ECT_rem2rel - p_death), 0, p_3rdREM2REL*rr_ECT_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Nresponse
  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,
  # 4th_TX
  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,
  # 4th_Response
  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,
  # 4th_Remission 
  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,
  # 4th_Nresponse 
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # 5th_TX 状态的转移
  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,
  # 5th_Response 
  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,
  # 5th_Remission 
  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,
  # 5th_Nresponse
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # Death 状态
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)


mod_ECT <- define_strategy(
  transition = mat_ECT,
  
  "3rd_TX" = define_state(
    cost = discount_cost(c_SOPC_ECT + c_3rdOP + c_3rdIP + c_AD + c_ECT1, d_rate, model_time),
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
             c_SOPC_ECT + c_3rdOP + c_AD + c_ECT1,
             ifelse(model_time == 2,
                    c_SOPC_ECT + c_3rdOP + c_AD + c_ECT2,
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
             c_SOPC_ECT + c_3rdOP + c_AD + c_ECT1,
             ifelse(model_time == 2,
                    c_SOPC_ECT + c_3rdOP + c_AD + c_ECT2,
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
  
  # 3rd_TX
  0, p_3rdTX2RES*rr_ESK_response, p_3rdTX2REM*rr_ESK_remission, (1 - p_3rdTX2RES*rr_ESK_response - p_3rdTX2REM*rr_ESK_remission - p_death), 0, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Response 
  0, (1 - p_3rdRES2REL*rr_ESK_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_ESK_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Remission 
  0, 0, (1 - p_3rdREM2REL*rr_ESK_rem2rel - p_death), 0, p_3rdREM2REL*rr_ESK_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Nresponse 
  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,
  # 4th_TX 
  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,
  # 4th_Response 
  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,
  # 4th_Remission 
  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,
  # 4th_Nresponse 
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # 5th_TX 
  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,
  # 5th_Response 
  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,
  # 5th_Remission 
  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,
  # 5th_Nresponse 
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # Death 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)


mat_ESK <- define_transition(
  state_names = c("3rd_TX", "3rd_Response", "3rd_Remission", "3rd_Nresponse", 
                  "4th_TX", "4th_Response", "4th_Remission", "4th_Nresponse", 
                  "5th_TX", "5th_Response", "5th_Remission", "5th_Nresponse", 
                  "Death"),
  
  # 3rd_TX
  #0, p_3rdTX2RES*rr_TMS_response, p_3rdTX2REM*rr_TMS_remission, (1 - p_3rdTX2RES*rr_TMS_response - p_3rdTX2REM*rr_TMS_remission - p_death), 0, 0, 0, 0, 0, 0, 0, 0, p_death,
  0, 
  (p_3rdTX2RES * rr_ESK_response) / 
    (p_3rdTX2RES * rr_ESK_response + p_3rdTX2REM * rr_ESK_remission + max(0, 1 - p_3rdTX2RES * rr_ESK_response - p_3rdTX2REM * rr_ESK_remission - p_death) + p_death),
  (p_3rdTX2REM * rr_ESK_remission) / 
    (p_3rdTX2RES * rr_ESK_response + p_3rdTX2REM * rr_ESK_remission + max(0, 1 - p_3rdTX2RES * rr_ESK_response - p_3rdTX2REM * rr_ESK_remission - p_death) + p_death),
  max(0, 1 - p_3rdTX2RES * rr_ESK_response - p_3rdTX2REM * rr_ESK_remission - p_death) / 
    (p_3rdTX2RES * rr_ESK_response + p_3rdTX2REM * rr_ESK_remission + max(0, 1 - p_3rdTX2RES * rr_ESK_response - p_3rdTX2REM * rr_ESK_remission - p_death) + p_death),
  0, 0, 0, 0, 0, 0, 0, 0,
  p_death / 
    (p_3rdTX2RES * rr_ESK_response + p_3rdTX2REM * rr_ESK_remission + max(0, 1 - p_3rdTX2RES * rr_ESK_response - p_3rdTX2REM * rr_ESK_remission - p_death) + p_death),
  # 3rd_Response
  0, (1 - p_3rdRES2REL*rr_ESK_res2rel - p_death), 0, 0, p_3rdRES2REL*rr_ESK_res2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Remission
  0, 0, (1 - p_3rdREM2REL*rr_ESK_rem2rel - p_death), 0, p_3rdREM2REL*rr_ESK_rem2rel, 0, 0, 0, 0, 0, 0, 0, p_death,
  # 3rd_Nresponse
  0, 0, 0, 0, (1 - p_death), 0, 0, 0, 0, 0, 0, 0, p_death,
  # 4th_TX
  0, 0, 0, 0, 0, p_4thTX2RES, p_4thTX2REM, (1 - p_4thTX2RES - p_4thTX2REM - p_death), 0, 0, 0, 0, p_death,
  # 4th_Response
  0, 0, 0, 0, 0, (1 - p_4thRES2REL - p_death), 0, 0, p_4thRES2REL, 0, 0, 0, p_death,
  # 4th_Remission
  0, 0, 0, 0, 0, 0, (1 - p_4thREM2REL - p_death), 0, p_4thREM2REL, 0, 0, 0, p_death,
  # 4th_Nresponse
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # 5th_TX
  0, 0, 0, 0, 0, 0, 0, 0, 0, p_5thTX2RES, p_5thTX2REM, (1 - p_5thTX2RES - p_5thTX2REM - p_death), p_death,
  # 5th_Response
  0, 0, 0, 0, 0, 0, 0, 0, p_5thRES2REL, (1 - p_5thRES2REL - p_death), 0, 0, p_death,
  # 5th_Remission
  0, 0, 0, 0, 0, 0, 0, 0, p_5thREM2REL, 0, (1 - p_5thREM2REL - p_death), 0, p_death,
  # 5th_Nresponse
  0, 0, 0, 0, 0, 0, 0, 0, (1 - p_death), 0, 0, 0, p_death,
  # Death
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
  AUG = mod_AUG,
  COM = mod_COM,
  mPSY = mod_mPSY,
  cPSY = mod_cPSY,
  TMS = mod_TMS,
  ECT = mod_ECT,
  ESK = mod_ESK,
  parameters = par_mod,
  cycles = 65,
  cost = cost,
  effect = QALY,
  method = "beginning",
  central_strategy = "AUG",
  init = c(1000, rep(0, 12))
);compare_model



psa_params <- define_psa(
  # cost (Gamma distribution)
  c_3rdOP ~ gamma(mean = 1165, sd = sqrt(1165)),
  c_4thOP ~ gamma(mean = 1341, sd = sqrt(1341)),
  c_5thOP ~ gamma(mean = 1686, sd = sqrt(1686)),
  c_3rdIP ~ gamma(mean = 405, sd = sqrt(405)),
  c_4thIP ~ gamma(mean = 405, sd = sqrt(405)),
  c_5thIP ~ gamma(mean = 572, sd = sqrt(572)),
  c_AD ~ gamma(mean = 12.5, sd = sqrt(12.5)),
  c_COM ~ gamma(mean = 25, sd = sqrt(25)),
  c_AUG ~ gamma(mean = 109, sd = sqrt(109)),
  c_PSY ~ gamma(mean = 12600, sd = sqrt(12600)),
  c_rTMS1 ~ gamma(mean = 60000, sd = sqrt(60000)),
  c_rTMS2 ~ gamma(mean = 30000, sd = sqrt(30000)),
  c_ECT1 ~ gamma(mean = 77760, sd = sqrt(77760)),
  c_ECT2 ~ gamma(mean = 38880, sd = sqrt(38880)),
  c_SOPC_PSY ~ gamma(mean = 4760, sd = sqrt(4760)),
  c_SOPC_ECT ~ gamma(mean = 65520, sd = sqrt(65520)),
  c_SOPC_TMS1 ~ gamma(mean = 23800, sd = sqrt(23800)),
  c_SOPC_TMS2 ~ gamma(mean = 11900, sd = sqrt(11900)),
  c_SOPC ~ gamma(mean = 1190, sd = sqrt(1190)),
  c_ESK_SOPC1 ~ gamma(mean = 9520, sd = sqrt(9520)),
  c_ESK_SOPC2 ~ gamma(mean = 4760, sd = sqrt(4760)),
  c_ESK_PDH1 ~ gamma(mean = 10080, sd = sqrt(10080)),
  c_ESK_PDH2 ~ gamma(mean = 5040, sd = sqrt(5040)),
  c_ESK_TX1 ~ gamma(mean = 31296, sd = sqrt(31296)),
  c_ESK_TX2 ~ gamma(mean = 15648, sd = sqrt(15648)),
  
  # utility (Beta)
  u_remission ~ beta(shape1 = 85, shape2 = 15),
  u_response ~ beta(shape1 = 72, shape2 = 28),
  u_nresponse ~ beta(shape1 = 58, shape2 = 42),
  u_tx ~ beta(shape1 = 58, shape2 = 42),
  
  # transition probability (Beta)
  p_3rdTX2RES ~ beta(shape1 = 18.6, shape2 = 81.4),
  p_3rdTX2REM ~ beta(shape1 = 19.8, shape2 = 80.2),
  p_3rdRES2REL ~ beta(shape1 = 28.1, shape2 = 71.9),
  p_3rdREM2REL ~ beta(shape1 = 12.5, shape2 = 87.5),
  p_4thTX2RES ~ beta(shape1 = 10.9, shape2 = 89.1),
  p_4thTX2REM ~ beta(shape1 = 10, shape2 = 90),
  p_4thRES2REL ~ beta(shape1 = 35.9, shape2 = 64.1),
  p_4thREM2REL ~ beta(shape1 = 12.6, shape2 = 87.4),
  p_5thTX2RES ~ beta(shape1 = 8.1, shape2 = 91.9),
  p_5thTX2REM ~ beta(shape1 = 7.3, shape2 = 92.7),
  p_5thRES2REL ~ beta(shape1 = 38, shape2 = 62),
  p_5thREM2REL ~ beta(shape1 = 22.8, shape2 = 77.2),
  
  # discount rate (Beta)
  d_rate ~ beta(shape1 = 3, shape2 = 97),
  
  # relative risk (Lognormal)
  rr_COM_remission ~ lognormal(meanlog = 0.432, sdlog = 0.323),
  rr_COM_response ~ lognormal(meanlog = 0.293, sdlog = 0.147),
  
  # relative risk (Lognormal)
  rr_AUG_remission ~ lognormal(meanlog = 0.247, sdlog = 0.069),
  rr_AUG_response ~ lognormal(meanlog = 0.239, sdlog = 0.042),
  
  # relative risk (Lognormal)
  rr_mPSY_remission ~ lognormal(meanlog = -0.24, sdlog = 0.161),
  rr_mPSY_response ~ lognormal(meanlog = -0.24, sdlog = 0.161),
  rr_mPSY_rem2rel ~ lognormal(meanlog = 0.02, sdlog = 0.101),
  rr_mPSY_res2rel ~ lognormal(meanlog = 0.02, sdlog = 0.101),
  
  # relative risk (Lognormal)
  rr_cPSY_remission ~ lognormal(meanlog = 0.652, sdlog = 0.139),
  rr_cPSY_response ~ lognormal(meanlog = 0.588, sdlog = 0.206),
  rr_cPSY_rem2rel ~ lognormal(meanlog = -0.16, sdlog = 0.069),
  rr_cPSY_res2rel ~ lognormal(meanlog = -0.16, sdlog = 0.069),
  
  # relative risk (Lognormal)
  rr_TMS_remission ~ lognormal(meanlog = 0.604, sdlog = 0.339),
  rr_TMS_response ~ lognormal(meanlog = 1.015, sdlog = 0.399),
  rr_TMS_rem2rel ~ lognormal(meanlog = -0.21, sdlog = 0.161),
  rr_TMS_res2rel ~ lognormal(meanlog = -0.42, sdlog = 0.193),
  
  # relative risk (Lognormal)
  rr_ECT_remission ~ lognormal(meanlog = 1.319, sdlog = 0.260),
  rr_ECT_response ~ lognormal(meanlog = 1.015, sdlog = 0.399),
  rr_ECT_rem2rel ~ lognormal(meanlog = -0.2, sdlog = 0.161),
  rr_ECT_res2rel ~ lognormal(meanlog = -0.42, sdlog = 0.193),
  
  # relative risk (Lognorma)
  rr_ESK_remission ~ lognormal(meanlog = 1.068, sdlog = 0.173),
  rr_ESK_response ~ lognormal(meanlog = 0.678, sdlog = 0.117),
  rr_ESK_rem2rel ~ lognormal(meanlog = -0.43, sdlog = 0.185),
  rr_ESK_res2rel ~ lognormal(meanlog = -0.43, sdlog = 0.185)
  
)



psa_results <- heemod::run_psa(
  model = compare_model,  
  psa = psa_params,
  N = 10000
)

saveRDS(psa_results, file = "psa_results_10000.rds")

# Monte Carlo

p2 <- getS3method("plot", "psa", envir = asNamespace("heemod"))(
  psa_results,
  type = "ce"
)

p2


monte_data <- ggplot_build(p2)$data[[1]]

monte_data$y <- monte_data$y / 7.8


head(monte_data)

monte_data$strategy <- factor(monte_data$group,
                              levels = 1:7,
                              labels = c("AUG", "COM", "cPSY", "ECT", "ESK", "mPSY", "TMS")
)


library(ggplot2)
library(dplyr)

cols <- c(
  "AUG"  = "#6A3D9A",
  "COM"  = "#E69F00",
  "cPSY" = "#A6761D",
  "ECT"  = "#377EB8",
  "ESK"  = "#1B9E77",
  "mPSY" = "#66A61E",
  "TMS"  = "#4D4D4D"
)



monte_data2 <- monte_data %>%
  mutate(
    strategy = as.character(strategy),
    x = as.numeric(x),
    y = as.numeric(y)
  )


ellipse_df <- monte_data2 %>%
  transmute(strategy = strategy, x = x, y = y) %>%
  distinct()

ellipse_ok <- ellipse_df %>%
  group_by(strategy) %>%
  filter(n() >= 10, sd(x, na.rm = TRUE) > 0, sd(y, na.rm = TRUE) > 0) %>%
  ungroup()


monte_nonAUG <- monte_data2 %>% filter(strategy != "AUG")
monte_AUG    <- monte_data2 %>% filter(strategy == "AUG")

ell_nonAUG <- ellipse_ok %>% filter(strategy != "AUG")
ell_AUG    <- ellipse_ok %>% filter(strategy == "AUG")

p_monte_final <- ggplot() +
  geom_point(
    data = monte_nonAUG,
    aes(x = x, y = y, color = strategy),
    alpha = 0.55, size = 0.3
  ) +
  stat_ellipse(
    data = ell_nonAUG,
    aes(x = x, y = y, color = strategy),
    type = "norm", level = 0.95,
    size = 0.5, show.legend = FALSE
  ) +
  geom_point(
    data = monte_AUG,
    aes(x = x, y = y, color = strategy),
    alpha = 0.95, size = 0.5
  ) +
  stat_ellipse(
    data = ell_AUG,
    aes(x = x, y = y, color = strategy),
    type = "norm", level = 0.95,
    size = 0.5, show.legend = FALSE
  ) +
  geom_abline(slope = 50000, intercept = 0, linetype = "dashed",
              color = "darkred", size = 0.8) +
  geom_abline(slope = 150000, intercept = 0, linetype = "dashed",
              color = "darkred", size = 0.8) +
  annotate("text", x = 0.2, y = 0.18 * 50000,
           label = "WTP = US$50,000 per QALY",
           color = "darkred", hjust = 0, size = 4, fontface = "bold") +
  annotate("text", x = 0.2, y = 0.18 * 150000,
           label = "3×WTP = US$150,000 per QALY",
           color = "darkred", hjust = 0, size = 4, fontface = "bold") +
  scale_color_manual(values = cols) +
  labs(
    x = "Incremental Effect (QALYs)",
    y = "Incremental Cost (US$)",
    color = "Treatment"
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    axis.line = element_line(color = "black"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.5),
    legend.key = element_blank()
  )


p_monte_final 

ggsave(
  filename = "psa_ce.tif",
  plot = p_monte_final,
  width = 12,
  height = 7.5,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)


# CEAC plot
library(scales)
library(heemod)
pacman::p_load(data.table, dplyr)
library(tidyr)
library(dplyr)
library(ggplot2)

p <- getS3method("plot", "psa", envir = asNamespace("heemod"))(
  psa_results,
  type = "ac",
  max_wtp = 4000000,
  log_scale = FALSE
)

ceac_data <- ggplot_build(p)$data[[1]]
ceac_data$x <- ceac_data$x / 7.8

ceac_data$strategy <- factor(
  ceac_data$group,
  levels = 1:7,
  labels = c("AUG", "COM", "cPSY", "ECT", "ESK", "mPSY", "TMS")
)

library(ggplot2)
library(scales)

cols <- c(
  "AUG"  = "#6A3D9A",
  "COM"  = "#E69F00",
  "cPSY" = "#A6761D",
  "ECT"  = "#377EB8",
  "ESK"  = "#1B9E77",
  "mPSY" = "#66A61E",
  "TMS"  = "#4D4D4D"
)

p_ceac <- ggplot(ceac_data, aes(x = x, y = y, color = strategy)) +
  geom_line(linewidth = 1.0) +
  geom_vline(
    xintercept = 50000,
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.7
  ) +
  geom_vline(
    xintercept = 150000,
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.7
  ) +
  scale_color_manual(values = cols) +
  scale_x_continuous(
    breaks = c(0, 100000, 200000, 300000, 400000, 500000),
    labels = comma,
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = label_number(accuracy = 0.01),
    expand = expansion(mult = c(0, 0.01))
  ) +
  coord_cartesian(xlim = c(0, 500000), ylim = c(0, 1)) +
  labs(
    x = "Willingness-to-Pay Threshold (US$)",
    y = "Probability of Cost-Effectiveness",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = c(0.8, 0.6),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.4),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.5),
    axis.line = element_line(color = "black"),
    plot.margin = margin(10, 25, 10, 10)
  )

print(p_ceac)

ggsave(
  filename = "psa_ac.tif",
  plot = p_ceac,
  width = 12,
  height = 7.5,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

library(dplyr)

ceac_50k <- ceac_data %>%
  group_by(strategy) %>%
  slice(which.min(abs(x - 50000))) %>%
  ungroup()

ceac_50k

ceac_150k <- ceac_data %>%
  group_by(strategy) %>%
  slice(which.min(abs(x - 150000))) %>%
  ungroup()

ceac_150k


library(dplyr)
library(tidyr)

com_esk <- ceac_data %>%
  filter(strategy %in% c("COM", "ESK")) %>%
  select(x, strategy, y) %>%
  pivot_wider(names_from = strategy, values_from = y)


intersection_com_esk <- com_esk %>%
  mutate(diff = abs(COM - ESK)) %>%
  slice(which.min(diff))

intersection_com_esk