library("readxl")
library("tidyr")
library("dplyr")
library("stargazer") 
library("tidysynth")
library(writexl)

data <- read_excel("Data.xlsx")
data <- subset(data, select = -c(`Говядина (кроме бескостного мяса), кг`, `Цитрамон, 10 таблеток`))
data

t_treat <- 34

data_long <- data %>%
  pivot_longer(
    cols = -c(date),
    names_to = "unit",
    values_to = "outcome"
  )%>%
  rename(time_unit = date)

data_long

made_synth_obj <- function(df, t = t_treat) {
  # t: номер периода, когда началось вмешательство
  
  pre_period_end <- t - 2 # Последний период для подбора весов (t-2)
  
  # Используем 32 месяца для подбора весов
  n_lags <- 32
  start_lag <- pre_period_end - (n_lags - 1)
  
  synth_obj <- df %>%
    synthetic_control(
      outcome = outcome,
      unit = unit,
      time = time_unit,
      i_unit = 'Treatment',
      i_time = t,
      generate_placebos = TRUE
    )
  
  # Создаем 32 лага в цикле
  for (i in 1:n_lags) {
    lag_month <- pre_period_end - (i - 1)
    predictor_name <- paste0("lag_", sprintf("%02d", i))
    
    synth_obj <- synth_obj %>%
      generate_predictor( # Добавляем предиктор для подбора весов
        time_window = lag_month, 
        !!predictor_name := outcome
      )
  }
  
  # Генерация весов
  synth_obj <- synth_obj %>% 
    generate_weights(
      optimization_window = start_lag:pre_period_end, # Период, по которому подбираем сходство
      margin_ipop = .02, 
      sigf_ipop = 7, 
      bound_ipop = 6
    )
  
  synth_obj <- generate_control(synth_obj)
  return(synth_obj)
}


synth_obj <- made_synth_obj(data_long, t = t_treat)

synth_obj


synth_obj %>% plot_trends() # тренды

delta <- synth_obj$.synthetic_control[[1]]['real_y']-synth_obj$.synthetic_control[[1]]['synth_y']
delta

synth_obj %>% plot_placebos(prune = FALSE)
synth_obj %>% plot_placebos(prune = TRUE)

synth_obj %>% plot_mspe_ratio()
synth_obj %>% grab_significance()

# Для просмотра весов контрольных единиц
weights <- synth_obj %>% grab_unit_weights()

# Для просмотра весов предикторов
synth_obj %>% grab_predictor_weights()

filename_w <- paste0("weights_output_усечённая_32_лага.xlsx", t_treat, ".xlsx")
write_xlsx(weights, filename_w)


# Сохраняем в Excel
significance_df <- synth_obj %>% 
  grab_significance()
filename_sign <- paste0("significance__усечённая_32_лага", t_treat, ".xlsx")
write_xlsx(significance_df, filename_sign)


results_df <- synth_obj$.synthetic_control[[1]] %>%
  rename(
    real_treatment = real_y,
    synthetic_control = synth_y
  ) %>%
  mutate(
    difference = real_treatment - synthetic_control,
    time_period = row_number(),
    period_type = ifelse(time_period < t_treat, "Pre-treatment", "Post-treatment")
  )

filename_sign <- paste0("res_усечённая_32_лага", t_treat, "_all.xlsx")
write_xlsx(results_df, filename_sign)
