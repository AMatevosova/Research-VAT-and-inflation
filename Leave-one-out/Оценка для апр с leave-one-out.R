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
      generate_placebos = FALSE
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


# Функция для leave-one-out synthetic control
synth_leave_one_out <- function(df, t = t_treat, excluded_unit) {
  # Исключаем указанную контрольную единицу
  df_filtered <- df %>%
    filter(unit != excluded_unit)
  
  # Запускаем синтетический контроль для него
  tryCatch({
    synth_obj_loo <- made_synth_obj(df_filtered, t = t_treat)
    
    # Извлекаем synthetic control ряд
    synthetic_series <- synth_obj_loo$.synthetic_control[[1]]$synth_y
    
    return(synthetic_series)
  }, error = function(e) {
    message(paste("Ошибка при исключении", excluded_unit, ":", e$message))
    return(rep(NA, length(unique(df$time_unit))))
  })
}

# Получаем список всех контрольных единиц (исключаем Treatment)
all_units <- unique(data_long$unit)
control_units <- all_units[all_units != "Treatment"]

# Создаем датафрейм с временными периодами
time_periods <- unique(data_long$time_unit) %>% sort()
results_loo <- data.frame(time_period = time_periods)

# Для каждой контрольной единицы (псевдо-treatment) строим synthetic control и сохраняем ряд
for (unit_excluded in control_units) {
  cat("Обработка исключения:", unit_excluded, "\n")
  
  synthetic_series <- synth_leave_one_out(data_long, t_treat, unit_excluded)
  
  # Сохраняем с именем столбца - исключённой единицы (псевдо-treatment)
  col_name <- paste0("excluded_", unit_excluded)
  results_loo[[col_name]] <- synthetic_series
}

filename_loo <- paste0("leave_one_out_results",".xlsx")
write_xlsx(results_loo, filename_loo)

