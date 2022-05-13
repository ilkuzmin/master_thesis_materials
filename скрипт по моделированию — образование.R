#### Зайцев Илья, э641. 06.05.2021 ####
#### Скрипт по гравитационным моделям

library(dplyr) # работа с данными
library(gravity) # работа с гравитационными моделями
library(plm) # работа с панельной структурой
library(stargazer) # экспорт таблиц
library(openxlsx) # экспорт эксель табличек 

#### 1. Полгрузка и преобразование данных ####
data <- read.csv('данные для моделирования/Датасет для моделирования образование.csv', 
                 encoding = 'UTF-8')

#### Зададим название колонок в более читаемом виде

columns <- c('origin','destination','year','total_arrival', 'higher',
             'postdoc', 'phd', 'undergrad', 'tvet', 'college', 'high school',
             'secondary', 'primary','not_indicated',
             'roads_dist','avg_inc_org','avg_wages_org','avg_inc_dest',
             'avg_wages_dest','pop_org','pop_dest','int_org',
             'int_dest','broadband_org','broadband_dest','unemp_org','unemp_dest',
             'wage_to_min_org','wage_to_min_dest','price_pr_housing_org',
             'price_pr_housing_dest')

colnames(data) <- columns

# Уникальные id для регион прибытия/регион отбытия
#data$id <- paste(data$origin, data$destination, sep = '_')

#### Преобразуем данные в панельный формат

#data <- pdata.frame(data, index = c('id', 'year'))

#### Сделаем разницу по уровню доходов и зп между регионами
data$avg_inc_diff <- data$avg_inc_dest/data$avg_inc_org
data$avg_wage_diff <- data$avg_wages_dest/data$avg_wages_org


#### И логарифмы цен на жилье c населением
data$log_price_pr_housing_org <- log(data$price_pr_housing_org)
data$log_price_pr_housing_dest <- log(data$price_pr_housing_dest)
data$log_pop_dest <- log(data$pop_dest)
data$log_pop_org <- log(data$pop_org)


#### Делаем год факторной 
#data$year <- as.factor(data$year)


#### Дескриптивные статистики по датасету (смотрим, есть ли "тяжелый хвост")
# descriptive <- t(summary(data[, c('higher','postdoc', 'phd', 'undergrad', 'college',
#                                   'primary')]))
# 
# 
# 
# write.xlsx(descriptive, 'дескриптивка образование.xlsx')

#### Собираем все в одну общую табличку, чтобы смотреть результаты было удобно ####
#### Скрипт по обычному интернету

cnames <- c("Ступень образования","Оценка коэффициента", "Стандартная ошибка", "P-value")

edu_int_fe <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(edu_int_fe) <- cnames

edu_int_tobit <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(edu_int_tobit) <- cnames

edu_int_ppml <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(edu_int_ppml) <- cnames

for (i in columns[c(4:14)]){
  fe_reg <- fixed_effects(dependent_variable = i,
                          distance = 'roads_dist',
                          additional_regressors = c('log_pop_dest', 'log_pop_org',
                                                     'int_org','int_dest', 'unemp_org',
                                                     'unemp_dest', 'avg_inc_diff', 'avg_wage_diff',
                                                     'wage_to_min_org', 'wage_to_min_dest',
                                                     'log_price_pr_housing_org', 
                                                     'log_price_pr_housing_dest', 'year'),
                          
                          code_origin = 'origin',
                          code_destination = 'destination',
                          data = data)
  
  
  
  tob_reg <- tobit(dependent_variable = i,
                   distance = 'roads_dist',
                   additional_regressors = c('log_pop_dest', 'log_pop_org',
                                             'int_org', 'int_dest', 'unemp_org',
                                             'unemp_dest', 'avg_inc_diff', 'avg_wage_diff',
                                             'wage_to_min_org', 'wage_to_min_dest',
                                             'log_price_pr_housing_org', 
                                             'log_price_pr_housing_dest'),
                   added_constant = 1,
                   data = data)
  
  ppml_reg <- ppml(dependent_variable = i,
                   distance = 'roads_dist',
                   additional_regressors = c('log_pop_dest', 'log_pop_org',
                                             'int_org','int_dest', 'unemp_org',
                                             'unemp_dest', 'avg_inc_diff', 'avg_wage_diff',
                                             'wage_to_min_org', 'wage_to_min_dest',
                                             'log_price_pr_housing_org', 
                                             'log_price_pr_housing_dest', 'year',
                                             'origin', 'destination'),
                   data = data)
  assign(paste(i, 'fe', sep = '_'), fe_reg)
  assign(paste(i, 'tobit', sep = '_'), tob_reg)
  assign(paste(i, 'ppml', sep = '_'), ppml_reg)
  
  edu_int_fe <-  rbind(edu_int_fe, summary(fe_reg)$coefficients[
    c(5:6), c(1,2,4)])
  rownames(edu_int_fe)[c((nrow(edu_int_fe)-1):nrow(edu_int_fe))] <- paste(
    rownames(edu_int_fe)[
      c((nrow(edu_int_fe)-1):nrow(edu_int_fe))], 
    i, sep = "_")
  
  edu_int_ppml <-  rbind(edu_int_ppml, summary(ppml_reg)$coefficients[
    c(5:6), c(1,2,4)])
  rownames(edu_int_ppml)[c((nrow(edu_int_ppml)-1):nrow(edu_int_ppml))] <- paste(
    rownames(edu_int_ppml)[
      c((nrow(edu_int_ppml)-1):nrow(edu_int_ppml))], 
    i, sep = "_")
  
  edu_int_tobit <-  rbind(edu_int_tobit, summary(tob_reg)$estimate[
    c(5:6), c(1,2,4)])
  rownames(edu_int_tobit)[c((nrow(edu_int_tobit)-1):nrow(edu_int_tobit))] <- paste(
    rownames(edu_int_tobit)[
      c((nrow(edu_int_tobit)-1):nrow(edu_int_tobit))], 
    i, sep = "_")
  
}

#### Округляем значения оценок
edu_int_fe[c(1)] <- round(edu_int_fe[c(1)], 4)
edu_int_ppml[c(1)] <- round(edu_int_ppml[c(1)], 4)
edu_int_tobit[c(1)] <- round(edu_int_tobit[c(1)], 4)

#### Округляем стандартные ошибки 
edu_int_fe[c(2)] <- round(edu_int_fe[c(2)], 3)
edu_int_ppml[c(2)] <- round(edu_int_ppml[c(2)], 3)
edu_int_tobit[c(2)] <- round(edu_int_tobit[c(2)], 3)

#### Округляем p-значения 
edu_int_fe[c(3)] <- round(edu_int_fe[c(3)], 2)
edu_int_ppml[c(3)] <- round(edu_int_ppml[c(3)], 2)
edu_int_tobit[c(3)] <- round(edu_int_tobit[c(3)], 2)


#### Скрипт по широкополосному интернету ####

edu_broad_fe <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(edu_broad_fe) <- cnames

edu_broad_tobit <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(edu_broad_tobit) <- cnames

edu_broad_ppml <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(edu_broad_ppml) <- cnames

for (i in columns[c(4:14)]){
  fe_reg <- fixed_effects(dependent_variable = i,
                          distance = 'roads_dist',
                          additional_regressors = c('log_pop_dest', 'log_pop_org',
                                                    'broadband_org','broadband_dest', 'unemp_org',
                                                    'unemp_dest', 'avg_inc_diff', 'avg_wage_diff',
                                                    'wage_to_min_org', 'wage_to_min_dest',
                                                    'log_price_pr_housing_org', 
                                                    'log_price_pr_housing_dest', 'year'),
                          
                          code_origin = 'origin',
                          code_destination = 'destination',
                          data = data)
  
  
  
  tob_reg <- tobit(dependent_variable = i,
                   distance = 'roads_dist',
                   additional_regressors = c('log_pop_dest', 'log_pop_org',
                                             'broadband_org', 'broadband_dest', 'unemp_org',
                                             'unemp_dest', 'avg_inc_diff', 'avg_wage_diff',
                                             'wage_to_min_org', 'wage_to_min_dest',
                                             'log_price_pr_housing_org', 
                                             'log_price_pr_housing_dest'),
                   added_constant = 1,
                   data = data)
  
  ppml_reg <- ppml(dependent_variable = i,
                   distance = 'roads_dist',
                   additional_regressors = c('log_pop_dest', 'log_pop_org',
                                             'broadband_org','broadband_dest', 'unemp_org',
                                             'unemp_dest', 'avg_inc_diff', 'avg_wage_diff',
                                             'wage_to_min_org', 'wage_to_min_dest',
                                             'log_price_pr_housing_org', 
                                             'log_price_pr_housing_dest', 'year',
                                             'origin', 'destination'),
                   data = data)
  assign(paste(i, 'fe', 'broadband',sep = '_'), fe_reg)
  assign(paste(i, 'tobit', 'broadband', sep = '_'), tob_reg)
  assign(paste(i, 'ppml', 'broadband', sep = '_'), ppml_reg)
  
  edu_broad_fe <-  rbind(edu_broad_fe, summary(fe_reg)$coefficients[
    c(5:6), c(1,2,4)])
  rownames(edu_broad_fe)[c((nrow(edu_broad_fe)-1):nrow(edu_broad_fe))] <- paste(
    rownames(edu_broad_fe)[
      c((nrow(edu_broad_fe)-1):nrow(edu_broad_fe))], 
    i, sep = "_")
  
  edu_broad_ppml <-  rbind(edu_broad_ppml, summary(ppml_reg)$coefficients[
    c(5:6), c(1,2,4)])
  rownames(edu_broad_ppml)[c((nrow(edu_broad_ppml)-1):nrow(edu_broad_ppml))] <- paste(
    rownames(edu_broad_ppml)[
      c((nrow(edu_broad_ppml)-1):nrow(edu_broad_ppml))], 
    i, sep = "_")
  
  edu_broad_tobit <-  rbind(edu_broad_tobit, summary(tob_reg)$estimate[
    c(5:6), c(1,2,4)])
  rownames(edu_broad_tobit)[c((nrow(edu_broad_tobit)-1):nrow(edu_broad_tobit))] <- paste(
    rownames(edu_broad_tobit)[
      c((nrow(edu_broad_tobit)-1):nrow(edu_broad_tobit))], 
    i, sep = "_")
  
}

#### Округляем значения оценок
edu_broad_fe[c(1)] <- round(edu_broad_fe[c(1)], 4)
edu_broad_ppml[c(1)] <- round(edu_broad_ppml[c(1)], 4)
edu_broad_tobit[c(1)] <- round(edu_broad_tobit[c(1)], 4)

#### Округляем стандартные ошибки 
edu_broad_fe[c(2)] <- round(edu_broad_fe[c(2)], 3)
edu_broad_ppml[c(2)] <- round(edu_broad_ppml[c(2)], 3)
edu_broad_tobit[c(2)] <- round(edu_broad_tobit[c(2)], 3)

#### Округляем p-значения 
edu_broad_fe[c(3)] <- round(edu_broad_fe[c(3)], 2)
edu_broad_ppml[c(3)] <- round(edu_broad_ppml[c(3)], 2)
edu_broad_tobit[c(3)] <- round(edu_broad_tobit[c(3)], 2)

#### Собираем файлики в одно место

edu_estimation <- list("FE интернет" = edu_int_fe, 
                             "Тобит интернет" = edu_int_tobit,
                             "Пуассон интернет" = edu_int_ppml,
                             "FE широкополосный" = edu_broad_fe, 
                             "Тобит широкополосный" = edu_broad_tobit,
                             "Пуассон широкополосный" =  edu_broad_ppml)

write.xlsx(edu_estimation, "Образование модели.xlsx", rowNames = T)

#TODO Додумать метрики качества, чтобы сравнивать модельки между собой