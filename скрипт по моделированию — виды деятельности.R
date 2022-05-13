#### Зайцев Илья, э641. 06.05.2022 ####
#### Скрипт по гравитационным моделям

library(dplyr) # работа с данными
library(gravity) # работа с гравитационными моделями
library(plm) # работа с панельной структурой
library(stargazer) # экспорт таблиц
library(openxlsx) # экспорт эксель табличек 

#### 1. Подгрузка и преобразование данных ####
data <- read.csv('данные для моделирования/Датасет для моделирования.csv', 
                 encoding = 'UTF-8')

#### Зададим название колонок в более читаемом виде

columns <- c('origin','destination','year','total_arrival','military',
             'managers','hq_total','hq_science','hq_health','hq_edu', 
             'hq_economics','hq_ict','hq_humanities','mq_total','mq_science', 
             'mq_health','mq_economics','mq_humanities','mq_ict','audition_total',
             'audition_mechanicus','audition_servicies','audition_archivist',
             'servicies','agro','industry','operator','unqualified','not_indicated',
             'roads_dist','avg_inc_org','avg_wages_org','avg_inc_dest',
             'avg_wages_dest','pop_org','pop_dest','int_org',
             'int_dest','broadband_org','broadband_dest','unemp_org','unemp_dest',
             'wage_to_min_org','wage_to_min_dest','price_pr_housing_org',
             'price_pr_housing_dest')

colnames(data) <- columns

#### Сделаем разницу по уровню доходов и зп между регионами
data$avg_inc_diff <- data$avg_inc_dest/data$avg_inc_org
data$avg_wage_diff <- data$avg_wages_dest/data$avg_wages_org


#### И логарифмы цен на жилье c населением
data$log_price_pr_housing_org <- log(data$price_pr_housing_org)
data$log_price_pr_housing_dest <- log(data$price_pr_housing_dest)
data$log_pop_dest <- log(data$pop_dest)
data$log_pop_org <- log(data$pop_org)


#### Делаем год факторной 
data$year <- as.factor(data$year)


#### Дескриптивные статистики по датасету (смотрим, есть ли "тяжелый хвост")
descriptive <- t(summary(data[, c('hq_total','hq_science','hq_health','hq_edu',
                                'hq_economics','hq_ict','hq_humanities')]))



#write.csv(descriptive, 'дескриптивка.csv')



#### 2. Обычный интернет ####
#### Пишем скрипт для построения моделей по всем видам деятельности
#### Собираем все в одну общую табличку (значение коэфициента + p-value)

cnames <- c("Вид деятельности","Оценка коэффициента", "P-value")

activity_int_fe <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(activity_int_fe) <- cnames

activity_int_tobit <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(activity_int_tobit) <- cnames

activity_int_ppml <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(activity_int_ppml) <- cnames

for (i in columns[c(4:28)]){
fe_reg <- fixed_effects(dependent_variable = i,
              distance = 'roads_dist',
              additional_regressors = c('log_pop_dest', 'log_pop_org',
                                        'int_org', 'int_dest', 'unemp_org',
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
                                                 'log_price_pr_housing_dest','year'),
                       added_constant = 1,
                       data = data)

ppml_reg <- ppml(dependent_variable = i,
                  distance = 'roads_dist',
                  additional_regressors = c('log_pop_dest', 'log_pop_org',
                                            'int_org', 'int_dest', 'unemp_org',
                                            'unemp_dest', 'avg_inc_diff', 'avg_wage_diff',
                                            'wage_to_min_org', 'wage_to_min_dest',
                                            'log_price_pr_housing_org', 
                                            'log_price_pr_housing_dest', 'year',
                                            'origin','destination'),
                  data = data)
assign(paste(i, 'fe', sep = '_'), fe_reg)
assign(paste(i, 'tobit', sep = '_'), tob_reg)
assign(paste(i, 'ppml', sep = '_'), ppml_reg)

activity_int_fe <-  rbind(activity_int_fe, summary(fe_reg)$coefficients[
  c(5:6), c(1,4)])
rownames(activity_int_fe)[c((nrow(activity_int_fe)-1):nrow(activity_int_fe))] <- paste(
                            rownames(activity_int_fe)[
                              c((nrow(activity_int_fe)-1):nrow(activity_int_fe))], 
                            i, sep = "_")

activity_int_ppml <-  rbind(activity_int_ppml, summary(ppml_reg)$coefficients[
  c(5:6), c(1,4)])
rownames(activity_int_ppml)[c((nrow(activity_int_ppml)-1):nrow(activity_int_ppml))] <- paste(
  rownames(activity_int_ppml)[
    c((nrow(activity_int_ppml)-1):nrow(activity_int_ppml))], 
  i, sep = "_")

activity_int_tobit <-  rbind(activity_int_tobit, summary(tob_reg)$estimate[
  c(5:6), c(1,4)])
rownames(activity_int_tobit)[c((nrow(activity_int_tobit)-1):nrow(activity_int_tobit))] <- paste(
  rownames(activity_int_tobit)[
    c((nrow(activity_int_tobit)-1):nrow(activity_int_tobit))], 
  i, sep = "_")

}

#### Округляем значения оценок
activity_int_fe[c(1)] <- round(activity_int_fe[c(1)], 4)
activity_int_ppml[c(1)] <- round(activity_int_ppml[c(1)], 4)
activity_int_tobit[c(1)] <- round(activity_int_tobit[c(1)], 4)

#### Округляем p-значения 
activity_int_fe[c(2)] <- round(activity_int_fe[c(2)], 2)
activity_int_ppml[c(2)] <- round(activity_int_ppml[c(2)], 2)
activity_int_tobit[c(2)] <- round(activity_int_tobit[c(2)], 2)

#### 3. Широкополосный интернет ####

activity_broad_fe <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(activity_broad_fe) <- cnames

activity_broad_tobit <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(activity_broad_tobit) <- cnames

activity_broad_ppml <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(activity_broad_ppml) <- cnames

for (i in columns[c(4:28)]){
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
                                             'broadband_org','broadband_dest', 'unemp_org',
                                             'unemp_dest', 'avg_inc_diff', 'avg_wage_diff',
                                             'wage_to_min_org', 'wage_to_min_dest',
                                             'log_price_pr_housing_org', 
                                             'log_price_pr_housing_dest','year'),
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
                                             'origin','destination'),
                   data = data)
  assign(paste(i, 'fe', 'broadband',  sep = '_'), fe_reg)
  assign(paste(i, 'tobit', 'broadband', sep = '_'), tob_reg)
  assign(paste(i, 'ppml', 'broadband', sep = '_'), ppml_reg)
  
  activity_broad_fe <-  rbind(activity_broad_fe, summary(fe_reg)$coefficients[
    c(5:6), c(1,4)])
  rownames(activity_broad_fe)[c((nrow(activity_broad_fe)-1):nrow(activity_broad_fe))] <- paste(
    rownames(activity_broad_fe)[
      c((nrow(activity_broad_fe)-1):nrow(activity_broad_fe))], 
    i, sep = "_")
  
  activity_broad_ppml <-  rbind(activity_broad_ppml, summary(ppml_reg)$coefficients[
    c(5:6), c(1,4)])
  rownames(activity_broad_ppml)[c((nrow(activity_broad_ppml)-1):nrow(activity_broad_ppml))] <- paste(
    rownames(activity_broad_ppml)[
      c((nrow(activity_broad_ppml)-1):nrow(activity_broad_ppml))], 
    i, sep = "_")
  
  activity_broad_tobit <-  rbind(activity_broad_tobit, summary(tob_reg)$estimate[
    c(5:6), c(1,4)])
  rownames(activity_broad_tobit)[c((nrow(activity_broad_tobit)-1):nrow(activity_broad_tobit))] <- paste(
    rownames(activity_broad_tobit)[
      c((nrow(activity_broad_tobit)-1):nrow(activity_broad_tobit))], 
    i, sep = "_")
}

#### Округляем значения оценок
activity_broad_fe[c(1)] <- round(activity_broad_fe[c(1)], 4)
activity_broad_ppml[c(1)] <- round(activity_broad_ppml[c(1)], 4)
activity_broad_tobit[c(1)] <- round(activity_broad_tobit[c(1)], 4)

#### Округляем p-значения 
activity_broad_fe[c(2)] <- round(activity_broad_fe[c(2)], 2)
activity_broad_ppml[c(2)] <- round(activity_broad_ppml[c(2)], 2)
activity_broad_tobit[c(2)] <- round(activity_broad_tobit[c(2)], 2)

# Расчет R2 (для сравнения моделе между собой)
cor(hq_total_ppml_broaband$y,hq_total_ppml_broaband$linear.predictors)^2
cor(hq_ict_ppml_broaband$y,hq_ict_ppml_broaband$linear.predictors)^2


#### Собираем файлики в одно место

activity_estimations <- list("FE интернет" = activity_int_fe, 
                              "Тобит интернет" = activity_int_tobit,
                              "Пуассон интернет" = activity_int_ppml,
                              "FE широкополосный" = activity_broad_fe, 
                             "Тобит широкополосный" = activity_broad_tobit,
                             "Пуассон широкополосный" =  activity_broad_ppml)

write.xlsx(activity_estimations, "Вид деятельности модели.xlsx", rowNames = T)
