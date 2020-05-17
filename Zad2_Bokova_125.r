#Бокова Арина 125 ПАЭ вариант 5
#создайте модель множественной линейной регрессии ночных потоков углекислого газа за весенний период 2013 года по 
#данным измерений методом турбулентной пульсации
#Настройка и проверка рабочей директории
setwd("D:/Group_125/Bokova/MathMod")
getwd()

#Подключим нужные пакеты
library(rnoaa)
library(tidyverse)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(readr)

#1.МАНИПУЛЯЦИИ С ДАННЫМИ

#Загрузим нашу таблицу, пропустив 1 и 3 строчки, а также заменив значения -9999 символом NA
eddypro = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
#Удалим пустую первую строку
eddypro = eddypro[-1,]
eddypro
#Просмотрим сами переменные и для этого воспользуеся функцией glimpse()
glimpse(eddypro)
#Переменная roll содержит только NA, а потому будет только мешать нам при анализе. Избавимся от нее с помощью 
#функции select:
eddypro = select(eddypro, -(roll))
eddypro
#В таблице довольно много переменных типа char, которые содержат повторяющиеся значения, т.к. 
#их текст, как таковой нас не интересует, преобразуем их все в факторы:
eddypro = eddypro %>% mutate_if(is.character, factor)
#Используем функцию str_replace_all из пакета stringr. Она позволяет, использую довольно простой 
#синтаксис, заменить ненужные нам символы:
names(eddypro) =  str_replace_all(names(eddypro), "[!]","_emph_")
#Воспользуемся оператором пайппинга, чтобы избавиться от ненужных символов
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(eddypro)
#Воспользуемся функцией drop_na(), чтобы избавиться от всех строк, где есть хоть одно значение NA
eddypro = drop_na(eddypro)
#Отфильтруем данные за весенний период с 1 марта (60 день) по 31 мая (151 день)
eddypro = filter(eddypro, DOY >= 60 & DOY <= 151)
#Функция cor работает только с численными данными, поэтому, чтобы перейти к корелляционному анализу 
#нужно выбрать все переменные типа numeric. Для этого воспользуемся двумя функциями - is.numeric(), и sapply()
sapply(eddypro,is.numeric)
#Подставим этот вектор в саму таблицу и получить таблицу состояющую только из интересующих нас колонок
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
#При этом очень легко получить таблицу, содержащую все остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]

#Теперь мы можем переходить к корелляционному анализу
cor_td = cor(eddypro_numeric)
cor_td

cor_td
#Полученные результаты довольно тяжело интерпретировать т.к. они выдаются в виде матрицы, поэтому 
#преобразуем матрицу в таблицу, выберем интересующий нас столбец, а из него возьмем только те имена 
#строк(переменных) для которых значения коэффициента детерминации было больше 0,1
cor_td = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
# Собрать все переменные из вектора с именнами переменных в одну формулу можно следующим образом:
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))
formula

#Создать произвольные(возможно пересекающиеся) обучающую и тестирующую выборки можно с помощью 
#команды sample_n из пакета dplyr
teaching_eddypro = sample_n(eddypro, floor(length(eddypro$date)*.7))
testing_eddypro = sample_n(eddypro, floor(length(eddypro$date)*.3))
#Cделать непересекающиеся подвыборки можно базовым набором функций
row_numbers = 1:length(eddypro$date)
teach = sample(row_numbers, floor(length(eddypro$date)*.7))
test = row_numbers[-teach]

teaching_eddypro_unq = eddypro_numeric[teach,]
testing_eddypro_unq = eddypro_numeric[test,]

#2. ПОСТРОЕНИЕ МОДЕЛЕЙ

# 1 МОДЕЛЬ по обучающей выборке с учётом всех переменных, записанных в formula
model1 = lm(formula , data = teaching_eddypro_unq)

#Посмотрим информацию о модели
summary(model1)
#Посмотрим коэффициенты
coef(model1)
#Выведем остатки
resid(model1)
#доверительный интервал
confint(model1)
#дисперсионный анализ
anova(model1)
#Посмотрим графики модели:
plot(model1)

# 2 МОДЕЛЬ - создадим модель и добавим в неё переменные, полученные при помощии функции anova() с коэффииентом
#значимости менше 0.01: с пометками "***", "**" и "*"
formula2 = co2_flux ~ (Tau + rand_err_Tau + H + LE + rand_err_LE + qc_co2_flux + rand_err_h2o_flux + 
                         u_star_ + T_star_ + un_Tau + un_H  + un_LE + un_co2_flux + un_h2o_flux)
model2 = lm(formula2, data = teaching_eddypro_unq)

#Посмотрим информацию о модели
summary(model2)
#Посмотрим коэффициенты
coef(model2)
#Выведем остатки
resid(model2)
#доверительный интервал
confint(model2)
#дисперсионный анализ
anova(model2) 
# Сравним модели 2 и 1
anova(model2, model1)
#Посмотрим графики модели
plot(model2)

#3. КОРРЕЛЯЦИОННЫЙ АНАЛИЗ ПЕРЕМЕННОЙ

#Обозначим только те переменные, которые участвуют в корреляционном анализе
cor_teaching_eddypro = select(teaching_eddypro_unq, Tau, rand_err_Tau, H, LE, rand_err_LE, qc_co2_flux, h2o_flux, 
                              rand_err_h2o_flux, u_star_, T_star_, un_Tau, un_H, un_LE, un_co2_flux, un_h2o_flux)
#Получаем таблицу коэффициентов корреляции
cor_eddypro = cor(cor_teaching_eddypro) %>% as.data.frame

#Построение графиков по полученной модели
#Построим график сo2_flux от h2o_flux, использовав значения, полученные на модели 2, и на основе обучающей выборки
qplot(co2_flux, co2_flux, data = teaching_eddypro_unq) + geom_line(aes(y = predict(model2, teaching_eddypro_unq)))
#График расположен под углом 45 градусов и проходит через все точки
#Построим график h2o_flux от h2o_flux, использовав значения, полученные на модели 2, и на основе тестирующей выборки
qplot(co2_flux, co2_flux, data = testing_eddypro_unq) + geom_line(aes(y = predict(model2, testing_eddypro_unq)))

#Для примера выведем несколько графиков зависимостей переменной h2o_flux от: Tau, h2o_flux и qc_co2_flux на основе тестирующей модели
qplot(Tau, co2_flux, data = testing_eddypro_unq) + geom_line(aes(y = predict(model2, testing_eddypro_unq)))
qplot(h2o_flux, co2_flux, data = testing_eddypro_unq) + geom_line(aes(y = predict(model2, testing_eddypro_unq)))
qplot(qc_co2_flux, co2_flux, data = testing_eddypro_unq) + geom_line(aes(y = predict(model2, testing_eddypro_unq)))

