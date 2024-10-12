library(pryr)
library(FSinR)
library(purrr)
library(tidyverse)
#### Генерация df ####
set.seed(0)
create_df <- function() {
  X <- round(rnorm(1000, mean = 0, sd = 1), 3)
  Y <- round(rnorm(1000, mean = 0, sd = 5), 3)
  Z <- 5 * X + 2 * Y + round(rnorm(1000, mean = 0, sd = sd(5 * X + 2 * Y)), 3)
  S1 <- round(rnorm(1000, mean = 0, sd = 1), 3)
  S2 <- round(rnorm(1000, mean = 0, sd = 0.5), 3)
  S3 <- round(rnorm(1000, mean = 0, sd = 2), 3)
  
  DF <- data.frame(Z = Z, X = X, Y = Y, S1 = S1, S2 = S2, S3 = S3)
  return(DF)
}
# Применяем функцию 10 раз и сохраняем результат в списке
DF <- lapply(1:10, function(x) create_df())

#### Разбиение df ####
split_data <- function(df) {
  # Определяем количество строк в датафрейме
  n <- nrow(df)
    # Случайно выбираем 50% индексов для тренировочной выборки
  train_indices <- sample(1:n, size = n * 0.5)
    # Создаем тренировочную и тестовую выборки
  train <- df[train_indices, ]
  test <- df[-train_indices, ]
  return(list(train = train, test = test))
}
# Применяем разделение к каждому датафрейму в списке DF
split_DF <- lapply(DF, split_data)

#### Первые вычисления ####
all_summary <- function(func) {
# Функция для расчета индекса Танимото
tanimoto_index <- function(original_subset, matrix_row) {
  # Убедимся, что вектор и строка матрицы имеют одинаковые имена
  if (!all(names(original_subset) == colnames(matrix_row))) {
    stop("Имена переменных в векторе и матрице не совпадают!")
  }
  
  # Извлекаем бинарные значения для подмножеств
  A <- as.numeric(original_subset)
  B <- as.numeric(matrix_row)
  
  # Пересечение (сколько общих переменных выбрано)
  intersection <- sum(A == 1 & B == 1)
  
  # Объединение (переменные, выбраны хотя бы в одном подмножестве)
  union <- sum(A == 1 | B == 1)
  
  # Индекс Танимото
  T_index <- intersection / union
  
  # Процент правильно определенных влияющих признаков
  correct_influencing_percentage <- sum((A == 1 & B == 1)) / sum(A == 1)
  
  # Процент правильно определенных невлияющих признаков
  correct_noninfluencing_percentage <- sum((A == 0 & B == 0)) / sum(A == 0)
  
  # Процент невключённых влияющих признаков
  missing_influencing_percentage <- sum((A == 1 & B == 0)) / sum(A == 1)
  
  # Процент включённых невлияющих признаков
  included_noninfluencing_percentage <- sum((A == 0 & B == 1)) / sum(A == 0)
  
  results <- list(
    T_index = T_index,
    correct_influencing_percentage = correct_influencing_percentage,
    correct_noninfluencing_percentage = correct_noninfluencing_percentage,
    missing_influencing_percentage = missing_influencing_percentage,
    included_noninfluencing_percentage = included_noninfluencing_percentage
  )
  
  return(results)
}
# Расчет индекса Танимото
original_subset <- c(X=1,Y=1,S1=0,S2=0,S3=0)
evaluate_performance <- function(DF, target_var, evaluator_func) {
  # Замер времени начала
  start_time <- Sys.time()
  # Замер использования памяти до выполнения
  before_mem <- mem_used()
  # Выполнение целевой функции
  result <- aco_search(DF, target_var, evaluator_func)$bestFeatures
  # Замер использования памяти после выполнения
  after_mem <- mem_used()
  # Вычисление использованной памяти
  memory_used <- after_mem - before_mem
  # Замер времени конца
  end_time <- Sys.time()
  # Вычисление общего времени выполнения
  total_time <- end_time - start_time
  # Формируем лист с результатами
  results <- list(
    execution_time = total_time,  # Общее время выполнения
    memory_used = memory_used,    # Использованная память
    result = result               # Результат работы aco_search
  )
  return(results)
}
evaluator <- filterEvaluator('determinationCoefficient')

aco_search <- func

performance_results_train <- lapply(split_DF, function(split) {
  evaluate_performance(split$train, 'Z', evaluator)
})
# Функция для создания одной строки для датафрейма
create_row <- function(performance_result) {
  # Извлекаем необходимые значения из результата
  execution_time <- as.numeric(performance_result$execution_time)  # Время выполнения
  memory_used <- performance_result$memory_used  # Использованная память
  selected_features <- performance_result$result  # Выбранные переменные
  
  # Рассчитываем индекс Танимото
  tanimoto_score <- tanimoto_index(original_subset, selected_features)
  
  # Возвращаем строку с результатами
  return(data.frame(
    execution_time = execution_time,
    memory_used = memory_used,
    tanimoto_score = tanimoto_score
  ))
}
# Применяем функцию к каждому элементу списка performance_results_train
results_method <- apply(do.call(rbind, lapply(performance_results_train, create_row)),2,mean)
# Просматриваем полученные результаты
return(results_method)
}
SS <- list(antColony(),geneticAlgorithm(),hillClimbing(),LasVegas(),simulatedAnnealing(),tabu(),
           whaleOptimization())
SA <- all_summary(antColony())
SA <- rbind(SA,all_summary(geneticAlgorithm()))
SA <- rbind(SA,all_summary(LasVegas()))
SA <- rbind(SA,all_summary(simulatedAnnealing()))
SA <- rbind(SA,all_summary(tabu()))
SA <- rbind(SA,all_summary(whaleOptimization()))
rownames(SA) <- c("antColony","geneticAlgorithm","LasVegas","simulatedAnnealing","tabu",
                  "whaleOptimization")
SA
#### Стабильность по записи ####
# Создание списка DF_stability
DF_stability_1 <- vector("list", length = 10)
for (j in seq_len(10)) {
  SS <- vector("list", length = 10)
  for (i in seq_len(10)) {
    SS[[i]] <- split_DF[[i]]$train %>% add_row(split_DF[[i]]$test[sample(1:500,50*j,FALSE),])
  }
  DF_stability_1[[j]]<-SS
}

DF_stability_2 <- vector("list", length = 9)
for (j in seq_len(9)) {
  SS <- vector("list", length = 10)
  for (i in seq_len(10)) {
    SS[[i]] <- split_DF[[i]]$train[sample(1:500,50*j,FALSE),]
  }
  DF_stability_2[[j]]<-SS
}

DF_stability <- c(DF_stability_2,DF_stability_1)

tanimoto_simple <- function(original_subset, matrix_row) {
  # Объединяем все имена переменных из обоих множеств
  all_names <- union(names(original_subset), names(matrix_row))
  # Преобразуем в числовые векторы с одинаковыми переменными (заполняем NA нулями)
  A <- as.numeric(original_subset[all_names])
  B <- as.numeric(matrix_row[all_names])
  # Заменяем NA на 0
  A[is.na(A)] <- 0
  B[is.na(B)] <- 0
  
  # Пересечение (сколько общих переменных выбрано)
  intersection <- sum(A == 1 & B == 1)
  
  # Объединение (переменные, выбраны хотя бы в одном подмножестве)
  union <- sum(A == 1 | B == 1)
  
  # Индекс Танимото (обработка случая, если объединение равно нулю)
  T_index <- if (union == 0) 0 else intersection / union
  
  return(T_index)
}

# Расчет индекса стабильности
evaluator <- filterEvaluator('determinationCoefficient')

stability_1 <- function(func) {
SS <- matrix(0, 1, 19)
for (l in seq_len(10)) {
  original_subset <- func(split_DF[[l]]$train, "Z", evaluator)$bestFeatures
  original_subset <- setNames(as.numeric(original_subset), colnames(original_subset))
  S<-c()
  for (k in seq_len(19)) {
    A <- func(DF_stability[[k]][[l]], "Z", evaluator)$bestFeatures
    A <- setNames(as.numeric(A), colnames(A))
    S <- c(S,tanimoto_simple(original_subset, A))
  }
SS <- rbind(SS,S)
  }
return(apply(SS[-1,],2,mean))
}
SB <- stability_1(antColony())
SB <- rbind(SB,stability_1(geneticAlgorithm()))
SB <- rbind(SB,stability_1(LasVegas()))
SB <- rbind(SB,stability_1(simulatedAnnealing()))
SB <- rbind(SB,stability_1(tabu()))
SB <- rbind(SB,stability_1(whaleOptimization()))
rownames(SB) <- c("antColony","geneticAlgorithm","LasVegas","simulatedAnnealing","tabu",
                  "whaleOptimization")
colnames(SB) <- c("10%","20%","30%","40%","50%","60%","70%","80%","90%","110%","120%","130%","140%","150%","160%","170%","180%",
                  "190%","200%")
SB

#### Стабильность по переменным ####
# Создадим новый список для хранения обновленных датафреймов
new_list <- list()
# Перебираем каждый элемент в split_DF
for (i in 1:length(split_DF)) {
  # Получаем объект train
  train_data <- split_DF[[i]]$train
  # Вложенный список для хранения модификаций одного train
  modified_train_list <- list()
  # Добавляем от 1 до 5 новых переменных с нормальным распределением
  for (j in 1:5) {
    # Копируем оригинальный датафрейм
    temp_data <- train_data
    # Добавляем j переменных с нормальным распределением
    for (k in 1:j) {
      temp_data[[paste0("new_var_", k)]] <- rnorm(nrow(train_data), sd = runif(1,0.50,2.00))
    }
    # Сохраняем модифицированный датафрейм в вложенный список
    modified_train_list[[paste0("version_", j+3)]] <- temp_data
  }
  # Сохраняем вложенный список в основной список
  new_list[[paste0("train_", i)]] <- modified_train_list
}

# Создаем новый список для хранения обновленных списков
new_list_2 <- list()
# Перебираем каждый элемент в split_DF
for (i in 1:length(split_DF)) {
  # Получаем объект train
  train_data <- split_DF[[i]]$train
  # Вложенный список для хранения модификаций одного train
  modified_train_list <- list()
  # Удаляем от 1 до 3 последних столбцов
  for (j in 1:3) {
    # Копируем оригинальный датафрейм
    temp_data <- train_data
    # Удаляем последние j столбцов
    temp_data <- temp_data[, -((ncol(train_data) - j + 1):ncol(train_data))]
    # Сохраняем модифицированный датафрейм в вложенный список
    modified_train_list[[paste0("version_", j)]] <- temp_data
  }
  # Сохраняем вложенный список в основной список
  new_list_2[[paste0("train_", i)]] <- modified_train_list
}

# Создаем новый список для объединенных данных
combined_list <- list()
# Предполагаем, что структура обоих списков одинаковая по размеру
for (i in 1:length(new_list)) {
  # Объединяем соответствующие списки из new_list и new_list_2
  combined_sublist <- c(new_list_2[[i]], new_list[[i]])
  # Сохраняем результат в комбинированный список
  combined_list[[paste0("train_", i)]] <- combined_sublist
}

evaluator <- filterEvaluator('determinationCoefficient')

stability_2 <- function(func) {
  SS <- matrix(0, 1, 8)
  for (l in seq_len(10)) {
    original_subset <- func(split_DF[[l]]$train, "Z", evaluator)$bestFeatures
    original_subset <- setNames(as.numeric(original_subset), colnames(original_subset))
    S<-c()
    for (k in seq_len(8)) {
      A <- func(combined_list[[l]][[k]], "Z", evaluator)$bestFeatures
      A <- setNames(as.numeric(A), colnames(A))
      S <- c(S,tanimoto_simple(original_subset, A))
    }
    SS <- rbind(SS,S)
  }
  return(apply(SS[-1,],2,mean))
}

SC <- stability_2(antColony())
SC <- rbind(SC,stability_2(geneticAlgorithm()))
SC <- rbind(SC,stability_2(LasVegas()))
SC <- rbind(SC,stability_2(simulatedAnnealing()))
SC <- rbind(SC,stability_2(tabu()))
SC <- rbind(SC,stability_2(whaleOptimization()))
rownames(SC) <- c("antColony","geneticAlgorithm","LasVegas","simulatedAnnealing","tabu",
                  "whaleOptimization")
colnames(SC) <- c("-3","-2","-1","+1","+2","+3","+4","+5")
SC
#### Стабильность по Ляпунову
new_train_list <- list()
# Перебираем каждый объект в split_DF
for (i in 1:length(split_DF)) {
  train_data <- split_DF[[i]]$train
  test_data <- split_DF[[i]]$test
  # Вложенный список для хранения разных версий train
  modified_train_list <- list()
  # Добавляем от 50 до 500 наблюдений и от 1 до 10 новых переменных
  for (j in 1:10) {
    # Количество добавляемых наблюдений (50, 100, ..., 500)
    num_obs <- j * 50
    # Добавляем нужное количество наблюдений из test
    if (nrow(test_data) >= num_obs) {
      extended_train <- rbind(train_data, test_data[1:num_obs, ])
    } else {
      extended_train <- rbind(train_data, test_data)
    }
    # Добавляем j нормально распределённых переменных
    for (k in 1:j) {
      extended_train[[paste0("norm_var_", k)]] <- rnorm(nrow(extended_train),sd = runif(1, 0.5,2))
    }
    # Сохраняем модифицированную версию train
    modified_train_list[[paste0("version_", j)]] <- extended_train
  }
  # Сохраняем вложенный список с модификациями train в основной список
  new_train_list[[paste0("train_", i)]] <- modified_train_list
}
stability_3 <- function(func) {
  SS <- matrix(0, 1, 10)
  for (l in seq_len(10)) {
    original_subset <- func(split_DF[[l]]$train, "Z", evaluator)$bestFeatures
    original_subset <- setNames(as.numeric(original_subset), colnames(original_subset))
    S<-c()
    for (k in seq_len(10)) {
      A <- func(new_train_list[[l]][[k]], "Z", evaluator)$bestFeatures
      A <- setNames(as.numeric(A), colnames(A))
      S <- c(S,tanimoto_simple(original_subset, A))
    }
    SS <- rbind(SS,S)
  }
  return(apply(SS[-1,],2,mean))
}

SD <- stability_3(antColony())
SD <- rbind(SD,stability_3(geneticAlgorithm()))
SD <- rbind(SD,stability_3(LasVegas()))
SD <- rbind(SD,stability_3(simulatedAnnealing()))
SD <- rbind(SD,stability_3(tabu()))
SD <- rbind(SD,stability_3(whaleOptimization()))
rownames(SD) <- c("antColony","geneticAlgorithm","LasVegas","simulatedAnnealing","tabu",
                  "whaleOptimization")
colnames(SD) <- c("1","2","3","4","5","6","7","8","9","10")
SD


library(openxlsx)
file_path <- "D:/Наука/Статьи и работы начатые/Отбор признаков/C001/Results.xlsx"
# Создаем новый workbook
wb <- createWorkbook()
# Добавляем датафреймы на разные листы
addWorksheet(wb, "Sheet1")  # Лист для SA
writeData(wb, "Sheet1", SA)
addWorksheet(wb, "Sheet2")  # Лист для SB
writeData(wb, "Sheet2", SB)
addWorksheet(wb, "Sheet3")  # Лист для SC
writeData(wb, "Sheet3", SC)
addWorksheet(wb, "Sheet4")  # Лист для SD
writeData(wb, "Sheet4", SD)
# Сохраняем файл
saveWorkbook(wb, file_path, overwrite = TRUE)

