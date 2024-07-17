# Instalar e carregar pacotes necessários para a análise de dados

install_and_load <- function (packages) {
    for (pkg in packages) {
        if (!require(pkg, character.only = TRUE)) {
            install.packages(pkg, dependencies = TRUE)
            library(pkg, character.only = TRUE)
        }
    }
}

packages <- c("dplyr", "caret", "randomForest", "pROC") 
install_and_load(packages)

# Carregar bibliotecas 
library(dplyr) 
library(caret) 
library(randomForest) 
library(pROC)

# Carregar dados de um arquivo local
data_path <- "C:/Users/marti/Desktop/WA_Fn-UseC_-Telco-Customer-Churn.csv"
data <- read.csv(data_path)

# Remover espaços em branco nos nomes das colunas
colnames(data) <- trimws(colnames(data))

# Converter 'TotalCharges' para numérico
data$TotalCharges <- as.numeric(as.character(data$TotalCharges))

# Tratar valores ausentes
data <- na.omit(data)

# Garantir que a variável 'Churn' é um fator com níveis consistentes
data$Churn <- factor(data$Churn, levels = c("Yes", "No"))

# Visualizar a distribuição de churn
print(table(data$Churn))
barplot(table(data$Churn), main = "Distribuição de Churn", col = c("blue", "red"))

# Relação entre MonthlyCharges e TotalCharges
plot(data$MonthlyCharges, data$TotalCharges, col = ifelse(data$Churn == "Yes", "red", "blue"), main = "Relação entre MonthlyCharges e TotalCharges")

# Converter todas as variáveis de caracteres(exceto 'Churn') para fatores
data <- data %>%
    mutate(across(where(is.character), as.factor))

# Converter todos os fatores(exceto 'Churn') para numéricos
factor_columns <- setdiff(names(data)[sapply(data, is.factor)], "Churn")
data[factor_columns] <- lapply(data[factor_columns], as.numeric)

# Dividir dados em conjuntos de treinamento e teste
set.seed(42)
trainIndex <- createDataPartition(data$Churn, p = 0.7, list = FALSE)
dataTrain <- data[trainIndex, ]
dataTest <- data[-trainIndex, ]

# Reconfirmar 'Churn' como fator com níveis em ambos os conjuntos de dados
dataTrain$Churn <- factor(dataTrain$Churn, levels = c("Yes", "No"))
dataTest$Churn <- factor(dataTest$Churn, levels = c("Yes", "No"))

# Treinar o modelo
model <- randomForest(Churn ~ ., data = dataTrain, ntree = 100, importance = TRUE)

# Prever no conjunto de teste
pred <- predict(model, dataTest)
pred <- factor(pred, levels = levels(dataTest$Churn))

# Matriz de confusão e relatório de classificação
conf_matrix <- confusionMatrix(pred, dataTest$Churn)
print(conf_matrix)

# Curva ROC e valor AUC
roc_obj <- roc(dataTest$Churn, as.numeric(pred))
auc_value <- auc(roc_obj)
cat("AUC value:", auc_value, "\n")
plot(roc_obj, main = paste("Curva ROC - AUC:", round(auc_value, 2)))
