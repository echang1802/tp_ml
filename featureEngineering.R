
rm(list = ls())
gc()

train<- read.csv("datasets/train_data.csv")

# Missings
NCol<- ncol(train) - 1
target0<- train$TARGET == 0
target1<- train$TARGET == 1
missingValues<- numeric(length = NCol - 1)
variance<- numeric(length = NCol - 1)
summaries<- list()
for(i in 2:NCol){
  missingValues[i-1]<- sum(is.na(train[,i]))
  variance[i-1]<- sd(train[,i], na.rm = TRUE)
  png(filename = paste("gráficos/",names(train)[i],".png", sep = ""))
  par(mfrow=c(1,2))
  hist(train[target0,i],xlab = names(train)[i],main = paste("",names(train)[i],"without churm"))
  hist(train[target1,i],xlab = names(train)[i],main = paste("",names(train)[i],"with churm"))
  dev.off()
  summaries[[i-1]]<- summary(train[,i])  
}

eliminarColumnas<- character()

summary(missingValues)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.1452  0.0000 45.0000 
table(missingValues)
# 0  45 
# 309   1 
# Solo hay una variable con missing values y solo tiene 45 missings, esta es:
names(train)[c(FALSE,missingValues > 0,FALSE)]
# [1] "nac"

summary(variance)
# Min.      1st Qu.    Median      Mean           3rd Qu.      Max.              NA's 
# 0         0          1           13108618       714          694550804         1 
sum(variance == 0, na.rm = TRUE)
# [1] 9
# Existen nueve variables que no tienen variación, estas son:
names(train)[c(FALSE,variance == 0,FALSE)]
# [1] "ind_var13_medio_0"             "ind_var13_medio"               "num_var13_medio_0"            
# [4] "num_var13_medio"               "saldo_var13_medio"             "num_meses_var13_medio_ult3"   
# [7] "saldo_medio_var13_medio_hace2" "saldo_medio_var13_medio_ult1"  "saldo_medio_var13_medio_ult3" 
# Las agregamos a la lista de columnas a eliminar
eliminarColumnas<- c(eliminarColumnas,names(train)[c(FALSE,variance == 0,FALSE)])

#Exploremos las variables indicadoras
indicadoras<- grep('ind', names(train), value=TRUE)
indicadorasChurn<- character()
indicadorasNOChurn<- character()
tol<- nrow(train)*0.005
for(ind in indicadoras){
  # Eliminamos los indicadores sin varianza
  if(sum(train[,ind] == 1) < tol | sum(train[,ind] == 0) < tol){
    eliminarColumnas<- c(eliminarColumnas, ind)
    next
  }
  if(sum(train[target1,ind] == 1)/sum(target1) > sum(train[target0,ind] == 1)/sum(target0)){
    indicadorasChurn<- c(indicadorasChurn,ind)
  }  
  else{
    indicadorasNOChurn<- c(indicadorasNOChurn, ind)
  }
}
train["ind_churn"]<- rowSums(train[,indicadorasChurn])
png(filename = paste("gráficos/ind_churn.png", sep = ""))
par(mfrow=c(1,2))
hist(train[target0,"ind_churn"],xlab = "ind_churn",main = paste("ind_churn without churm"))
hist(train[target1,"ind_churn"],xlab = "ind_churn",main = paste("ind_churn with churm"))
dev.off()
train["ind_no_churn"]<- rowSums(train[,indicadorasNOChurn])
png(filename = paste("gráficos/ind_no_churn.png", sep = ""))
par(mfrow=c(1,2))
hist(train[target0,"ind_no_churn"],xlab = "ind_no_churn",main = paste("ind_no_churn without churm"))
hist(train[target1,"ind_no_churn"],xlab = "ind_no_churn",main = paste("ind_no_churn with churm"))
dev.off()

# Exploramos las variables de Importes
importes<- grep('imp', names(train), value=TRUE)
importesChurn<- character()
importesNOChurn<- character()
for(imp in importes){
  if("delta" %in% imp) next
  # Eliminamos las variables de importe con baja varianza
  if(sum(train[,imp] > 0) < tol){
    eliminarColumnas<- c(eliminarColumnas, imp)
    next
  }
  if(sum(train[target1,imp] > 0)/sum(target1) > sum(train[target0,imp] > 0)/sum(target0)){
    importesChurn<- c(importesChurn,imp)
  }  
  else{
    importesNOChurn<- c(importesNOChurn, imp)
  }
}
train["imp_churn"]<- rowSums(train[,importesChurn])
png(filename = paste("gráficos/imp_churn.png", sep = ""))
par(mfrow=c(1,2))
hist(train[target0,"imp_churn"],xlab = "imp_churn",main = paste("imp_churn without churm"))
hist(train[target1,"imp_churn"],xlab = "imp_churn",main = paste("imp_churn with churm"))
dev.off()
train["imp_no_churn"]<- rowSums(train[,importesNOChurn])
png(filename = paste("gráficos/imp_no_churn.png", sep = ""))
par(mfrow=c(1,2))
hist(train[target0,"imp_no_churn"],xlab = "imp_no_churn",main = paste("imp_no_churn without churm"))
hist(train[target1,"imp_no_churn"],xlab = "imp_no_churn",main = paste("imp_no_churn with churm"))
dev.off()

train<- train[,!(names(train) %in% eliminarColumnas)]

exploringModel<- lm(TARGET ~ ., train)
exploringModel

