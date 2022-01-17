auta_audi <- read.csv(file = 'auta_audi.csv', stringsAsFactors = TRUE)
View(head(auta_audi,n=20))

#install.packages('tidyverse')
library(tidyverse)
library(dplyr)
#install.packages('sjmisc')
library(sjmisc)
nrow(auta_audi)

#install.packages("rcompanion")
require(rcompanion)
# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df), stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x = pull(df, xName)
    y = pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% dplyr::mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}




#drop NA
auta_audi <- auta_audi %>% filter(is.na(auta_audi$cena)!=TRUE)

str_contains(auta_audi$cena, ",")
?str_contains
# sprawdzanie czy kolumny ktore chcemy przekształcic zawieraja znaki nie alfanumeryczne
str_contains(auta_audi$Moc, ",")
str_contains(auta_audi$Moc, ".")
str_contains(auta_audi$Moc, " ")

str_contains(auta_audi$Pojemność.skokowa, ",")
str_contains(auta_audi$Pojemność.skokowa, ".")
str_contains(auta_audi$Pojemność.skokowa, " ")

# czyszczenie danych w wybranych kolumnach
auta_audi <- auta_audi %>% 
  mutate(cena= as.numeric(str_replace_all(auta_audi$cena, "[^\\d,]","" ) %>% str_replace_all(",",".")) )

auta_audi <- auta_audi %>% 
  mutate(Przebieg= as.numeric(str_replace_all(auta_audi$Przebieg, "[^\\d]","" ) %>% str_replace_all(",",".")) )

auta_audi <- auta_audi %>% 
  mutate(Pojemność.skokowa= as.numeric(str_replace_all(auta_audi$Pojemność.skokowa, "cm3","" ) %>% str_replace_all("[^\\d]","")) )

auta_audi <- auta_audi %>% 
  mutate(Moc= as.numeric(str_replace_all(auta_audi$Moc, "[^\\d]","" ) ))

# filtrujemy uszkodzone
auta_audi <- auta_audi %>% filter(is.na(auta_audi$Uszkodzony)!=FALSE)


audiZmienione<-auta_audi%>%select(cena,Oferta.od,Model.pojazdu,Rok.produkcji,Przebieg,Pojemność.skokowa,Rodzaj.paliwa,Moc,Skrzynia.biegów,Napęd,Typ.nadwozia,Liczba.drzwi,Liczba.miejsc,Kolor,Stan,Bezwypadkowy)

View(head(audiZmienione,n=20))
summary(audiZmienione)


summary(auta_audi$Moc)
summary(auta_audi)
view(mixed_assoc(audiZmienione))

# install.packages("vtreat")
library(vtreat)

# install.packages("vtreat")
library(vtreat)



names(audiZmienione) <- gsub("ść","sc",names(audiZmienione))
names(audiZmienione) <- gsub("ó","o",names(audiZmienione))
names(audiZmienione) <- gsub("ę","e",names(audiZmienione))
treatmentPlan<-design_missingness_treatment(audiZmienione,varlist = c("Przebieg","Pojemnosc.skokowa","Naped","Liczba.miejsc","Bezwypadkowy"))

prepared<-prepare(treatmentPlan,audiZmienione)
summary(prepared)
prepared<-prepared %>% mutate(Bezwypadkowy=as.factor(Bezwypadkowy))
View(mixed_assoc(prepared))

#DPLYR GROUP_BY SUMMARISE
#DPLYR GROUP_BY SUMMARISE

audiZmienione%>%group_by(Model.pojazdu,rok)%>%summarise(m=mean(Pojemność.skokowa))
audiZmienione%>%group_by(Model.pojazdu)%>%summarise(m=mean(Pojemność.skokowa)) prepared<-prepared%>%mutate(Naped=as.factor(Naped))

prepared<-prepared%>%filter(is.na(Moc)!=TRUE)
prepared<-prepared%>%filter(is.na(Skrzynia.biegow)!=TRUE)
prepared<-prepared%>%filter(is.na(Liczba.drzwi)!=TRUE)
View(prepared%>%group_by(Model.pojazdu)%>%summarise(n=n()) )

# str(preparedB)

#install.packages(c("regclass","caTools","randomForest","rpart","e1071","kknn","caret","mlr"))
library(caTools)
library(randomForest)
library(rpart)
library(e1071)
library(kknn)
library(caret)

preparedB<- prepared%>%group_by(Model.pojazdu)%>%
  filter(n()>50)%>%ungroup()
preparedB<-as.data.frame(preparedB)

preparedB<-preparedB%>% filter(Rodzaj.paliwa=="Benzyna" |Rodzaj.paliwa=="Diesel" )
View(mixed_assoc(preparedB))

preparedB<- preparedB%>% droplevels()
set.seed(2022)

sample<-sample.split(Y=preparedB,SplitRatio = .75)

trains<-subset(preparedB,sample==TRUE)
tests<-subset(preparedB,sample==FALSE)

modelRL<- glm(data=trains,Rodzaj.paliwa~.,family=binomial(link = "logit"))

summary(modelRL)

predictRL<-predict(modelRL,tests,type="response")
predictionsRL<-ifelse(predictRL>0.5,"Diesel","Benzyna")

table(truth=tests$Rodzaj.paliwa,prediction=predictionsRL)

cmrl<- table(truth=tests$Rodzaj.paliwa, prediction=predictionsRL)
acc<- (cmrl[1,1]+ cmrl[2,2])/(cmrl[1,1] + cmrl[2,2] + cmrl[1,2] + cmrl[2,1])


linearR1 <- lm(cena~Rok.produkcji,trains)
predictR1<- predict(linearR1,tests)
myMAElinear<- mean(abs(tests$cena-predictR1))
myMAEmean<- mean(abs(tests$cena-mean(trains$cena)))

# wczytuje tabele od Kawsniewicza
load("./preparedB")

# install.packages("mlr")
library(mlr)

zadanieRegresja<- mlr::makeRegrTask(id="regTask",preparedB,target="cena")

n<-getTaskSize(zadanieRegresja)
set.seed(123)
train.set<-sample(n, size=(n*3/4) )
'%ni%'<-Negate('%in%')
test.set<-seq(1,n,by=1)
test.set<-test.set[test.set%ni%train.set]


learner_rf<-makeLearner("regr.randomForest",id="rf",fix.factors.prediction = TRUE)
cost_rf<-train(learner_rf,zadanieRegresja,subset = train.set)
fitted_rf<-predict(cost_rf,zadanieRegresja,subset=test.set)
maeRF<- mean( abs(fitted_rf$data$truth-fitted_rf$data$response) )

learner_rpart<-makeLearner("regr.rpart",id="rpart",fix.factors.prediction = TRUE)
cost_rpart<-train(learner_rpart,zadanieRegresja,subset = train.set)
fitted_rpart<-predict(cost_rpart,zadanieRegresja,subset=test.set)
maeRF<- mean( abs(fitted_rpart$data$truth-fitted_rpart$data$response) )

learner_kknn <- makeLearner("regr.kknn",id="kknn",fix.factors.prediction = TRUE)
cost_kknn <- train(learner_kknn,zadanieRegresja,subset = train.set)
fitted_kknn <- predict(cost_kknn,zadanieRegresja,subset=test.set)
maeKKNN <- mean( abs(fitted_kknn$data$truth-fitted_kknn$data$response))

learner_svm <- makeLearner("regr.svm", id="svm", fix.factors.prediction = TRUE)
cost_svm <- train(learner_svm,zadanieRegresja,subset = train.set)
fitted_svm <- predict(cost_svm, zadanieRegresja, subset=test.set)
maeSvm <- mean(abs(fitted_svm$data$truth-fitted_svm$data$response))

learner_lm <- makeLearner("regr.lm", id="lm", fix.factors.prediction = TRUE)
cost_lm <- train(learner_lm,zadanieRegresja,subset = train.set)
fitted_lm <- predict(cost_lm, zadanieRegresja, subset=test.set)
maeLm <- mean (abs(fitted_lm$data$truth-fitted_lm$data$response) )

benchmark(
  learners=list(learner_rf,learner_rpart,learner_kknn,learner_svm,learner_lm),
  tasks=zadanieRegresja,
  resamplings=cv3,
  measures=mae,
  keep.pred = TRUE,
  keep.extract = FALSE,
  models = FALSE
)

ctrl<-makeTuneControlRandom(maxit = 10)
rdesc<-makeResampleDesc("CV",iters=3)
params<-makeParamSet(makeIntegerParam("ntree",500,800) )
resKK<-tuneParams(learner_rf,task=zadanieRegresja,resampling = rdesc,par.set = params,control=ctrl,measures = mae)
learner_rf_tuned<-makeLearner("regr.randomForest",id="rftuned",fix.factors.prediction = TRUE)
learner_rf_tuned<-setHyperPars(learner_rf_tuned,par.vals = resKK$x)