# Find the best strategies to improve for the next marketing campaign. 
# How can the financial institution have a greater effectiveness for 
# future marketing campaigns? In order to answer this, we have to analyze
# the last marketing campaign the bank performed and identify from the data
# the costumers profiles


# load packages and set seed
#####
set.seed(23)
require(ggplot2)
require(ggcorrplot)
require(class)
require(dplyr)
require(pROC)
require(psych)
require(leaps)
require(boot)
require(parallel)
require(ggrepel)
require(doMC)
require(doParallel)
#####

# initial analysis, load data and see dim
#####
rm( list = ls())
setwd("~/Desktop/bank")
bank = read.csv("bank-additional-full.csv", sep=";")
###
#remo<-which(bank$job=="retired" & bank$age<=40)
#remo
#bank = bank[-remo,]
###
rapresentative_sample = function(bank){
  n_yes = length(bank[bank$y =="yes", "y"])
  n_no = length(bank[bank$y =="no", "y"])
  yes_in = sample(which(bank$y=="yes"), size = n_yes/2, replace=FALSE)
  no_in = sample(which(bank$y=="no"), size = n_no/2, replace=FALSE)
  train = bank[c(yes_in, no_in), ]; dim(train)[1] == dim(bank)[1]/2
  
  no_train = bank[c(-yes_in, -no_in),]; dim(no_train)
  
  yes_in_2 = sample(which(no_train$y=="yes"), 
                    size = length(no_train[no_train$y =="yes", "y"])/2, replace=FALSE)
  no_in_2 = sample(which(no_train$y=="no"), 
                   size = length(no_train[no_train$y =="no", "y"])/2, replace=FALSE)  
  validation = no_train[c(-yes_in_2 , -no_in_2),]
  test = no_train[c(yes_in_2, no_in_2),]
  mylist = list("train" = train, "validation" = validation, "test" = test)
  return(mylist)
  print(dim(train)[1] + dim(validation)[1] + dim(test)[1] == dim(bank)[1])
}
df = rapresentative_sample(bank)
b.train = df$train
b.validation = df$validation
b.test = df$test
# some check
length(b.train[b.train$y=="yes","y"]) == (length(bank[bank$y =="yes", "y"]) / 2)

length(b.validation[b.validation$y=="yes","y"]) == (length(bank[bank$y =="yes", "y"]) / 4)

length(b.test[b.test$y=="yes","y"]) == (length(bank[bank$y =="yes", "y"]) / 4)

# so we have a training set consisting of 50% of the original dataset, with
# unbalanced yes and no qual to the originial, and 25% and 25% of the df for
# the validation and the test set, same condition. 
names(b.train)[21] = "response"
names(b.validation)[21] = "response"
names(b.test)[21] = "response"

names(b.validation)
move = b.validation[b.validation$default =="yes" & b.validation$age == 48,]
b.train = rbind(b.train, move)
b.validation = b.validation[-5404,]

###################################################
# study of outliers in retired
ggplot(b.train, aes(x=age, y =duration, color= response, shape = response)) +
  geom_point() +
  facet_wrap(~job, scales="free_y")
#ggsave("scatjobdur.png", dpi = 500)

###################################################


# pie plot
#####
pie_plot = function(b.train){
  row_yes =  length(b.train[b.train$response == "yes", "response"])
  row_no = length(b.train[b.train$response == "no", "response"])
  dim(b.train)[1] == row_yes + row_no
  prop_yes = round(row_yes / nrow(b.train) * 100, 2)
  prop_no = round(row_no / nrow(b.train) * 100, 2)
  prop = c(prop_yes, prop_no)
  response = c("yes", "no")
  lab_ypos = c(5.90, 56.3)
  nn = c(row_yes, row_no)
  df = data.frame(response, prop, nn, lab_ypos)
  pl = ggplot(df, aes(x = "", y = prop, fill = response)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = prop), color = "white", cex=4.5) +
    scale_fill_manual(values = c("#F8766D","#00BFC4")) +
    theme_void() +  
    theme(legend.position="left") +
    theme(legend.title = element_text("")) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=10), 
        legend.text=element_text(size=9))
  return(pl)
}
pie_plot(b.train)


ggsave("piechart.png", dpi = 300)
#####

#### BAR PLOT conditioned to the average duration #####
######################################################
par(mfrow=c(1,1))
mu = mean(duration) 
yes_above_length = length(bank[bank$duration > mu & bank$y == "yes", 
                               "duration" ]); yes_above_length
no_above_length = length(bank[bank$duration > mu & bank$y == "no", 
                              "duration" ]); no_above_length

pos = yes_above_length / (yes_above_length + no_above_length)
neg = no_above_length / (yes_above_length + no_above_length) 

yes_above = rep("yes above", yes_above_length)
no_above = rep("no above", no_above_length)
new = c(yes_above, no_above)
new = as.factor(new)
plot(new, col=c("yellow1","orange"))
legend("topright", legend = c("74.81%","25.18%"), 
       fill=c("yellow1","orange"), bty = "n" )


yes_below_length = length(bank[bank$duration < mu & bank$y == "yes",
                               "duration" ]); yes_below_length
no_below_length = length(bank[bank$duration < mu & bank$y == "no", 
                              "duration" ]); no_below_length

pos = yes_below_length / (yes_below_length + no_below_length)
neg = no_below_length / (yes_below_length + no_below_length) 


yes_below = rep("yes below", yes_below_length)
no_below = rep("no below", no_below_length)
new_1 = c(yes_below, no_below)
new_1 = as.factor(new_1)
plot(new_1, col=c("yellow1","orange"))
legend("topright", legend = c("95.64%"," 4.36%"), 
       fill=c("yellow1","orange"), bty = "n" )

attributes=c("no below", "yes below", "no above", "yes above")
att = factor(attributes, levels= c("no below", "yes below", "no above", "yes above"))

df  =  data.frame(attributes= att,
                 percentage=c(0.9564, 0.0436, 0.7481, 0.2518),
                 perc_100 = c(95.64, 4.36, 74.81, 25.18),
                 category = c("no", "yes", "no", "yes"))

# ggplot.
ggplot(data=df, aes(x=attributes, y=percentage, fill=category)) +
  geom_bar(stat="identity",  show.legend = F)  + xlab("") +
  labs(title ="Barplot based on average duration", 
          caption = "Increase in percantage of people signing a long term deposit
          when they are above the average duration") + 
  geom_text(aes(label=perc_100), position=position_dodge(0.9)) #facet_wrap(~ ,ncol=2)
ggsave("barplotdur.png", dpi = 200)
######################################################


# see boxplot for outliers, correlation plot
# study of quantitative variables
#####
summary(b.train)
quant_var = c("age", "duration", "campaign", "pdays", "previous",
              "emp.var.rate", "cons.price.idx", "cons.conf.idx",
              "euribor3m","nr.employed")
par(mfrow=c(1,1))
for(j in 1:length(quant_var)){
ggplot(b.train, aes(x = y, y = b.train[, quant_var[j]])) +
   geom_boxplot(show.legend = F) 
}

par(mfrow=c(2,3), mar = c(2,2,2,2))
for(j in 1:6) boxplot(b.train[, quant_var[j]] ~ b.train$y, main=quant_var[j])

par(mfrow=c(2,2), mar = c(2,2,2,2))
for(j in 7:10) boxplot(b.train[, quant_var[j]] ~ b.train$y, main=quant_var[j])
# duration, emp.var.rate, cons.price.idx, euribor3m, nr.employes
cor_matrix = cor(b.train[, quant_var[c(2, 6, 7, 8, 9, 10)]])
# install.packages("ggcorrplot")

ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", # remove euribor3m
           lab = TRUE) 
ggsave("correlationplot1.png", dpi = 200)

cor_matrix_1 = cor(b.train[, quant_var[c(2, 6, 7, 8, 10)]]) # remove emp.var.rate
ggcorrplot(cor_matrix_1, hc.order = TRUE, type = "lower",
           lab = TRUE)

ggsave("correlationplot2.png", dpi = 200)


cor_matrix_2 = cor(b.train[, quant_var[c(2, 7, 8, 10)]])
ggcorrplot(cor_matrix_2, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggsave("correlationplot3.png", dpi = 200)
# stop and keep : 
#  cons.price.idx, cons.conf.idx, duration, nr.employed
                                                                          

ggcorrplot(cor_matrix_2, hc.order = TRUE, type = "lower",
           lab = TRUE) + 
labs(title = "Correlation plot")
ggsave("correlationplot3.png", dpi = 600)


# scatter plot campaign vs duration
#####
names(b.train)[21] = "response"
par(mfrow=c(1,1))
#l = bank$y =="yes"
#plot(bank$duration, bank$campaign, col=l+1, pch=l*1+2)
#plot(1:20, pch=1:20)
#abline(h=7, col="green")
levels(job)

ggplot(b.train, aes(x=duration, y=campaign, shape = response,color=response)) +
  geom_point() + 
  geom_point(aes(x = duration, y=campaign)) +
  geom_hline(yintercept=7, linetype="dashed", color = "red") +
  facet_wrap(~ campaign)
  labs(fill="response")
  
  #+ geom_text(x=4000, y=8, label="# of contacts > 7")
ggsave("durcamp.png", dpi = 1200)

#####

# EDA of density
#####
boxplot(b.train$duration)
boxplot(log(b.train$duration)~ b.train$response)
par(mfrow=c(2,1))
plot(density(log(b.train$duration)))
plot(density(b.train$duration))
#####

# barplot function for job vs subscription rate
#####
plot_bar_jobs = function(b.train){
  jobs_levels = levels(b.train$job)
  n = length(jobs_levels)
  rate = rep(0, n); yes = rep(0, n); tot = rep(0, n)
    for(i in 1:n){
      yes[i] = nrow(b.train[b.train$job == jobs_levels[i] & b.train$response =="yes",])
      tot[i] = nrow(b.train[b.train$job == jobs_levels[i],])
      rate[i] =  yes[i]/tot[i] * 100
    }
  
  new_df = data.frame(rate, jobs_levels)
  perc = rev(c("10.11%","13.42%","10.49%","34.29%","8.17%","10.72%","24.73%",
           "10.95%","10.59%","7.56%","6.94%","13.30%"))
  positions = rev(c("student", "retired", "unemployed","admin.", "management" ,
                    "self-employed", "housemaid", 
                    "technician" ,"unknown",  "services", 
                "entrepreneur", "blue-collar"))
  par(mfrow=c(1,1))
  plot = ggplot(new_df, aes(x=jobs_levels, y=rate, fill=jobs_levels)) +
    geom_bar(stat="identity",  show.legend = F) + coord_flip() +
    labs(x="Jobs", y ="Subscription rate") +
    geom_text(aes(label=perc), position=position_dodge(0.9), size = 3.6) + 
    scale_x_discrete(limits = positions)
  
  return(plot)
}
plot_bar_jobs(b.train)
##ggsave("barjob.png", dpi = 700)
#####

#####
par(mfrow=c(1,1), mar=c(2,2,1,1))
plot(density(log(b.train[b.train$y =="yes", "duration"])))
plot(density(log(b.train[b.train$y =="no", "duration"])))

hist(b.train[b.train$y =="yes", "duration"], xlim=c(1,10), prob=T, border = 2)
hist(b.train[b.train$y =="no", "duration"], add=T, prob=T, border=1)
#####

# boxplot indices #
#####
require(gridExtra)


plot1 = ggplot(data=b.train, aes(x=response, y=cons.conf.idx, fill=response)) +
  geom_boxplot(show.legend = F)

plot2 = ggplot(data=b.train, aes(x=response, y=cons.price.idx, fill=response)) +
  geom_boxplot(show.legend = F)

plot3 = ggplot(data=b.train, aes(x=response, y=nr.employed, fill=response)) +
  geom_boxplot(show.legend = F)
grid.arrange(plot1, plot2, plot3, ncol=3)

#ggsave("boxplotsfun.png", dpi = 500)
#####
# last plots

ggplot(b.train,aes(x=age, y =duration, color=response)) + geom_point()+
  facet_wrap(~default, scales="free_y") +
  labs(title = "default")

ggplot(b.train, aes(x=age, y =duration, color=response)) + geom_point()+
  facet_wrap(~ education, scales="free_y") +
  labs(title = "contact")

ggplot(b.train, aes(x=age,y =duration,color=response)) + geom_point() +
  facet_wrap(~ month, scales="free_y") +
  labs(title = "Month")


ggplot(b.train, aes(x=age,y =duration,color=response)) + geom_point()+
  facet_wrap(~ poutcome, scales="free_y") +
  labs(title = "poutcome")


# time series plot
#####
# months
plot_bar_month = function(b.train){
  month_levels = c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  n3 = length(month_levels)
  ra = rep(0, n3); ye = rep(0, n3); to = rep(0, n3)
  for(i in 1:n3){
    ye[i] = nrow(b.train[b.train$month == month_levels[i] & b.train$response =="yes",])
    to[i] = nrow(b.train[b.train$month == month_levels[i],])
    ra[i] =  ye[i]/to[i] * 100
  }
  x = seq(1:10)
  new_db = data.frame(ra, month_levels, x)
  par(mfrow=c(1,1))
  plot2=ggplot(data = new_db, aes(x = x, y = ra))+ 
    geom_point(aes(color="red"),show.legend=FALSE) + geom_line(data=new_db, aes(color="red"),show.legend=FALSE) +
     
    xlab("Month") + ylab("Subscription Rate")+ geom_text_repel(aes(label = month_levels))
  
  
  return(plot2)
}

plot_bar_month(b.train)
ggsave("ts_1.png", dpi = 500)

# days
plot_bar_days = function(b.train){
  day_levels = c("mon","tue","wed","thu","fri")
  n3 = length(day_levels)
  ra = rep(0, n3); ye = rep(0, n3); to = rep(0, n3)
  for(i in 1:n3){
    ye[i] = nrow(b.train[b.train$day_of_week == day_levels[i] & b.train$response =="yes",])
    to[i] = nrow(b.train[b.train$day_of_week == day_levels[i],])
    ra[i] =  ye[i]/to[i] * 100
  }
  x = seq(1:5)
  new_db = data.frame(ra, day_levels, x)
  par(mfrow=c(1,1))
  plot2=ggplot(data = new_db, aes(x = x, y = ra))+ 
    geom_point(aes(color="red"),show.legend=FALSE) + geom_line(data=new_db, aes(color="red"),
                                                               show.legend=FALSE)  +
    xlab("Day") + ylab("Subscription rate")+ geom_text_repel(aes(label = day_levels))
  
  
  return(plot2)
}
plot_bar_days(b.train)
ggsave("ts_2.png", dpi = 500)

# end of EDA
##########################################################################

# knn classifier
# try to build a black box machine

# knn with balanced dataset (unuseful)
#####
b.validation = b.validation[,-11]
names(b.validation)
create_knn_input = function(dataset){
  #?model.matrix
  X = model.matrix(response ~., data = dataset)
  X = as.data.frame(X)

# Then we standardize data, since the knn is based on the
# euclidean norm
  quant_variables = X[, c("age", "campaign", "pdays", "previous",
                        "emp.var.rate", "cons.price.idx", "cons.conf.idx",
                        "euribor3m","nr.employed")]
  standardized.X = scale(quant_variables)[,-1]
  categorical_variables = select(X,-c(age, campaign, pdays, previous, 
                                    emp.var.rate, cons.price.idx, 
                                    cons.conf.idx, euribor3m, nr.employed ) )

  quant_variables = scale(quant_variables)[,-1]
  standardized.X = cbind(categorical_variables, quant_variables)[,-1]
  return(standardized.X)
}
train.X = create_knn_input(b.train); names(train.X)
test.X = create_knn_input(b.validation)
names(test.X)
head(test.X)
train.y = b.train$response
test.y = b.validation$response


# main function 
best_knn = function(train.X, test.X, train.y, k){
  k_value = k
  accuracy_output = rep(NA, k_value)
  to_numeric = function(vec){
    n = length(vec)
    new = rep(0, n)
    for(j in 1:n){
      if(vec[j]=="yes") new[j] = 1 
    }
    return(new)
  }
  t_1 = to_numeric(test.y)
  for(j in 1:k_value){
    knn.pred = knn(train.X, test.X, train.y, k=j, prob=TRUE)
    print(paste("Solved for k = ", j))
    t_2 = attributes(knn.pred)$prob
    roc_out = roc(t_1, t_2)
    accuracy_output[j] = auc(roc_out)[1]
  }
  knn_output = accuracy_output
  x = seq(j)
  #max(knn_output)
  data = data.frame(x, knn_output)
  pl = ggplot(data = data, aes(x=x, y=knn_output)) + geom_line() +
    labs(x="K considered", y ="AUC") +
    geom_point(data= data, aes(x=which.max(knn_output), 
                               y=max(knn_output)), col="red") 
  
  k_best = knn(train.X, test.X, train.y, k=which.max(knn_output), prob=TRUE)
  t_2 = attributes(k_best)$prob
  roc_pl = plot(roc(t_1, t_2), print.auc=TRUE, legacy.axes=TRUE,
                xlab="False positive rate", ylab="True positive rate")
  conf.matrix = table(k_best, test.y)
  
  my_list = list("line" = pl, 
                 "roc_curve" = plot(roc(t_1, t_2), print.auc=TRUE, 													legacy.axes=TRUE,
                                    xlab="False positive rate", ylab="True positive rate"), 
                 "confusion_matrix" = conf.matrix)
  return(my_list)
  
}

knn_output = best_knn(train.X, test.X, train.y, 10)
knn_output$confusion_matrix
#####

# knn with unbalanced dataset
#####  
names(b.train)
b.train = b.train[,-11]
b.validation = b.validation[,-11]
names(b.validation)
create_knn_input = function(dataset){
  #?model.matrix
  X = model.matrix(response ~., data = dataset)
  X = as.data.frame(X)
  
  # standardize data, since knn is based on the
  # euclidean norm
  require(dplyr)
  quant_variables = X[, c("age", "campaign", "pdays", "previous",
                          "emp.var.rate", "cons.price.idx", "cons.conf.idx",
                          "euribor3m","nr.employed")]
  standardized.X = scale(quant_variables)[,-1]
  categorical_variables = select(X, -c(age, campaign, pdays, previous, 
                                      emp.var.rate, cons.price.idx, 
                                      cons.conf.idx, euribor3m, nr.employed) )
  
  quant_variables = scale(quant_variables)[,-1]
  standardized.X = cbind(categorical_variables, quant_variables)[,-1]
  return(standardized.X)
}
train.X = create_knn_input(b.train); dim(train.X)
test.X = create_knn_input(b.validation); dim(test.X)
train.y = b.train$response; length(train.y)
test.y = b.validation$response; length(test.y)

# parallelize
detectCores()
set.seed(23)
# main function
best_knn = function(train.X, test.X, train.y, i, k){
  k_value = k
  accuracy_output = rep(NA, i)
  to_numeric = function(vec){
    n = length(vec)
    new = rep(0, n)
    for(j in 1:n){
      if(vec[j]=="yes") new[j] = 1 
    }
    return(new)
  }
  
  t_1 = to_numeric(test.y)
  
  out_i = sort(sample(k_value, size = i, replace = FALSE))
  count = seq(i)
  
  registerDoMC(cores= 3)
  registerDoParallel(cores = 3)
  knn.pred = foreach(j = out_i, .combine=cbind) %dopar% {
    attributes(knn(train.X, test.X, train.y, k = j , prob = TRUE))$prob
  }

  for(j in 1:length(out_i)){
    t_2 = knn.pred[,j]
    roc_out = roc(t_1, t_2)
    accuracy_output[j] = auc(roc_out)[1]
  }
  
  print(paste("Finished the machine consuming part, I am starting some plots"))
  
  knn_output = na.omit(accuracy_output)[1:length(out_i)]
  #x = seq(j)
  #max(knn_output)
  data = data.frame(out_i, knn_output)
  
  pl = ggplot(data = data, aes(x=out_i, y=knn_output)) + geom_line(linetype = "dashed", 
                                                                   col="#00AFBB") +
    geom_point(col="#00AFBB") + 
    labs(x="K considered", y ="AUC") +
    geom_point(data = data, aes(x=data[which.max(knn_output), "out_i"], 
                                y=max(knn_output)), col="#FC4E07") 
  
  k_best = knn(train.X, test.X, train.y, k=data[which.max(knn_output), "out_i"], prob=TRUE)
  t_2 = attributes(k_best)$prob
  roc_pl = plot(roc(t_1, t_2), print.auc=TRUE, legacy.axes=TRUE,
                xlab="False positive rate", ylab="True positive rate")
  
  conf.matrix = table(k_best, test.y)
  
  my_list = list("line" = pl, 
                 "roc_curve" = plot(roc(t_1, t_2), print.auc=TRUE, legacy.axes=TRUE,
                                    xlab="False positive rate", ylab="True positive rate"), 
                 "confusion_matrix" = conf.matrix)
  return(my_list)
}

knn_output = best_knn(train.X, test.X, train.y, i=25, k = 500)

knn_output$confusion_matrix
knn_output$line
ggsave("knnline.png", dpi = 500)
plot(knn_output$roc_curve, print.auc=T, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
best_knn = coords(knn_output$roc_curve, "best")
ggsave("aucknn.png", dpi = 500)

#####


###########################################
#           Logistic Regression         #
##########################################

# pday is 999 if person has never been contacted, the number of contact otherwise
# we set 999 to 0
length(b.train[b.train$pdays==999,"pdays"])
b.train[b.train$pdays==999,"pdays"] <- 0
ggplot(b.train, aes(x=previous, y =pdays, color=poutcome))  + geom_point()

names(b.train)
b.train = b.train[ , -11] # remove duration from the model

names(b.train)
# full model
fit_full = glm(response~.- emp.var.rate - euribor3m, 
               family = "binomial", data= b.train)
summary(fit_full)

# model reduced by hand
fit_red = glm(response ~ default + contact + month + day_of_week +
                poutcome +  previous + cons.conf.idx +  nr.employed + campaign, 
              family = "binomial", data = b.train)
summary(fit_red)


# method backward
set.seed(23)
regfit.bkw = regsubsets(response~. - emp.var.rate - euribor3m,
                        data=b.train, method="backward")
summary(regfit.bkw)


glm_model_back  =  glm(response ~  contact
                         + month + poutcome 
                       + cons.conf.idx + nr.employed , 
                       family= "binomial", data=b.train)

summary(glm_model_back)


#method hybrid
regfit.hyb = regsubsets(response~. - emp.var.rate - euribor3m,
                        data=b.train, method="seqrep")
summary(regfit.hyb)

glm_model_hyb  =  glm(response ~ contact + month + poutcome +
                       + nr.employed + cons.conf.idx, 
                      family= "binomial", data=b.train)
summary(glm_model_hyb)
# method exaustive
regfit.exh = regsubsets(response~.- emp.var.rate - euribor3m, method="exhaustive",
                        data=b.train, really.big = T)

summary(regfit.exh)
glm_model_exh  =  glm(response ~ month + contact + poutcome +
                       + nr.employed + cons.conf.idx
                      , family= "binomial", data=b.train)


# test the model
#####
# 
par(mfrow=c(1,1))
plot(regfit.bkw, scale="Cp", col="lightgreen")
# 
plot(regfit.hyb, scale="bic", col="lightblue")
plot(regfit.exh, scale="bic", col="orange2")

#####         

# see bic descendt and compare it between models
par(mfrow=c(1,3))
plot(summary(regfit.exh)$bic,type="l")
min.bic=which.min(summary(regfit.exh)$bic)
points(min.bic,summary(regfit.exh)$bic[min.bic],col="red",pch=16)

plot(summary(regfit.bkw)$bic,type="l")
min.bic=which.min(summary(regfit.bkw)$bic)
points(min.bic,summary(regfit.bkw)$bic[min.bic],col="red",pch=16)

plot(summary(regfit.hyb)$bic,type="l")
min.bic=which.min(summary(regfit.hyb)$bic)
points(min.bic,summary(regfit.hyb)$bic[min.bic],col="red",pch=16)


# comparison of deviance
# testing nested models
anova(glm_model_exh,fit_red,test="Chisq")
# from this output we notice that this variables are significant, 
# so they shouldn't be cut off.

# confusion matrix

# odd ratios
summary(glm_model_exh)
exp(cbind(coef(glm_model_exh)))
summary(glm_model_exh)
# validate the model

av_model = function(model, b.validation){
  logistic.prob = predict(fit_red, newdata = b.validation, type="response")
  logistic.pred = rep("No", nrow(b.validation))

  par(mfrow = c(1,1))
  roc.out = roc(b.validation$response, logistic.prob, levels=c("no","yes"))
  g = plot(roc.out, print.auc=T, legacy.axes=T, xlab="False positive rate", 
     ylab="True positive rate")
  x = coords(roc.out, "best")
  #the overall training rate is 
  logistic.pred_best = rep("No", nrow(b.validation))
  logistic.pred_best[logistic.prob > x[1]] = "Yes"
  conf_matrix = table(logistic.pred_best, b.validation$response)
  #return(conf_matrix)
  #return(g)
  return(roc(b.validation$response, logistic.prob)$auc[1])
}


m_1 = mean(c(av_model(fit_red, b.validation), av_model(fit_red, b.test)))

m_2 = mean(c(av_model(glm_model_back, b.validation), av_model(glm_model_back, b.test)))

m_3 = mean(c(av_model(glm_model_exh, b.validation), av_model(glm_model_exh, b.test)))

m_4 = mean(c(av_model(glm_model_hyb, b.validation), av_model(glm_model_hyb, b.test)))
# same results 

av_model = function(model, b.validation){
  logistic.prob = predict(model, newdata = b.validation, type="response")
  logistic.pred = rep("No", nrow(b.validation))
  
  par(mfrow = c(1,1))
  roc.out = roc(b.validation$response, logistic.prob, levels=c("no","yes"))
  g = plot(roc.out, print.auc=T, legacy.axes=T, xlab="False positive rate", 
           ylab="True positive rate")
  x = coords(roc.out, "best")
  # the overall training rate is 
  logistic.pred_best = rep("No", nrow(b.validation))
  logistic.pred_best[logistic.prob > x[1]] = "Yes"
  conf_matrix = table(logistic.pred_best, b.validation$response)
  my_list = list("conf" = conf_matrix, "outputs" = x)
  return(my_list)
  #return(g)
  #return(roc(b.validation$response, logistic.prob)$auc[1])
}

av_model(fit_red, b.validation)$conf
av_model(fit_red, b.validation)$outputs

av_model(glm_model_exh, b.test)$conf
av_model(glm_model_exh, b.test)$outputs
#####

### bootstrap and some other 
### ways to improve model
#####

fit_red = glm(response ~ month + contact + poutcome +
                + nr.employed + cons.conf.idx, 
              family = "binomial", data = b.train)
summary(fit_red)

bootstrap_data = function(){
  n_yes = length(b.train[b.train$response =="yes", "response"])
  n_no = length(b.train[b.train$response =="no", "response"])
  yes_in = sample(which(b.train$response=="yes"), size = n_yes, replace=TRUE)
  no_in = sample(which(b.train$response=="no"), size = n_no, replace=TRUE)
  train = b.train[c(yes_in, no_in), ]
  return(train)
}


trials = 1000
registerDoMC(cores= 3)
registerDoParallel(cores = 3)
r = foreach(icount(trials), .combine=cbind) %dopar% {
    ind = bootstrap_data()
    result = glm(response ~  month + contact + poutcome +
                   + nr.employed + cons.conf.idx, 
                   family= "binomial", data = ind)
    coefficients(result)
}
head(r); dim(r)

b.df = as.data.frame(r)
head(b.df)
b.df = t(b.df)
head(b.df); dim(b.df)
boot_coef = apply(b.df, 2, mean)

par(mfrow=c(3,3), mar=c(4,4,2,2))
for(j in 1:9){
hist(r[j,], col="grey", main = names(fit_red$coefficients)[j], xlab = "",
     prob=T)
legend("topright", bty = "n", legend = c("bootstrap", "estimated"), lty=c(1,2), 
       col=c("red", "blue"))
abline(v = coefficients(fit_red)[j], col="red", lwd=3)
abline(v = boot_coef[j], col="blue", lwd=3, lty=2)

}
length(boot_coef)
par(mfrow=c(3,2), mar=c(4,4,2,2))
for(j in 10:15){
  hist(r[j,], col="grey", main = names(fit_red$coefficients)[j], xlab = "",
       prob=T)
  legend("topright", bty = "n", legend = c("bootstrap", "estimated"), lty=c(1,2), 
         col=c("red", "blue"))
  abline(v = coefficients(fit_red)[j], col="red", lwd=3)
  abline(v = boot_coef[j], col="blue", lwd=3, lty=2)
}

X = model.matrix(response~., data = b.train)
head(X)
model_m = as.data.frame(X[, names(boot_coef)])
names(model_m)
head(model_m)

# copied this function from github, we need it to generate
# the same output of a glm model, but with the boostrap coefficients
# and then use the function predict
makeglm <- function(formula, ..., family, data=NULL) {
  dots <- list(...)
  out<-list()
  tt <- terms(formula, data=data)
  if(!is.null(data)) {
    mf <- model.frame(tt, data)
    vn <- sapply(attr(tt, "variables")[-1], deparse)
    
    if((yvar <- attr(tt, "response"))>0)
      vn <- vn[-yvar]
    xlvl <- lapply(data[vn], function(x) if (is.factor(x))
      levels(x)
      else if (is.character(x))
        levels(as.factor(x))
      else
        NULL)
    attr(out, "xlevels") <- xlvl[!vapply(xlvl,is.null,NA)]
    attr(tt, "dataClasses") <- sapply(data[vn], stats:::.MFclass)
  }
  out$terms <- tt
  coef <- numeric(0)
  stopifnot(length(dots)>1 & !is.null(names(dots)))
  for(i in seq_along(dots)) {
    if((n<-names(dots)[i]) != "") {
      v <- dots[[i]]
      if(!is.null(names(v))) {
        coef[paste0(n, names(v))] <- v
      } else {
        stopifnot(length(v)==1)
        coef[n] <- v
      }
    } else {
      coef["(Intercept)"] <- dots[[i]]
    }   
  }
  out$coefficients <- coef
  out$rank <- length(coef)
  if (!missing(family)) {
    out$family <- if (class(family) == "family") {
      family
    } else if (class(family) == "function") {
      family()
    } else if (class(family) == "character") {
      get(family)()
    } else {
      stop(paste("invalid family class:", class(family)))
    }
    out$qr <- list(pivot=seq_len(out$rank))
    out$deviance <- 1
    out$null.deviance <- 1
    out$aic <- 1
    class(out) <- c("glm","lm")
  } else {
    class(out) <- "lm"
    out$fitted.values <- predict(out, newdata=dd)
    out$residuals <- out$mf[attr(tt, "response")] - out$fitted.values
    out$df.residual <- nrow(data) - out$rank
    out$model <- data
    #QR doesn't work
  }
  out
}



boot_model = makeglm(response ~  monthaug + monthdec +
                       monthjul + monthjun +
                       monthmar + monthmay + monthnov + monthoct +
                       monthsep +  contacttelephone + poutcomenonexistent + 
                       poutcomesuccess + nr.employed + cons.conf.idx ,
                     data = model_m, family = binomial,
                     boot_coef[1], 
                     monthaug = boot_coef[2],  
                     monthdec = boot_coef[3],
                     monthjul = boot_coef[4],
                     monthjun = boot_coef[5],
                       monthmar = boot_coef[6], 
                     monthmay = boot_coef[7],
                     monthnov = boot_coef[8], 
                     monthoct = boot_coef[9],
                     monthsep = boot_coef[10],
                     contacttelephone = boot_coef[11],
                     poutcomenonexistent = boot_coef[12], 
                     poutcomesuccess= boot_coef[13], 
                     nr.employed = boot_coef[14],
                     cons.conf.idx = boot_coef[15]
                       )

boot_model

prova_1  = model.matrix(response~., data = b.validation)
prova_1 = as.data.frame(prova_1)
prova_1$response = b.validation$response
predict(boot_model, newdata=prova_1, type="response")



logistic.prob = predict(boot_model, newdata = prova_1, type="response")
logistic.pred = rep("No", nrow(b.validation))

par(mfrow = c(1,1))
roc.out = roc(b.validation$response, logistic.prob)
plot(roc.out, print.auc=T, legacy.axes=T, xlab="False positive rate", 
         ylab="True positive rate")

x = coords(roc.out, "best")
#the overall training rate is 
logistic.pred_best = rep("No", nrow(prova_1))
logistic.pred_best[logistic.prob > x[1]] = "Yes"
table(logistic.pred_best, prova_1$response)
av_model(glm_model_exh, b.validation)


# try to use a gam approach
names(b.train)


pairs(b.train[,c("age", "campaign")], pch=".")
require(gam)
glm.gam <- gam(response ~ s(age,10), data=b.train, family='binomial')

c("age", "campaign", "pdays", "previous",
  "emp.var.rate", "cons.price.idx", "cons.conf.idx",
  "euribor3m","nr.employed")

K = 1:20
accept.aic = rep(0.0 , length(K))

for (i in 1:length(K)) {
  ns.k = gam(response ~ month  + contact + poutcome 
             + s(emp.var.rate, 1) + s(cons.price.idx, 1) + s(cons.conf.idx, 18), 
             data =b.train, family = "binomial")
  accept.aic[i] = extractAIC(ns.k)[2]
}
id = which.min(accept.aic)
accept.k.min = K[id] ; accept.k.min


final_m = gam(response ~ month  + contact + poutcome 
    + s(emp.var.rate, 1) + s(cons.price.idx, 1) + s(cons.conf.idx, 18), 
    data =b.train, family = "binomial")



# lasso, study only which variables are != from zero
library(glmnet)
X=model.matrix(response~ . ,data=b.train)[,-1]
y=b.train$response
m.lasso=glmnet(X, y, alpha=1, family="binomial")
plot(m.lasso)
cv.m.lasso=cv.glmnet(X, y, alpha=1, family="binomial")

plot(cv.m.lasso)
m_lasso = glmnet(X, y, alpha = 1, family = "binomial", lambda = cv.m.lasso$lambda.1se)
m_lasso$beta
par(mfrow=c(1,1))
plot(m.lasso, xvar = "lambda")
abline(v = log(cv.m.lasso$lambda.1se), lty = 2, col = "black")

#### end #### 
#############
