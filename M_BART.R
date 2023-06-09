# 设置路径
setwd("D:/Users/R-BARTm")
library(rJava)
library(bartMachineJARs)
library(randomForest)
#to set a larger amount of RAM than the default of 500MB
options(java.parameters = "-Xmx5000m")
set_bart_machine_num_cores(6)
library(bartMachine)
library(ggplot2)
library(caret)

############################################### load data
# 设置路径,read data
setwd("D:/Users/R-BARTm")
dat_s1 <- read.csv("./soil_gx02.csv", fileEncoding = "UTF-8-BOM", header = T)
dat_s2 <- read.csv("./soil_ns02.csv", fileEncoding = "UTF-8-BOM", header = T)
dat_s3 <- read.csv("./soil_lj02.csv", fileEncoding = "UTF-8-BOM", header = T)
dat_s4 <- read.csv("./soil_qx02.csv", fileEncoding = "UTF-8-BOM", header = T)
dat_s5 <- read.csv("./soil_ql02.csv", fileEncoding = "UTF-8-BOM", header = T)
dim(dat_s1);dim(dat_s2);dim(dat_s3);dim(dat_s4);dim(dat_s5)
dat_s <- rbind(dat_s1, dat_s2, dat_s3,dat_s4,dat_s5)

dat_s <- dat_s[,-1]

dim(dat_s)
write.table(dat_s,"dat_s2.csv",row.names=FALSE,col.names=TRUE,sep=",")

#  summary(dat_s) 
library(psych)
describe(dat_s)

#################  绘图

library(plot3D)
# install.packages('scatterplot3d')
library(scatterplot3d)
dat_ss <- read.csv("./dat_s2.csv")

# 添加点标签

s3d <- scatterplot3d(dat_ss[,1:3], #pch = 16, color=colors
              )
text(s3d$xyz.convert(dat_ss[,1:3]), labels = dat_ss$Types,
     cex= 0.7, col = "black")

# 自定义点形状
"cl <- colors()
length(cl)
cl[1:6]"
detach(dat_s)
attach(dat_ss)
s3d <- scatterplot3d(N_Value, Es, h, pch = shapes,angle = 30)
legend("top", legend = levels(as.factor(dat_ss$Types)),
       pch = c(1, 2, 3, 4, 5, 6), 
       inset = -.15, xpd = TRUE, horiz = TRUE)

# 颜色1
par(mfrow = c(1,1), cex.lab = 1.11, font.lab = 2, cex = 0.83, family = 'serif')
shapes = c(10, 21, 22, 23, 24, 25)
shapes <- shapes[as.numeric(as.factor(dat_ss$Types))] # 按组更改点形状
colors <- c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00')
colors <- colors[as.numeric(as.factor(dat_ss$Types))]
s3d <- scatterplot3d(N_value, Es, h, pch = shapes, font.axis = 4, color=colors, cex.lab = 1.15, angle = 30)
legend("top", legend = levels(as.factor(dat_ss$Types)),
       col =  c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'), 
       pch = c(10, 21, 22, 23, 24, 25),cex = 1.25, inset = -0.1, xpd = TRUE, horiz = TRUE)
# 颜色2
par(mfrow = c(1,1), cex.lab = 1.11, font.lab = 2, cex = 0.83, family = 'serif')
shapes = c(10, 21, 22, 23, 24, 25)
shapes <- shapes[as.numeric(as.factor(dat_ss$Types))]
colors <- c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00')
colors <- colors[as.numeric(as.factor(dat_ss$Types))]
s3d <- scatterplot3d(Su, Es, h, pch = shapes, font.axis = 4, color=colors, cex.lab = 1.0, angle = 30)
legend("top", legend = levels(as.factor(dat_ss$Types)),
       col =  c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'), 
       pch = c(10, 21, 22, 23, 24, 25),cex = 1.25,inset = -0.1, xpd = TRUE, horiz = TRUE)
# 颜色
shapes = c(10, 21, 22, 23, 24, 25)
shapes <- shapes[as.numeric(as.factor(dat_ss$Types))]
colors <- c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00')
colors <- colors[as.numeric(as.factor(dat_ss$Types))]
s3d <- scatterplot3d(N_value, Es, h, pch = shapes, font.axis = 3, color=colors, cex.lab = 1.25, angle = 30)
legend("top", legend = levels(as.factor(dat_ss$Types)),
       col =  c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'), 
       pch = c(10, 21, 22, 23, 24, 25),inset = -0.15, cex = 1.15, xpd = TRUE, horiz = TRUE)

# convert
dat_s$Classification[dat_s$Classification == 'clay'] <- 1.0
dat_s$Classification[dat_s$Classification == 'clayey silt'] <- 2.0
dat_s$Classification[dat_s$Classification == 'sandy silt'] <- 3.0
dat_s$Classification[dat_s$Classification == 'silt'] <- 4.0
dat_s$Classification[dat_s$Classification == 'silty clay'] <- 5.0
dat_s$Classification[dat_s$Classification == 'silty sand'] <- 6.0

#  备用方案
par(mfrow = c(1,1), cex.lab = 1.5, font.lab = 3, cex = 0.83)
colvar <- as.numeric(as.factor(Types)) 
scatter3D(N_value, Es, h, colvar = colvar, phi = 45, theta = 45,
           NAcol = "white", breaks = NULL,
           colkey = NULL, panel.first = NULL, 
           clim = NULL, clab = NULL, 
          ticktype = "detailed",
           bty = "b2", CI = NULL, surf = NULL, 
           add = FALSE, plot = TRUE)

# https://www.bilibili.com/video/BV157411F7ZV?p=12&vd_source=2420c0c6bbc86080852b9b68230d0dba
#  调试
opar <- par(no.readonly = T)
par(mfrow = c(3,3), cex.lab = 1.5, font.lab = 3, cex = 0.83)
plot(Su,Es); plot(Cu,Es); plot(Cc,Es)
plot(ω,Es); plot(Gs,Es); plot(ρ,Es)
plot(IP,Es); plot(IL,Es); plot(N_value,Es)

opar <- par(no.readonly = T)
par(mfrow = c(3,3), cex.lab = 1.5, font.lab = 3, font.axis = 3, cex = 0.83)
ggplot(dat_s, aes(x = Su, y = Es, col = Types))+geom_point()
ggplot(dat_s, aes(x = Cu, y = Es, col = Types))+geom_point()
ggplot(dat_s, aes(x = Cc, y = Es, col = Types))+geom_point()
ggplot(dat_s, aes(x = ω, y = Es, col = Types))+geom_point()
ggplot(dat_s, aes(x = Gs, y = Es, col = Types))+geom_point()
ggplot(dat_s, aes(x = ρ, y = Es, col = Types))+geom_point()
ggplot(dat_s, aes(x = IP, y = Es, col = Types))+geom_point()
ggplot(dat_s, aes(x = IL, y = Es, col = Types))+geom_point()
ggplot(dat_s, aes(x = N_value, y = Es, col = Types))+geom_point()
# 散点图
par(opar)
windowsFonts()
ggplot(dat_s, aes(x = Su, y = Es, shape = Types , col = Types
                  ))+geom_point()+theme_bw()+
  theme(axis.title.x = element_text(face = 'italic', family = 'serif', size =14))+
  theme(axis.title.y = element_text(face ='italic', family = 'serif', size =14))+
  theme(text = element_text(face = 1, family = 'serif', size =14))+
  scale_shape_manual(values = c(10, 21, 22, 23, 24, 25))+
  scale_color_manual(values = c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'))
ggplot(dat_s, aes(x = Cu, y = Es, shape = Types , col = Types
))+geom_point()+theme_bw()+
  theme(axis.title.x = element_text(face = 'italic', family = 'serif', size =14))+
  theme(axis.title.y = element_text(face ='italic', family = 'serif', size =14))+
  theme(text = element_text(face = 1, family = 'serif', size =14))+
  scale_shape_manual(values = c(10, 21, 22, 23, 24, 25))+
  scale_color_manual(values = c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'))
ggplot(dat_s, aes(x = Cc, y = Es, shape = Types , col = Types
))+geom_point()+theme_bw()+
  theme(axis.title.x = element_text(face = 'italic', family = 'serif', size =14))+
  theme(axis.title.y = element_text(face ='italic', family = 'serif', size =14))+
  theme(text = element_text(face = 1, family = 'serif', size =14))+
  scale_shape_manual(values = c(10, 21, 22, 23, 24, 25))+
  scale_color_manual(values = c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'))
ggplot(dat_s, aes(x = ω, y = Es, shape = Types , col = Types
))+geom_point()+theme_bw()+
  theme(axis.title.x = element_text(face = 'italic', family = 'serif', size =14))+
  theme(axis.title.y = element_text(face ='italic', family = 'serif', size =14))+
  theme(text = element_text(face = 1, family = 'serif', size =14))+
  scale_shape_manual(values = c(10, 21, 22, 23, 24, 25))+
  scale_color_manual(values = c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'))
ggplot(dat_s, aes(x = Gs, y = Es, shape = Types , col = Types
))+geom_point()+theme_bw()+
  theme(axis.title.x = element_text(face = 'italic', family = 'serif', size =14))+
  theme(axis.title.y = element_text(face ='italic', family = 'serif', size =14))+
  theme(text = element_text(face = 1, family = 'serif', size =14))+
  scale_shape_manual(values = c(10, 21, 22, 23, 24, 25))+
  scale_color_manual(values = c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'))
ggplot(dat_s, aes(x = ρ, y = Es, shape = Types , col = Types
))+geom_point()+theme_bw()+
  theme(axis.title.x = element_text(face = 'italic', family = 'serif', size =14))+
  theme(axis.title.y = element_text(face ='italic', family = 'serif', size =14))+
  theme(text = element_text(face = 1, family = 'serif', size =14))+
  scale_shape_manual(values = c(10, 21, 22, 23, 24, 25))+
  scale_color_manual(values = c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'))
ggplot(dat_s, aes(x = IP, y = Es, shape = Types , col = Types
))+geom_point()+theme_bw()+
  theme(axis.title.x = element_text(face = 'italic', family = 'serif', size =14))+
  theme(axis.title.y = element_text(face ='italic', family = 'serif', size =14))+
  theme(text = element_text(face = 1, family = 'serif', size =14))+
  scale_shape_manual(values = c(10, 21, 22, 23, 24, 25))+
  scale_color_manual(values = c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'))
ggplot(dat_s, aes(x = IL, y = Es, shape = Types , col = Types
))+geom_point()+theme_bw()+
  theme(axis.title.x = element_text(face = 'italic', family = 'serif', size =14))+
  theme(axis.title.y = element_text(face ='italic', family = 'serif', size =14))+
  theme(text = element_text(face = 1, family = 'serif', size =14))+
  scale_shape_manual(values = c(10, 21, 22, 23, 24, 25))+
  scale_color_manual(values = c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'))
ggplot(dat_s, aes(x = N_value, y = Es, shape = Types ,  col = Types
))+geom_point()+theme_bw()+
  theme(axis.title.x = element_text(family = 'serif', size =14))+
  theme(axis.title.y = element_text(face ='italic', family = 'serif', size =14))+
  theme(text = element_text(face = 1, family = 'serif', size =14))+
  scale_shape_manual(values = c(10, 21, 22, 23, 24, 25))+
  scale_color_manual(values = c('#030303','#0000EE','#EE3B3B','#458B00','#006400','#FF7F00'))


par(opar)
hist(Su); hist(Cu); hist(Cc)
hist(ω); hist(Gs); hist(ρ)
hist(IP); hist(IL); hist(N_value)
hist(Es)

detach(dat_ss)    #从库中移除包
hist(Es);

attach(dat_s)
# install.packages("psych")
par()
library(psych)
describe(dat_s) 
### missingno plot 
# https://blog.csdn.net/devoteto/article/details/109054768
# install.packages(c("VIM","mice","corrplot"))
# dat_s <- dat_s[,-1]; dat_s <- dat_s[,-10]
opar <- par(no.readonly = T)
par(mfrow = c(1,1), cex.lab = 1.5, font.lab = 3, cex = 0.83)
windowsFonts()
library(mice)
md.pattern(dat_s)
md.pairs(dat_s)

library(VIM)
aggr(dat_s, prop = T, number = T,cex.lab = 1.2,font.lab = 2, family = '$serif', col = c('#BFEFFF','red','gray'))

# aggr(dat_s, prop = F, number = T)
matrixplot(dat_s,family = 'serif')

#### 影子矩阵
library(corrplot)
str(dat_s)
cordata<-as.data.frame(abs(is.na(dat_s)))
head(cordata)
sddata<-NULL
for(i in 1:length(cordata)){
  sddata<-c(sddata,sd(cordata[,i])>0)
}
sddata
cor1<-cor(cordata[,sddata])#去除没有缺失值的列

corrplot(cor1,method = "pie",type="lower", family = 'serif', col = COL1('Oranges', 200), font=3)

corrplot(cor1,method = "number",type="lower", font=3)


# data split

trainlist = createDataPartition(dat_s$Es, p= 0.7, list = F )
trainset = dat_s[trainlist,]
testset = dat_s[-trainlist,]
dim(trainset); dim(testset)

# RMSE MAE MAPE R2
reg_metric = function(true,predict){
  RMSE = sqrt(mean((true-predict)^2))
  MAE = mean(abs(true-predict))
  MAPE = mean(abs((true-predict)/true))
  R2 = 1-sum((true-predict)^2)/sum((true-mean(true))^2)
  return(list(RMSE = RMSE,
              MAE = MAE,
              MAPE = MAPE,
              R2 = R2))
}

#dim(na.omit(dat_s))
#dim(na.pass(dat_s))

br = bartMachine(trainset[,-11],trainset$Es,use_missing_data = TRUE,
                 use_missing_data_dummies_as_covars = TRUE)
pred_br = predict(br, testset[,-11])

#pred_br = round(predict(br, testset[,-11]))

cred_int = calc_credible_intervals(br, testset[,-11]) 
print(head(cred_int))

plot_convergence_diagnostics(br)
plot_y_vs_yhat(br,testset[,-11],testset$Es, prediction_intervals = TRUE)

sigsqs = get_sigsqs(br, plot_hist = TRUE)

cov_importance_test(br, covariates = c(1))
cov_importance_test(br, covariates = c(10))
check_bart_error_assumptions(br)
check_bart_error_assumptions(br, hetero_plot = "ys")
investigate_var_importance(br)

pd_plot(br, j = "label_s")

# xgboost
library(xgboost)
train_s = data.matrix(trainset[,-11]) 
train_l = list(data = train_s, label = trainset$Es)
train_m = xgb.DMatrix(data = train_l$data, label = train_l$label)

test_s = data.matrix(testset[,-11]) 
test_l = list(data = test_s, label = testset$Es)
test_m = xgb.DMatrix(data = test_l$data, label = test_l$label)
xgb_t = xgboost(data = train_m,  missing = NA, nround=25)
pred_xgb = predict(xgb_t, newdata = test_m)


pred_xgb = round(predict(xgb_t, newdata = test_m))
# Randoforest
# install.packages('partykit')
library(grid)
library(libcoin)
library(mvtnorm)
library(partykit)

cf <- cforest(Es ~h+Cu+Cc+ω+Gs+ρ+IP+IL+Classification+SPT.N, data = trainset, control = ctree_control(MIA = T))
cf1 <- cforest(Es ~h+Cu+Cc+ω+Gs+ρ+IP+IL+Classification+SPT.N, data = trainset)
cf2 <- cforest(Es ~h+Cu+Cc+ω+Gs+ρ+IP+IL+Classification+SPT.N, data = trainset,na.action = na.omit)
pred_cf = predict(cf2, newdata = testset[,-11])



ct <- ctree(Es ~h+Cu+Cc+ω+Gs+ρ+IP+IL+Classification+SPT.N, data = trainset, control = ctree_control(MIA = F))
ct1 <- ctree(Es ~h+Cu+Cc+ω+Gs+ρ+IP+IL+Classification+SPT.N, data = trainset)

pred_ct = predict(ct)
#varImpPlot(rf,main = "Variable Importance in Randomforset")
reg_metric(testset$Es, pred_br)
reg_metric(testset$Es, pred_xgb)
reg_metric(testset$Es, pred_cf)
reg_metric(trainset$Es, pred_ct)

# xgboost  调试
library(Matrix)
dat_s1 = data.matrix(dat_s[,-12]) 
dat_sp = Matrix(dat_s1, sparse = T)  # dense -> sparse
dat_l = list(data = dat_s1, label = dat_s$Es)
dat_m = xgb.DMatrix(data = dat_l$data, label = dat_l$label)
xgb_t = xgboost(data = dat_m,  nround=25)
pre_xgb = round(predict(xgb_t, newdata = dat_m))
reg_metric(dat_s$Es, pre_xgb)


