data<-read.csv("C:/~/train_mdf.csv",header=T)
raw<-read.csv("C:/~/train_mdf.csv",header=T)

library(naniar)
library(ggplot2)
library(corrplot)
library(rcompanion)
library(dplyr)
library(DMwR2)

#######-week1-#######
miss<-miss_var_summary(data)
vis_miss(data,cluster=T)

plot(data$PoolArea, data$SalePrice)
plot(data$MiscVal, data$SalePrice)


pool<-data[,c("PoolArea","PoolQC")]


tmp<-data
tmp$SF<-data$X1stFlrSF+data$X2ndFlrSF
sum(!(tmp$SF==tmp$GrLivArea))

miss<-data[is.na(data$BsmtFinType2),]
miss<-data[is.na(data$BsmtExposure),]


plot(data$OverallCond, data$OverallQual)
cor(data$OverallCond, data$OverallQual, method="spearman")
cor(data$RoofStyle, data$RoofMatl, method="spearman")


tmp$flg<-ifelse(tmp$OpenPorchSF!=0, 1, 
                ifelse(tmp$EnclosedPorch!=0,1,
                       ifelse(tmp$X3SsnPorch!=0,1,
                              ifelse(tmp$ScreenPorch!=0,1,0))))

tmp$BsmtArea<-tmp$BsmtFinSF1+tmp$BsmtFinSF2+tmp$BsmtUnfSF
sum(tmp$BsmtArea!=tmp$TotalBsmtSF)

boxplot(data$LotArea,main="Lot Area")
boxplot(data$SalePrice,main="Sale Price")
boxplot(data$GrLivArea,main="GrLivArea")

barplot(table(data$BedroomAbvGr),main="Number of Bedroom")
barplot(table(data$MSZoning),main="MSZoning")
barplot(table(data$TotRmsAbvGrd),main="Total Rooms AbvGrd")

barplot(table(data$MSSubClass),main="MSSubClass")
barplot(table(data$Neighborhood),main="Neighborhood",las=2)

barplot(table(data$Exterior1st),main="Exterior1st",las=2)
barplot(table(data$Exterior2nd),main="Exterior2nd",las=2)
sum(data$Exterior1st==data$Exterior2nd)


barplot(table(data$SaleType),main="SaleType")

barplot(table(data$Condition1),main="Condition1")
barplot(table(data$Condition2),main="Condition2")
sum(data$Condition1==data$Condition2)

barplot(table(data$Functional),main="Functional")

barplot(table(data$SaleCondition),main="SaleCondition")

barplot(table(data$FireplaceQu),main="FireplaceQu")

barplot(table(data$HouseStyle),main="HouseStyle")


area<-data[,c('LotArea','TotalBsmtSF','GrLivArea',
              'GarageArea','WoodDeckSF','PoolArea')]
corrplot(cor(area),type='lower',method='number')

#######-week2-#######

hist(data$GrLivArea)
boxplot(data$GrLivArea,main="GrLivArea",horizontal=T)


plot(density(data$GrLivArea))
rug(jitter(data$GrLivArea))


origin<-data[data$YearRemodAdd==data$YearBuilt,]
remod<-data[data$YearRemodAdd!=data$YearBuilt,]

par(mfrow=c(1,2))
boxplot(origin$SalePrice)
boxplot(remod$SalePrice)



linearity<-data[,c('LotArea','YearBuilt','YearRemodAdd',
                   'BsmtFinSF1','BsmtFinSF2','BsmtUnfSF',
                   'X1stFlrSF','X2ndFlrSF','LowQualFinSF','GrLivArea',
                   'BsmtFullBath','BsmtHalfBath','FullBath','HalfBath',
                   'BedroomAbvGr','KitchenAbvGr','TotRmsAbvGrd',
                   'Fireplaces','GarageCars','GarageArea',
                   'WoodDeckSF','OpenPorchSF','X3SsnPorch','ScreenPorch',
                   'PoolArea','MiscVal','SalePrice')]

par(mfrow=c(2,2))
plot(linearity[,10],linearity[,1],main='LotArea')
plot(linearity[,10],linearity[,2],main='YearBuilt')
plot(linearity[,10],linearity[,3],main='YearRemodAdd')
plot(linearity[,10],linearity[,4],main='BsmtFinSF1')

plot(linearity[,10],linearity[,5],main='BsmtFinSF2')
plot(linearity[,10],linearity[,6],main='BsmtUnfSF')
plot(linearity[,10],linearity[,7],main='1stFlrSF')
plot(linearity[,10],linearity[,8],main='2ndFlrSF')

plot(linearity[,10],linearity[,9],main='LowQualFinSF')
plot(linearity[,10],linearity[,11],main='BsmtFullBath')
plot(linearity[,10],linearity[,12],main='BsmtHalfBath')
plot(linearity[,10],linearity[,13],main='FullBath')

plot(linearity[,10],linearity[,14],main='HalfBath')
plot(linearity[,10],linearity[,15],main='BedroomAbvGr')
plot(linearity[,10],linearity[,16],main='KitchenAbvGr')
plot(linearity[,10],linearity[,17],main='TotRmsAbvGrd')

plot(linearity[,10],linearity[,18],main='Fireplaces')
plot(linearity[,10],linearity[,19],main='GarageCars')
plot(linearity[,10],linearity[,20],main='GarageArea')
plot(linearity[,10],linearity[,21],main='WoodDeckSF')

plot(linearity[,10],linearity[,22],main='OpenPorchSF')
plot(linearity[,10],linearity[,23],main='X3SsnPorch')
plot(linearity[,10],linearity[,24],main='ScreenPorch')
plot(linearity[,10],linearity[,27],main='SalePrice')


#######-week3-#######
data<-raw
#mssubclass
barplot(table(data$MSSubClass),main="MSSubClass")
boxplot(data$GrLivArea~data$MSSubClass)
data[data$MSSubClass==20,"MSSubClass"]=1
data[data$MSSubClass==30,"MSSubClass"]=1
data[data$MSSubClass==40,"MSSubClass"]=1
data[data$MSSubClass==45,"MSSubClass"]=2
data[data$MSSubClass==50,"MSSubClass"]=2
data[data$MSSubClass==60,"MSSubClass"]=3
data[data$MSSubClass==70,"MSSubClass"]=3
data[data$MSSubClass==75,"MSSubClass"]=3
data[data$MSSubClass==80,"MSSubClass"]=4
data[data$MSSubClass==85,"MSSubClass"]=4
data[data$MSSubClass==90,"MSSubClass"]=4
data[data$MSSubClass==120,"MSSubClass"]=1
data[data$MSSubClass==150,"MSSubClass"]=2
data[data$MSSubClass==160,"MSSubClass"]=3
data[data$MSSubClass==180,"MSSubClass"]=4
data[data$MSSubClass==190,"MSSubClass"]=4
barplot(table(data$MSSubClass),main="MSSubClass")

#mszoning
barplot(table(data$MSZoning),main="MSZoning")
boxplot(raw$GrLivArea~raw$MSZoning)
rl<-subset(raw, MSZoning=="RL");rl<-rl[,47]
rm<-subset(raw, MSZoning=="RM");rm<-rm[,47]
var.test(rl,rm)
t.test(rl,rm)
plot(density(rm))
lines(density(rl))
data[data$MSZoning=="RL","MSZoning"]=1
data[data$MSZoning=="RM","MSZoning"]=2
data[data$MSZoning=="RH","MSZoning"]=3
data[data$MSZoning=="FV","MSZoning"]=3
data[data$MSZoning=="C (all)","MSZoning"]=3
barplot(table(data$MSZoning),main="MSZoning")

#lotfrontage
plot(data$LotFrontage, data$GrLivArea, main='LotFrontage')
lf<-data[is.na(data$LotFrontage),]
lf<-lf[,c("Id","LotFrontage")]
box<-data[!(is.na(data$LotFrontage)),'LotFrontage']
sample<-sample(box,size=259,replace=T)
lf$LotFrontage<-sample
j=1
for(i in 1:1460){
  if(is.na(data$LotFrontage[i])){
    data$LotFrontage[i]<-lf$LotFrontage[j]
    j<-j+1
  }
}
write.csv(data,'tmp.csv')

#lotarea
plot(data$LotArea,data$GrLivArea)

#street
barplot(table(data$Street),main="Street")
data<-subset(data, select=-c(Street))

#alley
barplot(table(data$Alley),main="Alley")
sum(is.na(data$Alley))
data<-subset(data, select=-c(Alley))

#lotshape
barplot(table(data$LotShape),main="LotShape")
data[data$LotShape=="IR1","LotShape"]=1
data[data$LotShape=="IR2","LotShape"]=1
data[data$LotShape=="IR3","LotShape"]=1
data[data$LotShape=="Reg","LotShape"]=0

#landcontour
barplot(table(data$LandContour),main="LandContour")
data[data$LandContour=="Bnk","LandContour"]=1
data[data$LandContour=="HLS","LandContour"]=1
data[data$LandContour=="Low","LandContour"]=1
data[data$LandContour=="Lvl","LandContour"]=0

#utilities
barplot(table(data$Utilities),main="Utilities")
data<-subset(data, select=-c(Utilities))

#lotconfig
barplot(table(data$LotConfig),main="LotConfig")
data[data$LotConfig=="Inside","LotConfig"]=1
data[data$LotConfig=="Corner","LotConfig"]=2
data[data$LotConfig=="CulDSac","LotConfig"]=3
data[data$LotConfig=="FR2","LotConfig"]=3
data[data$LotConfig=="FR3","LotConfig"]=3

#landslope
barplot(table(data$LandSlope),main="LandSlope")
data[data$LandSlope=="Gtl","LandSlope"]=0
data[data$LandSlope=="Mod","LandSlope"]=1
data[data$LandSlope=="Sev","LandSlope"]=1

#neighborhood
barplot(table(data$Neighborhood),main="Neighborhood")
data[data$Neighborhood=="NAmes","Neighborhood"]=1
data[data$Neighborhood=="NoRidge","Neighborhood"]=1
data[data$Neighborhood=="NPkVill","Neighborhood"]=1
data[data$Neighborhood=="NridgHt","Neighborhood"]=1
data[data$Neighborhood=="NWAmes","Neighborhood"]=1
data[data$Neighborhood=="Veenker","Neighborhood"]=1
data[data$Neighborhood=="StoneBr","Neighborhood"]=1
data[data$Neighborhood=="Blmngtn","Neighborhood"]=1
data[data$Neighborhood=="BrkSide","Neighborhood"]=1
data[data$Neighborhood=="Somerst","Neighborhood"]=1
data[data$Neighborhood=="BrDale","Neighborhood"]=1

data[data$Neighborhood=="BrkSide","Neighborhood"]=2
data[data$Neighborhood=="Crawfor","Neighborhood"]=2
data[data$Neighborhood=="Gilbert","Neighborhood"]=2
data[data$Neighborhood=="OldTown","Neighborhood"]=2

data[data$Neighborhood=="MeadowV","Neighborhood"]=3
data[data$Neighborhood=="Mitchel","Neighborhood"]=3
data[data$Neighborhood=="Timber","Neighborhood"]=3

data[data$Neighborhood=="Sawyer","Neighborhood"]=4
data[data$Neighborhood=="SawyerW","Neighborhood"]=4
data[data$Neighborhood=="ClearCr","Neighborhood"]=4
data[data$Neighborhood=="CollgCr","Neighborhood"]=4
data[data$Neighborhood=="Blueste","Neighborhood"]=4
data[data$Neighborhood=="Edwards","Neighborhood"]=4
data[data$Neighborhood=="IDOTRR","Neighborhood"]=4
data[data$Neighborhood=="SWISU","Neighborhood"]=4

barplot(table(data$Neighborhood))
write.csv(data,'tmp.csv')

data<-read.csv("tmp.csv")
#condition
barplot(table(data$Condition1),main="condition1")
barplot(table(data$Condition2),main="condition2")
data<-subset(data, select=-c(Condition2))
data[data$Condition1=="Norm","Condition1"]=0
data[data$Condition1=="Artery","Condition1"]=1
data[data$Condition1=="Feedr","Condition1"]=1
data[data$Condition1=="PosA","Condition1"]=1
data[data$Condition1=="PosN","Condition1"]=1
data[data$Condition1=="RRAe","Condition1"]=1
data[data$Condition1=="RRAn","Condition1"]=1
data[data$Condition1=="RRNe","Condition1"]=1
data[data$Condition1=="RRNn","Condition1"]=1

#bldgtype
barplot(table(data$BldgType),main="BldgType")
boxplot(raw$GrLivArea~raw$BldgType)
data[data$BldgType=="1Fam","BldgType"]=1
data[data$BldgType=="2fmCon","BldgType"]=1
data[data$BldgType=="Duplex","BldgType"]=2
data[data$BldgType=="Twnhs","BldgType"]=2
data[data$BldgType=="TwnhsE","BldgType"]=2

#housestyle
barplot(table(data$HouseStyle),main='HouseStyle')
data[data$HouseStyle=="1Story","HouseStyle"]=1
data[data$HouseStyle=="1.5Fin","HouseStyle"]=1
data[data$HouseStyle=="1.5Unf","HouseStyle"]=1
data[data$HouseStyle=="2Story","HouseStyle"]=2
data[data$HouseStyle=="2.5Fin","HouseStyle"]=2
data[data$HouseStyle=="2.5Unf","HouseStyle"]=2
data[data$HouseStyle=="SFoyer","HouseStyle"]=3
data[data$HouseStyle=="SLvl","HouseStyle"]=3

#overallqual

#overallcond

#yearbuilt, yearremodadd
plot(data$YearBuilt, data$YearRemodAdd,main='Year')
data$Remod<-ifelse(data$YearBuilt<=1950 & data$YearRemodAdd==1950
                   | data$YearBuilt==data$YearRemodAdd , 0, 1)
data<-subset(data, select=-c(YearBuilt, YearRemodAdd))

#roof
barplot(table(data$RoofStyle),main='RoofStyle')
data[data$RoofStyle=="Gable","RoofStyle"]=0
data[data$RoofStyle=="Flat","RoofStyle"]=1
data[data$RoofStyle=="Gambrel","RoofStyle"]=1
data[data$RoofStyle=="Hip","RoofStyle"]=1
data[data$RoofStyle=="Mansard","RoofStyle"]=1
data[data$RoofStyle=="Shed","RoofStyle"]=1

barplot(table(data$RoofMatl),main='RoofMatl')
data<-subset(data, select=-c(RoofMatl))

#exterior
barplot(table(data$Exterior1st),main="Exterior1st")
barplot(table(data$Exterior2nd),main="Exterior2nd")
ex<-filter(data, data$Exterior1st!=data$Exterior2nd)%>%
  select(Exterior1st, Exterior2nd)
barplot(table(ex$Exterior1st),main="Exterior1st")
barplot(table(ex$Exterior2nd),main="Exterior2nd")

data$Exterior_d<-ifelse(data$Exterior1st==data$Exterior2nd, 0, 1)
colnames(data)[18]<-"Exterior"
data<-subset(data, select=-c(Exterior2nd))
data[data$Exterior=="VinylSd","Exterior"]=1
data[data$Exterior=="HdBoard","Exterior"]=2
data[data$Exterior=="MetalSd","Exterior"]=3
data[data$Exterior=="Wd Sdng","Exterior"]=4

data[data$Exterior=="AsbShng","Exterior"]=5
data[data$Exterior=="AsphShn","Exterior"]=5
data[data$Exterior=="BrkComm","Exterior"]=5
data[data$Exterior=="BrkFace","Exterior"]=5
data[data$Exterior=="CBlock","Exterior"]=5
data[data$Exterior=="CemntBd","Exterior"]=5
data[data$Exterior=="ImStucc","Exterior"]=5
data[data$Exterior=="Plywood","Exterior"]=5
data[data$Exterior=="Stone","Exterior"]=5
data[data$Exterior=="Stucco","Exterior"]=5
data[data$Exterior=="WdShing","Exterior"]=5
data<-subset(data, select=-c(Exterior1st))

write.csv(data,'tmp.csv')

data<-read.csv("tmp.csv")
#masvnr
barplot(table(data$MasVnrType),main="MasVnrType")
hist(data$MasVnrArea)
data$MasVnrType<-ifelse(is.na(data$MasVnrType), 'None',data$MasVnrType)
data[data$MasVnrType=="None","MasVnrType"]=0
data[data$MasVnrType=="BrkFace","MasVnrType"]=1
data[data$MasVnrType=="BrkCmn","MasVnrType"]=1
data[data$MasVnrType=="Stone","MasVnrType"]=1
data$MasVnrArea<-ifelse(is.na(data$MasVnrArea), 0, data$MasVnrArea)

#exter
barplot(table(data$ExterCond),main="ExterCond")
barplot(table(data$ExterQual),main="ExterQual")
data<-subset(data, select=-c(ExterCond))
data[data$ExterQual=="TA","ExterQual"]=0
data[data$ExterQual=="Fa","ExterQual"]=0
data[data$ExterQual=="Ex","ExterQual"]=1
data[data$ExterQual=="Gd","ExterQual"]=1

#foundation
barplot(table(data$Foundation),main="Foundation")
data[data$Foundation=="PConc","Foundation"]=1
data[data$Foundation=="CBlock","Foundation"]=2
data[data$Foundation=="BrkTil","Foundation"]=3
data[data$Foundation=="Slab","Foundation"]=3
data[data$Foundation=="Stone","Foundation"]=3
data[data$Foundation=="Wood","Foundation"]=3

write.csv(data,'tmp.csv')
#bsmt
data<-data[!is.na(data$BsmtQual),]
barplot(table(data$BsmtQual),main="BsmtQual")
data[data$BsmtQual=="TA","BsmtQual"]=0
data[data$BsmtQual=="Fa","BsmtQual"]=0
data[data$BsmtQual=="Ex","BsmtQual"]=1
data[data$BsmtQual=="Gd","BsmtQual"]=1

barplot(table(data$BsmtCond),main="BsmtCond")
data<-subset(data, select=-c(BsmtCond))

data<-data[!is.na(data$BsmtExposure),]
barplot(table(data$BsmtExposure),main="BsmtExposure")
boxplot(data$GrLivArea~data$BsmtExposure)
data[data$BsmtExposure=="No","BsmtExposure"]=0
data[data$BsmtExposure=="Av","BsmtExposure"]=1
data[data$BsmtExposure=="Gd","BsmtExposure"]=1
data[data$BsmtExposure=="Mn","BsmtExposure"]=1

data<-data[!is.na(data$BsmtFinType1),]
data<-data[!is.na(data$BsmtFinType2),]
barplot(table(data$BsmtFinType1),main="BsmtFinType1")
barplot(table(data$BsmtFinType2),main="BsmtFintype2")
data$BsmtFinType_d<-ifelse(data$BsmtFinType2=='Unf',0,1)
data<-subset(data, select=-c(BsmtFinType2))
data[data$BsmtFinType1=="GLQ","BsmtFinType1"]=1
data[data$BsmtFinType1=="ALQ","BsmtFinType1"]=1
data[data$BsmtFinType1=="BLQ","BsmtFinType1"]=1
data[data$BsmtFinType1=="Rec","BsmtFinType1"]=2
data[data$BsmtFinType1=="LwQ","BsmtFinType1"]=0
data[data$BsmtFinType1=="Unf","BsmtFinType1"]=0

data<-subset(data, select=-c(BsmtFinSF1, BsmtFinSF2))

#Heating
barplot(table(data$Heating),main="Heating")
data<-subset(data, select=-c(Heating))

barplot(table(data$HeatingQC),main="HeatingQC")
data[data$HeatingQC=="Po","HeatingQC"]=0
data[data$HeatingQC=="Fa","HeatingQC"]=0
data[data$HeatingQC=="TA","HeatingQC"]=0
data[data$HeatingQC=="Gd","HeatingQC"]=1
data[data$HeatingQC=="Ex","HeatingQC"]=2

#centralair
barplot(table(data$CentralAir),main="CentralAir")
data[data$CentralAir=="Y","CentralAir"]=1
data[data$CentralAir=="N","CentralAir"]=0

#electrical
data<-data[!is.na(data$Electrical),]
barplot(table(data$Electrical),main="Electrical")
data[data$Electrical=="SBrkr","Electrical"]=0
data[data$Electrical=="FuseA","Electrical"]=1
data[data$Electrical=="FuseF","Electrical"]=1
data[data$Electrical=="FuseP","Electrical"]=1
data[data$Electrical=="Mix","Electrical"]=1

#flrsf
data$X1stR<-data$X1stFlrSF/data$GrLivArea
data$X2ndR<-data$X2ndFlrSF/data$GrLivArea
data$LowR<-data$LowQualFinSF/data$GrLivArea
data<-subset(data, select=-c(X1stFlrSF, X2ndFlrSF, LowQualFinSF))

write.csv(data,'tmp.csv')

data<-read.csv('tmp.csv')
#kitchen
barplot(table(data$KitchenQual),main="KitchenQual")
data[data$KitchenQual=="Fa","KitchenQual"]=0
data[data$KitchenQual=="TA","KitchenQual"]=0
data[data$KitchenQual=="Gd","KitchenQual"]=1
data[data$KitchenQual=="Ex","KitchenQual"]=2

#functional
barplot(table(data$Functional),main="Functional")
data<-subset(data, select=-c(Functional))

#fireplace
data<-subset(data, select=-c(FireplaceQu))

#garage
barplot(table(data$GarageType),main="GarageType")
data$GarageType<-ifelse(is.na(data$GarageType), 'Attchd', data$GarageType)
data[data$GarageType=="Attchd","GarageType"]=0
data[data$GarageType=="BuiltIn","GarageType"]=0
data[data$GarageType=="2Types","GarageType"]=1
data[data$GarageType=="Basment","GarageType"]=1
data[data$GarageType=="CarPort","GarageType"]=1
data[data$GarageType=="Detchd","GarageType"]=1

data$GarageYrBlt<-ifelse(is.na(data$GarageYrBlt), raw$YearBuilt, data$GarageYrBlt)

barplot(table(data$GarageFinish),main="GarageFinish")
data$GarageFinish<-ifelse(is.na(data$GarageFinish), 'Unf', data$GarageFinish)
data[data$GarageFinish=="Unf","GarageFinish"]=0
data[data$GarageFinish=="Fin","GarageFinish"]=1
data[data$GarageFinish=="RFn","GarageFinish"]=1


barplot(table(data$GarageQual),main="GarageQual")
data<-subset(data, select=-c(GarageQual))

barplot(table(data$GarageCond),main="GarageCond")
data<-subset(data, select=-c(GarageCond))

#pavedrive
barplot(table(data$PavedDrive),main="PavedDrvie")
data[data$PavedDrive=="Y","PavedDrive"]=1
data[data$PavedDrive=="P","PavedDrive"]=1
data[data$PavedDrive=="N","PavedDrive"]=0

#wooddecksf

#porch
data$PorchSF<-(data$OpenPorchSF+data$EnclosedPorch+data$X3SsnPorch+data$ScreenPorch)/4
data<-subset(data, select=-c(OpenPorchSF, EnclosedPorch, X3SsnPorch,
                             ScreenPorch))

#pool
data<-subset(data, select=-c(PoolQC))

#fence
data<-subset(data, select=-c(Fence))


#misc
data<-subset(data, select=-c(MiscFeature))

#sold
data<-subset(data, select=-c(MoSold, YrSold))

#sale
data<-subset(data, select=-c(SaleType, SaleCondition))

data<-data[,c(-1,-2)]
write.csv(data,'tmp.csv')

#############################################################
library(car)
library(MASS)
data<-read.csv('tmp.csv')

data$MSSubClass<-as.character(data$MSSubClass)
data$MSZoning<-as.character(data$MSZoning)
data$LotConfig<-as.character(data$LotConfig)
data$BldgType<-as.character(data$BldgType)
data$Neighborhood<-as.character(data$Neighborhood)
data$HouseStyle<-as.character(data$HouseStyle)
data$Exterior<-as.character(data$Exterior)
data$Foundation<-as.character(data$Foundation)
data$BsmtFinType1<-as.character(data$BsmtFinType1)
data$HeatingQC<-as.character(data$HeatingQC)
data$KitchenQual<-as.character(data$KitchenQual)

dummy<-dummyVars('~.', data=data)
newdata<-data.frame(predict(dummy, newdata=data))
newdata<-newdata[,-1]

newdata<-subset(newdata, select=-c(MSSubClass4, MSZoning3, LotConfig3,
                                   Neighborhood4, BldgType2, HouseStyle3,
                                   Exterior1, Foundation3, BsmtFinType12,
                                   HeatingQC2, KitchenQual2))


model_1<-lm(GrLivArea~. , data=newdata)
summary(model_1)
vif(model_1)

newdata<-subset(newdata, select=-c(X2ndR))
newdata<-subset(newdata, select=-c(MSSubClass1, MSSubClass2, MSSubClass3))

model_2<-lm(GrLivArea~. , data=newdata)
summary(model_2)
vif(model_2)


model_3<-step(model_2, direction='both')
summary(model_3)
vif(model_3)

plot(model_3,1)
plot(model_3,2)
plot(model_3,3)
plot(model_3,4)

newdata<-newdata[-c(511,1149,1262),]

durbinWatsonTest(residuals(model_3))

par(mfrow=c(2,2))
plot(model_3)


shapiro.test(residuals(model_3))

influencePlot(model_3)

ncvTest(model_3)

newdata<-newdata[-c(511,699,1262,1384),]


newdata<-newdata[-(newdata$GrLivArea>=(1786+1.5*(1786-1138))),]

library(forecast)
accuracy(model_3)

#p-value cut
cutdata<-subset(newdata, select=c(LotFrontage,LotConfig2,LandSlope,
                                  Condition1, HouseStyle1, HouseStyle2,
                                  MasVnrType, MasVnrArea, Foundation1,
                                  Foundation2, BsmtExposure, TotalBsmtSF,
                                  FullBath, HalfBath, BedroomAbvGr, 
                                  KitchenQual0, KitchenQual1, TotRmsAbvGrd,
                                  Fireplaces, GarageYrBlt, GarageCars,
                                  GarageArea, WoodDeckSF, PoolArea, SalePrice,
                                  Remod, Exterior_d, X1stR, PorchSF,GrLivArea))


model_4<-lm(GrLivArea~. , data=cutdata)
summary(model_4)



#elastic
library(glmnet)
library(reshape)
library(ggplot2)

idx<-sample(1:nrow(newdata), replace=F,nrow(newdata)*0.7)
train<-newdata[idx,]
test<-newdata[-idx,]

train.x<-scale(train[,-35])
train.y<-train[,35]

test.x<-scale(test[,-35])
test.y<-scale(test[,35])

elastic_model<-glmnet(train.x, train.y, alpha=0.5)
plot(elastic_model, xvar="lambda")


cv.Elasticnet<-cv.glmnet(train.x, train.y, alpha=0.5)
plot(cv.Elasticnet)



beta<-coef(lasso_model)
tmp<-as.data.frame(as.matrix(beta))
tmp$coef<-row.names(tmp)
tmp<-reshape::melt(tmp, id='coef')
tmp$variable<-log(lasso_model$lambda[tmp$variable+1])
tmp$norm<-apply(abs(beta[-1,]),2,sum)[tmp$variable+1]

ggplot(tmp[tmp$coef!="(Intercept)",], aes(lambda, value, color=coef,
                                          linetype=coef))+
  geom_line(size=1)+
  xlab("Lambda(log scale)")+
  ylab("Coefficients")+
  guides(color=guide_legend(title=""),
         linetype=guide_legend(title=""))+
  theme_bw()+
  theme(legend.key.width = unit(3,"lines"))

###################################################
library(randomForest)

idx<-sample(1:nrow(newdata), nrow(newdata)*0.7)
train<-newdata[idx,]
test<-newdata[-idx,]

rf_model<-randomForest(GrLivArea~ . , data=train)
randomForest::importance(rf_model)
randomForest::varImpPlot(rf_model)



###################################################
library(xgboost)

idx<-sample(1:nrow(newdata), nrow(newdata)*0.7)
train<-newdata[idx,]
test<-newdata[-idx,]

train_x<-data.matrix(train[,-35])
train_y<-train[,35]

test_x<-data.matrix(test[,-35])
test_y<-test[,35]

xgb_train<-xgb.DMatrix(data=train_x, label=train_y)
xgb_test<-xgb.DMatrix(data=test_x, label=test_y)

watchlist<-list(train=xgb_train, test=xgb_test)

model<-xgb.train(data=xgb_train, max.depth=2, watchlist=watchlist,
                 nrounds=200)

model_xgboost<-xgboost(data=xgb_train, max.depth=3, nrounds=86, verbose=0)
summary(model_xgboost)

importance_matrix=xgb.importance(colnames(xgb_train), model=model_xgboost)
importance_matrix
xgb.plot.importance(importance_matrix[1:10,])

pred_y<-predict(model_xgboost, xgb_test)

mean((test_y-pred_y)^2) #MSE
caret::RMSE(test_y, pred_y) #RMSE

x=1:length(test_y)
plot(x, test_y, col='red',type='l')
lines(x, pred_y, col='blue',type='l')
legend(x=1, y=38, legend=c('original test_y', 'predicted test_y'),
       col=c('red','blue'), box.lty=1, cex=0.8, lty=c(1,1))
