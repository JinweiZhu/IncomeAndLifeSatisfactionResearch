library(readxl)
Gini <- read_excel("Desktop/Honors Tutorial Papers/WIID_Cleaned.xlsx", 
                   range = "B1:O2419", col_types = c("text", 
                                                     "skip", "text", "numeric", "skip", 
                                                     "skip", "skip", "skip", "skip", "skip", 
                                                     "skip", "skip", "skip", "numeric"))
View(Gini)
GiniMod=data.frame(Gini)
countryCode='AFG'

# Function to create new data frame
insertRow <- function(existingDF, newrow, r) {
  existingDF <- rbind(existingDF[1:r,],newrow,existingDF[-(1:r),])
}

for (i in 2:nrow(Gini)){
  if ((Gini[i,2]==Gini[i-1,2])&(Gini[i,3]>Gini[i-1,3]+1)){
      #print(i);
      temp=Gini[i,3]-Gini[i-1,3]-1;
      print(temp);
      for (j in 1:as.integer(temp)){
        newRow=c(Gini[i-1,1],Gini[i-1,2],Gini[i-1,3]+j,(Gini[i-1,4]+Gini[i,4])/2);
        GiniMod=insertRow(GiniMod,newRow,i-1+j);
      }
  }
  
  #print(Gini[i]);
} 
rownames(GiniMod) <- 1:nrow(GiniMod)

#Now we importa Gallup Life satisfaction report
LS=GallupAnalytics_Export_20211205_173627
remove(GallupAnalytics_Export_20211205_173627)

#library(readxl)
library(readxl)
GDP <- read_excel("Desktop/Honors Tutorial Papers/PennWorldTable1981-2019 copy.xlsx", 
                  sheet = "Data", col_types = c("text", 
                                                "text", "skip", "numeric", "numeric", 
                                                "skip", "numeric", "numeric"))
View(GDP)
#Calculate per capita GDP and GDP growth rate
colnames(GDP)[6]<-'GDPPerCapita'
GDP['PerCapitaGDPGrowthRate']<-0
for (i in 2:nrow(GDP)){
  if ((!is.null(GDP[i,6]))&(!is.null(GDP[i-1,6]))&(GDP[i,2]==GDP[i-1,2])){
    GDP[i,7]=(GDP[i,6]-GDP[i-1,6])/GDP[i-1,6];
  }
}
GDPAndGini=merge(GDP, GiniMod,by.x=c("country","year"),by.y=c("country","year"))

#Here we run the regression of LSnow
GDPGiniAndLS=merge(GDPAndGini,LS, by.x=c("country","year"),by.y=c("Geography","Time"))

unique(GDPGiniAndLS$country)

Regression1=lm(LSNow~factor(CountryFixed)+factor(YearFixed)+log(GDP)+ChangeInGDP+Gini,data=GDPGiniAndLS)
summary(timeFixedRegression)
anova(Regression1)
#eliminate the error: Error in plot.new() : figure margins too large
par("mar")
par(mar=c(1,1,1,1))
par(mar = c(5,5,5,5))
#Ploting all the avPlot to see if all the variables are necessary.
#In this case, avPlot shows that GDP growth is not necessary.
car::avPlot(
  model = Regression1,
  variable ="log(GDP)"
)
car::avPlot(
  model = Regression1,
  variable ="ChangeInGDP"
)
car::avPlot(
  model = Regression1,
  variable ="Gini"
)

#Here we import the LSin5Years data and run the regression
library(readxl)
LSin5Years <- read_excel("Desktop/Honors Tutorial Papers/LSin5Years.xlsx", sheet = "Life in Five Years  ")
View(LSin5Years)
GDPGiniAndLSin5=merge(GDPAndGini,LSin5Years, by.x=c("country","year"),by.y=c("Geography","Time"))
unique(GDPGiniAndLSin5$country)



colnames(GDPGiniAndLSin5)[1]="CountryFixed"
colnames(GDPGiniAndLSin5)[6]="GDP"
colnames(GDPGiniAndLSin5)[7]="ChangeInGDP"
colnames(GDPGiniAndLSin5)[10]="Gini"
colnames(GDPGiniAndLSin5)[2]="YearFixed"
colnames(GDPGiniAndLSin5)[11]="Aggregate"
colnames(GDPGiniAndLSin5)[13]="LS5Years"
Regression2=lm(LS5Years~factor(CountryFixed)+factor(YearFixed)+log(GDP)+ChangeInGDP+Gini,data=GDPGiniAndLSin5)
summary(Regression2)

car::avPlot(
  model = Regression2,
  variable ="log(GDP)"
)
car::avPlot(
  model = Regression2,
  variable ="ChangeInGDP"
)
car::avPlot(
  model = Regression2,
  variable ="Gini"
)


#Now, let the dependent variable be the difference between LS5years and LSnow
GDPGiniAndLSDif=merge(GDPGiniAndLS,GDPGiniAndLSin5, by.x=c("CountryFixed","YearFixed"),by.y=c("CountryFixed","YearFixed"))
Regression3=lm((LS5Years-LSNow)~factor(CountryFixed)+factor(YearFixed)+log(GDP)+ChangeInGDP+Gini,data=GDPGiniAndLSDif)
summary(Regression3)

car::avPlot(
  model = Regression3,
  variable ="log(GDP)"
)
car::avPlot(
  model = Regression3,
  variable ="ChangeInGDP"
)
car::avPlot(
  model = Regression3,
  variable ="Gini"
)
rm(temp)

stargazer(Regression1,Regression2, Regression3, 
          title="Regression 1,2,3 (Country and year fixed effects)",
          notes.label="Significant levels",
          omit = c("CountryFixed","YearFixed"),
          type="html",
          out="Desktop/regression.htm"
          )

logTimeFixedRegression=lm(log(Value)~factor(country)+factor(year)+log(GDPPerCapita)+GDPGrowth+log(gini_std),data=GDPGiniAndLS)
summary(logTimeFixedRegression)


#Regression 4 quintile LS now.
WIID_quintileIncome=data.frame(matrix(ncol = 5, nrow = 0))
colnames(WIID_quintileIncome) = c('CountryFixed', 'YearFixed', 'IncomeQuintileFixed','AbsoluteIncome','Gini')
for (i in 1:nrow(WIID_decileIncome)){
  if (!is.na(WIID_decileIncome[i,4])){
    WIID_quintileIncome[nrow(WIID_quintileIncome)+1,]=c(WIID_decileIncome[i,1],WIID_decileIncome[i,2],"Poorest 20%",(WIID_decileIncome[i,4]+WIID_decileIncome[i,5])/2,WIID_decileIncome[i,3])
    WIID_quintileIncome[nrow(WIID_quintileIncome)+1,]=c(WIID_decileIncome[i,1],WIID_decileIncome[i,2],"Second 20%",(WIID_decileIncome[i,6]+WIID_decileIncome[i,7])/2,WIID_decileIncome[i,3])
    WIID_quintileIncome[nrow(WIID_quintileIncome)+1,]=c(WIID_decileIncome[i,1],WIID_decileIncome[i,2],"Middle 20%",(WIID_decileIncome[i,8]+WIID_decileIncome[i,9])/2,WIID_decileIncome[i,3])
    WIID_quintileIncome[nrow(WIID_quintileIncome)+1,]=c(WIID_decileIncome[i,1],WIID_decileIncome[i,2],"Fourth 20%",(WIID_decileIncome[i,10]+WIID_decileIncome[i,11])/2,WIID_decileIncome[i,3])
    WIID_quintileIncome[nrow(WIID_quintileIncome)+1,]=c(WIID_decileIncome[i,1],WIID_decileIncome[i,2],"Richest 20%",(WIID_decileIncome[i,12]+WIID_decileIncome[i,13])/2,WIID_decileIncome[i,3])
  }
}
#Script to revise the country name.
tempData=WIID_quintileIncome
tempData$CountryFixed[tempData$CountryFixed=="Congo, Republic of the"]="Congo"
tempData$CountryFixed[tempData$CountryFixed=="Czechia"]="Czech Republic"
tempData$CountryFixed[tempData$CountryFixed=="United Kingdom"]="United Kingdom of Great Britain and Northern Ireland"
tempData$CountryFixed[tempData$CountryFixed=="Moldova"]="Moldova, Republic of"
tempData$CountryFixed[tempData$CountryFixed=="Gambia, The"]="Gambia"
tempData$CountryFixed[tempData$CountryFixed=="Korea, Republic of"]="South Korea"
tempData$CountryFixed[tempData$CountryFixed=="West Bank and Gaza"]="Palestinian Territories"
tempData$CountryFixed[tempData$CountryFixed=="Russia"]="Russian Federation"
tempData$CountryFixed[tempData$CountryFixed=="West Bank and Gaza"]="Palestinian Territories"
#"Serbia and Montenegro"!= Serbia or Montenegro, so there is not much I can do there.
tempData$CountryFixed[tempData$CountryFixed=="United States"]="United States of America"

WIID_quintileIncome=tempData


QuintileIncomeGiniAndLS=merge(WIID_quintileIncome,QuintileLS, by.x=c("CountryFixed","YearFixed","IncomeQuintileFixed"),by.y=c("CountryFixed","YearFixed","IncomeQuintileFixed"))
QuintileIncomeGiniAndLS=rename(QuintileIncomeGiniAndLS, QuintileLSNow=Value)
Regression4=lm(QuintileLSNow~factor(CountryFixed)+factor(YearFixed)+factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+log(AbsoluteIncome)+Gini,data=QuintileIncomeGiniAndLS)
summary(Regression4)


car::avPlot(
  model = Regression4,
  variable ="log(AbsoluteIncome)"
)

car::avPlot(
  model = Regression4,
  variable ="Gini"
)


#Regression 5 quintile LS in 5 years.
colnames(QuintileLSin5) = c('CountryFixed', 'YearFixed', 'IncomeQuintileFixed','QuintileLS5Years')

QuintileIncomeGiniAndLSin5=merge(WIID_quintileIncome,QuintileLSin5, by.x=c("CountryFixed","YearFixed","IncomeQuintileFixed"),by.y=c("CountryFixed","YearFixed","IncomeQuintileFixed"))
Regression5=lm(QuintileLS5Years~factor(CountryFixed)+factor(YearFixed)+factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+log(AbsoluteIncome)+Gini,data=QuintileIncomeGiniAndLSin5)
summary(Regression5)


car::avPlot(
  model = Regression5,
  variable ="log(AbsoluteIncome)"
)

car::avPlot(
  model = Regression5,
  variable ="factor(CountryFixed)"
)

car::avPlot(
  model = Regression5,
  variable ="Gini"
)

#Regression 6 quintile LS difference.
QuintileIncomeGiniAndLSdif=merge(QuintileIncomeGiniAndLSin5,QuintileIncomeGiniAndLS, by.x=c("CountryFixed","YearFixed","IncomeQuintileFixed"),by.y=c("CountryFixed","YearFixed","IncomeQuintileFixed"))
QuintileIncomeGiniAndLSdif=rename(QuintileIncomeGiniAndLSdif, AbsoluteIncome=AbsoluteIncome.x)
QuintileIncomeGiniAndLSdif=rename(QuintileIncomeGiniAndLSdif, Gini=Gini.x)
Regression6=lm(QuintileLS5Years-QuintileLSNow~factor(CountryFixed)+factor(YearFixed)+factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+log(AbsoluteIncome)+Gini,data=QuintileIncomeGiniAndLSdif)
summary(Regression6)

car::avPlot(
  model = Regression6,
  variable ="log(AbsoluteIncome)"
)

car::avPlot(
  model = Regression6,
  variable ="Gini"
)

stargazer(Regression4,Regression5, Regression6, 
          title="Regression 4,5,6 (Country and year and income quintile fixed effects)",
          notes.label="Significant levels",
          omit = c("CountryFixed","YearFixed"),
          covariate.labels=c("Second 20%","Middle 20%","Fourth 20%","Richest 20%"),
          type="html",
          out="Desktop/regression.htm"
)

#Regression 7,8,9 are regression 5,6,7 without country fix effect.
QuintileIncomeGiniAndLS=QuintileIncomeGiniAndLS[-c(2710,2715),]
Regression7=lm(QuintileLSNow~factor(YearFixed)+factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+log(AbsoluteIncome)+Gini,data=QuintileIncomeGiniAndLS)
summary(Regression7)

car::avPlot(
  model = Regression7,
  variable ="log(AbsoluteIncome)"
)

car::avPlot(
  model = Regression7,
  variable ="Gini"
)
QuintileIncomeGiniAndLSin5=QuintileIncomeGiniAndLSin5[-c(6,9,10,2695,2690),]

Regression8=lm(QuintileLS5Years~factor(YearFixed)+factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+log(AbsoluteIncome)+Gini,data=QuintileIncomeGiniAndLSin5)
summary(Regression8)

car::avPlot(
  model = Regression8,
  variable ="log(AbsoluteIncome)"
)

car::avPlot(
  model = Regression8,
  variable ="Gini"
)

QuintileIncomeGiniAndLSdif=QuintileIncomeGiniAndLSdif[-c(2690,2695),]
Regression9=lm(QuintileLS5Years-QuintileLSNow~factor(YearFixed)+factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+log(AbsoluteIncome)+Gini,data=QuintileIncomeGiniAndLSdif)
summary(Regression9)
car::avPlot(
  model = Regression9,
  variable ="log(AbsoluteIncome)"
)

car::avPlot(
  model = Regression9,
  variable ="Gini"
)

stargazer(Regression7,Regression8, Regression9, 
          title="Regression 7,8,9 (year and income quintile fixed effects, no country fix effect)",
          notes.label="Significant levels",
          omit = c("CountryFixed","YearFixed"),
          covariate.labels=c("Second 20%","Middle 20%","Fourth 20%","Richest 20%"),
          type="html",
          out="Desktop/regression.htm"
)

Regression7noGini=lm(QuintileLSNow~factor(YearFixed)+factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+log(AbsoluteIncome),data=QuintileIncomeGiniAndLS)

# 
# 
# Regression10=plm(QuintileLSNow~factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))
#                  +log(AbsoluteIncome)+Gini+YearFixed, model="between", index="CountryFixed", data=QuintileIncomeGiniAndLS)
# 
# 
# #the rest is normal, as suggested by Prof.
# Regression11=plm(QuintileLS5Years~factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))
#                  +log(AbsoluteIncome)+Gini+YearFixed,model="between", index="CountryFixed", data=QuintileIncomeGiniAndLSin5)
# Regression12=plm(QuintileLS5Years-QuintileLSNow~factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))
#                  +log(AbsoluteIncome)+Gini+YearFixed,model="between", index="CountryFixed", data=QuintileIncomeGiniAndLSdif)
# 
# 
# stargazer(Regression10,Regression11, Regression12, 
#           title="Time Between Effects not controlling different countries",
#           notes.label="Significant levels",
#           #omit = c("CountryFixed","YearFixed"),
#           covariate.labels=c("Second 20%","Middle 20%","Fourth 20%","Richest 20%"),
#           type="html",
#           out="Desktop/regression.htm"
# )


#colnumber refers to the colnumber of the index you want to take yearly average over.
getAvg=function(datafrom,datato,colnumber,logarithmCol){

  for (i in 1:nrow(datafrom)){
      if (is.na(paste0(datafrom[i,1],datafrom[i,3]))){
        next
      }
      #match year and income quintile
      j=match(paste0(datafrom[i,1],datafrom[i,3]),
              datato[,6],nomatch=NULL)
      #print(paste0(datafrom[i,1],datafrom[i,3]))
      #print(paste(datato[,1],datato[,2]))
      #if the row is present.
      if (!is.na(j)){
        if (logarithmCol){
          datato[j,3]=datato[j,3]+log(datafrom[i,colnumber])
        }
        else {
          datato[j,3]=datato[j,3]+datafrom[i,colnumber]
        }
        datato[j,4]=datato[j,4]+1
      }
      #If the row is not present.
      else{
        datato[nrow(datato)+1,1]=datafrom[i,1]
        datato[nrow(datato),2]=datafrom[i,3]
        if (logarithmCol){
          datato[nrow(datato),3]=log(datafrom[i,colnumber])
        }
        else{
          datato[nrow(datato),3]=datafrom[i,colnumber]
        }
        datato[nrow(datato),4]=1
        datato[nrow(datato),6]=paste0(datafrom[i,1],datafrom[i,3])
      }
  }
  for (i in 1:nrow(datafrom)){
    datato[i,5]=datato[i,3]/datato[i,4]
  }
  datato=datato%>% drop_na()
  return(datato)
}

QuintileIncomeAvg=data.frame(matrix(ncol = 6, nrow = 0))
colnames(QuintileIncomeAvg) = c('CountryFixed', 'IncomeQuintileFixed','QuintileIncomeLogSum','QuintileIncomeCount','QuintileIncomeLogAvg','CountryConcatIncome')
QuintileIncomeAvg=getAvg(QuintileIncomeGiniAndLS,QuintileIncomeAvg,4,TRUE)

#Do the same for LSAvg
QuintileLSAvg=data.frame(matrix(ncol = 6, nrow = 0))
colnames(QuintileLSAvg) = c('CountryFixed', 'IncomeQuintileFixed','QuintileLSSum','QuintileLSCount','QuintileLSAvg','CountryConcatIncome')
QuintileLSAvg=getAvg(QuintileIncomeGiniAndLS,QuintileLSAvg,7,FALSE)
#Merge the tables

QuintileIncomeAndLSAvg=merge(QuintileLSAvg,QuintileIncomeAvg, by.x=c("CountryFixed", "IncomeQuintileFixed"),by.y=c("CountryFixed","IncomeQuintileFixed"))

Regression10=lm(QuintileLSAvg~factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+QuintileIncomeLogAvg,data=QuintileIncomeAndLSAvg)

car::avPlot(
  model = Regression10,
  variable ="QuintileIncomeLogAvg"
)


#Now repeat the above regression for LS and income in 5 years.

QuintileIncomeAvgin5=data.frame(matrix(ncol = 6, nrow = 0))
colnames(QuintileIncomeAvgin5) = c('CountryFixed', 'IncomeQuintileFixed','QuintileIncomeLogSum','QuintileIncomeCount','QuintileIncomeLogAvg','CountryConcatIncome')
QuintileIncomeAvgin5=getAvg(QuintileIncomeGiniAndLSin5,QuintileIncomeAvgin5,4,TRUE)

QuintileLSAvgin5=data.frame(matrix(ncol = 6, nrow = 0))
colnames(QuintileLSAvgin5) = c('CountryFixed', 'IncomeQuintileFixed','QuintileLSSum','QuintileLSCount','QuintileLSAvg','CountryConcatIncome')
QuintileLSAvgin5=getAvg(QuintileIncomeGiniAndLSin5,QuintileLSAvgin5,6,FALSE)

QuintileIncomeAndLSAvgin5=merge(QuintileLSAvgin5,QuintileIncomeAvgin5, by.x=c("CountryFixed", "IncomeQuintileFixed"),by.y=c("CountryFixed","IncomeQuintileFixed"))
QuintileIncomeAndLSAvgin5=rename(QuintileIncomeAndLSAvgin5,QuintileLSAvgin5=QuintileLSAvg)
Regression11=lm(QuintileLSAvgin5~factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+QuintileIncomeLogAvg,data=QuintileIncomeAndLSAvgin5)

car::avPlot(
  model = Regression11,
  variable ="QuintileIncomeLogAvg"
)

#Now repeat the above regressions with the dif data.
QuintileIncomeAndLSAvgdif=merge(QuintileIncomeAndLSAvgin5,QuintileIncomeAndLSAvg, by.x=c("CountryFixed", "IncomeQuintileFixed"),by.y=c("CountryFixed","IncomeQuintileFixed"))
QuintileIncomeAndLSAvgdif=rename(QuintileIncomeAndLSAvgdif,QuintileIncomeLogAvg=QuintileIncomeLogAvg.x)
#QuintileIncomeAndLSAvgdif=rename(QuintileIncomeAndLSAvgdif,QuintileLSAvgin5=QuintileLSAvg.x)
#QuintileIncomeAndLSAvgdif=rename(QuintileIncomeAndLSAvgdif,QuintileLSAvgNow=QuintileLSAvg.y)
Regression12=lm(QuintileLSAvgin5-QuintileLSAvgNow~factor(IncomeQuintileFixed,levels=c("Poorest 20%","Second 20%","Middle 20%", "Fourth 20%","Richest 20%"))+QuintileIncomeLogAvg,data=QuintileIncomeAndLSAvgdif)

car::avPlot(
  model = Regression12,
  variable ="QuintileIncomeLogAvg"
)

stargazer(Regression10,Regression11, Regression12, 
          title="Regression 10,11,12 (Country Quintile Between effects model over the years)",
          notes.label="Significant levels",
          #omit = c("CountryFixed","YearFixed"),
          covariate.labels=c("Second 20%","Middle 20%","Fourth 20%","Richest 20%"),
          type="html",
          out="Desktop/regression.htm"
)
