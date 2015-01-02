version new
mydata <- read.csv("halbtrockenrasen2.csv", sep=";")

#1
mylogit <-glm(formula = mydata$brimed ~ 1, family=binomial(link='logit'),data=mydata)

mylogit2 <-glm(formula = mydata$brimed ~mydata$N, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')
mylogit2 <-glm(formula = mydata$brimed ~mydata$phosphor, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')
mylogit2 <-glm(formula = mydata$brimed ~mydata$kalium, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')
mylogit2 <-glm(formula = mydata$brimed ~mydata$pH, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')
mylogit2 <-glm(formula = mydata$brimed ~mydata$Corg, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')
mylogit2 <-glm(formula = mydata$brimed ~mydata$nsjahr, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')
mylogit2 <-glm(formula = mydata$brimed ~mydata$nsmarmai, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')
mylogit2 <-glm(formula = mydata$brimed ~mydata$nsjunaug, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')
mylogit2 <-glm(formula = mydata$brimed ~mydata$nsnovfeb, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')
mylogit2 <-glm(formula = mydata$brimed ~mydata$temp, family=binomial(link='logit'),data=mydata)
anova(mylogit,mylogit2,test='Chisq')

#2
mylogit2 <-glm(formula = mydata$brimed ~mydata$phosphor, family=binomial(link='logit'),data=mydata)

mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$N, family=binomial(link='logit'),data=mydata)
anova(mylogit2,mylogit3,test='Chisq')
mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$kalium, family=binomial(link='logit'),data=mydata)
anova(mylogit2,mylogit3,test='Chisq')
mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$pH, family=binomial(link='logit'),data=mydata)
anova(mylogit2,mylogit3,test='Chisq')
mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg, family=binomial(link='logit'),data=mydata)
anova(mylogit2,mylogit3,test='Chisq')
mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$nsjahr, family=binomial(link='logit'),data=mydata)
anova(mylogit2,mylogit3,test='Chisq')
mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$nsmarmai, family=binomial(link='logit'),data=mydata)
anova(mylogit2,mylogit3,test='Chisq')
mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$nsjunaug, family=binomial(link='logit'),data=mydata)
anova(mylogit2,mylogit3,test='Chisq')
mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$nsnovfeb, family=binomial(link='logit'),data=mydata)
anova(mylogit2,mylogit3,test='Chisq')
mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$temp, family=binomial(link='logit'),data=mydata)
anova(mylogit2,mylogit3,test='Chisq')

#3
mylogit3 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg, family=binomial(link='logit'),data=mydata)


mylogit4 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$N, family=binomial(link='logit'),data=mydata)
anova(mylogit3,mylogit4,test='Chisq')
mylogit4 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium, family=binomial(link='logit'),data=mydata)
anova(mylogit3,mylogit4,test='Chisq')
mylogit4 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$pH, family=binomial(link='logit'),data=mydata)
anova(mylogit3,mylogit4,test='Chisq')
mylogit4 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$nsjahr, family=binomial(link='logit'),data=mydata)
anova(mylogit3,mylogit4,test='Chisq')
mylogit4 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$nsmarmai, family=binomial(link='logit'),data=mydata)
anova(mylogit3,mylogit4,test='Chisq')
mylogit4 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$nsjunaug, family=binomial(link='logit'),data=mydata)
anova(mylogit3,mylogit4,test='Chisq')
mylogit4 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$nsnovfeb, family=binomial(link='logit'),data=mydata)
anova(mylogit3,mylogit4,test='Chisq')
mylogit4 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$temp, family=binomial(link='logit'),data=mydata)
anova(mylogit3,mylogit4,test='Chisq')

#4
mylogit4 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium, family=binomial(link='logit'),data=mydata)

mylogit5 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$N, family=binomial(link='logit'),data=mydata)
anova(mylogit4,mylogit5,test='Chisq')
mylogit5 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$pH, family=binomial(link='logit'),data=mydata)
anova(mylogit4,mylogit5,test='Chisq')
mylogit5 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$nsjahr, family=binomial(link='logit'),data=mydata)
anova(mylogit4,mylogit5,test='Chisq')
mylogit5 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$nsmarmai, family=binomial(link='logit'),data=mydata)
anova(mylogit4,mylogit5,test='Chisq')
mylogit5 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$nsjunaug, family=binomial(link='logit'),data=mydata)
anova(mylogit4,mylogit5,test='Chisq')
mylogit5 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$nsnovfeb, family=binomial(link='logit'),data=mydata)
anova(mylogit4,mylogit5,test='Chisq')
mylogit5 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp, family=binomial(link='logit'),data=mydata)
anova(mylogit4,mylogit5,test='Chisq')

#5
mylogit5 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp, family=binomial(link='logit'),data=mydata)

mylogit6 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$N, family=binomial(link='logit'),data=mydata)
anova(mylogit5,mylogit6,test='Chisq')
mylogit6 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$pH, family=binomial(link='logit'),data=mydata)
anova(mylogit5,mylogit6,test='Chisq')
mylogit6 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjahr, family=binomial(link='logit'),data=mydata)
anova(mylogit5,mylogit6,test='Chisq')
mylogit6 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsmarmai, family=binomial(link='logit'),data=mydata)
anova(mylogit5,mylogit6,test='Chisq')
mylogit6 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug, family=binomial(link='logit'),data=mydata)
anova(mylogit5,mylogit6,test='Chisq')
mylogit6 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsnovfeb, family=binomial(link='logit'),data=mydata)
anova(mylogit5,mylogit6,test='Chisq')

#6
mylogit6 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug, family=binomial(link='logit'),data=mydata)

mylogit7 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug+mydata$N, family=binomial(link='logit'),data=mydata)
anova(mylogit6,mylogit7,test='Chisq')
mylogit7 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug+mydata$pH, family=binomial(link='logit'),data=mydata)
anova(mylogit6,mylogit7,test='Chisq')
mylogit7 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug+mydata$nsjahr, family=binomial(link='logit'),data=mydata)
anova(mylogit6,mylogit7,test='Chisq')
mylogit7 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug+mydata$nsmarmai, family=binomial(link='logit'),data=mydata)
anova(mylogit6,mylogit7,test='Chisq')
mylogit7 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug+mydata$nsnovfeb, family=binomial(link='logit'),data=mydata)
anova(mylogit6,mylogit7,test='Chisq')

library('car')
mmps(mylogit6)

mylogit7 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug+mydata$Corg*mydata$kalium, family=binomial(link='logit'),data=mydata)
anova(mylogit6,mylogit7,test='Chisq')

mylogit7 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug+log(mydata$Corg), family=binomial(link='logit'),data=mydata)
anova(mylogit6,mylogit7,test='Chisq')

mmps(mylogit7)

mylogit7 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug+log(mydata$temp), family=binomial(link='logit'),data=mydata)
anova(mylogit6,mylogit7,test='Chisq')

mylogit8 <-glm(formula = mydata$brimed ~mydata$phosphor+mydata$Corg+mydata$kalium+mydata$temp+mydata$nsjunaug+log(mydata$temp)+log(mydata$Corg), family=binomial(link='logit'),data=mydata)
anova(mylogit7,mylogit8,test='Chisq')

mmps(mylogit8)

