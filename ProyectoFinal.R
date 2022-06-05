library(dplyr)
data(algae,package = "DMwR2")
algae

getwd()

algae <- read.table('Analysis.txt',header=FALSE, dec='.',col.names=c('season','size','speed','mxPH','mnO2','Cl','NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4','a5','a6','a7'),na.strings=c('XXXXXXX'))

tibble::as_tibble(algae)

algae
summary(algae$season)

table(algae$season)

library(ggplot2)
ggplot(algae,aes(x=mxPH)) + geom_histogram(aes(y=..density..))

ggplot(algae,aes(x=mxPH)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(color="red") + geom_rug() +
  ggtitle("El histograma de mxPH (maximo pH)") +
  xlab("") + ylab("")

library(car)
qqPlot(algae$mxPH,main='Gráfico QQ normal del pH máximo',ylab="")


 ggplot(algae,aes(x=factor(0),y=oPO4)) +
   geom_boxplot() + geom_rug() +
   geom_hline(aes(yintercept=mean(algae$oPO4, na.rm = TRUE)),
               linetype=2,colour="red") +
   ylab("Ortofosfato (oPO4)") + xlab("") + scale_x_discrete(breaks=NULL)
 
 
  plot(algae$NH4, xlab = "")
  abline(h = mean(algae$NH4, na.rm = T), lty = 1)
  abline(h = mean(algae$NH4, na.rm = T) + sd(algae$NH4, na.rm = T), lty = 2)
  abline(h = median(algae$NH4, na.rm = T), lty = 3)
  identify(algae$NH4) 
 
 
   plot(algae$NH4, xlab = "")
   clickedRows <- identify(algae$NH4)
   algae[clickedRows, ]
 
   library(dplyr)
   filter(algae, NH4 > 19000)
 
  ggplot(algae,aes(x=size,y=a1)) + geom_boxplot() +
    xlab("River Size") + ylab("Algal A1")
 
   library(forcats)
   algae <- mutate(algae,
                     size=fct_relevel(size,c("small","medium","large")),
                     speed=fct_relevel(speed,c("low","medium","high")),
                     season=fct_relevel(season,c("spring","summer","autumn","winter")))
 
   ggplot(algae,aes(x=size,y=a1)) +
      geom_violin() + geom_jitter() + xlab("TAMAÑO DEL RIO") + ylab("Algal A1")
   
   
   data2graph <- filter(algae,!is.na(mnO2)) %>%
      mutate(minO2=cut(mnO2, quantile(mnO2,c(0,0.25,.5,.75,1)), include.lowest=TRUE))
   
   ggplot(data2graph,aes(x=a3,y=season, color=season)) + geom_point() + facet_wrap(~ minO2) + guides(color=FALSE)
   
   
   
library(DMwR2)
library(dplyr)
data(algae) 
algae   

filter(algae, !complete.cases(algae) )   
algae <- na.omit(algae)  

data(algae, package="DMwR2") # only necessary if you executed the above na.omit()
algae <- algae[-c(62, 199), ]
   
apply(algae, 1, function(x) sum(is.na(x)))   
   
data(algae, package="DMwR2")
manyNAs(algae, 0.2)   
   
algae <- algae[-manyNAs(algae), ]   

algae[48, "mxPH"] <- mean(algae$mxPH, na.rm = TRUE)   
algae[is.na(algae$Chla), "Chla"] <- median(algae$Chla, na.rm = TRUE)

data(algae, package="DMwR2")
algae <- algae[-manyNAs(algae), ]
algae <- centralImputation(algae)

cor(algae[, 4:18], use = "complete.obs")
symnum(cor(algae[,4:18],use="complete.obs"))


library(corrplot)
cm <- cor(algae[,4:18], use="complete.obs")
corrplot(cm, type="upper", tl.pos="d")
corrplot(cm, add=TRUE, type="lower", method="number",diag=FALSE, tl.pos="n", cl.pos="n")


data(algae, package="DMwR2")
algae <- algae [-manyNAs(algae),]
lm(PO4 ~ oPO4, data = algae)

algae[28, "PO4"] <- 42.897 + 1.293 * algae[28, "oPO4"]

data(algae, package="DMwR2")
algae <- algae[-manyNAs(algae), ]
fillPO4 <- function(oP) ifelse(is.na(oP),NA,42.897 + 1.293 * oP)
algae[is.na(algae$PO4), "PO4"] <- sapply(algae[is.na(algae$PO4), "oPO4"], fillPO4)

library(ggplot2)
library(forcats)
algae <- mutate(algae,
                  size=fct_relevel(size,c("small","medium","large")),
                  speed=fct_relevel(speed,c("low","medium","high")),
                  season=fct_relevel(season,c("spring","summer","autumn","winter")))
ggplot(algae, aes(x=mxPH)) + geom_histogram(binwidth=0.5) + facet_wrap(~ season)

ggplot(algae, aes(x=mxPH)) + geom_histogram(binwidth=0.5) +
    facet_wrap(size ~ speed)

ggplot(algae, aes(x=mxPH, y=size, color=size)) + geom_point() +
    facet_wrap(~speed) + geom_jitter(height = 0.4)

data(algae, package="DMwR2")
algae <- algae[-manyNAs(algae), ]

algae <- knnImputation(algae, k = 10)
algae <- knnImputation(algae, k = 10, meth = "median")

algae

data(algae, package="DMwR2")
 algae <- algae[-manyNAs(algae), ]
 clean.algae <- knnImputation(algae, k = 10)

 lm.a1 <- lm(a1 ~ ., data = clean.algae[, 1:12])
 summary(lm.a1)
 anova(lm.a1)
 lm2.a1 <- update(lm.a1, . ~ . - season)
 summary(lm2.a1)
 
 anova(lm.a1,lm2.a1)
 
 final.lm <- step(lm.a1)
 
 summary(final.lm)
 plot(final.lm)
 
 
 library(rpart)
 
 data(algae, package="DMwR2")
 
 
 
 algae <- algae[-manyNAs(algae), ]
 rt.a1 <- rpart(a1 ~ ., data = algae[, 1:12])
 
 rt.a1
 
 library(rpart.plot)
  prp(rt.a1,extra=101,box.col="orange",split.box.col="grey")
 
  printcp(rt.a1)
 
  rt2.a1 <- prune(rt.a1, cp = 0.08)
  rt2.a1
  (rt.a1 <- rpartXse(a1 ~ ., data = algae[, 1:12]))
 
  first.tree <- rpart(a1 ~ ., data = algae[, 1:12])
  snip.rpart(first.tree, c(4, 7))
  
  plot(first.tree)
   text(first.tree)
   snip.rpart(first.tree)
  
   lm.predictions.a1 <- predict(final.lm, clean.algae)
   rt.predictions.a1 <- predict(rt.a1, algae)
  
   (mae.a1.lm <- mean(abs(lm.predictions.a1 - algae[["a1"]])))
   (mae.a1.rt <- mean(abs(rt.predictions.a1 - algae[["a1"]])))
   (mse.a1.lm <- mean((lm.predictions.a1 - algae[["a1"]])^2))
   (mse.a1.rt <- mean((rt.predictions.a1 - algae[["a1"]])^2))
  
  
   
    (nmse.a1.lm <- mean((lm.predictions.a1-algae[['a1']])^2)/ mean((mean(algae[['a1']])-algae[['a1']])^2))
   (nmse.a1.rt <- mean((rt.predictions.a1-algae[['a1']])^2)/ mean((mean(algae[['a1']])-algae[['a1']])^2))
  
   
   library(ggplot2)
   dg <- data.frame(lm.a1=lm.predictions.a1,
                     rt.a1=rt.predictions.a1,
                     true.a1=algae[["a1"]])
   
   ggplot(dg,aes(x=lm.a1,y=true.a1)) +
       geom_point() + geom_abline(slope=1,intercept=0,color="red") +
       ggtitle("Modelo lineal")
   
    ggplot(dg,aes(x=rt.a1,y=true.a1)) +
       geom_point() + geom_abline(slope=1,intercept=0,color="red") +
       ggtitle("Árbol de regresión")
   
  
    plot(lm.predictions.a1,algae[['a1']],main="Linear Model",
            xlab="Predictions",ylab="True Values")
    abline(0,1,col="red")
    algae[identify(lm.predictions.a1,algae[['a1']]),] 
   
   
    sensible.lm.predictions.a1 <- ifelse(lm.predictions.a1 < 0, 0, lm.predictions.a1)
     (mae.a1.lm <- mean(abs(lm.predictions.a1 - algae[["a1"]])))
   
    (smae.a1.lm <- mean(abs(sensible.lm.predictions.a1 - algae[["a1"]])))
   
    library(performanceEstimation)
     res <- performanceEstimation(
        PredTask(a1 ~ ., algae[, 1:12], "a1"),
        c(Workflow(learner="lm",pre="knnImp",post="onlyPos"),
            workflowVariants(learner="rpartXse",learner.pars=list(se=c(0,0.5,1)))),
        EstimationTask(metrics="nmse",method=CV(nReps=5,nFolds=10))
        )
   
   summary(res)
   plot(res)
   
   getWorkflow("rpartXse.v1", res)
   
   DSs <- sapply(names(algae)[12:18],
                  function(x,names.attrs) {
                     f <- as.formula(paste(x, "~ ."))
                     PredTask(f, algae[,c(names.attrs,x)], x, copy=TRUE)
                     },
                  names(algae)[1:11])
    res.all <- performanceEstimation(DSs,
       c(Workflow(learner="lm", pre="knnImp", post="onlyPos"),
           workflowVariants(learner="rpartXse", learner.pars=list(se=c(0,0.5,1)))),
       EstimationTask(metrics="nmse" ,method=CV(nReps=5, nFolds=10)))  
   
    plot(res.all)
   
    topPerformers(res.all)  
   
    library(randomForest)
     res.all <- performanceEstimation(DSs,
                                      c(Workflow(learner="lm", pre="knnImp",post="onlyPos"),
                                         workflowVariants(learner="rpartXse",
                                                            learner.pars=list(se=c(0,0.5,1))),
                                         workflowVariants(learner="randomForest", pre="knnImp",
                                                            learner.pars=list(ntree=c(200,500,700)))),
                                       EstimationTask(metrics="nmse",method=CV(nReps=5,nFolds=10)))
     
     
   rankWorkflows(res.all, top=3)
     
   p <- pairedComparisons(res.all,baseline="randomForest.v3")
   p$nmse$F.test
     
   p$nmse$BonferroniDunn.test
     
   CDdiagram.BD(p)  
     
   wfs <- sapply(taskNames(res.all), function(t) topPerformer(res.all,metric="nmse",task=t))
    wfs[["a1"]]
    wfs[["a7"]]
   
 full.test.algae <- cbind(test.algae, algae.sols)

 pts <- array(dim = c(140,7,2),
                    dimnames = list(1:140, paste0("a",1:7), c("trues","preds")))

 for(i in 1:7) { res <- runWorkflow(wfs[[i]],
                             as.formula(paste(names(wfs)[i],"~.")),
                             algae[,c(1:11,11+i)],
                             full.test.algae[,c(1:11,11+i)])
        pts[,i,"trues"] <- res$trues
        pts[,i,"preds"] <- res$preds
        }  
     
 pts[1:3,c("a1","a3"),]   
     
 avg.preds <- apply(algae[,12:18], 2, mean)
  apply((pts[,,"trues"] - pts[,,"preds"])^2, 2 ,sum) /
     apply( (scale(pts[,,"trues"], avg.preds, FALSE))^2, 2, sum)    
     
     
     
                                      