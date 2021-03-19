library(manipulate)
library(psych)
library(cowplot)
library(devtools)
library(nlme)
library(jsonlite)
library(tidyverse)
library(table1)
library(kableExtra)
library(knitr)
library(measurements)
library(digest)
library(tinytex)
library(dplyr)
library(tidyr)
library(car)
library(MASS)
library(caret)
library(ggplot2);
library(glmnet)
library(psych)
library(corrplot)
library(QuantPsyc)
library(leaps)
library(lm.beta)
library(ggpubr)
library("readxl")
library(darksky)
library(imager)
library(cowplot)
library(profvis)
library(gridExtra)
library(CCA)
library(yacca)

source("C:\\Users\\G_MAN\\OneDrive\\DePaul\\Advanced Data Anlaysis\\Functions\\PCA_Plot.R")
###################################################################
# This is a nice function for computing the Wilks lambdas for 
# CCA data from the CCA library's method
# It computes the wilkes lambas the degrees of freedom and te 
# p-values
###################################################################

printModel = function(fName,folder,type,table) {
  test <- table
  
  if (type == "Linear") {
    
    path <- paste("./",folder,"/",fName,".png",sep = "")
    
    
    res <- summary(test$residuals)
    res <- as.matrix(t(res))
    rownames(res) <- "Residuals"
    adR2 <- as.data.frame(test$adj.r.squared)
    rownames(adR2) <- "Adjusted R^2 %"
    colnames(adR2) <- "Estimated"
    fstat <- as.data.frame(t(test$fstatistic))
    fstat$pvalue <- pf(fstat[,1], fstat[,2], fstat[,3], lower.tail = FALSE)
    rownames(fstat) <- "F statistic"
    colnames(fstat) <- c("F-value","From Df","To Df","P-value")
    rows <-nrow(res) + nrow(test$coefficients) + nrow(adR2) + nrow(fstat)
    png(path,height = (rows * 45), width = 800)
    grid.arrange(
      tableGrob(round(res,4)),
      tableGrob(round(test$coefficients,4)),
      tableGrob(round(adR2,4)),
      tableGrob(round(fstat,4)),
      nrow=4
    )
    dev.off()
  }
  if (type == "Regular") {
    path <- paste(folder,fName,".png",sep = "")
    rows <-nrow(test)
    colnames(test) <- c("Estimated Beta Weights")
    
    png(path)
    grid.table(round(test,4))
    dev.off()
  }
  path
}

colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, 
            x)
  } else x
}

pOutput = function(plot,w=480,h=480,vName,output,folder,subfolder=NULL){
  
  folder = paste("./",folder,sep = "")
  subfolder = paste(folder,"/",subfolder,sep = "")
  path = paste(subfolder,"/",sep = "")
  
  if(dir.exists(folder)==F){dir.create(folder)}
  if(dir.exists(subfolder)==F){dir.create(subfolder)}
  if(dir.exists(path)==F){dir.create(path)}
  
  
  if(output=="png") {
    vName = paste(path,vName,".png",sep = "")
    png(filename=vName,width = w, height = h,
        units = "px", pointsize = 12, bg = "white", res = NA,
        restoreConsole = TRUE) 
    plot
    dev.off()
    
  }
  vName
}

SummaryPlots = function(df,z=0,folder,subfolder,response=NULL,predictors=NULL,transform=TRUE) {
  
  cont <- paste(folder,"/",subfolder,"/","Continuous",sep = "") 
  cat <- paste(folder,"/",subfolder,"/","Categorical",sep = "")
  
    
  pathCreator(path = cont)
  pathCreator(path = cat)

  cont <- paste(cont,"/",sep = "") 
  cat <- paste(cat,"/",sep = "")
  
  if (is.null(response) == FALSE) {
      df <- df[,c(predictors,response)]
  }
  
  categorical <- select_if(df, is.factor)                                         # Seperate Categorical 
  continuous <- select_if(df,is.numeric)                                          # Seperate Continuous
  
  plots_categorical <- vector('list', ncol(categorical))                          # Create List to store Categorical plots
  plots_continuous <- vector('list', ncol(continuous))                            # Create list to store Numerical plots
  
  plots_responseVSpredictors <- vector('list', ncol(df[predictors]))
  
  if(z==1){ 
    continuous <- continuous %>% 
      mutate_if(is.numeric,funs(as.numeric(scale(.))))
  }
 
if (is.null(response) == FALSE) {
  for (i in seq_along(df[predictors])){
    
    x <- df[predictors[i]]
    y <- df[response]
    names(x) <- c("x")
    names(y) <- c("y")
    
    if (is.numeric(x$x)==TRUE) {
    vName1 = paste(cont,colnames(df[predictors])[i],"_vs Response.png",sep = "") # into plots_categorical
      
    }
    if (is.factor(x$x)==TRUE) {
      vName1 = paste(cat,colnames(df[predictors])[i],"_vs Response.png",sep = "") # into plots_categorical
      
    }    # Loop through every categorical element
    plots_responseVSpredictors[[i]] <- local({                                             # create a histogram and store it 
      i <- i
      x <- df[predictors[i]]
      y <- df[response]
      names(x) <- c("x")
      names(y) <- c("y")
 
     if (is.numeric(x$x)==TRUE) {
        p <- ggplot(df, aes(x=x$x,y=y$y)) + geom_point() + theme(axis.text.x = element_text(angle = 45)) +   ggtitle(paste(colnames(df[predictors[i]])," vs ", colnames(df[response]), sep = "")) + ylab(colnames(df[response])) + xlab(colnames(df[predictors[i]]))                        
        
      } else {
        p <- ggplot(df, aes(x=x$x,y=y$y)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45)) +   ggtitle(paste(colnames(df[predictors[i]])," vs ", colnames(df[response]), sep = "")) + ylab(colnames(df[response])) + xlab(colnames(df[predictors[i]]))                        
        
      }    # Loop through every categorical element
      
      
    })                                                                            #
    plots_responseVSpredictors[[i]]
    ggsave(vName1)
  }
}
  
  for (i in seq_along(categorical)){
    vName1 = paste(cat,colnames(categorical)[i],".png",sep = "") # into plots_categorical
                                                                                  # Loop through every categorical element
    plots_categorical[[i]] <- local({                                             # create a histogram and store it 
      i <- i
      
      p <- ggplot(categorical,aes(x=categorical[[i]])) + geom_bar() +               # 
        theme(axis.text.x = element_text(angle = 45)) +                          #
        ggtitle(colnames(categorical)[i]) + xlab("")                                #

          })                                                                            #
    plots_categorical[[i]]
    ggsave(vName1)
  }
  
  
  for (i in seq_along(continuous)){                                                # Loop through every numerical element 
    vName1 = paste(cont,colnames(continuous)[i],".png",sep = "")
    plots_continuous[[i]] <- local({                                               # and create a histogram.
      i <- i
      #
      if (transform == TRUE) {
        p <- ggplot(continuous,aes(x=continuous[[i]])) + geom_histogram() +         # Also create a histogram based on the
          theme(axis.text.x = element_text(angle = 45)) + xlab("Org")# +            # logged values
        # Also create a histogram based on the sqrt                                                                              # values
        lg <- ggplot(continuous,aes(x=log(continuous[[i]]+1))) +                    #
          geom_histogram() + theme(axis.text.x = element_text(angle = 45)) +       # Create a new plot that contains all three 
          xlab("Logged") #  +                                                      # histograms side by side for easy 
        # comparisons
        sq <- ggplot(continuous,aes(x=(sqrt(continuous[[i]]+1)))) +                 #
          geom_histogram() + theme(axis.text.x = element_text(angle = 90)) +       #
          xlab("Sqrt")  #  +                                                       #
        #
        f <- plot_grid(p,lg,sq,align = "hv",nrow = 2, ncol = 2)                                #
        #
        annotate_figure(f, top = text_grob(colnames(continuous)[i],                 #
                                           face = "bold", size = 14))
      } 
      else {
        p <- ggplot(continuous,aes(x=continuous[[i]])) + geom_histogram() +         # Also create a histogram based on the
          theme(axis.text.x = element_text(angle = 45)) + xlab("")# +              # logged values

        annotate_figure(p, top = text_grob(colnames(continuous)[i],                 #
                                           face = "bold", size = 14))
        
      }
      
    })
    plots_continuous[[i]]
    ggsave(vName1)
  }
  
  
  sTable <- summary(df)

  names(plots_categorical) <- colnames(categorical)
  names(plots_continuous) <- colnames(continuous)
  
  if (is.null(response) == FALSE) {
  names(plots_responseVSpredictors) <- colnames(df[predictors])
  }
  
  all_plots <- list(plots_categorical, plots_continuous,plots_responseVSpredictors,sTable)
  
  names(all_plots) <- c("Categorical","Continuous","Predictor Vs Response","Summary Table")
  all_plots  
}

SummaryCorrelations = function(df,strong,name="none",corMethods = c("pearson","spearman","kendall"),z=0,folder,subfolder=NULL) {
  continuous <- select_if(df,is.numeric)                                           # Seperate Continuous
  
  path <- list()
  Correlations <- list()

  pathCreator(path = paste(folder,subfolder,"Correlations","Pearson",sep = "/"))
  pathCreator(path = paste(folder,subfolder,"Correlations","Spearman",sep = "/"))
  pathCreator(path = paste(folder,subfolder,"Correlations","Kendall",sep = "/"))
  
  path[[1]] <- paste(".",folder,subfolder,"Correlations","Pearson",sep = "/") 
  path[[2]] <- paste(".",folder,subfolder,"Correlations","Spearman",sep = "/") 
  path[[3]] <- paste(".",folder,subfolder,"Correlations","Kendall",sep = "/") 
  
  
  if (name == "none"){name <- deparse(substitute(df))}
  v1 <- vector()                                                                    # Initialize vector v1

  if(z==1){ 
    continuous <- continuous %>% 
      mutate_if(is.numeric,funs(as.numeric(scale(.))))
  }
  
for (method in seq(1,length(corMethods),1)) {
  
  cor.data = cor(continuous,method = corMethods[[method]])                         # Store all correlations into cor.data
  
  
  for (i in 1:nrow(cor.data)){                                                      # Loop through every correlation combination and 
    correlations <-  which((abs(cor.data[i,]) > strong) &                           # look for strong correlations that have an
                             (abs(cor.data[i,]) != 1))                             # absolute value between 0.65 and 1. 
    if(length(correlations)> 0){                                                   
      v1 <-  unique(c(v1,i,unname(correlations)))                                  # Store unique columns that contain the strong correlation
    }                                                                              # into vector v1,
  }                                                                                 #
    
  vplot = "Cor.Matrix"
  vName1 = paste(path[method],"/",vplot,"_",name,".png",sep = "")
  png(filename=vName1,width = 1000, height = 1000,
      units = "px", pointsize = 12, bg = "white", res = NA,
      restoreConsole = TRUE) 
  pairs.panels(continuous[,c(v1)])                                                  # Create a Plot, Histograms and Correlation                                                                                                       # Matrix 
  # Based on the strong correlations found                                                                                                          # from prior loop
  dev.off()
  
  vplot = "Cor.Color_all"
  vName2 = paste(path[method],"/",vplot,"_",name,".png",sep = "")
  png(filename=vName2,width = 1000, height = 1000,
      units = "px", pointsize = 12, bg = "white", res = NA,
      restoreConsole = TRUE) 
  corrplot(cor.data, 
           method = "ellipse",
           type = 'upper',
           diag = TRUE,tl.col = 'black',tl.offset = .1,
           title = 'All variables')
  dev.off()
  
  vplot = "Cor.Color_all_ellipseAOE"
  vName3 = paste(path[method],"/",vplot,"_",name,".png",sep = "")
 
  png(filename=vName3,width = 1000, height = 1000,
      units = "px", pointsize = 12, bg = "white", res = NA,
      restoreConsole = TRUE) 
  corrplot(cor.data, 
           method = "ellipse",
           type = 'upper',
           diag = TRUE, 
           order = "AOE",
           title = 'All variables - AOE')
  dev.off()

  vplot = "Cor.COlor_strong"
  vName4= paste(path[method],"/",vplot,"_",name,".png",sep = "")
  
  png(filename=vName4) 
  corrplot(cor.data[c(v1),c(v1)], 
           method = "ellipse",
           type = 'upper',
           diag = TRUE, 
           title = 'Strong Variables')
  
  
  dev.off()
  
  vplot = "Cor.COlor_strong_ellipseAOE"
  vName5= paste(path[method],"/",vplot,"_",name,".png",sep = "")

    png(filename=vName5) 
    corrplot(cor.data[c(v1),c(v1)], 
             method = "ellipse",
             type = 'upper',
             diag = TRUE, 
             order = "AOE",
             title = 'Strong variables - AOE')
    dev.off()
  
  corPlots <- list()
  corPlots[[1]] <- ggdraw() + draw_image(vName1)
  corPlots[[2]] <- ggdraw() + draw_image(vName2)
  corPlots[[3]] <- ggdraw() + draw_image(vName3)
  corPlots[[4]] <- ggdraw() + draw_image(vName4)
  corPlots[[5]] <- ggdraw() + draw_image(vName5)
  
  names(corPlots) <- c(
    "Cor.Matrix",
    "Cor.Color_all",
    "Cor.Color_all_ellipseAOE",
    "Cor.COlor_strong",
    "Cor.COlor_strong_ellipseAOE"
  )
  
  corList <- list("Plots" = corPlots,"StrongCorr"= as.vector(colnames(continuous[v1])))
  Correlations[[method]] <- corList
}

  names(Correlations) <- corMethods
  vplot = "Comparisons"
  vName5= paste(".",folder,subfolder,"correlations",paste(vplot,"_",name,".png",sep=""),sep = "/")
  png(filename=vName5,width = 1000,height = 1000) 
  print(
  plot_grid(
    Correlations$pearson$Plots$Cor.Color_all_ellipseAOE + 
      draw_label("Pearson",fontface = 'bold',x = 0,hjust = -.6,vjust = -12),
    
    Correlations$spearman$Plots$Cor.Color_all_ellipseAOE+ 
      draw_label("Spearman",fontface = 'bold',x = 0,hjust = -.6,vjust = -12),
    
    Correlations$kendall$Plots$Cor.Color_all_ellipseAOE+ 
      draw_label("Kendall",fontface = 'bold',x = 0,hjust = -.6,vjust = -12),
    align = "h",nrow = 1,ncol = 3
  ))
  
   dev.off()
 
   
   vplot = "Comparisons_Strong"
   vName5= paste(".",folder,subfolder,"correlations",paste(vplot,"_",name,".png",sep=""),sep = "/")
   png(filename=vName5,width = 1000,height = 1000) 
   print(
     plot_grid(
       Correlations$pearson$Plots$Cor.COlor_strong_ellipseAOE + 
         draw_label("Pearson",fontface = 'bold',x = 0,hjust = -.6,vjust = -11),
       
       Correlations$spearman$Plots$Cor.COlor_strong_ellipseAOE+ 
         draw_label("Spearman",fontface = 'bold',x = 0,hjust = -.6,vjust = -11),
       
       Correlations$kendall$Plots$Cor.COlor_strong_ellipseAOE+ 
         draw_label("Kendall",fontface = 'bold',x = 0,hjust = -.6,vjust = -11),
       align = "h",nrow = 1,ncol = 3
     ))
   
   dev.off()
   
  Correlations[[4]] <- ggdraw() + draw_image(vName5)
  Correlations[[5]] <- ggdraw() + draw_image(vName5)
  
  names(Correlations) <- c('pearson','spearman','kendall',"Comparison - All","Comparison - Strong")
  Correlations
}

summaryTable = function(df,split=NULL,fontSize=12,label=NULL) {
  df <- df %>% mutate_if(is.numeric, round, 2)
  
  if(is.null(label)==T){label="5-Number Summary"}else{label=label}
  
  total <- data.frame()
  continuous <- select_if(df,is.numeric)                                            # Seperate Continuous
  c <- as.data.frame(summary(continuous))
  names <- colnames(continuous)
  for(i in seq(1, length(names), 1)) {
    to = 6 * i
    fr = to - 5
    if(i == 1) {
      b <- separate(c[fr:to,],3,c("Statistic",names[i]),sep=":")
      total <- b[,3:4]
    }  else {
      b <- separate(c[fr:to,],3,c("Statistic",names[i]),sep=":")
      total <- merge(total, b[,3:4], by="Statistic")}
  }

  if(is.null(split)==T){

    table1 <- kable(total, booktabs = T,label = label) 
    table1 <- kable_styling(kable_input = table1,position = "center",latex_options = "HOLD_position")
    table1 <- row_spec(table1,0:6,align = "c",font_size = fontSize) 
    
    } else {
  table1 <- kable(total[,c(1,split)], booktabs = T,label = label)
  table1 <- kable_styling(kable_input = table1,position = "center",latex_options = "HOLD_position")
  table1 <- row_spec(table1,0:6,align = "c",font_size = fontSize)
  }
  table1
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

CorTest = function(df,lmin,lmax,cf,z=0,folder,subfolder=NULL){
  
  df <- select_if(df,is.numeric)                                            # Seperate Continuous
  if(z==1){ 
    df <- df %>% 
      mutate_if(is.numeric,funs(as.numeric(scale(.))))
  }
  
  dfCorrTest = corr.test(df,adjust="none")
  pvalue = dfCorrTest$p
  ptest = ifelse(pvalue <(1-cf),T,F)
  testSum <- colSums(ptest)-1
  testSum <- as.data.frame(testSum)
  
  vUnder <- which(testSum/(nrow(testSum)-1) < lmin)
  vOver <-  which(testSum/(nrow(testSum)-1) > lmax)
  vUnder <- as.vector(vUnder)
  vOver <- as.vector(vOver)
  vName <- paste("Alpha = ",(1-cf)," Correlation Thresholds",sep = "")
  
  p1 <- pOutput(
    plot=
      barplot(rev(testSum[order(testSum$testSum),]/(nrow(testSum)-1)),
              main= vName,
              beside = T, horiz = F,names.arg=rownames(testSum),las=1)+abline(h=lmax, col="red")+abline(h=lmin, col="blue"),  
    vName=vName,
    output = "png",
    folder = folder,
    subfolder = subfolder
  )
  p1 <- ggdraw() + draw_image(p1)
  exclude <- as.vector(rbind(vOver,vUnder))
  excludeNames <- colnames(df[,c(exclude)])
  
  results <- list(excludeNames,exclude,p1)
  
  names(results) <- c("Over|Under Names","Over|Under Vector","Barplot")
  results
  }

ccaWilks = function(set1, set2, cca) {
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}

pathCreator = function(path) {
  build <- strsplit(path,"/")
  build <- unlist(build)
  
  for ( i in seq(1,length(build),1)){
    if (i == 1) {
      vname <- paste(build[i],"/",sep = "")
      folder = paste("./",vname,sep = "")
    } else {
      vname <- paste(vname,build[i],"/",sep = "")
      folder = paste("./",vname,sep = "")
      
    }
    if(dir.exists(folder)==F){dir.create(folder)}
  }
}

PC_Analysis = function(df,exclude=NULL,factors=NULL,type="pearson",folder,PCAName){
  
  subfolder1 = "PrComp"
  subfolder2 = "Principal"
  
  folder1 = paste("./",folder,sep = "")
  subfolder1 = paste(folder,"/",subfolder1,sep = "")
  subfolder2 = paste(folder,"/",subfolder2,sep = "")
  
 
  path1 = paste(subfolder1,"/",sep = "")
  path2 = paste(subfolder2,"/",sep = "")
  
  pathCreator(path1)
  pathCreator(path2)
  

  if (is.null(exclude)==FALSE){
    pca_data <- dplyr::select_if(df,is.numeric)
    pca_data <- dplyr::select(pca_data, -exclude)
    
  }   else {
    pca_data <- dplyr::select_if(df,is.numeric)
  }
  
  nFactors               <- ncol(pca_data)
  
  if(type=="spearman") {
    pca_scale <- cor(pca_data,method = "spearman")
  } 
  if(type=="pearson") {
    pca_scale <- cor(pca_data,method = "pearson")
  } 
  if(type=="kendall") {
    pca_scale <- cor(pca_data,method = "kendall")
  } 
  
  if(is.null(factors)==TRUE){factors=nFactors} else {factors = factors}
  if(is.null(factors)==TRUE){ncols=nFactors} else {ncols = factors}
  
  
  
  # PrComp Variables and Summaries
  pca1_notScaled         <- prcomp(pca_data,scale. = F,rank. = factors)
  pca1_Scaled            <- prcomp(pca_scale,scale. = T,rank. = factors)
  
  
  s1 <- pOutput(
    output = "png",
    plot = plot(pca1_notScaled,main="Unscaled"),
    vName =  paste(PCAName," - Unrotated - Unscaled",sep = ""),
    folder = folder,subfolder = "PrComp")
  
  s2 <- pOutput(
    output = "png",
    plot = plot(pca1_Scaled,main="Scaled") + abline(h=1,col="red"),
    vName =  paste(PCAName," - Unrotated - Scaled",sep = ""),
    folder = folder,subfolder = "PrComp")
  
  s3 <- plot_grid(
    ggdraw() + draw_label("Scree Plots - Unscaled vs Scaled", fontface = 'bold',x = 0,hjust = 0),
    plot_grid(
      ggdraw() + draw_image(s1),
      ggdraw() + draw_image(s2),
      align = "h",nrow = 1,ncol = 2),
    align = "v",ncol = 1,rel_heights = c(.05,1))
 
  
  
  # Principal Variables
  pca2_notScaled         <- principal(pca_data,rotate = "none",cor = "cov",nfactors = factors )
  pca2_Scaled            <- principal(pca_scale,rotate = "none",nfactors = factors )
  pca3_notScaled         <- principal(pca_data,rotate = "varimax",cor = "cov",nfactors = factors )
  pca3_Scaled            <- principal(pca_scale,rotate = "varimax",nfactors = factors )
  

  
  principle1.cv.t1 <- kable(round(pca2_notScaled$Vaccounted,2)[1:3,],booktabs=T, caption = "PCA Importance Table Unrotated - Unscaled (Co-variance)",linesep = "\\addlinespace" ) %>% column_spec(1, width = "1in") %>%  column_spec(c(1:ncols+1), width = ".3in") %>% column_spec(2:(factors),color = "black", background = "#5bc0de",bold=T) %>% row_spec(c(1:2),color = "black", background = "white") %>%  column_spec(c(1,(factors):ncols),color = "black", background = "white",bold = TRUE) %>% kable_styling(latex_options = "hold_position")
  principle1.cr.t2 <- kable(round(pca2_Scaled$Vaccounted,2)[1:3,],booktabs=T, caption = "PCA Importance Table Unrotated - Scaled (Correlation)",linesep = "\\addlinespace" ) %>% column_spec(c(1), width = "1in") %>%  column_spec(c(1:ncols+1), width = ".3in") %>% column_spec(2:(factors),color = "black", background = "#5bc0de",bold=T) %>% row_spec(c(1:2),color = "black", background = "white") %>% column_spec(c(1,(factors):ncols),color = "black", background = "white",bold = TRUE) %>% kable_styling(latex_options = "hold_position")
  principle1.cv.t3 <- kable(round(t(pca2_notScaled$loadings[c(1:ncols),c(1:factors)]),2),booktabs=T, caption = "PCA No Rotation Formulas - Unscaled (Co-variance)",linesep = "\\addlinespace" ) %>% column_spec(c(1), width = ".5in") %>%  column_spec(c(1:ncols+1), width = ".4in") %>% kable_styling(latex_options = "hold_position")
  principle1.cr.t4 <- kable(round(t(pca2_Scaled$loadings[c(1:ncols),c(1:factors)]),2),booktabs=T, caption = "PCA No Rotation Formulas - Scaled (Correlation)",linesep = "\\addlinespace" ) %>% column_spec(c(1), width = ".5in") %>%  column_spec(c(1:ncols+1), width = ".4in") %>% kable_styling(latex_options = "hold_position")

  principle2.cv.t1 <- kable(round(pca3_notScaled$Vaccounted,2)[1:3,],booktabs=T, caption = "PCA Importance Table Rotated - Unscaled (Co-variance)",linesep = "\\addlinespace" ) %>% column_spec(1, width = "1in") %>%  column_spec(c(1:ncols+1), width = ".3in") %>% column_spec(2:(factors),color = "black", background = "#5bc0de",bold=T) %>% row_spec(c(1:2),color = "black", background = "white") %>%  column_spec(c(1,(factors):ncols),color = "black", background = "white",bold = TRUE) %>% kable_styling(latex_options = "hold_position")
  principle2.cr.t2 <- kable(round(pca3_Scaled$Vaccounted,2)[1:3,],booktabs=T, caption = "PCA Importance Table Rotated - Scaled (Correlation)",linesep = "\\addlinespace" ) %>% column_spec(c(1), width = "1in") %>%  column_spec(c(+1), width = ".3in") %>% column_spec(2:(factors),color = "black", background = "#5bc0de",bold=T) %>% row_spec(c(1:2),color = "black", background = "white") %>% column_spec(c(1,(factors):ncols),color = "black", background = "white",bold = TRUE) %>% kable_styling(latex_options = "hold_position")
  principle2.cv.t3 <- kable(round(t(pca3_notScaled$loadings[c(1:ncols),c(1:factors)]),2),booktabs=T, caption = "PCA Rotation Formulas - Unscaled (Co-variance)",linesep = "\\addlinespace" ) %>% column_spec(c(1), width = ".5in") %>%  column_spec(c(1:ncols+1), width = ".4in") %>% kable_styling(latex_options = "hold_position")
  principle2.cr.t4 <- kable(round(t(pca3_Scaled$loadings[c(1:ncols),c(1:factors)]),2),booktabs=T, caption = "PCA Rotation Formulas - Scaled (Correlation)",linesep = "\\addlinespace" ) %>% column_spec(c(1), width = ".5in") %>%  column_spec(c(1:ncols+1), width = ".4in") %>% kable_styling(latex_options = "hold_position")
  
  PCAs = list()
  
  PCAs[[1]] <-  pca1_notScaled
  PCAs[[2]] <-  pca1_Scaled
  PCAs[[3]] <-  pca2_notScaled
  PCAs[[4]] <-  pca2_Scaled
  PCAs[[5]] <-  pca3_notScaled
  PCAs[[6]] <-  pca3_Scaled
  
  names(PCAs) <- c(
    "PrComp-notScaled",
    "PrComp-Scaled",
    "Principal Unrotated - Not Scaled",
    "Principal Unrotated - Scaled",
    "Principal Rotated - Scaled",
    "Principal Rotated - Not Scaled"
  )
  
  latexTables = list()
  
  latexTables[1] <- principle1.cv.t1
  latexTables[2] <- principle1.cr.t2
  latexTables[3] <- principle1.cv.t3
  latexTables[4] <- principle1.cr.t4
  
  latexTables[5] <- principle2.cv.t1
  latexTables[6] <- principle2.cr.t2
  latexTables[7] <- principle2.cv.t3
  latexTables[8] <- principle2.cr.t4
  
  names(latexTables) <- c(
    "PCA Importance Table Unrotated - Unscaled (Co-variance)",
    "PCA Importance Table Unrotated - Scaled (Correlation)",
    "PCA No Rotation Formulas - Unscaled (Co-variance)",
    "PCA No Rotation Formulas - Scaled (Correlation)",
    "PCA Importance Table Rotated - Unscaled (Co-variance)",
    "PCA Importance Table Rotated - Scaled (Correlation)",
    "PCA Rotation Formulas - Unscaled (Co-variance)",
    "PCA Rotation Formulas - Scaled (Correlation)"
  )                          
  
  plots = list()
  plots[[1]]  <- s3
  plots[[2]]  <- PCA_Plot(pca1_notScaled)
  plots[[3]]  <- PCA_Plot_Secondary(pca1_notScaled)
  plots[[4]]  <- PCA_Plot(pca1_Scaled)
  plots[[5]]  <- PCA_Plot_Secondary(pca1_Scaled)
  
  plots[[6]] <- PCA_Plot_Psyc_R(pca3_notScaled)
  plots[[7]] <- PCA_Plot_Psyc_Secondary_R(pca3_notScaled)
  plots[[8]] <- PCA_Plot_Psyc_R(pca3_Scaled)
  plots[[9]] <- PCA_Plot_Psyc_Secondary_R(pca3_Scaled)
  
  plots[[10]] <- PCA_Plot_Psyc(pca2_notScaled)
  plots[[11]] <- PCA_Plot_Psyc_Secondary(pca2_notScaled)
  plots[[12]] <- PCA_Plot_Psyc(pca2_Scaled)
  plots[[13]] <- PCA_Plot_Psyc_Secondary(pca2_Scaled)
  
  
    plotNames <- c(
    "Scree plot Scaled vs Unscaled",
    "PCA Plot Unscaled - P1 and P2",
    "PCA Plot Unscaled - P3 and P4",
    "PCA Plot Scaled - P1 and P2",
    "PCA Plot Scaled - P3 and P4", 
    "PCA Plot Unscaled and Rotated - P1 and P2",
    "PCA Plot Unscaled and Rotated - P3 and P4",
    "PCA Plot Scaled and Rotated - P1 and P2",
    "PCA Plot Scaled and Rotated - P3 and P4",
    
    "PCA Plot Unscaled and Unrotated - P1 and P2",
    "PCA Plot Unscaled and Unrotated - P3 and P4",
    "PCA Plot Scaled and Unrotated - P1 and P2",
    "PCA Plot Scaled and Unrotated - P3 and P4"
    
    
  )  
   
  names(plots) <- plotNames

  for ( i in seq(1,5,1)){
    
    png(filename=paste(path1,PCAName," - ",plotNames[i],".png",sep=""),width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE) 
    print(plots[[i]])
    dev.off()
  }
  
  for ( i in seq(6,13,1)){
    
    png(filename=paste(path2,PCAName," - ",plotNames[i],".png",sep=""),width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE) 
    print(plots[[i]])
    dev.off()
  }
  
  
  results <- list(PCAs,latexTables,plots)
  
  names(results) <- c(
    "PCA results",
    "Latex Tables",
    "Plots"
  )
  results
  
}

Regularization = function(df,response,predictors,folder,subfolder){

 methods <- list()
 partition <- sample(2,nrow(df), replace=TRUE, prob = c(0.80, 0.20))
 train <- df[partition== 1, ]
 test <- df[partition==2 , ]
# 
#  train <- df
#  test <- df
 
  lambdas = c("lambda.min","lambda.1se")
  results <- list()
 
  folders <- list()
  folderNames <- c(
    "Min.Lasso.Residuals",
    "Min.Lasso.ModelPlots",
    "Min.Ridge.Residuals",
    "Min.Ridge.ModelPlots",
    "Min.net.Residuals",
    "Min.net.ModelPlots",
    "Min.best.Residuals",
    "Min.best.ModelPlots",
    "1se.Lasso.Residuals",
    "1se.Lasso.ModelPlots",
    "1se.Ridge.Residuals",
    "1se.Ridge.ModelPlots",
    "1se.net.Residuals",
    "1se.net.ModelPlots",
    "1se.best.Residuals",
    "1se.best.ModelPlots"
    
  )
  
  folders[[1]] <- paste(folder,"/",subfolder,"/","Regularization","/","lambda.min","/","Lasso","/","Residuals",sep = "")
  folders[[2]] <- paste(folder,"/",subfolder,"/","Regularization","/","lambda.min","/","Lasso","/","Model Plots",sep = "")
  
  folders[[3]] <- paste(folder,"/",subfolder,"/","Regularization","/","lambda.min","/","Ridge","/","Residuals",sep = "")
  folders[[4]] <- paste(folder,"/",subfolder,"/","Regularization","/","lambda.min","/","Ridge","/","Model Plots",sep = "")
  
  folders[[5]] <- paste(folder,"/",subfolder,"/","Regularization","/","lambda.min","/","Net","/","Residuals",sep = "")
  folders[[6]] <- paste(folder,"/",subfolder,"/","Regularization","/","lambda.min","/","Net","/","Model Plots",sep = "")
  
  folders[[7]] <- paste(folder,"/",subfolder,"/","Regularization","/","lambda.min","/","Best","/","Residuals",sep = "")
  folders[[8]] <- paste(folder,"/",subfolder,"/","Regularization","/","lambda.min","/","Best","/","Model Plots",sep = "")
  
  folders[[9]] <- paste(folder,"/",subfolder,"/","Regularization","/","Lambda.1se","/","Lasso","/","Residuals",sep = "")
  folders[[10]] <- paste(folder,"/",subfolder,"/","Regularization","/","Lambda.1se","/","Lasso","/","Model Plots",sep = "")
  
  folders[[11]] <- paste(folder,"/",subfolder,"/","Regularization","/","Lambda.1se","/","Ridge","/","Residuals",sep = "")
  folders[[12]] <- paste(folder,"/",subfolder,"/","Regularization","/","Lambda.1se","/","Ridge","/","Model Plots",sep = "")
  
  folders[[13]] <- paste(folder,"/",subfolder,"/","Regularization","/","Lambda.1se","/","Net","/","Residuals",sep = "")
  folders[[14]] <- paste(folder,"/",subfolder,"/","Regularization","/","Lambda.1se","/","Net","/","Model Plots",sep = "")
  
  folders[[15]] <- paste(folder,"/",subfolder,"/","Regularization","/","Lambda.1se","/","Best","/","Residuals",sep = "")
  folders[[16]] <- paste(folder,"/",subfolder,"/","Regularization","/","Lambda.1se","/","Best","/","Model Plots",sep = "")
  
  for(i in seq(1,length(folders),1)) {
    pathCreator(folders[[i]])
  }
 
  x = 1
  # Train GLM models
  for (lambda in lambdas){
  net_models <-  list()
  net_train  <-   data.frame(alpha=integer(),RMSETrain = integer(),R2Train = integer())
  net_test   <-   data.frame(alpha=integer(),RMSETest = integer(),R2Test = integer())
  
  i = 1
  x_train <- data.matrix(train[,c(predictors)])
  y_train <- data.matrix(train[,c(response)])
  
  alpha = seq(0,1,.05)
  modelName = c()
  for (a in alpha){
    set.seed(123)  
    modelName <- c(modelName,paste("Alpha = ",a,sep = "")) 
    net         <- cv.glmnet(x_train,y_train,family="gaussian",alpha=a)
    net_models[[i]] <- net
    residuals_net_train <- y_train - predict(net,x_train,s=lambda,data=data)
    rmse_net_train <- sqrt(mean(residuals_net_train^2))
    
    sst_net_train <- sum((y_train - mean(y_train))^2)
    sse_net_train <- sum((residuals_net_train)^2)
    r2_net_train  <- 1 - sse_net_train / sst_net_train
    
    net_train     <- rbind(net_train, 
                           data.frame(alpha=a,RMSETrain = rmse_net_train,R2Train = r2_net_train)
    )
    i <- i + 1
  }
  names(net_models) <- modelName
  results[[1]] <- net_models
  
  x_test = data.matrix(test[,c(predictors)])
  y_test = data.matrix(test[,c(response)])
  
  x_gen <- data.matrix(df[,c(predictors)])
  y_gen <- data.matrix(df[,c(response)])
  
  
  i = 1
  
  for (n in net_models){
    set.seed(123)
    residuals_net_test = y_test - predict(n,x_test,s=lambda,data=data)
    rmse_net_test = sqrt(mean(residuals_net_test^2))
    sst_net_test <- sum((y_test - mean(y_test))^2)
    sse_net_test <- sum((residuals_net_test)^2)
    r2_net_test <- 1 - sse_net_test / sst_net_test
    net_test     <- rbind(net_test,
                          data.frame(alpha=alpha[i],RMSETest = rmse_net_test,R2Test = r2_net_test)
    )
    i = i + 1
  }
  
  reg_results <- merge(net_test,net_train)
  results[[2]] <- reg_results
  
  
  best_stats <- reg_results[ which(reg_results[c("RMSETest")] == min(reg_results[,c("RMSETest")])),]
  results[[3]] <- best_stats
  best_model <- net_models[c(paste("Alpha = ",best_stats[1,1],sep = ""))]
  results[[4]] <- best_model
  
  modelgraphs <- list()
  modelgraphs[[1]] <- net_models[[1]]
  modelgraphs[[2]] <- net_models[[11]]
  modelgraphs[[3]] <- net_models[[21]]
  modelgraphs[[4]] <- best_model[[1]]
  names(modelgraphs) <- c("Ridge","Net","Lasso","Best")
  for (modelgraph in seq(1,length(modelgraphs),1)) {
  modelResults <- list()
  
  model <- modelgraphs[[modelgraph]]
  prediction_gen <- predict(model,x_gen,s=lambda,data=data)
  residuals_gen <- y_gen - prediction_gen
  res <- df[,c(response)]
  res_name <- colnames(res)
  colnames(res) <- c("y")
  modelPlots <- list()
  

  folder1 = paste(folder,"/",subfolder,"/","Regularization","/",lambda,"/",names(modelgraphs[modelgraph]),"/","Residuals",sep = "")
  vName1 = paste("Response - ",res_name," vs Residuals",sep = "")
  w = 800
  h = 800
  path = paste("./",folder1,"/",sep = "")
  vName1 = paste(path,vName1,".png",sep = "")
  
  png(filename=vName1,width = w, height = h,units = "px",pointsize = 12, bg = "white", 
      res = NA, restoreConsole = TRUE)
  
  print(plot(res$y,scale(residuals_gen,scale = T,center = T),
             main = paste(res_name," vs Residuals",sep = ""),
             xlab= res_name,ylab="Standardized Residuals"))
  dev.off()
  
  s1 <- vName1

  folder1 = paste(folder,"/",subfolder,"/","Regularization","/",lambda,"/",names(modelgraphs[modelgraph]),"/","Model Plots",sep = "")
  vName2 = "Glmnet.fit"
  w = 800
  h = 800
  path = paste("./",folder1,"/",sep = "")
  vName2 = paste(path,vName2,".png",sep = "")
  
  png(filename=vName2,width = w, height = h,units = "px",pointsize = 12, bg = "white", 
      res = NA, restoreConsole = TRUE)
  
  print(plot(model$glmnet.fit))
  dev.off()
  s2 <- vName2
  
  
  folder1 = paste(folder,"/",subfolder,"/","Regularization","/",lambda,"/",names(modelgraphs[modelgraph]),"/","Model Plots",sep = "")
  vName3 = "Model"
  w = 800
  h = 800
  path = paste("./",folder1,"/",sep = "")
  vName3 = paste(path,vName3,".png",sep = "")
  
  png(filename=vName3,width = w, height = h,units = "px",pointsize = 12, bg = "white", 
      res = NA, restoreConsole = TRUE)
  
  print(plot(model))
  dev.off()
  s3 <- vName3
  
  
  folder1 = paste(folder,"/",subfolder,"/","Regularization","/",lambda,"/",names(modelgraphs[modelgraph]),"/","Model Plots",sep = "")
  vName4 = "Residual Histogram"
  w = 800
  h = 800
  path = paste("./",folder1,"/",sep = "")
  vName4 = paste(path,vName4,".png",sep = "")
  
  png(filename=vName4,width = w, height = h,units = "px",pointsize = 12, bg = "white", 
      res = NA, restoreConsole = TRUE)
  
  print(histogram(scale(residuals_gen,center=TRUE,scale=TRUE),
                  main="Residual Histograms",xlab="Standardized Residuals"))
  dev.off()
  
  s4 <- vName4
  
  folder1 = paste(folder,"/",subfolder,"/","Regularization","/",lambda,"/",names(modelgraphs[modelgraph]),"/","Model Plots",sep = "")
  vName5 = "Fitted Values vs Standardized Residuals"
  w = 800
  h = 800
  path = paste("./",folder1,"/",sep = "")
  vName5 = paste(path,vName5,".png",sep = "")
  
  png(filename=vName5,width = w, height = h,units = "px",pointsize = 12, bg = "white", 
      res = NA, restoreConsole = TRUE)
  
  print(plot(prediction_gen,scale(residuals_gen,center=TRUE,scale=TRUE),
                  main="Fitted Values vs Standardized Residuals",xlab = "Fitted Values",ylab="Standardized Residuals"))
  dev.off()
  
  s5 <- vName5
  

  modelPlots[[1]] <- ggdraw() + draw_image(s1)
  modelPlots[[2]] <- ggdraw() + draw_image(s2)
  modelPlots[[3]] <- ggdraw() + draw_image(s3)
  modelPlots[[4]] <- ggdraw() + draw_image(s4)
  modelPlots[[5]] <- ggdraw() + draw_image(s5)
  modelPlots[[6]] <- scale(residuals_gen,center=TRUE,scale=TRUE)
  modelPlots[[7]] <- prediction_gen
  
  names(modelPlots) <- c("Response vs Residuals","GlmFit","Model Plot","Standardized Residuals Histgram","Fitted Values vs Standardized Residuals","Standardized Residuals","Fitted.Values")
  modelResults[[1]] <- modelPlots 
   
  residuals_x <- list()
  
  for (i in seq(1,ncol(df[,c(predictors)]),1)) {
    p <- df[,c(predictors)][i]
    n <- colnames(p)
    colnames(p) <- c("x")
    
   
    folder1 = paste(folder,"/",subfolder,"/","Regularization","/",lambda,"/",names(modelgraphs[modelgraph]),"/","Residuals",sep = "")
    w = 800
    h = 800
    path = paste("./",folder1,"/",sep = "")
    vName4 = n
    vName4 = paste(path,vName4,".png",sep = "")
    
    png(filename=vName4,width = w, height = h,units = "px",pointsize = 12, bg = "white", 
        res = NA, restoreConsole = TRUE)
    
    print(plot(p$x,scale(residuals_gen,scale = T,center = T),
               main = paste(n," vs Residuals",sep = ""),
               xlab= n,ylab="Standardized Residuals"))
    dev.off()
    s4 <- vName4
     residuals_x[[i]] <- ggdraw() + draw_image(s4)
  }
  
  names(residuals_x) <- colnames(df[,c(predictors)])
  modelResults[[2]] <- residuals_x 
  
  if (x == 1){
    result1 <- results
    modelResults[[4]] <- coef(model,s=model$lambda.min)
    
    folder1 = paste(folder,"/",subfolder,"/","Regularization","/",lambda,"/",names(modelgraphs[modelgraph]),"/","Model Plots",sep = "")
    path1 = paste("./",folder1,"/",sep = "")

    sum <- printModel(
      fName = "Model Summary",
      folder = path1,
      type = "Regular",
      table = coef(model,s=model$lambda.min)
    )

    modelResults[[5]] <- ggdraw() + draw_image(sum)  
  
  } else {
    result2 <- results
    modelResults[[4]] <- coef(model,s=model$lambda.1se)

    folder1 = paste(folder,"/",subfolder,"/","Regularization","/",lambda,"/",names(modelgraphs[modelgraph]),"/","Model Plots",sep = "")
    path1 = paste("./",folder1,"/",sep = "")
    
    sum <- printModel(
      fName = "Model Summary",
      folder = path1,
      type = "Regular",
      table = coef(model,s=model$lambda.1se)
    )
    modelResults[[5]] <- ggdraw() + draw_image(sum)
  }
  modelResults[[6]] <- model
  
  
  
  names(modelResults) <- c("Model Plots","Residual Plots","Coefficients","Model","Model Summary")
  
  methods[[x]] <- modelResults
  
  x = x + 1
  }
  
  
  }
  resultNames <- c("All Models","Train vs Test RMSE Results","Best Model RMSE Results","Best Model")
  methodNames <- c("Ridge","Net","Lasso","Best","Results")
  names(result1) <- resultNames 
  names(result2) <- resultNames 
  method1 <- methods[1:4]
  method2 <- methods[5:8]

  method1[[5]] <- result1
  method2[[5]] <- result2
  
  names(method1) <- methodNames
  names(method2) <- methodNames
  
  m1 <- result1[[3]]
  m1 <- m1[[1]]
  
  m2 <- result2[[3]]
  m2 <- m2[[1]]
  
  
  
  methods <- list()
  methods[[1]] <- method1
  methods[[2]] <- method2
  
  if(m1<m2){
    
      m1 <- result1[[4]]
      methods[[3]] <- m1[[1]]
      names(methods) <- c(lambdas,paste("Lambda Min Best Model at alpha = ",result1[[3]]$alpha,sep = ""))  
  } else {
    
    m2 <- result2[[4]]
    methods[[3]] <- m2[[1]]
    names(methods) <- c(lambdas,paste("Lambda 1SE Best Model at alpha = ",result2[[3]]$alpha,sep = ""))
  }
    
    methods
}

LinearModels = function(df,predictors,response,folder,subfolder) {
  set.seed(123)
  results <- list()
  models <- list()
  modelResults <- list()
  modelNames <- c("Linear","Backward","Forward")  
  f <- as.formula(paste(response, paste(predictors, collapse = " + "), sep = " ~ "))
  n <- as.formula(paste(response, "1", sep = " ~ "))
  
  partition <- sample(2,nrow(df), replace=TRUE, prob = c(0.80, 0.20))
  train <- df[partition== 1, ]
  test <- df[partition==2 , ]
  
  df1 = df
  df2 = df
  
  subName = subfolder
  subfolder2 = "Linear Models"
  subName2 = subfolder2
  subfolder3 = "Linear"
  subName3 = subfolder3
  subfolder4 = "Backward"
  subName4 = subfolder4
  subfolder5 = "Forward"
  subName5 = subfolder5
  
  subfolder6 = "Residuals"
  subfolder7 = "Model Plots"
  
  folder = paste("./",folder,sep = "")
  subfolder = paste(folder,"/",subfolder,sep = "")
  subfolder2 = paste(subfolder,"/",subfolder2,sep = "")
  subfolder3 = paste(subfolder2,"/",subfolder3,sep = "")
  subfolder4 = paste(subfolder2,"/",subfolder4,sep = "")
  subfolder5 = paste(subfolder2,"/",subfolder5,sep = "")
  
  path = paste(subfolder4,"/",sep = "")
  path1 = paste(path,subfolder6,sep = "")
  path2 = paste(path,subfolder7,sep = "")
  path3 = paste(subfolder3,"/",sep = "")
  path4 = paste(path3,subfolder6,sep = "")
  path5 = paste(path3,subfolder7,sep = "")
  path6 = paste(subfolder5,"/",sep = "")
  path7 = paste(path6,subfolder6,sep = "")
  path8 = paste(path6,subfolder7,sep = "")
  
  if(dir.exists(folder)==F){dir.create(folder)}
  if(dir.exists(subfolder)==F){dir.create(subfolder)}
  if(dir.exists(subfolder2)==F){dir.create(subfolder2)}
  if(dir.exists(path)==F){dir.create(path)}
  if(dir.exists(path1)==F){dir.create(path1)}
  if(dir.exists(path2)==F){dir.create(path2)}
  if(dir.exists(path3)==F){dir.create(path3)}
  if(dir.exists(path4)==F){dir.create(path4)}
  if(dir.exists(path5)==F){dir.create(path5)}
  if(dir.exists(path6)==F){dir.create(path6)}
  if(dir.exists(path7)==F){dir.create(path7)}
  if(dir.exists(path8)==F){dir.create(path8)}
  
  
  # Train LM models
  
  lm_train <- lm(f, data = df1)
  lm_null  <- lm(n, data = df1)
  lm_step     <- step(lm_train, scope = list(lower=lm_null, upper=lm_train),direction="backward",trace=FALSE)
  lm_stepfwd  <- step(lm_null, scope = list(lower=lm_null, upper=lm_train),direction="forward",trace=FALSE)
  
  t1 <- mean(lm_train$residuals^2)
  t2 <- mean(lm_step$residuals^2)
  t3 <- mean(lm_stepfwd$residuals^2)
  
  rmse_train <- sqrt(t1)
  rmse_step_train <- sqrt(t2)
  rmse_stepfwd_train <- sqrt(t3)
  
  rmse_train_results <- data.frame()
  rmse_train_results <- rbind(rmse_train,rmse_step_train,rmse_stepfwd_train)
  colnames(rmse_train_results) <- "RMSETrain"
  
  models[[1]] <- lm_train
  models[[2]] <- lm_step
  models[[3]] <- lm_stepfwd
  names(models) <- modelNames
  
  
  # Test LM models
  
  lm_residuals_test <- df2[,c(response)] - predict(lm_train,data = df2)
  step_resid_test      <- df2[,c(response)] - predict(lm_step,data = df2)
  stepfwd_resid_test      <- df2[,c(response)] - predict(lm_stepfwd,data = df2)
  
  t4 <- colMeans(lm_residuals_test^2)
  t5 <- colMeans(step_resid_test^2)
  t6 <- colMeans(stepfwd_resid_test^2)
  
  rmse_test <- sqrt(t4)
  rmse_test_step <- sqrt(t5)
  rmse_test_stepfwd <- sqrt(t6)
  
  rmse_test_results <- data.frame()
  rmse_test_results <- rbind(rmse_test,rmse_test_step,rmse_test_stepfwd)
  colnames(rmse_test_results) <- "RMSETest"
  
  reg_results <- cbind(rmse_train_results,rmse_test_results)
  rownames(reg_results) <- modelNames 
  
  for ( m in seq(1,length(models),1)) {
    model <- models[[m]]
    residuals_best <- model$residuals
    res <- df1[,c(response)]
    res_name <- colnames(res)
    colnames(res) <- c("y")
    modelPlots <- list()
    residuals_x <- list()
    
    for (i in seq(1,ncol(df1[,c(predictors)]),1)) {
      p <- df1[,c(predictors)][i]
      n <- colnames(p)
      colnames(p) <- c("x")
      s1 <- pOutput(
        plot = plot(p$x,scale(residuals_best,center = T,scale = T),main = paste(n," vs Residuals",sep = ""),
                    xlab= n,ylab="Standardized Residuals"),
        w = 800,h = 800,vName = n,output = "png",folder = "Models",
        subfolder = paste(subName,"/Linear Models/",names(models[m]),"/Residuals",sep = "")
      )
      residuals_x[[i]] <- ggdraw() + draw_image(s1)
    }
    
    s1 <- pOutput(
      plot = plot(res$y,scale(residuals_best,center = T,scale = T),main = paste(res_name," vs Residuals",sep = ""),
                  xlab= res_name,ylab="Standardized Residuals"),
      w = 800,h = 800,vName = paste("Response - ",res_name," vs Residuals",sep = ""),
      output = "png",folder = "Models",
      subfolder = paste(subName,"/Linear Models/",names(models[m]),"/Residuals",sep = "")
    )
    
    folder = "Models"
    vName1 = "Residual Histogram"
    w = 800
    h = 800
    subfolder = paste(subName,"/Linear Models/",names(models[m]),"/Model Plots",sep = "")
    subfolder = paste(folder,"/",subfolder,sep = "")
    path = paste(subfolder,"/",sep = "")
    vName1 = paste(path,vName1,".png",sep = "")
    
    png(filename=vName1,width = w, height = h,units = "px",pointsize = 12, bg = "white", 
        res = NA, restoreConsole = TRUE)
    
    print(histogram(scale(model$residuals,center=TRUE,scale=TRUE),
                    main="Residual Histograms",xlab="Standardized Residuals"))
    dev.off()
    
    s2 <- vName1
    
    folder = "Models"
    vName2 = "QQ Plot"
    w = 800
    h = 800
    subfolder = paste(subName,"/Linear Models/",names(models[m]),"/Model Plots",sep = "")
    subfolder = paste(folder,"/",subfolder,sep = "")
    path = paste(subfolder,"/",sep = "")
    vName2 = paste(path,vName2,".png",sep = "")
    png(filename=vName2,width = w, height = h,units = "px", 
        pointsize = 12, bg = "white", res = NA,restoreConsole = TRUE) 
    
    qqnorm(scale(model$residuals,center=TRUE,scale=TRUE))
    qqline(scale(model$residuals,center=TRUE,scale=TRUE))
    
    dev.off()
    s3 <- vName2
    
    stdCoef <- coef(lm.beta(model))
    s4 <- pOutput(
      plot = barplot(rev(sort(stdCoef)),main = "Standarized Beta Weights"),
      w = 800,h = 800,vName = "Standarized Beta Weights",output = "png",folder = "Models",
      subfolder = paste(subName,"/Linear Models/",names(models[m]),"/Model Plots",sep = "")
    )  
    
    s5 <- pOutput(
      plot = plot(model$fitted.values,scale(summary(model)$residuals,center=TRUE,scale=TRUE),
                  main="Fitted Values to Standardized Residuals",
                  xlab="Fitted Values",ylab="Standardized Residuals"),
      w = 800,h = 800,vName = "Fitted Values to Standardized Residuals",output = "png",
      folder = "Models",
      subfolder = paste(subName,"/Linear Models/",names(models[m]),"/Model Plots",sep = "")
    )
    folder = "Models"

    s6 <- printModel(
      fName = "Model Summary",
      folder = paste(folder,"/",subName,"/Linear Models/",names(models[m]),"/Model Plots",sep = ""),
      type = "Linear",
      table = summary(model)
      )
    
  
    modelPlots[[1]] <- ggdraw() + draw_image(s1)
    modelPlots[[2]] <- ggdraw() + draw_image(s2)
    modelPlots[[3]] <- ggdraw() + draw_image(s3)
    modelPlots[[4]] <- ggdraw() + draw_image(s4)
    modelPlots[[5]] <- ggdraw() + draw_image(s5)
    modelPlots[[6]] <- ggdraw() + draw_image(s6)

    names(modelPlots) <- c(
      "Response vs Standardized Residuals",
      "Standarized Residual Histogram",
      "QQ Plot",
      "Standardized Beta Weights",
      "Fitted Values to Standardized Residuals",
      "Model Summary"
    )
    
    names(residuals_x) <- colnames(df1[,c(predictors)])
    modelResults[[1]] <- modelPlots
    modelResults[[2]] <- residuals_x
    modelResults[[3]] <- model
    
    names(modelResults) <- c("Model Plots","Standardized Residuals","Model")
    results[[m]] <- modelResults
    
  }
  
  best_name <- which(reg_results[,c("RMSETest")] == min(reg_results[,c("RMSETest")]))
  if (length(best_name) > 1) {best_name <- best_name[1]}
  best_name <- names(best_name)
  results[[4]] <- as.data.frame(reg_results)
  names(results) <- c(modelNames,paste("RMSE Results - ",best_name," Smallest RMSE"))
  
  results
}

interation = function(df,describe,response,predictors,folder,subfolder) {
  
  interation1 <- list()
  
  interation1[[1]] <- describe
  
  interation1[[2]] <- Regularization(
    df = df,
    predictors = predictors,
    response = response,
    folder=folder,
    subfolder=subfolder
  )
  
  interation1[[3]] <- LinearModels(
    df = df,
    predictors = predictors,
    response = response,
    folder=folder,
    subfolder=subfolder
  ) 
  names(interation1) <- c("Description","Regularization","OLS")
  interation1
}
