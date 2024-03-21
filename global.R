# preparation for html href & src -----------------------------------------------
shiny::addResourcePath('www', here::here("www"))

# 1.packages---------------------
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(shinyFeedback)
library(purrr)
library(stringr)
library(shinyscroll)
library(shinyhelper)
library(spsComps)
library(dplyr)
library(readxl)
library(readr)
library(survminer)
library(survival)
library(reshape)
library(reshape2)
library(tibble)
library(countup)
library(emayili)
library(htmltools)

# 2.1 load data--------------------
load('www/data/pathwaylist.Rdata')
load('www/data/final_group.Rdata') # grouplist
pathway <- as.character(pathwaylist$pathway) # pathway

load("www/data/download_dt_test.Rdata") #info_agg_dt

# 2.2 read_select_dt-----------------------------------------
read_select_dt <- function (x) {
  # load(file = 'www/data/final_group.Rdata')
  file_path <- paste0('www/data/PathwayDatatable/',final_group$group[which(final_group$pathway==x)],'.Rdata')
  load(file = file_path)
  subdt <- subdt[which(subdt$GeneSet == x),]
  subdt <- subdt[,c(1:6,10:12,7:9,13:16,20:22,17:19)]
  subdt
}

# 2.3 create_btns-------------------------------------------------------------
create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="choosem_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-paintbrush"></i></button>
                   <button class="btn btn-default action-button btn-default action_button" id="chooseb_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-paintbrush"></i></button>'
                     ))
}

# 2.4 PLOT in box4_pathway_plotbox ----------------------------------------------------------------
pathway_km_plot <- function(cancertype, gse, gpl, survival, pathway,  cutoff, col_1, col_2) {
  tryCatch(
    { 
      exp_file <- paste0("www/data/", cancertype, "/", gse, "/", survival, "_ssGSEA", cutoff, "_", gpl, ".Rdata")
      cli_file <- paste0("www/data/", cancertype, "/", gse,"/cli.Rdata")
      sur_index<-c(survival,paste0(survival,'_time'))
      load(exp_file)
      load(cli_file)
      
      cli<-cli[,sur_index]
      cli[,1]<-as.numeric(cli[,1]);cli[,2]<-as.numeric(cli[,2])
      cli<-na.omit(cli)
      exp <- ssgsea[pathway] 
      
       
      
      if(identical(rownames(cli),rownames(exp))){
         rt<-cbind(cli,exp)
     }else{
        both_pt<-intersect(rownames(cli),rownames(exp))
        cli<-cli[both_pt,]
        exp<-exp[both_pt,]
        rt<-cbind(cli,exp)
      }
      
      colnames(rt)[1:2]<-c('status','time')
      my.surv <- Surv(as.numeric(rt$time), as.numeric(rt$status))
      
      final_pathway<-data.frame(
        row.names = paste0(paste0('A',c(1:c(ncol(rt)-2)))),
        pathway = colnames(rt)[3:ncol(rt)],
        pathway1 = paste0(paste0('A',c(1:c(ncol(rt)-2)))))
      
      colnames(rt)[3:ncol(rt)]<-paste0(paste0('A',c(1:c(ncol(rt)-2))))
      
      if(cutoff %in% c('m' , 'b')) {
        
      pathway2 <-colnames(rt)[3]
      group = rt[,pathway2]
      data = cbind(rt[,1:2],group)
      group <- factor(group, levels = c(0,1))
      fit <- surv_fit(my.surv ~ group, data = data)
      data.survdiff <- survdiff(my.surv ~ group)
      p.val = 1 - pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1)
      HR = (data.survdiff$obs[2]/data.survdiff$exp[2])/(data.survdiff$obs[1]/data.survdiff$exp[1])
      up95 = exp(log(HR) + qnorm(0.975)*sqrt(1/data.survdiff$exp[2]+1/data.survdiff$exp[1]))
      low95 = exp(log(HR) - qnorm(0.975)*sqrt(1/data.survdiff$exp[2]+1/data.survdiff$exp[1]))
      HR <- paste("HR: ", format(HR,scientific= T ,digits = 3), sep = "")
      CI <- paste("95%CI: ", paste(format(low95,scientific= T ,digits = 3), format(up95,scientific= T ,digits = 3),sep = " ~ "), sep = "")
      }
      
      if(nchar(pathway)>52){
        pathway_re <- paste0(substr(pathway,1,52),"\n",substring(pathway,53))
      }else{
        pathway_re <- pathway
      }
      
      ggsurvplot(fit,
                 data = data,
                 # conf.int = TRUE,
                 # conf.int.style = "ribbon",
                 risk.table = "nrisk_cumcensor",#nrisk_cumcensor;abs_pct;nrisk_cumevents
                 tables.theme = theme_cleantable(),
                 tables.height = 0.2,
                 fontsize= 3,
                 tables.y.text=F,
                 surv.median.line='hv',
                 legend=c(0.8,0.8),
                 legend.title = pathway_re,
                 legend.labs = c(paste0("Low ssGSEA Scores"," (",fit$n[1],")"),
                                 paste0("High ssGSEA Scores"," (",fit$n[2],")")),
                 xlab=paste0(sur_index[1]," Time (Months)"),
                 ylab=paste0("Survival Rate for ",cancertype,"\n",gse,'-',gpl),
                 pval = paste(pval = paste("p = ",format(p.val,scientific= T ,digits = 3), sep = ""),
                              HR, CI, sep = "\n"),
                 pval.size=3,
                 palette = c(col_1, col_2),
                 censor=T,
                 censor.shape="|",
                 censor.size=2.5,
                 ggtheme=theme(axis.text.x = element_text(face="bold", color="black", size=8),    #font size
                               axis.text.y = element_text(face="bold",  color="black", size=8),
                               axis.title.x = element_text(face="bold", color="black", size=8),
                               axis.title.y = element_text(face="bold",color="black", size=8),
                               legend.text= element_text(face="bold", color="black", size=8),
                               legend.title = element_text(face="bold", color="black", size=8),
                               #legend.key.width = unit(8, 'mm'),
                               #legend.key.height = unit(8, 'mm'),
                               axis.line=element_line(color = "black"),
                               panel.background = element_blank(),
                               plot.title=element_text(face="bold", color="black",size=8)))
    },
    error = function(e){
      d <- data.frame(x=1, y=1, lab=c('Dear user, your current analysis encountered an error,\nif the same error is still reported after re-analysis,\nplease send your analysis to the following email\n(shoshanashi@i.smu.edu.cn), we will update the web tool\nand give you feedback as soon as possible.'))
      ggplot(d, aes(x, y)) + 
        geom_text(aes(label=lab), size=6)+
        theme(axis.text.x = element_blank(),    #font size
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.text= element_blank(),
              legend.title =element_blank(),
              axis.line=element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_blank(),
              plot.title=element_blank())
    }
  )
}

# 4 Comment box -----------------------------------------------------------
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}