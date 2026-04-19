specification_curve_generator<- function(spec_object, decision_columns, beta_column, p_column, samplesize_col=F, 
                                                 p_threshold=0.05, fx_output="plot", decision_key=F, plot_line=F, 
                                                 poa_unidirectional=T, poa_uni_beta=0, plot_lim=c(2000,0.25)){
  #u7.22.22
  # spec_object <- multi_spec
  # decision_columns = c(1,2,3,4,5)
  # beta_column = 6
  # p_column = 7
  # samplesize_col = 8
  # fx_output = "plot"
  # poa_unidirectional = F
  # decision_key = decision_key_input
  # plot_line=T
  # samplesize_col <- F
  # p_threshold=0.05
  
  library(reshape2)
  library(cowplot)
  
  #updated 10.20.22
  if(is.null(dim(decision_key)[1])==T){
    decision_key <- data.frame(c(unique(unlist(spec_object[,decision_columns])),colnames(spec_object[,decision_columns])),
                               c(unique(unlist(spec_object[,decision_columns])),colnames(spec_object[,decision_columns])))
  }
  ifelse(is.null(dim(samplesize_col)[1])==T, spec_object$sample_size <- 1, spec_object$sample_size <- spec_object[,samplesize_col])
  ifelse(plot_line==T, plot_line_alpha <- 0.2, plot_line_alpha <- 0)
  ifelse(plot_line==T, plot_point_size <- 4, plot_point_size <- 2)
  
  varmatch_generic <- function(var_vector, df_key){
    print("vartable is vector; first df_key col is uncorrected var")
    base_match <- base::match(var_vector, df_key[,1])
    neworder <- df_key[base_match,2]
    neworder
  }
  
  spec_object$beta_column <- spec_object[,beta_column]
  spec_object <- spec_object[order(as.numeric(as.character(spec_object[,beta_column]))),]
  spec_object$specnum <- c(1:dim(spec_object)[1])
  
  #plot generator
  spec_object$p_color <- ifelse(as.numeric(spec_object[,p_column])<p_threshold,paste0("p < ", round(p_threshold,3)), "p = ns")
  
  
  specification_curve_plot <- ggplot(data = spec_object, aes(x=specnum,y=as.numeric(beta_column), color=as.factor(p_color))) +
    geom_point(data = spec_object, aes(x=specnum,y=as.numeric(beta_column)), size=2, alpha=0.8)+
    labs(color="", y="Beta Weight", x="Specification Number", subtitle=paste0("Specification Curve (specifications=", dim(spec_object), ")"))+
    scale_fill_discrete(name = "")+
    theme_bw()+
    theme(legend.position=c(0.2, 0.85), 
          axis.text = element_text(size = 10), 
          axis.title = element_text(size=12),
          title = element_text(size=12),
          legend.key.size = unit(0.5, 'cm'),
          legend.text = element_text(size=10),
          legend.background=element_blank())
  
  #+  expand_limits(x = dim(spec_object)[1]*1.1, y = max(spec_object$Estimate)*1.1)
  
  #table generator
  spec_object_table <- spec_object
  rownames(spec_object_table) <- spec_object_table$specnum
  plotDat_spec <- melt(as.matrix(spec_object_table[,c(decision_columns)]))
  plotDat_spec$check_col <- paste0(plotDat_spec$Var2, "_", plotDat_spec$value)
  plotDat_spec$decision <- varmatch_generic(plotDat_spec$value,decision_key)
  plotDat_spec$value <- 1
  # if(length(table(plotDat_spec$check_col)) - length( table(plotDat_spec$decision)) >0){print("Error: Non-unique labels (decision_key_input) were used")}
  # check_unique_names <- rbind(table(plotDat_spec$check_col), table(plotDat_spec$decision))
  # if(max(abs(check_unique_names[1,]- check_unique_names[2,]))>0) {print("Error: Non-unique labels (decision_key_input) were used")}
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  specification_curve_table<- ggplot2::ggplot(plotDat_spec, aes(Var1, decision, label = value, fill=c("blue")[value])) +
    geom_tile() +
    scale_fill_identity() +
    scale_colour_manual(values=cbPalette)+
    theme_minimal() +
    labs(x="Specification Number", y="Analytic Decision")+
    theme(axis.text = element_text(size = 10))
  
  sc_table <- specification_curve_table + coord_cartesian(xlim=c(0,dim(spec_object)[1]*1.1))
  sc_plot <- specification_curve_plot + coord_cartesian(xlim=c(0,dim(spec_object)[1]*1.1))
  sc_all <- list(sc_plot, sc_table)
  
  #beta table by analytic decision
  specmelt <- spec_object[,c(decision_columns, beta_column)]
  specmelt <- melt(specmelt, id.vars = colnames(spec_object)[beta_column])
  specmelt[,colnames(spec_object)[beta_column]] <- as.numeric(specmelt[,colnames(spec_object)[beta_column]])
  full_decisionlist <- names(table(specmelt$variable))
  bw_table <- data.frame()
  bw_full_table <- data.frame()
  for (a in 1:length(full_decisionlist)){
    specmelt_temp <- specmelt[specmelt$variable==full_decisionlist[a],] 
    ind_decisionlist <- names(table(specmelt_temp$value))
    bw_perdecision <- data.frame()
    for (b in 1:length(ind_decisionlist)){
      specmelt_dt <- specmelt_temp[specmelt_temp$value==ind_decisionlist[b],]
      specmelt_nodt <- specmelt_temp[specmelt_temp$value!=ind_decisionlist[b],]
      sd_pooled <- sqrt((sd(specmelt_dt[,colnames(spec_object)[beta_column]])^2 + sd(specmelt_nodt[,colnames(spec_object)[beta_column]])^2)/2)
      cohend <- (mean(specmelt_dt[,colnames(spec_object)[beta_column]]) -mean(specmelt_nodt[,colnames(spec_object)[beta_column]]))/sd_pooled
      cohend <- round(cohend, 2)
      bw_val <- round(mean(specmelt_dt[,colnames(spec_object)[beta_column]]),3)
      bw_val_median <- round(median(specmelt_dt[,colnames(spec_object)[beta_column]]),3)
      tempout <- c(full_decisionlist[a], ind_decisionlist[b], bw_val, bw_val_median, cohend)
      bw_perdecision <- c(bw_perdecision, list(tempout))
    }
    bw_pd_print <- do.call("rbind", bw_perdecision)
    colnames(bw_pd_print) <- c("Decision", "Specification", "Mean Beta","Median Beta", "Cohens D")
    bw_full_table <- c(bw_full_table, list(bw_pd_print))
  }
  bw_full_table_print <- do.call("rbind", bw_full_table)
  bw_full_table_print <- as.data.frame(bw_full_table_print)
  bw_full_table_print <- bw_full_table_print[order(bw_full_table_print$Decision, bw_full_table_print$Specification),]
  
  #poa generator
  spec_object_temp <- spec_object
  poa_dir <- "all"
  if(poa_unidirectional==T){
    spec_object_temp[,beta_column] <- as.numeric(spec_object_temp[,beta_column])
    beta_dir <- length(which(spec_object_temp[,colnames(spec_object_temp)[beta_column]]>=0))/length(spec_object_temp[,beta_column])
    ifelse(beta_dir>=0.5, spec_object_temp <- spec_object[which(spec_object[,colnames(spec_object)[beta_column]]>=0),],
           spec_object_temp <- spec_object[which(spec_object[,colnames(spec_object)[beta_column]]<0),])
    ifelse(beta_dir>=0.5, poa_dir <- "positive", poa_dir <- "negative")
  }
  specmelt <- spec_object_temp[,c(decision_columns, p_column, which(colnames(spec_object_temp)=="sample_size"))]
  
  specmelt <- melt(specmelt, id.vars = colnames(spec_object_temp)[c(p_column, which(colnames(spec_object_temp)=="sample_size"))])
  full_decisionlist <- names(table(specmelt$variable))
  poa_table <- data.frame()
  poa_full_table <- data.frame()
  for (a in 1:length(full_decisionlist)){
    specmelt_temp <- specmelt[specmelt$variable==full_decisionlist[a],] 
    ind_decisionlist <- names(table(specmelt_temp$value))
    poa_perdecision <- data.frame()
    for (b in 1:length(ind_decisionlist)){
      specmelt_dt <- specmelt_temp[specmelt_temp$value==ind_decisionlist[b],]
      poa_val <- length(which(specmelt_dt[,colnames(spec_object)[p_column]]<0.05))/length(specmelt_dt[,colnames(spec_object)[p_column]])
      size_val <- round(mean(specmelt_dt$sample_size), 0)
      tempout <- c(full_decisionlist[a], ind_decisionlist[b], poa_val, size_val, length(which(specmelt_dt[,colnames(spec_object)[p_column]]<0.05)), length(specmelt_dt[,colnames(spec_object)[p_column]]))
      poa_perdecision <- c(poa_perdecision, list(tempout))
    }
    poa_pd_print <- do.call("rbind", poa_perdecision)
    colnames(poa_pd_print) <- c("Decision", "Specification", "poa", "n", "N (sig specs)","N (all specs)")
    poa_pd_print <- as.data.frame(poa_pd_print)
    poa_pd_print$poa <- as.numeric(as.character(poa_pd_print$poa))
    poa_pdout <- c(max(poa_pd_print$poa)-min(poa_pd_print$poa),
                   poa_pd_print[poa_pd_print$poa==max(poa_pd_print$poa), "Specification"], max(poa_pd_print$poa),poa_pd_print[poa_pd_print$poa==max(poa_pd_print$poa), "n"],
                   poa_pd_print[poa_pd_print$poa==min(poa_pd_print$poa), "Specification"], min(poa_pd_print$poa), poa_pd_print[poa_pd_print$poa==min(poa_pd_print$poa), "n"])
    poa_pdout[c(1,3,6)] <-  round(100*as.numeric(as.character(poa_pdout[c(1,3,6)])),1)
    poa_full_table <- c(poa_full_table, list(poa_pd_print))
    poa_table <- c(poa_table, list(c(full_decisionlist[a], poa_pdout)))
  }
  poa_full_table_print <- do.call("rbind", poa_full_table)
  poa_full_table_print <- as.data.frame(poa_full_table_print)
  poa_full_table_print <- poa_full_table_print[order(poa_full_table_print$Decision, poa_full_table_print$Specification),]
  poa_full_table_print$poa <- round(100*poa_full_table_print$poa,1)
  poa_full_table_print <- poa_full_table_print[order(poa_full_table_print$Decision, poa_full_table_print$Specification),]
  poa_full_table_print <- cbind(poa_full_table_print, poa_dir, dim(spec_object_temp)[1])
  colnames(poa_full_table_print)[c(3,7,8)] <- c("POA", "POA Direction", "Total possible bidirectional Specs")
  bwpoa_datatable <- cbind(bw_full_table_print, poa_full_table_print)
  
  #summary_table
  specmelt <- spec_object[,c(decision_columns, beta_column, p_column, which(colnames(spec_object)=="sample_size"))]
  specmelt[,colnames(spec_object)[beta_column]] <- as.numeric(specmelt[,colnames(spec_object)[beta_column]])
  summary_table <- c(mean(specmelt[,colnames(spec_object)[beta_column]]), 
                     median(specmelt[,colnames(spec_object)[beta_column]]),
                     length(which(as.numeric(specmelt[,colnames(spec_object)[p_column]])<0.05))/dim(specmelt)[1],
                     dim(specmelt)[1],
                     length(which(as.numeric(spec_object_temp[,colnames(spec_object_temp)[p_column]])<0.05))/dim(spec_object_temp)[1],
                     dim(spec_object_temp)[1],
                     poa_dir,
                     mean(specmelt$sample_size))
  summary_table <- as.data.frame(t(summary_table))
  colnames(summary_table) <- c("Mean B", "Median B", "POA","POA-#/Full Specs","POA-Unidirectional", "POA-Unidirectional-#","POA-direction", "Sample Size")
  # dec_num_total <- 1
  # for (a in 1:length(decision_columns)){
  #   unique_vals <- length(unique(spec_object[,decision_columns[a]]))
  #   dec_num_total <- c(dec_num_total, unique_vals)
  # }
  # 
  
  print(fx_output)
  #output type identifier
  if(fx_output=="all"){
    multiverse_output <- sc_all
    cowplot::plot_grid(plotlist=sc_all, ncol=1, align='v')
  }
  
  if(fx_output=="table"){
    multiverse_output <- specification_curve_table
    plot(specification_curve_table)
  }
  if(fx_output=="plot"){
    multiverse_output <- specification_curve_plot
    plot(specification_curve_plot)
  }
  if(fx_output=="data"){
    multiverse_output <- bwpoa_datatable
  }
  if(fx_output=="summary"){
    multiverse_output <- summary_table  
  }
  multiverse_output
}
