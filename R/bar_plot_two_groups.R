#' @title Bar plot of distribution of a variable stratified by two groups
#' @description The function returns a ggplot bar group of a continuous variable stratified by a binary grouping variable.
#' @return a ggplot object

#' @param dat  data table. one of the 3: dat.rds,dat7_1L.rds,dat7_min_Result_1L.rds, merged with FirstLineTherapyGroup.rds.
#' @param cohort a character string. Column name in `dat`, used as a grouping variable. This variable should be a class a factor in `dat`
#' @param y a character string. Column name in `dat`. A variable we want to plot the bar graph for. The column in dat should be a numeric variable.
#' @param geom_text_location_prop double, bounded btw 0 and 1. A propotion of x axis window where the location of N(%) will be written. The closer the value to 1, more right the text will be.
#' @param this.fontsize font size for y labels and N(%).
#' @param weight a character string. A column name in `dat` which is used for weighting the `cohort` column. If not given, non-weighted plot will be returned.
#' @param trim.percent Natural number between 1 through 100. The minimum percent of treatment to be plotted. Any treatment less than this percent will be excluded from plotting. If not given, all treatment will be plotted. The percent is up to 2 digits.

#' @import data.table ggplot2
#' @export

bar_plot_two_groups<-function(dat,cohort,geom_text_location_prop=0.8,this.fontsize=3,weight=NULL, trim.percent=NULL){


  #change the variable name to cohort
  setnames(dat,old=cohort,new="cohort")

  #If we are given weight, change the name of the weight variable
  if(!is.null(weight)){
    setnames(dat,old=weight,new="sw")

  }


  #Get the cohort levels
  cohort_levels<-as.character(levels(dat$cohort))
  cohort1<-cohort_levels[1]
  cohort2<-cohort_levels[2]


  #Generate the data table for plotting. Different for weighted and non-weighted version
  if(!is.null(weight)){
    trt_counts<-dat[,sum(sw,na.rm = TRUE),by=c("cohort","FirstLineTherapyGroup")]
    setnames(trt_counts, "V1","N")
    trt_counts_wide<-dcast(trt_counts, FirstLineTherapyGroup~cohort, value.var = "N")

    #sort it by most common on the top to least on the bottom.
    trt_counts_wide[,total:=sum(c(get(cohort1),get(cohort2)),na.rm = T),by=FirstLineTherapyGroup]
    trt_counts_wide<-trt_counts_wide[order(total,decreasing = T)]
    trt_counts_wide

    trt_counts[,cohort_N:=as.numeric(NA)]

    #Count the number of cohort for each cohort
    #number of cohort1
    cohort1_N<-dat[cohort==(cohort1),sum(sw)]
    cohort2_N<-dat[cohort==(cohort2),sum(sw)]

    trt_counts[cohort==cohort1,cohort_N:=(cohort1_N)]
    trt_counts[cohort==cohort2,cohort_N:=(cohort2_N)]

    trt_counts[,proportion:=N/cohort_N]
    trt_counts[,percent:=sprintf("%.2f",proportion*100)]

    #round to 1 decimal place
    trt_counts[,N:=sprintf("%.1f",N)]
    trt_counts[,cohort_N:=sprintf("%.1f",cohort_N)]
    trt_counts[,res:=stringr::str_c(N,"(",percent,"%)")]
    trt_counts[order(N, decreasing = T),]


  }else{
    #generate data.table for counting different therapy
    trt_counts<-dat[,.N,by=c("cohort","FirstLineTherapyGroup")]
    trt_counts_wide<-dcast(trt_counts, FirstLineTherapyGroup~cohort, value.var = "N")
    #sort it by most common on the top to least on the bottom.
    trt_counts_wide[,total:=sum(c(get(cohort1),get(cohort2)),na.rm = T),by=FirstLineTherapyGroup]

    trt_counts_wide<-trt_counts_wide[order(total,decreasing = T)]
    trt_counts_wide

    #Plot
    trt_counts[,cohort_N:=as.numeric(NA)]
    trt_counts[cohort==cohort1,cohort_N:=sum(dat$cohort==cohort1)]
    trt_counts[cohort==cohort2,cohort_N:=sum(dat$cohort==cohort2)]

    trt_counts[,proportion:=N/cohort_N]
    trt_counts[,percent:=sprintf("%.2f",proportion*100)]
    trt_counts[,res:=stringr::str_c(N,"(",percent,"%)")]
    trt_counts[order(N, decreasing = T),]
  }





  #Order the FirstLineTherapyGroup to appear most at the top and least at the bottom.
  trt_counts[
    ,
    FirstLineTherapyGroup:=
      factor(FirstLineTherapyGroup,
             levels = rev(trt_counts_wide$FirstLineTherapyGroup),
             label=rev(trt_counts_wide$FirstLineTherapyGroup))
  ]


  #Print the 5 most common 1L therapy for each cohort
  for(i in 1:2){
    these_1L<-
      trt_counts[
        cohort==cohort_levels[i]
      ][
        order(proportion,decreasing = T)
      ][
        1:5,
        paste(FirstLineTherapyGroup,res)
      ]
    cat("The 5 most common 1Ls for",
        cohort_levels[i],
        "group in the order of greatest to smallest are:",
        paste(these_1L,collapse = "/"),".\n")
  }

  # return(trt_counts)
  trt_counts[,N:=as.numeric(N)]
  #Change the cohort label so that plot can include count (N=)
  if(!is.null(weight)){
    cohort1_label<-paste(cohort1," (N=",sprintf("%.1f",cohort1_N),")", sep="")
    cohort2_label<-paste(cohort2," (N=",sprintf("%.1f",cohort2_N),")", sep="")
  }else{
    cohort1_count<-trt_counts[cohort==cohort1,sum(N)]
    cohort2_count<-trt_counts[cohort==cohort2,sum(N)]
    cohort1_label<-paste(cohort1," (N=",cohort1_count,")", sep="")
    cohort2_label<-paste(cohort2," (N=",cohort2_count,")", sep="")
  }

  levels(trt_counts$cohort)<-c(cohort1_label,cohort2_label)


  #Exclude all treatment less than minimum percentage
  if(!is.null(trim.percent)){
    #Use proportion instead. percent is character. Proportion is numeric.
    trt_counts<-trt_counts[proportion>=(trim.percent/100),]
  }

  #plot
  ggplot(trt_counts,
         aes(fill=cohort, y=FirstLineTherapyGroup, x=proportion)) +
    geom_bar(position="dodge", stat="identity", show.legend = FALSE)+
    facet_grid(cols = vars(cohort))+
    geom_text(position=position_dodge(width = 0.9),
              stat='identity',
              aes(x=max(proportion)*geom_text_location_prop,label=res),
              size=this.fontsize)+
    xlab("Proportion")+
    ylab("First Line Therapy Group")

}
