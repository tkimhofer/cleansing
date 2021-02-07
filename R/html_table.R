

#' @importFrom stats addmargins
#' @importFrom htmlTable addHtmlTableStyle htmlTable
html_table<-function(test, var_name='Health insurance in place', foot = '*Transcription ongoing', html=T, ord_row=NULL){
  require(magrittr)
  at=attr(test,"split_labels")
  at_un=apply(at, 2, unique)

  # format numeric
  if(ncol(test[[1]])>1 && colnames(test[[1]])[2]=='Freq'){

    # loop over levels
    tt=lapply(test, function(x){
      pp=round(x$perc)
      x$out=paste0(formatC(x$Freq, big.mark = ','),'<br/>(', ifelse(pp==0 & x$perc>0, '<1', pp), '%)')
      idx=grep('(0', x$out, fixed = T)
      x$out[idx]='0'
      x$out
    })
    tt=do.call(cbind, tt)
    rownames(tt)=test[[1]]$x

    idx_ord=order(test[[1]]$perc, decreasing = T)
    tt=tt[idx_ord,]

    # idx=grep('Other (', rownames(test), fixed=T)
    # if(length(idx)==1){
    #   iseq=1:nrow(test)
    #   reord=c(iseq[-idx], iseq[idx])
    #   test=test[reord,]
    # }


    # extract highest level
    seg_top=sapply(strsplit(colnames(tt), '.', fixed = T), '[[', 1)
    colnames(tt)=gsub('.*\\.', '', colnames(tt))

    dimnames(tt) = list(rownames(tt),
                    gsub('.*\\.', '', colnames(tt)))

  }else{
    # loop over levels
    tt=lapply(test, function(x){
      x=round(x$x, 1)
      out=paste0(x[3],'<br/>(', x[1], '-', x[5], ')')
      if(all(grepl('0', x[c(1,5)], fixed = T))){
        out='0'
      }
      if(all(is.na(x))){
        out='N/A'
      }
      out
    })


    # extract highest level
    seg_top=sapply(strsplit(colnames(tt), '.', fixed = T), '[[', 1)
    colnames(tt)=gsub('.*\\.', '', colnames(tt))

    dimnames(tt) = list(NULL,
                        gsub('.*\\.', '', colnames(tt)))



  }
    # colgroup

    ts=table(seg_top)
    if(!all(ts==1)){
      tg=unique(cbind(as.numeric(ts)[match(seg_top, names(ts))], seg_top))
      idx=which(tg[,2]=='2020')
      tg[idx,2]=paste0(tg[idx,2], '*')

      tab_out = tt %>%
        addHtmlTableStyle(css.cell = "padding-left: .5em; padding-right: .2em; padding-top: .2em",
                          css.header = "font-weight: normal; font-family: Arial, Helvetica, sans-serif;",
                          col.rgroup = c("none", "#F1F0FA")) %>%
        htmlTable( cgroup =tg[,2],
                   n.cgroup = as.numeric(tg[,1]),
                   caption =   paste("<b> <center> <font-family:font-family: Arial, Helvetica, sans-serif>", var_name, "<br>"),
                   tfoot = foot)
    }else{
      tab_out = tt %>%
        addHtmlTableStyle(css.cell = "padding-left: .5em; padding-right: .2em; padding-top: .2em",
                          css.header = "font-weight: normal; font-family: Arial, Helvetica, sans-serif;",
                          col.rgroup = c("none", "#F1F0FA")) %>%
        htmlTable(caption =   paste("<b> <center> <font-family:font-family: Arial, Helvetica, sans-serif>", var_name, "<br>"),
                   tfoot = foot)
    }

  return(tab_out)
}
