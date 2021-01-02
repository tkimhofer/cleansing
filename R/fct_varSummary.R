.summary_var_xy<-function(x, y=NULL){

  if(is.null(y)){
    y=rep('All', length(x))
    y.multi=F
    y.dtype='fac'
  }


  x.ori=x
  y.ori=y

  # x_na=.count_na(x)
  # y_na=.count_na(y)

  x=.conv_na(x)
  y=.conv_na(y)

  nas=.count_na(x, y)
  x_na_rm=x[nas$idx_x]
  y_na_rm=y[nas$idx_y]
  xy_na_rm=x[nas$idx_xORy]

  le_xy=length(x_na_rm)
  un_le_x=length(unique(x_na_rm))
  un_le_y=length(unique(y_na_rm))

  ml_x=infer_level(x_na_rm, split_cutoff = 20, n = le_xy, n_un = un_le_x)
  ml_y=infer_level(y_na_rm, split_cutoff = 20, n = le_xy, n_un = un_le_y)


  # different ways to combine ml variable
  # x is unilevel, y is multilevel: what is age distribution for visitors answering with level x
  # x is multilevel, y is unilevel: testing motivation responses for HIV test previously
  # x and y are multilevel:
  # the statistics should be done on individual level, since thge absolute number or responses does not reflect visitor numbers after separating multi-responses...owing to the fact that two individuals can provide different number of responses

  # if(ml_x$multiresponse & !ml_y$multiresponse){}
  # if(!ml_x$multiresponse & ml_y$multiresponse){}
  # if(ml_x$multiresponse & ml_y$multiresponse){}
  if(ml_x$multiresponse){
    x_na_rm=.split_multiresp_df(x_na_rm, sep=ml_x$sep_inferred)

  }
  if(ml_y$multiresponse){
    y_na_rm=.split_multiresp_df(y_na_rm, sep=ml_y$sep_inferred)
  }

  # combine x and y
  #browser()
  xy_df=.comb_xy(x_na_rm, y_na_rm)


  x_dtype=infer_dtype(xy_df$x)
  y_dtype=infer_dtype(xy_df$y)
  #browser()

  out=list(

    nas=nas$summary,
    x=x_dtype,
    y=y_dtype,
    avg_resp=round(nrow(xy_df)/le_xy),
    le_id_noNA=length(unique(xy_df$id)),
    x_ml=c(ml_x$multiresponse, ml_x$sep_inferred),
    y_ml=c(ml_y$multiresponse, ml_y$sep_inferred)
  )
  return(out)
}

.summary_var<-function(x){

  x.ori=x
  x=.conv_na(x)
  #browser()
  nas=.count_na_x(x)

  x_na_rm=x[nas$idx_x]

  ml_x=infer_level(x_na_rm, split_cutoff = 20,  n_un = length(unique(x_na_rm)))

  # different ways to combine ml variable
  # x is unilevel, y is multilevel: what is age distribution for visitors answering with level x
  # x is multilevel, y is unilevel: testing motivation responses for HIV test previously
  # x and y are multilevel:
  # the statistics should be done on individual level, since thge absolute number or responses does not reflect visitor numbers after separating multi-responses...owing to the fact that two individuals can provide different number of responses

  # if(ml_x$multiresponse & !ml_y$multiresponse){}
  # if(!ml_x$multiresponse & ml_y$multiresponse){}
  # if(ml_x$multiresponse & ml_y$multiresponse){}
  if(ml_x$multiresponse){
    x_na_rm=.split_multiresp_df(x_na_rm, sep=ml_x$sep_inferred)
  }else{
    x_na_rm=data.frame(id=seq_along(x_na_rm), val=x_na_rm, stringsAsFactors = F)
  }
  x_dtype=infer_dtype(x_na_rm$val)

  x_na_rm$val=.conv_dtype(x_na_rm$val, dtype = x_dtype$dtype_inferred)


  out=list(
    nas=nas$summary,
    x=x_dtype,
    avg_resp=round(x_dtype$x_le/nas$summary$x_val[1],1),
    le_id_noNA=nas$summary$x_val[1],
    x_ml=c(ml_x$multiresponse, ml_x$sep_inferred),
    x_na_rm=x_na_rm
  )

  dst=.descrStats_x(out)
  out$dst=dst

  return(out)
}

# create table from summary statistics from x, one column per variable, multiple rows

tbl_summary<-function(svar, var='x'){

  #browser()
  n=format(svar$nas$x_n[1], big.mark = ',')
  cmp_p=round(svar$nas$x_val[2], 1)
  complt_perc=paste0(ifelse({cmp_p==100 & (svar$nas$x_val[2]<100)}, '>99' ,cmp_p), '%')
  un_p=round(svar$x$n_un_perc, 1)*100
  un_perc=paste0(ifelse({un_p==0 & (svar$x$n_un_perc>0)}, '<1' ,un_p), '%')

  if(svar$x_ml[1]){
    #dml=paste(svar$x_ml[1], "(sep=\"", svar$x_ml[2], "\)", sep="")
    dtype=paste('str ML ', "(sep=\"", svar$x_ml[2], "\")", sep="")
  }else{dtype=svar$x$dtype_inferred}

  #browser()
  out=matrix(c(n, dtype, complt_perc, un_perc, svar$avg_resp, svar$dst[[1]]), ncol=1)
  return(out)
}


add_summary<-function(ds1){
  summary_list=lapply(1:ncol(ds1), function(i){
    s=.summary_var(ds1[,i])
    print(i)
    tbl_summary(s)
  })

  out=do.call(cbind, summary_list)
  rownames(out)=c('n', 'dtype', 'Complt', 'Unique', 'Avg Resp', 'Summary')
  colnames(out)=colnames(ds1)
  return(out)

}





