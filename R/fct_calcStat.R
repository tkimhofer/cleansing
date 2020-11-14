#' @title Summaris variable dependent on stratification level
#' @param list of two: 1: p values semgentation levels, 2: contigency table (abs, %)
#' @return table ready to export

# idx=which(ds1$yr >=2017 & ds1$yr<=2019)
# x=ds1$age[idx]
# x[x<16]=NA
# y=ds1$yr[idx]
# x.multi=F
# y.multi=F
# x.dtype=NULL
# x.levs=NULL
# y.dtype=NULL
# y.levs=NULL
#
# pv=prep_var(x, y,
#          x.multi,
#          y.multi,
#          x.dtype,
#          x.levs,
#          y.dtype,
#          y.levs)


prep_var<-function(x, y, x.multi=F, y.multi=F, x.dtype=c('num', 'str', 'fac'),
                   x.levs=NULL, y.dtype=c('num', 'str', 'fac'), y.levs=NULL){

  x.ori=x
  y.ori=y

  x=.clean_na(x)
  y=.clean_na(y)

  nas=.count_na(x, y)

  # determinet type of variable uni, multi; num, char, fac
  if(is.null(x.dtype)){
  iid=length(which(is.na(as.numeric(x))))
  if(iid>length(x)){x.dtype='str'}else{
    x.dtype=='num'
  }
  }



  if(x.multi){
    x=.split_multiresp(x, sep=', ', return='list')
  }
  if(y.multi){
    y=.split_multiresp(y, sep=', ', return='list')
  }

  x=.conv_dtype(x, x.dtype, x.levs)
  y=.conv_dtype(y, y.dtype, y.levs)

  if(y.dtype=='num'){
    y=cut(y, breaks = quantile(y, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm = T), include.lowest = T)
  }

  df=.comb_xy(x, y)
  idx_keep=df[[2]]
  ds=df[[1]]

  ds$y=.combine_lowFreqLevels(ds$y, n=2, repl='Other', includeLevN=T)

  if(x.dtype!='num'){
    ds$x=.combine_lowFreqLevels(ds$x, n=2, repl='Other', includeLevN=T)
    res=.countsFreq_chiSq(ds, x.multi, y.multi)
    res_perc=.ct_to_perc(res[[1]])
    return(list(p_values=res, count=res_perc, nas=nas))
  }else{
    res=.num_groupComp(ds$x, ds$y)
    res_descr=.num_descrStats(ds$x, ds$y)
    res_pairwise<-.num_groupComp_pairwise(ds$x, ds$y)
    return(list( nas=nas, stats=res, summary=res_descr, p_values_cd=res_pairwise))
  }
}











