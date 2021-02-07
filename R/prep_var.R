#' @export
#' @title Variable summary on stratification level
#' @description Variable summary on stratification level
#' @param x variable for summary stats
#' @param y stratification variable
#' @param x.multi logic, indicating if x is mult-response variable variable to be cleaned
#' @param y.multi logic, indicating if y is mult-response variable variable to be cleaned
#' @param x.dtype data type of x ('num', 'str', 'fac', logic)
#' @param y.dtype data type of y ('num', 'str', 'fac', logic)
#' @param x.levs levels of x in case x is factor
#' @param y.levs levels of y in case y is factor
#' @param n_perc frequency cutoff in percentage to combine categorical levels into class 'other'
#' @return table ready to export

prep_var<-function(dat=dds, vars=.(hivtestreason, Ori1, yr), x.multi=F, y.multi=F, x.dtype=c('num', 'str', 'fac'),
                   x.levs=NULL, y.dtype=c('num', 'str', 'fac'), y.levs=NULL, n_perc=5){


  idx=match(names(vars), colnames(dat))
  if(length(idx)>3){stop('too many variables')}
  if(length(idx)<length(vars) | any(is.na(idx))){stop('check variables')}

  # s1: check if stratification level is present
  x=dat[,idx[1]]
  xp=.prep_x(x, x.multi=NULL)
  if(length(idx)==1){
    strat=F

    if(x.dtype=='num'){
      x_stats=.num_descrStats_x(df$x)
      # summary stats
    }else{

      # summary table
      cs=as.data.frame(table(df$x))
      cs$perc=(cs$Freq/sum(cs$Freq)) *100
      cs=cs[order(cs$Freq, decreasing = T),]
      x_stats=cs
    }
  }else{
    strat=T

    # remove nas in full ds
    ds=dat[,idx]

    # count na and rm na
    idx_no_na=apply(ds, 1, function(x){!any(is.na(x))})
    ds=ds[idx_no_na,]


    # prep x and ys

    # calc_stats for each stratification level
    test=dlply(ds, rev(names(vars)[-1]), function (x){
      if(x.dtype=='num'){
        x_stats=.num_descrStats_x(x[,2])
        x_stats
        # summary stats
      }else{
        xp=.prep_x(x[,1], x.multi=NULL)
        df=xp[[2]]
        df$x=factor(df$x, levels=xp[[3]])
        # summary table
        cs=as.data.frame(table(df$x))

        cs$perc=(cs$Freq/sum(cs$Freq)) *100
        cs=cs[order(cs$Freq, decreasing = T),]
        cs
      }

    })


  }


  # count and rm na

  # if s1: divide x into stratification levels
  if(strat){

  }else{
    # check var type
  }

  # calc summary statistic independently with stratification level


  # if s1: perform statistical group comparisons


  if(is.null(y)){
    y=rep('All', length(x))
    y.multi=F
    y.dtype='fac'
  }


  df.ori=data.frame(x, y)
  df=df.ori[apply(df.ori, 1, function(x){!any(is.na(x))}),]

  y_lev=length(unique(df$y))>2
  if(!y_lev) { y.dtype='str'}

  if(is.null(x.multi)) x.multi=ifelse(infer_level(x)=="Single level", F, T)
  if(is.null(y.multi)) y.multi=ifelse(infer_level(y)=="Single level", F, T)

  nas=.count_na(df.ori$x, df.ori$y)

  # determinet type of variable uni, multi; num, char, fac
  # if(is.null(x.dtype)){
  # iid=length(which(is.na(as.numeric(x))))
  # if(iid>length(x)*0.4){x.dtype='str'}else{
  #   x.dtype='num'
  # }
  # }


  if(x.multi){
    x=.split_multiresp(x, sep=', ', return='list')
  }
  if(y.multi){
    y=.split_multiresp(y, sep=', ', return='list')
  }

  if(is.null(x.dtype)) x.dtype=infer_dtype(df$x)$dtype_inferred
  if(is.null(y.dtype) & !y_lev) y.dtype=infer_dtype(df$y)$dtype_inferred


  df$x=.conv_dtype(df$x, x.dtype, x.levs)
  df$y=.conv_dtype(df$y, y.dtype, y.levs)

  if(y.dtype=='num'){
    y=cut(y, breaks = quantile(y, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm = T), include.lowest = T)
  }

  df=.comb_xy(df$x, df$y)
  ds=df

  #browser()
  if(!y_lev){ds$y=.combine_lowFreqLevels(ds$y, n=n_perc, repl='Other', includeLevN=T)}


  if(x.dtype!='num'){
    ds$x=.combine_lowFreqLevels(ds$x, n=n_perc, repl='Other', includeLevN=T)
    res=.countsFreq_chiSq(ds, x.multi = x.multi, y.multi = y.multi)
    res_perc=.ct_to_perc(res[[1]])
    return(list(counts=res, perc=res_perc, nas=nas))
  }else{
    res_descr=.num_descrStats_x(ds$x)
    if(!y_lev){
      res=.num_groupComp(ds$x, ds$y)
      res_descr=.num_descrStats_xy(ds$x, ds$y)
      res_pairwise<-.num_groupComp_pairwise(x = ds$x, y = ds$y)
      return(list( nas=nas, stats=NULL, summary=res_descr, p_values_cd=NULL))
    }else{
      return(list( nas=nas, stats=res, summary=res_descr, p_values_cd=res_pairwise))
      }
  }
}
