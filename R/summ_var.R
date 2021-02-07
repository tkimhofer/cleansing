#' @export
#' @title Variable summary on stratification level
#' @description Variable summary on stratification level
#' @param x variable for summary stats
#' @param y stratification variable
#' @param x.multi logic, indicating if x is mult-response variable variable to be cleaned
#' @param x.dtype data type of x ('num', 'str', 'fac', logic)
#' @param y.dtype data type of y ('num', 'str', 'fac', logic)
#' @param x.levs levels of x in case x is factor
#' @param y.levs levels of y in case y is factor
#' @param n_perc frequency cutoff in percentage to combine categorical levels into class 'other'
#' @return table ready to exporthivtestreason


# table(dat$Ori1)
# dat$Ori1=factor(dat$Ori1, levels=c('MSM', 'MSW', 'WSM', 'WSW', 'Unklar'))
summ_var<-function(dat=dds, vars=.(age, Ori1, yr), x.multi=NULL, n_perc=5, x.dtype=NULL){

  idx=match(names(vars), colnames(dat))
  if(length(idx)>3){stop('too many variables')}
  if(length(idx)<length(vars) | any(is.na(idx))){stop('check variables')}

  # s1: check if stratification level is present
  # remove nas in full ds
  ds=dat[,idx]

  # count na and rm na
  idx_no_na=apply(ds, 1, function(x){!any(is.na(x))})
  ds=ds[idx_no_na,]
  # rm nas first
  xp=.prep_x(x = ds[,1], x.multi=NULL, n_perc = n_perc, x.dtype=x.dtype)
  if(is.null(x.dtype)){x.dtype=infer_dtype(xp[[2]]$x)$dtype_inferred}

  if(length(idx)==1){
    strat=F

    if(x.dtype=='num'){
      x_stats=.num_descrStats_x(xp[[2]]$x)
      # summary stats
    }else{

      # summary table
      cs=as.data.frame(table(xp[[2]]$x))
      cs$perc=(cs$Freq/sum(cs$Freq)) *100
      #cs=cs[order(cs$Freq, decreasing = T),]
      x_stats=cs
    }
  }else{
    strat=T
    # prep x and ys


    # calc_stats for each stratification level
    x_stats=dlply(ds, rev(names(vars)[-1]), function (x){

      if(x.dtype=='num'){
        x_stats=data.frame(x=.num_descrStats_x(x[,1]))
        return(x_stats)
        # summary stats
      }else{

        xps=.prep_x(x = x[,1], x.multi=NULL, un=xp[[3]],  n_perc = 0, x_lev=xp[[4]], x.dtype=x.dtype)

        idx=which(!xps[[2]]$x %in% xp[[3]])

        if(length(idx)>0){
          xps[[2]]$x[idx]=grep('Other (', xp[[3]], fixed = T, value=T)
        }

        df=xps[[2]]
        x=factor(df$x, levels=xp[[3]])
        # summary table
        cs=as.data.frame(table(x))

        cs$perc=(cs$Freq/sum(cs$Freq)) *100

        #cs=cs[order(cs$Freq, decreasing = T),]
        return(cs)
      }

    })
  }

  return(x_stats)


}
