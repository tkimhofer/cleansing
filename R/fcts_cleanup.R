#' Set non-informative values to NA
#' @param x variable to be cleaned
#' @return variable with values such as "na" set to NA
#' @details Replaced strings: NA, nan, inf, "NA", "NAN", "null" with NA
.conv_na<-function(x){
  # remove leading and traling whitespaces
  x=gsub('^ | $', '',  x)
  # clean NA
  gsub('^na$|^null$|^nan$|^inf$|^null$|^NA$|^\\s*$|^$', NA, x, ignore.case = T)
}

#' Remove all na values
#' @param x variable to be cleaned
#' @return number of non-informative values
.rm_na<-function(x){
  x[which(!is.na(x))]
}

#' @title Counting the number of NA
#' @param x variable for summary statistic
#' @param y variable for stratification
#' @return df counting NA, abs, %
.count_na<-function(x, y){
  # x is variable to be summaryised
  # y is stratification
  # x,y are clean, ie no other variables
  n_x=length(x)
  n_y=length(y)

  if(n_x != n_y){stop('count_na -> vectors have different lengths')}

  idx_x=(is.na(x))
  idx_y=(is.na(y))

  x_na=length(which(idx_x))
  y_na=length(which(idx_y))
  xANDy_na=length(which(idx_x & idx_y))
  xORy_na=length(which(idx_x | idx_y))

  x_na=c(x_na, x_na/n_x*100)
  y_na=c(y_na, y_na/n_x*100)
  xANDy_na=c(xANDy_na, xANDy_na/n_x*100)
  xORy_na=c(xORy_na, xORy_na/n_x*100)

  sum_tbl=data.frame(n_x=c(n_x, 100), x_na, n_y=c(n_y, 100), y_na, xANDy_na, xORy_na)
  rownames(sum_tbl)=c('abs', 'perc')

  out=list(summary=sum_tbl, idx_xORy=which(!idx_x & !idx_y), idx_x=which(!idx_x), idx_y=which(!idx_y))

  return(out)
}

.count_na_x<-function(x){
  # x is variable to be summaryised
  # y is stratification
  # x,y are clean, ie no other variables
  n_x=length(x)
  idx_x=(is.na(x))
  x_na=length(which(idx_x))

  x_na=c(x_na, x_na/n_x*100)
  x_val=c(n_x-x_na[1], 100-x_na[2])

  sum_tbl=data.frame(x_n=c(n_x, 100), x_na, x_val)
  rownames(sum_tbl)=c('abs', 'perc')

  out=list(summary=sum_tbl, idx_x=which(!idx_x))

  return(out)
}

#' @title Variable class conversion
#' @param x variable to be converted
#' @param dtype desired variable class (numeric, string, factor)
#' @param levs levels in case of class factor
#' @return converted x
.conv_dtype<-function(x, dtype=c('num', 'str', 'fac', 'bool'), levs=NULL){


  .conv<-function(x, dtype){
    switch(dtype,
           'num'={as.numeric(x)},
           'fac'={
             if(!is.null(levs)) {factor(x, levels=levs)} else{
               as.factor(x)
             }
           },
           'str'=as.character(x),
           'bool'=as.character(x)
           )
  }


  if(is.list(x)){
    out=lapply(x, function(xx) {.conv(xx, dtype)})
  }else{
    out=.conv(x, dtype)
  }

  return(out)

}

#' @title Split multilevel variables
#' @param x Multilevel variable to be split, given as string
#' @param sep Symbol that separates levels within a string
#' @param return return list or vector (see Details)
#' @return converted x
#' @details return argument is list: each x element is list element, return argument is vector: list is vectorised to 1D array, length does not equal number of elements in x then.
.split_multiresp<-function(x, sep=', ', return=c('list', 'vector')){

  x_split=strsplit(x, sep) # this is a list
  x_split=lapply(x_split, gsub, pattern="^ | $", replacement="")

  if(return=='vector'){
    # create array of indeces to track responses from db rows/entries
    names(x_split)=paste0(as.character(1:length(x_split)), '.')
    return(unlist(x_split))
  }else{
    return(x_split)
  }
}



.split_multiresp_df<-function(x, sep=', '){

  x_split=strsplit(x, sep) # this is a list
  x_split=lapply(x_split, gsub, pattern="^ | $", replacement="")

  ids=unlist(sapply(seq(x_split), function(i){
    rep(i,  length(x_split[[i]]))
  }))
  out=data.frame(id=ids, val=unlist(x_split))

  return(out)
}

#' @title Merge low freq levels into a single level
#' @param x array, categorical variable to be processed
#' @param n num Percent frequency value of x level, for assigning new level
#' @param repl char, Name of new level
#' @param includeLevN logic, Level name indicates number of merges
#' @return x with low freq levels merged into new level
.combine_lowFreqLevels<-function(x, n=2, repl='Other', includeLevN=T){
  cs=table(x)
  idx=which(cs<(sum(cs)*(n/100)))

  if(length(idx)>0){
    lev_repl=names(idx)
    if(includeLevN) {
      n=length(idx)
      repl=paste0(repl, ' (', n, ' levels)')
    }

    lev_repl=gsub('(', '\\(', lev_repl, fixed=T)
    lev_repl=gsub(')', '\\)', lev_repl, fixed=T)
    x_repl=gsub(paste0('^', paste0(lev_repl, collapse='$|^'), '$'), repl, x, fixed = F)
    return(x_repl)
  }else{
    return(x)
  }

}

#' @title Combine uni and/or multilevel variables, rm NA's
#' @param x array, categorical or numeric variable to be merged with y
#' @param y array, categorical or numeric variable to be merged with x
#' @return list, 1: merged x and y, 2: indices of NA in x or y
.comb_xy<-function(x, y){
  if(is.atomic(x) & is.atomic(y)){
    out=data.frame(x=x, y=y, id=seq_along(along.with=x))
  }

  if(is.data.frame(x) & is.atomic(y)){
    out=data.frame(x=x$val, y=y[x$id], id=x$id)
    }

  if(is.data.frame(y) & is.atomic(x)){
    #browser()
    out=data.frame(x=x[y$id], y=y$val, id=y$id)}

  if(is.data.frame(y) & is.data.frame(x)){
    browser()
  }
  return(out)
}


#' @title Univariate stats for a categorical variable
#' @param ds data.frame, categorical variable and segmentation variable
#' @param x.multi logic, is x cleaned multi-level variable?
#' @param y.multi logic, is y cleaned multi-level variable?
#' @return list, 1: contingency table(x,y) and y, 2: Chi-squared p value
#' @importFrom stats chisq.test
.countsFreq_chiSq<-function(ds, x.multi, y.multi){
  if(y.multi & !x.multi){
    y_lev=unique(ds$y)
    ds_uni=unique(ds[, colnames(ds) %in% c('x', 'ids')])
    ds_uni$y=F
    ds_uni$y=factor(ds_uni$y, levels=c(T, F))
    ds_uni$x=factor(ds_uni$x, levels=unique(ds$x))
    ct=sapply(y_lev, function(lev, ds_u=ds_uni){
      idc=ds$ids[which(ds$y==lev)]
      ds_u$y[ds_u$ids %in% idc]=T
      table(x=ds_u$x, seg=ds_u$y)[,1]
    })
  }

  if(x.multi & !y.multi){
    x_lev=unique(ds$x)
    ds_uni=unique(ds[, colnames(ds) %in% c('y', 'ids')])
    ds_uni$x=F
    ds_uni$y=factor(ds_uni$y, levels=unique(ds$y))
    ds_uni$x=factor(ds_uni$x, levels=c(T, F))
    ct=sapply(x_lev, function(lev, ds_u=ds_uni){
      idc=ds$ids[which(ds$x==lev)]
      ds_u$x[ds_u$ids %in% idc]=T
      table(x=ds_u$x, seg=ds_u$y)[1,]
    })
  }

  if(!x.multi & !y.multi){
    ct=table(ds$x, ds$y)
  }

  if(any(ct<5 & ct!=0)) warning('Low number of counts, p value instable')
   cs=chisq.test(ct)
   return(list(ct, cs))
}

.ct_to_perc<-function(ct){
  if(is.null(nrow(ct))){ct/sum(ct)}else{
    ct=apply(ct, 2, function(x){
      x/sum(x)
    })
  }
  ct*100
}


#' @title Statistical group comparison for a numeric variable
#' @param x num array, variable for group comparison
#' @param y cat array, segementation variable defining groups
#' @return list, 1: p value for comparing all groups and y, 2: contingency table y
#' @details Pariwise group comparison is performed with fct .num_groupComp_pariwise
#' @importFrom stats kruskal.test
#' @importFrom utils combn
.num_groupComp<-function(x, y){
  ct=table(y)
  ctp=(ct/sum(ct))*100
  if(any(ct<5)) stop('low number of counts')
  if(any(ctp<15)) warning('unequal group sizes, p value instable')
  if(length(ct)==1) stop('no segmantation level')
    p=kruskal.test(x, factor(y))$p.value
    # could do pairwise effect sizes here
    return(list(p, ctp))
}


#' @title Pairwise statistical group comparison for a numeric variable
#' @param x num array, variable for group comparison
#' @param y cat array, segementation variable defining groups
#' @return matrix, row 1: p value, row 2: Cliff's delta effect size
#' @importFrom stats kruskal.test
.num_groupComp_pairwise<-function(x, y){
  pw_combn=combn(unique(as.character(y)), 2)
  ps=apply(pw_combn, 2, function(lev){
    idx=which(y %in% lev)
    g=factor(y[idx])
    val=x[idx]
    p=kruskal.test(val, g)$p.value
    dc=es_cdelta(ref=x[which(g==lev[1])], comp=x[which(g==lev[2])])
    c(p, dc)
  })
  ps=as.data.frame(ps)
  colnames(ps)=apply(pw_combn, 2, paste, collapse=' vs.')

  return(ps)
}

#' @title Pairwise effect size - Cliff's delta
#' @param ref num array, variable values for group 1 (reference group)
#' @param comp ref num array, variable values for group 2 (comparison group)
#' @return Cliff's delta value
es_cdelta <- function(ref, comp) {

  if (!is.numeric(ref) | !is.numeric(comp))
    stop("Input not numeric")

  ind_ref <- is.na(ref) | is.infinite(ref)
  ind_comp <- is.na(comp) | is.infinite(comp)

  if (any(ind_ref)) {
    ref <- ref[!ind_ref]
    message("Removing NA or infinite values from reference group.")
  }
  if (any(ind_comp)) {
    comp <- comp[!ind_comp]
    message("Removing NA or infinite values from comparator group.")
  }

  if (length(ref) < 5 | length(comp) < 5)
    stop("Low number of values (< 5)")

  top_counts <- vapply(ref, function(x, y = comp) {
    names(x) <- NULL
    names(y) <- NULL
    c(length(which(x > y)), length(which(x < y)))
  }, FUN.VALUE = c(2, length(ref)))
  out <- ((sum(top_counts[1, ]) - sum(top_counts[2, ]))/(length(ref) * length(comp))) *
    (-1)
  return(out)
}






#' @title Descriptive stats for a numeric variable startified by second variable
#' @param out list generated with .summary_var
#' @return array of quantiles (range, IQR point estimates, median), length and %total for each segmentation level
.descrStats_x<-function(out){

  x=out$x_na_rm$val
  if(out$x$dtype_inferred=='num'){
    # median range
    stats=paste0(round(median(x), 1),' (', round(min(x),1),'-', round(max(x),1), ')')
    lab='Median (Range)'
    out=list(stats, lab)

  }else{
    # level freq (perc)
    ct=table(x, rep('all', length(x)))
    ctp=.ct_to_perc(ct)
    if(nrow(ct)>1){

      out=.comb_tbls(ct, ctp, format = 'long', html = F)
      out=paste(rownames(out), out[,1], sep=': ')
      idx=1:(length(out)-1)
      out[idx]=out[idx][order(ct, decreasing = T)]
      out=paste(out, collapse='\n')
    }else{
      out=paste(paste(rownames(ct), ct[1], sep=': '), ' (100%)', sep='')
        }
  }

  return(out)
}


#' @importFrom plyr ddply
#' @importFrom stats quantile
.num_descrStats_xy<-function(x, y){
  ct=table(y)
  ctp=(ct/sum(ct))*100

  if(any(ct<5)) stop('low number of counts')
  if(any(ctp<15)) warning('unequal group sizes, p value instable')
  if(length(ct)==1) stop('no segmantation level')

  cd=data.frame(x, y)
  quants=ddply(cd, .(y), function(ds, ntot=nrow(cd)){
    c(quantile(ds$x, probs=c(0, 0.25, 0.5, 0.75, 1)), n=nrow(ds), n_perc=(nrow(ds)/ntot) *100)
  })

  return(quants)
  # could do pairwise effect sizes here
}






#  Create table of abs and rel frequencies (%) for categorical variables
#  list of two: 1: p values semgentation levels, 2: contigency table (abs, %)
#  table ready to export
# .to_tbl_cat<-function(pv){
#   nr=nrow(pv[[1]][[1]])
#   nc=ncol(pv[[1]][[1]])
#   ct_out=t(sapply(seq(nr), function(i){
#     paste0(pv[[1]][[1]][i,], ' (', round(pv[[2]][i,]), '%)')
#   }))
#
#   colnames(ct_out)=colnames(pv[[2]])
#
#   ct_out=data.frame(id=rownames(pv[[2]]), ct_out)
#   add=rbind(rep("", nc+1), rep("", nc+1), rep("", nc+1))
#   add[2,1]='p value'
#   add[3,1]=format.pval(pv[[1]][[2]]$p.value, digits = 2)
#   colnames(add)=colnames(ct_out)
#
#   return(rbind(ct_out, add))
#
#
# }


#' @title Create table of summary stats for continuous variables
#' @param pv percentage contingency value
#' @return table ready to export
.to_tbl_num<-function(pv){
  nr=nrow(pv[[1]][[1]])
  nc=ncol(pv[[1]][[1]])
  ct_out=t(sapply(seq(nr), function(i){
    paste0(pv[[1]][[1]][i,], ' (', round(pv[[2]][i,]), '%)')
  }))

  colnames(ct_out)=colnames(pv[[2]])

  ct_out=data.frame(id=rownames(pv[[2]]), ct_out)
  add=rbind(rep("", nc+1), rep("", nc+1), rep("", nc+1))
  add[2,1]='p value'
  add[3,1]=format.pval(pv[[1]][[2]]$p.value, digits = 2)
  colnames(add)=colnames(ct_out)

  rbind(ct_out, add)


}


#' @importFrom stats addmargins
#' @importFrom htmlTable addHtmlTableStyle htmlTable
.comb_tbls<-function(abs, ct, format, html=T){

  if(format=='long'){
    abs=addmargins(abs)
    ct=addmargins(ct)

    out=sapply(seq(ncol(abs)), function(i){
      pp=as.character(round(ct[,i]))
      pp[pp=='0' & abs[,i]>0]='<1'
      paste0(formatC(abs[,i], big.mark = ','),' (', pp, '%)')
    })


    if(is.null(nrow(out))) out=t(out)
    colnames(out)=colnames(abs)
    rownames(out)=rownames(abs)
    return(out)
  }
  if(format=='1row'){

    out=sapply(seq(nrow(abs)), function(i){
        pp=as.character(round(ct[i,]))
        pp[pp=='0' & abs[i,]>0]='<1'
        oo=paste0(formatC(abs[i,], big.mark = ','),' (', pp, '%)')
        oo
    })
    out=c(out)

    olong=matrix(out, nrow=1, ncol=length(out))
    olong=rbind(rep(colnames(ct), nrow(ct)), olong)


    if(html==T){
      tbl=olong
      # html table
      tbl[2,] %>%
        addHtmlTableStyle(align="c",
                          # col.columns = c(rep("none", 2),
                          #                 rep("#F5FBFF", 4)),
                          css.cgroup='background-color: #F5FBFF;') %>%

        htmlTable(
          header=olong[1,],
          cgroup=rownames(ct),
          n.cgroup=c(rep(ncol(ct), nrow(ct)))
        ) -> ex

    }else{
      ex=rbind(add[1:ncol(olong)], olong)
      rownames(ex)=NULL


    }


    return(ex)

  }


}



#' @export
.tbl_output<-function(res, format, html=T){

  if(format=='long'){
    n_tot=formatC(sum(res$counts[[1]])+sum(res$nas[1,]), big.mark = ',')
    out<-.comb_tbls(res$counts[[1]], res$perc, format)
    if(ncol(out)<3) {
      add=matrix("", nrow=nrow(out), ncol=ncol(out)-3)
      out=cbind(out, add)
    }
    out[,ncol(out)]=gsub(' \\(.*', '', out[,ncol(out)])
    add<-add1<-rep("", ncol(out))
    out<-rbind(out, " "=add)
    add1[1]=n_tot
    out=rbind(out, "n_total"=add1)
    add[1]<-c(format.pval(res$counts[[2]]$p.value, 2))
    out<-rbind(out, p.value=add)

    nas<-.comb_tbls(t(t(res$nas[1,])), t(t(res$nas[2,])), format)
    nas=c(paste(c('Var:', 'Seg:', 'Both:'), nas)[c(2,1,3)], rep('', ncol(out)-3))
    out=rbind(out, "nas"=nas)
    rownames(out)[nrow(out)]='NAs'


  }
  if(format=='1row'){

    if(html){
      out=.comb_tbls(res$counts[[1]], res$perc, format, html)
    }else{
      out=.comb_tbls(res$counts[[1]], res$perc, format)
    }
  }


  return(out)

}


#' @title infer variable data type
#' @param x variable of interest
#' @param gsub_pattern regex for str to remove (e.g., '<|>')
#' @param n_un_lim number of unique values in x to define numeric (see details)
#' @param n_un_perc_lim fraction of unique values in x to define numeric (see details)
#' @details dtype inferred is one of num, str, logic, x should be free of na or non-informative values (see fct .rm_na). Parameters n_un_lim and n_un_frac_lim define absolute and fraction of unique values, respectively, to define x as numeric. This is to exclude categorical values from being defined as numeric (e.g., x levels of low medium high).
#' @return data type as string
infer_dtype=function(x, gsub_pattern='<|>', n_un_lim=5, n_un_perc_lim=10){
  t_num=NA
  t_str=NA
  t_bool=NA

  x_le=length(x) # this is not adjusted for multi-responses
  n_un=length(unique(x))
  n_un_perc=n_un/x_le

  x_num_le=length(which(!is.na(grep('^-?[0-9].*$', x))))/x_le

  # if all numeric na in x_num and more than n_un_lim unique values
  t_num=ifelse({x_num_le==1}, T, F)
  # if x has two or less than two values (numeric or not)
  t_bool=ifelse({n_un<=2}, T, F)
  t_str=ifelse({x_num_le<1 | n_un<=n_un_lim}, T, F)

  dtype=c('num', 'str', 'bool')[which(c(t_num, t_str, t_bool))]

  # establish priorities
  dtype_taken=NA
  if(length(dtype)>1){
    if(any(grepl('str', dtype))){dtype_taken='str'}
    if(any(grepl('bool', dtype))){dtype_taken='bool'}
  }else{
    dtype_taken=dtype
  }

  out=list(dtypes=dtype,
           dtype_inferred=dtype_taken,
           x_le=x_le, # this is not adjusted for multiple responses
           n_un=n_un,
           n_un_perc=n_un_perc
  )
  return(out)
}



#' @title infer if variable is of type multi-response
#' @param split_cutoff cut-off in percent for minimal entries with multi-responses
#' @param x variable to be cleaned
#' @param n_un length unique values of x
#' @param sep separators
#' @details Multi-response variables defined as strings with where multiple values are separated with comma, semicolon or any other alpha-numeric value indicated in sep argument
#' @return list of possible separators
infer_level=function(x, split_cutoff=20, n_un, sep=c(',', ';')){

#browser()
  mat=sapply(sep, function(sep){
    #browser()
        un_split=length(unique(gsub('^ | $', '', unique(unlist(strsplit(x, sep, fixed = T))))))
        impro=100-(un_split/n_un*100)
        if(is.nan(impro)){impro=0}
        return(impro)
  })

  out=list(tested_sep=mat)

  if(any(mat>split_cutoff)){
    idx=which.max(mat)
    out$sep_inferred=names(mat[idx])
    out$multiresponse=T
  }else{
    out$multiresponse=F
  }
  return(out)
}


.tbl_output_x_num<-function(res, format, html=T){

  x_tot=formatC(sum(c(unlist(res$nas[1,]), res$summary$n)), big.mark = ',', format='d')
  if(format=='long'){
    out=res$summary[,-1]
    out$n_perc=round(as.numeric(out$n_perc), 1)
    out$n=formatC(as.numeric(out$n), big.mark = ',', format='d')
    add=add1=rep('', ncol(out))
    out=rbind(out, add)
    add[1]=x_tot
    out=rbind(out, add)
    add[1]=format.pval(res$stats[[1]], digits = 2)
    out=rbind(out, add)
    add[1]=round(res$stats[[1]], digits = 2)
    out=rbind(out, add)

    rownames(out)=c(as.character(res$summary[,1]),'','n', 'p value', 'Cd')

  }
  if(format=='1row'){
    x_sum=apply(res$summary[,-1], 1, function(x){
      # check if integer
      if(all(as.integer(x[1:5])==x[1:5])) {r=0 }else{ r=2}
      ra=round(c(x[1], x[5]), r)
      paste(round(x[3], r), ' (', ra[1], '-', ra[2], ')', sep='')

    })
    df=as.data.frame(t(x_sum))
    colnames(df)=res$summary[,1]

    out=df
    if(html){

      tbl=df
      rownames(tbl)=NULL
      # html table
      tbl %>%
        addHtmlTableStyle(align="c",
                          css.cell = "padding-left: .5em; padding-right: .4em;") %>%
        htmlTable(
          rnames=F,
          n.cgroup=c(rep(ncol(tbl)))
        ) -> out
    }else{
      out=df
    }
  }

  return(out)

}

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

  ml_x=infer_level(x_na_rm, split_cutoff = 20,  n_un = un_le_x)
  ml_y=infer_level(y_na_rm, split_cutoff = 20,  n_un = un_le_y)

  if(ml_x$multiresponse){
    x_na_rm=.split_multiresp_df(x_na_rm, sep=ml_x$sep_inferred)

  }
  if(ml_y$multiresponse){
    y_na_rm=.split_multiresp_df(y_na_rm, sep=ml_y$sep_inferred)
  }

  # combine x and y
  xy_df=.comb_xy(x_na_rm, y_na_rm)


  x_dtype=infer_dtype(xy_df$x)
  y_dtype=infer_dtype(xy_df$y)

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

#' @importFrom stats median
.summary_var<-function(x){

  x.ori=x
  x=.conv_na(x)
  nas=.count_na_x(x)

  x_na_rm=x[nas$idx_x]
  ml_x=infer_level(x_na_rm, split_cutoff = 20,  n_un = length(unique(x_na_rm)))

  # different ways to combine ml variable
  # x is unilevel, y is multilevel: what is age distribution for visitors answering with level x
  # x is multilevel, y is unilevel: testing motivation responses for HIV test previously
  # x and y are multilevel:
  # the statistics should be done on individual level, since thge absolute number or responses does not reflect visitor numbers after separating multi-responses...owing to the fact that two individuals can provide different number of responses

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

  n=format(svar$nas$x_n[1], big.mark = ',')
  cmp_p=round(svar$nas$x_val[2], 1)
  complt_perc=paste0(ifelse({cmp_p==100 & (svar$nas$x_val[2]<100)}, '>99' ,cmp_p), '%')
  un_p=round(svar$x$n_un_perc, 1)*100
  un_perc=paste0(ifelse({un_p==0 & (svar$x$n_un_perc>0)}, '<1' ,un_p), '%')

  if(svar$x_ml[1]){
    dtype=paste('str ML ', "(sep=\"", svar$x_ml[2], "\")", sep="")
  }else{dtype=svar$x$dtype_inferred}

  out=matrix(c(n, dtype, complt_perc, un_perc, svar$avg_resp, svar$dst[[1]]), ncol=1)
  return(out)
}

