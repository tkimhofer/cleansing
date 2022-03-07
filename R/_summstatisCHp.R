library(htmlTable)
library(dplyr)

load(file='../chp2020/clean_20210206_tidyDiag2019.rda') # ds
ds$Ori1=factor(ds$Ori1, levels=c('MSM', 'MSW', 'WSM', 'WSW', 'Unklar'))

idx=which(grepl('0|1|Yes', ds$berhiv))
ds$berhiv[idx]='HIV (rapid)'

test=summ_var(dat=ds, vars=.(berhiv, Ori1, yr), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 1: Performed HIV screening tests<br>', html=T,foot = NULL)

test=summ_var(dat=ds, vars=.(yr, Ori1, HIV), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 1: Performed HIV screening tests<br>', html=T,foot = NULL)


test=summ_var(dat=ds, vars=.(HIV, Ori1, yr), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 1: HIV screening outcomes<br>', html=T,foot = NULL)

# Syph

test=summ_var(dat=ds, vars=.(bertp, Ori1, yr), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 2a: Performed Syphilis screening tests<br>', html=T,foot = NULL)

test=summ_var(dat=ds, vars=.(TP, Ori1, yr), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 2b: Syphilis screening outcomes<br>', html=T,foot = NULL)


# Chlam und gon
table(ds$berchg)
ds$berchg1=gsub('-C/G', '', gsub('-Chl/Gon', '', ds$berchg, fixed = T))
ds$berchg1=gsub('Rachen', 'Throat', ds$berchg1)
ds$berchg1=gsub('Rektum', 'Anal', ds$berchg1)
ds$berchg1=gsub('Yes', NA, ds$berchg1)

test=summ_var(dat=ds, vars=.(berchg1, Ori1, yr), x.multi=NULL, n_perc=5)
html_table(test, var_name='Table 3a: Performed Chlamydia and Gonorrhea screenings<br>', html=T,foot = NULL)

test=summ_var(dat=ds, vars=.(Chlam, Ori1, yr), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 3b: Chlamydia screening outcomes<br>', html=T,foot = NULL)

test=summ_var(dat=ds, vars=.(Gon, Ori1, yr), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 3c: Gonorrhea screening outcomes<br>', html=T,foot = NULL)



# HCV

test=summ_var(dat=ds, vars=.(berhcv, Ori1, yr), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 4a: Performed HCV screening tests<br>', html=T,foot = NULL)

test=summ_var(dat=ds, vars=.(HCV, Ori1, yr), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 4b: HCV screening outcomes<br>', html=T,foot = NULL)

table(ds$HCV)



# ori

test=summ_var(dat=ds, vars=.(yr, Ori1), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 5b: Sexual orientation<br>', html=T,foot = NULL)

test=summ_var(dat=ds, vars=.(yr, gender), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 5a: Gender<br>', html=T,foot = NULL)

table(ds$HCV)


test=summ_var(dat=ds, vars=.(age, Ori1, yr), x.multi=NULL, n_perc=0.)
html_table(test, var_name='Table 6: Visitor age<br>', html=T,foot = '*Data entry ongoing')

test=summ_var(dat=ds, vars=.(cob, Ori1, yr), x.multi=NULL, n_perc=1)
html_table(test, var_name='Table 7: Visitor\'s country of birth<br>', html=T,foot = '*Data entry ongoing')

map=c('Yes'=1, 'No'=0)
ds$immParents1=names(map)[match(ds$immParents, map)]
test=summ_var(dat=ds, vars=.(immParents1, Ori1, yr), x.multi=NULL, n_perc=1)
html_table(test, var_name='Table 8: Parental immigration<br>', html=T,foot = '*Data entry ongoing')

# now create html table with stratified results

test=summ_var(dat=ds, vars=.(degree, Ori1, yr), x.multi=NULL, n_perc=0)
html_table(test, var_name='Table 9: Highest degree<br>', html=T,foot = '*Data entry ongoing')


test=summ_var(dat=ds, vars=.(job, Ori1, yr), x.multi=NULL, n_perc=0)
html_table(test, var_name='Table 10: Occupation<br>', html=T,foot = '*Data entry ongoing')

map=c('> EUR 800 pcm'=1, '<= EUR 800 pcm'=0)
ds$inc1=names(map)[match(ds$inc, map)]
test=summ_var(dat=ds, vars=.(inc1, Ori1, yr), x.multi=NULL, n_perc=0)
html_table(test, var_name='Table 11: Net income per calendar month (pcm)<br>', html=T,foot = '*Data entry ongoing')

map=c('Yes'=1, 'No'=0, 'Unsure'=2)
ds$kv1=names(map)[match(ds$kv, map)]
test=summ_var(dat=ds, vars=.(kv1, Ori1, yr), x.multi=NULL, n_perc=0)
html_table(test, var_name='Table 12: Health insurance plan<br>', html=T,foot = '*Data entry ongoing')

test=summ_var(dat=ds, vars=.(relform, Ori1, yr), x.multi=NULL, n_perc=0)
html_table(test, var_name='Table 13: Relationship form<br>', html=T,foot = '*Data entry ongoing')

# idx=which(ds$relform!='Single')
# test=summ_var(dat=ds[idx,], vars=.(relle, Ori1, yr), x.multi=NULL, n_perc=0)
# html_table(test, var_name='Table 12: Relationship length in months<br>', html=T,foot = '*Data entry ongoing')


table(ds$sexpm_s, useNA = 'always')
test=summ_var(dat=ds, vars=.(sexpm_s, Ori1, yr), x.multi=F, n_perc=0, x.dtype='str')
html_table(test, var_name='Table 14a: Est. total number of male sex-partners per year<br>', html=T,foot = '*Data entry ongoing')


test=summ_var(dat=ds, vars=.(sexpm_us, Ori1, yr), x.multi=F, n_perc=0, x.dtype='str')
html_table(test, var_name='Table 14b: Est. number of male sex-partners per year - unsafe sex-practices<br>', html=T,foot = '*Data entry ongoing')




test=summ_var(dat=ds, vars=.(sexpw_s, Ori1, yr), x.multi=F, n_perc=0, x.dtype='str')
html_table(test, var_name='Table 15a: Est. total number of female sex-partners per year<br>', html=T,foot = '*Data entry ongoing')


test=summ_var(dat=ds, vars=.(sexpw_us, Ori1, yr), x.multi=F, n_perc=0, x.dtype='str')
html_table(test, var_name='Table 15b: Est. number of female sex-partners per year - unsafe sex-practices<br>', html=T,foot = '*Data entry ongoing')



map=c('Yes'=1, 'No'=0, 'Unsure'=2)
ds$ssex1=names(map)[match(ds$ssex, map)]

idx=which(ds$yr>2017)
test=summ_var(dat=ds[idx,], vars=.(ssex1, Ori1, yr), x.multi=F, n_perc=0, x.dtype='str')
html_table(test, var_name='Table 16a: Safe-sex practices other than condoms
<br>', html=T,foot = '*Data entry ongoing')


idx=which(ds$ssex=='1'& ds$yr>2017)
ds$ssexyes=gsub('^PrEP', 'Ich nehme PrEP', ds$ssexyes, ignore.case = T)
test=summ_var(dat=ds, vars=.(ssexyes, Ori1, yr), x.multi=F, n_perc=0, x.dtype='str')
html_table(test, var_name='Table 16b: Safe-sex practices other than condoms - which one?
<br>', html=T,foot = '*Data entry ongoing')



idx=(grep('Prep', ds$ssexyes, ignore.case = T))
map=c('Daily'=1, 'On demand'=2)
ds$prep_freq1=names(map)[match(ds$prep_freq, map)]
test=summ_var(dat=ds[idx,], vars=.(prep_freq1, Ori1, yr), x.multi=F, n_perc=0, x.dtype='str')
html_table(test, var_name='Table 17a: PrEP dosing<br>', html=T,foot = '*Data entry ongoing')

idx=(grep('Prep', ds$ssexyes, ignore.case = T))
test=summ_var(dat=ds[idx,], vars=.(prep_duration, Ori1, yr), x.multi=F, n_perc=0)
html_table(test, var_name='Table 17b: PrEP therapy duration<br>', html=T,foot = '*Data entry ongoing')








x=ds$hivtestreason
y=NULL

dds=ds

svar=.summary_var(x)


idx=1:nrow(ds)
x=ds$age[idx]

x.multi=F
y.multi=F
x.dtype=NULL
x.levs=NULL
y.dtype=NULL
y.levs=NULL

pv=prep_var(x, y,
            x.multi,
            y.multi,
            x.dtype,
            x.levs,
            y.dtype,
            y.levs)



infer_dtype(y)

