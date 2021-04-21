library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(purrr)
library(tidyr)
setwd('/Users/petermannino/Documents/School/Classes/Education Policy/Issue Brief')


##### DOJ school crime dataset #####

sro_link<-'2020054/schooldisc.csv'

sro_df<-read_csv(sro_link) %>% rename(sro_present=C0610,
                                      physical_restraints=C0621,
                                      pepper_spray=C0622,
                                      guns=C0624,
                                      body_cams=C0626,
                                      traffic_ctrl=C0628,
                                      patrol=C0630,
                                      discipline=C0632,
                                      problem_solve=C0636,
                                      tchr_training=C0638,
                                      mentoring=C0640,
                                      teaching=C0642,
                                      reporting_probs=C0644,
                                      legal_consult=C0646,
                                      mou_signed=C0650,
                                      mental_health_service=C0661)

# function recode variables so no=0 and yes=1
recode<-function(x) {
  if (x==2) {
    return(0)}
  else if (x==-1){
    return(NA)}
  else {
    return(x)}
  }

# recode variables
cols<-c('sro_present','pepper_spray','physical_restraints','guns',
        'body_cams','traffic_ctrl','patrol','discipline', 'problem_solve','tchr_training',
        'mentoring','teaching','reporting_probs','legal_consult','mou_signed','mental_health_service')

for (col in cols) {
  sro_df[col]<-apply(sro_df[col],MARGIN = 1,recode)
}

# % of schools with sworn law enforcement officer
pct_sro<-weighted.mean(sro_df$sro_present,sro_df$FINALWGT)
pct_sro # a little over half of schools have an SRO

# what equipment do sro's carry?
cols=c('pepper_spray','physical_restraints','guns','body_cams')
ops<-list()
for (col in cols) {
  ops[col]<-weighted.mean(sro_df[col],sro_df['FINALWGT'],na.rm = TRUE)
}

as.data.frame(ops) # >91% carry a gun(!) and handcuffs, only a third have body cams

# What do SROs do?
cols=c('traffic_ctrl','patrol','discipline', 'problem_solve','tchr_training',
       'mentoring','teaching','reporting_probs','legal_consult')
ops<-list()
for (col in cols) {
  ops[col]<-weighted.mean(sro_df[col],sro_df['FINALWGT'],na.rm = TRUE)
}

as.data.frame(ops) # 'only' 50% are involved in discipline, but 61% report problems to school authority. 63% mentor

# what percent of schools have MOUs or other documented policy agreements
pct_mou<-weighted.mean(sro_df$mou_signed,sro_df$FINALWGT, na.rm=TRUE)
pct_mou # over a third of schools have no written agreement on policies for sro's to follow!

# is there a tradeoff between having an SRO and a mental health professional?
sro_df %>% 
  group_by(sro_present) %>% 
  summarise(pct_mhPro=weighted.mean(mental_health_service,FINALWGT,na.rm=TRUE))
# schools that hire SRO are actually more likely to have mental health professionals on staff

# Do schools with an SRO report a larger share of nonviolent incidence to the police?
sro_df$wgt_nonv_inc<-sro_df$NONVIOINC18*sro_df$FINALWGT
sro_df$wgt_nonv_inc_pol<-sro_df$NONVIOPOL18*sro_df$FINALWGT

report_police<-sro_df %>% 
               group_by(sro_present,mou_signed) %>% 
               summarize(wgt_nonv_inc=sum(wgt_nonv_inc), 
                         wgt_nonv_inc_pol= sum(wgt_nonv_inc_pol)) %>%
               mutate(report_rate=wgt_nonv_inc_pol/wgt_nonv_inc)
report_police #schools with an SRO report nonviolent incidents to the police at about twice the rate


##### Office of Civil Rights Data #####

#import files
skool_chars<-read_csv('../2017-18 Public-Use Files/Data/SCH/CRDC/CSV/School Support.csv')
skool_enroll<-read_csv('../2017-18 Public-Use Files/Data/SCH/CRDC/CSV/Enrollment.csv')
skool_suspension<-read_csv('../2017-18 Public-Use Files/Data/SCH/CRDC/CSV/Suspensions.csv')

############### Examine presense of SRO and Guard based on racial makeup of school

#recode missing values
recoder2<-function(x) {
  if (is.na(x)){
    return(x)}
  else if (x<0){
    return(NA)}
  else {
    return(x)}
}

#merge sro and enrollment data
skool_chars_enroll<-skool_chars %>%
                    left_join(skool_enroll,by=c('COMBOKEY' = 'COMBOKEY')) %>%
                    mutate(any_sro=if_else(SCH_FTESECURITY_LEO>0,1,0),
                           any_guard=if_else(SCH_FTESECURITY_GUA>0,1,0))

# convert missing values to NAs
metrics<-c('SCH_ENR_BL_M','SCH_ENR_BL_F','SCH_ENR_HI_M','SCH_ENR_HI_F','SCH_ENR_WH_M',
           'SCH_ENR_WH_F','TOT_ENR_M','TOT_ENR_F','SCH_FTESECURITY_LEO','SCH_FTESECURITY_GUA')
for (m in metrics){
  skool_chars_enroll[m] <- apply(skool_chars_enroll[m],1,recoder2)
}

#calc total enrollment by race/ethnicity
skool_chars_enroll['black']<-rowSums(skool_chars_enroll[c('SCH_ENR_BL_M','SCH_ENR_BL_F')],na.rm = TRUE)
skool_chars_enroll['hisp']<-rowSums(skool_chars_enroll[c('SCH_ENR_HI_M','SCH_ENR_HI_F')],na.rm = TRUE)
skool_chars_enroll['white']<-rowSums(skool_chars_enroll[c('SCH_ENR_WH_M','SCH_ENR_WH_F')],na.rm = TRUE)
skool_chars_enroll['tot_enroll']<-rowSums(skool_chars_enroll[c('TOT_ENR_M','TOT_ENR_F')],na.rm = TRUE)


# graph relationship between % black student body and presense of guards or SROs. Drop missing enrollment data
skool_pct_b<- skool_chars_enroll %>% 
              drop_na(metrics) %>%
              mutate(pct_black_enroll=black/tot_enroll,
                     pct_black_enroll_sq=pct_black_enroll*pct_black_enroll) 

# function to bin percentages
bucket<- function(x) {
  if (x >=.9) {
    return('90%+')}
  else if (x >= .8) {
    return('80%-90%')}
  else if (x >=.7) {
    return('70%-80%')}
  else if (x >= .6) {
    return('60%-70%')}
  else if (x >= .5){
    return('50%-60%')}
  else if (x >=.4) {
    return('40%-50%')}
  else if (x >= .3) {
    return('30%-40%')}
  else if (x >= .2) {
    return('20%-30%')}
  else if (x > .1) {
    return('10%-20%')}
  else {
    return('0%-10%')}
}

skool_pct_b['pct_b_group'] <-apply(skool_pct_b['pct_black_enroll'],1,bucket)

#graph schools with SRO against black student %. Drop missing SRO data
ggplot(skool_pct_b, aes(x=pct_b_group,y=any_sro)) + 
  stat_summary(fun = mean,geom='bar') + labs(x='School Percent Black Kids', y='Percent of Schools with at least 1 SRO')

#graph schools with guard against black student %. Drop missing guard data
ggplot(skool_pct_b, aes(x=pct_b_group,y=any_guard)) + 
  stat_summary(fun = mean,geom='bar') + labs(x='School Percent Black Kids', y='Percent of Schools with at least 1 Guard')
# Output:
# % of schools with an SRO peaks at 40-50% black and then declines. Schools that are >70% black are least likely to have an SRO
# however, the presense of a non-police guard increases as the percent of the student body that is black increases


####################### Examine suspension rates with and without sros

# merge sro data with suspension data
skool_ces<-skool_chars_enroll %>%
  left_join(skool_suspension, by=c('COMBOKEY' = 'COMBOKEY'))

# convert missign values to NA
suspension_metrics<-c('SCH_DISCWODIS_SINGOOS_BL_M','SCH_DISCWODIS_SINGOOS_BL_F','SCH_DISCWODIS_MULTOOS_BL_M',
                      'SCH_DISCWODIS_MULTOOS_BL_F','SCH_DISCWODIS_ISS_BL_M','SCH_DISCWODIS_ISS_BL_F',
                      'SCH_DISCWODIS_SINGOOS_WH_M','SCH_DISCWODIS_SINGOOS_WH_F','SCH_DISCWODIS_MULTOOS_WH_M',
                      'SCH_DISCWODIS_MULTOOS_WH_F','SCH_DISCWODIS_ISS_WH_M','SCH_DISCWODIS_ISS_WH_F', 'SCH_ENR_BL_M',
                      'SCH_ENR_BL_F','SCH_ENR_WH_M','SCH_ENR_WH_F','SCH_FTESECURITY_LEO')
for (met in suspension_metrics) {
  skool_ces[met]<-apply(skool_ces[met],1,recoder2)
}

# suspensions black and white students
skool_ces['black_ins_suspension']<-rowSums(skool_ces[c('SCH_DISCWODIS_ISS_BL_M','SCH_DISCWODIS_ISS_BL_F')],na.rm = TRUE) 
skool_ces['black_ous_suspension']<-rowSums(skool_ces[c('SCH_DISCWODIS_SINGOOS_BL_M',
                                                       'SCH_DISCWODIS_SINGOOS_BL_F',
                                                       'SCH_DISCWODIS_MULTOOS_BL_M',
                                                       'SCH_DISCWODIS_MULTOOS_BL_F')],na.rm = TRUE)

skool_ces['white_ins_suspension']<-rowSums(skool_ces[c('SCH_DISCWODIS_ISS_WH_M','SCH_DISCWODIS_ISS_WH_F')],na.rm = TRUE) 
skool_ces['white_ous_suspension']<-rowSums(skool_ces[c('SCH_DISCWODIS_SINGOOS_WH_M',
                                                       'SCH_DISCWODIS_SINGOOS_WH_F',
                                                       'SCH_DISCWODIS_MULTOOS_WH_M',
                                                       'SCH_DISCWODIS_MULTOOS_WH_F')],na.rm = TRUE)


# compare black and white suspension rates with and without SROs
suspension_table<-skool_ces %>% 
  drop_na(suspension_metrics) %>%
  group_by(any_sro) %>%
  summarize(blacktot=sum(black),
            black_ins_sus = sum(black_ins_suspension),
            black_ous_sus = sum(black_ous_suspension),
            whitetot=sum(white),
            white_ins_sus=sum(white_ins_suspension),
            white_ous_sus=sum(white_ous_suspension)) %>%
  mutate(black_ins_rate=black_ins_sus/blacktot,
         black_ous_rate=black_ous_sus/blacktot,
         white_ins_rate=white_ins_sus/whitetot,
         white_ous_rate=white_ous_sus/whitetot)
View(suspension_table)
# Output:
# black and white in-school and out-of-school suspension rate is higher in schools with SROs, 
# but the increase is greater for black students

##### Map of SRO prevalence across states #####

states<-st_read('../tl_2019_us_state') %>%
  filter(!STUSPS %in% c('HI','AK','PR'))

state_sro<-skool_chars_enroll %>% 
  group_by(LEA_STATE.x) %>%
  summarize(pct_sro=mean(any_sro,na.rm = TRUE))

states_sro_sf<-states %>%
  inner_join(state_sro, by=c('STUSPS' = 'LEA_STATE.x'))

ggplot(data=states_sro_sf) + 
  geom_sf(aes_string(fill='pct_sro'),size=.1) +
  scale_fill_viridis_c(limits=c(0,1)) +
  labs(fill=' ', title='Percent of Schools with SRO') +
  theme_bw()







