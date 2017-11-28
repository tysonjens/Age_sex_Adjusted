##this program reads in a member months file and a referrals file, groups
##each by physician, then calculates age-sex adjusted referrals rates for
##the organization and then provides a distribution of provider referrals 
##rates.

setwd("C:/Users/TWard/Desktop/Tyson - Work/AGE_SEX_ADJUST_STUDY")

## 1 read both files into memory
referrals <- read.csv("referrals.csv", header=TRUE, sep="|")
mms <- read.csv("member_months.csv", header=TRUE, sep = "|")

## 2 subset referrals to only those made by a PCP
uniquepcps <- unique(mms[c("HCP_PCP_NAME", "HCP_PCP_KEY", "HCP_SITE_NAME", "REGION_NAME")])
referrals <- merge(referrals, uniquepcps, by.x='REFERRING_PROV_KEY', by.y='HCP_PCP_KEY')

## 3 load dplyr library for grouping and summarizing
library(dplyr)

##4 for mms file, create age and sex buckets
mms$age_sex <- paste(mms$AGE_GROUP, mms$SEX, sep="_")

##  5group mms by age_sex and summarize in mms_age_sex
mms_gpby_age_sex <- group_by(mms, age_sex)
mms_age_sex <- summarize(mms_gpby_age_sex, members = n())

##6 now we want to merge mms into referrals so we can calculate Number of
##6 referrals by age_sex group
merge_refs_mms <- merge(referrals, mms, by.x='REGISTRANT_ID', by.y = 'ï..REGISTRANT_ID', all.x=TRUE)

##7 now we group merge_refs_mms by age_sex and summarize in counts_by_age_sex
merge_refs_mms_gpby_as <- group_by(merge_refs_mms, age_sex)
countsby_age_sex <- summarize(merge_refs_mms_gpby_as, referrals = n())

##8 now we merge countsby_age_sex and mms_age_sex and calc overall rates

market_age_sex_summary <- merge(countsby_age_sex, mms_age_sex, by.x='age_sex', by.y = 'age_sex')
market_age_sex_summary$rate <- market_age_sex_summary$referrals / market_age_sex_summary$members

##Now we'd like to calculate expected referrals for each PCP
##First, we group mms by PCP, then by age_sex
mms_gpby_PCP_as <- group_by(mms, HCP_PCP_KEY, age_sex)
mms_grby_PCP_as <- summarize(mms_gpby_PCP_as, count = n())

## 10 now we merge mms_grby_PCP_as with market_age_sex_summary
expmms_by_PCP <- merge(mms_grby_PCP_as, market_age_sex_summary, by.x='age_sex', by.y='age_sex', all.x = TRUE)
expmms_by_PCP$exprefs <- expmms_by_PCP$count * expmms_by_PCP$rate

##now we group expmms_by_PCP by HCP_PCP_KEY and sum to get exp referrals
expmms_by_PCP_grpd <- group_by(expmms_by_PCP, HCP_PCP_KEY)
expmms_by_PCP_grpd <- summarize(expmms_by_PCP_grpd, exprefs = sum(exprefs))

##now we group referrals by HCP_PCP_KEY and summarize to find actual referrals
refs_grpby_PCP <- group_by(referrals, REFERRING_PROV_KEY)
refs_grpby_PCP <- summarize(refs_grpby_PCP, actrefs=n())


##get panel sizes
mms_gpby_hcppcp <- group_by(mms, HCP_PCP_KEY)
mms_gpby_hcppcp <- summarize(mms_gpby_hcppcp, members = n())

##merge mms_gpby_hcppcp, refs_grpby_PCP, expmms_by_PCP_grpd
summarytable <- merge(mms_gpby_hcppcp, expmms_by_PCP_grpd, by.x="HCP_PCP_KEY", by.y="HCP_PCP_KEY")
summarytable <- merge(summarytable, refs_grpby_PCP, by.x="HCP_PCP_KEY", by.y="REFERRING_PROV_KEY")
summarytable <- merge(summarytable, uniquepcps, by="HCP_PCP_KEY")
summarytable <- summarytable[!duplicated(summarytable[1]),]
summarytable$index <- summarytable$actrefs / summarytable$exprefs
summarytable$exprefs_per_member <- summarytable$exprefs / summarytable$members
summarytable$adjpanel <- (summarytable$exprefs/(summarytable$members * .439))*summarytable$members

# 15 drop pediatricians
summarytable <- summarytable[(summarytable$exprefs_per_member > .15),]
#drop pcps with panels small than 200 hundred
summarytable <- summarytable[(summarytable$members > 50),]
##normalize index
medindex <- median(summarytable$index, na.rm = TRUE)
sdindex <- sd(summarytable$index, na.rm = TRUE)
summarytable$index <- (summarytable$index - medindex) / sdindex

summarytable <- arrange(summarytable, REGION_NAME, HCP_SITE_NAME,desc(index))

cor(members, actrefs, use = "complete.obs", method = "pearson")
cor(members, exprefs, use = "complete.obs", method = "pearson")

save(market_age_sex_summary, file = "marketreferralrates")
save(summarytable, file = "summarytable")


## just making a little change to test github




