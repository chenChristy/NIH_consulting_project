library(janitor)
library(stringr)
library(sqldf)
library(dplyr)
library(tidystringdist)
library(ggplot2)
grant<-read.csv("NIGMS 2017 RPG Grant Data.csv")
pub<-read.csv("NIGMS 2017 RPG Pub Data.csv")



#2017
data_clean <- function(grant_data,pub_data) {
  #data preprocessing
  grant_data<-clean_names(grant_data)
  pub_data<-clean_names(pub_data)
  grant_data$clean_project_number <- substr(grant_data$project_number,2,12)
  
  #subset data
  use_columns_grant <- c("clean_project_number", "contact_pi_project_leader", "organization_name","contact_pi_person_id")
  grant_data_subset<-grant_data[use_columns_grant]
  grant_data_subset_unique<-unique(grant_data_subset)
  use_columns_pub <- c("core_project_number", "pmid")
  pub_data_subset<-pub_data[use_columns_pub]
  pub_data_subset_unique<-unique(pub_data_subset)
  colnames(pub_data_subset_unique)[1]<-("clean_project_number")

  #merge grant and pub
  merge_grant_pub<-merge(grant_data_subset_unique, pub_data_subset_unique, by="clean_project_number")
  
  #subset scopus
  scopus_use_column<-c("authid", "authname","surname", "given-name" ,"initials" ,"affilname","pubmed-id")
  scopus_out_dfsubset<-scopus_out_df[scopus_use_column]
  colnames(scopus_out_dfsubset) <- c("scopus_id", "scopus_author_name","scopus_surname", "scopus_given_name" ,"scopus_initials" ,"scopus_affilname","pmid")
  
  #merge nih rpg data and scopus
  match_use_dataset<-merge(scopus_out_dfsubset,merge_grant_pub, by="pmid")
  
  rexp <- "^(\\w+)\\s?(.*)$"
  match_use_dataset<- data.frame(match_use_dataset$pmid, 
                                 match_use_dataset$clean_project_number,
                                 match_use_dataset$contact_pi_person_id,
                                 PPID_last_name=sub(rexp,"\\1",match_use_dataset$contact_pi_project_leader), 
                                 PPID_First_name=sub(rexp,"\\2",match_use_dataset$contact_pi_project_leader),
                                 match_use_dataset$organization_name,
                                 match_use_dataset$scopus_id, 
                                 match_use_dataset$scopus_author_name,
                                 match_use_dataset$scopus_surname, 
                                 match_use_dataset$scopus_given_name,
                                 match_use_dataset$scopus_initials,
                                 match_use_dataset$scopus_affilname
                                 )

  match_use_dataset$PPID_First_name<-substr(match_use_dataset$PPID_First_name, 2, 20)
  
  colnames(match_use_dataset)[1:12]<-c( "pmid","clean_project_number","contact_pi_person_id", "PPID_last_name", "PPID_First_name","organization_name","scopus_id",      "scopus_author_name" ,"scopus_surname", 
                                        "scopus_given_name","scopus_initials"  ,  "scopus_affilname"   )

  match_use_dataset <<- match_use_dataset %>% 
    mutate_if(is.factor, as.character)%>%
    mutate(PPID_last_name = iconv(gsub(" ", "",PPID_last_name),from="US-ASCII//TRANSLIT", to='UTF-8')) %>%
    mutate(PPID_First_name = iconv(gsub("\\.","",gsub(" ","",PPID_First_name)),from="US-ASCII//TRANSLIT", to='UTF-8'))%>%
    mutate(scopus_surname = iconv(toupper(gsub(" ","",scopus_surname)),from="US-ASCII//TRANSLIT", to='UTF-8'))%>%
    mutate(scopus_given_name = iconv(toupper(gsub("\\.","",gsub(" ","",scopus_given_name))), from="US-ASCII//TRANSLIT", to='UTF-8'))%>%
    mutate(organization_name = iconv(toupper(gsub(" ","",organization_name)),from="US-ASCII//TRANSLIT", to='UTF-8'))%>%
    mutate(scopus_affilname = iconv(toupper(gsub(" ","",scopus_affilname)),from="US-ASCII//TRANSLIT", to='UTF-8'))
}

data_clean(grant, pub)























data_join_and_match <- function(match_use_dataset){
  matched_df <<- sqldf('select * from match_use_dataset where "PPID_last_name" = "scopus_surname" and "PPID_First_name" = "scopus_given_name"')
  matched_df <<- unique(matched_df)
  unmatched_df <<- unique(sqldf('select * from match_use_dataset where "PPID_last_name" != "scopus_surname" or "PPID_First_name" != "scopus_given_name"'))
  unmatched_firstname_df <<- unique(sqldf('select * from match_use_dataset where "PPID_last_name" = "scopus_surname" and "PPID_First_name" != "scopus_given_name"'))
  unmatched_lastname_df <<- unique(sqldf('select * from match_use_dataset where "PPID_last_name" != "scopus_surname" and "PPID_First_name" = "scopus_given_name"'))
  unmatched_firstname_df_add_org <<-unique(sqldf('select * from match_use_dataset where "PPID_last_name" = "scopus_surname" and "PPID_First_name" != "scopus_given_name" and "organization_name" = "scopus_affilname"'))
  matched_rate <- length(unique(matched_df$contact_pi_person_id))/length(unique(match_use_dataset$contact_pi_person_id))
  sprintf("%f of project leader are Successful matched,others need further match", matched_rate)
}


data_join_and_match(match_use_dataset)


data_analysis <- function(unmatched_firstname_df){
  org_df <- data.frame(nih_org = unmatched_firstname_df$organization_name, scopus_org = unmatched_firstname_df$scopus_affilname)
  org_score <- tidy_stringdist(org_df, v1 = nih_org,v2 = scopus_org, method = "osa")
  unmatched_org_firstname_df<<-cbind(unmatched_firstname_df, org_score)
  
  success_org <- subset(unmatched_org_firstname_df,osa<=13)
  success_org<- success_org[,1:14]
  
  not_success_org<<-subset(unmatched_org_firstname_df,(osa>13)|(osa=NA))
  not_success_org<-not_success_org[,1:14]


  success_org_firstname_df <- data.frame(nih_firstname =success_org$PPID_First_name, scopus_firstname = success_org$scopus_given_name)
  success_org_firstname_score <- tidy_stringdist(success_org_firstname_df, v1 = nih_firstname,v2 = scopus_firstname, method = c("cosine", "jaccard", "jw"))
  matched_org_firstname_df<<-cbind(success_org, success_org_firstname_score)
  
  not_success_org_firstname_df <<- data.frame(nih_firstname =not_success_org$PPID_First_name, scopus_firstname =not_success_org$scopus_given_name)
  not_success_org_firstname_score <<- tidy_stringdist(not_success_org_firstname_df, v1 = nih_firstname,v2 = scopus_firstname, method = c("cosine", "jaccard", "jw","osa"))
  unmatched_org_firstname_df<<- cbind(not_success_org, not_success_org_firstname_score)
  
  
  
}
data_analysis(unmatched_firstname_df)




data_matching

#already matched first name last name
matched_subset<-unique(matched_df[c("contact_pi_person_id","PPID_last_name","PPID_First_name","scopus_id","scopus_author_name")])
nrow(unique(matched_subset))


#successful match with last name matched

matched_success_org_and_name <- subset(matched_org_firstname_df,(cosine<= 0.53)&(jaccard<=0.5)&(jw <= 0.276))
matched_success_org_and_name <- matched_success_org_and_name[,1:14]
nrow(matched_success_org_and_name)

matched_wrong_org_success_name <- subset(unmatched_org_firstname_df,(cosine<0.53)&(jaccard<0.5)&(jw<=0.276))
matched_wrong_org_success_name <- matched_wrong_org_success_name[,1:14]
nrow(matched_wrong_org_success_name)

matched_wrong_org_success_name_subset <-unique(matched_wrong_org_success_name[c("contact_pi_person_id","PPID_last_name","PPID_First_name","scopus_id","scopus_author_name")])
matched_success_org_and_name_subset <-unique(matched_success_org_and_name[c("contact_pi_person_id","PPID_last_name","PPID_First_name","scopus_id","scopus_author_name")])

nrow(unique(matched_wrong_org_success_name_subset))
nrow(unique(matched_success_org_and_name_subset))


#successful match with last name 3 letter and first name 3 letter
unmatched_df_print$PPID_last_name_3<-substr(unmatched_df_print$PPID_last_name, 0, 3)
unmatched_df_print$PPID_First_name_3<-substr(unmatched_df_print$PPID_First_name, 0, 3)


unmatched_df_print$scopus_surname_3<-substr(unmatched_df_print$scopus_surname, 0, 3)
unmatched_df_print$scopus_given_name_3<-substr(unmatched_df_print$scopus_given_name, 0, 3)

head(unmatched_df_print)
matched_df_3 <- sqldf('select * from unmatched_df_print where "PPID_last_name_3" = "scopus_surname_3" and "PPID_First_name_3" = "scopus_given_name_3"')
nrow(matched_df_3)

matched_df_3 <- unique(matched_df_3[c("contact_pi_person_id","PPID_last_name","PPID_First_name","scopus_id","scopus_author_name")])
nrow(matched_df_3)



#successful match with last name 3 letter
matched_df_3_last<- sqldf('select * from unmatched_df_print where "PPID_last_name_3" = "scopus_surname_3"and "PPID_First_name_3" != "scopus_given_name_3"')
nrow(matched_df_3_last)

matched_df_3_last <- unique(matched_df_3_last[c("contact_pi_person_id","PPID_last_name","PPID_First_name","scopus_id","scopus_author_name")])
nrow(matched_df_3_last)




###success lname fname both not matched through excel
unmatched_flname <- read.csv("/Users/ricky/Desktop/pract_project/abcd.csv")


unmatched_flname_perfect <- subset(unmatched_flname, unmatched_flname$Similarity == 1)
unmatched_flname_perfect <- unique(unmatched_flname_perfect[c("contact_pi_person_id","PPID_last_name","PPID_First_name","scopus_id","scopus_author_name")])
nrow(unmatched_flname_perfect)




#overall performance
match_outcome <- unique(rbind(matched_subset,matched_wrong_org_success_name_subset,matched_success_org_and_name_subset,matched_df_3,matched_df_3_last,unmatched_flname_perfect))
nrow(match_outcome)

matched_success_percentage <- unique(sqldf('select * from grant_subset g left join match_outcome m on g.ppid = m.contact_pi_person_id'))

nrow(matched_success_percentage)
a<-length(matched_success_percentage$ppid)
b<-length(matched_success_percentage$contact_pi_person_id)-sum(is.na(matched_success_percentage$contact_pi_person_id))
b/a


#still not match
matched_success_percentage
still_unmatch <- subset(matched_success_percentage, is.na(matched_success_percentage$scopus_id))
still_unmatch_ppid <- still_unmatch$ppid

still_unmatch_subset <- unique(subset(match_use_dataset, match_use_dataset$contact_pi_person_id %in% still_unmatch_ppid))

head(match_use_dataset)


#unmatchrate
#getinfo <- function(ppid){
#  matchoutcome <- subset(successful_match, successful_match$contact_pi_person_id == ppid )
#  if (nrow(matchoutcome) == 0){
#    print("Sorry, match not found")
#  }
#  else{
#    return(matchoutcome)
#  }
#}

#getinfo(1231231)
#unmatchrate

#head(successful_match)
#numberofscopus <- sqldf("select contact_pi_person_id as ppid, count(scopus_id) as num from successful_match group by contact_pi_person_id")
#ggplot(df, aes(x=weight)) + geom_histogram()
