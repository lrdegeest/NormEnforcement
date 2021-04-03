# about -------------------------------------------------------------------
# this file reshapes the targeting data so to estimate contribution norms
# produces "targeting_data.dta"
# author: @lrdegeest

# import data and set up --------------------------------------------------
library(dplyr)
library(reshape2) # note to self: wrote this a few years ago before switching over to tidyverse everything
library(readstata13)
df <- readstata13::read.dta13("data_sanctions_reshape.dta", generate.factors = T)
df$endowment <- factor(df$endowment)
names(df) <- gsub("_","", names(df))  
names(df) <- gsub("([[:alpha:]])([1-4])", "\\1_\\2", names(df)) # underscore precedes sender ID (indnum)
names(df) <- gsub("([1-4])([1-4])", "\\1.\\2", names(df)) # period precedes target (rank)
n = 4 # group size
df <- df[with(df, order(groupid,indnum,period)),] # this sort is SUPER important

# functions ---------------------------------------------------------------
do_recast <- function(targets_data, sender_data=NULL) {
  "
  arguments:  
    - targets_data    target dataframe (contributions, endowments or punishment). Type: wide panel
    - sender_data     sender dataframe, indexed by indnum (session, date, group, endow, cont, sanctioncost). Type: wide panel
  
  returns:
    - data_long       reshaped target dataframe. Type: long panel
  "
  targets_data$groupid <- sender_data$groupid
  targets_data$period <- sender_data$period
  targets_data$indnum <- sender_data$indnum
  data_long <- reshape2::melt(targets_data, id=c("groupid", "indnum", "period"))
  data_long <- data_long[order(data_long$period),]
  colname <- names(targets_data)[1]
  colname <- gsub("([[:punct:]][[:digit:]])", "", colname)
  colname <- paste0("target", "_", colname)
  names(data_long)[names(data_long)=="value"] <- colname
  data_long$variable <- NULL
  data_long <- data_long[with(data_long, order(groupid,indnum, period)),] # same sort as original data
  return(data_long)
}

get_sanctions_indnum <- function(data, id) { # "data" is full imported data ("df")
  "
  arguments:  
    - data    full imported data with all indnums
    - indnum  the sender's ID (indnum)

  returns:
    - df.sender.all   complete reshaped dataframe for sender and all targets 

  example:
    d1 <- get_sanctions_indnum(df,1) # get sanctions data for all indnum==1
  "
  df.indnum <- subset(df, indnum == id) # subset for the indnum
  df.sender <- df.indnum[,1:10] # sender columns
  df.targets <- df.indnum[,11:ncol(df.indnum)] #targets columns
  df.targets <- subset(df.targets, select = grepl(paste0("_",id), names(df.targets))) # select only the indnum columns
  target_list <- list("endow" = df.targets[,1:3], "cont" = df.targets[,4:6], "punish"= df.targets[,7:9])
  target_list_recast <- lapply(target_list, do_recast, sender_data = df.sender) # reshape endow, cont and punish data
  df.sender <- df.sender[rep(1:nrow(df.sender),each=(n-1)),] # extra rows for sender before merge; must come AFTER lapply
  df.sender.all <- cbind(df.sender, target_list_recast[[1]], target_list_recast[[2]], target_list_recast[[3]]) # bind
  df.sender.all <- df.sender.all[, !duplicated(colnames(df.sender.all))] # drop duplicate cols
  df.sender.all$target_rank <- rep(seq(1:3),length(unique(df.sender.all$period))) # add the target rank, always same order (1,2,3)
  return(df.sender.all)
}

get_all_sanctions_data <- function(data) {
  "
  arguments:  
  - data    full imported data with all indnums

  returns:
  - all_data  sender-target dataframe for all senders (indnum=1,2,3,4). Type: panel, three observations per subject 
  "
  data_list <- list()
  for(i in 1:4){
    sender <-  paste0("indnum_",i)
    data_list[[sender]] <- get_sanctions_indnum(data,i)
  }
  all_data <- do.call("rbind", data_list)
  return(all_data)
}

# do it -------------------------------------------------------------------
all_data <- get_all_sanctions_data(df)
# lag sanctioncost
all_data <- all_data %>% group_by(subjectid) %>% 
  mutate(lagsanctioncost = dplyr::lag(sanctioncost, n = 3, default = NA))
readstata13::save.dta13(all_data, file = "targeting_data.dta", data.label = "Targeting data (sender to target)", convert.factors = T)