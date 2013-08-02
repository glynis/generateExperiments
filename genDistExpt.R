# This program takes .csv files of target sentences and fillers 
# and generates 24 lists for 24 subjects.
# Created by Glynis MacMillan on 7/22/2013, based on a script by Cybelle Smith

library(gdata) # allows us to use trim function
library(stringr) # allows us to use str_split
outputfilepath <- "~/Dropbox/Sandcastle/RScripts/Rscripts_distance/";
outputdir <- "Seahorse.exptfiles";
setwd("~/Dropbox/Sandcastle/RScripts/Rscripts_distance/");
fileName <- "seahorse_finalized_stimuli.csv";
targ.outputfilename <- "targ_output_file.csv";
fill.outputfilename <- "fill_output_file.csv";

########################
### TARGET SENTENCES ###
########################

# input data:
sentIn <- read.csv(fileName, header = T)
data <- data.frame(sentIn)

# set up columns
data <- subset(data, select=c(cond.name, blockindex, condcode, Condition, cond.cloze, good.bad, set, Listnum, Word1, Word2, Word3, Word4, Word5, Word6, Word7, Word8, Target, Continuation, Target.Prob)) ###

# trim and concatenate sentence frame

data$Frame <- (paste(trim(data$Word1), trim(data$Word2), trim(data$Word3), trim(data$Word4), trim(data$Word5), trim(paste(trim(data$Word6), trim(data$Word7),sep=" ")), trim(data$Word8), sep = " ", collapse = NULL))

# new data structure
data <- subset(data, select=c(cond.name, condcode, Condition, cond.cloze, good.bad, set, blockindex, Listnum, Frame, Target, Continuation, Target.Prob)) ### prob

# capitalize beginning of sentences
for(i in 1:nrow(data))
{
  data$Frame[i] <- str_replace(data$Frame[i], "^the", "The") 

}

# write to csv file 
write.csv(data, targ.outputfilename, row.names = F)

targ <- data

# end targ script

########################
### FILLER SENTENCES ###
########################

fileName <- "fillers_final.csv"
input <- read.csv(fileName, header = T, stringsAsFactors = FALSE)
data <- data.frame(input)

data$Condition1 <- "g"
data$Condition2 <- "b"

# un-capitalize letters in congruous/incongrous elements!
library(stringr)
data$Congruous <- tolower(data$Congruous)
data$Incongruous <- tolower(data$Incongruous)

# alternate which is congruous/incongruous
congruousList <- subset(data, select=c(index, Condition1, Frame, Congruous, Continuation,Cloze))
incongruousList <- subset(data, select=c(index, Condition2, Frame, Incongruous, Continuation,Cloze))

# insert condcode for fillers
congruousList$condcode <- NA
congruousList[congruousList$index<181,]$condcode <- 5
congruousList[congruousList$index>=181 & congruousList$index<=240 ,]$condcode <- 7

incongruousList$condcode <- NA
incongruousList[incongruousList$index<181,]$condcode <- 6
incongruousList[incongruousList$index>=181 & incongruousList$index<=240 ,]$condcode <- 8

names(congruousList) = names(incongruousList) = c("index","good.bad","Frame","Target","Continuation","Cloze","condcode");
fill <- rbind(congruousList,incongruousList);
fill$List <- c(rep(c(1,2),nrow(fill)/4),rep(c(2,1),nrow(fill)/4));
fill <- fill[order(fill$List),];
fill <- fill[c("List","index","condcode","good.bad","Frame","Target","Continuation","Cloze")]

write.csv(fill, fill.outputfilename, row.names = F)

# end filler script

# now we have two lists: targ (experimental items), fill (filler items)

#############
### MATCH ###
#############

# generate matrix of combinations of lists from targ and fill:
# for two lists, this is most easily done mannually, as below, 
# for 3+ lists see the for loop in megacode_cybelle.R
match <- matrix(nrow = 8, ncol = 2, dimnames = NULL)

match[,1] <- c(1,1,2,2,3,3,4,4)
match[,2] <- rep(c(1,2))

match <- data.frame(match);
names(match) <- c("targ","fill")

###############
### JUMBLE ###
##############

# this randomizes the items in a vector
jumble = function(vec)
{
  return(sample(vec,length(vec),replace=FALSE));
}

#######################
### PSUEDORANDOMIZE ###
#######################

# for each block, go through each row, check using item.type then good.bad
# if too many of one item.type or good.bad in a row, swap out items
pseudorandomize = function(data,maxinarow,conditions){	 # maxinarow <- c(3,3) ; conditions <- c("item.type","good.bad")
  for(block in 1:max(data$blockindex)){ # block==1
    pseudorandom <- FALSE;
    #print(block);
    while(!pseudorandom){
      pseudorandom <- TRUE;
      for (item in 1:(length(data$blockindex[data$blockindex==block])-1)){	# item == 1
        # print(item);
        for (i in 1:length(conditions)){								# i == 1
          condition <- conditions[i]; 	# "item.type"
          curr.maxinarow <- maxinarow[i]; 	# 2
          nextitems <- (item + 1):(item + curr.maxinarow); 	#2:3
          cond <- data[[condition]][data$blockindex==block & data$order==item]; # cond <- data[["item.type"]][data$blockindex==1 & data$order==1] # "fill" or "targ" or "sol" when i==1
          if(length(data[[condition]][data$blockindex==block &	# length(data[["item.type"]][data$blockindex==1 & data$order %in% nextitems & data[["item.type"]]==cond])
            data$order %in% nextitems &				# the first length() can be 0, 1 or 2 when curr.maxinarow==2 (i.e., when condition=="item.type")
            data[[condition]]==cond]) == length(nextitems)){	# length(nextitems) == 2 (when i==1)
            pseudorandom <- FALSE;
            # print(pseudorandom);
            # pick a new order number from a different condition.
            oldplace <- nextitems[1];	# oldplace <- 2
            newplace <- sample(data$order[data$blockindex==block & data[[condition]] != cond],1);	# newplace <- sample(data$order[data$blockindex==1 & data[["item.type"]]!="fill"],1) (it depends on the first row)
			# newplace is sampled (once) from data$order with criteria about blockindex and "item.type"
            data$order[data$blockindex==block & data$order==oldplace]<- newplace;
            data$order[data$blockindex==block & data$order==newplace & data[[condition]]!=cond] <- oldplace; # & data[[condition]]!=cond is redundant because newplace is always sampled with this criterion in place data[["item.type"]]!="fill"
            data <- data[order(data$blockindex,data$order),]; # order the rows within the blocks with new data$order
          } # end if
        } # end for loop
      } # end for loop
    } # end while loop
  } # end for loop
  return(data);
}

###########################
### GENERATE SUBJ FILES ###
###########################

generate.subj.files <- function(subj,items){
  megalist <- getmegalist(items);
  writefiles(subj,megalist);
}

####################
### GET MEGALIST ###
####################

getmegalist <- function(items){
  nblocks <- 5;
  item.type <- c();
  for (item in names(items)){
    item.type <- c(item.type,rep(item,nrow(items[[item]]))); # repeat "fill" for however many rows fill exists
  }
  megalist <- data.frame(item.type);
  for (colname in c("List","block","order","blockindex","index","condcode","Condition","good.bad","cloze","Frame","Target","Continuation")){
  col <- c(); # initializing as blank
    for (item in names(items)){
      if(colname %in% names(items[[item]])){
        col <- c(col,as.character(items[[item]][[colname]]));
      } else {
        col <- c(col,rep(NA,nrow(items[[item]])));
      }
    }
    megalist[[colname]] <- col; # col becomes the column in megalist
  }
  megalist$condcode <- c(items$targ$condcode, items$fill$condcode);
  megalist$index <- c(items$targ$set,items$fill$index);
  megalist$List <- c(items$targ$Listnum,items$fill$List);
  megalist$cloze <- c(items$targ$Target.Prob,items$fill$Cloze);
  megalist$cloze[megalist$good.bad=="b" & megalist$item.type=="fill"] <- NA;
  megalist$blockindex <- c(items$targ$blockindex,getblocks(megalist$blockindex[megalist$item.type=="fill"],nblocks));
  megalist <- randomize(megalist,nblocks);
  return(megalist);
}

##################
### GET BLOCKS ###
##################

# n = nblocks, or length(col)/nblocks, depending on what you want to do.
getblocks <- function(col,n){
  return(rep(c(1:n),length(col)/n));
}

##################
### RANDOMIZE ###
#################

# uses jumble and pseudorandomize to randomize blocks and pseudorandomize items within blocks

randomize <- function(megalist,nblocks){
  maxinarow <- c(2,4); # maximum 2 of same item type in a row and 3 of same felicity ("good" or "bad") in a row
  blockorder <- jumble(1:nblocks);
  for(i in 1:nblocks){
    megalist$block[megalist$blockindex==i] <- blockorder[i];
  }
  megalist$order <- jumble(1:nrow(megalist));
  megalist <- megalist[order(megalist$block,megalist$order),]
  megalist$order <- getblocks(megalist$order,nrow(megalist)/nblocks);
  megalist <- pseudorandomize(megalist,maxinarow,c("item.type","good.bad")); #,"good.bad"  actually, item.type ensures Condition won't occur more than 3 times in a row
  return(megalist);
}

####################
### WRITE FILES ###
###################

writefiles <- function(subj,megalist){
  foldername <- str_c("Subject",subj);
  dir.create(foldername);
  writeexptfile(foldername,max(megalist$block));
  currdir <- getwd();
#   print(currdir)
  setwd(str_c(currdir,"/",foldername));
  write.csv(megalist,"stimulus_list.csv");
  for (block in 1:max(megalist$block)){
    writeblock(megalist,block);
  }
  setwd(currdir);
}

###################
### WRITE BLOCK ###
###################

writeblock <- function(megalist,block){
  question <- "Is this sentence plausible?";
  block.df <- megalist[megalist$block==block,];
  blockfilename <- str_c("block",block,".txt");
  for (i in 1:nrow(block.df)){
    if(block.df$item.type[i]=="targ"){
      addtarg(block.df[i,],blockfilename,question);
    }
    if(block.df$item.type[i]=="fill"){
#       print("ok")
      addfill(block.df[i,],blockfilename,question);
    }
  }
}

################
### ADD TARG ###
################

# adds triggers to target sentences

addtarg <- function(row,blockfilename,question){
  conditioncode <- row$condcode; 
  gbcode <- getgbcode(row);
  qtrigger = 100 + conditioncode*10 + gbcode;
  compq <- str_c("? ",qtrigger," \"",question,"\"\n");
  frame <- str_split(row$Frame," ");
  continuation <- str_split(row$Continuation," ");
  triggers <- c(rep(1,(length(frame[[1]]) + length(continuation[[1]]))),conditioncode*10);
  if (conditioncode <=2){
  indices <- c((length(frame[[1]]) - 4):(length(frame[[1]]) + 4)); # 4 triggers before target word to 3 after target word
  nTriggers <- length(triggers);
  for (j in 1:length(indices)){
    triggers[indices[j]] <- 10*conditioncode + j;
  } # end if
} else if (conditioncode >=3) {   
  indices1 <- c((length(frame[[1]]) - 7):(length(frame[[1]]) - 4)); # places triggers before PP
  indices2 <- c(length(frame[[1]]):(length(frame[[1]]) + 4)); # places triggers from 'had' on
  nTriggers <- length(triggers);
  for (j in 1:length(indices1)){
    triggers[indices1[j]] <- 10*conditioncode + j;
  }
  for (j in 1:length(indices2)){
    triggers[indices2[j]] <- 10*conditioncode + (j+4);
  }
} # end else if
  triggers <- triggers[1:nTriggers];                   # not necessary?
  triggers[length(triggers)] <- conditioncode*10;      # ensures sentence final trigger is conditioncode*10
  words <- c(frame[[1]],row$Target,continuation[[1]]); 
  stim <- c(interweveT(words,triggers),compq);         # interweves triggers with words, adds comprehension question after each item
  cat(stim,file=blockfilename,append=T);               # append will add new rows to existing txt 
}                                                      # requires you to delete all previous subj files before running

### test addtarg ###                                   # replace lines 263 -- 269 to troubleshoot
#   conditioncode <- 3
#   frame1.text <- "Sally knew which doctor the patient in the coat had"
#   continuation.text <- "in the hospital." 
#   frame <- str_split(frame1.text," ");
#   continuation <- str_split(continuation.text," ");
#   triggers <- c(rep(1,(length(frame[[1]]) + length(continuation[[1]]))),conditioncode*10);
### end test ###


################
### ADD FILL ###
################

addfill <- function(row,blockfilename,question){
  conditioncode <- row$condcode
  gbcode <- getgbcode(row);
  qtrigger = 100 + conditioncode*10 + gbcode; 
  compq <- str_c("? ",qtrigger," \"",question,"\"\n"); ### add this
#   compq <- str_c("? ",conditioncode," \"",question,"\"\n");
  frame <- str_split(row$Frame," ");
  continuation <- str_split(row$Continuation," ");
  triggers <- c(rep(1,(length(frame[[1]]) + length(continuation[[1]]))),conditioncode*10);
  indices <- c((length(frame[[1]]) - 2):(length(frame[[1]]) + 4)); # 4 triggers before target word to 3 after target word
  nTriggers <- length(triggers);
  for (j in 1:length(indices)){
    triggers[indices[j]] <- 10*conditioncode + j + 2;
  }
  triggers <- triggers[1:nTriggers];                         
  triggers[length(triggers)] <- conditioncode*10;             
  words <- c(frame[[1]],row$Target,continuation[[1]]);
  stim <- c(interweveT(words,triggers),compq);
  cat(stim,file=blockfilename,append=T);
}

##########################
### INTERWEVE TRIGGERS ###
##########################

interweveT <- function(word,trigger){
  matrix <- t(as.matrix(cbind(as.character(word),trigger)));    # column bind word and trigger, and creates a matrix. t transposes it
  dims <- dim(matrix);                                          # gets dimensions of matrix
  dim(matrix) <- c((dims[1] * dims[2]),1);                      # alters dimensions of matrix
  return(matrix);
}

########################
### GOOD/BAD TRIGGER ###
########################

getgbcode <-function(row){
  if(row$good.bad=="g") return(1);
  if(row$good.bad=="b") return(0);
}

########################
### WRITE EXPT FILES ###
########################

writeexptfile <- function(foldername,nblocks){
  exptfile <- "Slides/introduction.txt
Slides/introduction2.txt
Slides/practice_sent1.txt
Slides/practice_slide1.txt
Slides/practice_sent2.txt
Slides/practice_slide2.txt
Slides/practice_sent3.txt
Slides/practice_slide3.txt
Slides/practice_sent4.txt
Slides/practice_slide4.txt
Slides/practice_sent5.txt
Slides/practice_slide5.txt
Slides/practice_sent6.txt
Slides/practice_slide6.txt
Slides/practice_end.txt
";
  for (block in 1:nblocks){
    exptfile <- str_c(exptfile,str_c(foldername,"/block",block,".txt\n"));
    if(block<nblocks){
      exptfile <- str_c(exptfile,str_c("Slides/break.txt\n"));
    }
  }
  exptfile <- str_c(exptfile,"Slides/conclusion.txt\n");
  cat(exptfile,file=str_c(foldername,".expt"));
}

#########################
### CREATE OUTPUT DIR ###
#########################

setwd(outputfilepath);
dir.create(outputdir);
setwd(str_c(outputfilepath,"/",outputdir));

#########################
### CATALYST FOR LOOP ###
#########################

# for each subject, pick the lists, then execute larger function
# requires you to delete all previous subj files before running
for (cycle in 1:3){
  for (listseq in 1:8){
#     cycle = 1;
#     listseq = 1;
    subj=8*(cycle-1)+listseq;
    curr.lists <- match[listseq,];
    items <- list();
    items[["targ"]] <- targ[targ$List==curr.lists$targ,];
    items[["fill"]] <- fill[fill$List==curr.lists$fill,];
    generate.subj.files(subj,items);
  }
}

write.csv(match,"list_combinations.csv");
