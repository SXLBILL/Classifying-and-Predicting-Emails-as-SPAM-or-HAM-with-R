load("E:\\U course\\TrainingMessages.rda")
trm=trainMessages

# 1
isSpam<-function(i){
  # whether mail is Spam (TRUE) or Ham (FALSE)
  grepl("[S|s]pam",names(trm)[i])
}


# 2
isRe<-function(i){
  # if the string Re: appears as the first word in the subject of the message 
  if(sum(grep("^Subject$", names(trm[[i]]$header)))==0) return(0/0)
  
  j=grep("^Subject$", names(trm[[i]]$header))
  num=unlist(gregexpr("^[R][e]",trm[[i]]$header[j]))
  num[1]>0
}


# 3
numLinesInBody<-function(i){
  # a count of the number of lines in the body of the email message 
    length(trm[[i]]$body[which(trm[[i]]$body!="")])
}


# 4
bodyCharacterCount<-function(i){
  # the number of characters in the body of the email message 
   totalnum=unlist(gregexpr("[A-Z|a-z]",trm[[i]]$body))
   total=length(totalnum[totalnum>0])
   return(total)
} 


# 5
replyUnderline<-function(i){
  # whether the Reply-To field in the header has an underline and numbers/letters 
  message=trm[[i]]$header[grepl("^[R|r]eply-[T|t]o$", names(trm[[i]]$header))]
  if(sum(grep("^[R|r]eply-[T|t]o$", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    grepl("_[A-Z|a-z|0-9]", message)
} 


# 6
subjectExclamationCount<- function(i){
  # a count of the number of exclamation marks (!) in the subject of the message 
  if(sum(grep("^Subject$", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    j=grep("^Subject$", names(trm[[i]]$header))
    num=unlist(gregexpr("[!]",trm[[i]]$header[j]))
    num=length(num[num>0])
    return(num)
}


# 7
subjectQuestCount<-function(i){
  # the number of question marks in the subject
  if(sum(grep("^Subject$", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    j=grep("Subject", names(trm[[i]]$header))
    num=unlist(gregexpr("[?]",trm[[i]]$header[j]))
    num=length(num[num>0])
    return(num)
} 


# 8
numAttachments<-function(i){
  # the number of attachments in the message. 
  if(sum(grep("attachments", names(trm[[i]])))==0) return(0/0)
  
  else
  length(names(trm[[i]]$attachments))
} 


# 9
priority<-function(i){
  # whether the message's header had an X-Priority or X-Msmail-Priority that was set to high 
  if(sum(grep("X-Priority|X-Msmail-Priority", names(trm[[i]]$header)))==0) return(0/0)
   
  else
    k=grep("X-Msmail-Priority", names(trm[[i]]$header))
    j=grep("X-Priority", names(trm[[i]]$header))
    if(sum(j)!=0 & sum(k)!=0)
    {
      return(grepl("1|2",trm[[i]]$header[j])| grepl("High",trm[[i]]$header[k]))
    }
    
    else 
      l=grep("X-Priority|X-Msmail-Priority", names(trm[[i]]$header))
      return(grepl("1|2|High",trm[[i]]$header[l]))      
}


# 10
numRecipients<-function(i){
  # the number of recipients in the To, Cc fields
  if(sum(grep("To|Cc", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    k=grep("To", names(trm[[i]]$header))
    j=grep("Cc", names(trm[[i]]$header))
    
    if(sum(j)!=0 & sum(k)!=0)
    {
      return(length(gregexpr("@",trm[[i]]$header[j])[[1]]>0)+length(gregexpr("@",trm[[i]]$header[k])[[1]]>0))
    }
    
    else 
      l=grep("To|Cc", names(trm[[i]]$header))
      return(length(gregexpr("@",trm[[i]]$header[l])[[1]]>0))
} 


# 11
percentCapitals<-function(i){
  # the percentage of the characters in the body of the email that are upper case
  totalnum=unlist(gregexpr("[A-Z|a-z]",trm[[i]]$body))
  total=length(totalnum[totalnum>0])
  uppernum=unlist(gregexpr("[A-Z]",trm[[i]]$body))
  upper=length(uppernum[uppernum>0])
  return(upper/total)
}


# 12
isInReplyTo<-function(i){
  # whether the header of the message has an In-Reply-To field.
  sum(grep("^In-Reply-To$", names(trm[[i]]$header)))!=0
} 


# 13
subjectPunctuationCheck<-function(i){
  # whether the subject has punctuation or digits surrounded by characters
  if(sum(grep("^Subject$", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    j=grep("Subject", names(trm[[i]]$header))
    num=gregexpr("[a-z|A-Z][0-9|[:punct:]][a-z|A-Z]",trm[[i]]$header[j])[[1]]
    length(num[num>0])>0
}


# 14
hourSent<-function(i){
  # the hour in the day the mail was sent (0 -- 23) 
  if(sum(grep("^Date$", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    j=grep("^Date$", names(trm[[i]]$header))
    num=gregexpr(":", trm[[i]]$header[j])[[1]][1]
    substr(trm[[i]]$header[j],num-2,num-1)
} 


# 15
subjectSpamWords<-function(i){
  # whether the subject contains one of the following phrases: viagra, pounds, free, weight, guarantee, millions, dollars, credit, risk, prescription, generic, drug, money back, credit card.
  if(sum(grep("^Subject$", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    j=grep("Subject", names(trm[[i]]$header))
    sum(grep("viagra|pounds|free|weight|guarantee|millions|dollars|credit|risk|prescription|generic|drug|money back|credit card",trm[[i]]$header[j]))>0
}


# 16
percentSubjectBlanks<-function(i){
  # the percentage of blanks in the subject
  if(sum(grep("^Subject$", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    j=grep("Subject", names(trm[[i]]$header))
    totalnum=unlist(gregexpr(".", trm[[i]]$header[j]))
    total=length(totalnum[totalnum>0])
    blanknum=unlist(gregexpr(" ", trm[[i]]$header[j]))
    blank=length(blanknum[blanknum>0])
    return(blank/total)
}


# 17
isYelling<-function(i){
  # whether the Subject of the mail is in capital letters
  if(sum(grep("^Subject$", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    j=grep("^Subject$", names(trm[[i]]$header))
    sum(grep("[a-z]",trm[[i]]$header[j]))==0
} 


# 18
isPGPsigned<-function(i){
  # indicates whether the mail was digitally signed (e.g. using PGP or GPG) 
  if(sum(grep("^[C|c]ontent-[T|t]ype$", names(trm[[i]]$header)))==0) return(0/0)
  
  else
    j=grep("^[C|c]ontent-[T|t]ype$", names(trm[[i]]$header))
    num=unlist(gregexpr("[S|s]igned", trm[[i]]$header[j]))
    length(num[num>0])>0
} 


# 19
numDollarSigns<-function(i){
  # the number of dollar signs in the body of the message 
  num=unlist(gregexpr("[$]", trm[[i]]$body))
  length(num[num>0])
}


# 20
isDear<-function(i){
  # whether the message body contains a form of the introduction Dear ...
  num=unlist(gregexpr("^Dear", trm[[i]]$body))
  length(num[num>0])>0
}


sapply(1:length(names(trm)), function(x) isSpam(x))->V1

sapply(1:length(names(trm)), function(x) isRe(x))->V2

sapply(1:length(names(trm)), function(x) numLinesInBody(x))->V3

sapply(1:length(names(trm)), function(x) bodyCharacterCount(x))->V4

sapply(1:length(names(trm)), function(x) replyUnderline(x))->V5

sapply(1:length(names(trm)), function(x) subjectExclamationCount(x))->V6

sapply(1:length(names(trm)), function(x) subjectQuestCount(x))->V7

sapply(1:length(names(trm)), function(x) numAttachments(x))->V8

sapply(1:length(names(trm)), function(x) priority(x))->V9

sapply(1:length(names(trm)), function(x) numRecipients(x))->V10

sapply(1:length(names(trm)), function(x) percentCapitals(x))->V11

sapply(1:length(names(trm)), function(x) isInReplyTo(x))->V12

sapply(1:length(names(trm)), function(x) subjectPunctuationCheck(x))->V13

sapply(1:length(names(trm)), function(x) hourSent(x))->V14

sapply(1:length(names(trm)), function(x) subjectSpamWords(x))->V15

sapply(1:length(names(trm)), function(x) percentSubjectBlanks(x))->V16

sapply(1:length(names(trm)), function(x) isYelling(x))->V17

sapply(1:length(names(trm)), function(x) isPGPsigned(x))->V18

sapply(1:length(names(trm)), function(x) numDollarSigns(x))->V19

sapply(1:length(names(trm)), function(x) isDear(x))->V20

checkv=data.frame(names(trm), V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20)

save(checkv, file = "20Variables.rda")()

variablename=c("isSpam","isRe","numLinesInBody","bodyCharacterCount","replyUnderline","subjectExclamationCount","subjectQuestCount","numAttachments","priority","numRecipients",
    "percentCapitals","isInReplyTo","subjectPunctuationCheck","hourSent","subjectSpamWords","percentSubjectBlanks","isYelling","isPGPsigned","numDollarSigns","isDear")

# ---------- Test Methods---------------

# method 1
picture<-function(i){
  # draw scatterplot of variables
  numHAM=length(which(V1==FALSE))
  numSPAM=length(which(V1==TRUE))
  V=list(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20)
  plot(V[[i]], pch=c(rep(1,numHAM), rep(4,numSPAM)), col=c(rep('blue',numHAM), rep('red',numSpam)), xlab="index of email", ylab=" ", main=variablename[i])
  legend("left", legend=c("HAM","SPAM"), col=c("blue","red"), pch=c(1,4))
}
picture(1)
picture(2)
#...
picture(20)


# method 2
ratio<-function(i){
  # compute the ratio of TRUE in HAM and SPAM
  numHAM=length(which(V1==FALSE))
  numSPAM=length(which(V1==TRUE))
  ham=V[[i]][1:numHAM]
  spam=V[[i]][(numHAM+1):(numHAM+numSPAM)]
  # THE TRUE Number in HAM
  tnham=length(ham[which(ham==TRUE)])
  # THE FALSE Number in HAM
  fnham=length(ham[which(ham==FALSE)])
  # THE TRUE Number in SPAM
  tnspam=length(spam[which(spam==TRUE)])
  # THE FALSE Number in SPAM
  fnspam=length(spam[which(spam==FALSE)])
  # THE TRUE RATIO IN HAM
  RATIOHAM=tnham/(tnham+fnham)
  # THE TRUE RATIO IN SPAM
  RATIOSPAM=tnspam/(tnspam+fnspam)
  r=c(RATIOHAM,RATIOSPAM)
  names(r)=c("ratio of TRUE in HAM", "    The ratio of TRUE in SPAM")
  return(r)
}


# method 3
# draw the boxplot based on the SPAM and HAM
boxplot(V8~V1,main='multiple boxplots', xlab='SPAM',ylab=' ')
