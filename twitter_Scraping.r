#twitter code for Scraping & Sentiment Analysis

library(twitteR)
library(devtools)
library(plyr)
library(stringr)
library(ggplot2)
library(doBy)
library(Quandl)

#Connecting & Authentication to Twitter Dev Api

api_key <- "****"
api_secret <- "****"
access_token <- "9*****"
access_token_secret <- "*****"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Scan for positive & negative words from lexicon & user added



score.sentiment <- function(sentences, pos.words, neg.words, .progress ='none')
{
  require(plyr)
  require(stringr)
  
  scores <- laply(sentences, function(sentence,pos.words,neg.words)
  {
    
    sentence <- gsub('[[:punct:]]', "", sentence)
    
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    
    sentence <- gsub('\\d+', "",sentence)
    
    sentence <- tolower(sentence)
    
    word.list <- str_split(sentence, '\\s+')
    
    words <- unlist(word.list)
    
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
  
}
pos <- scan('C:/Users/zax/Documents/lexicon/pos.txt', what='character', comment.char =';')
neg <- scan('C:/Users/zax/Documents/lexicon/neg.txt', what='character', comment.char =';')
neg.words = c(neg,'worst','shitty','terrible','slow','WTF','terrible','depressing')
pos.words = c(pos,'great','amazing','unbelievable','gr8')

collect.and.score <-function (handle, sports, code, pos.words, neg.words) {
  tweets <- searchTwitter( handle, n=100)
  text <- laply(tweets, function(t)  t$getText())
  score <- score.sentiment(text, pos.words, neg.words)
  score$sports = sports
  score$code = code
  return (score)
}



#conducting match & assign values
RelianceJio.scores = collect.and.score("#Jio","operator","Jio",pos.words,neg.words)
Airtel.scores = collect.and.score("#Airtel","operator","Airtel",pos.words,neg.words)
Vodafone.scores = collect.and.score("VodafoneIN","operator","Vodafone",pos.words,neg.words)
Idea.scores = collect.and.score("#idea","operator","Idea",pos.words,neg.words)
Sys.sleep(30)
Aircel.scores = collect.and.score("#Aircel","operator","Aircel",pos.words,neg.words)
BSNL.scores = collect.and.score("#BSNL","operator","BSNL",pos.words,neg.words)
BSNL3G.scores = collect.and.score("#BSNL | #3G |#B","operator","BSNL3G",pos.words,neg.words)
Sys.sleep(30)
Dolphin.scores = collect.and.score("#Dolphin","operator","Dolphin",pos.words,neg.words)
Telenor.scores = collect.and.score("#Telenor | #Nor","operator","Telenor",pos.words,neg.words)
Orange.scores = collect.and.score("#Orange","operator","Orange",pos.words,neg.words)
Sys.sleep(30)
Loop.scores = collect.and.score("#Loop","operator","Loop",pos.words,neg.words)
Matrix.scores = collect.and.score("#Matrix","operator","Matrix",pos.words,neg.words)
T24.scores = collect.and.score("#T24 | #T","operator","T24",pos.words,neg.words)
Sys.sleep(30)
TSIM.scores = collect.and.score("#TSIM | #Sim","operator","TSIM",pos.words,neg.words)
MTS.scores = collect.and.score("#MTS | #M","operator","MTS",pos.words,neg.words)
Sys.sleep(30)
TataDocomo.scores = collect.and.score("#tata | #docomo","operator","TataDocomo",pos.words,neg.words)
TataIndicom.scores = collect.and.score("#tata | #Indicom","operator","TataIndicom",pos.words,neg.words)
Uniconnect.scores = collect.and.score("#Uniconnect | #Uni","operator","Uniconnect",pos.words,neg.words)
Sys.sleep(30)
Reliance.scores = collect.and.score("#Reliance","operator","Reliance",pos.words,neg.words)
Vectone.scores = collect.and.score("#Vectone | #tone","operator","Vectone",pos.words,neg.words)
MTNL.scores = collect.and.score("#MTNL | MTNL","operator","MTNL",pos.words,neg.words)
Sys.sleep(30)
SkyCell.scores = collect.and.score("#Skycell | #Cell","operator","SkyCell",pos.words,neg.words)
EarthRoam.scores = collect.and.score("#EarthRoam | #Earth","operator","EarthRoam",pos.words,neg.words)
Garuda.scores = collect.and.score("#Garuda | #Cell","operator","Garuda",pos.words,neg.words)
Lyca.scores = collect.and.score("#Lyca","operator","Lyca",pos.words,neg.words)
Sys.sleep(30)
Nextel.scores = collect.and.score("#Nextel | #Next","operator","Nextel",pos.words,neg.words)
Merchant.scores = collect.and.score("#Merchant","operator","Merchant",pos.words,neg.words)
Amantel.scores = collect.and.score("#Amantel | #Aman","operator","Amantel",pos.words,neg.words)
Sys.sleep(30)
XXSIM.scores = collect.and.score("#XXSIM | #Bad","operator","XXSIM",pos.words,neg.words)
DoveConnect.scores = collect.and.score("#Dove | #Connect","operator","DoveConnect",pos.words,neg.words)
Roam1.scores = collect.and.score("#Roam1 | #Roam","operator","Roam1",pos.words,neg.words)
Sys.sleep(30)
DurjatelGlobal.scores = collect.and.score("#Durjatel | #Global","operator","DurjatelGlobal",pos.words,neg.words)
Videocon.scores = collect.and.score("#Videocon | #Video","operator","Videocon",pos.words,neg.words)
Clay.scores = collect.and.score("#Clay | Telecom","operator","Clay",pos.words,neg.words)
Sys.sleep(30)
TMobile.scores = collect.and.score("#TMobile | #T","operator","TMobile",pos.words,neg.words)
Spice.scores = collect.and.score("#Spice","operator","Spice",pos.words,neg.words)
Glomobile.scores = collect.and.score("#Glomobile | #mobile","operator","Glomobile",pos.words,neg.words)
Sys.sleep(30)
Globacom.scores = collect.and.score("#Globacom | #com","operator","Globacom",pos.words,neg.words)
OneCellNet.scores = collect.and.score("#1Cell | #Cell | #Net","operator","1CellNet",pos.words,neg.words)
Verizon.scores = collect.and.score("#Verizon","operator","Verizon",pos.words,neg.words)


#generating calculated results

Airtel.scores$very.pos = as.numeric(sum(Airtel.scores$score >=1))
Airtel.scores$very.neg = as.numeric(sum(Airtel.scores$score <=-1))
Airtel.scores$neutral = as.numeric(sum(Airtel.scores$score == 0))

RelianceJio.scores$very.pos = as.numeric(sum(RelianceJio.scores$score >=1))
RelianceJio.scores$very.neg = as.numeric(sum(RelianceJio.scores$score <=-1))
RelianceJio.scores$neutral = as.numeric(sum(RelianceJio.scores$score == 0))

Vodafone.scores$very.pos = as.numeric(sum(Vodafone.scores$score >=1))
Vodafone.scores$very.neg = as.numeric(sum(Vodafone.scores$score <=-1))
Vodafone.scores$neutral = as.numeric(sum(Vodafone.scores$score == 0))

TataDocomo.scores$very.pos = as.numeric(sum(TataDocomo.scores$score >=1))
TataDocomo.scores$very.neg = as.numeric(sum(TataDocomo.scores$score <=-1))
TataDocomo.scores$neutral = as.numeric(sum(TataDocomo.scores$score == 0))

TataIndicom.scores$very.pos = as.numeric(sum(TataIndicom.scores$score >=1))
TataIndicom.scores$very.neg = as.numeric(sum(TataIndicom.scores$score <=-1))
TataIndicom.scores$neutral = as.numeric(sum(TataIndicom.scores$score == 0))

Reliance.scores$very.pos = as.numeric(sum(Reliance.scores$score >=1))
Reliance.scores$very.neg = as.numeric(sum(Reliance.scores$score <=-1))
Reliance.scores$neutral = as.numeric(sum(Reliance.scores$score == 0))

MTS.scores$very.pos = as.numeric(sum(MTS.scores$score >=1))
MTS.scores$very.neg = as.numeric(sum(MTS.scores$score <=-1))
MTS.scores$neutral = as.numeric(sum(MTS.scores$score == 0))

MTNL.scores$very.pos = as.numeric(sum(MTNL.scores$score >=1))
MTNL.scores$very.neg = as.numeric(sum(MTNL.scores$score <=-1))
MTNL.scores$neutral = as.numeric(sum(MTNL.scores$score == 0))

Idea.scores$very.pos = as.numeric(sum(Idea.scores$score >=1))
Idea.scores$very.neg = as.numeric(sum(Idea.scores$score <=-1))
Idea.scores$neutral = as.numeric(sum(Idea.scores$score == 0))

BSNL.scores$very.pos = as.numeric(sum(BSNL.scores$score >=1))
BSNL.scores$very.neg = as.numeric(sum(BSNL.scores$score <=-1))
BSNL.scores$neutral = as.numeric(sum(BSNL.scores$score == 0))

BSNL3G.scores$very.pos = as.numeric(sum(BSNL3G.scores$score >=1))
BSNL3G.scores$very.neg = as.numeric(sum(BSNL3G.scores$score <=-1))
BSNL3G.scores$neutral = as.numeric(sum(BSNL3G.scores$score == 0))

Dolphin.scores$very.pos = as.numeric(sum(Dolphin.scores$score >=1))
Dolphin.scores$very.neg = as.numeric(sum(Dolphin.scores$score <=-1))
Dolphin.scores$neutral = as.numeric(sum(Dolphin.scores$score == 0))

Aircel.scores$very.pos = as.numeric(sum(Aircel.scores$score >=1))
Aircel.scores$very.neg = as.numeric(sum(Aircel.scores$score <=-1))
Aircel.scores$neutral = as.numeric(sum(Aircel.scores$score == 0))

Telenor.scores$very.pos = as.numeric(sum(Telenor.scores$score >=1))
Telenor.scores$very.neg = as.numeric(sum(Telenor.scores$score <=-1))
Telenor.scores$neutral = as.numeric(sum(Telenor.scores$score == 0))

Orange.scores$very.pos = as.numeric(sum(Orange.scores$score >=1))
Orange.scores$very.neg = as.numeric(sum(Orange.scores$score <=-1))
Orange.scores$neutral = as.numeric(sum(Orange.scores$score == 0))

Loop.scores$very.pos = as.numeric(sum(Loop.scores$score >=1))
Loop.scores$very.neg = as.numeric(sum(Loop.scores$score <=-1))
Loop.scores$neutral = as.numeric(sum(Loop.scores$score == 0))

Matrix.scores$very.pos = as.numeric(sum(Matrix.scores$score >=1))
Matrix.scores$very.neg = as.numeric(sum(Matrix.scores$score <=-1))
Matrix.scores$neutral = as.numeric(sum(Matrix.scores$score == 0))

T24.scores$very.pos = as.numeric(sum(T24.scores$score >=1))
T24.scores$very.neg = as.numeric(sum(T24.scores$score <=-1))
T24.scores$neutral = as.numeric(sum(T24.scores$score == 0))

TSIM.scores$very.pos = as.numeric(sum(TSIM.scores$score >=1))
TSIM.scores$very.neg = as.numeric(sum(TSIM.scores$score <=-1))
TSIM.scores$neutral = as.numeric(sum(TSIM.scores$score == 0))

Uniconnect.scores$very.pos = as.numeric(sum(Uniconnect.scores$score >=1))
Uniconnect.scores$very.neg = as.numeric(sum(Uniconnect.scores$score <=-1))
Uniconnect.scores$neutral = as.numeric(sum(Uniconnect.scores$score == 0))

Vectone.scores$very.pos = as.numeric(sum(Vectone.scores$score >=1))
Vectone.scores$very.neg = as.numeric(sum(Vectone.scores$score <=-1))
Vectone.scores$neutral = as.numeric(sum(Vectone.scores$score == 0))

SkyCell.scores$very.pos = as.numeric(sum(SkyCell.scores$score >=1))
SkyCell.scores$very.neg = as.numeric(sum(SkyCell.scores$score <=-1))
SkyCell.scores$neutral = as.numeric(sum(SkyCell.scores$score == 0))

EarthRoam.scores$very.pos = as.numeric(sum(EarthRoam.scores$score >=1))
EarthRoam.scores$very.neg = as.numeric(sum(EarthRoam.scores$score <=-1))
EarthRoam.scores$neutral = as.numeric(sum(EarthRoam.scores$score == 0))

Garuda.scores$very.pos = as.numeric(sum(Garuda.scores$score >=1))
Garuda.scores$very.neg = as.numeric(sum(Garuda.scores$score <=-1))
Garuda.scores$neutral = as.numeric(sum(Garuda.scores$score == 0))

Lyca.scores$very.pos = as.numeric(sum(Lyca.scores$score >=1))
Lyca.scores$very.neg = as.numeric(sum(Lyca.scores$score <=-1))
Lyca.scores$neutral = as.numeric(sum(Lyca.scores$score == 0))

Nextel.scores$very.pos = as.numeric(sum(Nextel.scores$score >=1))
Nextel.scores$very.neg = as.numeric(sum(Nextel.scores$score <=-1))
Nextel.scores$neutral = as.numeric(sum(Nextel.scores$score == 0))

Merchant.scores$very.pos = as.numeric(sum(Merchant.scores$score >=1))
Merchant.scores$very.neg = as.numeric(sum(Merchant.scores$score <=-1))
Merchant.scores$neutral = as.numeric(sum(Merchant.scores$score == 0))

Amantel.scores$very.pos = as.numeric(sum(Amantel.scores$score >=1))
Amantel.scores$very.neg = as.numeric(sum(Amantel.scores$score <=-1))
Amantel.scores$neutral = as.numeric(sum(Amantel.scores$score == 0))

XXSIM.scores$very.pos = as.numeric(sum(XXSIM.scores$score >=1))
XXSIM.scores$very.neg = as.numeric(sum(XXSIM.scores$score <=-1))
XXSIM.scores$neutral = as.numeric(sum(XXSIM.scores$score == 0))

DoveConnect.scores$very.pos = as.numeric(sum(DoveConnect.scores$score >=1))
DoveConnect.scores$very.neg = as.numeric(sum(DoveConnect.scores$score <=-1))
DoveConnect.scores$neutral = as.numeric(sum(DoveConnect.scores$score == 0))

Roam1.scores$very.pos = as.numeric(sum(Roam1.scores$score >=1))
Roam1.scores$very.neg = as.numeric(sum(Roam1.scores$score <=-1))
Roam1.scores$neutral = as.numeric(sum(Roam1.scores$score == 0))

DurjatelGlobal.scores$very.pos = as.numeric(sum(DurjatelGlobal.scores$score >=1))
DurjatelGlobal.scores$very.neg = as.numeric(sum(DurjatelGlobal.scores$score <=-1))
DurjatelGlobal.scores$neutral = as.numeric(sum(DurjatelGlobal.scores$score == 0))

Videocon.scores$very.pos = as.numeric(sum(Videocon.scores$score >=1))
Videocon.scores$very.neg = as.numeric(sum(Videocon.scores$score <=-1))
Videocon.scores$neutral = as.numeric(sum(Videocon.scores$score == 0))

Clay.scores$very.pos = as.numeric(sum(Clay.scores$score >=1))
Clay.scores$very.neg = as.numeric(sum(Clay.scores$score <=-1))
Clay.scores$neutral = as.numeric(sum(Clay.scores$score == 0))

TMobile.scores$very.pos = as.numeric(sum(TMobile.scores$score >=1))
TMobile.scores$very.neg = as.numeric(sum(TMobile.scores$score <=-1))
TMobile.scores$neutral = as.numeric(sum(TMobile.scores$score == 0))

Spice.scores$very.pos = as.numeric(sum(Spice.scores$score >=1))
Spice.scores$very.neg = as.numeric(sum(Spice.scores$score <=-1))
Spice.scores$neutral = as.numeric(sum(Spice.scores$score == 0))

Glomobile.scores$very.pos = as.numeric(sum(Glomobile.scores$score >=1))
Glomobile.scores$very.neg = as.numeric(sum(Glomobile.scores$score <=-1))
Glomobile.scores$neutral = as.numeric(sum(Glomobile.scores$score == 0))

Globacom.scores$very.pos = as.numeric(sum(Globacom.scores$score >=1))
Globacom.scores$very.neg = as.numeric(sum(Globacom.scores$score <=-1))
Globacom.scores$neutral = as.numeric(sum(Globacom.scores$score == 0))

OneCellNet.scores$very.pos = as.numeric(sum(OneCellNet.scores$score >=1))
OneCellNet.scores$very.neg = as.numeric(sum(OneCellNet.scores$score <=-1))
OneCellNet.scores$neutral = as.numeric(sum(OneCellNet.scores$score == 0))

Verizon.scores$very.pos = as.numeric(sum(Verizon.scores$score >=1))
Verizon.scores$very.neg = as.numeric(sum(Verizon.scores$score <=-1))
Verizon.scores$neutral = as.numeric(sum(Verizon.scores$score == 0))



all.scores = rbind(RelianceJio.scores,Airtel.scores,Vodafone.scores,Idea.scores,Aircel.scores,BSNL.scores,BSNL3G.scores,Dolphin.scores,Telenor.scores,Orange.scores,Loop.scores,Matrix.scores,T24.scores,TSIM.scores,MTS.scores,TataDocomo.scores,TataIndicom.scores,Uniconnect.scores,Reliance.scores,Vectone.scores,MTNL.scores,SkyCell.scores,EarthRoam.scores,Garuda.scores,Lyca.scores,Nextel.scores,Merchant.scores,Amantel.scores,XXSIM.scores,DoveConnect.scores,Roam1.scores,DurjatelGlobal.scores,Videocon.scores,Clay.scores,TMobile.scores,Spice.scores,Glomobile.scores,Globacom.scores,OneCellNet.scores,Verizon.scores)                                                                                                                                                                                                                                                                       

#all.scores = rbind(all.scores,all.scores2)

all.scores <- unique(all.scores[,4:7])

write.csv(all.scores,"C:/Users/zax/Documents/DWBI_codes/TelcoTwitterScores.csv",row.names= FALSE)
