#CHANGE THE PATH TO YOUR FILE AREA HERE
setwd("C:/Users/Yicheng/Desktop/SASData/ppbrehm-6100-project-brehm (1)/ppbrehm-6100-project-brehm/csubrama-dsba-6100-u-90-sp-2017-project/csubrama-dsba-6100-u-90-sp-2017-project")
#READ IN DATA, KEEP ALL DATA IN THE SAME PATH
score <- read.csv("CollegeScorecard.csv", header = TRUE)
crime <- read.csv("Crime_2015.csv", header = TRUE)
#cleaning
str(crime)
crime1 <- cbind(as.character(crime$ViolentCrime),as.character(crime$PropertyCrime))
crime1[,2]
as.numeric(crime1[,2])
split1 <- strsplit(crime1[,1],",")
split2 <- strsplit(crime1[,2],",")
split1a <- c()
split2a <- c()
length(split2[[1]]) 
for(i in 1:length(split1)){
  if(length(split1[[i]]) == 2){
    split1a[i] <- as.numeric(split1[[i]])[1]*1000 + as.numeric(split1[[i]])[2]
  } else{
    split1a[i] <- as.numeric(split1[[i]])[1]
  }
}
for(i in 1:length(split2)){
  if(length(split2[[i]]) == 2){
    split2a[i] <- as.numeric(split2[[i]])[1]*1000 + as.numeric(split2[[i]])[2]
  } else{
    split2a[i] <- as.numeric(split2[[i]])[1]
  }
}
crime2 <- cbind(split1a, split2a)
crime2[is.na(crime2)] <- 0
median(sort(crime2[,1])[2:378])
median(sort(crime2[,2])[7:378])
crime2[,1][which(crime2[,1] == 0)] <- median(sort(crime2[,1])[2:378])
crime2[,2][which(crime2[,2] == 0)] <- median(sort(crime2[,2])[7:378])
summary(crime2)
totalcrime <- crime2[,1] + crime2[,2]
median(totalcrime)
which(totalcrime < median(totalcrime))
crime$City[which(totalcrime < median(totalcrime))]
which(score$LOCALE == 11 | score$LOCALE == 12 | score$LOCALE == 21)

metro1 <- read.csv("metrocomp1.csv", header = TRUE)
metro2 <- read.csv("metrocomp2.csv", header = TRUE)
metro1$Metro.Area.Main.City[which(metro1$Rank.2015 < 11)]

filters <- which(c(score$CIP11ASSOC == 1 | score$CIP11BACHL == 1 | score$CIP11CERT1 == 1 |
      score$CIP11CERT2 == 1 | score$CIP11CERT4 == 1 | score$CIP52ASSOC == 1 | score$CIP52BACHL == 1 | score$CIP52CERT1 == 1 |
      score$CIP52CERT2 == 1 | score$CIP52CERT4 == 1) & score$main == 1 &
      score$AccredAgency != "" & 
       c(score$LOCALE == 11 | score$LOCALE == 12 | score$LOCALE == 21) )
score$CITY[filters]
