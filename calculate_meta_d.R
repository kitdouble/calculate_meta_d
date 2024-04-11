calculate_meta_d <- function(data = mydata, accuracy = "correct", identifier = "participant_id", response = "response", confidence = "confidence", target_left = "target_left"){


metad <- matrix(NA, nrow = length(unique(data[,identifier])), ncol = 4)


for(i in 1:length(unique(data[,identifier]))){
  
  ID = unique(data[,identifier])[i]
  print(ID)
  df <- data[data[,identifier] == ID,]
  
  # Meta D
  (y <- table(factor(df[,confidence], levels = 1:6), df[,response], df[,target_left]))
  nr_s1 <- c(rev(y[,1,1]),y[,2,1])
  nr_s2 <- c(rev(y[,1,2]),y[,2,2])

  fit_MLE <- fit_meta_d_SSE(nr_s1,nr_s2, add_constant = T)
  metad[i,] <- c(ID, fit_MLE$M_ratio[1], fit_MLE$meta_da[1], fit_MLE$da[1])
}

metad <- as.data.frame(metad)
colnames(metad) <- c("participant_id", "M_ratio", "meta_da", "da")
aggdata <- metad
aggdata$M_ratio <- as.numeric(aggdata$M_ratio)
aggdata$meta_da <- as.numeric(aggdata$meta_da)
aggdata$da <- as.numeric(aggdata$da)

# Add mean accuracy and confidence
sumdata <- data[,c(confidence, accuracy, identifier)]
colnames(sumdata) <- c("mean_conf", "mean_acc", "participant_id")

sumdata <- aggregate(cbind(mean_conf, mean_acc) ~ participant_id, sumdata, mean)

aggdata <- merge(aggdata, sumdata, by = "participant_id", all =T)
return(aggdata)

}


