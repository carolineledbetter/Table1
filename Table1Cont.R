Table1 <- function(rowvars, data, incl_missing = F) {
  if (!is.atomic(rowvars)) stop("Please pass row variables as a vector")
  
  colname <- paste0('N = ', format(nrow(data), big.mark = ',', trim = T))
  
  #determine row types and names
  vartypes <- lapply(rowvars, function(i){is.factor(data[,i])})
  catvars <- rowvars[vartypes == T]
  
  #add missing level for factors 
  if(incl_missing == T) {
    for(i in catvars){
      if(any(is.na(data[,i]))){
        levels(data[,i]) <- c(levels(data[,i]),'Missing')
        data[,i][is.na(data[,i])] <- 'Missing'
      }
    }; remove(i)
  }
  
  numlevels <- lapply(catvars, function(i){length(levels(data[,i]))})
  binaryvars <- catvars[numlevels == 2]
  binarylabs <- unlist(lapply(binaryvars, function(i){
    title <- ifelse(is.numeric(i), names(data)[i], i)
    lab <- c(title,paste("\\  ",levels(data[,i])[2], sep = ''))
    return(lab)
    }))
  nonbinary <- catvars[!(numlevels == 2)]
  nonbinlab <- unlist(lapply(nonbinary, function(x){
    title <- ifelse(is.numeric(x), names(data)[x], x)
    lab <- c(title,paste("\\  ",levels(data[,x]), sep = ''))
    return(lab)
    }))
  
  
  contvars <- rowvars[vartypes == F]
  continuous_labels <- ifelse(is.numeric(contvars), 
                              unlist(lapply(contvars, 
                                            function(i){names(data)[i]})), contvars)
  
  rnames <- c(" ", binarylabs, nonbinlab," ",continuous_labels) 

  #function to return row for binary categorical variables
  returnRowBin <- function(var){
    n <- table(data[,var])
    percent <- round(n[2]/sum(n)*100, digits = 0)
    n_per <- paste0(format(n[2], big.mark = ',', trim = T), "(", percent, ")")
    returnRow <- matrix(c("", n_per),nrow = 2, byrow = T)
    return(returnRow)
  }
  
  returnRowNonBin <- function(var){
      levs <- length(levels(data[,var]))
      n <- table(data[,var])
      percent <- sapply(1:levs, function(i){round(n[i]/sum(n)*100, digits = 0)})
      n_per <- matrix(paste(format(n, big.mark = ',', trim = T), 
                                  "(", percent, ")", sep = ''),nrow = levs, 
                            byrow = F)
      returnRow <- rbind("", n_per)
    return(returnRow)
  }
  
  returnRowContinuous <- function(var){
    summ <- c(mean(data[,var], na.rm = T), sd(data[,var], na.rm = T))
    if (summ[1] >= 10) m_sd <- paste(round(summ[1],digits=0), "(", 
                                       round(summ[2],digits = 0), ")", sep = '')
    else if (summ[1] >= 1) m_sd <- paste(sprintf('%.1f',summ[1]), "(", 
                                           sprintf('%.1f',summ[2]), ")", sep = '')
    else if (summ[1] >= 0.1) m_sd <- paste(sprintf('%.2f',summ[1]), "(", 
                                             sprintf('%.2f',summ[2]), ")", sep = '')
    else if (summ[1] >= 0.01) m_sd <- paste(sprintf('%.2e',summ[1]), "(", 
                                              sprintf('%.2e',summ[2]), ")", sep = '')
    returnRow <- matrix(m_sd, nrow = 1, byrow = T)
    return(returnRow)
  }
  
  #put together table
  cattable <- do.call(rbind, c(lapply(binaryvars, returnRowBin), 
                               lapply(nonbinary, returnRowNonBin)))
  
  conttable <- do.call(rbind, lapply(contvars, returnRowContinuous))
  
  finaltab <- rbind("N(%)", cattable, "Mean(SD)", conttable)
  dimnames(finaltab) <- list(rnames, colname)
  return(finaltab)
  
}


