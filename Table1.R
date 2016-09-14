Table1 <- function(rowvars, colvariable, data, continuous_labels) {
  if (!is.atomic(rowvars)) stop("Please pass row variables as a vector")
  if (!is.factor(data[,colvariable])) data[,colvariable] <- factor(data[,colvariable])
  #set column names
  Col_n <- table(data[,colvariable])
  cnames <- c(paste(levels(data[,colvariable]), " (n=", Col_n, ")", sep = ''), 'p-value')
  
  #col dimensions
  col_dim <- length(levels(data[,colvariable]))
  
  #determine row types and names
  vartypes <- lapply(rowvars, function(i){is.factor(data[,i])})
  catvars <- rowvars[vartypes == T]
  numlevels <- lapply(catvars, function(i){length(levels(data[,i]))})
  binaryvars <- catvars[numlevels == 2]
  binarylabs <- unlist(lapply(binaryvars, function(i){
    if (is.numeric(i)) title <- names(data)[i]
    else title <- i
    lab <- c(title,paste("\\  ",levels(data[,i])[2], sep = ''))
    return(lab)
    }))
  nonbinary <- catvars[!(numlevels == 2)]
  nonbinlab <- unlist(lapply(nonbinary, function(x){
    if (is.numeric(x)) title <- names(data)[x]
    else title <- x
    lab <- c(title,paste("\\  ",levels(data[,x]), sep = ''))
    return(lab)
    }))
   
  contvars <- rowvars[vartypes == F]
  if(missing(continuous_labels)){
    if (is.numeric(contvars)) continuous_labels <- unlist(lapply(contvars, function(i){names(data)[i]}))
    else continuous_labels <- contvars
  }
  else if (!is.atomic(continuous_labels)) continuous_labels <- unlist(continuous_labels)
   rnames <- c("", binarylabs, nonbinlab,"",continuous_labels) 

  #function to return row for binary categorical variables
  returnRowBin <- function(var){
    n <- table(data[,var],data[,colvariable])
    if (length(n[n<5]) == 0){p <- chisq.test(n)$p.value}
    else {p <- fisher.test(n)$p.value}
    percent <- round(n[2,]/table(data[,colvariable])*100, digits = 0)
    n_per <- c(paste(n[2,], "(", percent, ")", sep = ''), "")
    returnRow <- matrix(c(replicate(col_dim,""),
                            sprintf('%.2f',round(p, digits = 2)), n_per),nrow = 2, byrow = T)
    return(returnRow)
  }
  
  returnRowNonBin <- function(var){
      levs <- length(levels(data[,var]))
      n <- table(data[,var],data[,colvariable])
      if (length(n[n<5]) == 0){p <- chisq.test(n)$p.value}
      else {p <- fisher.test(n)$p.value}
      percent <- matrix(unlist(lapply(1:levs, function(i){round(n[i,]/table(
       data[,colvariable])*100, digits = 0)})),nrow = levs, byrow = TRUE)
      n_per <- cbind(matrix(paste(n, "(", percent, ")", sep = ''),nrow = levs, byrow = T),replicate(levs,""))
      returnRow <- rbind(c(replicate(col_dim,""),sprintf('%.2f',round(p, digits = 2))), n_per)
    return(returnRow)
  }
  
  returnRowContinuous <- function(var){
    require(doBy)
    df <- data.frame(x = data[,var],y = data[,colvariable])
    summ <- summaryBy(x ~ y, data = df, FUN=c(mean,sd))
    p <- summary(aov(x ~ y, data=df))[[1]][5][1,]
    m_sd <- paste(round(summ[,2],digits=0),"(",round(summ[,3],digits = 0),")",sep = '')
    returnRow <- matrix(c(m_sd, sprintf('%.2f',round(p, digits = 2))),nrow = 1, byrow = T)
    return(returnRow)
  }
  
  #put together table
  cattable <- do.call(rbind, lapply(c(lapply(binaryvars, returnRowBin),lapply(nonbinary, returnRowNonBin)),
                                    data.frame, stringsAsFactors=FALSE))
  conttable <- do.call(rbind, lapply(lapply(contvars, returnRowContinuous),
                                     data.frame, stringsAsFactors=FALSE))
  finaltab <- as.matrix(rbind.data.frame(c(replicate(col_dim,"N(%)"), ''),cattable,c(replicate(col_dim,"Mean(SD)"), ''),
                         conttable,
                         make.row.names = F,
                         stringsAsFactors = F))
  dimnames(finaltab) <- list(rnames,cnames)
  return(finaltab)
  
}


