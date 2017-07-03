Table1 <- function(rowvars, colvariable, data, row_var_names = NULL, 
                   incl_missing = F, incl_pvalues = T) {
  if (incl_missing == T & incl_pvalues == T) 
    warning('P values are only calculated on non-missing observations')
  if (!is.atomic(rowvars)) stop("Please pass row variables as a vector")
  if (length(unique(data[,colvariable])) > 20) 
    stop("Column Variable has more than 20 unique values, please pass a column variable with less than 20 unique values")
  if (!is.factor(data[,colvariable])) data[,colvariable] <- factor(data[,colvariable])
  if (!is.null(row_var_names) & length(rowvars) != length(row_var_names))
    stop("Length of Row Variable Names is not equal to Row Variables")
  if (length(unique(rowvars)) != length(rowvars))
    stop('You may not pass duplicate row variables')
  
  #set column names
  Col_n <- table(data[,colvariable])
  if(incl_pvalues == T){
    cnames <- c(paste0(levels(data[,colvariable]), " (n=", 
                       format(Col_n, big.mark = ',', trim = T), 
                       ")"), 'p-value')
  } else {
    cnames <- c(paste0(levels(data[,colvariable]), " (n=", 
                       format(Col_n, big.mark = ',', trim = T), 
                       ")"))
  }
  
  #col dimensions
  col_dim <- length(levels(data[,colvariable]))
  
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
  
  if (is.numeric(contvars)){
    continuous_labels <- unlist(lapply(contvars, 
                                       function(i){names(data)[i]}))
  } else {
    continuous_labels <- contvars
    }
  
  if(incl_missing == T & length(contvars) != 0) {
    continuous_labels  <- unlist(
      lapply(1:length(contvars), function(x){
        if (sum(is.na(data[,contvars[x]])) >0){
          return(list(continuous_labels[x], '\\ Missing N(%)'))
        }
        return(continuous_labels[x])
      }))
  }
   
  # put together all rownames
  rnames <- c(" ", binarylabs, nonbinlab," ",continuous_labels)
  
  if (length(catvars) == 0) {
    rnames <- c(" ",continuous_labels)
  }
  if (length(contvars) == 0){
    rnames <- c(" ", binarylabs, nonbinlab)
  }
 
  if(!is.null(row_var_names = NULL)){
      if (is.numeric(rowvars)){
        n <- match(names(data)[rowvars], rnames)
      } else{
        n <- match(rowvars, rnames)
        }
    rnames[n] <- row_var_names
  }
  
  # function to return row for binary categorical variables
  returnRowBin <- function(var){
    n <- table(data[,var],data[,colvariable])
    p <- NULL
    rep <- 0
    if (incl_pvalues == T){
      p <- anova(glm(as.formula(paste0(colvariable, "~", var)), 
                     data = data, 
                     family = binomial()), test = 'LRT')$`Pr(>Chi)`[2]
      p <- ifelse(p < 0.01, '<0.01', sprintf('%.2f',p))
      rep <- 1
    }
    
    percent <- round(n[2,]/table(data[,colvariable])*100, digits = 0)
    n_per <- c(paste0(format(n[2,], big.mark = ',', trim = T), 
                      "(", percent, ")"), replicate(rep, " "))
    returnRow <- matrix(c(replicate(col_dim, 
                                    " "), p, n_per), nrow = 2, 
                        byrow = T)
    return(returnRow)
  }

  # function to return row for nonbinary categorical variables
  returnRowNonBin <- function(var){
    levs <- length(levels(data[,var]))
    n <- table(data[,var],data[,colvariable])
    p <- NULL
    rep <- 0
    if (incl_pvalues == T){
      p <- anova(glm(as.formula(paste0(colvariable, "~", var)), 
                     data = data[data[,var] != 'Missing',], 
                     family = binomial()), test = 'LRT')$`Pr(>Chi)`[2]
      p <- ifelse(p < 0.01, '<0.01', sprintf('%.2f',p))
      rep <- levs
    }
    
    percent <- t(sapply(1:levs, function(i){round(n[i,]/table(
      data[,colvariable])*100, digits = 0)}))
    n_per <- cbind(matrix(paste(format(n, big.mark = ',', trim = T), 
                                "(", percent, ")", sep = ''),nrow = levs, 
                          byrow = F), replicate(rep, " "))
    returnRow <- rbind(c(replicate(col_dim, ""), p), n_per)
    return(returnRow)
  }
  

  
  # function to return continuous rows 
  returnRowContinuous <- function(var){
    df <- data.frame(x = data[,var], y = data[,colvariable])
    summ <- sapply(levels(data[,colvariable]), function(i) {
      mean <- mean(data[,var][data[,colvariable] == i], 
                   na.rm = T)
      sd <- sd(data[,var][data[,colvariable] == i], 
               na.rm = T)
      return(c(mean,sd))
    })
    
    p <- NULL
    if (incl_pvalues == T){
      p <- summary(aov(x ~ y, data=df))[[1]][5][1,]
      p <- ifelse (p < 0.01, '<0.01', sprintf('%.2f',p))
    }
    
    if (abs(summ[1,1]) >= 10){
      m_sd <- paste0(round(summ[1,],digits=0), "(", 
                     round(summ[2,],digits = 0), ")")
    } else{
      if (abs(summ[1,2]) >= 1){
        m_sd <- paste0(sprintf('%.1f',summ[1,]), "(", 
                       sprintf('%.1f',summ[2,]), ")")
      } else{
        if (abs(summ[1,2]) >= 0.1){
          m_sd <- paste0(sprintf('%.2f',summ[1,]), "(", 
                         sprintf('%.2f',summ[2,]), ")")
        } else{
          if (abs(summ[1,2]) >= 0.01){
            m_sd <- paste0(sprintf('%.3f',summ[1,]), "(", 
                           sprintf('%.3f',summ[2,]), ")")
          }
          m_sd <- paste0(sprintf('%.2e',summ[1,]), "(", 
                           sprintf('%.2e',summ[2,]), ")")
          }}}
    returnRow <- matrix(c(m_sd, p),nrow = 1, byrow = T)
    if (incl_missing == T & sum(is.na(df$x)) > 0){
      N <- summaryBy(x ~ y, data = df, 
                               FUN = function(i) sum(is.na(i)))[,2]
      pct <- as.vector(round((N/table(df$y))*100,0))
      spacer <- NULL
      if (incl_pvalues == T){
        spacer <- ' '
      }
      N_pct <- c(paste0(N[], '(', pct[], ")"), spacer)
      returnRow <- matrix(c(returnRow, N_pct), nrow = 2, byrow = T)
    }
    return(returnRow)
  }
  
  #put together table
  rowheadercat <- NULL
  rowheadercont <- NULL
  cattable <- NULL
  conttable <- NULL
  if (length(catvars) != 0){
    cattable <- do.call(rbind, 
                        lapply(c(lapply(binaryvars, returnRowBin), 
                                 lapply(nonbinary, returnRowNonBin)), 
                               data.frame, stringsAsFactors=FALSE))
    if(incl_pvalues == T){
      rowheadercat <- c(replicate(col_dim, "N(%)"), '')
    }
    else{
      rowheadercat <- c(replicate(col_dim, "N(%)"))
    }
  }
  if (length(contvars) != 0){
    conttable <- do.call(rbind, 
                         lapply(lapply(contvars, returnRowContinuous), 
                                data.frame, stringsAsFactors=FALSE))
    if(incl_pvalues == T){
      rowheadercont <- c(replicate(col_dim, "Mean(SD)"), '')
    }
    else{
      rowheadercont <- c(replicate(col_dim, "Mean(SD)"))
    }
  }

  finaltab <- as.matrix(rbind.data.frame(rowheadercat, 
                                         cattable, 
                                         rowheadercont, 
                                         conttable,
                                         make.row.names = F,
                                         stringsAsFactors = F))


  dimnames(finaltab) <- list(rnames,cnames)
  return(finaltab)
}


