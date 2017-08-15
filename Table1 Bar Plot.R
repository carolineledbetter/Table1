Tbl1BarPlot <- function(rowvars, colvariable, data, row_var_names = NULL, 
                        incl_missing = F, colpal = 'Blues', colmix = T, 
                        ordervars = F) {
  # determine if data is a design object or data frame
  weighted <- F
  if (!is.data.frame(data)){
    classData <- class(data)
    if('survey.design' %in% classData) {
      if (!requireNamespace('survey', quietly = T)) {
        stop('Survey Package is required for weighted tables')
      }
      design <- data
      data <- design$variables[0,]
      weighted <- T
      if (incl_missing == T) {
        warning('Missing is turned off for weighted tables')
        incl_missing <- F
      }
    } else {
        stop('Data is not a data frame or design object')
      }
  }
  
  
  #check that all arguments are valid 
  if (!is.atomic(rowvars)) stop("Please pass row variables as a vector")
  if (!all(sapply(data[, rowvars], is.factor))) 
    stop('All row variables must be factors')
  
  if (weighted == T) {
    if (length(unique(design$variables[, colvariable])) > 20) {
      stop(paste0("Column Variable has more than 20 unique values,", 
                  "please pass a column variable with less", 
                  "than 20 unique values"))
    }
    
    if (!is.factor(design$variables[, colvariable])) {
      design$variables[, colvariable] <- 
            factor(design$variables[, colvariable])
      data[, colvariable] <- factor(design$variables[, colvariable])[0]
    }
  } 
  
  if (length(unique(data[, colvariable])) > 20) {
    stop(paste0("Column Variable has more than 20 unique values,", 
         "please pass a column variable with less than 20 unique values"))
  }
  
  if (!is.factor(data[, colvariable])) {
    data[, colvariable] <- factor(data[, colvariable])
  }
  
  if (!is.null(row_var_names) & length(rowvars) != length(row_var_names)){
    stop("Length of Row Variable Names is not equal to Row Variables")
  }
    
  if (length(unique(rowvars)) != length(rowvars)){
    stop('You may not pass duplicate row variables')
  }
  
  # set numeric colvariable and rownames to character names so they 
  # can be used in formula arguments also names will be used in table
  if (is.numeric(rowvars)){
    rowvars <- names(data)[rowvars]
  }
  
  if (is.numeric(colvariable))  colvariable <- names(data)[colvariable]

  #set column names
  #col dimensions
  col_dim <- length(levels(data[, colvariable]))
  
  #add missing level for factors 
  if(incl_missing == T) {
    for(i in catvars){
      if(any(is.na(data[,i]))){
        levels(data[,i]) <- c(levels(data[,i]),'Missing')
        data[,i][is.na(data[,i])] <- 'Missing'
      }
    }; remove(i)
  }
  
  
  numlevels <- sapply(rowvars, function(i) {length(levels(data[, i]))})
  
  #reorder high to low
  if(ordervars) rowvars <- names(sort(numlevels, decreasing = T))
  
  rnames <- rowvars
  
  # replace variable names with row variable names if they were provided
  if(!is.null(row_var_names)){
    tmp <- rowvars
    n <- match(tmp, rnames)
    rnames[n] <- row_var_names
  }
  
  coul <- RColorBrewer::brewer.pal(max(numlevels), colpal)
  if(colmix) {
    set.seed(1111)
    coul <- coul[sample(1:length(coul), length(coul), replace = F)]
  }
    
  # function to return rows for categorical variables
  returnGraph <- function(var){
    levs <- length(levels(data[,var]))
    if (weighted == T){
      n <- survey::svytable(as.formula(paste0("~", var, ' + ', 
                                      colvariable)), design, 
                    round = T)
      svychisq(as.formula(paste0("~", var, ' + ', 
                                 colvariable)), design, statistic = 'F')
    } else {
      n <- table(data[, var],data[, colvariable])
    }
    
    percent <- t(sapply(1:levs, function(i){round(n[i,] / apply(n,2,sum) 
                                                  * 100, digits = 0)}))
    colnames(percent) <- c('Full', 'Marginal', 'Low', 'Very Low')
    barplot(percent, border="white", col = coul[1:levs], horiz = T, las = 1,
            xaxt = 'n', cex.names = 2, cex.lab  = 4, ylab = var)
  }
  # Make a stacked barplot--> it will be in %!
  par(mfrow=c(length(rowvars),1), oma = c(2, 0, 3, 0), 
      family = 'serif', mar = c(1, 10, 1, 0), mgp = c(7, 0, 0))
  returnGraph(rowvars[1])
  returnGraph(rowvars[2])
  returnGraph(rowvars[3])
  returnGraph(rowvars[4])
  returnGraph(rowvars[5])
  
  mtext("Characterstics of Study Participants by Food Security Category", 
        outer=TRUE, cex = 2)
  mtext("x-axis", 1, 1, outer=TRUE)
  mtext("y-axis", 2, 1, outer=TRUE, las=0)
  
 


  
  #put together table
  rowheadercat <- NULL
  rowheadercont <- NULL
  cattable <- NULL
  conttable <- NULL
  if (length(catvars) != 0){
    cattable <- do.call(rbind, 
                        lapply(c(lapply(binaryvars, returnRowCat, r = 1), 
                                 lapply(nonbinary, returnRowCat, r = 0)), 
                               data.frame, stringsAsFactors=FALSE))
    names(cattable) <-  c(1:length(cattable))
    rowheadercat <- rep("N(%)", col_dim)
    if(incl_pvalues == T){
      rowheadercat <- c(rowheadercat, '')
    }
  }
  if (length(contvars) != 0){
    conttable <- do.call(rbind, 
                         lapply(lapply(contvars, returnRowContinuous), 
                                data.frame, stringsAsFactors=FALSE))
    names(conttable) <- c(1:length(conttable))
    rowheadercont <- rep("Mean(SD)", col_dim)
    if(incl_pvalues == T){
      rowheadercont <- c(rowheadercont, '')
    }
  }
  
  
  finaltab <- as.matrix(rbind.data.frame(rowheadercat, 
                                         cattable, 
                                         rowheadercont, 
                                         conttable,
                                         stringsAsFactors = F))


  dimnames(finaltab) <- list(rnames, cnames)
  return(finaltab)
}


