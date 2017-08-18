This is the the Table1 function repository created by Caroline Ledbetter.

Table1 is a function I created to provide characteristics of a study group stratified by some variable (usually an exposure). 

The function can accept variable arguments as the numeric column position in the dataframe, or as the column name in quotes.  
The output of this function is designed to be used with pander in rmarkdown, but all row name formatting can be removed with the option: emphasis = 'n'.

It outputs a matrix with N and % for categorical variables and mean and sd for continuous ones.
In addition, only the 2nd factor of binary categorical variables is displayed. All determinations of categorical, binary, or continuous are performed automatically based on factors. Only numeric(continuous) or factored(categorical) are permitted. Variables are displayed in the following order: binary, non-binary categorical, and continuous. If no continuous variable is provided, summary statistics on the entire sample are provided. No p-values can be provided in this case. If a design object is passed in lieu of a data frame, weighted numbers using the survey package are provided. (The survey package must be installed in this case.)

Row variable names can be passed if desired, but must equal the number of row variables passed. In addition there are options on whether p-values and missing should be included. 


