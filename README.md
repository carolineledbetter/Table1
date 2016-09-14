This is the the Table1 function repository created by Caroline Ledbetter.

Table1 is a function I created to provide characteristics of a study group stratified by some variable (usually an exposure). 

The function can accept variable arguments as the numeric column position in the dataframe, or as the column name in quotes. 

It outputs a matrix with N and % for categorical variables and mean and sd for continuous ones.
In addition, only the 2nd factor of binary categorical variables is displayed. All determinations of categorical, binary, or continuous are performed automatically based on factors. At this time, string variables are not accepted. Variables are displayed in the following order: binary, non-binary categorical, and continuous.



