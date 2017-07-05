row_names <- c('Binary Balanced', 
               'Binary Unbalanced Missing', 
               'Categorical Balanced Missing', 
               'Categorical Balanced', 
               'Continuous Balanced', 
               'Categorical Unbalanced Missing', 
               'Binary Balanced Missing', 
               'Continuous Unbalanced Missing')
rows <- c('m', 'n', 'p', 'q', 'w', 'x', 'y', 'z')
col <- 'outcome'
source(file = 'Table1.R')
load(file = 'test.rda')
test_table1 <- Table1(1:8, 9, test)
test_table2 <- Table1(1:8, 9, test, row_var_names = row_names)
test_table3 <- Table1(1:8, 9, test, row_var_names = row_names, emphasis = 'b')
test_table4 <- Table1(1:8, 9, test, row_var_names = row_names, emphasis = 'b',
                      incl_missing = T)
test_table5 <- Table1(rows, 9, test, row_var_names = row_names, 
                      emphasis = 'b',
                      incl_missing = T)
test_table6 <- Table1(1:4, 9, test, emphasis = 'b', incl_missing = T
                      )
test_table7 <- Table1(c('w', 'z'), 9, test, emphasis = 'b', incl_missing = T
                      )
