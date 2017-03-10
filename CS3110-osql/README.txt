  **************************
  |   OSQL COMPILE & RUN   |
+-**************************-+
| - step 1 -                 |
| cd to src                  |  
| - step 2 -                 |
| enter cmd [make]           |  
| - step 3 -                 |           
| cd ..                      |  
| - step 4 -                 |
| enter cmd [./main.byte]    |  
| * osql should be running * |
+----------------------------+
| other option:              |
| use utop to run some       |
| dynamic statements.        |
| see <build_table.ml> for   | 
| examples.                  |
| - step 1 -                 |
| open utop in main directory|
| - step 2 -                 |   
| #use "build_table.ml";;    |   
| - step 3 -                 | 
| build ();;                 |   
| * now there should be 2 *  |
| * tables created in     *  |
| * database:             *  |
| *   - pkt_info          *  |
| *   - pkt_addr          *  |
+----------------------------+

+----------SYMBOLS----------+
|                           |
|     {{ <data_type> }}     | 
|       [ <optional> ]      | 
| { <opt1> | <opt2> | ... } | 
|                           |
+---------------------------+

+------------SAMPLE TABLE <customers>------------+  
| +----+----------+-----+-----------+----------+ |
| | ID | NAME     | AGE | ADDRESS   | SALARY   | |
| +----+----------+-----+-----------+----------+ |
| |  1 | Ramesh   |  32 | Ahmedabad |  2000.   | |
| |  2 | Khilan   |  25 | Delhi     |  1500.   | |
| |  3 | kaushik  |  23 | Kota      |  2000.   | |
| |  4 | Chaitali |  25 | Mumbai    |  6500.   | |
| |  5 | Hardik   |  27 | Bhopal    |  8500.   | |
| |  6 | Komal    |  22 | MP        |  4500.   | |
| |  7 | Muffy    |  24 | Indore    | 10000.   | |
| +----+----------+-----+-----------+----------+ |
+------------------------------------------------+

                  ***********************
                  |   OSQL STATEMENTS   |
                  ***********************
+---------------------------------------------------------+
| value{{ int | float | boolean | string | char | null }} |
| -----                                                   |
| data_type:                                              |
| ---------                                               |
|   { int | float | boolean | string | char | null }      |   
| create_def:                                             | 
| ----------                                              |
|    col_def1, col_def2, ...                              |   
| col_def: col_attri                                      | 
| --------                                                |
| col_attri:                                              |
| ---------                                               |
|   col_name;                                             |   
|   data_type;                                            |
|   not_null{{boolean}};                                  |       
|   default{{value}};                                     |   
|   auto_incr{{boolean}}                                  |       
|                                                         |   
| tbl_as: SELECT ... (some valid select statement)        |
| ------                                                  |
| sel_expr: col_op [AS sub_name]                          |
| --------                                                |
| /* the col_op[i] should be the same as sel_expr,        |    
|  * except that you can't have sub_name, hence no        |      
|  * keyward AS */                                        |   
| tbl_ref:                                                |
| -------                                                 |
|   tbl_name [AS sub_name]                                |       
|   [                                                     |       
|    {LEFT | RIGHT | INNER } JOIN tbl_name                |   
|    [AS sub_name]                                        |   
|    [ON col_op]                                          |   
|   ]                                                     |       
| where_cond: col_op                                      |
| ----------                                              |
+---------------------------------------------------------+

. ############################ 1 ########################### .  
. +----------------------------------------------------+     .
. | SELECT [DISTINCT]                                  |     .
. |    sel_expr1 [, sel_expr2 ...]                     |     .    
. |    FROM tbl_ref                                    |     .
. |    [WHERE where_cond]                              |     .
. |    [GROUP BY col_op1 [, col_op2 ...] [ASC | DESC]] |     .
. |    [HAVING where_cond]                             |     .
. |    [ORDER BY col_op1 [, col_op2 ...] [ASC | DESC]] |     .
. |    [LIMIT offset{{int}}]                           |     .
. +----------------------------------------------------+     .
.                                                            .
. || ^^^^^^^^^^^^^^^^^^^^^ EXAMPLE 1 ^^^^^^^^^^^^^^^^^^^^ || .
. ||                                                      || .
. || osql> SELECT id FROM personal_info;                  || .
. || +---+                                                || .
. || |id |                                                || .
. || +---+                                                || .
. || |0  |                                                || . 
. || |1  |                                                || . 
. || |4  |                                                || . 
. || |2  |                                                || . 
. || |3  |                                                || . 
. || +---+                                                || . 
. ||                                                      || .
. || ---------------------------------------------------- || .
. || ^^^^^^^^^^^^^^^^^^^^^ EXAMPLE 2 ^^^^^^^^^^^^^^^^^^^^ || .
. ||                                                      || .
. || osql> SELECT id, name FROM personal_info;            || .
. || +---+-----------------+                              || .
. || |id |name             |                              || .
. || +---+-----------------+                              || .
. || |0  |Mary Ji          |                              || .
. || |1  |Yanghui Ou       |                              || .
. || |4  |Yiwen Huang      |                              || .
. || |2  |Nina G           |                              || .
. || |3  |Michael Clarkson |                              || .
. || +---+-----------------+                              || .
. ||                                                      || .
. || ---------------------------------------------------- || .
. || ^^^^^^^^^^^^^^^^^^^^^ EXAMPLE 3 ^^^^^^^^^^^^^^^^^^^^ || .
. ||                                                      || .
. || osql> SELECT id, name FROM personal_info             || .  
. ||   >>> WHERE id > 1;                                  || .
. || +---+-----------------+                              || .
. || |id |name             |                              || .
. || +---+-----------------+                              || .
. || |4  |Yiwen Huang      |                              || .
. || |2  |Nina G           |                              || .
. || |3  |Michael Clarkson |                              || .
. || +---+-----------------+                              || .
. ||                                                      || .
. || ---------------------------------------------------- || .
. || ^^^^^^^^^^^^^^^^^^^^^ EXAMPLE 4 ^^^^^^^^^^^^^^^^^^^^ || .
. ||                                                      || .
. || osql> SELECT * FROM personal_info                    || .
. ||   >>> ORDER BY id asc;                               || .
. || +---+-----------------+-----------------+----------+ || .
. || |id |name             |major            |grade     | || .
. || +---+-----------------+-----------------+----------+ || .
. || |0  |Mary Ji          |Computer Science |Sophomore | || .
. || |1  |Yanghui Ou       |ECE              |Junior    | || .
. || |2  |Nina G           |Computer Science |Junior    | || .
. || |3  |Michael Clarkson |Computer Science |Professor | || .
. || |4  |Yiwen Huang      |Computer Science |Sophomore | || .
. || +---+-----------------+-----------------+----------+ || . 
. ||                                                      || . 
. || ---------------------------------------------------- || . 
. || ^^^^^^^^^^^^^^^^^^^^^ EXAMPLE 5 ^^^^^^^^^^^^^^^^^^^^ || . 
. ||                                                      || . 
. || osql> SELECT * FROM personal_info                    || . 
. ||   >>> WHERE id > 1                                   || . 
. ||   >>> ORDER BY id desc;                              || . 
. || +---+-----------------+-----------------+----------+ || .
. || |id |name             |major            |grade     | || .
. || +---+-----------------+-----------------+----------+ || .
. || |4  |Yiwen Huang      |Computer Science |Sophomore | || .
. || |3  |Michael Clarkson |Computer Science |Professor | || .
. || |2  |Nina G           |Computer Science |Junior    | || .
. || +---+-----------------+-----------------+----------+ || .
. ||                                                      || .
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
.                                                            .
. ########################################################## .

. ################################## 2 ################################## .
.                                                                         .
. +---------------------------------------+                               . 
. | CREATE TABLE [IF NOT EXISTS] tbl_name |                               . 
. |    create_def                         |                               .  
. |    [AS tbl_as]                        |                               .
. +---------------------------------------+                               .
.                                                                         .   
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^ EXAMPLE 1 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
. ||                                                                   || .
. || /* table <customers> is yet to be created */                      || .
. ||                            - step 1 -                             || .
. || osql> CREATE TABLE customers(                                     || .   
. ||             ID    int NOT NULL,                                   || .       
. ||           NAME string NOT NULL,                                   || .       
. ||            AGE    int NOT NULL,                                   || .       
. ||        ADDRESS   char         ,                                   || .       
. ||         SALARY  float         ,                                   || .       
. ||       );                                                          || .   
. ||                            - step 2 -                             || .    
. ||   (option 1)                                                      || .   
. ||    osql> CREATE TABLE customers(...)                              || . 
. ||          /* exceptions will be raised because IF NOT EXISTS       || . 
. ||           * isn't in the query */                                 || . 
. ||   (option 2)                                                      || .   
. ||    osql> CREATE TABLE IF NOT EXISTS customers(...)                || .   
. ||          /* nothing happens because IF NOT EXISTS is indicated */ || .
. ||                                                                   || .
. || ----------------------------------------------------------------- || .
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^ EXAMPLE 2 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
. ||                                                                   || .
. || /* valid query */                                                 || .   
. || osql> CREATE TABLE customers (...) AS                             || .   
. ||   >>> SELECT *                                                    || .   
. ||   >>> FROM company                                                || .   
. ||   >>> WHERE salary = "Undecided";                                 || .   
. ||                                                                   || .
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
.                                                                         .
. ####################################################################### .

. ######################### 3 ######################### .
. +--------------------------------------------------+  .
. | INSERT INTO tbl_name                             |  .  
. |    col_name1 = value1 [, col_name2 = value2] ... |  .
. +--------------------------------------------------+  .
.                                                       .  
. || ^^^^^^^^^^^^^^^^^^^ EXAMPLES ^^^^^^^^^^^^^^^^^^ || . 
. ||                                                 || .
. || /* table <customers> is yet to be filled out */ || .         
. || osql> INSERT INTO customers (                   || .         
. ||   >>> ID = 1,                                   || .     
. ||   >>> NAME = Ramesh,                            || .     
. ||   >>> AGE = 32,                                 || .     
. ||   >>> ADDRESS = Ahmedabad,                      || .         
. ||   >>> SALARY = 2000.                            || .     
. ||   >>> );                                        || .     
. || +----+----------+-----+-----------+----------+  || .
. || | ID | NAME     | AGE | ADDRESS   | SALARY   |  || .
. || +----+----------+-----+-----------+----------+  || .
. || |  1 | Ramesh   |  32 | Ahmedabad |  2000.00 |  || .
. || +----+----------+-----+-----------+----------+  || .
. ||                                                 || .
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
.                                                       .
. ##################################################### .

. ######################### 4 ######################## .
.                                                      .
. +-----------------------+                            .
. | DELETE FROM tbl_name  |                            .
. |    [WHERE where_cond] |                            .
. +-----------------------+                            .
. || ^^^^^^^^^^^^^^^^^^ EXAMPLE ^^^^^^^^^^^^^^^^^^^ || .
. ||                                                || .  
. || osql> DELETE FROM customers                    || .  
. ||   >>> WHERE ID = 4;                            || .  
. || +----+----------+-----+-----------+----------+ || .
. || | ID | NAME     | AGE | ADDRESS   | SALARY   | || .
. || +----+----------+-----+-----------+----------+ || .
. || |  1 | Ramesh   |  32 | Ahmedabad |  2000.   | || .
. || |  2 | Khilan   |  25 | Delhi     |  1500.   | || .
. || |  3 | kaushik  |  23 | Kota      |  2000.   | || .
. || |  5 | Hardik   |  27 | Bhopal    |  8500.   | || .
. || |  6 | Komal    |  22 | MP        |  4500.   | || .
. || |  7 | Muffy    |  24 | Indore    | 10000.   | || .
. || +----+----------+-----+-----------+----------+ || .
. ||                                                || .
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
.                                                      .
. #################################################### .

. ############################ 5 ########################### .
. +------------------------------------------------------+   .
. | UPDATE tbl_name                                      |   . 
. |    SET col_name1 = value1 [, col_name2 = value2] ... |   .
. |    [WHERE where_cond]                                |   . 
. +------------------------------------------------------+   .
.                                                            .
. || ^^^^^^^^^^^^^^^^^^^^^ EXAMPLE 1 ^^^^^^^^^^^^^^^^^^^^ || .
. ||                                                      || .
. || osql> SELECT * FROM personal_info;                   || . 
. || +---+-----------------+-----------------+----------+ || .
. || |id |name             |major            |grade     | || .
. || +---+-----------------+-----------------+----------+ || .
. || |0  |Mary Ji          |Computer Science |Sophomore | || .
. || |1  |Yanghui Ou       |ECE              |Junior    | || .
. || |4  |Yiwen Huang      |Computer Science |Sophomore | || .
. || |2  |Nina G           |Computer Science |Junior    | || . 
. || |3  |Michael Clarkson |Computer Science |Professor | || . 
. || |5  |rando            |                 |          | || . 
. || |5  |rando            |rando            |rando     | || . 
. || +---+-----------------+-----------------+----------+ || . 
. ||                                                      || . 
. || osql> UPDATE personal_info                           || .      
. ||   >>> SET major = "Undecided"                        || .      
. ||   >>> WHERE major = "rando";                         || .  
. ||                                                      || .     
. || osql> SELECT * FROM personal_info;                   || . 
. || +---+-----------------+-----------------+----------+ || .
. || |id |name             |major            |grade     | || .
. || +---+-----------------+-----------------+----------+ || .
. || |0  |Mary Ji          |Computer Science |Sophomore | || .
. || |1  |Yanghui Ou       |ECE              |Junior    | || .
. || |4  |Yiwen Huang      |Computer Science |Sophomore | || .
. || |2  |Nina G           |Computer Science |Junior    | || .
. || |3  |Michael Clarkson |Computer Science |Professor | || .
. || |5  |rando            |                 |          | || .
. || |5  |rando            |Undecided        |rando     | || .
. || +---+-----------------+-----------------+----------+ || .
. ||                                                      || .
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
.                                                            .
. ########################################################## .

. ################################ 6 ############################### .
.                                                                    .
. ALTER TABLE tbl_name alt_tbl                                       . 
. ----------------------------                                       .
. alt_tbl:                                                           .
.    | ADD COLUMN col_name data_type                                 .                 
.    | DROP COLUMN col_name                                          .    
.                                                                    .
. ||^^^^^^^^^^^^^^^^^^^^^^^^^^ EXAMPLES ^^^^^^^^^^^^^^^^^^^^^^^^^^|| .
. ||                                                              || .
. ||  ++++ osql> ALTER TABLE customers ADD COLUMN sex char; ++++  || .        
. ||  +----+---------+-----+-----------+----------+------+        || .    
. ||  | ID | NAME    | AGE | ADDRESS   | SALARY   | SEX  |        || .    
. ||  +----+---------+-----+-----------+----------+------+        || .    
. ||  |  1 | Ramesh  |  32 | Ahmedabad |  2000.   | NULL |        || .    
. ||  |  2 | Ramesh  |  25 | Delhi     |  1500.   | NULL |        || .    
. ||  |  3 | kaushik |  23 | Kota      |  2000.   | NULL |        || .    
. ||  |  4 | kaushik |  25 | Mumbai    |  6500.   | NULL |        || .    
. ||  |  5 | Hardik  |  27 | Bhopal    |  8500.   | NULL |        || .    
. ||  |  6 | Komal   |  22 | MP        |  4500.   | NULL |        || .    
. ||  |  7 | Muffy   |  24 | Indore    | 10000.   | NULL |        || .    
. ||  +----+---------+-----+-----------+----------+------+        || .    
. ||  ++++   osql> ALTER TABLE customers DROP COLUMN sex;   ++++  || .    
. ||  +----+----------+-----+-----------+----------+              || .    
. ||  | ID | NAME     | AGE | ADDRESS   | SALARY   |              || .    
. ||  +----+----------+-----+-----------+----------+              || .    
. ||  |  1 | Ramesh   |  32 | Ahmedabad |  2000.   |              || .    
. ||  |  2 | Khilan   |  25 | Delhi     |  1500.   |              || .    
. ||  |  3 | kaushik  |  23 | Kota      |  2000.   |              || .    
. ||  |  4 | Chaitali |  25 | Mumbai    |  6500.   |              || .    
. ||  |  5 | Hardik   |  27 | Bhopal    |  8500.   |              || .    
. ||  |  6 | Komal    |  22 | MP        |  4500.   |              || .    
. ||  |  7 | Muffy    |  24 | Indore    | 10000.   |              || .    
. ||  +----+----------+-----+-----------+----------+              || . 
. ||                                                              || .
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
.                                                                    .
. ################################################################## .

. ############################# 7 ############################ .
.                                                              .
. DROP TABLE [IF EXISTS] tbl_name                              .
. -------------------------------                              .
.                                                              .    
. || ^^^^^^^^^^^^^^^^^^^^^^ EXAMPLES ^^^^^^^^^^^^^^^^^^^^^^ || . 
. ||                       - step 1 -                       || . 
. || osql> DROP TABLE customers                             || . 
. || /* table <customers> is now nonexistent */             || . 
. ||                       - step 2 -                       || . 
. ||   (option 1)                                           || .     
. ||   osql> DROP TABLE customers                           || .     
. ||   /* exception will be raised because IF EXISTS        || . 
. ||    * isn't in the query */                             || . 
. ||   (option 2)                                           || .     
. ||   osql> DROP TABLE IF EXISTS customers                 || . 
. ||   /* nothing happens because IF EXISTS is indicated */ || .
. ||                                                        || .
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
.                                                              .
. ############################################################ .


. ######################### 8 ######################## .
. TRUNCATE TABLE tbl_name                              . 
. -----------------------                              .
.                                                      .
. || ^^^^^^^^^^^^^^^^^^ EXAMPLES ^^^^^^^^^^^^^^^^^^ || .                   
. ||                                                || .               
. ||  ++++++ osql> SELECT * FROM customers; ++++++  || .                     
. || +----+----------+-----+-----------+----------+ || .                
. || | ID | NAME     | AGE | ADDRESS   | SALARY   | || .                
. || +----+----------+-----+-----------+----------+ || .
. ||                                                || .
. || ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ || .
.                                                      .
. #################################################### .

                   **********************
                   |   OSQL OPERATORS   |
            +------**********************------+
            | +  ::= PLUS                      |  
            | -  ::= MINUS                     |  
            | *  ::= MULTIPLY                  |      
            | \  ::= DIVIDE                    |  
            | %  ::= MOD                       |  
            | >  ::= GREATER THAN              |          
            | <  ::= LESS THAN                 |      
            | =  ::= EQUAL TO                  |      
            | <> ::= NOT EQUAL TO              |           
            | >= ::= GREATHER THAN OR EQUAL TO |
            | <= ::= LESS THAN OR EQUAL TO     |  
            ------------------------------------
              *******************************
              |   OSQL BUILT-IN FUNCTIONS   |
              *******************************

 | ################ AGGREGATE FUNCTIONS ################ |  
 |                      MAX(expr)                        |  
 |                      ---------                        |  
 | * returns the maximum value of <expr>.             *  |
 | * max() may take a string argument; in such cases, *  | 
 | * it returns the maximum string value.             *  |
 | * returns NULL if there were no matching rows.     *  |
 | ----------------------------------------------------- | 
 |                      MIN(expr)                        |  
 |                      ---------                        |  
 | * returns the minimum value of <expr>.             *  | 
 | * max() may take a string argument; in such cases, *  | 
 | * it returns the minimum string value.             *  |
 | * returns NULL if there were no matching rows.     *  |
 | ----------------------------------------------------- | 
 |                      AVG(expr)                        | 
 |                      ---------                        |    
 |   * returns the average value of <expr>.         *    |
 |   * returns NULL if there were no matching rows. *    |
 | ----------------------------------------------------- |       
 |                      SUM(expr)                        |
 |                      ---------                        |   
 |* returns the sum of <expr>.                         * |
 |* if the return set has no rows, sum() returns NULL. * |
 |* returns NULL if there were no matching rows.       * |
 | ----------------------------------------------------- |  
 |                      COUNT(expr)                      |   
 |                      -----------                      |   
 |* returns a count of the number of non-NULL values of *| 
 |* <expr> in the rows retrieved by a SELECT statement. *|
 |* the result is a INT value.                          *| 
 |* returns 0 if there were no matching rows.           *|
 ---------------------------------------------------------
 #########################################################
 
| ################*## STRING OPERATORS ###############*### |
|                    CONCAT(str1,str2)                     | 
|                    -----------------                     | 
|  * returns the string that results from concatenating *  | 
|  * the two arguments.                                 *  |
|  * returns NULL if any argument is NULL.              *  |
|                                                          | 
|             SUBSTRING_INDEX(str,delim,count)             | 
|             --------------------------------             | 
| * returns the substring from string <str> before      *  |    
| * <count> occurrences of the delimiter <delim>.       *  |    
| * if <count> is positive, everything to the left of   *  |
| * final delimiter (counting from the left) is         *  |
| * returned.                                           *  |
| * if <count> is negative, everything to the right of  *  | 
| * the final delimiter (counting from the right) is    *  |
| * returned.                                           *  |
| * performs a case-sensitive match when searching for  *  |
| * <delim>.                                            *  |
| * returns <str> if <delim> is not found or <count> is *  |
| * greater than the number of occurrences of <delim>   *  |
| * in <str>.                                           *  |
| -------------------------------------------------------- | 
|                        UPPER(str)                        | 
|                        ----------                        | 
| * returns the string <str> with all characters changed * | 
| * to uppercase according to the current character set  * |
| * mapping.                                             * |
| -------------------------------------------------------- | 
|                        LOWER(str)                        |
|                        ----------                        | 
| * returns the string <str> with all characters changed * |
| * to lowercase according to the current character set  * | 
| * mapping.                                             * | 
| -------------------------------------------------------- | 
|                     CHAR_LENGTH(str)                     |     
|                     ----------------                     | 
| * returns the length of the string <str>, measured in  * | 
| * characters. A multibyte character counts as a single * | 
| * character.                                           * | 
| -------------------------------------------------------- | 
|                INSERT(str,pos,len,newstr)                | 
|                --------------------------                | 
|* returns the string <str>, with the substring beginning *|
|* at position <pos> and <len> characters long replaced   *| 
|* by the string <newstr>.                                *|    
|* returns the original string if <pos> is not within the *|
|* length of the string.                                  *| 
|* replaces the rest of the string from position <pos> if *|
|* <len> is not within the length of the rest of the      *| 
|* string.                                                *| 
|* returns NULL if any argument is NULL.                  *|
| -------------------------------------------------------- |      
|        LOCATE(substr,str), LOCATE(substr,str,pos)        |     
|        ------------------  ----------------------        | 
| * the first syntax returns the position of the first   * |  
| * occurrece of the substring <substr> in string <str>. * |  
| * the second syntax returns the position of the first  * |  
| * occurrence of substring <substr> in string <str>,    * |  
| * starting at position <pos>.                          * |  
| * returns 0 if <substr> is not in <str>.               * |       
| * returns NULL if <substr> or <str> is NULL.           * |  
| -------------------------------------------------------- |  
|    TRIM({BOTH | LEADING | TRAILING} [remstr] FROM str)   | 
|    ---------------------------------------------------   | 
|* returns the string <str> with all <remstr> prefixes or *|
|* suffixes removed.                                      *| 
|* <remstr> is optional and, if not specified, spaces are *|
|* removed.                                               *|
| -------------------------------------------------------- |
|                        REVERSE(str)                      |
|                        ------------                      |
|     * returns the string <str> with the order of the *   |
|     * characters reversed.                           *   |
------------------------------------------------------------
############################################################