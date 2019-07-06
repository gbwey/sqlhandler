# sqlhandler

* supports multiple result sets 
* allows the user to specify a type safe signature that fully describes the sql inputs and outputs
* the Encoding module has encoders for converting to haskell values to sql input parameters
* the Decoding module has decoders for converting from sql output to haskell values
* the TablePrinter module is used for pretty printing sql resultsets in tabular form
* fully support sql output predicates to ensure the data is valid

see sqlhandler-odbc for the HDBC-odbc database implementations

