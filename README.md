# sqlhandler

## support library for [sqlhandler-odbc](https://github.com/gbwey/sqlhandler-odbc)

* supports multiple result sets
* allows the user to specify a type safe signature that fully describes the SQL inputs and outputs
* the Encoder module has encoders for converting to haskell values to SQL input parameters
* the Decoder module has decoders for converting from SQL output to haskell values
* the TablePrinter module is used for pretty printing SQL resultsets in tabular form
* fully support SQL output predicates to ensure the data is valid

see [sqlhandler-odbc](https://github.com/gbwey/sqlhandler-odbc) for the HDBC-odbc database implementations

