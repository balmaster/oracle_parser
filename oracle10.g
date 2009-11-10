grammar oracle10;

options {
	output=AST;
	ASTLabelType=CommonTree;
	backtrack=false;
}

start_rule
	:	select
	;

select    
	:	subquery  
	;

subquery
	:	scalar_subquery (order_by_clause)?
	;

scalar_subquery
	:	/*subquery_factoring_clause*/ SELECT /*(hint)?*/ (ALL|UNIQUE|DISTINCT)? select_list
		FROM
		scalar_subquery1
		where_clause?
		hierarchical_query_clause?
		group_by_clause?
		(HAVING condition)?
		//model_clause?
		((UNION ALL?|INTERSECT|MINUS) LB subquery RB)?
	;

fragment
scalar_subquery1
	:	table_reference (COMMA table_reference)*
	|	(join_clause) => join_clause
	;

/*
fragment
hint
    :
    ;
*/    

subquery_factoring_clause
	:	WITH (query_name AS LB subquery RB)(COMMA query_name AS LB subquery RB)*    
	;
    
fragment
query_name
	:	IDENT
	;    
    
select_list
	:	select_list1 (COMMA select_list1)*
	;

fragment    
select_list1
	:	(obj_path_expression DOT)? ASTERISK
	|	expr ((AS)? c_alias)?
	;
    
fragment    
schema
	:	IDENT
	;
    
fragment
c_alias
	:	IDENT
	;                    
    
table_reference
	:	query_table_expression /*flashback_query_clause?*/ t_alias?
	;
    
fragment    
t_alias
	:	IDENT
	;

    
flashback_query_clause
	:	(VERSIONS BETWEEN (SCN| TIMESTAMP) (expr| MINVALUE) AND (expr|MAXVALUE))? AS OF (SCN|TIMESTAMP) expr
	;

query_table_expression
	:	(schema DOT)? IDENT (AT_SIGN dblink)?
	|	LB subquery subquery_restriction_clause? RB
	//|    table_collection_expression
	;    
    
//fragment         
//query_table_expression1
//    :    IDENT query_table_expression2?
//    ;
    
fragment    
dblink
	:	IDENT
	;    
    
//query_table_expression2
//    :    (( PARTITION LB partition RB)|( SUBPARTITION LB subpartition RB))? sample_clause?
//    |    sample_clause
//    ;
    
fragment    
partition
	:	IDENT
	;
    
fragment    
subpartition
    :    IDENT
    ;        
    
fragment    
sample_clause
	:	SAMPLE BLOCK? LB sample_percent RB ( SEED LB seed_value RB)?    
	;
    
fragment    
sample_percent
	:	integer
	;    
    
fragment    
seed_value
    :    integer
    ;    
    
subquery_restriction_clause
    :    WITH (( READ ONLY)|( CHECK OPTION ( CONSTRAINT condition)? ))    
    ;
    
  
table_collection_expression
    :    TABLE LB collection_expression     RB (LB PLUS RB)+
    ;

collection_expression
    :    IDENT
    ;


join_clause
    :    table_reference (inner_cross_join_clause|outer_join_clause)*
    ;


inner_cross_join_clause
	:	INNER? JOIN table_reference (( ON condition)|( USING LB column(COMMA column)* RB))    
	|	( CROSS|( NATURAL INNER?)) JOIN table_reference
	;
    
        
column
	:    IDENT
	;

outer_join_clause
    :    query_partition_clause? NATURAL? outer_join_type JOIN
    table_reference /*query_partition_clause?*/    ((ON condition)| (USING LB column(COMMA column)* RB))?             
    ;
    
query_partition_clause
    :    PARTITION BY (query_partition_clause1|(LB query_partition_clause1 RB))    
    ;
    
fragment    
query_partition_clause1
    :    column(COMMA column)*
    ;    
    
outer_join_type
    :    ( FULL | LEFT | RIGHT ) OUTER?
    ;

where_clause
    :    WHERE condition
    ;
    
hierarchical_query_clause
    :    ( START WITH condition )? CONNECT BY NOCYCLE? condition
    ;
    
group_by_clause
    :    GROUP BY group_by_clause1(COMMA group_by_clause1)*
    ;
         
fragment
group_by_clause1            
    :    expr
    |    rollup_cube_clause
    |    grouping_sets_clause
    ;
    
rollup_cube_clause
    :    ( ROLLUP | CUBE ) LB grouping_expression_list RB    
    ;
    
grouping_sets_clause
    :    GROUPING SETS LB grouping_sets_clause1 (COMMA grouping_sets_clause)* RB    
    ;
    
fragment    
grouping_sets_clause1
    :     rollup_cube_clause
    |    grouping_expression_list
    ;
    
grouping_expression_list
    :    expression_list(COMMA expression_list)*
    ;
    
expression_list
    :    expr
    ;
    
/*    
model_clause
    :    MODEL cell_reference_options? return_rows_clause? reference_model* main_model
    ;
*/
    
cell_reference_options
    :    (( IGNORE | KEEP ) NAW )? (UNIQUE ( DIMENSION |( SINGLE REFERENCE)))?    
    ;
    
return_rows_clause
    :    RETURN ( UPDATED|ALL) ROWS
    ;
    
    /*
reference_model
    :    REFERENCE reference_model_name ON LB subquery RB model_column_clauses cell_reference_options?
    ;
    */

fragment
reference_model_name
    :    IDENT
    ;    

/*    
main_model
    :    ( MAIN main_model_name)? model_column_clauses cell_reference_options? model_rules_clause
    ;
*/
    
fragment    
main_model_name
    :    IDENT
    ;    
    
model_column_clauses
    :    (query_partition_clause c_alias?)? DIMENSION BY LB model_column(COMMA model_column)*RB
    MEASURES LB model_column(COMMA model_column)* RB
    ;
    
model_column
    :    expr (AS? c_alias)?    
    ;
    
model_rules_clause
    :    ( RULES (( UPDATE|( UPSERT ALL?)))?)? (( AUTOMATIC| SECUENTIAL) ORDER)?
    ( ITERATE)? LB number RB ( UNTIL LB condition RB)?
    LB model_rules_clause1(COMMA model_rules_clause1)* RB                         
    ;
    
fragment    
model_rules_clause1
    :    ((UPDATE|(UPSERT ALL?)))? cell_assignment order_by_clause? '=' expr
    ;
    
cell_assignment
    :    measure_column LSB ((cell_assignment1(COMMA cell_assignment1)*)|multi_column_for_loop) RSB    
    ;
    
measure_column
    :    IDENT
    ;    
    
fragment    
cell_assignment1
    :    expr
    |    single_column_for_loop
    ;
    
single_column_for_loop
    :    FOR dimension_column single_column_for_loop1        
    ;
    
dimension_column
    :    IDENT
    ;    
    
fragment    
single_column_for_loop1
    :    IN LB ((literal(COMMA literal)*)|subquery)  RB    
    |    ( LIKE pattern)? FROM literal TO literal ( INCREMENT| DECREMENT) literal
    ;
    
fragment    
pattern
    :    expr
    ;    
    
multi_column_for_loop
    :    FOR dimension_column(COMMA dimension_column)* RB IN ((multi_column_for_loop1(COMMA multi_column_for_loop1)*)|subquery)
    ;
    
fragment    
multi_column_for_loop1
    :    LB literal(COMMA literal)* RB
    ;
        
literal
    :    IDENT
    ;        
                            
order_by_clause
    :    ORDER SIBLINGS? BY order_by_clause1(COMMA order_by_clause1)*
    ;
    
fragment    
order_by_clause1
    :    (expr) ( ASC| DESC)? (( NULLS FIRST)|(NULLS LAST))?    
    ;
    
    
for_update_clause
    :    FOR UPDATE (OF for_update_clause1(COMMA)for_update_clause1)? ( NOWAIT|( WAIT integer))?    
    ;
    
fragment    
for_update_clause1
    :    ((schema DOT)?(IDENT)DOT)? column
    ;
    
condition
	:	(b1 OR) => b1 OR condition
	|	b1
	;
    
fragment    
expr_comparison
	:	expr ( '=' | '!=' | '^=' | '<>' | '>' | '<' | '>=' | '<=' )  
		(
			expr 
			|	( ANY | SOME | ALL ) LB ( expression_list | subquery ) RB
		)
	;    

fragment
expr_list_condition
	:	 LB expr (COMMA expr )+ RB ( '=' | '!=' | '^=' | '<>' ) 
		(
			LB subquery RB
			|	( ANY | SOME | ALL )  LB ((expression_list (COMMA expression_list)*) | subquery) RB
		)
	;    
    
floating_point_condition
	:	expr IS NOT? ( NAN | INFINITE )
	;
    
	
fragment
b1
	:	(b2 AND) => b2 AND condition
	|	b2
	;
	
fragment 
b2
	:	NOT condition
	|	LB condition RB
	|	(expr ( '=' | '!=' | '^=' | '<>' | '>' | '<' | '>=' | '<=' ) ) => expr_comparison
	|	(LB expr (COMMA expr )+ RB ( '=' | '!=' | '^=' | '<>' )) => expr_list_condition
	|	(floating_point_condition) => floating_point_condition
//    |    model_condition
//    |    multiset_condition
//    |    pattern_matching_condition
	|	(range_condition) => range_condition
	|	(null_condition) => null_condition
//    |    XML_condition
    //|    LB condition RB
	|	(exists_condition) => exists_condition
	|	(in_condition) => in_condition
//	|	(is_of_type_condition) => is_of_type_condition
	;
    
model_condition
	:	is_any_condition
	|	is_present_condition
	;
    
is_any_condition
	:	(dimension_column IS )? ANY
	;
    
is_present_condition
	:	cell_reference IS PRESENT
	;
    
cell_reference
	:	IDENT
	;    
    
multiset_condition
    :    is_a_set_conditions
    |    is_empty_conditions
    |    member_condition
    |    submultiset_conditions
    ;
    
is_a_set_conditions
    :    nested_table IS NOT? A_SIGN SET
    ;
    
nested_table
    :    IDENT
    ;    
    
is_empty_conditions
    :    nested_table IS NOT? EMPTY
    ;

member_condition
    :    expr NOT? MEMBER OF? nested_table
    ;
    
submultiset_conditions
    :    nested_table NOT? SUBMULTISET OF? nested_table
    ;
    
//pattern_matching_condition
//    :    like_condition
//    |    regexp_like_condition
//    ;
    
like_condition
    :
//    :    CHAR NOT? ( LIKE | LIKEC | LIKE2 | LIKE4 ) CHAR ( ESCAPE char)?
    ;
    
//char
//    :    ( 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '#' )
//    ;    
    
//regexp_like_condition
//    :    REGEXP_LIKE LB char COMMA pattern (COMMA match_parameter )? RB
//    ;
    
match_parameter
    :    expr
    ;    
    
range_condition
    :    expr NOT? BETWEEN expr AND expr
    ;

null_condition
    :    expr IS NOT? NULL
    ;

exists_condition
    :    EXISTS LB subquery RB
    ;

in_condition
	:	(expr) => expr NOT? IN LB ( expression_list | subquery ) RB
	|	(LB expr (COMMA expr)+ RB) NOT? IN LB ( (expression_list(COMMA expression_list)*)| subquery) RB
	;

is_of_type_condition
    :    expr IS NOT? OF TYPE LB is_of_type_condition1 (COMMA is_of_type_condition1)* RB
    ;
    
fragment    
is_of_type_condition1
    : ONLY? (schema DOT) type
    ;

type
	:	IDENT
	;

expr
    :   (compound_expr) => compound_expr
    |    case_expression
//    |    cursor_expression
    |    datetime_expression
//    |    function_expression
//    |    interval_expression
//    |    object_access_expression
    |    scalar_subquery_expression
//    |    model_expression
//    |    type_constructor_expression
//    |    variable_expression
    ;

obj_path_expression
	:	IDENT (DOT IDENT)? (AT_SIGN schema)?
	;
    

integer
    :    number
    ;

number
	:     NUMBER
    ;

sequence
   	:	IDENT
	;


compound_expr
	:	(e1 ( PLUS | MINUS| DOUBLEVERTBAR)) => e1 ( PLUS | MINUS | DOUBLEVERTBAR) expr
	|	e1
	;
	
fragment	
e1
	:	(e2 (ASTERISK | DIVIDE) )=> e2 (ASTERISK | DIVIDE) expr
	|	e2
	;

fragment
e2
	:	PRIOR expr
	|	LB expr RB
	|	(obj_path_expression DOT)? ( column | ROWID )
	|	ROWNUM
	|	NULL
	|	QUOTE IDENT_CHAR* QUOTE
	|	number
	|	(sequence DOT ( CURRVAL | NEXTVAL )) => sequence DOT ( CURRVAL | NEXTVAL )
	;

case_expression
    :    CASE ( simple_case_expression | searched_case_expression) (ELSE expr)? END
    ;

simple_case_expression
    :    expr ( WHEN condition THEN expr)+
    ;
    
   
searched_case_expression
    :    (WHEN condition THEN expr)+
    ;


cursor_expression
    :    CURSOR LB subquery RB
    ;

datetime_expression
    :    datetime_value_expr AT ( LOCAL | ('TIME' 'ZONE' datetime_expression1))
    ;
    
datetime_value_expr
    :
    ;    
    
datetime_expression1
//    :    ' [ + | - ] hh:mm'
    :    DBTIMEZONE
//     :    ' time_zone_name '
    |    expr
        ;
               
function_expression
    :
    ;
    

interval_expression
    :    interval_value_expr interval_expression1
    ;
   
interval_value_expr
    :
//    :    expr
    ;   
   
interval_expression1
    :    DAY ( LB leading_field_precision RB )? TO SECOND ( LB fractional_second_precision RB )?
    |    YEAR ( LB leading_field_precision RB )? TO MONTH
    ;

leading_field_precision
    :    integer
    ;

fractional_second_precision
    :    integer
    ;

   
scalar_subquery_expression
    :   LB  scalar_subquery RB
    ;
    
model_expression
    :
    ;
    
type_constructor_expression
    :
    ;
    
variable_expression
    :    COLON host_variable (INDICATOR? COLON indicator_variable)?
    ;
    
fragment    
host_variable
    :    IDENT
    ;    
    
fragment    
indicator_variable
    :    IDENT
    ;    
    
    
SL_COMMENT
    :    '--' (options {greedy=false;} : .)* ('\r'|'\n'|'\r\n')
        {$channel=HIDDEN;}
    ;
    
ML_COMMENT
    :   '/*' (options {greedy=false;} : .)* '*/'
        {$channel=HIDDEN;}
    ;

    
WS    :    (' '|'\r'|'\t'|'\n') {$channel=HIDDEN;}
    ;


fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');


SELECT
	:	S E L E C T 
	;

ALL
	:	A L L
	;

UNIQUE
	:	U N I Q U E
	;

DISTINCT
	:	D I S T I N C T
	;

FROM
	:	F R O M
	;

HAVING
	:	H A V I N G
	;

UNION
	:	U N I O N
	;

INTERSECT
	:	I N T E R S E C T
	;

MINUS
	:	M I N U S
	;

LB
	:	'('
	;

RB
	:	')'
	;

WITH
	:	W I T H
	;

AS
	:	A S
	;

COMMA
	:	','
	;

DOT
	:	'.'
	;

ONLY
	:	O N L Y
	;

VERSIONS
	:	V E R S I O N S
	;

BETWEEN
	:	B E T W E E N
	;

SCN
	:	S C N
	;

TIMESTAMP
	:	T I M E S T A M P
	;

MINVALUE
	:	M I N V A L U E
	;

AND
    :    'AND'
    ;

MAXVALUE
    :    'MAXVALUE'
    ;

OF
    :    'OF'
    ;

TABLE
    :    'TABLE'
    ;

VIEW
    :    'VIEW'
    ;

MATERIALIZED
    :    'MATERIALIZED'
    ;

PARTITION
    :    'PARTITION'
    ;

SUBPARTITION
    :    'SUBPARTITION'
    ;

AT_SIGN
    :    '@'
    ;

SAMPLE
    :    'SAMPLE'
    ;

BLOCK
    :    'BLOCK'
    ;

SEED
    :    'SEED'
    ;

READ
    :    'READ'
    ;

CHECK
    :    'CHECK'
    ;

OPTION
    :    'OPTION'
    ;

CONSTRAINT
    :    'CONSTRAINT'
    ;

INNER
    :    'INNER'
    ;

JOIN
    :    'JOIN'
    ;

ON
    :    'ON'
    ;

USING
    :    'USING'
    ;

CROSS
    :    'CROSS'
    ;

NATURAL
    :    'NATURAL'
    ;

FULL
    :    'FULL'
    ;

LEFT
    :    'LEFT'
    ;

RIGHT
    :    'RIGHT'
    ;

OUTER
    :    'OUTER'
    ;

WHERE
    :    'WHERE'
    ;

START
    :    'START'
    ;

CONNECT
    :    'CONNECT'
    ;

BY
    :    'BY'
    ;

NOCYCLE
    :    'NOCYCLE'
    ;

GROUP
    :    'GROUP'
    ;

ROLLUP
    :    'ROLLUP'
    ;

CUBE
    :    'CUBE'
    ;

GROUPING
    :    'GROUPING'
    ;

SETS
    :    'SETS'
    ;

MODEL
    :    'MODEL'
    ;

IGNORE
    :    'IGNORE'
    ;

KEEP
    :    'KEEP'
    ;

NAW
    :    'NAW'
    ;

DIMENSION
    :    'DIMENSION'
    ;

SINGLE
    :    'SINGLE'
    ;

REFERENCE
    :    'REFERENCE'
    ;

RETURN
    :    'RETURN'
    ;

UPDATED
    :    'UPDATED'
    ;

ROWS
    :    'ROWS'
    ;

MAIN
    :    'MAIN'
    ;

MEASURES
    :    'MEASURES'
    ;

RULES
    :    'RULES'
    ;

UPDATE
    :    'UPDATE'
    ;

UPSERT
    :    'UPSERT'
    ;

AUTOMATIC
    :    'AUTOMATIC'
    ;

SECUENTIAL
    :    'SECUENTIAL'
    ;

ORDER
    :    'ORDER'
    ;

ITERATE
    :    'ITERATE'
    ;

UNTIL
    :    'UNTIL'
    ;

LSB
    :    '['
    ;

RSB
    :    ']'
    ;

FOR
    :    'FOR'
    ;

IN
    :    'IN'
    ;

LIKE
    :    'LIKE'
    ;

TO
    :    'TO'
    ;

INCREMENT
    :    'INCREMENT'
    ;

DECREMENT
    :    'DECREMENT'
    ;

SIBLINGS
    :    'SIBLINGS'
    ;

ASC
    :    'ASC'
    ;

DESC
    :    'DESC'
    ;

NULLS
    :    'NULLS'
    ;

FIRST
    :    'FIRST'
    ;

LAST
    :    'LAST'
    ;

NOWAIT
    :    'NOWAIT'
    ;

WAIT
    :    'WAIT'
    ;

ANY
    :    'ANY'
    ;

SOME
    :    'SOME'
    ;

IS
    :    'IS'
    ;

NOT
    :    'NOT'
    ;

NAN
    :    'NAN'
    ;

INFINITE
    :    'INFINITE'
    ;

PRESENT
    :    'PRESENT'
    ;

A_SIGN
    :    'A'
    ;

SET
    :    'SET'
    ;

EMPTY
    :    'EMPTY'
    ;

MEMBER
    :    'MEMBER'
    ;

SUBMULTISET
    :    'SUBMULTISET'
    ;

LIKEC
    :    'LIKEC'
    ;

LIKE2
    :    'LIKE2'
    ;

LIKE4
    :    'LIKE4'
    ;

ESCAPE
    :    'ESCAPE'
    ;

REGEXP_LIKE
    :    'REGEXP_LIKE'
    ;

NULL
    :    'NULL'
    ;

OR
    :    'OR'
    ;

EXISTS
    :    'EXISTS'
    ;

TYPE
    :    'TYPE'
    ;

ASTERISK
    :    '*'
    ;

ROWID
    :    'ROWID'
    ;

ROWNUM
    :    'ROWNUM'
    ;

CURRVAL
    :    'CURRVAL'
    ;

NEXTVAL
    :    'NEXTVAL'
    ;

PRIOR
    :    'PRIOR'
    ;

CASE
    :    'CASE'
    ;

END
    :    'END'
    ;

WHEN
    :    'WHEN'
    ;

THEN
    :    'THEN'
    ;

ELSE
    :    'ELSE'
    ;

CURSOR
    :    'CURSOR'
    ;

AT
    :    'AT'
    ;

LOCAL
    :    'LOCAL'
    ;

DBTIMEZONE
    :    'DBTIMEZONE'
    ;

DAY
    :    'DAY'
    ;

YEAR
    :    'YEAR'
    ;

SECOND
    :    'SECOND'
    ;

MONTH
    :    'MONTH'
    ;

INDICATOR
    :    'INDICATOR'
    ;

PLUS
    :    '+'
    ;



SEMICOLON
    :    ';'
    ;

COLON
    :    '\:'
    ;

DOUBLEVERTBAR
    :    '||'
    ;

DIVIDE
    :    '/'
    ;


QUOTE
    :    '\''
    ;

fragment    
N_
    : '0' .. '9' ( '0' .. '9' )*
    ;
    
    
NUMBER
	:	(
			( N_ DOT ) => N_ DOT N_?
	        	|DOT N_
		        |N_
		)
		( 'E' ( PLUS | MINUS )? N_ )?
	;

fragment
IDENT_CHAR
    :'A' .. 'Z'
    |'a'..'z'
    ;
            
IDENT
    :    IDENT_CHAR ( IDENT_CHAR | N | '_' | '$' | '#' )*
//    |    '"' IDENT '"'    
    ;    


