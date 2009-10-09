grammar oracle10;

options {
	language=Java;
	k=*;
	backtrack=true;
	memoize=true;
	output=AST;
}

start_rule
	:	select EOF
	;


select	
	:	subquery  SEMICOLON
	;

subquery
	:	subquery_factoring_clause SELECT (hint)? ((ALL|UNIQUE|DISTINCT))? select_list 
	FROM ((join_clause|(LB join_clause RB)|((table_reference)(COMMA table_reference)*))+
	(where_clause)? (hierarchical_query_clause)? (group_by_clause)?
	(HAVING condition)? (model_clause)?
	(((UNION (ALL)?)|INTERSECT|MINUS)) LB subquery RB )? (order_by_clause)?
	;

fragment
hint
	:
	;
	
subquery_factoring_clause
	:	WITH (query_name AS LB subquery RB)(COMMA query_name AS LB subquery RB)*	
	;
	
fragment
query_name
	:	IDENT
	;	
	
select_list
	:	((select_list1 (COMMA select_list1)* )
	| ASTERISK)
	;

fragment	
select_list1
	:	(((query_name|((schema DOT)? IDENT)) DOT ASTERISK)
	|	(expr ((AS)? c_alias)?))
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
	:	ONLY LB query_table_expression RB flashback_query_clause? t_alias?
	|	query_table_expression flashback_query_clause? t_alias?	
	;
	
fragment	
t_alias
	:	IDENT
	;

	
flashback_query_clause
	:	(VERSIONS BETWEEN (SCN| TIMESTAMP) (expr| MINVALUE) AND (expr|MAXVALUE))? AS OF (SCN|TIMESTAMP) expr
	;

query_table_expression
	:	query_name
	|	(schema DOT)? query_table_expression1
	|	LB subquery subquery_restriction_clause? RB
	|	table_collection_expression
	;	 
	
fragment	 	
query_table_expression1
	:	TABLE (query_table_expression2 | AT_SIGN dblink)?
	|	VIEW AT_SIGN dblink
	|	MATERIALIZED VIEW AT_SIGN dblink
	;
	
fragment	
dblink
	: 	IDENT
	;	
	
query_table_expression2
	:	(( PARTITION LB partition RB)|( SUBPARTITION LB subpartition RB))? sample_clause?
	|	sample_clause
	|	AT_SIGN dblink
	;
	
fragment	
partition
	:	IDENT
	;
	
fragment	
subpartition
	:	IDENT
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
	:	integer
	;	
	
subquery_restriction_clause
	:	WITH (( READ ONLY)|( CHECK OPTION ( CONSTRAINT constraint)? ))	
	;
	
fragment	
constraint
	:	condition
	;	
	
table_collection_expression
	:	TABLE LB collection_expression 	RB (LB PLUS RB)+
	;

collection_expression
	:	IDENT
	;

fragment
join_clause
	:	table_reference	(inner_cross_join_clause|outer_join_clause)*
	;

fragment
inner_cross_join_clause
	:	INNER? JOIN table_reference (( ON condition)|( USING LB inner_cross_join_clause1(COMMA inner_cross_join_clause1)* RB))	
	|	( CROSS|( NATURAL INNER?)) JOIN table_reference
	;
	
fragment		
column
	:	IDENT
	;

fragment
inner_cross_join_clause1
	:	column
	;


outer_join_clause
	:	query_partition_clause? NATURAL? outer_join_type JOIN 
	table_reference query_partition_clause?	((ON condition)|(USING inner_cross_join_clause1(COMMA inner_cross_join_clause1)*))?		 	
	;
	
query_partition_clause
	:	PARTITION BY (query_partition_clause1|(LB query_partition_clause1 RB))	
	;
	
fragment	
query_partition_clause1
	:	column(COMMA column)*
	;	
	
outer_join_type
	:	( FULL | LEFT | RIGHT ) OUTER?
	;

where_clause
	:	WHERE condition
	;
	
hierarchical_query_clause 
	:	( START WITH condition )? CONNECT BY NOCYCLE? condition
	;
	
group_by_clause
	:	GROUP BY group_by_clause(COMMA group_by_clause)* (HAVING condition)?
	;
		
fragment
group_by_clause1			
	:	expr
	|	rollup_cube_clause
	|	grouping_sets_clause
	;
	
rollup_cube_clause
	:	( ROLLUP | CUBE ) LB grouping_expression_list RB	
	;
	
grouping_sets_clause
	:	GROUPING SETS LB grouping_sets_clause1 (COMMA grouping_sets_clause)* RB	
	;
	
fragment	
grouping_sets_clause1
	: 	rollup_cube_clause
	|	grouping_expression_list
	;
	
grouping_expression_list
	:	expression_list(COMMA expression_list)*
	;
	
expression_list
	:	expr
	|	LB expr RB
	;
	
model_clause
	:	MODEL cell_reference_options? return_rows_clause? reference_model* main_model
	;
	
cell_reference_options
	:	(( IGNORE | KEEP ) NAW )? (UNIQUE ( DIMENSION |( SINGLE REFERENCE)))?	
	;
	
return_rows_clause
	:	RETURN ( UPDATED|ALL) ROWS
	;
	
reference_model
	:	REFERENCE reference_model_name ON LB subquery RB model_column_clauses cell_reference_options?
	;

fragment
reference_model_name
	:	IDENT
	;	
	
main_model
	:	( MAIN main_model_name)? model_column_clauses cell_reference_options? model_rules_clause
	;
	
fragment	
main_model_name
	:	IDENT
	;	
	
model_column_clauses
	:	(query_partition_clause c_alias?)? DIMENSION BY LB model_column(COMMA model_column)*RB 
	MEASURES LB model_column(COMMA model_column)* RB
	;
	
model_column
	:	expr (AS? c_alias)?	
	;
	
model_rules_clause
	:	( RULES (( UPDATE|( UPSERT ALL?)))?)? (( AUTOMATIC| SECUENTIAL) ORDER)?
	( ITERATE)? LB number RB ( UNTIL LB condition RB)?
	LB model_rules_clause1(COMMA model_rules_clause1)* RB					 	
	;
	
fragment	
model_rules_clause1
	:	((UPDATE|(UPSERT ALL?)))? cell_assignment order_by_clause? '=' expr
	;
	
cell_assignment
	:	measure_column LSB ((cell_assignment1(COMMA cell_assignment1)*)|multi_column_for_loop) RSB	
	;
	
measure_column
	:	IDENT
	;	
	
fragment	
cell_assignment1
	:	condition
	|	expr
	|	single_column_for_loop
	;
	
single_column_for_loop
	:	FOR dimension_column single_column_for_loop1		
	;
	
dimension_column
	:	IDENT
	;	
	
fragment	
single_column_for_loop1
	:	IN LB ((literal(COMMA literal)*)|subquery)  RB	
	|	( LIKE pattern)? FROM literal TO literal ( INCREMENT| DECREMENT) literal
	;
	
fragment	
pattern
	:	expr
	;	
	
multi_column_for_loop
	:	FOR dimension_column(COMMA dimension_column)* RB IN ((multi_column_for_loop1(COMMA multi_column_for_loop1)*)|subquery)
	;
	
fragment	
multi_column_for_loop1
	:	LB literal(COMMA literal)* RB
	;
		
literal
	:	IDENT
	;		
							
order_by_clause
	:	ORDER SIBLINGS? BY order_by_clause1(COMMA order_by_clause1)*
	;
	
fragment	
order_by_clause1
	:	(expr|c_alias) ( ASC| DESC)? (( NULLS FIRST)|(NULLS LAST))?	
	;
	
	
for_update_clause
	:	FOR UPDATE (OF for_update_clause1(COMMA)for_update_clause1)? ( NOWAIT|( WAIT integer))?	
	;
	
fragment	
for_update_clause1
	:	((schema DOT)?(IDENT)DOT)? column
	;
	
condition
	:	comparison_condition
	|	floating_point_condition
	|	logical_condition
	|	model_condition
	|	multiset_condition
	|	pattern_matching_condition
	|	range_condition
	|	null_condition
	|	XML_condition
	|	( LB condition RB|NOT expr|expr ( AND | OR ) expr )
	|	exists_condition
	|	in_condition
	|	is_of_type_condition
	;
	
comparison_condition
	:	simple_comparison_condition
	|	group_comparison_condition	
	;
	
simple_comparison_condition
	: 	expr ( '=' | '!=' | '^=' | '<>' | '>' | '<' | '>=' | '<=' )  expr
	|	LB expr (COMMA expr )* RB ( '=' | '!=' | '^=' | '<>' ) LB subquery RB
	;

group_comparison_condition
	: 	expr ( '=' | '!=' | '^=' | '<>' | '>' | '<' | '>=' | '<=' ) ( ANY | SOME | ALL )
     	LB ( expression_list | subquery ) RB
	|	expr (COMMA expr)* ( '=' | '!=' | '^=' | '<>' ) ( ANY | SOME | ALL )
	LB ((expression_list (COMMA expression_list)*)|subquery) RB
	;

floating_point_condition
	:	expr IS NOT? ( NAN | INFINITE )
	;
	
logical_condition
	:	expr AND expr
	|	expr OR	expr
	|	NOT expr
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
	:	is_a_set_conditions
	|	is_empty_conditions
	|	member_condition
	|	submultiset_conditions
	;
	
is_a_set_conditions
	:	nested_table IS NOT? A_SIGN SET
	;
	
nested_table
	:	IDENT
	;	
	
is_empty_conditions
	:	nested_table IS NOT? EMPTY
	;

member_condition
	:	expr NOT? MEMBER OF? nested_table
	;
	
submultiset_conditions
	:	nested_table NOT? SUBMULTISET OF? nested_table
	;
	
pattern_matching_condition
	:	like_condition
	|	regexp_like_condition
	;
	
like_condition
	:	char NOT? ( LIKE | LIKEC | LIKE2 | LIKE4 ) char ( ESCAPE char)?
	;
	
char
	:	( 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '#' )
	;	
	
regexp_like_condition
	:	REGEXP_LIKE LB char COMMA pattern (COMMA match_parameter )? RB
	;
	
match_parameter
	:	expr
	;	
	
range_condition
	:	expr NOT? BETWEEN expr AND expr
	;

null_condition
	:	expr IS NOT? NULL
	;

XML_condition
	:	
	;
	
exists_condition
	:	EXISTS LB subquery RB
	;

in_condition
	:	expr NOT? IN LB ( expression_list | subquery ) RB
	|	LB expr (COMMA expr)* RB NOT? IN LB ( (expression_list(COMMA expression_list)*)| subquery) RB
	;

is_of_type_condition
	:	expr IS NOT? OF TYPE LB is_of_type_condition1 (COMMA is_of_type_condition1)* RB
	;
	
fragment	
is_of_type_condition1
	: ONLY? (schema DOT) type
	;

type
	:	IDENT
	;

expr
	: 	simple_expression
	|	compound_expression
	|	case_expression
	|	cursor_expression
	|	datetime_expression
	|	function_expression
	|	interval_expression
	|	object_access_expression
	|	scalar_subquery_expression
	|	model_expression
	|	type_constructor_expression
	|	variable_expression
	;
	
simple_expression
	: 	( query_name DOT | (schema DOT IDENT DOT)? ( column | ROWID ))
	|	ROWNUM
	|	string
	|	number
	|	sequence DOT ( CURRVAL | NEXTVAL )
	|	NULL
	;

string
	:	QUOTE char* QUOTE
	;


integer
	:	NUMBER
	;

number
	:	 NUMBER
	;

sequence
	:	IDENT
	;

compound_expression
	:	LB expr RB
	|	( PLUS | MINUS | PRIOR ) expr
	|	expr_add
	|	expr_mul
	|	expr_sign
	;

expr_add
	:	expr_mul (PLUS | MINUS | DOUBLEVERTBAR) expr_mul
	;
	
expr_mul
	:	expr_sign ( ( ASTERISK | DIVIDE ) expr_sign )*	
	;	

expr_sign
	:	( PLUS | MINUS ) expr
	;


case_expression
	:	CASE ( simple_case_expression | searched_case_expression) else_clause? END
	;

simple_case_expression
	:	expr ( WHEN comparison_expr THEN expr)+
	;
	
comparison_expr
	:	expr
	;	
	
searched_case_expression
	:	(WHEN condition THEN expr)+
	;

else_clause
	:	ELSE expr
	;

cursor_expression
	:	CURSOR LB subquery RB
	;

datetime_expression
	:	datetime_value_expr AT ( LOCAL | ('TIME' 'ZONE' datetime_expression1))
	;
	
datetime_value_expr
	:
	;	
	
datetime_expression1
//	:	' [ + | - ] hh:mm'
	:	DBTIMEZONE
// 	:	' time_zone_name '
	|	expr
        ;
               
function_expression
	:
	;
	

interval_expression
	:	interval_value_expr interval_expression1
	;
   
interval_value_expr
	:
//	:	expr
	;   
   
interval_expression1
	:	DAY ( LB leading_field_precision RB )? TO SECOND ( LB fractional_second_precision RB )?
	|	YEAR ( LB leading_field_precision RB )? TO MONTH
	;

leading_field_precision
	:	integer
	;

fractional_second_precision
	:	integer
	;

object_access_expression
	:
	;
	
scalar_subquery_expression
	:	subquery
	;
	
model_expression
	:
	;
	
type_constructor_expression
	:
	;
	
variable_expression
	:	COLON host_variable (INDICATOR? COLON indicator_variable)?
	;
	
fragment	
host_variable
	:	IDENT
	;	
	
fragment	
indicator_variable
	:	IDENT
	;	
	
IDENT
	:	'A' .. 'Z' ( 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '#' )*
	|	'"' 'A' .. 'Z' ( 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '#' )* '"'	
	;	


	
QUOTE
	:	'\''
	;

	
N
	: '0' .. '9' ( '0' .. '9' )*
	;
	
	
NUMBER
	:	(	( N DOT N ) => N DOT N
		|	DOT N
		|	N
		)
		( 'E' ( PLUS | MINUS )? N )?
	;
			



SELECT
	:	'SELECT'
	;

ALL
	:	'ALL'
	;

UNIQUE
	:	'UNIQUE'
	;

DISTINCT
	:	'DISTINCT'
	;

FROM
	:	'FROM'
	;

HAVING
	:	'HAVING'
	;

UNION
	:	'UNION'
	;

INTERSECT
	:	'INTERSECT'
	;

MINUS
	:	'MINUS'
	;

LB
	:	'('
	;

RB
	:	')'
	;

WITH
	:	'WITH'
	;

AS
	:	'AS'
	;

COMMA
	:	','
	;

DOT
	:	'.'
	;

ONLY
	:	'ONLY'
	;

VERSIONS
	:	'VERSIONS'
	;

BETWEEN
	:	'BETWEEN'
	;

SCN
	:	'SCN'
	;

TIMESTAMP
	:	'TIMESTAMP'
	;

MINVALUE
	:	'MINVALUE'
	;

AND
	:	'AND'
	;

MAXVALUE
	:	'MAXVALUE'
	;

OF
	:	'OF'
	;

TABLE
	:	'TABLE'
	;

VIEW
	:	'VIEW'
	;

MATERIALIZED
	:	'MATERIALIZED'
	;

PARTITION
	:	'PARTITION'
	;

SUBPARTITION
	:	'SUBPARTITION'
	;

AT_SIGN
	:	'@'
	;

SAMPLE
	:	'SAMPLE'
	;

BLOCK
	:	'BLOCK'
	;

SEED
	:	'SEED'
	;

READ
	:	'READ'
	;

CHECK
	:	'CHECK'
	;

OPTION
	:	'OPTION'
	;

CONSTRAINT
	:	'CONSTRAINT'
	;

INNER
	:	'INNER'
	;

JOIN
	:	'JOIN'
	;

ON
	:	'ON'
	;

USING
	:	'USING'
	;

CROSS
	:	'CROSS'
	;

NATURAL
	:	'NATURAL'
	;

FULL
	:	'FULL'
	;

LEFT
	:	'LEFT'
	;

RIGHT
	:	'RIGHT'
	;

OUTER
	:	'OUTER'
	;

WHERE
	:	'WHERE'
	;

START
	:	'START'
	;

CONNECT
	:	'CONNECT'
	;

BY
	:	'BY'
	;

NOCYCLE
	:	'NOCYCLE'
	;

GROUP
	:	'GROUP'
	;

ROLLUP
	:	'ROLLUP'
	;

CUBE
	:	'CUBE'
	;

GROUPING
	:	'GROUPING'
	;

SETS
	:	'SETS'
	;

MODEL
	:	'MODEL'
	;

IGNORE
	:	'IGNORE'
	;

KEEP
	:	'KEEP'
	;

NAW
	:	'NAW'
	;

DIMENSION
	:	'DIMENSION'
	;

SINGLE
	:	'SINGLE'
	;

REFERENCE
	:	'REFERENCE'
	;

RETURN
	:	'RETURN'
	;

UPDATED
	:	'UPDATED'
	;

ROWS
	:	'ROWS'
	;

MAIN
	:	'MAIN'
	;

MEASURES
	:	'MEASURES'
	;

RULES
	:	'RULES'
	;

UPDATE
	:	'UPDATE'
	;

UPSERT
	:	'UPSERT'
	;

AUTOMATIC
	:	'AUTOMATIC'
	;

SECUENTIAL
	:	'SECUENTIAL'
	;

ORDER
	:	'ORDER'
	;

ITERATE
	:	'ITERATE'
	;

UNTIL
	:	'UNTIL'
	;

LSB
	:	'['
	;

RSB
	:	']'
	;

FOR
	:	'FOR'
	;

IN
	:	'IN'
	;

LIKE
	:	'LIKE'
	;

TO
	:	'TO'
	;

INCREMENT
	:	'INCREMENT'
	;

DECREMENT
	:	'DECREMENT'
	;

SIBLINGS
	:	'SIBLINGS'
	;

ASC
	:	'ASC'
	;

DESC
	:	'DESC'
	;

NULLS
	:	'NULLS'
	;

FIRST
	:	'FIRST'
	;

LAST
	:	'LAST'
	;

NOWAIT
	:	'NOWAIT'
	;

WAIT
	:	'WAIT'
	;

ANY
	:	'ANY'
	;

SOME
	:	'SOME'
	;

IS
	:	'IS'
	;

NOT
	:	'NOT'
	;

NAN
	:	'NAN'
	;

INFINITE
	:	'INFINITE'
	;

PRESENT
	:	'PRESENT'
	;

A_SIGN
	:	'A'
	;

SET
	:	'SET'
	;

EMPTY
	:	'EMPTY'
	;

MEMBER
	:	'MEMBER'
	;

SUBMULTISET
	:	'SUBMULTISET'
	;

LIKEC
	:	'LIKEC'
	;

LIKE2
	:	'LIKE2'
	;

LIKE4
	:	'LIKE4'
	;

ESCAPE
	:	'ESCAPE'
	;

REGEXP_LIKE
	:	'REGEXP_LIKE'
	;

NULL
	:	'NULL'
	;

OR
	:	'OR'
	;

EXISTS
	:	'EXISTS'
	;

TYPE
	:	'TYPE'
	;

ASTERISK
	:	'*'
	;

DOT_ASTERISK
	:	'.*'
	;

ROWID
	:	'ROWID'
	;

ROWNUM
	:	'ROWNUM'
	;

CURRVAL
	:	'CURRVAL'
	;

NEXTVAL
	:	'NEXTVAL'
	;

PRIOR
	:	'PRIOR'
	;

CASE
	:	'CASE'
	;

END
	:	'END'
	;

WHEN
	:	'WHEN'
	;

THEN
	:	'THEN'
	;

ELSE
	:	'ELSE'
	;

CURSOR
	:	'CURSOR'
	;

AT
	:	'AT'
	;

LOCAL
	:	'LOCAL'
	;

DBTIMEZONE
	:	'DBTIMEZONE'
	;

DAY
	:	'DAY'
	;

YEAR
	:	'YEAR'
	;

SECOND
	:	'SECOND'
	;

MONTH
	:	'MONTH'
	;

INDICATOR
	:	'INDICATOR'
	;

PLUS
	:	'+'
	;



SEMICOLON
	:	';'
	;

COLON
	:	'\:'
	;

DOUBLEVERTBAR
	:	'||'
	;

DIVIDE
	:	'/'
	;
