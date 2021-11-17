
// $antlr-format columnLimit 150
// $antlr-format groupedAlignments false
lexer grammar GQLLexer;

SPACE:							[ \t\r\n]+ -> channel(HIDDEN);

/* reserved word */
END_NODE:						'endNode';
IN_DEGREE:						'inDegree';
L_TRIM:							'lTrim';
OUT_DEGREE:						'outDegree';
PERCENTILE_CONT:				'percentileCont';
PERCENTILE_DIST:				'percentileDist';
R_TRIM:							'rTrim';
START_NODE:						'startNode';
ST_Dev:							'stDev';
ST_Dev_P:						'stDevP';
TAIL:							'tail';
TO_LOWER:						'toLower';
TO_UPPER:						'toUpper';
/* case insensitive reserved word */
// a
ABS:							A B S;
ACOS:							A C O S;
AGGREGATE:						A G G R E G A T E;
ALIAS:							A L I A S;
ALL:							A L L;
AND:							A N D;
ADD:							A D D;
ANY:							A N Y;
ARRAY:							A R R A Y;
AS:								A S;
ASC:							A S C;
ASCENDING:						A S C E N D I N G;
ASIN:							A S I N;
AT:								A T;
ATAN:							A T A N;
AVG:							A V G;
// b
BINARY:							B I N A R Y;
BOOLEAN:						B O O L E A N;
BOTH:							B O T H;
BY:								B Y;
// c
CALL:							C A L L;
CASE:							C A S E;
CATALOG:						C A T A L O G;
CEIL:							C E I L;
CEILING:						C E I L I N G;
CHARACTER:						C H A R A C T E R;
CHARACTER_LENGTH:				C H A R A C T E R '_' L E N G T H;
CLEAR:							C L E A R;
CLONE:							C L O N E;
CLOSE:							C L O S E;
COALESCE:						C O A L E S C E;
COLLECT:						C O L L E C T;
COMMIT:							C O M M I T;
CONSTANT:						C O N S T A N T;
CONSTRUCT:						C O N S T R U C T;
COPY:							C O P Y;
COS:							C O S;
COSH:							C O S H;
COST:							C O S T;
COT:							C O T;
COUNT:							C O U N T;
CURRENT_DATE:					CURRENT '_' DATE;
CURRENT_GRAPH:					CURRENT '_' GRAPH;
CURRENT_PROPERTY_GRAPH:			CURRENT '_' PROPERTY '_' GRAPH;
CURRENT_ROLE:					CURRENT '_' ROLE;
CURRENT_SCHEMA:					C U R R E N T '_' S C H E M A;
CURRENT_TIME:					CURRENT '_' TIME;
CURRENT_TIMESTAMP:				CURRENT '_' TIMESTAMP;
CURRENT_USER:					CURRENT '_' USER;
CREATE:							C R E A T E;
// d
DATE:							D A T E;
DATETIME:						D A T E T I M E;
DECIMAL:						D E C I M A L;
DEFAULT:						D E F A U L T;
DEGREES:						D E G R E E S;
DELETE:							D E L E T E;
DETACH:							D E T A C H;
DESC:							D E S C;
DESCENDING:						D E S C E N D I N G;
DIRECTORIES:					D I R E C T O R I E S;
DIRECTORY:						D I R E C T O R Y;
DISTINCT:						D I S T I N C T;
DO:								D O;
DROP:							D R O P;
DURATION:						D U R A T I O N;
// e
ELSE:							E L S E;
END:							E N D;
ENDS:							E N D S;
EMPTY_BINDING_TABLE:			EMPTY '_' BINDING '_' TABLE;
EMPTY_GRAPH:					EMPTY '_' GRAPH;
EMPTY_PROPERTY_GRAPH:			EMPTY '_' PROPERTY '_' GRAPH;
EMPTY_TABLE:					EMPTY '_' TABLE;
EXCEPT:							E X C E P T;
EXISTS:							E X I S T S;
EXISTING:						E X I S T I N G;
EXP:							E X P;
EXPLAIN:						E X P L A I N;
// f
FALSE:							F A L S E;
FILTER:							F I L T E R;
FLOAT128:						FLOAT '128';
FLOAT32:						FLOAT '32';
FLOAT64:						FLOAT '64';
FLOAT:							F L O A T;
FLOOR:							F L O O R;
FOR:							F O R;
FROM:							F R O M;
FUNCTION:						F U N C T I O N;
FUNCTIONS:						F U N C T I O N S;
// g
GQLSTATUS:						G Q L S T A T U S;
GRANT:							G R A N T;
GROUP:							G R O U P;
// h
HAVING:							H A V I N G;
HOME_GRAPH:						HOME '_' GRAPH;
HOME_PROPERTY_GRAPH:			HOME '_' PROPERTY '_' GRAPH;
HOME_SCHEMA:					H O M E '_' S C H E M A;
// i
IN:								I N;
INSERT:							I N S E R T;
INTEGER32:						INTEGER '32';
INTEGER64:						INTEGER '64';
INTEGER16:						INTEGER '16';
INTEGER8:						INTEGER '8';
INTEGER128:						INTEGER '128';
INTEGER:						I N T E G E R;
INTERSECT:						I N T E R S E C T;
IF:								I F;
IS:								I S;
// k
KEEP:							K E E P;
// l
LEADING:						L E A D I N G;
LEFT:							L E F T;
LENGTH:							L E N G T H;
LET:							L E T;
LIKE:							L I K E;
LIKE_REGEX:						L I K E '_' R E G E X;
LIMIT:							L I M I T;
LIST:							L I S T;
LN:								L N;
LOCALDATETIME:					L O C A L D A T E T I M E;
LOCALTIME:						L O C A L T I M E;
LOCALTIMESTAMP:					L O C A L T I M E S T A M P;
LOG10:							L O G '10';
LOG:							L O G;
LOWER:							L O W E R;
// m
MANDATORY:						M A N D A T O R Y;
MAP:							M A P;
MATCH:							M A T C H;
MAX:							M A X;
MERGE:							M E R G E;
MIN:							M I N;
MOD:							M O D;
MULTI:							M U L T I;
MULTIPLE:						M U L T I P L E;
MULTISET:						M U L T I S E T;
// n
NEW:							N E W;
NOT:							N O T;
NORMALIZE:						N O R M A L I Z E;
NOTHING:						N O T H I N G;
NULL:							N U L L;
NULLIF:							N U L L I F;
// o
OCCURRENCES_REGEX:				O C C U R R E N C E S '_' R E G E X;
OCTET_LENGTH:					O C T E T '_' L E N G T H;
OF:								O F;
OFFSET:							O F F S E T;
ON:								O N;
OPTIONAL:						O P T I O N A L;
OR:								O R;
ORDER:							O R D E R;
ORDERED:						O R D E R E D;
OTHERWISE:						O T H E R W I S E;
// p
PARAMETER:						P A R A M E T E R;
PATH:							P A T H;
PATHS:							P A T H S;
PARTITION:						P A R T I T I O N;
POSITION_REGEX:					P O S I T I O N '_' R E G E X;
POWER:							P O W E R;
PROCEDURE:						P R O C E D U R E;
PROCEDURES:						P R O C E D U R E S;
PRODUCT:						P R O D U C T;
PROFILE:						P R O F I L E;
PROJECT:						P R O J E C T;
// q
QUERY:							Q U E R Y;
QUERIES:						Q U E R I E S;
// r
RADIANS:						R A D I A N S;
RECORD:							R E C O R D;
RECORDS:						R E C O R D S;
REFERENCE:						R E F E R E N C E;
REMOVE:							R E M O V E;
REPLACE:						R E P L A C E;
REQUIRE:						R E Q U I R E;
RESET:							R E S E T;
RESULT:							R E S U L T;
RETURN:							R E T U R N;
REVOKE:							R E V O K E;
RIGHT:							R I G H T;
ROLLBACK:						R O L L B A C K;
// s
SCALAR:							S C A L A R;
SCHEMA:							S C H E M A;
SCHEMAS:						S C H E M A S;
SCHEMATA:						S C H E M A T A;
SELECT:							S E L E C T;
SESSION:						S E S S I O N;
SET:							S E T;
SKIP_:							S K I P;
SIN:							S I N;
SINGLE:							S I N G L E;
SINH:							S I N H;
SQRT:							S Q R T;
START:							S T A R T;
STARTS:							S T A R T S;
STRING:							S T R I N G;
SUBSTRING:						S U B S T R I N G;
SUBSTRING_REGEX:				S U B S T R I N G '_' R E G E X;
SUM:							S U M;
// t
TAN:							T A N;
TANH:							T A N H;
THEN:							T H E N;
TIME:							T I M E;
TIMESTAMP:						T I M E S T A M P;
TRAILING:						T R A I L I N G;
TRANSLATE_REGEX:				T R A N S L A T E '_' R E G E X;
TRIM:							T R I M;
TRUE:							T R U E;
TRUNCATE:						T R U N C A T E;
// u
UNION:							U N I O N;
UNIT:							U N I T;
UNIT_BINDING_TABLE:				UNIT '_' BINDING '_' TABLE;
UNIT_TABLE:						UNIT '_' TABLE;
UNIQUE:							U N I Q U E;
UNNEST:							U N N E S T;
UNKNOWN:						U N K N O W N;
UNWIND:							U N W I N D;
UPPER:							U P P E R;
USE:							U S E;

// v
VALUE:							V A L U E;
VALUES:							V A L U E S;
// w
WHEN:							W H E N;
WHERE:							W H E R E;
WITH:							W I T H;
// x
XOR:							X O R;
// y
YIELD:							Y I E L D;
// z
ZERO:							Z E R O;

/* case insensitive non reserved word */
ACYCLIC:						A C Y C L I C;
BINDING:						B I N D I N G;
CLASS_ORIGIN:					C L A S S '_' O R I G I N;
COMMAND_FUNCTION_CODE:			C O M M A N D '_' F U N C T I O N '_' C O D E;
CONDITION_NUMBER:				C O N D I T I O N '_' N U M B E R;
CONNECTING:						C O N N E C T I N G;
DESTINATION:					D E S T I N A T I O N;
DIRECTED:						D I R E C T E D;
EDGE:							E D G E;
EDGES:							E D G E S;
FINAL:							F I N A L;
GRAPH:							G R A P H;
GRAPHS:							G R A P H S;
GROUPS:							G R O U P S;
INDEX:							I N D E X;
LABEL:							L A B E L;
LABELS:							L A B E L S;
MESSAGE_TEXT:					M E S S A G E '_' T E X T;
MORE_:							M O R E;
MUTABLE:						M U T A B L E;
NFC:							N F C;
NFD:							N F D;
NFKC:							N F K C;
NFKD:							N F K D;
NODE:							N O D E;
NODES:							N O D E S;
NORMALIZED:						N O R M A L I Z E D;
NUMBER:							N U M B E R;
ONLY:							O N L Y;
ORDINALITY:						O R D I N A L I T Y;
PATTERN:						P A T T E R N;
PATTERNS:						P A T T E R N S;
PROPERTY:						P R O P E R T Y;
PROPERTIES:						P R O P E R T I E S;
READ:							R E A D;
RELATIONSHIP:					R E L A T I O N S H I P;
RELATIONSHIPS:					R E L A T I O N S H I P S;
RETURNED_GQLSTATUS:				R E T U R N E D '_' G Q L S T A T U S;
SHORTEST:						S H O R T E S T;
SIMPLE:							S I M P L E;
SUBCLASS_ORIGIN:				S U B C L A S S '_' O R I G I N;
TABLE:							T A B L E;
TABLES:							T A B L E S;
TIES:							T I E S;
TO:								T O;
TRAIL:							T R A I L;
TRANSACTION:					T R A N S A C T I O N;
TYPE:							T Y P E;
TYPES:							T Y P E S;
UNDIRECTED:						U N D I R E C T E D;
VERTEX:							V E R T E X;
VERTICES:						V E R T I C E S;
WALK:							W A L K;
WRITE:							W R I T E;
ZONE:							Z O N E;

/* punctuation */
P_AMPERSAND:					'&';
P_STAR:							'*';
P_circumflex:					'^';
P_COLON:						':';
P_DOLLAR:						'$';
P_DOUBLE_QUOTE:					'"';
P_EQUAL:						'=';
P_NOT_EQUAL:					'<>';
P_LESS_THAN:					P_L_ANGLE_BRACKET;
P_LESS_EQUAL:					'<=';
P_GREATE_THAN:					P_R_ANGLE_BRACKET;
P_GREATE_EQUAL:					'>=';
P_EXCLAMATION:					'!';
P_R_ANGLE_BRACKET:				'>';
P_BACKQUOTE:					'`';
P_L_BRACE:						'{';
P_L_BRACKET:					'[';
P_L_PAREN:						'(';
P_L_ANGLE_BRACKET:				'<';
P_MINUS:						'-';
P_PERCENT:						'%';
P_DOT:							'.';
P_DOUBLE_DOT:					'..';
P_PLUS:							'+';
P_QUESTION:						'?';
P_QUOTE:						'\'';
P_REVERSE_SOLIDUS:				'\\';
P_R_BRACE:						'}';
P_R_BRACKET:					']';
P_R_PAREN:						')';
P_SEMI:							';';
P_SOLIDUS:						'/';
P_TILDE:						'~';
P_UNDERSCORE:					'_';
P_V_BAR:						'|';
P_COMMA:						',';
P_CONCATENATION:				'||';
P_MULTISET_ALTERNATION:			'|+|';
P_EXP:                          E;
// raw
LEFT_MINUS_SLASH:				'<-/';
SLASH_MINUS:					'/-';
TILDE_SLASH:					'~/';
SLASH_TILDE:					'/~';
MINUS_SLASH:					'-/';
SLASH_MINUS_RIGHT:				'/->';
LEFT_TILDE_SLASH:				'<~/';
SLASH_TILDE_RIGTH:				'/~>';
MINUS_LEFT_BRACKET:				'-[';
BRACKET_RIGHT_ARROW:			']->';
LEFT_ARROW_BRACKET:				'<-[';
RIGHT_BRACKET_MINUS:			']-';
TILDE_LEFT_BRACKET:				'~[';
LEFT_ARROW_TILDE_BRACKET:		'<~[';
RIGHT_BRACKET_TILDE:			']~';
BRACKET_TILDE_RIGHT_ARROW:		']~>';
RIGHT_ARROW:					'->';
LEFT_ARROW_TILDE:				'<~';
TILDE_RIGHT_ARROW:				'~>';
LEFT_MINUS_RIGHT:				'<->';
LEFT_ARROW:						'<-';

// other

NODE_SYNONYM:					NODE | VERTEX;
EDGE_SYNONYM:					EDGE | RELATIONSHIP;
IF_EXISTS:						IF EXISTS;
IF_NOT_EXISTS:					IF NOT EXISTS;

// identifier
IDENTIFIER:						ID_START ID_CONTINUE*;
DELIMITED_IDENTIFIER:			P_BACKQUOTE ('\\' . | '``' | ~('`' | '\\'))* P_BACKQUOTE;

// literal
DOUBLE_QUOTE_STRING_LITERAL:	'"' ('\\' . | '""' | ~('"' | '\\'))* '"';
QUOTE_STRING_LITERAL:			'\'' ('\\' . | '\'\'' | ~('\'' | '\\'))* '\'';
STRING_LITERAL:					QUOTE_STRING_LITERAL | DOUBLE_QUOTE_STRING_LITERAL;

UNSIGNED_BIN_INTEGER:			'0b' BIN_DIGIT (P_UNDERSCORE? BIN_DIGIT)*;
UNSIGNED_DECIMAL_INTEGER:		DIGIT ( P_UNDERSCORE? DIGIT)*;
UNSIGNED_HEX_INTEGER:			'0x' HEX_DIGIT (P_UNDERSCORE? HEX_DIGIT)*;
UNSIGNED_OCT_INTEGER:			'0o' OCT_DIGIT (P_UNDERSCORE? OCT_DIGIT)*;
BINARY_STRING_LITERAL:          X '\'' HEX_DIGIT+ '\'';
fragment USER:					U S E R;
fragment CURRENT:				C U R R E N T;
fragment HOME:					H O M E;
fragment ROLE:					R O L E;
fragment EMPTY:					E M P T Y;
fragment HEX_DIGIT:				[0-9a-fA-F];
fragment OCT_DIGIT:				[0-7];
fragment BIN_DIGIT:				[01];
fragment DIGIT:					[0-9];
fragment A:						[aA];
fragment B:						[bB];
fragment C:						[cC];
fragment D:						[dD];
fragment E:						[eE];
fragment F:						[fF];
fragment G:						[gG];
fragment H:						[hH];
fragment I:						[iI];
fragment J:						[jJ];
fragment K:						[kK];
fragment L:						[lL];
fragment M:						[mM];
fragment N:						[nN];
fragment O:						[oO];
fragment P:						[pP];
fragment Q:						[qQ];
fragment R:						[rR];
fragment S:						[sS];
fragment T:						[tT];
fragment U:						[uU];
fragment V:						[vV];
fragment W:						[wW];
fragment X:						[xX];
fragment Y:						[yY];
fragment Z:						[zZ];
fragment ID_START:				[\p{L}\p{Nl}];
fragment ID_CONTINUE:			[\p{ID_Start}\p{Mn}\p{Mc}\p{Nd}\p{Pc}];
