parser grammar GQLParser;

options {
	tokenVocab = GQLLexer;
}

root: requests? EOF;

requests: (gqlRequest P_SEMI | emptyRequest)* (
		gqlRequest P_SEMI?
	)?
	| emptyRequest;

emptyRequest: P_SEMI;
/* Section 6 */

// Section 6.1 <GQL-request>
gqlRequest: requestParameterSet? gqlProgram;

// Sectoin 6.2 request parameter set
requestParameterSet:
	requestParameter (P_COMMA requestParameter)*;

requestParameter: parameterDefinition;
// Section 6.3 <GQL-program>
gqlProgram: preamble? mainActivity;
mainActivity:
	sessionActivity
	| sessionActivity? (transactionActivity sessionActivity?)+ sessionCloseCommand?
	| sessionCloseCommand;
sessionActivity:
	sessionClearCommand sessionParameterCommand*
	| sessionParameterCommand+;
sessionParameterCommand:
	sessionSetCommand
	| sessionRemoveCommand;
transactionActivity:
	startTransactionCommand (
		procedureSpecification endTransactionCommand?
	)?
	| procedureSpecification endTransactionCommand?
	| endTransactionCommand;

// Section 6.4 <preamble>
preamble: preambleOption (P_COMMA preambleOption)*;
preambleOption:
	PROFILE
	| EXPLAIN
	| preambleOptionIdentifier (P_EQUAL literal)?;
preambleOptionIdentifier: identifier;

/* Section 7 Session management */
// Section 7.1 <session set command>
sessionSetCommand:
	SESSION SET (
		sessionSetSchemaClause
		| sessionSetGraphClause
		| sessionSetTimeZoneClause
		| sessionSetParameterClause
	);
sessionSetSchemaClause: schemaResolutionExpression;
sessionSetGraphClause: graphResolutionExpression;
sessionSetTimeZoneClause: TIME ZONE setTimeZoneValue;
setTimeZoneValue: stringValueExpression;
sessionSetParameterClause:
	sessionParameterFlag? sessionParameter (IF NOT EXISTS)?;
sessionParameter: PARAMETER parameterDefinition;
sessionParameterFlag: MUTABLE | FINAL;
// Section 7.2 <session remove command>
sessionRemoveCommand: SESSION? REMOVE parameter (IF EXISTS)?;
// Section 7.3 <session clear command>
sessionClearCommand: SESSION? CLEAR;
// Section 7.4 <session close command>
sessionCloseCommand: SESSION? CLOSE;
/* Section 8 Transaction Management */
// Section 8.1 <start transaction command>
startTransactionCommand:
	START TRANSACTION transactionCharacteristics?;
//Section 8.2 <end transaction command>
endTransactionCommand: commitCommand | rollbackCommand;
//Section 8.3 <transactin characteristics>
transactionCharacteristics:
	transactionMode (P_COMMA transactionMode)*;
transactionMode:
	transactionAccessMode
	| implementationDefinedAccessMode;
transactionAccessMode: READ ONLY | READ WRITE;
implementationDefinedAccessMode:; //TODO: Syntax Rules
// Section 8.4 <rollback command>
rollbackCommand: ROLLBACK;
// Section 8.5 <commit command>
commitCommand: COMMIT;
/* Section 9 Procedure */
// Section 9.1 <procedure specification>
nestedProcedureSpecification: '(' procedureSpecification ')';
procedureSpecification:
	catalogModifyingProcedureSpecification
	| dataModifyingProcedureSpecification
	| querySpecification
	| functionSpecification;
nestedCatalogModifyingProcedureSpecification:
	'(' catalogModifyingProcedureSpecification ')';
catalogModifyingProcedureSpecification:
	procedureBody; // NOTE: Predicative production rule.
nestedDataModifyingProcedureSpecification:
	'(' dataModifyingProcedureSpecification ')';
dataModifyingProcedureSpecification:
	procedureBody; //NOTE: Predicative production rule.
// Section 9.2 <query specification>
nestedQuerySpecification: '(' querySpecification ')';
querySpecification:
	procedureBody; //NOTE: Predicative production rule.
// Section 9.3 <function specification>
nestedFunctionSpecification: '(' functionSpecification ')';
functionSpecification:
	procedureBody; //NOTE: Predicative production rule.
// Section 9.4 <procedure body>
procedureBody: definitionBlock? statementBlock;
definitionBlock:
	staticVariableDefinition* bindingVariableDefinition*;
statementBlock: statement (THEN statement)*;

/* Section 10 Variable and parameter declarations and definitions */
// Section 10.1 Static variable definitions
staticVariableDefinitionList:
	staticVariableDefinition (P_COMMA staticVariableDefinition)*;
staticVariableDefinition:
	schemaVariableDefinition
	| graphTypeVariableDefinition
	| procedureVariableDefinition
	| queryVariableDefinition
	| functionVariableDefinition
	| pathPatternVariableDefinition;
// Section 10.2 Schema variable definition
schemaVariableDefinition:
	SCHEMA staticVariableName schemaInitializer;
schemaInitializer:
	(AS | P_EQUAL) schemaReference
	| P_COLON catalogSchemaReference;
// Section 10.3 Graph type variable definition
graphTypeVariableDefinition:
	PROPERTY? GRAPH TYPE graphTypeVariable graphTypeInitializer;
graphTypeVariable: bindingVariableName;
graphTypeInitializer:
	asGraphType
	| P_COLON catalogGraphTypeReference;
// Section 10.4 Procedure variable definition
procedureVariableDefinition:
	CATALOG? PROCEDURE procedureVariable ofTypeSignature procedureInitializer;
procedureVariable: staticVariableName;
procedureInitializer:
	(AS | P_EQUAL) procedureReference
	| AS? nestedProcedureSpecification
	| P_COLON catalogProcedureReference;
// Section 10.5 Query variable definition
queryVariableDefinition:
	QUERY queryVariable ofTypeSignature queryInitializer;
queryVariable: staticVariableName;
queryInitializer:
	(AS | P_EQUAL) queryReference
	| AS? nestedQuerySpecification
	| P_COLON catalogQueryReference;
// Section 10.6 Functin variable definition
functionVariableDefinition:
	FUNCTION functionVariable ofTypeSignature functionInitializer;
functionVariable: staticVariableName;
functionInitializer:
	(AS | P_EQUAL) functionReference
	| AS? nestedFunctionSpecification
	| P_COLON catalogFunctionReference;
// Section 10.7 Path pattern variable definition
pathPatternVariableDefinition:
	PATH PATTERN pathPatternVariable pathPatternInitializer;
pathPatternInitializer:
	(AS | P_EQUAL)? pathPatternDefinition
	| P_COLON catalogPathPatternReference;
pathPatternDefinition:
	COPY OF pathPatternReference
	| pathPatternExpression;
pathPatternVariable: staticVariableName;
// Section 10.8 Binding variable and parameter declarations and definitions 
compactVariableDeclarationList:
	compactVariableDeclaration (
		P_COMMA compactVariableDeclaration
	)*;
compactVariableDeclaration:
	bindingVariableDeclaration
	| valueVariable;
bindingVariableDeclaration:
	graphVariableDeclaration
	| bindingTableVariableDeclaration
	| valueVariableDeclaration;
compactVariableDefinitionList:
	compactVariableDefinition (P_COMMA compactVariableDefinition)*;
compactVariableDefinition:
	compactValueVariableDefinition
	| bindingVariableDefinition;
compactValueVariableDefinitionList:
	compactValueVariableDefinition (
		P_COMMA compactValueVariableDefinition
	)*;
compactValueVariableDefinition:
	valueVariable P_EQUAL valueExpression;
bindingVariableDefinitionList:
	bindingVariableDefinition (P_COMMA bindingVariableDefinition)*;
bindingVariableDefinition:
	graphVariableDefinition
	| bindingTableVariableDefinition
	| valueVariableDefinition;
optionalBindingVariableDefinition:
	optionalGraphVariableDefinition
	| optionalBindingTableVariableDefinition
	| optionalValueVariableDefinition;
parameterDefinition:
	graphParameterDefinition
	| bindingTableParameterDefinition
	| valueParameterDefinition;
// Section 10.9 Graph variable and parameter declaration and definition
graphVariableDeclaration:
	PROPERTY? GRAPH graphVariable ofGraphType;
optionalGraphVariableDefinition: graphVariableDefinition;
graphVariableDefinition:
	PROPERTY? GRAPH graphVariable ofGraphType graphInitializer;
graphParameterDefinition:
	PROPERTY? GRAPH parameterName IF_NOT_EXISTS? ofGraphType graphInitializer;
graphVariable: bindingVariableName;
graphInitializer:
	(AS | P_EQUAL) graphExpression
	| AS? nestedGraphQuerySpecification
	| P_COLON catalogGraphTypeReference;

// Section 10.10 Binding table variable and parameter declaration and definition
bindingTableVariableDeclaration:
	BINDING? TABLE bindingTableVariable ofBindingTableType;
optionalBindingTableVariableDefinition:
	bindingTableVariableDefinition;
bindingTableVariableDefinition:
	BINDING? TABLE bindingTableVariable ofBindingTableType bindingTableInitializer;
bindingTableParameterDefinition:
	BINDING? TABLE parameter IF_NOT_EXISTS? ofBindingTableType bindingTableInitializer;
bindingTableVariable: bindingVariableName;
bindingTableInitializer:
	(AS | P_EQUAL) bindingTableReference
	| AS? nestedQuerySpecification
	| P_COLON catalogBindingTableReference;
// Section 10.11 Value variable and parameter declaration and definition
valueVariableDeclaration: VALUE valueVariable ofValueType;
optionalValueVariableDefinition: valueVariableDefinition;
valueVariableDefinition:
	VALUE valueVariable ofValueType? valueInitializer;
valueParameterDefinition:
	VALUE parameter IF_NOT_EXISTS? ofValueType? valueInitializer;
valueVariable: bindingVariableName;
valueInitializer:
	(AS | P_EQUAL) valueExpression
	| AS? nestedQuerySpecification
	| P_COLON catalogObjectReference;

/* Section 11 Object expressions */

//Section 11.2 <primary result object expression>
primaryResultObjectExpression:
	graphExpression
	| bindingTableReference;
// Section 11.3 <graph expression>
graphExpression:
	copyGraphExpression
	| graphSpecification
	| graphReference;
copyGraphExpression: COPY OF graphExpression;
// Section 11.4 <graph type expression>
graphTypeExpression:
	copyGraphTypeExpression
	| likeGraphExpression
	| graphTypeSpecification
	| graphTypeReference;
asGraphType:
	(AS | P_EQUAL) graphTypeExpression
	| likeGraphExpressionShorthand
	| AS? nestedGraphTypeSpecification;
copyGraphTypeExpression: COPY OF graphTypeReference;
likeGraphExpression:
	PROPERTY? GRAPH TYPE likeGraphExpressionShorthand;
ofGraphType:
	ofTypePrefix? graphTypeExpression
	| likeGraphExpressionShorthand
	| ofTypePrefix? nestedGraphTypeSpecification;
likeGraphExpressionShorthand: LIKE graphExpression;

// Section 11.5 <binding table type expression>
ofBindingTableType:
	ofTypePrefix? bindingTableTypeExpression
	| likeBindingTableShorthand;
bindingTableTypeExpression:
	bindingTableType
	| likeBindingTableType;
bindingTableType: BINDING? TABLE recordValueType;
likeBindingTableType: BINDING? TABLE likeBindingTableShorthand;
likeBindingTableShorthand: LIKE bindingTableReference;
/* Section 12 statements */
// Section 12.1 <statement>
statement:
	atSchemaClause? (
		catalogModifyingStatement
		| dataModifyingStatement
		| queryStatement
	);
catalogModifyingStatement: linearCatalogModifyingStatement;
dataModifyingStatement:
	conditionalDataModifyingStatement
	| linearDataModifyingStatement;
queryStatement:
	compositeQueryStatement
	| conditionalQueryStatement
	| selectStatement;
// Section 12.2 <call procedure statement>
callProcedureStatement: statementMode? CALL procedureCall;
statementMode: OPTIONAL | MANDATORY;
// Section 12.3 Statement classes
simpleCatalogModifyingStatement:
	primitiveCatalogModifyingStatement
	| callCatalogModifyingProcedureStatement;
primitiveCatalogModifyingStatement:
	createGraphStatement
	| createGraphTypeStatement
	| createConstantStatement
	| createProcedureStatement
	| createQueryStatement
	| createFunctionStatement
	| createPathPatternStatement
	| dropGraphStatement
	| dropGraphTypeStatement
	| dropConstantStatement
	| dropProcedureStatement
	| dropQueryStatement
	| dropFunctionStatement
	| dropPathPatternStatement;

simpleDataAccessingStatement:
	simpleQueryStatement
	| simpleDataModifyingStatement;
simpleDataModifyingStatement:
	primitiveDataModifyingStatement
	| doStatement
	| callDataModifyingProcedureStatement;
primitiveDataModifyingStatement:
	insertStatement
	| mergeStatement
	| setStatement
	| removeStatement
	| deleteStatement;
simpleQueryStatement:
	simpleDataTransformingStatement
	| simpleDataReadingStatement;
simpleDataReadingStatement:
	matchGraphStatement
	| matchStatement
	| callQueryStatement;
simpleDataTransformingStatement:
	primitiveDataTransformingStatement
	| callFunctionStatement;

primitiveDataTransformingStatement:
	optionalStatement
	| mandatoryStatement
	| letStatement
	| forStatement
	| aggregateStatement
	| filterStatement
	| orderByAndPageStatement;
/* Section 13 Catalog-modifying statements*/
// Section 13.1 <linear catalog-modifying statement>
linearCatalogModifyingStatement:
	simpleCatalogModifyingStatement+;
// Section 13.2 <create schema statement>
createSchemaStatement:
	CREATE (
		SCHEMA catalogSchemaParentAndName IF_NOT_EXISTS?
		| OR REPLACE SCHEMA catalogSchemaParentAndName
	);
// Section 13.3 <drop schema statement>
dropSchemaStatement:
	DROP SCHEMA catalogSchemaParentAndName IF_EXISTS;
// Section 13.4 <create graph statement>
createGraphStatement:
	CREATE (
		PROPERTY? GRAPH catalogGraphParentAndName IF_NOT_EXISTS?
		| OR REPLACE PROPERTY? GRAPH catalogGraphParentAndName
	) ofGraphType? graphSource?;
graphSource: AS copyGraphExpression;

// Section 13.5<graph specification>
graphSpecification:
	PROPERTY? GRAPH (
		nestedGraphQuerySpecification
		| nestedAmbientDataModifyingProcedureSpecification
	);
nestedGraphQuerySpecification: nestedQuerySpecification;
nestedAmbientDataModifyingProcedureSpecification:
	nestedDataModifyingProcedureSpecification;
// Section 13.6 <drop graph statement>
dropGraphStatement:
	DROP GRAPH catalogGraphParentAndName IF_EXISTS?;
// Section 13.7 <create graph type statement>
createGraphTypeStatement:
	CREATE (
		PROPERTY? GRAPH TYPE catalogGraphTypeParentAndName IF_NOT_EXISTS?
		| OR REPLACE PROPERTY? GRAPH TYPE catalogGraphTypeParentAndName
	) graphTypeInitializer;
// Section 13.8 <graph type specification>
graphTypeSpecification:
	PROPERTY? GRAPH TYPE nestedGraphTypeSpecification;
nestedGraphTypeSpecification:
	'(' graphTypeSpecificationBody ')';
graphTypeSpecificationBody: elementTypeDefinitionList;
elementTypeDefinitionList:
	elementTypeDefinition (P_COMMA elementTypeDefinition)*;
elementTypeDefinition: nodeTypeDefinition | edgeTypeDefinition;
// Section 13.9 <node type definition>
nodeTypeDefinition:
	'(' nodeTypeName? nodeTypeFiller? ')'
	| NODE_SYNONYM TYPE? nodeTypeName nodeTypeFiller;
nodeTypeName:
	elementTypeName; // NOTE: Predicative production rule.
nodeTypeFiller:
	nodeTypeLabelSetDefinition
	| nodeTypePropertyTypeSetDefinition
	| nodeTypeLabelSetDefinition nodeTypePropertyTypeSetDefinition;
nodeTypeLabelSetDefinition:
	labelSetDefinition; // NOTE: Predicative production rule.
nodeTypePropertyTypeSetDefinition:
	propertyTypeSetDefinition; // NOTE: Predicative production rule.
// Section 13.10 <edge type definition>
edgeTypeDefinition:
	fullEdgeTypePattern
	| abbreviatedEdgeTypePattern
	| edgeKind EDGE_SYNONYM TYPE? edgeTypeName edgeTypeFiller endPointDefinition;
edgeTypeName:
	elementTypeName; // NOTE: Predicative production rule.
edgeTypeFiller:
	edgeTypeLabelSetDefinition
	| edgeTypePropertyTypeSetDefinition
	| edgeTypeLabelSetDefinition edgeTypePropertyTypeSetDefinition;
edgeTypeLabelSetDefinition:
	labelSetDefinition; // NOTE: Predicative production rule.
edgeTypePropertyTypeSetDefinition:
	propertyTypeSetDefinition; // NOTE: Predicative production rule.
fullEdgeTypePattern:
	fullEdgeTypePatternPointingRight
	| fullEdgeTypePatternPointingLeft
	| fullEdgeTypePatternAndDirection;
fullEdgeTypePatternPointingRight:
	sourceNodeTypeReference arcTypePointingRight destinationNodeTypeReference;
fullEdgeTypePatternPointingLeft:
	destinationNodeTypeReference arcTypePointingLeft sourceNodeTypeReference;
fullEdgeTypePatternAndDirection:
	sourceNodeTypeReference arcTypeAnyDirection destinationNodeTypeReference;
arcTypePointingRight:
	MINUS_LEFT_BRACKET arcTypeFiller BRACKET_RIGHT_ARROW;
arcTypePointingLeft:
	LEFT_ARROW_BRACKET arcTypeFiller RIGHT_BRACKET_MINUS;
arcTypeAnyDirection:
	TILDE_LEFT_BRACKET arcTypeFiller RIGHT_BRACKET_TILDE;
arcTypeFiller: edgeTypeName? edgeTypeFiller?;
abbreviatedEdgeTypePattern:
	abbreviatedEdgeTypePatternPointingRight
	| abbreviatedEdgeTypePatternPointingLeft
	| abbreviatedEdgeTypePatternAnyDirection;
abbreviatedEdgeTypePatternPointingRight:
	sourceNodeTypeReference RIGHT_ARROW destinationNodeTypeReference;
abbreviatedEdgeTypePatternPointingLeft:
	destinationNodeTypeReference LEFT_ARROW sourceNodeTypeReference;
abbreviatedEdgeTypePatternAnyDirection:
	sourceNodeTypeReference P_TILDE destinationNodeTypeReference;
sourceNodeTypeReference:
	'(' sourceNodeTypeName ')'
	| '(' nodeTypeFiller? ')';
destinationNodeTypeReference:
	'(' destinationNodeTypeName ')'
	| '(' nodeTypeFiller? ')';
edgeKind: DIRECTED | UNDIRECTED;
endPointDefinition: CONNECTING endpointPairDefinition;
endpointPairDefinition:
	endpointPairDefinitionPointingRight
	| endpointPairDefinitionPointingLeft
	| endpointPairDefinitionAndDirection
	| abbreviatedEdgeTypePattern;
endpointPairDefinitionPointingRight:
	'(' sourceNodeTypeName connectorPointingRight destinationNodeTypeName ')';
endpointPairDefinitionPointingLeft:
	'(' sourceNodeTypeName LEFT_ARROW destinationNodeTypeName ')';
endpointPairDefinitionAndDirection:
	'(' sourceNodeTypeName connectorAnyDirection destinationNodeTypeName ')';
connectorPointingRight: TO | RIGHT_ARROW;
connectorAnyDirection: TO | P_TILDE;
sourceNodeTypeName:
	elementTypeName; // NOTE: Predicative production rule.
destinationNodeTypeName:
	elementTypeName; // NOTE: Predicative production rule.
// Section 13.11 <lable set definition>
labelSetDefinition:
	LABEL label
	| LABELS labelExpression
	| isLabelExpression;

// Section 13.12 <property type set definition>
propertyTypeSetDefinition: '{' propertyTypeDefinitionList? '}';
propertyTypeDefinitionList:
	propertyTypeDefinition (P_COMMA propertyTypeDefinition)*;
propertyTypeDefinition: propertyName typeName;

// Section 13.13 <drop graph type statement>
dropGraphTypeStatement:
	DROP PROPERTY? GRAPH TYPE catalogGraphTypeParentAndName IF_EXISTS?;
// Section 13.14 <create constant statement>
createConstantStatement:
	CREATE (
		CONSTANT VALUE? catalogValueParentAndName IF_NOT_EXISTS?
		| OR REPLACE CONSTANT VALUE? catalogValueParentAndName
	) valueInitializer;
// Section 13.15 <drop constant statement>
dropConstantStatement:
	DROP CONSTANT VALUE? catalogValueParentAndName IF_EXISTS?;
// Section 13.16 <create procedure statement>
createProcedureStatement:
	CREATE (
		PROCEDURE catalogProcedureParentAndName ofTypeSignature IF_NOT_EXISTS?
		| OR REPLACE PROCEDURE catalogProcedureParentAndName ofTypeSignature
	) procedureInitializer;
// Section 13.17 <drop procedure statement>
dropProcedureStatement:
	DROP PROCEDURE catalogProcedureParentAndName IF_EXISTS?;
// Section 13.18 <create query statement>
createQueryStatement:
	CREATE (
		QUERY catalogQueryParentAndName ofTypeSignature IF_NOT_EXISTS?
		| OR REPLACE QUERY catalogQueryParentAndName ofTypeSignature
	) queryInitializer;
// Section 13.19 <drop query statement>
dropQueryStatement:
	DROP QUERY catalogQueryParentAndName IF_EXISTS?;
// Section 13.20 <create function statement>
createFunctionStatement:
	CREATE (
		FUNCTION catalogFunctionParentAndName ofTypeSignature IF_NOT_EXISTS?
		| OR REPLACE FUNCTION catalogFunctionParentAndName ofTypeSignature
	) functionInitializer;
// Section 13.21 <drop function statement>
dropFunctionStatement:
	DROP FUNCTION catalogFunctionParentAndName IF_EXISTS?;
// Section 13.22 <create path pattern statement>
createPathPatternStatement:
	CREATE (
		PATH PATTERN catalogPathPatternParentAndName IF_NOT_EXISTS?
		| OR REPLACE PATH PATTERN catalogPathPatternParentAndName
	) pathPatternInitializer;
// Section 13.23 <drop path pattern statement>
dropPathPatternStatement:
	DROP PATH PATTERN catalogPathPatternParentAndName IF_EXISTS?;
// Sectoin 13.24 <call catalog-modifying procedure statement>
callCatalogModifyingProcedureStatement: callProcedureStatement;
/* Section 14 Data-modifying statements */
// Section 14.1 <linear data-modifying statement>
linearDataModifyingStatement:
	focusedLinearDataModifyingStatement
	| ambientLinearDataModifyingStatement;
focusedLinearDataModifyingStatement:
	useGraphClause focusedLinearDataModifyingStatementBody+;
focusedLinearDataModifyingStatementBody:
	simpleLinearQueryStatement? (
		useGraphClause simpleLinearQueryStatement
	)* simpleDataModifyingStatement simpleDataAccessingStatement* (
		useGraphClause simpleDataAccessingStatement
	)* primitiveResultStatement?
	| nestedDataModifyingProcedureSpecification;
ambientLinearDataModifyingStatement:
	simpleLinearQueryStatement? simpleDataModifyingStatement simpleDataAccessingStatement*
		primitiveResultStatement?
	| nestedDataModifyingProcedureSpecification;
// Section 14.2 <conditional data-modiyfing statement>
conditionalDataModifyingStatement:
	whenThenLinearDataModifyingStatementBranch+ elseLinearDataModifyingStatementBranch?;
whenThenLinearDataModifyingStatementBranch:
	whenClause THEN linearDataModifyingStatement
	| whenClause nestedDataModifyingProcedureSpecification;
elseLinearDataModifyingStatementBranch:
	ELSE linearDataModifyingStatement;
whenClause: WHEN searchCondition;
// Section 14.3 <do statement>
doStatement: DO nestedDataModifyingProcedureSpecification;
// Section 14.4 <insert statement>
insertStatement:
	INSERT simpleGraphPattern
	| OPTIONAL INSERT simpleGraphPattern whenClause?;
// Section 14.5 <merge statement>
mergeStatement: MERGE simpleGraphPattern;
// Section 14.6 <set statement>
setStatement: SET setItemList whenClause?;
setItemList: setItem (P_COMMA setItem)*;
setItem: setPropertyItem | setAllPropertiesItem | setLabelItem;
setPropertyItem:
	bindingVariable P_DOT propertyName P_EQUAL valueExpression;
setAllPropertiesItem: bindingVariable P_EQUAL valueExpression;
setLabelItem: labelSetExpression;
labelSetExpression:
	P_AMPERSAND label+ (P_AMPERSAND label+); // grammar error?
// Section 14.7 <remove statement>
removeStatement: REMOVE removeItemList whenClause?;
removeItemList: removeItem (P_COMMA removeItem)*;
removeItem: removePropertyItem | removeLabelItem;
removePropertyItem: bindingVariable P_DOT propertyName;
removeLabelItem: bindingVariable P_COLON labelSetExpression;

// Section 14.8 <delete statement>
deleteStatement: DETACH? DELETE deleteItemList whenClause?;
deleteItemList: deleteItem (P_COMMA deleteItem)*;
deleteItem: valueExpression;
// Section 14.9 <call data-moidifying procedure statement>
callDataModifyingProcedureStatement: callProcedureStatement;

/* Section 15 Querty statements*/
// Section 15.1 <composite query statement>
compositeQueryStatement: compositeQueryExpression;
// Section 15.2 <conditional query statement>
conditionalQueryStatement:
	whenThenLinearQueryBranch+ elseLinearQueryBranch?;
whenThenLinearQueryBranch:
	whenClause THEN linearQueryExpression
	| whenClause nestedQuerySpecification;
elseLinearQueryBranch: ELSE linearQueryExpression;
// Session 15.3 <composite query expression>
compositeQueryExpression:
	linearQueryExpression
	| compositeQueryExpression queryConjunction linearQueryExpression;
queryConjunction: setOperator | OTHERWISE;
setOperator:
	(UNION | EXCEPT | INTERSECT) setOperatorQuantifier?;
setOperatorQuantifier: setQuantifier | MAX;
// Session 15.4 <linear query expression>
linearQueryExpression: linearQueryStatement;
// Session 15.5 <linear query statement>
linearQueryStatement:
	focusedLinearQueryStatement
	| ambientLinearQueryStatement;
focusedLinearQueryStatement:
	fromGraphClause focusedLinearQueryStatementBody;
focusedLinearQueryStatementBody:
	simpleLinearQueryStatement (
		fromGraphClause simpleLinearQueryStatement
	)* primitiveResultStatement
	| nestedQuerySpecification;
ambientLinearQueryStatement:
	simpleLinearQueryStatement primitiveResultStatement
	| nestedQuerySpecification;
simpleLinearQueryStatement: simpleQueryStatement;

/* Section 15.6 Data-Reading statments  */
// Section 15.6.1 <match graph statement>
matchGraphStatement:
	statementMode? MATCH graphKeywords graphObjectPattern;
graphKeywords: PROPERTY? (GRAPH | GRAPHS);
// Section 15.6.2 <match statement>
matchStatement: statementMode? MATCH graphPattern;
// Section 15.6.3 <call query statement>
callQueryStatement: callProcedureStatement;

/* Section 15.7 Data-transforming statements */
// Section 15.7.1 <mandatory statement>
mandatoryStatement: MANDATORY procedureCall;
// Section 15.7.2 <optional statement>
optionalStatement: OPTIONAL procedureCall;
// Section 15.7.3 <filter statement>
filterStatement: FILTER (whereClause | searchCondition);

// Section 15.7.4 <let statement>
letStatement:
	LET compactVariableDefinitionList
	| statementMode LET compactVariableDefinitionList whereClause;
// Section 15.7.5 <aggregate statement>
aggregateStatement:
	AGGREGATE compactValueVariableDefinitionList whereClause;
// Section 15.7.6 <for statement>
forStatement:
	statementMode? FOR forItemList forOrdinalityOrIndex? whereClause?;
forItemList: forItem (AND forItem)*;
forItem: forItemAlias collectionValueExpression;
forItemAlias: <identifier> IN;
forOrdinalityOrIndex: WITH (ORDINALITY | INDEX) identifier?;
// Section 15.7.7 <order by and page statement>
orderByAndPageStatement: orderingAndPagingClauses;

// Section 15.7.8 <call function statement>
callFunctionStatement: callProcedureStatement;
/* Section 15.8 Result projection statements */
// Section 15.8.1 <primitive result statement>
primitiveResultStatement:
	returnStatement
	| projectStatement
	| END;
// Section 15.8.2 <return statement>
returnStatement: RETURN returnStatementBody;
returnStatementBody:
	setQuantifier? (P_STAR | returnItemList) groupByClause? orderingAndPagingClauses?;
returnItemList: returnItem (P_COMMA returnItem)*;
returnItem: valueExpression returnItemAlias?;
returnItemAlias: AS identifier;
// Section 15.8.3 <select statement>
selectStatement:
	SELECT setQuantifier? selectItemList selectStatementBody whereClause? groupByClause?
		havingClause? orderByClause? offsetClause? limitClause?;
selectItemList: selectItem (P_COMMA selectItem)*;
selectItem: valueExpression selectItemAlias?;
selectItemAlias: AS identifier;
havingClause: HAVING searchCondition;
selectStatementBody:
	selectGraphMatches+
	| selectQuerySpecification;
selectGraphMatches: fromGraphClause matchStatement+;
selectQuerySpecification:
	FROM nestedQuerySpecification
	| fromGraphClause nestedQuerySpecification;
// Section 15.8.4 <project statement>
projectStatement: PROJECT valueExpression;
/* Section 16 Common elements */
// Section 16.1 <from graph clause>
fromGraphClause: FROM graphExpression;
// Section 16.2 <use graph clause>
useGraphClause: USE graphExpression;
// Section 16.3 <at schema clause>
atSchemaClause: AT schemaReference;
// Section 16.4 Named elements
staticVariable: staticVariableName;
bindingVariable: bindingVariableName;
label: labelName;
parameter: parameterName;
// Section 16.5 <type signature>
ofTypeSignature: ofTypePrefix? typeSignature;
typeSignature:
	parenthesizedFormalParameterList ofTypePrefix? procedureResultType;
parenthesizedFormalParameterList: '(' formalParameterList? ')';
formalParameterList:
	mandatoryFormalParameterList (
		P_COMMA optionalFormalParameterList
	)?
	| optionalFormalParameterList;
mandatoryFormalParameterList: formalParameterDeclarationList;
optionalFormalParameterList:
	OPTIONAL formalParameterDefinitionList;
formalParameterDeclarationList:
	formalParameterDeclaration (
		P_COMMA formalParameterDeclaration
	)*;

formalParameterDefinitionList:
	formalParameterDefinition (P_COMMA formalParameterDefinition)*;
formalParameterDeclaration:
	parameterCardinality compactVariableDeclaration;
formalParameterDefinition:
	parameterCardinality compactVariableDefinition;
optionalParameterCardinality: parameterCardinality?;
parameterCardinality: SINGLE | MULTI | MULTIPLE;
procedureResultType: valueType;
// Section 16.6 <graph object pattern>
graphObjectPattern:
	graphObjectPatternVariableList whereClause? yieldClause?;
graphObjectPatternVariableList:
	graphObjectPatternVariable (
		P_COMMA graphObjectPatternVariable
	)*;
graphObjectPatternVariable: graphVariable labelExpression?;
// Section 16.7 <graph pattern>
graphPattern:
	pathPatternList keepClause? graphPatternWhereClause? yieldClause?;
pathPatternList: pathPattern (P_COMMA pathPattern)*;
pathPattern:
	(pathVariable P_EQUAL)? pathPatternPrefix? pathPatternExpression;
keepClause: KEEP pathPatternPrefix;
graphPatternWhereClause: WHERE searchCondition;

// Section 16.8 <path pattern expression>
pathPatternExpression:
	pathTerm
	| pathMultisetAlternation
	| pathPatternUnion;
pathMultisetAlternation:
	pathTerm P_MULTISET_ALTERNATION pathTerm (
		P_MULTISET_ALTERNATION pathTerm
	)*;
pathPatternUnion: pathTerm P_V_BAR pathTerm (P_V_BAR pathTerm)*;
pathTerm: pathFactor | pathConcatenation;
pathConcatenation: pathTerm pathFactor;
pathFactor:
	pathPrimary
	| quantifiedPathPrimary
	| questionedPathPrimary;
quantifiedPathPrimary: pathPrimary graphPatternQuantifier;
questionedPathPrimary: pathPrimary P_QUESTION;
pathPrimary:
	elementPattern
	| parenthesizedPathPatternExpression
	| simplifiedPathPatternExpression;
elementPattern: nodePattern | edgePattern;
nodePattern: '(' elementPatternFiller ')';
elementPatternFiller:
	elementVariableDeclaration? isLabelExpression? elementPatternPredicate? elementPatternCostClause
		?;
elementVariableDeclaration: elementVariable;
isLabelExpression: (IS | P_COLON) labelExpression;
elementPatternPredicate:
	elementPatternWhereClause
	| elementPropertySpecification;
elementPatternWhereClause: WHERE searchCondition;
elementPropertySpecification: '{' propertyKeyValuePairList '}';
propertyKeyValuePairList:
	propertyKeyValuePair (P_COMMA propertyKeyValuePair)*;
propertyKeyValuePair: propertyName P_COLON valueExpression;
elementPatternCostClause: costClause;
costClause: COST valueExpression (DEFAULT valueExpression)?;
edgePattern: fullEdgePattern | abbreviatedEdgePattern;
fullEdgePattern:
	fullEdgePointingLeft
	| fullEdgePointingRight
	| fullEdgeUndirected
	| fullEdgeLeftOrUndirected
	| fullEdgeUndirectedOrRight
	| fullEdgeLeftOrRight
	| fullEdgeAnyDirection;
fullEdgePointingLeft:
	LEFT_ARROW_BRACKET elementPatternFiller RIGHT_BRACKET_MINUS;
fullEdgePointingRight:
	MINUS_LEFT_BRACKET elementPatternFiller BRACKET_RIGHT_ARROW;
fullEdgeUndirected:
	TILDE_LEFT_BRACKET elementPatternFiller RIGHT_BRACKET_TILDE;
fullEdgeLeftOrUndirected:
	LEFT_ARROW_TILDE_BRACKET elementPatternFiller RIGHT_BRACKET_TILDE;
fullEdgeUndirectedOrRight:
	TILDE_LEFT_BRACKET elementPatternFiller BRACKET_TILDE_RIGHT_ARROW;
fullEdgeLeftOrRight:
	LEFT_ARROW_BRACKET elementPatternFiller BRACKET_RIGHT_ARROW;
fullEdgeAnyDirection:
	MINUS_LEFT_BRACKET elementPatternFiller RIGHT_BRACKET_MINUS;
abbreviatedEdgePattern:
	LEFT_ARROW
	| P_TILDE
	| RIGHT_ARROW
	| LEFT_ARROW_TILDE
	| TILDE_RIGHT_ARROW
	| LEFT_MINUS_RIGHT
	| P_MINUS;
graphPatternQuantifier:
	P_STAR
	| P_PLUS
	| fixedQuantifier
	| generalQuantifier;
fixedQuantifier: '{' unsignedInteger '}';
generalQuantifier:
	'{' lowerBound = unsignedInteger? P_COMMA upperBound = unsignedInteger? '}';
parenthesizedPathPatternExpression:
	'(' subpathVariableDeclaration? pathModePrefix? pathPatternExpression
		parenthesizedPathPatternWhereClause? parenthesizedPathPatternCostClause? ')'
	| '[' subpathVariableDeclaration? pathModePrefix? pathPatternExpression
		parenthesizedPathPatternWhereClause? parenthesizedPathPatternCostClause? ']';
subpathVariableDeclaration: subpathVariable P_EQUAL;
parenthesizedPathPatternWhereClause: WHERE searchCondition;
parenthesizedPathPatternCostClause: costClause;
// Section 16.9 <path pattern prefix>
pathPatternPrefix: pathModePrefix | pathSearchPrefix;
pathModePrefix: pathMode pathOrPaths?;
pathMode: WALK | TRAIL | SIMPLE | ACYCLIC;
pathSearchPrefix:
	allPathSearch
	| anyPathSearch
	| shortestPathSearch;
allPathSearch: ALL pathMode? pathOrPaths?;
pathOrPaths: (PATH | PATHS);
anyPathSearch: ANY numberOfPaths? pathMode? pathOrPaths?;
numberOfPaths: simpleValueSpecification;
shortestPathSearch:
	allShortestPathSearch
	| anyShortestPathSearch
	| countedShortestPathSearch
	| countedShortestGroupSearch;
allShortestPathSearch: ALL SHORTEST pathMode? pathOrPaths?;
anyShortestPathSearch: ANY SHORTEST pathMode? pathOrPaths?;
countedShortestPathSearch:
	SHORTEST numberOfPaths pathMode? pathOrPaths?;
countedShortestGroupSearch:
	SHORTEST numberOfgroups pathMode? pathOrPaths? (
		GROUP
		| GROUPS
	);
numberOfgroups: simpleValueSpecification;

// Section 16.10<simple graph pattern>
simpleGraphPattern: simplePathPatternList;
simplePathPatternList:
	simplePathPattern (P_COMMA simplePathPattern)*;
simplePathPattern:
	pathPattern; // NOTE: Predicative production rule.
//Section 16.11<label expression>
labelExpression: labelTerm | labelDisjunction;
labelDisjunction: labelExpression P_V_BAR labelTerm;
labelTerm: labelFactor | labelConjunction;
labelConjunction: labelTerm P_AMPERSAND labelFactor;
labelFactor: labelPrimary | labelNegation;
labelNegation: P_EXCLAMATION labelPrimary;
labelPrimary:
	label
	| wildcardLabel
	| parenthesizedLabelExpression;
wildcardLabel: P_PERCENT;
parenthesizedLabelExpression:
	'(' labelExpression ')'
	| '[' labelExpression ']';
//Section 16.12<simplified path pattern expression>
simplifiedPathPatternExpression:
	simplifiedDefaultingLeft
	| simplifiedDefaultingUndirected
	| simplifiedDefaultingRight
	| simplifiedDefaultingLeftOrUndirected
	| simplifiedDefaultingUndirectedOrRight
	| simplifiedDefaultingLeftOrRight
	| simplifiedDefaultingAnyDirection;
simplifiedDefaultingLeft:
	LEFT_MINUS_SLASH simplifiedContents SLASH_MINUS;
simplifiedDefaultingUndirected:
	TILDE_SLASH simplifiedContents SLASH_TILDE;
simplifiedDefaultingRight:
	MINUS_SLASH simplifiedContents SLASH_MINUS_RIGHT;
simplifiedDefaultingLeftOrUndirected:
	LEFT_TILDE_SLASH simplifiedContents SLASH_TILDE;
simplifiedDefaultingUndirectedOrRight:
	TILDE_SLASH simplifiedContents SLASH_TILDE_RIGTH;
simplifiedDefaultingLeftOrRight:
	LEFT_MINUS_SLASH simplifiedContents SLASH_MINUS_RIGHT;
simplifiedDefaultingAnyDirection:
	MINUS_SLASH simplifiedContents SLASH_MINUS;
simplifiedContents:
	simplifiedTerm
	| simplifiedPathUnion
	| simplifiedMultisetAlternation;
simplifiedPathUnion: simplifiedTerm (P_V_BAR simplifiedTerm)+;
simplifiedMultisetAlternation:
	simplifiedTerm (P_MULTISET_ALTERNATION simplifiedTerm)+;
simplifiedTerm: simplifiedFactorLow | simplifiedConcatenation;
simplifiedConcatenation: simplifiedTerm simplifiedFactorLow;
simplifiedFactorLow:
	simplifiedFactorHigh
	| simplifiedConjunction;
simplifiedConjunction:
	simplifiedFactorLow P_AMPERSAND simplifiedFactorHigh;
simplifiedFactorHigh:
	simplifiedTertiary
	| simplifiedQuantified
	| simplifiedQuestioned;
simplifiedQuantified: simplifiedTertiary graphPatternQuantifier;
simplifiedQuestioned: simplifiedTertiary P_QUESTION;
simplifiedTertiary:
	simplifiedDirectionOverride
	| simplifiedSecondary;
simplifiedDirectionOverride:
	simplifiedDirectionOverrideLeft
	| simplifiedDirectionOverrideUndirected
	| simplifiedDirectionOverrideRight
	| simplifiedDirectionOverrideLeftOrUndirected
	| simplifiedDirectionOverrideUndirectedOrRight
	| simplifiedDirectionOverrideLeftOrRight
	| simplifiedDirectionOverrideAnyDirection;
simplifiedDirectionOverrideLeft:
	P_L_ANGLE_BRACKET simplifiedSecondary;
simplifiedDirectionOverrideUndirected:
	P_TILDE simplifiedSecondary;
simplifiedDirectionOverrideRight:
	simplifiedSecondary P_R_ANGLE_BRACKET;
simplifiedDirectionOverrideLeftOrUndirected:
	LEFT_ARROW_TILDE simplifiedSecondary;
simplifiedDirectionOverrideUndirectedOrRight:
	P_TILDE simplifiedSecondary P_R_ANGLE_BRACKET;
simplifiedDirectionOverrideLeftOrRight:
	P_L_ANGLE_BRACKET simplifiedSecondary P_R_ANGLE_BRACKET;
simplifiedDirectionOverrideAnyDirection:
	P_MINUS simplifiedSecondary;
simplifiedSecondary: simplifiedPrimary | simplifiedNegation;
simplifiedNegation: P_EXCLAMATION simplifiedPrimary;
simplifiedPrimary:
	label
	| '(' simplifiedContents ')'
	| '[' simplifiedContents ']';

//Section 16.13<where clause>
whereClause: WHERE searchCondition;

//Section 16.14<procedure call>
procedureCall: inlineProcedureCall | namedProcedureCall;

//Section 16.15<inline procedure call>
inlineProcedureCall: nestedProcedureSpecification;

//Section 16.16<named procedure call>
namedProcedureCall:
	procedureReference '(' procedureArgumentList? ')' yieldClause?;
procedureArgumentList:
	procedureArgument (P_COMMA procedureArgument)*;
procedureArgument: valueExpression;

//Section 16.17<yield clause>
yieldClause: YIELD yieldItemList;
yieldItemList: yieldItem (P_COMMA yieldItem)*;
yieldItem: yieldItemName yieldItemAlias?;
yieldItemName: identifier;
yieldItemAlias: AS identifier;
//Section 16.18<group by clause>
groupByClause: GROUP BY groupingElementList;
groupingElementList:
	groupingElement (P_COMMA groupingElement)*
	| emptyGroupingSet;
groupingElement: bindingVariable;
emptyGroupingSet: '(' ')';

//Section 16.19<ordering and paging clauses>
orderingAndPagingClauses: singleOrderingAndPagingClause*;
singleOrderingAndPagingClause:
	orderByClause
	| offsetClause
	| limitClause;
//Section 16.20<order by clause>
orderByClause:
	ORDER BY sortSpecificationList orderByOrdinalityOrIndex?;
orderByOrdinalityOrIndex: WITH (ORDINALITY | INDEX) identifier?;

//Section 16.21<aggregate function>
aggregateFunction:
	COUNT '(' P_STAR ')'
	| generalSetFunction
	| binarySetFunction;
generalSetFunction:
	generalSetFunctionType '(' setQuantifier valueExpression ')';
binarySetFunction:
	binarySetFunctionType '(' dependentValueExpression P_COMMA independentValueExpression ')';
generalSetFunctionType:
	AVG
	| COUNT
	| MAX
	| MIN
	| SUM
	| PRODUCT
	| COLLECT
	| ST_Dev
	| ST_Dev_P;
setQuantifier: (DISTINCT | ALL);
binarySetFunctionType: PERCENTILE_CONT | PERCENTILE_DIST;
dependentValueExpression: setQuantifier? numericValueExpression;
independentValueExpression: numericValueExpression;

//Section 16.22<sort specification list>
sortSpecificationList:
	sortSpecification (P_COMMA sortSpecification)*;
sortSpecification: sortKey orderingSpecification?;
sortKey: valueExpression;
orderingSpecification: (ASC | DESC);
//Section 16.23<limit clause>
limitClause: LIMIT numericValueExpression;

//Section 16.24<offset clause>
offsetClause: OFFSET numericValueExpression;
/* Section 17 Object references */
//Section 17.1 Schema references
schemaReference:
	schemaResolutionExpression
	| localSchemaReference;
schemaResolutionExpression: SCHEMA catalogSchemaReference;
catalogSchemaReference:
	catalogSchemaParentAndName
	| predefinedSchemaParameter
	| externalObjectReference;
catalogSchemaParentAndName:
	schemaParentSpecification schemaName
	| urlPathParameter;
schemaParentSpecification: parentCatalogObjectReference?;
localSchemaReference: schemaName;

//Section 17.2 Graph references
graphReference: graphResolutionExpression | localGraphReference;
graphResolutionExpression:
	PROPERTY? GRAPH catalogGraphReference;
catalogGraphReference:
	catalogGraphParentAndName
	| predefinedGraphParameter
	| externalObjectReference;
catalogGraphParentAndName:
	graphParentSpecification graphName
	| urlPathParameter;
graphParentSpecification:
	parentCatalogObjectReference? (qualifiedObjectName P_DOT)?;
localGraphReference: qualifiedGraphName;
qualifiedGraphName: (qualifiedObjectName P_DOT)? graphName;
//Section 17.3 Graph type references
graphTypeReference:
	graphTypeResolutionExpression
	| localGraphTypeReference;
graphTypeResolutionExpression:
	PROPERTY? GRAPH TYPE catalogGraphTypeReference;
catalogGraphTypeReference:
	catalogGraphTypeParentAndName
	| externalObjectReference;
catalogGraphTypeParentAndName:
	graphTypeParentSpecification graphTypeName
	| urlPathParameter;
graphTypeParentSpecification:
	(parentCatalogObjectReference)? (qualifiedObjectName P_DOT)?;
localGraphTypeReference: qualifiedGraphTypeName;
qualifiedGraphTypeName:
	(qualifiedObjectName P_DOT)? graphTypeName;
//Section 17.4 Binding table references
bindingTableReference:
	bindingTableResolutionExpression
	| localBindingTableReference;
bindingTableResolutionExpression:
	BINDING? TABLE catalogBindingTableReference;
catalogBindingTableReference:
	catalogBindingTableParentAndName
	| predefinedTableParameter
	| externalObjectReference;
catalogBindingTableParentAndName:
	bindingTableParentSpecification bindingTableName
	| urlPathParameter;
bindingTableParentSpecification:
	parentCatalogObjectReference? (qualifiedObjectName P_DOT)?;
localBindingTableReference: qualifiedBindingTableName;
qualifiedBindingTableName:
	(qualifiedObjectName P_DOT)? bindingTableName;
//Section 17.5 Value references
valueReference: valueResolutionExpression | localValueReference;
valueResolutionExpression: VALUE catalogValueReference;
catalogValueReference:
	catalogValueParentAndName
	| externalObjectReference;
catalogValueParentAndName:
	valueParentSpecification valueName
	| urlPathParameter;
valueParentSpecification:
	parentCatalogObjectReference? (qualifiedObjectName P_DOT)?;
localValueReference: qualifiedValueName;
qualifiedValueName: (qualifiedObjectName P_DOT)? valueName;
//Section 17.6 Procedure references
procedureReference:
	procedureResolutionExpression
	| localProcedureReference;
procedureResolutionExpression:
	PROCEDURE catalogProcedureReference;
catalogProcedureReference:
	catalogProcedureParentAndName
	| externalObjectReference;
catalogProcedureParentAndName:
	procedureParentSpecification procedureName
	| urlPathParameter;
procedureParentSpecification:
	parentCatalogObjectReference? (qualifiedObjectName P_DOT)?;
localProcedureReference: qualifiedProcedureName;
qualifiedProcedureName:
	(qualifiedObjectName P_DOT)? procedureName;
// Section 17.7 Query references
queryReference: queryResolutionExpression | localQueryReference;
queryResolutionExpression: QUERY catalogQueryReference;
catalogQueryReference:
	catalogQueryParentAndName
	| externalObjectReference;
catalogQueryParentAndName:
	queryParentSpecification queryName
	| urlPathParameter;
queryParentSpecification:
	parentCatalogObjectReference? (qualifiedObjectName P_DOT)?;
localQueryReference: qualifiedQueryName;
qualifiedQueryName: (qualifiedObjectName P_DOT)? queryName;
// Section 17.8 Function references
functionReference:
	functionResolutionExpression
	| localFunctionReference;
functionResolutionExpression: FUNCTION catalogFunctionReference;
catalogFunctionReference:
	catalogFunctionParentAndName
	| externalObjectReference;
catalogFunctionParentAndName:
	functionParentSpecification functionName
	| urlPathParameter;
functionParentSpecification:
	parentCatalogObjectReference? (qualifiedObjectName P_DOT)?;
localFunctionReference: qualifiedFunctionName;
qualifiedFunctionName:
	(qualifiedObjectName P_DOT)? functionName;

// Section 17.9 Path pattern references
pathPatternReference:
	pathPatternResolutionExpression
	| localPathPatternReference;
pathPatternResolutionExpression:
	PATH PATTERN catalogPathPatternReference;
catalogPathPatternReference:
	catalogPathPatternParentAndName
	| externalObjectReference;
catalogPathPatternParentAndName:
	pathPatternParentSpecification pathPatternName
	| urlPathParameter;
pathPatternParentSpecification:
	parentCatalogObjectReference? (qualifiedObjectName P_DOT)?;
localPathPatternReference: qualifiedPathPatternName;
qualifiedPathPatternName:
	(qualifiedObjectName P_DOT)? pathPatternName;
// Section 17.10<catalog object reference>
catalogObjectReference: catalogUrlPath;
parentCatalogObjectReference: catalogObjectReference P_SOLIDUS?;
catalogUrlPath:
	absoluteUrlPath
	| relativeUrlPath
	| parameterizedUrlPath;
absoluteUrlPath: P_SOLIDUS simpleUrlPath?;
relativeUrlPath:
	parentObjectRelativeUrlPath
	| simpleRelativeUrlPath
	| P_DOT;
parentObjectRelativeUrlPath:
	predefinedParentObjectParameter (P_SOLIDUS simpleUrlPath)?;
simpleRelativeUrlPath:
	P_DOUBLE_DOT (P_SOLIDUS P_DOUBLE_DOT)* (
		P_SOLIDUS simpleUrlPath
	)?
	| simpleUrlPath;
parameterizedUrlPath:
	urlPathParameter (P_SOLIDUS simpleUrlPath)?;
simpleUrlPath: urlSegment (P_SOLIDUS urlSegment)*;
urlSegment: identifier;

// Section 17.11<qualified object name>
qualifiedObjectName: qualifiedNamePerfix objectName;
qualifiedNamePerfix: (objectName P_DOT)*;
// Section 17.12<url path parameter>
urlPathParameter: parameter;

// Section 17.13<external object reference>
externalObjectReference: externalObjectUrl;
/*
 TODO 1) Let EOR be the <external object reference>. 2) Let EOU be the <external object url>
 immediately contained in EOR 3) EOU shall either be an absolute-URL string or an
 absolute-URL-with-fragment string as specified by URL or it alternatively shall be an URI with a
 mandatory scheme as specified by RFC 3986 and RFC 3978. 4) EOU shall not conform to the Format for
 a <catalog url path>.
 */
externalObjectUrl:;

/* Section 19 Predicates */
// Section 19.1 <search condition>
searchCondition: booleanValueExpression;
// Section 19.2 <predicate>
predicate:
	comparisonPredicate
	| existsPredicate
	| betweenPredicate
	| nullPredicate
	| normalizedPredicate;
//Section 19.3 <comparison predicate>
comparisonPredicate:
	nonParenthesizedValueExpressionPrimary compOp nonParenthesizedValueExpressionPrimary;
compOp: (
		P_EQUAL
		| P_NOT_EQUAL
		| P_LESS_THAN
		| P_GREATE_THAN
		| P_LESS_EQUAL
		| P_GREATE_EQUAL
	);
//Section 19.4 <exists predicate>
existsPredicate:
	EXISTS ('(' graphPattern ')' | nestedQuerySpecification);
//Section 19.5 <between predicate>
/*
 This Subclause is a placeholder. Syntax for the TigerGraph variant is a subset of SQL’s. It is
 apparently absent from Cypher. Discussion is needed as to whether to adopt the TigerGraph or the
 SQL variant. It is in any case just a syntactic shorthand for a <search condition> involving
 <comparison predicate>s). See Possible Problem GQL-195 .
 */
betweenPredicate:; //TODO(hs.zhang)

//Section 19.6 <null predicate>
nullPredicate: valueExpressionPrimary IS NOT? NULL;
//Section 19.7 <normalized predicate>
normalizedPredicate:
	stringValueExpression IS NOT? normalForm? NORMALIZED;
/* Section 20 Value expression */
// Section 20.1 <value specification>
valueSpecification: literal | parameterValueSpecification;
unsignedValueSpecification:
	unsignedLiteral
	| parameterValueSpecification;
simpleValueSpecification: unsignedLiteral | parameter;
parameterValueSpecification: parameter | predefinedParameter;
predefinedParameter:
	predefinedParentObjectParameter
	| predefinedTableParameter
	| CURRENT_ROLE
	| CURRENT_USER;
predefinedParentObjectParameter:
	predefinedSchemaParameter
	| predefinedGraphParameter;
predefinedSchemaParameter: HOME_SCHEMA | CURRENT_SCHEMA;
predefinedGraphParameter:
	EMPTY_PROPERTY_GRAPH
	| EMPTY_GRAPH
	| HOME_PROPERTY_GRAPH
	| HOME_GRAPH
	| CURRENT_PROPERTY_GRAPH
	| CURRENT_GRAPH;
predefinedTableParameter:
	EMPTY_BINDING_TABLE
	| EMPTY_TABLE
	| UNIT_BINDING_TABLE
	| UNIT_TABLE;
// Section 20.2 <value expression>
valueExpression: untypedValueExpression ofValueType?;
untypedValueExpression:
	commonValueExpression
	| booleanValueExpression;
commonValueExpression:
	numericValueExpression
	| stringValueExpression
	| datetimeValueExpression
	| durationValueExpression
	| collectionValueExpression
	| mapValueExpression
	| recordValueExpression
	| referenceValueExpression;
referenceValueExpression:
	primaryResultObjectExpression
	| graphElementValueExpression;

collectionValueExpression:
	listValueExpression
	| multisetValueExpression
	| setValueExpression
	| orderedSetValueExpression;
setValueExpression: valueExpressionPrimary;
orderedSetValueExpression: valueExpressionPrimary;
mapValueExpression: valueExpressionPrimary;
recordValueExpression: valueExpressionPrimary;
// Section 20.3 <boolean value expression>
booleanValueExpression:
	booleanTerm
	| booleanValueExpression OR booleanTerm
	| booleanValueExpression XOR booleanTerm;
booleanTerm: booleanFactor | booleanTerm AND booleanTerm;
booleanFactor: NOT? booleanTest;
booleanTest:
	booleanPrimary (
		((IS NOT?) | P_EQUAL | P_NOT_EQUAL) truthValue
	)?;
truthValue: TRUE | FALSE | UNKNOWN | NULL;
booleanPrimary: predicate | booleanPredicand;
booleanPredicand:
	parenthesizedBooleanValueExpression
	| nonParenthesizedValueExpressionPrimary;
parenthesizedBooleanValueExpression:
	'(' booleanValueExpression ')';
// Section 20.4 <numeric value expression>
numericValueExpression:
	term
	| numericValueExpression P_PLUS term
	| numericValueExpression P_MINUS term;
term: factor | term P_STAR factor | term P_SOLIDUS factor;
factor: (P_MINUS | P_PLUS)? numericPrimary;
numericPrimary: valueExpressionPrimary | numericValueFunction;
// Section 20.5 <value expression primary>
valueExpressionPrimary:
	parenthesizedValueExpression
	| nonParenthesizedValueExpressionPrimary;
parenthesizedValueExpression: '(' valueExpression ')';
nonParenthesizedValueExpressionPrimary:
	propertyReference
	| bindingVariable
	| parameterValueSpecification
	| unsignedValueSpecification
	| aggregateFunction
	| collectionValueConstructor
	| valueQueryExpression
	| caseExpression
	| castSpecification;

// Section 20.6 <numeric value function>
numericValueFunction:
	lengthExpression
	| absoluteValueExpression
	| modulusExpression
	| trigonometricFunction
	| generalLogarithmFunction
	| commonLogarithm
	| naturalLogarithm
	| exponentialFunction
	| squareRoot
	| floorFunction
	| ceilingFunction
	| inDegreeFunction
	| outDegreeFunction;
lengthExpression:
	charLengthExpression
	| octetLengthExpression
	| pathLengthExpression;
charLengthExpression:
	CHARACTER_LENGTH '(' characterValueExpression ')';
octetLengthExpression:
	OCTET_LENGTH '(' stringValueExpression ')';
pathLengthExpression: LENGTH '(' bindingVariable ')';
absoluteValueExpression: ABS '(' numericValueExpression ')';
modulusExpression:
	MOD '(' numericValueExpressionDividend P_COMMA numericValueExpressionDivisor ')';
numericValueExpressionDividend: numericValueExpression;
numericValueExpressionDivisor: numericValueExpression;
trigonometricFunction:
	trigonometricFunctionName '(' numericValueExpression ')';
trigonometricFunctionName:
	SIN
	| COS
	| TAN
	| COT
	| SINH
	| COSH
	| TANH
	| ASIN
	| ACOS
	| ATAN
	| DEGREES
	| RADIANS;
generalLogarithmFunction:
	LOG '(' generalLogarithmBase P_COMMA generalLogarithmArgument ')';
generalLogarithmBase: numericValueExpression;
generalLogarithmArgument: numericValueExpression;
commonLogarithm: LOG10 '(' numericValueExpression ')';
naturalLogarithm: LN '(' numericValueExpression ')';
exponentialFunction: EXP '(' numericValueExpression ')';
powerFunction:
	POWER '(' base = numericValueExpression P_COMMA exp = numericValueExpression ')';
squareRoot: SQRT '(' numericValueExpression ')';
floorFunction: FLOOR '(' numericValueExpression ')';
ceilingFunction:
	(CEIL | CEILING) '(' numericValueExpression ')';
inDegreeFunction: IN_DEGREE '(' bindingVariable ')';
outDegreeFunction: OUT_DEGREE '(' bindingVariable ')';
// Section 20.7 <string value expression>
stringValueExpression:
	characterValueExpression
	| binaryValueExpression;
characterValueExpression: characterFactor | concatenation;
concatenation:
	characterValueExpression P_CONCATENATION characterFactor;
characterFactor: characterPrimary;
characterPrimary: valueExpressionPrimary | stringValueFunction;
binaryValueExpression: binaryConcatenation | binaryFactor;
binaryFactor: binaryPrimary;
binaryPrimary: valueExpressionPrimary | stringValueFunction;
binaryConcatenation:
	binaryValueExpression P_CONCATENATION binaryFactor;
// Section 20.8 <string value function>
stringValueFunction:
	characterValueFunction
	| binaryValueFunction;
characterValueFunction:
	characterSubstringFunction
	| fold
	| trimFunction
	| normalizeFunction;
characterSubstringFunction:
	SUBSTRING '(' characterValueExpression P_COMMA startPosition (
		P_COMMA stringLength
	)? ')'
	| LEFT '(' characterValueExpression P_COMMA stringLength ')'
	| RIGHT '(' characterValueExpression P_COMMA stringLength ')';
fold:
	(UPPER | TO_UPPER | LOWER | TO_LOWER) '(' characterValueExpression ')';
trimFunction:
	TRIM '(' trimSource (
		P_COMMA trimSpecification trimCharacter?
	)? ')'
	| L_TRIM '(' trimSource ')'
	| R_TRIM '(' trimSource ')';
trimSource: characterValueExpression;
trimSpecification: LEADING | TRAILING | BOTH;
trimCharacter: characterValueExpression;
normalizeFunction:
	NORMALIZE '(' characterValueExpression (P_COMMA normalForm) ')';
normalForm: NFC | NFD | NFKC | NFKD;
binaryValueFunction:
	binarySubstringFunction
	| binaryTrimFunction;
binarySubstringFunction:
	SUBSTRING '(' binaryValueExpression P_COMMA startPosition (
		P_COMMA stringLength
	)? ')'
	| LEFT '(' binaryValueExpression P_COMMA stringLength ')'
	| RIGHT '(' binaryValueExpression P_COMMA stringLength ')';
binaryTrimFunction:
	TRIM '(' binaryTrimSource (
		P_COMMA trimSpecification trimOctet?
	)? ')'
	| L_TRIM '(' binaryTrimSource ')'
	| R_TRIM '(' binaryTrimSource ')';
binaryTrimSource: binaryValueExpression;
trimOctet: binaryValueExpression;
startPosition: numericValueExpression;
stringLength: numericValueExpression;
// Section 20.9 <datetime value expression>
datetimeValueExpression:
	datetimeTerm
	| durationValueExpression P_PLUS datetimeTerm
	| datetimeTerm (P_PLUS | P_MINUS) durationValueExpression;
datetimeTerm: datetimeFactor;
datetimeFactor: datetimePrimary;
datetimePrimary: valueExpressionPrimary | datetimeValueFunction;
// Section 20.10<datetime value function>
datetimeValueFunction:
	dateFunction
	| timeFunction
	| datetimeFunction
	| localTimeFunction
	| localDatetimeFunction;
dateFunction:
	CURRENT_DATE
	| DATE '(' dateFunctionParameters? ')';
timeFunction:
	CURRENT_TIME
	| TIME '(' timeFunctionParameters? ')';
localTimeFunction:
	LOCALTIME
	| LOCALTIME '(' timeFunctionParameters? ')';
datetimeFunction:
	CURRENT_TIMESTAMP
	| DATETIME '(' datetimeFunctionParameters? ')';
localDatetimeFunction:
	LOCALTIMESTAMP
	| LOCALDATETIME '(' datetimeFunctionParameters? ')';
dateFunctionParameters: dateString | mapValueConstructor;
timeFunctionParameters: timeString | mapValueConstructor;
datetimeFunctionParameters:
	datetimeString
	| mapValueConstructor;

// Section 20.11<duration value expression>
durationValueExpression:
	durationTerm
	| durationValueExpression (P_PLUS | P_MINUS) durationTerm
	| '(' datetimeValueExpression P_MINUS datetimeValueExpression ')';
durationTerm:
	durationFactor
	| durationTerm P_STAR factor
	| durationTerm P_SOLIDUS factor
	| term P_STAR durationFactor;
durationFactor: (P_PLUS | P_MINUS)? durationPrimary;
durationPrimary: valueExpressionPrimary | durationValueFunction;
// Section 20.12<duration value function>
durationValueFunction:
	durationFunction durationAbsoluteValueFunction;
durationFunction: DURATION '(' durationFunctionParameters ')';
durationFunctionParameters:
	durationString = STRING_LITERAL
	| mapValueConstructor;
durationAbsoluteValueFunction:
	ABS '(' durationValueExpression ')';
// Section 20.13<graph element value expression>
graphElementValueExpression: graphElementPrimary;
graphElementPrimary:
	graphElementFunction
	| valueExpressionPrimary;
// Section 20.14<graph element function>
graphElementFunction: startNodeFunction | endNodeFunction;
startNodeFunction: START_NODE '(' bindingVariable ')';
endNodeFunction: END_NODE '(' bindingVariable ')';
// Section 20.15<collection value constructor>
collectionValueConstructor:
	listValueConstructor
	| setValueConstructor
	| multisetValueConstructor
	| orderedSetValueConstructor
	| mapValueConstructor
	| recordValueConstructor;
// Section 20.16<list value expression>
listValueExpression: listConcatenation | listPrimary;
listConcatenation:
	listValueExpression P_CONCATENATION listPrimary;
listPrimary: listValueFunction | valueExpressionPrimary;

// Section 20.17<list value function>
listValueFunction: tailListFunction | trimListFunction;
tailListFunction: TAIL '(' listValueExpression ')';
trimListFunction:
	TRIM '(' listValueExpression P_COMMA numericValueExpression ')';
// Section 20.18<list value constructor>
listValueConstructor: listValueConstructorByEnumeration;
listValueConstructorByEnumeration:
	listValueTypeName '[' listElementList ']';
listElementList: listElement (P_COMMA listElement)*;
listElement: valueExpression;
// Section 20.19<multiset value expression>
multisetValueExpression:
	multisetTerm
	| multisetValueExpression MULTISET (UNION | EXCEPT) (
		ALL
		| DISTINCT
	)? multisetTerm;
multisetTerm:
	multisetPrimary
	| multisetTerm MULTISET INTERSECT (ALL | DISTINCT)? multisetPrimary;
multisetPrimary: multisetValueFunction | valueExpressionPrimary;
// Section 20.20<multiset value function>
multisetValueFunction: multisetSetFunction;
multisetSetFunction: SET '(' multisetValueExpression ')';

// Section 20.21<multiset value constructor>
multisetValueConstructor: multisetValueConstructorByEnumeration;
multisetValueConstructorByEnumeration:
	MULTISET '{' multisetElementList '}';
multisetElementList: multisetElement (P_COMMA multisetElement)*;
multisetElement: valueExpression;
// Section 20.22<set value constructor>
setValueConstructor: setValueConstructorByEnumeration;
setValueConstructorByEnumeration: SET '{' setElementList '}';
setElementList: setElement (P_COMMA setElement)*;
setElement: valueExpression;
// Section 20.23<ordered set value constructor>
orderedSetValueConstructor:
	orderedSetValueConstructorByEnumeration;
orderedSetValueConstructorByEnumeration:
	ORDERED SET (
		'{' orderedSetElementList '}'
		| '[' orderedSetElementList ']'
	);

orderedSetElementList:
	orderedSetElement (P_COMMA orderedSetElement)*;
orderedSetElement: valueExpression;
// Section 20.24<map value constructor>
mapValueConstructor: mapValueConstructorByEnumeration;
mapValueConstructorByEnumeration: MAP '{' mapElementList '}';
mapElementList: mapElement (P_COMMA mapElement)*;
mapElement: mapKey mapValue;
mapKey: valueExpression P_COLON;
mapValue: valueExpression;
// Section 20.25<record value constructor>
recordValueConstructor:
	recordValueConstructorByEnumeration
	| UNIT;
recordValueConstructorByEnumeration: RECORD? '{' fieldList '}';
fieldList: field (P_COMMA field)*;
field: fieldName fieldValue;
fieldValue: valueExpression;
// Section 20.26<property reference>
propertyReference: graphElementPrimary P_DOT propertyName;

// Section 20.27<value query expression>
valueQueryExpression: VALUE nestedQuerySpecification;

// Section 20.28<case expression>
caseExpression: caseAbbreviation | caseSpecification;
caseAbbreviation:
	NULLIF '(' valueExpression P_COMMA valueExpression ')'
	| COALESCE '(' valueExpression (P_COMMA valueExpression)* ')';
caseSpecification: simpleCase | searchedCase;
simpleCase: CASE caseOperand simpleWhenClause+ elseClause? END;
searchedCase: CASE searchedWhenClause* elseClause? END;
simpleWhenClause: WHEN whenOperandList THEN result;
searchedWhenClause: WHEN searchCondition THEN result;
elseClause: ELSE result;
caseOperand: nonParenthesizedValueExpressionPrimary;
whenOperandList: whenOperand (P_COMMA whenOperand)*;
whenOperand:
	nonParenthesizedValueExpressionPrimary
	| compOp nonParenthesizedValueExpressionPrimary
	| IS NOT? NULL;
result: resultExpression | NULL;
resultExpression: valueExpression;
// Section 20.29<cast specification>
/*
 This Subclause is a placeholder. Syntax for SQL & Cypher variants are very different. SQL has a
 single <case specification> whereas Cypher has a set on individual functions: toBoolean, toInteger,
 toFloat, and toString. Cloning the SQL <case specification> since that is available in SQL/PGQ is
 probably preferable with the Cypher functions defined as syntactic shorthands. See Possible Problem
 GQL-195 .
 */

castSpecification:; //TODO
/* Section Lexical elements */
literal: signedNumericLiteral | generalLiteral;
generalLiteral:
	predefinedTypeLiteral
	| listLiteral
	| setLiteral
	| multisetLiteral
	| orderedSetLiteral
	| mapLiteral
	| recordLiteral;
predefinedTypeLiteral:
	booleanLiteral
	| characterStringLiteral
	| binaryStringLiteral
	| temporalLiteral
	| durationLiteral
	| nullLiteral;
unsignedLiteral: unsignedNumericLiteral | generalLiteral;
booleanLiteral: TRUE | FALSE | UNKNOWN;
characterStringLiteral:
	unbrokenCharacterStringLiteral (
		(SPACE | P_COMMA)* unbrokenCharacterStringLiteral
	)*;
unbrokenCharacterStringLiteral: STRING_LITERAL;
binaryStringLiteral: BINARY_STRING_LITERAL;
numericLiteral: signedNumericLiteral | unsignedNumericLiteral;
signedNumericLiteral: ('+' | '-')? unsignedNumericLiteral;
unsignedNumericLiteral:
	exactNumericLiteral
	| approximateNumericLiteral;
exactNumericLiteral:
	unsignedInteger
	| unsignedDecimalInteger (P_DOT unsignedDecimalInteger?)?
	| P_DOT unsignedDecimalInteger;
unsignedInteger:
	unsignedDecimalInteger
	| unsignedHexadecimalInteger
	| unsignedOctalInteger
	| unsignedBinaryInteger;
unsignedDecimalInteger: UNSIGNED_DECIMAL_INTEGER;
unsignedHexadecimalInteger: UNSIGNED_HEX_INTEGER;
unsignedOctalInteger: UNSIGNED_OCT_INTEGER;
unsignedBinaryInteger: UNSIGNED_BIN_INTEGER;
signedDecimalInteger: ('+' | '-')? unsignedDecimalInteger;
approximateNumericLiteral: mantissa P_EXP exponent;
mantissa: exactNumericLiteral;
exponent: signedDecimalInteger;
temporalLiteral: dateLiteral | timeLiteral | datetimeLiteral;
dateLiteral: DATE dateString;
dateString: STRING_LITERAL;
timeLiteral: TIME timeString;
timeString: STRING_LITERAL;
datetimeLiteral: (DATETIME | TIMESTAMP) datetimeString;
datetimeString: STRING_LITERAL;
durationLiteral: STRING_LITERAL;
nullLiteral: NULL;
listLiteral: listValueConstructorByEnumeration;
setLiteral: setValueConstructorByEnumeration;
multisetLiteral: multisetValueConstructorByEnumeration;
orderedSetLiteral: orderedSetValueConstructorByEnumeration;
mapLiteral: mapValueConstructorByEnumeration;
recordLiteral: recordValueConstructorByEnumeration;

// Section 21.2 <value type>
valueType:
	ANY
	| predefinedType
	| graphElementType
	| collectionType
	| mapValueType
	| recordValueType
	| graphTypeExpression
	| bindingTableTypeExpression
	| NOTHING;
ofValueType: ofTypePrefix? valueType;
ofTypePrefix: P_COLON P_COLON | OF;
predefinedType:
	booleanType
	| stringType
	| binaryStringType
	| numericType
	| temporalType;
booleanType: BOOLEAN;
stringType: CHARACTER? STRING;
binaryStringType: BINARY STRING;
numericType: exactNumericType | approximateNumericType;
exactNumericType:
	binaryExactNumericType
	| decimalExactNumericType;
binaryExactNumericType: INTEGER | INTEGER32 | INTEGER64;
decimalExactNumericType:
	DECIMAL '(' precision (P_COMMA scale)? ')';
precision: unsignedDecimalInteger;
scale: unsignedDecimalInteger;
approximateNumericType: FLOAT | FLOAT32 | FLOAT64 | FLOAT128;
temporalType:
	DATETIME
	| LOCALDATETIME
	| DATE
	| TIME
	| LOCALTIME
	| DURATION;
graphElementType: NODE | VERTEX | EDGE | RELATIONSHIP | PATH;
collectionType:
	listValueType
	| setValueType
	| multisetValueType
	| orderedSetValueType;
listValueType: valueType listValueTypeName;
listValueTypeName: LIST | ARRAY;
multisetValueType: valueType MULTISET;
setValueType: valueType SET;
orderedSetValueType: valueType ORDERED SET;
mapValueType: MAP '<' mapKeyType P_COMMA valueType '>';
mapKeyType: predefinedType;
recordValueType: RECORD? '{' fieldTypeList? '}';
fieldTypeList: fieldType (P_COMMA fieldType)*;
fieldType: fieldName ofTypePrefix? valueType;
//Section 21.3 Names and identifiers
objectName: identifier;
schemaName: identifier;
graphName: identifier;
elementTypeName: typeName;
typeName: identifier;
graphTypeName: identifier;
bindingTableName: identifier;
valueName: identifier;
procedureName: identifier;
queryName: identifier;
functionName: identifier;
labelName: identifier;
propertyName: identifier;
fieldName: identifier;
pathPatternName: identifier;
parameterName: P_DOLLAR identifier;
elementVariable: identifier;
pathVariable: identifier;
subpathVariable: identifier;
staticVariableName: identifier;
bindingVariableName: identifier;
identifier: regularIdentifier | delimitedIdentifier;
regularIdentifier: IDENTIFIER;
delimitedIdentifier: DELIMITED_IDENTIFIER;
//Section 21.4 <token> and <separator> other
