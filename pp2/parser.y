/* File: parser.y
       in the pp2 handout.
 */

%{
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(char *msg); // standard error-handling routine

%}
 
/* yylval 
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser. 
 *
 * pp2: You will need to add new fields to this union as you add different 
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    char *stringConstant;
    double doubleConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *declList;


    Type *type;
    VarDecl *var;
    List<VarDecl*> *dvarList;
    FnDecl *fDecl;
    Stmt *stmt;
    List<Stmt*> *stmtList;
    Expr *expr;
    List<Expr*> *exprList;
    LValue *lValue;
    NamedType *cType;
    List<NamedType*> *cTypeList;
}


/* Tokens
 */
%token   T_Void T_Bool T_Int T_Double T_String T_Class 
%token   T_LessEqual T_GreaterEqual T_Equal T_NotEqual T_Dims
%token   T_And T_Or T_Null T_Extends T_This T_Interface T_Implements
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_New T_NewArray T_Print T_ReadInteger T_ReadLine

%token   <identifier> T_Identifier
%token   <stringConstant> T_StringConstant 
%token   <integerConstant> T_IntConstant
%token   <doubleConstant> T_DoubleConstant
%token   <boolConstant> T_BoolConstant


/* Non-terminal types
 */
%type <declList>  DeclList PrototypeList FieldList 
%type <decl>      Decl Field ClassDecl InterfaceDecl
%type <cType>     OptExt 
%type <cTypeList> OptImpl ImpList

%type <type>      Type
%type <var>       Variable VariableDecl
%type <dvarList>  Formals FormalList VariableDeclList
%type <stmt>      Stmt StmtBlock OptElse ';'
%type <stmtList>  StmtList
%type <expr>      Expr  OptExpr  Constant Call  LValue
%type <exprList>  ExprList Actuals
//%type <lValue>    LValue
%type <fDecl>     FunctionDecl Prototype


%left      '='
%left      T_Or
%left      T_And 
%nonassoc  T_Equal T_NotEqual
%nonassoc  '<' '>' T_LessEqual T_GreaterEqual
%left      '+' '-'
%left      '*' '/' '%'  
%nonassoc  T_UnaryMinus '!' 
%left T_Incremental T_Decremental
%nonassoc  '.' '[' 
%nonassoc  T_Complete_Else
%nonassoc  T_Else

/**
%nonassoc '='
%left T_Equal T_NotEqual
%left '!' T_UnaryMinus
%left '[' '.'
**/



%%
/* Rules
 */
Program   :    DeclList            { 
                                      @1; 
                                      /* pp2: The @1 is needed to convince 
                                       * yacc to set up yylloc. You can remove 
                                       * it once you have other uses of @n*/
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0) 
                                          program->Print(0);
                                    }
          ;

DeclList  :    DeclList Decl        { ($$=$1)->Append($2); }
          |    Decl                 { ($$ = new List<Decl*>)->Append($1); }
          ;

Decl      :    VariableDecl    { $$ = $1; } 
          |    FunctionDecl    {$$ = $1;}
          |    ClassDecl       {$$ = $1;}
          |    InterfaceDecl   {$$ = $1;}
          ;

VariableDecl  : Variable ';'
              ;

Variable      :  Type T_Identifier   {  $$ = new VarDecl(new Identifier(@2, $2), $1); }
              ;

Type          :   T_Int              { $$ = Type::intType; }
              |   T_Bool             { $$ = Type::boolType; }
              |   T_String           { $$ = Type::stringType; }
              |   T_Double           { $$ = Type::doubleType; }
              |   T_Identifier       { $$ = new NamedType(new Identifier(@1,$1)); }
              |   Type T_Dims        { $$ = new ArrayType(Join(@1, @2), $1); }
              ;

FunctionDecl  :  Type T_Identifier '(' Formals ')' StmtBlock 
                 {($$ = new FnDecl(new Identifier(@2, $2), $1, $4))->SetFunctionBody($6);}
              |  T_Void T_Identifier '(' Formals ')' StmtBlock 
                 {($$ = new FnDecl(new Identifier(@2, $2), Type::voidType, $4))->SetFunctionBody($6);}
              ;

Formals       :    FormalList     { $$ = $1; }
              |                   { $$ = new List<VarDecl*>; }
              ;

FormalList    :  FormalList ',' Variable   { ($$=$1)->Append($3); }
              |  Variable                  { ($$ = new List<VarDecl*>)->Append($1); }
              ; 

ClassDecl :    T_Class T_Identifier OptExt OptImpl '{' FieldList '}'   { $$ = new ClassDecl(new Identifier(@2, $2), $3, $4, $6); }
          ; 

FieldList :    FieldList Field      { ($$=$1)->Append($2); }
          |                         { $$ = new List<Decl*>(); }
          ;

Field     :    VariableDecl {$$ = $1;}
          |    FunctionDecl {$$ = $1;}
          ;

OptExt    :    T_Extends T_Identifier         { $$ = new NamedType(new Identifier(@2, $2)); }
          |                                   { $$ = NULL; }
          ;

OptImpl   :    T_Implements ImpList           { $$ = $2; }
          |                                   { $$ = new List<NamedType*>; }
          ;

ImpList   :    ImpList ',' T_Identifier        { ($$=$1)->Append(new NamedType(new Identifier(@3, $3))); }
          |    T_Identifier                    { ($$=new List<NamedType*>)->Append(new NamedType(new Identifier(@1, $1))); }
          ;

InterfaceDecl : T_Interface T_Identifier '{' PrototypeList '}'  {$$ = new InterfaceDecl(new Identifier(@2, $2), $4);}
              ;

PrototypeList : PrototypeList Prototype    {$$->Append($2);}
              |                            {$$ = new List<Decl*>;}
              ;

Prototype     : Type T_Identifier '(' Formals ')'';'  {$$ = new FnDecl(new Identifier(@2, $2), $1, $4);}
              | T_Void T_Identifier '('Formals')'';'  {$$ = new FnDecl(new Identifier(@2, $2), Type::voidType, $4);}
              ;



StmtBlock     :    '{' VariableDeclList StmtList '}'  { $$ = new StmtBlock($2, $3); }
              |    '{''}'                             {$$ = new StmtBlock(new List<VarDecl*>, new List<Stmt*>);}
              ;

VariableDeclList  :    VariableDeclList VariableDecl    { ($$=$1)->Append($2); }
                  |                                     { $$ = new List<VarDecl*>; }
                  ;

StmtList      :  StmtList  Stmt        { ($$ = $1)->Append($2); }
              | Stmt                               {($$ = new List<Stmt*>)->Append($1);}
              ;  

Stmt      :    OptExpr ';'                     { $$ = $1; }
          |    ';'                             {$$ = $1;}
          |    T_If '(' Expr ')' Stmt OptElse  { $$ = new IfStmt($3, $5, $6); }
          |    T_While '(' Expr ')' Stmt       { $$ = new WhileStmt($3, $5); }
          |    T_For '(' OptExpr ';' Expr ';' OptExpr ')' Stmt  { $$ = new ForStmt($3, $5, $7, $9); } 
          |    T_Return Expr ';'               { $$ = new ReturnStmt(@1, $2); }
          |    T_Return ';'                    { $$ = new ReturnStmt(@1, new EmptyExpr()); }
          |    T_Break ';'                     { $$ = new BreakStmt(@1); }
          |    T_Print '(' ExprList ')' ';'    { $$ = new PrintStmt($3); }
          |    StmtBlock
          ;

OptExpr   :    Expr                 { $$ = $1; }
          |                         { $$ = new EmptyExpr(); }
          ;

OptElse   :    T_Else Stmt                 { $$ = $2; }
          |    %prec T_Complete_Else     { $$ = NULL; }
          ;

ExprList   : ExprList ',' Expr           {($$ = $1)->Append($3);}
           | Expr                        {($$ = new List<Expr*>)->Append($1);}
           ;

Expr    :       LValue                           {$$ = $1;}
        |       Constant                         {$$ = $1;}
        |       T_This                           {$$ = new This(@1);}
        |       Call                             {$$ = $1;}
        |       LValue '=' Expr                  {$$ = new AssignExpr($1, new Operator(@2, "="), $3);}
        | '('   Expr ')'                         {$$ = $2;}
        |       Expr '+' Expr                    {$$ = new ArithmeticExpr($1, new Operator(@2, "+"), $3);}
        |       Expr '-' Expr                    {$$ = new ArithmeticExpr($1, new Operator(@2, "-"), $3);}
        |       Expr '*' Expr                    {$$ = new ArithmeticExpr($1, new Operator(@2, "*"), $3);}
        |       Expr '/' Expr                    {$$ = new ArithmeticExpr($1, new Operator(@2, "/"), $3);}
        |       Expr '%' Expr                    {$$ = new ArithmeticExpr($1, new Operator(@2, "%"), $3);}
        | '-'   Expr  %prec T_UnaryMinus         {$$ = new ArithmeticExpr(new Operator(@1, "-"), $2);}
        |       Expr '<' Expr                    {$$ = new RelationalExpr($1, new Operator(@2, "<"), $3);}
        |       Expr T_LessEqual Expr            {$$ = new RelationalExpr($1, new Operator(@2, "<="), $3);}
        |       Expr '>' Expr                    {$$ = new RelationalExpr($1, new Operator(@2, ">"), $3);}
        |       Expr T_GreaterEqual Expr         {$$ = new RelationalExpr($1, new Operator(@2, ">="), $3);}
        |       Expr T_Equal Expr                {$$ = new EqualityExpr($1, new Operator(@2, "=="), $3);}
        |       Expr T_NotEqual Expr             {$$ = new EqualityExpr($1, new Operator(@2, "!="), $3);}
        |       Expr T_And Expr                  {$$ = new LogicalExpr($1, new Operator(@2, "&&"), $3);}
        |       Expr T_Or Expr                   {$$ = new LogicalExpr($1, new Operator(@2, "||"), $3);}
        | '!'   Expr                             {$$ = new LogicalExpr(new Operator(@1, "!"), $2);}
        |       Expr T_Incremental  %prec T_Incremental  {$$ = new IncrementalExpr(new Operator(@2, "++"), $1);}
        |       Expr T_Decremental  %prec T_Decremental  {$$ = new DecrementalExpr(new Operator(@2, "--"), $1);}
        |       T_ReadInteger '(' ')'            {$$ = new ReadIntegerExpr(@1);}
        |       T_ReadLine    '(' ')'            {$$ = new ReadLineExpr(@1);}
        |       T_New '(' T_Identifier ')'       {$$ = new NewExpr(@1, new NamedType(new Identifier(@3, $3)));}
        |       T_NewArray '(' Expr ',' Type ')' {$$ = new NewArrayExpr(@1, $3, $5);}
        ;

LValue    :    T_Identifier          { $$ = new FieldAccess(NULL, new Identifier(@1, $1)); }
          |    Expr '.' T_Identifier { $$ = new FieldAccess($1, new Identifier(@3, $3)); } 
          |    Expr '[' Expr ']'     { $$ = new ArrayAccess(@1, $1, $3); }
          ;

Call      :    T_Identifier '(' Actuals ')'               { $$ = new Call(Join(@1,@4), NULL, new Identifier(@1,$1), $3); }
          |    Expr '.' T_Identifier '(' Actuals ')'      { $$ = new Call(Join(@1,@6), $1, new Identifier(@3,$3), $5); }
          ;

Actuals   :    ExprList             { $$ = $1; }
          |                         { $$ = new List<Expr*>; }
          ;        

Constant  :    T_IntConstant        { $$ = new IntConstant(@1,$1); }
          |    T_BoolConstant       { $$ = new BoolConstant(@1,$1); }
          |    T_DoubleConstant     { $$ = new DoubleConstant(@1,$1); }
          |    T_StringConstant     { $$ = new StringConstant(@1,$1); }
          |    T_Null               { $$ = new NullConstant(@1); }
          ;     

%%


/* Function: InitParser
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
