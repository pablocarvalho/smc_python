import sys
sys.path.insert(0, "../..")

import ply.lex as lex
import ply.yacc as yacc
import os
import node as nd
import smc


class Parser:
    """
    Base class for a lexer/parser that has the rules defined as methods
    """
    tokens = ()
    precedence = ()

    def __init__(self, **kw):
        self.debug = kw.get('debug', 0)
        self.names = {}
        try:
            modname = os.path.split(os.path.splitext(__file__)[0])[
                1] + "_" + self.__class__.__name__
        except:
            modname = "parser" + "_" + self.__class__.__name__
        self.debugfile = modname + ".dbg"
        self.tabmodule = modname + "_" + "parsetab"
        # print self.debugfile, self.tabmodule

        # Build the lexer and parser
        lex.lex(module=self, debug=self.debug)
        yacc.yacc(module=self,
                  debug=self.debug,
                  debugfile=self.debugfile,
                  tabmodule=self.tabmodule)

    def run(self):
        while 1:
            try:
                s = raw_input('calc > ')
            except EOFError:
                break
            if not s:
                continue
            result = yacc.parse(s)
            #print_tree(result)
            return result



class Parser:
    """
    Base class for a lexer/parser that has the rules defined as methods
    """
    tokens = ()
    precedence = ()

    def __init__(self, **kw):
        self.debug = kw.get('debug', 0)
        self.names = {}
        try:
            modname = os.path.split(os.path.splitext(__file__)[0])[
                1] + "_" + self.__class__.__name__
        except:
            modname = "parser" + "_" + self.__class__.__name__
        self.debugfile = modname + ".dbg"
        self.tabmodule = modname + "_" + "parsetab"
        # print self.debugfile, self.tabmodule

        # Build the lexer and parser
        lex.lex(module=self, debug=self.debug)
        yacc.yacc(module=self,
                  debug=self.debug,
                  debugfile=self.debugfile,
                  tabmodule=self.tabmodule)

    def run(self):
        while 1:
            try:
                s = raw_input('calc > ')
            except EOFError:
                break
            if not s:
                continue
            result = yacc.parse(s)
            #print_tree(result)
            return result



class Calc(Parser):
    
    reserved = {
        'if' : 'IF', 
        'then':'THEN',
        'else':'ELSE',
        'while':'WHILE',
        'do':'DO',
        #'tt':'TT',
        'true':'TRUE',
        #'ff':'FF',
        'false':'FALSE',
        'nil':'NIL',
        'or':'OR',
        'and':'AND',
        'add':'ADD',
        'sub':'SUB',
        'mul':'MUL',
        'div':'DIV',
        'not':'NOT',
        'eq':'EQ',
        'nat':'NAT',
        'bol':'BOL',
        'const':'CONST',
        'var':'VAR',
        'T':'TYPE',
        'endif':'ENDIF',
        'print':'PRINT',
        'proc':'PROC',        
        
    }

    tokens = list(reserved.values()) + [
        'NAME', 'NUMBER',        
        'EQUALS',
        'LPAREN', 'RPAREN',
        'LBRACE', 'RBRACE',
        'SEMI', 
        'COMA','ATRIB'
    ]

    # Tokens

    #t_PLUS = r'\+'
    #t_MINUS = r'-'
    #t_EXP = r'\*\*'
    #t_TIMES = r'\*'
    #t_DIVIDE = r'/'
    t_EQUALS = r':='
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_LBRACE = r'\{'
    t_RBRACE = r'\}'
    t_COMA = r':'
    t_ATRIB = r'='

    def t_NAME(self,t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = self.reserved.get(t.value,'NAME')    # Check for reserved words
        return t

    #t_NAME = r'[a-zA-Z_]'

    # Delimeters
    t_SEMI = r';'

    

    def t_NUMBER(self, t):
        r'\d+'
        try:
            t.value = int(t.value)
        except ValueError:
            print("Integer value too large %s" % t.value)
            t.value = 0
        # print "parsed number %s" % repr(t.value)
        return t

    t_ignore = " \t"

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += t.value.count("\n")

    def t_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    # Parsing rules

    precedence = (
        ('left', 'ADD', 'SUB'),
        ('left', 'MUL', 'DIV'),
        #('left', 'EXP'),
        ('right', 'UMINUS'),
    )

    # Definicao de comandos ========================================================

    def p_command_assign(self, p):
        'command : NAME EQUALS expression'
        #self.names[p[1]] = p[3]
        p[0] = nd.Node("assign",[p[1],p[3]],p[2])
    
	#regra teste pra assign de booleano
    def p_booleanexpression_assign(self, p):
        'command : NAME EQUALS booleanexpression'
        #self.names[p[1]] = p[3]
        p[0] = nd.Node("boolassign",[p[1],p[3]],p[2])
    
    def p_command_assignnil(self, p):
        'command : NIL'
        p[0] = nd.Node("nil", [], p[1])         

    def p_command_separator(self, p):
        'command : command SEMI command'
        p[0] = nd.Node("separator", [p[1],p[3]], p[2])

    def p_command_declaration(self, p):
        'command : declaration SEMI command'
        p[0] = nd.Node("separator", [p[1],p[3]], p[2])

    def p_command_block(self, p):
        'command_block : LBRACE command RBRACE'
        p[0] = nd.Node("command_block", [p[2]], [p[1],p[3]])        
        

    def p_command_conditional(self,p):
        'command : IF booleanexpression THEN command_block ELSE command_block'
        p[0] = nd.Node("conditional", [p[2],p[4],p[6]], [p[1],p[3],p[5]])             

    def p_command_loop(self,p):
        'command : WHILE booleanexpression DO command'
        p[0] = nd.Node("loop", [p[2],p[4]], [p[1],p[3]])   

    def p_command_print(self,p):
        """command : PRINT LPAREN expression RPAREN
                   | PRINT LPAREN booleanexpression RPAREN """
        p[0] = nd.Node("print",[p[3]],[p[1]])

    def p_command_procedureCall(self,p):
        'command : NAME LPAREN parameters RPAREN'
        p[0] = nd.Node("procedure_call",[p[1],p[3]],[])


   
    # Fim da definicao de comandos ======================================================

   


    def p_booleanexpression_binop(self,p):
        """
        booleanexpression : booleanexpression OR booleanexpression            
                         | booleanexpression AND booleanexpression            
                         | expression EQ expression            
        """
        p[0] = nd.Node("booleanexpression_binop", [p[1],p[3]], p[2])

    def p_booleanexpression(self,p):
        'booleanexpression : NOT booleanexpression'
        p[0] = nd.Node("booleanexpressionnot", [p[2]], p[1])        

    def p_booleanexpression_values(self,p):
        """ booleanexpression : TRUE 
                             | FALSE
        """
        #p[0] = nd.Node("booleanvalues", [], p[1]) 
        p[0] = p[1]

    def p_booleanexpression_group(self, p):
        'booleanexpression : LPAREN booleanexpression RPAREN'
        #p[0] = p[2]
        p[0] = nd.Node("booleanexpression_block", [p[2]], [p[1],p[3]])


    def p_booleanexpression_var(self, p):
        'booleanexpression : NAME'
        p[0] = p[1]



    #Expressoes ==========================================================
    def p_expression_binop(self, p):
        """
        expression : ADD LPAREN expression expression RPAREN
                  | SUB LPAREN expression expression RPAREN
                  | MUL LPAREN expression  expression RPAREN
                  | DIV LPAREN expression expression RPAREN
        """
        p[0] = nd.Node("binop", [p[3],p[4]], [p[1],p[2],p[5]])                
   
    def p_expression_uminus(self, p):
        'expression : SUB expression %prec UMINUS'
        p[0] = -p[2]

    def p_expression_group(self, p):
        'expression : LPAREN expression RPAREN'
        p[0] = p[2]

    def p_expression_naturalValue(self,p):
        'expression : NAT LPAREN NUMBER RPAREN'
        p[0] = nd.Node("natural_number", [p[3]], p[1])

    def p_expression_booleanValue(self,p):
        'expression : BOL LPAREN booleanexpression RPAREN'
        p[0] = nd.Node("boolean_value", [p[3]], p[1])

    def p_expression_number(self, p):
        'expression : NUMBER'
        p[0] = p[1]

    def p_expression_conditional(self,p):
        'expression : IF booleanexpression THEN expression ELSE expression'
        p[0] = nd.Node("expression_conditional", [p[2],p[4],p[6]], [p[1],p[3],p[5]])        

    def p_expression_name(self, p):
        'expression : NAME'
        p[0] = p[1]
        #try:
        #    p[0] = self.names[p[1]]
        #except LookupError:
        #    print("Undefined name '%s'" % p[1])
        #    p[0] = 0

    #==============================================================================

    #Declaracoes===================================================================
    def p_declaration_const(self,p):
        'declaration : CONST NAME COMA TYPE ATRIB expression '
        p[0] = nd.Node("declaration_const", [p[2],p[4],p[6]], [p[1],p[3],p[5]])

    def p_declaration_var(self,p):
        'declaration : VAR NAME COMA TYPE ATRIB expression '
        p[0] = nd.Node("declaration_var", [p[2],p[4],p[6]], [p[1],p[3],p[5]])

    def p_declaration_boolconst(self,p):
        'declaration : CONST NAME COMA TYPE ATRIB booleanexpression '
        p[0] = nd.Node("declaration_const", [p[2],p[4],p[6]], [p[1],p[3],p[5]])

    def p_declaration_boolvar(self,p):
        'declaration : VAR NAME COMA TYPE ATRIB booleanexpression '
        p[0] = nd.Node("declaration_var", [p[2],p[4],p[6]], [p[1],p[3],p[5]])

    def p_declaration_procedure(self,p):
        'declaration : PROC NAME LPAREN declParameters RPAREN commandblock'
        p[0] = nd.Node("declaration_procedure",[p[2],p[4],p[6]], [p[1]])

    def p_declaration_parameter(self,p):
        'declarationParameter: TYPE VAR NAME'
        p[0] = nd.Node("declaration_procedure",[p[1],p[2],p[3]], [])

    def p_declaration_parameters(self,p):
        'declarationParameters : declarationParameter'
        p[0] = nd.Node("declaration_procedure",[p[1]], [])

    def p_declaration_parameters(self,p):
        'declParameters : declarationParameter SEMI declarationParameter'
        p[0] = nd.Node("declaration_procedure",[p[1]], [])

    #==============================================================================

    def p_error(self, p):
        if p:
            print("Syntax error at '%s'" % p.value)
        else:
            print("Syntax error at EOF")

