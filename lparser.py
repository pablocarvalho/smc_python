import sys
sys.path.insert(0, "../..")

if sys.version_info[0] >= 3:
    raw_input = input

import ply.lex as lex
import ply.yacc as yacc
import os
import node as nd



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
        'tt':'TT',
        'ff':'FF',
        'nil':'NIL',
        'or':'OR',
        'and':'AND',
        'const':'CONST',
        'var':'VAR',
        'let':'LET',
        'in':'IN',
        'bool':'BOOL',
        'int':'INT',
    }

    tokens = list(reserved.values()) + [
        'NAME', 'NUMBER',
        'PLUS', 'MINUS', 'EXP', 'TIMES', 'DIVIDE', 'EQUALS',
        'LPAREN', 'RPAREN',
        'SEMI',                
        'EQ',        
        'NOT',
        'COLON',
    ]

    # Tokens

    t_PLUS = r'\+'
    t_MINUS = r'-'
    t_EXP = r'\*\*'
    t_TIMES = r'\*'
    t_DIVIDE = r'/'
    t_EQUALS = r':='
    t_LPAREN = r'\('
    t_RPAREN = r'\)'

    t_EQ = r'='
    t_COLON = r':'

    t_NOT = r"~"

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
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE'),
        ('left', 'EXP'),
        ('right', 'UMINUS'),
    )

    def p_command_assign(self, p):
        'command : NAME EQUALS expression'
        self.names[p[1]] = p[3]
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

    def p_command_block(self, p):
        'command : LPAREN command RPAREN'
        p[0] = nd.Node("command_block", [p[2]], [p[1],p[3]])        
        

    def p_command_conditional(self,p):
        """
		command : IF booleanexpression THEN command ELSE command
               |  IF expression THEN command ELSE command
		"""
        p[0] = nd.Node("conditional", [p[2],p[4],p[6]], [p[1],p[3],p[5]])             

    def p_command_loop(self,p):
        """
		command : WHILE booleanexpression DO command
               |  WHILE expression DO command
		"""
        p[0] = nd.Node("loop", [p[2],p[4]], [p[1],p[3]])


    def p_booleanexpression_binop(self,p):
        """
        booleanexpression : booleanexpression OR booleanexpression            
                         | booleanexpression AND booleanexpression            
                         | expression EQ booleanexpression            
                         | booleanexpression EQ expression            
                         | expression EQ expression            
        """
        p[0] = nd.Node("booleanexpression_binop", [p[1],p[3]], p[2])

    def p_booleanexpression(self,p):
        'booleanexpression : NOT booleanexpression'
        p[0] = nd.Node("booleanexpressionnot", [p[2]], p[1])        

    def p_booleanexpression_values(self,p):
        """ booleanexpression : TT 
                             | FF
        """
        p[0] = p[1]

    def p_expression_binop(self, p):
        """
        expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression EXP expression
        """
        p[0] = nd.Node("binop", [p[1],p[3]], p[2])
   
    def p_expression_uminus(self, p):
        'expression : MINUS expression %prec UMINUS'
        p[0] = -p[2]

    def p_booleanexpression_group(self, p):
        'booleanexpression : LPAREN booleanexpression RPAREN'
        #p[0] = p[2]
        p[0] = nd.Node("booleanexpression_block", [p[2]], [p[1],p[3]]) 

    def p_expression_group(self, p):
        'expression : LPAREN expression RPAREN'
        p[0] = p[2]	

    def p_expression_number(self, p):
        'expression : NUMBER'
        p[0] = p[1]

    def p_expression_name(self, p):
        'expression : NAME'
        p[0] = p[1]
        # try:
            # p[0] = self.names[p[1]]
        # except LookupError:
            # print("Undefined name '%s'" % p[1])
            # p[0] = 0

    def p_command_declaration(self, p):
        'command : declaration'
        p[0] = p[1]

    def p_declaration_int(self, p):
        """
        declaration : CONST NAME COLON INT EQ expression
                  | VAR NAME COLON INT EQ expression
        """
        p[0] = nd.Node("declaration", [p[1],p[2],p[4],p[6]], [p[3],p[5]])

    def p_declaration_bool(self, p):
        """
        declaration : CONST NAME COLON BOOL EQ booleanexpression
                  | VAR NAME COLON BOOL EQ booleanexpression
        """
        p[0] = nd.Node("booldeclaration", [p[1],p[2],p[4],p[6]], [p[3],p[5]])

    def p_declaration_and(self, p):
        """
        declaration : declaration AND declaration
        """
        p[0] = nd.Node("declarationand", [p[1],p[3]], p[2])

    def p_declaration_in(self, p):
        """
        declaration : declaration IN declaration
        """
        p[0] = nd.Node("declarationin", [p[1],p[3]], p[2])

    def p_declaration_let(self, p):
        """
        declaration : LET declaration IN expression
        """
        p[0] = nd.Node("declarationlet", [p[2],p[4]], [p[1],p[3]])

    def p_command_assign_if(self, p):
        """command : NAME EQUALS IF booleanexpression THEN expression ELSE expression
                     |  NAME EQUALS IF expression THEN expression ELSE expression
		"""
        # self.names[p[1]] = p[3]
        p[0] = nd.Node("assignif",[p[1],p[4],p[6],p[8]],[p[2],p[3],p[5],p[7]])

    def p_expression_declaration_bool_if(self, p):
        """declaration : CONST NAME COLON BOOL EQ IF booleanexpression THEN expression ELSE expression
                  |  VAR NAME COLON BOOL EQ IF booleanexpression THEN expression ELSE expression
                  |  CONST NAME COLON BOOL EQ IF expression THEN expression ELSE expression
                  |  VAR NAME COLON BOOL EQ IF expression THEN expression ELSE expression
		"""
        # self.names[p[1]] = p[3]
        p[0] = nd.Node("declarationboolif",[p[1],p[2],p[4],p[7],p[9],p[11]],[p[3],p[5],p[6],p[8],p[10]])

    def p_expression_declaration_int_if(self, p):
        """declaration : CONST NAME COLON INT EQ IF booleanexpression THEN expression ELSE expression
                  |  VAR NAME COLON INT EQ IF booleanexpression THEN expression ELSE expression
                  |  CONST NAME COLON INT EQ IF expression THEN expression ELSE expression
                  |  VAR NAME COLON INT EQ IF expression THEN expression ELSE expression
		"""
        # self.names[p[1]] = p[3]
        p[0] = nd.Node("declarationintif",[p[1],p[2],p[4],p[7],p[9],p[11]],[p[3],p[5],p[6],p[8],p[10]])

    def p_error(self, p):
        if p:
            print("Syntax error at '%s'" % p.value)
        else:
            print("Syntax error at EOF")

