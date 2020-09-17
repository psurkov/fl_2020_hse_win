import ply.lex as lex 
import sys

reserved = {
  'module': 'MODULE', 
  'sig': 'SIG',
  'type': 'TYPE'
}

tokens = [
  'ID',
  'NUM', 
  'LITERAL',
  'ARROW', 
  'DECLARE',
  'COMMA',
  'DOT',
  'LEFTBKT',
  'RIGHTBKT',
  'BAR',
] + list(reserved.values())

def t_ID(t):
  r'[a-z_A-Z][a-z_0-9A-Z]*'
  t.type = reserved.get(t.value, 'ID')
  return t

def t_NUM(t): 
  r'[0-9]+'
  t.value = int(t.value)
  return t

def t_LITERAL(t):
  r'"[^"]*"'
  t.value = t.value[1:-1]
  return t;

t_ARROW = r'->'
t_DECLARE = r':-'
t_COMMA = r','
t_DOT = r'\.'
t_LEFTBKT = r'\['
t_RIGHTBKT = r']'
t_BAR= r'\|'

t_ignore = ' \t'

def t_newline(t): 
  r'\n+'
  t.lexer.lineno += len(t.value)

def t_error(t): 
  print("Illegal character '%s'" % t.value[0])
  t.lexer.skip(1)

inputFilename = sys.argv[1]
outputFilename = inputFilename.rsplit('.', 1)[0] + ".out"

lexer = lex.lex() 
with open(inputFilename) as file:
  lexer.input(file.read())

with open(outputFilename, 'w') as file:
  while True: 
    tok = lexer.token() 
    if not tok: 
      break

    file.write(' '.join([tok.type, str(tok.value), str(tok.lineno), str(tok.lexpos)]) + '\n')
