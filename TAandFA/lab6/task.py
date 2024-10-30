import re

token_specification = [
    # While конструкция
    ('WHILE', r'while'),                       # ключевое слово while
    ('LBRACKET', r'\('),                       # левая скобка (
    ('RBRACKET', r'\)'),                       # правая скобка )
    ('LBRACE', r'\{'),                         # левая фигурная скобка {
    ('RBRACE', r'\}'),                         # правая фигурная скобка }

    # Условие
    ('OP', r'==|!=|<=|>=|<|>'),                # операторы сравнения
    ('ID', r'[A-Za-z_][A-Za-z0-9_]*'),         # идентификатор
    ('NUM', r'\b\d+(\.\d*)?'),  # числа (HEX?)

    # Выражения
    ('literal_string', r'"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*'),  # Строковые литералы
    ('ASSIGN', r'='),                          # оператор присваивания
    ('COMPOUND_ASSIGN', r'\+=|-=|\*=|/='),     # сокращенные операторы присваивания
    ('SEMICOLON', r';'),                       # точка с запятой
    ('ARITH_OP', r'[+\-*/]'),                  # арифметические операторы +, -, *, /
    ('separator', r'[{}();,]'),                # Разделители
    ('comment', r'\/\*[\s\S]*?\*\/|\/\/.*'),   # Комментарии
    ('WHITESPACE', r'\s+'),                    # Пробелы (игнорируются)
]
token_regex = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in token_specification)
get_token = re.compile(token_regex)

# Лексический анализ
def lex_analysis(text):
    tokens = []
    for match in re.finditer(get_token, text):
        token_type = match.lastgroup
        token_value = match.group(token_type)
        if token_type != 'WHITESPACE' and token_type != 'comment':  # Пропуск пробелов и комментариев
            tokens.append((token_type, token_value))
    return tokens

# Синтаксический анализатор
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def consume(self, expected_type):
        if self.pos < len(self.tokens) and self.tokens[self.pos][0] == expected_type:
            self.pos += 1
        else:
            raise SyntaxError(f"Expected {expected_type} at position {self.pos}")

    def parse(self):
        self.while_stmt()
        if self.pos < len(self.tokens):
            raise SyntaxError("Extra input after end of while statement")

    def while_stmt(self):
        self.consume('WHILE')
        self.consume('LBRACKET')
        self.condition()
        self.consume('RBRACKET')
        self.consume('LBRACE')
        self.body()
        self.consume('RBRACE')

    def condition(self):
        self.expression()
        self.consume('OP')
        self.expression()

    def expression(self):
        self.term()
        while self.pos < len(self.tokens) and self.tokens[self.pos][0] in ('ARITH_OP', 'COMPOUND_ASSIGN'):
            self.pos += 1
            self.term()

    def term(self):
        if self.pos < len(self.tokens) and self.tokens[self.pos][0] in ('ID', 'NUM'):
            self.pos += 1
        else:
            raise SyntaxError(f"Expected identifier or number at position {self.pos}")

    def body(self):
        while self.pos < len(self.tokens) and self.tokens[self.pos][0] != 'RBRACE':
            self.statement()

    def statement(self):
        self.consume('ID')

        # Либо сокращенка либо классика
        if self.tokens[self.pos][0] == 'COMPOUND_ASSIGN':
            self.consume('COMPOUND_ASSIGN')
        else:
            self.consume('ASSIGN')
        self.expression()
        self.consume('SEMICOLON')

# Проверка
input_code = """
while (x 3232> a + 25) {
}
"""
tokens = lex_analysis(input_code)

print("Токены:")
for i, token in enumerate(tokens, start=1):
    print(f"{i}: {token}")
parser = Parser(tokens)

try:
    parser.parse()
    print("Строка соответствует грамматике цикла while.")
except SyntaxError as e:
    print(f"Синтаксическая ошибка: {e}")

