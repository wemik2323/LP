import re

token_specification = [
    ('preprocessor', r'#\b(?:include|define|if|endif|ifdef|ifndef|pragma)\b'),  # Препроцессорные директивы
    ('header', r'<[A-Za-z_][A-Za-z0-9_]*>'),  # Заголовочные файлы внутри <>
    ('keyword', r'\b(?:if|else|while|for|return|int|float|double|char|bool|void|class)\b'),  # Ключевые слова
    ('identifier', r'[A-Za-z_][A-Za-z0-9_]*'),  # Идентификаторы (переменные, функции и т.д.)
    ('operator', r'[+\-*/=<>!]=|==|!=|&&|\|\||[+\-*/=<>]'),  # Операторы
    ('separator', r'[{}();,]'),  # Разделители
    ('literal_number', r'\b\d+(\.\d*)?|\b0x[0-9A-Fa-f]+\b'),  # Литералы чисел
    ('literal_string', r'"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])\''),  # Строковые литералы
    ('comment', r'\/\*[\s\S]*?\*\/|\/\/.*'),  # Комментарии
    ('whitespace', r'\s+'),  # Пробелы игнорируются
]

# Компиляция регулярных выражений
token_regex = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in token_specification)
get_token = re.compile(token_regex)

def lex_analysis(text):
    tokens = []
    for match in re.finditer(get_token, text):
        token_type = match.lastgroup
        token_value = match.group(token_type)
        if token_type != 'whitespace':  # Пропуск пробелов
            tokens.append((token_type, token_value))
    return tokens

# Тест
filename = input("Введите название файла для лексического разбора: ")
with open(filename, 'r', encoding='utf-8') as file:
    input_code = file.read()

tokens = lex_analysis(input_code)
for token in tokens:
    print(token)

