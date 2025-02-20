def lex(code):
    i = 0
    tokens = []
    length = len(code)
    
    while i < length:
        char = code[i]

        # Пропуск пробелов
        if char.isspace():
            i += 1
            continue

        # Идентификаторы и ключевые слова
        if char.isalpha() or char == '_':
            start = i
            while i < length and (code[i].isalnum() or code[i] == '_'):
                i += 1
            identifier = code[start:i]
            if identifier in ['if', 'else', 'while', 'return']:  # ключевые слова
                tokens.append(('KEYWORD', identifier))
            else:
                tokens.append(('IDENTIFIER', identifier))
            continue

        # Числа
        if char.isdigit():
            start = i
            while i < length and code[i].isdigit():
                i += 1
            if i < length and code[i] == '.':  # если встречается десятичная точка
                i += 1
                while i < length and code[i].isdigit():
                    i += 1
            tokens.append(('NUMBER', code[start:i]))
            continue

        # Операторы
        if char in '+-*/=':
            if i + 1 < length and code[i+1] == '=':
                tokens.append(('OPERATOR', char + '='))
                i += 2
            else:
                tokens.append(('OPERATOR', char))
                i += 1
            continue

        # Разделители
        if char in '{}();,':
            tokens.append(('SEPARATOR', char))
            i += 1
            continue

        # Если токен не распознан
        raise ValueError(f"Unknown character: {char}")

    return tokens

def main():
    filename = input("Введите имя файла: ")
    with open(filename, 'r', encoding='utf-8') as file:
        code = file.read()

    tokens = lex(code)
    print(tokens)
 
if __name__ == '__main__':
    main()

    
