import random

rules = {
    'S': ['AA'],
    'A': ['aAb', 'ab']
}

def generate_word(symbol):
    if symbol in rules:
        production = random.choice(rules[symbol])
        return ''.join(generate_word(s) for s in production)
    else:
        return symbol

random_word = generate_word('S')
print(f"Случайное слово: {random_word}")

