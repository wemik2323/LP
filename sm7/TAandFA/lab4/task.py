import re

def extract_phone_numbers(filename):
    with open(filename, 'r', encoding='utf-8') as file:
        content = file.read()

    phone_pattern = re.compile(
        r'(\+7\s?\(\d{3}\)\s?\d{3}-\d{2}-\d{2})|' # +7 (XXX) XXX-XX-XX
        r'(\+7\s?\d{3}\s?\d{3}-\d{2}-\d{2})|'     # +7 XXX XXX-XX-XX
        r'(8\s?\(\d{3}\)\s?\d{3}-\d{2}-\d{2})|'   # 8 (XXX) XXX-XX-XX
        r'(8\s?\d{3}\s?\d{3}-\d{2}-\d{2})'        # 8 XXX XXX-XX-XX
    )
    
    phone_numbers = phone_pattern.findall(content)
    
    for number_group in phone_numbers:
        for number in number_group:
            if number:
                print(number)

if __name__ == "__main__":
    filename = "file.html"
    extract_phone_numbers(filename)

