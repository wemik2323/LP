def dfa(word):
    state = 0

    for char in word:
        if state == 0:
            if char == '1':
                state = 1 
            elif char == '0':
                state = 0  
        elif state == 1:
            if char == '1':
                state = 2   
            elif char == '0':
                state = 0   
        elif state == 2:
            if char == '1':
                state = 2  
            elif char == '0':
                state = 0  

    if state == 2:
        return True, state  
    else:
        return False, state  


word = input("Введите слово из алфавита {0, 1}: ")
belongs, final_state = dfa(word)
if belongs:
    print(f"Слово принадлежит языку. Конечное состояние: {final_state}")
else:
    print(f"Слово не принадлежит языку. Конечное состояние: {final_state}")

