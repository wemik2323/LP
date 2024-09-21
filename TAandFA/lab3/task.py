class VendingMachine:
    def __init__(self):
        self.state = 0  

    def insert_coin(self, coin):
        if self.state == 0:
            if coin == 1:
                self.state = 1
            elif coin == 2:
                self.state = 2
            elif coin == 5:
                self.state = 5
        elif self.state == 1:
            if coin == 1:
                self.state = 2
            elif coin == 2:
                self.state = 3
            elif coin == 5:
                self.state = 6
        elif self.state == 2:
            if coin == 1:
                self.state = 3
            elif coin == 2:
                self.state = 4
            elif coin == 5:
                self.state = 7
        elif self.state == 3:
            if coin == 1:
                self.state = 4
            elif coin == 2:
                self.state = 5
            elif coin == 5:
                self.state = 8
        elif self.state == 4:
            if coin == 1:
                self.state = 5
            elif coin == 2:
                self.state = 6
            elif coin == 5:
                self.state = 9
        elif self.state == 5:
            self.state = 0
        elif self.state == 6:
            self.state = 0
        elif self.state == 7:
            self.state = 0
        elif self.state == 8:
            self.state = 0
        elif self.state == 9:
            self.state = 0

    def output_state(self):
        if self.state == 0:
            print("Сумма: 0 рублей, ничего не делаем")
        elif self.state == 1:
            print("Сумма: 1 рубль, ничего не делаем")
        elif self.state == 2:
            print("Сумма: 2 рубля, ничего не делаем")
        elif self.state == 3:
            print("Сумма: 3 рубля, ничего не делаем")
        elif self.state == 4:
            print("Сумма: 4 рубля, ничего не делаем")
        elif self.state == 5:
            print("Сумма: 5 рублей, выдаём товар")
        elif self.state == 6:
            print("Сумма: 6 рублей, выдаём товар и сдачу 1 рубль")
        elif self.state == 7:
            print("Сумма: 7 рублей, выдаём товар и сдачу 2 рубля")
        elif self.state == 8:
            print("Сумма: 8 рублей, выдаём товар и сдачу 3 рубля")
        elif self.state == 9:
            print("Сумма: 9 рублей, выдаём товар и сдачу 4 рубля")


if __name__ == "__main__":
    vm = VendingMachine()

    while True:
        try:
            coin = int(input("Вставьте монету (1, 2, 5): "))
            if coin not in [1, 2, 5]:
                print("Неверная монета. Попробуйте снова.")
                continue
            vm.insert_coin(coin)
            vm.output_state()
        except ValueError:
            print("Пожалуйста, введите числовое значение.")

