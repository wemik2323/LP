class VendingMachine:
    def __init__(self):
        self.state = 0  

    def insert_coin(self, coin):
        if self.state == 0:
            if coin == 1:
                self.state = 1
                return -1
            elif coin == 2:
                return 0
            elif coin == 5:
                return 3
            elif coin == 10:
                return 8

        elif self.state == 1:
            if coin == 1:
                self.state = 0
                return 0
            elif coin == 2:
                self.state = 0
                return 1
            elif coin == 5:
                self.state = 0
                return 4
            elif coin == 10:
                self.state = 0
                return 9

    def output_state(self, change):
        if self.state == 0:
            if change >= 0:
                print(f"Сумма: 0 рублей. Выдаем товар. Сдача: {change}")
            elif change < 0:
                print("Сумма: 0 рублей. AFK.")
        elif self.state == 1:
            print("Сумма: 1 рубль. AFK.")


if __name__ == "__main__":
    vm = VendingMachine()

    while True:
        try:
            coin = int(input("Вставьте монету (1, 2, 5, 10): "))
            if coin not in [1, 2, 5, 10]:
                print("Неверная монета. Попробуйте снова.")
                continue
            change = vm.insert_coin(coin)
            vm.output_state(change)
        except ValueError:
            print("Пожалуйста, введите числовое значение.")

