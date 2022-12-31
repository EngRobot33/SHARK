import shark


while True:
    text = input('SHARK > ')
    result, error = shark.run('<stdin>', text)

    print(error) if error else print(result)
