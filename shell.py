import shark


while True:
    text = input('SHARK > ')
    result, error = shark.run('<stdin>', text)

    if error:
        print(error)
    elif result:
        print(str(result))
