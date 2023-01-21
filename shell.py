import shark


while True:
    text = input('SHARK > ')
    if text.strip() == "": continue
    result, error = shark.run('<stdin>', text)

    if error:
        print(error)
    elif result:
        if len(result.elements) == 1:
            print(str(result.elements[0]))
        else:
            print(str(result))
