def collatz(n: int) -> list[int]:
    res: list[int] = [n]
    while n != 1:
        if n % 2 == 0:
            n //= 2
        else:
            n = 3 * n + 1
        res.append(n)
    return res


def solve_collatz(n: int) -> int:
    answer, current_max = 0, 0
    for i in range(1, n + 1):
        collatz_length = len(collatz(i))
        if collatz_length > current_max:
            answer = i
            current_max = collatz_length
    return answer


def to_words(n: int) -> str:
    ones = [
        "", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
        "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
    ]
    
    tens = [
        "", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"
    ]
    
    hundred = "hundred"

    if n == 1000:
        return "one thousand"
    
    word = ""

    if n >= 100:
        word += ones[n // 100] + " " + hundred
        if n % 100 != 0:
            word += " and "

    n = n % 100
    if n < 20:
        word += ones[n]
    else:
        word += tens[n // 10]
        if n % 10 != 0:
            word += "-" + ones[n % 10]
    
    return word


def solve_number_letters() -> int:
    words_nums = [to_words(num) for num in range(1, 1001)]
    lengths = [len(words.replace(' ', '').replace('-', '')) for words in words_nums]
    return sum(lengths)


print("Problem 17:", solve_number_letters())
print("Problem 14:", solve_collatz(1_000_000))
