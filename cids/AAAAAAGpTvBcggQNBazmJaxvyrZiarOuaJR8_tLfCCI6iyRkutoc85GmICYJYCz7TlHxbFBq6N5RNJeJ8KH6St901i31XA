def roman(number):
    """Write a positive integer in Roman numerals."""
    numerals = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
                (100, "C"), (90, "XC"), (50, "L"), (40, "XL"),
                (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]
    letters = ""
    for value, numeral in numerals:
        count, number = divmod(number, value)
        letters += numeral * count
    return letters
