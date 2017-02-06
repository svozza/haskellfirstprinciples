stops  = "pbtdkg"
vowels = "aeiou"

svs = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

svsP = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["dog", "cat"]
verbs = ["walk", "ran"]

sentences = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc' x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))
