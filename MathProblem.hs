
-- solution #1
is_palindrome x = x == reverse x

-- solution #2
is_palindrome_rec x | length x <= 1 = True
                | head x == last x = is_palindrome_rec . tail. init $ x
                | otherwise = False

-- solution #3
isPalindrome = (==) <*> reverse
