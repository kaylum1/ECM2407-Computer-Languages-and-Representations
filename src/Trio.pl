%QUESTION 3.1 GENERATING NUMBER PERFECT SQUARE FORM 1,000 - 1,000,000
%return true if number is perfect square
is_perfect_square(N) :-
    S is floor(sqrt(N)),
    S * S =:= N.



generator3(N) :-
    between(1000, 1000000, N),  % Generate numbers between 1000 and 1000000 (inclusive)
    is_perfect_square(N).       % Check if the number is a perfect square


x_generator3( N ) :- 
    x_generator3_loop(
        [ 1024 , 9409 , 23716 , 51529 
        , 123904 , 185761 , 868624 , 962361 
        , 982081, 1000000 ], 0, N ).

x_generator3_loop( [], C, C ). 
x_generator3_loop( [T|TS], C, N ) :-
    generator3( T ),
    C1 is C + 1,
    x_generator3_loop( TS, C1, N ). 
x_generator3_loop( [_|TS], C, N ) :-
    x_generator3_loop( TS, C, N ).




%QUESTION 3.2 ADDING NEW CRITERIA FOR N ----------------------------------------

% set function to makw sure all digits in a number is 0
all_digits_different(N) :-
    number_codes(N, Digits),
    all_digits_different_helper(Digits).

all_digits_different_helper([]).
all_digits_different_helper([H|T]) :-
    \+ member(H, T),
    all_digits_different_helper(T).


%set of function to make sure there is only one zero in
one_and_only_one_zero(N) :-
    number_codes(N, Digits),
    count_occurrences(48, Digits, 1).

count_occurrences(_, [], Count) :-
    Count = 0.  % Ensure only one occurrence is allowed
count_occurrences(E, [H|T], Count) :-
    (E = H -> NewCount is Count - 1 ; NewCount is Count),
    count_occurrences(E, T, NewCount).



% check to see if a number second to last is odd 
penultimate_digit_odd(N) :-
    N >= 10,  % Ensure Number has at least two digits
    PenultimateDigit is (N // 10) mod 10,
    1 is PenultimateDigit mod 2.



length_list([], 0).
length_list([_|T], N) :-
   length_list(T, W), 
   N is W + 1.
%co,apre the last digit and length 
length_equals_last_digit(N) :-
   number_chars(N, Chars),
   maplist(char_code, Chars, AsciiValues),
   maplist(subtract_48, AsciiValues, Digits),
   length_list(Digits, Length),
   last(Digits, LastDigit),
   Length =:= LastDigit.

subtract_48(AsciiValue, Digit) :-
   Digit is AsciiValue - 48.

%comapres the first, second, second to last digits multiple of first
compare_first_second_digits9(N) :-
    number_codes(N, [FirstCode, SecondCode | _]),
    digit_code(FirstDigit, FirstCode),
    digit_code(SecondDigit, SecondCode),
    is_multiple2(FirstDigit, SecondDigit).

compare_first_third_digits(N) :-
    number_codes(N, [FirstCode, _ , ThirdCode | _]),
    digit_code(FirstDigit, FirstCode),
    digit_code(ThirdDigit, ThirdCode),
    is_multiple2(FirstDigit, ThirdDigit).


compare_first_secondLast_digits(N) :-
  number_codes(N, Codes),
  length(Codes, Length),
  Length1 is Length - 2,
  nth0(0, Codes, FirstDigitCode),
  nth0(Length1, Codes, SecondLastDigitCode),
  digit_code(FirstDigit, FirstDigitCode),
  digit_code(SecondLastDigit, SecondLastDigitCode),
  is_multiple2(FirstDigit, SecondLastDigit).


is_multiple2(A, B) :-
    B mod A =:= 0.

digit_code(Digit, Code) :-
    Digit is Code - 48.  % Convert character code to integer




tester3(N) :-
    all_digits_different(N),
    one_and_only_one_zero(N),
    penultimate_digit_odd(N),
    length_equals_last_digit(N),
    compare_first_second_digits9(N),
    compare_first_third_digits(N),
    compare_first_secondLast_digits(N).





x_tester3( N ) :- 
    x_tester3_loop(
        [ 123056 , 128036 , 139076 
        , 142076 , 148056 , 159076 
        , 173096 , 189036, 193056, 198076 ], 0, N ).
x_tester3_loop( [], C, C ). 
x_tester3_loop( [T|TS], C, N ) :-
    tester3( T ),
    C1 is C + 1,
    x_tester3_loop( TS, C1, N ).
x_tester3_loop( [_|TS], C, N ) :-
    x_tester3_loop( TS, C, N ).




main :-
%generator3( N ).
%x_tester3( N ).
%generator3( N ), tester3( N ), write( N ).

