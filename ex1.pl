
% Auxilery predicates for Task 1 / Task 2, learned in class.

/*
 * unary_nat(X), will succeed if and only if X is a natural number in unary notation.
 */
unary_nat(0).
unary_nat(s(X)):- unary_nat(X).


/*
 * Assuming X, Y and Z are natural numbers in unary notation, unary_add(X, Y, Z) will succeed if and only if X + Y = Z.
 */
unary_add(0, X, X).
unary_add(s(X), Y, s(Z)):-
    unary_add(X, Y, Z).

/*
 * unary_times(X, Y , Z) will succeed if and only X, Y  and Z are natural numbers in unary notation and if X * Y = Z
 * Slightly changed version of the one learned in class, to allow mode unary_times(-, -, +).
 */
unary_times(0, X, 0):- unary_nat(X).
unary_times(s(X), Y, Z):-
    unary_add(Y, W, Z),
    unary_times(Y, X, W).





% Task 1.
/*
 * Assuming N, K are natural numbers in unary notation, unary_sqrt(N, K) will succeed if and only if 
 * K * K <= N, (K + 1) * (K + 1) > N, with mode unary_sqrt(+, -).
 * We will use an auxilary predicate to remember N, unary_sqrt_aux(M, K, N) which succeeds if and only if there is a P <= M,
   such that K * K = P, and (K + 1) * (K + 1) > N. By initiallising M as N we will get the sqrt of N.
 */
unary_sqrt_aux(s(M), K, N):-
    unary_sqrt_aux(M, K, N).  % decrease M

unary_sqrt_aux(M, K , N):-
    unary_times(K,K, M), 
    unary_times(s(K), s(K), L),
    unary_add(s(_), N, L).   % If N + X = L , X > 0 then L >  N.



unary_sqrt(N, K):-
    unary_sqrt_aux(N, K, N).  % using the auxilary predicate with M = N.               
 




% Task 2.
/*
 * Assuming N, K are natural numbers in unary notation, unary_divisor(N, K) will succeed if and only if K is a divisor of N.
 * With mode unary_divisor(+, -), which given an integer N in unary notation, unifies K with every possible divisor of N.
 */
unary_divisor(N, s(K)):-  % K(divisor) cannot be 0.
    unary_times(_, s(K), N).  % if K * X = N, where X is a natural number, then K is a divisor of N.





% Task 3.
/*
 * binary_plus(X, Y, Z),  with mode binary_plus(+, +, -), will succeed if and only if
 * X, Y and Z are natural numbers in binary notation such that X + Y = Z,
 * We will use 3 additional predicates to help with the task.
 */


/*
 * positive_binary_number(X) succeeds if and only if X is a positive number in binary notation
 */
positive_binary_number([1]). % postive numbers in binary notation must end with '1' (MSB).
positive_binary_number([X | Xs]):-
    member(X, [0,1]),
    positive_binary_number(Xs).


/*
 * full_adder(X, Y, Cin, Cout, Z) will be used to calculate for X + Y + Cin ,the bits Cout and Z given the bits X, Y and Cin.
 * Assumes X, Y, Cin, Cout, Z are all bits ( 0 or 1 )
 */
full_adder(X, X, 0, X, 0).
full_adder(X, X, 1, X, 1).
full_adder(0, 1, 0, 0, 1).
full_adder(0, 1, 1, 1, 0).
full_adder(1, 0, 0, 0, 1).
full_adder(1, 0, 1, 1, 0).



/*
 * binary_plus_aux(X, Y, Cin, Z) assumes X, Y and Z are positive numbers in binary notation, and Cin is a bit ( 0 or 1 ).
 * Will succeed if and only if X + Y + Cin = Z.
 * We will use the auxilary predicate to pass the calculated Cout as the Cin of the next bit starting from the LSB of X and Y.
 */
% Base rules for reaching an empty list ( 0 ) in either X , Y or both.
binary_plus_aux([], [], 1, [1]). % 0 + 0 + 1 ( Cin) = 1.
binary_plus_aux([], Y,  0, Y). % 0 + Y + 0 = Y.
binary_plus_aux(X, [], 0, X). % X + 0 + X = 0.

binary_plus_aux([], [Y | Ys], 1, [Z | Zs]):- % Still more bits in Y but none in X and Cin = 1 => Z = Y + Cin.
    full_adder(0, Y, 1, Cout, Z),
    binary_plus_aux([], Ys, Cout, Zs).

binary_plus_aux([X | Xs], [], 1, [Z | Zs]):- % Still more bits in X but none in Y and Cin = 1 => Z = X + Cin.
    full_adder(0, X, 1, Cout, Z),
    binary_plus_aux(Xs, [], Cout, Zs). 

binary_plus_aux([X | Xs], [Y | Ys], Cin, [Z | Zs]):- 
    full_adder(X, Y, Cin, Cout, Z), % Get Cout and check if Z = X + Y + Cin
    binary_plus_aux(Xs, Ys, Cout, Zs). % Cout is the new Cin.
 


% Base rules for 0 + 0, 0 + X and X + 0.
binary_plus([], [], []).
binary_plus([], X, X):- positive_binary_number(X).
binary_plus(X, [], X):- positive_binary_number(X).

% Checking that Both X and Y are positive binary numbers since we covered base cases for 0.
binary_plus(X, Y, Z):-
    positive_binary_number(X),
    positive_binary_number(Y),
    binary_plus_aux(X, Y, 0, Z). % Using the auxilary predicate with Cin of 0 will get succeed if X + Y = Z.





% Task 4.
/*
 * binary_times(X, Y, Z), with mode binary_times(+, +, -), will succeed if and only if
 * X, Y and Z are natural numbers in binary notation such that X * Y = Z.
 * We will use an algorithem that iterates over the bits of Y starting from the LSB preforming long multiplaction
 * each step shifting X to the right
 */


/*
 * We will use an auxilary predicate binary_times_aux(X, Y, Z, AcumSum) to pass the accumlated sum of all the steps so far.
 */
binary_times_aux(_, [], Z, Z). % finished iterating over Y, succeeds only if the accumulated sum = Z.

binary_times_aux(X, [0 | Ys], Z, AcumSum):- % X * 0 = 0, no need to add to the summ.
    binary_times_aux([0 | X], Ys, Z, AcumSum). % Shifting X to the right by adding 0 as the new LSB

binary_times_aux(X, [1 | Ys], Z, AcumSum):- % X * 1 = X
    binary_plus(AcumSum, X, StepSum), % Calculate StepSum which is the new accumulated sum.
    binary_times_aux([0 | X], Ys, Z, StepSum). % Keep iterating Y with the new AcumSum and Shift X to the right.


% Base rules for 0 * 0, 0 * X, X * 0.  
binary_times([],[],[]).
binary_times([], X, []):- positive_binary_number(X).
binary_times(X, [], []):- positive_binary_number(X).

% Checking that Both X and Y are positive binary numbers since we covered base cases for 0.
binary_times(X, Y, Z):-
    positive_binary_number(X),
    positive_binary_number(Y),
    binary_times_aux(X, Y, Z, []). % initializing the accumulated sum to 0.


 


% Task 5
/*
 * is_prime(N), with mode is_prime(+), will succeed if and only if N is a prime number.
 * The primality testing algorithem used is Sieve of Eratosthenes.
 * External source used for the algorithem : https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
 * Given a positive number N, initialize a List with numbers 2....sqrt(N).
 * Iterate over the list.
 * If N mod ai == 0, then N is not a prime number.
 * Else, N mod ai =\= 0, remove all elements aj of the list such that ai * x = aj for x natural number > 0.
 * If reached the end of the list, N is a prime number.
 * We can remove all the elements aj since if ai is not a divisor of N then ai * x is not a divisor of N for x natural number > 0.
 */ 


/*
 * intList(Start, End, List) with mode intList(+, +, -), will succeed if and only if List is a list of integers from Start to End.
 * We will use this predicate to initialize a list with numbers from 3 to sqrt(N) with increments of 2,
   Since we will only test once if N is an even number.
 */
intList(Start, End, []):- 
    End < Start.

intList(Start, End, [Start | List]):-
    Start =< End,
    Next is Start + 2,
    intList(Next, End, List).


/*
 * removeMultiples(Multiplier, List, NewList), with mode removeMultiples(+, +, -), will succeed if and only if
   after removing all the multiples of Multiplier from List we get NewList.
 */   
removeMultiples(_, [], []). % reached the end of the list. 

removeMultiples(Multiplier, [Num | NumList], NewNumList):- % not adding Num if Num is a mutiple of Multiplier.
    0 is Num mod Multiplier,
    removeMultiples(Multiplier, NumList, NewNumList).

removeMultiples(Multiplier, [Num | NumList], [Num | NewNumList]):- % adding Num from List only if Num isnt a multpile of Multiplier.
    Num mod Multiplier =\= 0,
    removeMultiples(Multiplier, NumList, NewNumList).


/*
 * is_prime_sieve(N, List) will succeed if and only if N mod ai =\= 0 for all ai in List.
 * Will be used so we can "remember" the list of integers.
 */ 
is_prime_sieve(_, []).
is_prime_sieve(N, [Num | List]):-
    N mod Num =\= 0, 
    removeMultiples(Num, List, NewList), % remove all multiples of Multiplier from List.
    is_prime_sieve(N, NewList). % Keep iterating over the new list 



is_prime(2).
is_prime(N):-
    N > 2, % checked for 2 in base case.
    N mod 2 =\= 0, % N is not an even number.
    SqrtN is round(sqrt(N)), 
    intList(3, SqrtN, List), % initialize List from 3 to the square root of N with incrememnts of 2.
    is_prime_sieve(N, List).





% Task 6. 
/*
 * right_prime(N), with mode right_prime(+), will succeed if and only if N is a right truncatable prime.
 */
% base case for N with 1 digit
right_prime(N):-
    N < 10,
    is_prime(N).

right_prime(N):-
    is_prime(N),
    Truncat is round(div(N , 10)), % truncat N (divide by 10 and round), eventually reaching 0, which is not a prime.
    right_prime(Truncat). % test the truncat N to be a right prime.
    




% Task 7.
/*
 * right_prime_gen(N) with mode right_prime_gen(-) will unify its arguement with right primes.
 * we will use the algorithem suggested in the provided link of https://oeis.org/A024770.
 * There are 83 right primes, by using the algorithem we will eventually reach a none prime number on all calls and backtracks.
 */ 
 

/*
 * By using an auxilary predicate right_prime_gen_aux(N, A), which will recieve a prime number A,
   shift A to the left and add either  1 , 3 , 7 or 9 to make a A1.
 * If A1 is a prime number then recursive call for A1, eventually reaching a none prime number.
 */
right_prime_gen_aux(N, N). % for any prime number A we will call right_prime_gen_aux with, A is a right prime.
right_prime_gen_aux(N, A):-
    member(S, [1, 3, 7, 9]), % Testing all possibilities of S that can be appended to make a right prime.
    A1 is (A * 10 + S), % Shifting left in base 10 and adding S
    is_prime(A1), 
    right_prime_gen_aux(N, A1). % Keep testing A1 if A1 is a prime number



right_prime_gen(N):-
    member(A, [2, 3, 5, 7]), % Only prime single digit numbers can be the base of a right prime (2, 3, 5, 7).
    right_prime_gen_aux(N, A).





% Task 8
/*
 * gentree(Preorder, Inorder, Tree), with mode gentree(+, +, -), will succeed if and only if the binary tree
 * Tree has preorder and inorder traversals that correspond to Preorder and Inorder.
 * The algorithem will iterate over Preorder, for each value in Preorder we will compare it to root of the current tree / subtree
   and recursivly call itself first on the left Subtree of Tree with all the values in Inorder that are left of value, then call itself
   on the Right subtree of Tree with all the values in Inorder that are right of value with what remains of Preorder after the left call.
 */ 


/*
 * We will use an auxilary predicate gentree_aux(Preorder, Inorder, Tree, NewPreorder), which will succeed if and only if
   the binary tree Tree has a preorder and inorder traversals that correspond to Inorder and a sublist X of Preorder
   such that appending NewPreorder to X will result in Preorder.
 * This will allow us to pass the values that remain in Preorder after the left recursive call.
 */
gentree_aux(X, [], nil, X). % no more value, Tree is nil, NewPreorder should be what is left in Preorder.
gentree_aux([Root | Preorder], Inorder, tree(L, Root, R) , NewPreorder):- % Testing Root against first value in Preorder
    append(InorderLeft , [Root | InorderRight], Inorder), % Splitting Inorder
    gentree_aux(Preorder, InorderLeft, L, RemainPreorder),
    gentree_aux(RemainPreorder, InorderRight, R, NewPreorder).  % RemainPreorder is the Preorder for the Right subtree.


% By using the auxilary predicate with an empty list as the NewPreorder, we will make sure Preorder and Inorder are of the same length.
gentree(Preorder, Inorder, Tree):-
    gentree_aux(Preorder, Inorder, Tree, []).





% Task 9.
/*
 * evaluate(Expression, Value), with mode evaluate(+, -), will succeed if and only if the arithmetic expression
   Expression given as a list with numbers and operators *, + evaluates to Value, Assuming Expressions represents a legal
   and not empty expression.
  
 * We will use 2 auxilary predicates to first calculate all the * operators in Expression, which will leave us with an expression
   containing only + operators
 * Then we will use a predicate to calculate the "plus" Expression.
 */


/*
 * evaluate_multiply(Expression, PlusExpression), with mode evaluate_multiply(+, -), will succeed if and only if
   after calculating all the * operators in Expression, Expression is identical to PlusExpression.
 */
evaluate_multiply([Val], [Val]). % Expressions holds 1 value and no operators.

evaluate_multiply([Operand | [+ | Expression]], [Operand | [+ | PlusExpression]]):- % + operator
    evaluate_multiply(Expression, PlusExpression). % No need to do more then checking the expressions are the same so far.

evaluate_multiply([Operand1 | [* | [Operand2 | Expression]]], PlusExpression):- % * operator, need to calculate.
    Val is Operand1 * Operand2, 
	evaluate_multiply([Val | Expression], PlusExpression). % Recursive call after calculating the multiplication into Val.


/*
 * evaluate_plus(Expression, Value), with mode evaluate_plus(+, -), will succeed if and only if the arithmetic expression
   Expression given as a list with numbers and operator + evaluates to Value, Assuming Expressions represents a legal
   and not empty expression.
 */
evaluate_plus([Value], Value). % Expressions holds 1 value, Value = value.
evaluate_plus([Operand1 | [+ | [Operand2 | Expression]]], Value):-
    Val is Operand1 + Operand2,
    evaluate_plus([Val | Expression], Value).  % recursive call after calculating the addition into Val.



evaluate(Expression, Value):-
    evaluate_multiply(Expression, PlusExpression), % evaluate the * operators, getting a new Expression with only + operator.
    evaluate_plus(PlusExpression, Value). % evaluate the new PlusExpression against Value.
        