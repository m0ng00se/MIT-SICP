Exercise 3.10
=============

[working... answer below is wrong]

In the ```make-withdraw``` procedure, the local variable ```balance``` is created as a parameter of ```make-withdraw```. 

We could also create the local state variable explicitly, using ```let```, as follows:

```scheme
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
```

Recall from section 1.3.2 that ```let``` is simply syntactic sugar for a procedure call:

```scheme
(let ((<var> <exp>)) <body>)
```

is interpreted as an alternate syntax for 

```scheme
((lambda (<var>) <body>) <exp>)
```

Use the environment model to analyze this alternate version of ```make-withdraw```, drawing figures like the ones above to illustrate the interactions

```scheme
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
```

Show that the two versions of ```make-withdraw``` create objects with the same behavior. How do the environment structures differ for the two versions?

Solution
-------- 

The expression 

```scheme
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
```

can be transformed into

```scheme
(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds"))) initial-amount))
```

The environment structure that results upon defining ```make-withdraw``` and evaluating the expression ```(define W1 (make-withdraw 100))``` in the global environment is as follows (click to enlarge):

[![](https://farm9.staticflickr.com/8596/16626009792_b755c69bf0_b.jpg)](https://farm9.staticflickr.com/8596/16626009792_b755c69bf0_b.jpg)

The ```make-withdraw``` procedure takes a single argument, ```initial-amount```. 

When ```(make-withdraw 100)``` is evaluated, a new frame ```E1``` is created in which the formal parameter ```inital-amount``` is bound to the value ```100```. 

```make-withdraw``` defines an internal lambda procedure that also takes a single argument, ```balance```. Evaluation of ```make-withdraw``` causes the formal parameter ```initial-amount``` to be applied to the formal parameter ```balance``` of this internal lambda procedure, so that a second frame ```E2``` is created in which the formal parameter ```balance``` is bound to the value supplied for ```initial-amount``` (which in this case is 100).

When ```(make-withdraw initial-amount)``` is evaluated, the value of ```initial-parameter``` is applied to the formal parameter ```balance``` in this internal lambda procedure, which causes a second frame ```E2``` to be created in which formal parameter ```balance``` is bound to the value of ```initial-amount``` (in this case, ```100```).

Evaluation of ```make-withdraw``` causes this formal parameter ```initial-amount``` to be applied to an internal lambda procedure that also takes a single argument, ```balance```, so that a second frame ```E2``` is created in which the formal parameter ```balance``` is bound to the value of ```initial-parameter```, or 100.


the ```initial-amount``` argument is applied to an internal lambda procedure that also takes a single argument, ```balance```, and which returns a second lambda procedure that likewise takes single argument, ```amount```. Evaluation of ```(make-withdraw initial-amount)``` thus creates a new environment frame, ```E1```, in which ```initial-amount``` is bound to the value that was supplied as an argument (in this case, ```100```), and to which the resulting lambda procedure (the one that takes ```amount``` as an argument) points.

The environment structure that results upon evaluating ```(W1 50)``` is as follows (click to enlarge):

[![](https://farm9.staticflickr.com/8588/16631884685_602db4befa_b.jpg)](https://farm9.staticflickr.com/8588/16631884685_602db4befa_b.jpg)

```W1``` points to a lambda procedure that takes one argument, ```amount```. Applying the argument 50 to ```W1``` creates a new environment frame, ```E2```, where the value 50 is bound to the variable ```amount```. This new frame ```E2``` points to the ```E1``` frame in which the variable ```initial-amount``` is bound, since ```E1``` is the frame pointed to by the lambda procedure ```W1```.

When the lambda procedure is evaluated, the binding for ```amount``` is found in the frame that has just been created, and the binding for ```initial-amount``` is found in the frame E1 that was created when ```make-withdraw``` was defined. The procecdure updates the value of ```initial-amount``` in E1, setting it to ```(- initial-amount amount)``` or 50.

When evaluation of the lambda procedure terminates, the resulting environment structure looks like:

[![](https://farm9.staticflickr.com/8566/16631990315_25ba0701a5_b.jpg)](https://farm9.staticflickr.com/8566/16631990315_25ba0701a5_b.jpg)

The value of ```initial-amount``` in E1 has been updated, and the frame in which ```amount``` was bound and in which the lambda procedure was evaluated has been discarded since the lambda procedure has terminated and there are no other pointers to that frame from elsewhere in the environment.

The environment structure that results upon evaluating ```(define W2 (make-withdraw 100))``` is as follows:

[working]