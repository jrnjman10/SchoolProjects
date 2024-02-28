#lang racket

;;; Project 0 Tic-tac-toe with Racket
;;; 
;;; Please immediately read README.md

(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;; 

; Check whether a list is a valid board
;integer?, sqrt
(define (board? lst)
  (if (integer? (sqrt (length lst))) ;checks intergers
      (if (equal? (+ (count (lambda (x) (equal? x 'X)) lst) (count (lambda (x) (equal? x 'O)) lst) (count (lambda (x) (equal? x 'E))lst)) (length lst)) ;Compares X + O + Es if = length of list
          (if (or (equal? (count (lambda (x) (equal? x 'X)) lst) (count(lambda (x) (equal? x 'O)) lst)) (equal? (- (count (lambda (x) (equal? x 'X)) lst) (count (lambda (x) (equal? x 'O)) lst)) 1)) ;checks is Xs and O are equal or if X-O <= 1
              #t
              #f)
          #f)
      #f))
;;fin

;;; From the board, calculate who is making a move this turn
(define (next-player board)
  (if (equal? (count (lambda (x) (equal? x 'O)) board) (count (lambda (x) (equal? x 'X)) board))
      ;it's x's turn
      'X
      ;it's O's turn
      'O))
  

;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)
  ;;list-ref to index

(define (valid-move? board row col player)
  (if (board? board) ;is it a board?
      (if(and  (integer? row) (integer? col)) ;are row and col integers
         (if (or (equal? 'X player) (equal? 'O player)) ;player X or O?
             (if (< (sub1 (* (add1 row) (add1 col))) (length board)) ;is it in the board?
                 (if (equal? 'E (list-ref board (sub1 (* (add1 row) (add1 col))))) ;is the spot empty?
                     (if (board? (append (append (take board (- (* (add1 row) (add1 col)) 1)) (list player)) (drop board  (* (add1 row) (add1 col)) )))
                         #t
                         #f)
                     #f)
                 #f)
             #f)
         #f)
      #f))

;;; To make a move, replace the position at row col to player ('X or 'O)
(define (take l n )
  (if (equal? n 0)
      '()
      (append (list (car l)) (take (cdr l) (- n 1)))))

(define (drop l n)
(if (<= n 0)
    l
    (drop (rest l) (- n 1))))

(define (make-move board row col player)
  (define size (length board))
  (define index (+ col (* row size)))
  (cons (append (take board index) player) (cdr (drop board index))))
        

;;; To determine whether there is a winner?
;; Go row by row
;; Column by column
;; then check diagals


(define (get-rows board)
  (define (h b)
    (if (empty? b)
        '()
        (cons (take b (sqrt (length board))) ( h ( drop b (sqrt (length board)))))))
  (h board))

(define (diagonal board)
  (define size (sqrt (length board)))
  (define (get-ith b l)
    (if (empty? l)
        '()
         (cons (list-ref b (+ (first l)(* (first l) size))) (get-ith b (rest l)))))
  (define (other b l)
    (if (empty? l)
        '()
        (cons (list-ref b (*(- size 1)(+ (first l) 1))) (other b (rest l)))))
  (cons (get-ith board (range size)) (list (other board (range size)))))

(define (column board)
  (define size (sqrt (length board)))
  (define (c b)
    (if (empty? b)
        '()
        (cons (first b) (c (drop b size)))))
  (c board))

(define (get-columns board)
  (define size (sqrt (length board)))
  (define (shuffle b n)
    (if (> n 0)
        (cons (column(append (rest b) (list (first b)))) (shuffle (append (rest b) (list (first b))) (- n 1)))
        '()))
        (shuffle board size))
        
(define (forall f l)
  (if (empty? l)
      f
      (and (equal? (first l) f) (forall f (rest l)))))

;; do need a compare function
(define (winner? board)
  ;(define size (sqrt (length board)))
  ;(define (rowL) (get-rows board))
  ;(define (columnL)(get-columns board))
  ;(define (diagonalL) (diagonal board))
  (define (checker l)
    (if (empty? l)
        #f
    (or (forall 'X (first l)) (forall 'O (first l)) (checker (rest l)))))
   (or (checker (get-rows board)) (checker (get-columns board)) (checker (diagonal board))))
    

    ;;start with the 2x2
    
;;; The board is the list containing E O X 
;;; Player will always be 'O
;;; returns a pair of x and y

(define (calculate-next-move board player)
  'todo)

