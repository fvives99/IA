#lang racket

;; Defines number of rows and columns for the board (6x5)

(define num-rows 5)
(define num-cols 6)

(define initial-piece-count 3)

;; Creates a nested list of zeros with num-rows rows and num-cols columns 
(define empty (build-list num-rows (lambda (unused) (build-list num-cols (lambda (unused) 0)))))

;; Hardcoded template from youtube video example.

(define auto-fill-template '(
  (0 2 1 2 1 0)
  (2 1 2 1 2 1)
  (1 1 2 1 2 2)
  (0 2 1 2 1 1)
  (0 0 2 2 1 0)
))

; ---------------------------------------------------------------------

;; Planned to develop project following a stack of values, consumes values and result gets returned
;; List of global variables used in the program, also opcodes for custom arithmetic functions to be used in stack:

(define print     0 )
(define readdata  1 )
(define push      2 )
(define pop       3 )
(define get       4 )
(define set       5 )
(define call      6 )
(define return    7 )
(define array-get 8 )
(define array-set 9 )
(define add       10)
(define sub       11)
(define mul       12)
(define lt        13) 
(define leq       14) 
(define eq        15)
(define neq       16)
(define geq       17) 
(define gt        18)
(define branch    19)
(define nop       20)

;;test
(define debug  10086)

; ---------------------------------------------------------------------
; Main Menu of the program, selection for auto or manual and fills board

;; 
(define program `(

  (,push ,empty)                       ;
  (,set board)                         ; board = empty
  (label retry-choice)
  (,push "cacacacacac")                 ;
  (,push "Do you want to auto fill? ") ;
  (,print)                             ; print("Do you want to auto fill")
  (,readdata)                          ;
  (,set choice)                        ; choice = readdata()
  (,get choice)                        ;
  (,push y)                            ;
  (,eq)                                ;
  (,branch (label auto-fill))          ; if (choice == "y") goto auto-fill
  (,get choice)                        ;
  (,push n)                            ;
  (,eq)                                ;
  (,branch (label manual-fill))        ; if (choice == "n") goto manual-fill
  (,push "Please reply y/n\n")         ;
  (,print)                             ; print("Please reply y/n\n")
  (,push #t)                           ;
  (,branch (label retry-choice))       ; goto retry-choice
  (label manual-fill)                  ;
  (,get board)                         ;
  (,call (label place-pieces))         ;
  (,set board)                         ; board = place-pieces(board)
  (,push #t)                           ;
  (,branch (label done-fill))          ; goto done-fill
  (label auto-fill)                    ;
  (,push ,auto-fill-template)          ;
  (,set board)                         ; board = auto-fill-template
  (label done-fill)                    ;  
  (,get board)                         ;
  (,call (label move-pieces))          ; move-pieces(board)
  (,return)                            ; return

  ; ---------------------------------------------------------------------
  ; Determines player turn and validates positions
  
  (label place-pieces)                                     ; function place_pieces(board):
  (,set board)                                             ;
  (,push 1)                                                ;
  (,set turn)                                              ; turn = 1
  (,push 0)                                                ;
  (,set count)                                             ; count = 0
  (label place-pieces-1)                                   ; place-pieces-1
  (,get count)                                             ;
  (,push ,(* initial-piece-count 2))                       ;
  (,eq)                                                    ;
  (,branch (label place-pieces-done))                      ; if count == initial-piece-count x 2 { goto place-pieces-done }
  (,get board)                                             ;
  (,call (label display-board))                            ; display_board(board)
  (,push "Player ")                                        ;
  (,print)                                                 ; print("Player ")
  (,get turn)                                              ;
  (,print)                                                 ; print(turn)
  (,push " is placing piece now\n")                        ;
  (,print)                                                 ; print(" is placing piece now\n")
  (,push "Row? ")                                          ;
  (,print)                                                 ; print("Row? ")
  (,readdata)                                              ;
  (,set row)                                               ; row = readdata()
  (,push "Col? ")                                          ;
  (,print)                                                 ; print("Col? ")
  (,readdata)                                              ;
  (,set col)                                               ; col = readdata()
  (,get row)                                               ; 
  (,push 0)                                                ; 
  (,lt)                                                    ; 
  (,branch (label place-pieces-row-too-small))             ; if (row < 0) { goto place-pieces-row-too-small }
  (,get row)                                               ; 
  (,push ,num-rows)                                        ; 
  (,geq)                                                   ; 
  (,branch (label place-pieces-row-too-big))               ; if (row >= num_rows) { goto place-pieces-row-too-big }
  (,get col)                                               ; 
  (,push 0)                                                ; 
  (,lt)                                                    ; 
  (,branch (label place-pieces-col-too-small))             ; if (col < 0) { goto place-pieces-col-too-small }
  (,get col)                                               ; 
  (,push ,num-cols)                                        ; 
  (,geq)                                                   ; 
  (,branch (label place-pieces-col-too-big))               ; if (col >= num_cols) { goto place-pieces-col-too-big }
  (,get board)                                             ;
  (,get row)                                               ;
  (,array-get)                                             ;
  (,get col)                                               ;
  (,array-get)                                             ;
  (,push 0)                                                ;
  (,neq)                                                   ;
  (,branch (label place-pieces-occupied))                  ; if (board[row][col] != 0) { goto place-pieces-occupied }
  (,get board)                                             ;
  (,get row)                                               ;
  (,get board)                                             ;
  (,get row)                                               ;
  (,array-get)                                             ;
  (,get col)                                               ;
  (,get turn)                                              ;
  (,array-set)                                             ;
  (,array-set)                                             ;
  (,set board-temp)                                        ; board_temp[row][col] = board with [row][col] changed to turn
  (,get turn)                                              ;
  (,get board-temp)                                        ;
  (,call (label check-three))                              ; 
  (,branch (label place-pieces-consecutive))               ; if (check_three(board_temp, turn)) { goto place-pieces-consecutive }
  (,get board-temp)                                        ;
  (,set board)                                             ; board = board_temp
  (,push 3)                                                ;
  (,get turn)                                              ;
  (,sub)                                                   ;
  (,set turn)                                              ; turn = 3 - turn 
  (,get count)                                             ;
  (,push 1)                                                ;
  (,add)                                                   ;
  (,set count)                                             ; count count + 1
  (,push #t)                                               ;
  (,branch (label place-pieces-1))                         ; goto place-pieces-1
  (label place-pieces-done)                                ;
  (,get board)                                             ;
  (,return)                                                ;
  (label place-pieces-row-too-small)                       ; place-pieces-row-too-small:
  (,push "Row is too small\n")                             ; 
  (,print)                                                 ; print("Row is too small\n")
  (,push #t)                                               ;
  (,branch (label place-pieces-1))                         ; goto label place-pieces-1
  (label place-pieces-row-too-big)                         ; place-pieces-row-too-big
  (,push "Row is too big\n")                               ; 
  (,print)                                                 ; print("Row is too big\n")
  (,push #t)                                               ;
  (,branch (label place-pieces-1))                         ; goto label place-pieces-1
  (label place-pieces-col-too-small)                       ; place-pieces-col-too-small:
  (,push "Col is too small\n")                             ; 
  (,print)                                                 ; print("Col is too small\n")
  (,push #t)                                               ;
  (,branch (label place-pieces-1))                         ; goto label place-pieces-1
  (label place-pieces-col-too-big)                         ; place-pieces-col-too-big
  (,push "Col is too big\n")                               ; 
  (,print)                                                 ; print("Col is too big\n")
  (,push #t)                                               ;
  (,branch (label place-pieces-1))                         ; goto label place-pieces-1
  (label place-pieces-occupied)                            ; place-pieces-occupied
  (,push "The position is already occupied\n")             ; 
  (,print)                                                 ; print("The position is already occupied\n")
  (,push #t)                                               ;
  (,branch (label place-pieces-1))                         ; goto label place-pieces-1
  (label place-pieces-consecutive)                         ; place-pieces-consecutive
  (,push "Invalid move, that will be 3 pieces in a row\n") ; 
  (,print)                                                 ; print("Invalid move, that will be 3 pieces in a row\n")
  (,push #t)                                               ;
  (,branch (label place-pieces-1))                         ; goto label place-pieces-1

  ; ---------------------------------------------------------------------
  ; This is where the validation for 3 pieces is taking place
  
  (label display-board)                   ; function display_board(board):
  (,set board)                            ;
  (,push 0)                               ; row = 0
  (,set row)                              ;
  (label display-board-1-start)           ; display-board-1-start:
  (,get row)                              ;
  (,push ,num-rows)                       ;
  (,eq)                                   ;
  (,branch (label display-board-1-done))  ; if (row == num-rows) goto display-board-1-done
  (,push 0)                               ; col = 0
  (,set col)                              ;
  (label display-board-2-start)           ; display-board-2-start:
  (,get col)                              ;
  (,push ,num-cols)                       ;
  (,eq)                                   ;
  (,branch (label display-board-2-done))  ; if (col == num-cols) goto display-board-2-done
  (,get board)                            ;
  (,get row)                              ;
  (,array-get)                            ;
  (,get col)                              ;
  (,array-get)                            ;
  (,print)                                ; print(board[row][col])
  (,push " ")                             ;
  (,print)                                ;
  (,get col)                              ;
  (,push 1)                               ;
  (,add)                                  ;
  (,set col)                              ; col = col + 1
  (,push #t)                              ;
  (,branch (label display-board-2-start)) ; goto display-board-2-start
  (label display-board-2-done)            ; display-board-2-done:
  (,get row)                              ;
  (,push 1)                               ;
  (,add)                                  ;
  (,set row)                              ; row = row + 1
  (,push #t)                              ;
  (,push "\n")                            ;
  (,print)                                ; print("\n")
  (,branch (label display-board-1-start)) ; goto display-board-1-start
  (label display-board-1-done)            ; display-board-1-done:
  (,return)                               ; return
  (label check-three)                       ; function check-three(board, turn):
  (,set board)                              ;
  (,set turn)                               ;
  (,push 0)                                 ;
  (,set row)                                ; row = 0
  (label check-three-1-start)               ;
  (,get row)                                ;
  (,push ,num-rows)                         ;
  (,eq)                                     ;
  (,branch (label check-three-1-done))      ; if row == num-rows { goto check-three-1-done }
  (,push 0)                                 ;
  (,set col)                                ; col = 0
  (label check-three-2-start)               ;
  (,get col)                                ;
  (,push ,(- num-cols 2))                   ;
  (,eq)                                     ;
  (,branch (label check-three-2-done))      ; if col == num-cols - 2 { goto check-three-2-done }
  (,push 0)                                 ;
  (,set count)                              ; count = 0
  (,push 0)                                 ;
  (,set col-offset)                         ; col-offset = 0
  (label check-three-3-start)               ;
  (,get col-offset)                         ; 
  (,push 3)                                 ;
  (,eq)                                     ;
  (,branch (label check-three-3-done))      ; if col-offset == 3 { goto check-three-3-done }
  (,get board)                              ;
  (,get row)                                ;
  (,array-get)                              ;
  (,get col)                                ;
  (,get col-offset)                         ;
  (,add)                                    ;
  (,array-get)                              ;
  (,get turn)                               ;
  (,neq)                                    ; 
  (,branch (label check-three-skip-add-1))  ; if board[row][col + col-offset] != turn { skip increment count }
  (,get count)                              ;
  (,push 1)                                 ;
  (,add)                                    ;
  (,set count)                              ; count = count + 1
  (label check-three-skip-add-1)            ;
  (,get col-offset)                         ;
  (,push 1)                                 ;
  (,add)                                    ;
  (,set col-offset)                         ; col-offset = col-offset + 1
  (,push #t)                                ;
  (,branch (label check-three-3-start))     ; goto check-three-3-start
  (label check-three-3-done)                ;
  (,get count)                              ;
  (,push 3)                                 ;
  (,eq)                                     ;
  (,branch (label check-three-return-true)) ; if count == 3 { goto check-three-return-true }
  (,get col)                                ;
  (,push 1)                                 ;
  (,add)                                    ;
  (,set col)                                ; col = col + 1
  (,push #t)                                ;
  (,branch (label check-three-2-start))     ; goto check-three-2-start
  (label check-three-2-done)                ;
  (,get row)                                ;
  (,push 1)                                 ;
  (,add)                                    ;
  (,set row)                                ; row = row + 1
  (,push #t)                                ;
  (,branch (label check-three-1-start))     ; goto check-three-1-start
  (label check-three-1-done)                ;
  (,push 0)                                 ;
  (,set col)                                ; col = 0
  (label check-three-4-start)               ;
  (,get col)                                ;
  (,push ,num-cols)                         ;
  (,eq)                                     ;
  (,branch (label check-three-4-done))      ; if col == num-cols { goto check-three-4-done }
  (,push 0)                                 ;
  (,set row)                                ; row = 0
  (label check-three-5-start)               ;
  (,get row)                                ;
  (,push ,(- num-rows 2))                   ;
  (,eq)                                     ;
  (,branch (label check-three-5-done))      ; if row == num-rows - 2 { goto check-three-5-done }
  (,push 0)                                 ;
  (,set count)                              ; count = 0
  (,push 0)                                 ;
  (,set row-offset)                         ; row-offset = 0
  (label check-three-6-start)               ;
  (,get row-offset)                         ; 
  (,push 3)                                 ;
  (,eq)                                     ;
  (,branch (label check-three-6-done))      ; if row-offset == 3 { goto check-three-6-done }
  (,get board)                              ;
  (,get row)                                ;
  (,get row-offset)                         ;
  (,add)                                    ;
  (,array-get)                              ;
  (,get col)                                ;
  (,array-get)                              ;
  (,get turn)                               ;
  (,neq)                                    ; 
  (,branch (label check-three-skip-add-2))  ; if board[row + row-offset][col] != turn { skip increment count }
  (,get count)                              ;
  (,push 1)                                 ;
  (,add)                                    ;
  (,set count)                              ; count = count + 1
  (label check-three-skip-add-2)            ;
  (,get row-offset)                         ;
  (,push 1)                                 ;
  (,add)                                    ;
  (,set row-offset)                         ; row-offset = row-offset + 1
  (,push #t)                                ;
  (,branch (label check-three-6-start))     ; goto check-three-6-start
  (label check-three-6-done)                ;
  (,get count)                              ;
  (,push 3)                                 ;
  (,eq)                                     ;
  (,branch (label check-three-return-true)) ; if count == 3 { goto check-three-return-true }
  (,get row)                                ;
  (,push 1)                                 ;
  (,add)                                    ;
  (,set row)                                ; row = row + 1
  (,push #t)                                ;
  (,branch (label check-three-5-start))     ; goto check-three-5-start
  (label check-three-5-done)                ;
  (,get col)                                ;
  (,push 1)                                 ;
  (,add)                                    ;
  (,set col)                                ; col = col + 1
  (,push #t)                                ;
  (,branch (label check-three-4-start))     ; goto check-three-4-start
  (label check-three-4-done)                ;
  (,push #f)                                ;
  (,return)                                 ; return false
  (label check-three-return-true)           ;
  (,push #t)                                ;
  (,return)                                 ; return true

  ; ---------------------------------------------------------------------
  ; Player turns
  
  (label move-pieces)                                    ; function move-pieces(board):
  (,set board)                                           ;
  (,push 1)                                              ;
  (,set turn)                                            ; turn = 1
  (,push ,initial-piece-count)                           ;
  (,set piece1)                                          ; piece1 = initial-piece-count
  (,push ,initial-piece-count)                           ;
  (,set piece2)                                          ; piece2 = initial-piece-count
  (label move-pieces-1)                                  ;
  (,get board)                                           ;
  (,call (label display-board))                          ; display_board(board)
  (,get piece1)                                          ;
  (,push 2)                                              ;
  (,eq)                                                  ;
  (,branch (label move-piece-game-over-1))               ; if (piece1 == 2) { goto move-piece-game-over-1 }
  (,get piece2)                                          ;
  (,push 2)                                              ;
  (,eq)                                                  ;
  (,branch (label move-piece-game-over-2))               ; if (piece2 == 2) { goto move-piece-game-over-2 }
  (,push "Player ")                                      ;
  (,print)                                               ; print("Player ")
  (,get turn)                                            ;
  (,print)                                               ; print(turn)
  (,push " is moving piece now\n")                       ;
  (,print)                                               ; print(" is placing piece now\n")
  (,push "Src Row? ")                                    ;
  (,print)                                               ; print("Src Row? ")
  (,readdata)                                            ;
  (,set src-row)                                         ; src-row = readdata()
  (,push "Src Col? ")                                    ;
  (,print)                                               ; print("Src Col? ")
  (,readdata)                                            ;
  (,set src-col)                                         ; src-col = readdata()
  (,get src-row)                                         ;
  (,push 0)                                              ;
  (,lt)                                                  ;
  (,branch (label move-pieces-row-too-small))            ; if (src-row < 0) { goto move-pieces-row-too-small }
  (,get src-row)                                         ;
  (,push ,num-rows)                                      ;
  (,geq)                                                 ;
  (,branch (label move-pieces-row-too-big))              ; if (src-row >= num_rows) { goto move-pieces-row-too-big }
  (,get src-col)                                         ;
  (,push 0)                                              ;
  (,lt)                                                  ;
  (,branch (label move-pieces-col-too-small))            ; if (src-col < 0) { goto move-pieces-col-too-small }
  (,get src-col)                                         ;
  (,push ,num-cols)                                      ;
  (,geq)                                                 ;
  (,branch (label move-pieces-col-too-big))              ; if (src-col >= num_cols) { goto move-pieces-col-too-big }
  (,get board)                                           ;
  (,get src-row)                                         ;
  (,array-get)                                           ;
  (,get src-col)                                         ;
  (,array-get)                                           ;
  (,get turn)                                            ;
  (,neq)                                                 ;
  (,branch (label move-pieces-invalid))                  ; if (board[row][col] != turn) { goto move-pieces-invalid }
  (,push "Dst Row? ")                                    ;
  (,print)                                               ; print("Dst Row? ")
  (,readdata)                                            ;
  (,set dst-row)                                         ; dst-row = readdata()
  (,push "Dst Col? ")                                    ;
  (,print)                                               ; print("Dst Col? ")
  (,readdata)                                            ;
  (,set dst-col)                                         ; dst-col = readdata()
  (,get dst-row)                                         ;
  (,push 0)                                              ;
  (,lt)                                                  ;
  (,branch (label move-pieces-row-too-small))            ; if (dst-row < 0) { goto move-pieces-row-too-small }
  (,get dst-row)                                         ;
  (,push ,num-rows)                                      ;
  (,geq)                                                 ;
  (,branch (label move-pieces-row-too-big))              ; if (dst-row >= num_rows) { goto move-pieces-row-too-big }
  (,get dst-col)                                         ;
  (,push 0)                                              ;
  (,lt)                                                  ;
  (,branch (label move-pieces-col-too-small))            ; if (dst-col < 0) { goto move-pieces-col-too-small }
  (,get dst-col)                                         ;
  (,push ,num-cols)                                      ;
  (,geq)                                                 ;
  (,branch (label move-pieces-col-too-big))              ; if (dst-col >= num_cols) { goto move-pieces-col-too-big }
  (,get board)                                           ;
  (,get dst-row)                                         ;
  (,array-get)                                           ;
  (,get dst-col)                                         ;
  (,array-get)                                           ;
  (,push 0)                                              ;
  (,neq)                                                 ;
  (,branch (label move-pieces-occupied))                 ; if (board[row][col] != 0) { goto move-pieces-occupied }
  (,get dst-col)                                         ;
  (,get dst-row)                                         ;
  (,get src-col)                                         ;
  (,get src-row)                                         ;
  (,call (label is-dist-invalid))                        ;
  (,branch (label move-too-far))                         ; if (is-dist-invalid (...)) { goto move-too-far }
  (,get board)                                           ;
  (,get src-row)                                         ;
  (,get board)                                           ;
  (,get src-row)                                         ;
  (,array-get)                                           ;
  (,get src-col)                                         ;
  (,push 0)                                              ;
  (,array-set)                                           ;
  (,array-set)                                           ;
  (,set board)                                           ; board[src-row][src-col] = 0
  (,get board)                                           ;
  (,get dst-row)                                         ;
  (,get board)                                           ;
  (,get dst-row)                                         ;
  (,array-get)                                           ;
  (,get dst-col)                                         ;
  (,get turn)                                            ;
  (,array-set)                                           ;
  (,array-set)                                           ;
  (,set board)                                           ; board[dst-row][dst-col] = turn
  (,get turn)                                            ;
  (,get board)                                           ;
  (,call (label check-three))                            ;
  (,branch (label move-pieces-consecutive))              ; if (check_three(board_temp, turn)) { goto move-pieces-consecutive }
  (,push 3)                                              ;
  (,get turn)                                            ;
  (,sub)                                                 ;
  (,set turn)                                            ; turn = 3 - turn
  (,push #t)                                             ;
  (,branch (label move-pieces-1))                        ; goto move-pieces-1
  (label move-pieces-consecutive)                        ;
  (,get board)                                           ;
  (,call (label display-board))                          ;  display-board(board)
  (,push 3)                                              ;
  (,get turn)                                            ;
  (,sub)                                                 ;
  (,set turn)                                            ; turn = 3 - turn
  (label move-pieces-retry-capture)                      ;
  (,push "select a piece of your opponent to capture\n") ;
  (,print)                                               ; print("select a piece of your opponent to capture\n")
  (,push "Cap Row? ")                                    ;
  (,print)                                               ; print("Cap Row? ")
  (,readdata)                                            ;
  (,set cap-row)                                         ; cap-row = readdata()
  (,push "Cap Col? ")                                    ;
  (,print)                                               ; print("Cap Col? ")
  (,readdata)                                            ;
  (,set cap-col)                                         ; cap-col = readdata()
  (,get cap-row)                                         ;
  (,push 0)                                              ;
  (,lt)                                                  ;
  (,branch (label move-pieces-row-too-small-cap))        ; if (cap-row < 0) { goto move-pieces-row-too-small-cap }
  (,get cap-row)                                         ;
  (,push ,num-rows)                                      ;
  (,geq)                                                 ;
  (,branch (label move-pieces-row-too-big-cap))          ; if (cap-row >= num_rows) { goto move-pieces-row-too-big-cap }
  (,get cap-col)                                         ;
  (,push 0)                                              ;
  (,lt)                                                  ;
  (,branch (label move-pieces-col-too-small-cap))        ; if (cap-col < 0) { goto move-pieces-col-too-small-cap }
  (,get cap-col)                                         ;
  (,push ,num-cols)                                      ;
  (,geq)                                                 ;
  (,branch (label move-pieces-col-too-big-cap))          ; if (cap-col >= num_cols) { goto move-pieces-col-too-big-cap }
  (,get board)                                           ;
  (,get cap-row)                                         ;
  (,array-get)                                           ;
  (,get cap-col)                                         ;
  (,array-get)                                           ;
  (,get turn)                                            ;
  (,neq)                                                 ;
  (,branch (label move-pieces-invalid-cap))              ; if (board[row][col] != turn) { goto move-pieces-invalid-cap }
  (,get board)                                           ;
  (,get cap-row)                                         ;
  (,get board)                                           ;
  (,get cap-row)                                         ;
  (,array-get)                                           ;
  (,get cap-col)                                         ;
  (,push 0)                                              ;
  (,array-set)                                           ;
  (,array-set)                                           ;
  (,set board)                                           ; board[cap-row][cap-col] = 0
  (,get turn)                                            ;
  (,push 1)                                              ;
  (,eq)                                                  ;
  (,branch (label move-piece-dec-1))                     ; if turn == 1 { goto move-piece-dec-1 }
  (,get piece2)                                          ;
  (,push 1)                                              ;
  (,sub)                                                 ;
  (,set piece2)                                          ; piece2 = piece2 - 1
  (,push #t)                                             ;
  (,branch (label move-piece-dec-done))                  ; goto move-piece-dec-done
  (label move-piece-dec-1)                               ;
  (,get piece1)                                          ;
  (,push 1)                                              ;
  (,sub)                                                 ;
  (,set piece1)                                          ; piece1 = piece1 - 1
  (label move-piece-dec-done)                            ;
  (,push #t)                                             ;
  (,branch (label move-pieces-1))                        ; goto move-pieces-1
  (label move-piece-game-over-1)                         ;
  (,push "Game over, player 2 wins")                     ; 
  (,print)                                               ; print("Game over, player 2 wins")
  (,return)                                              ; return
  (label move-piece-game-over-2)                         ;
  (,push "Game over, player 1 wins")                     ;
  (,print)                                               ; print("Game over, player 1 wins")
  (,return)                                              ; return
  (label move-pieces-row-too-small)                      ;
  (,push "Row is too small\n")                           ;
  (,print)                                               ; print("Row is too small\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-1))                        ; goto label move-pieces-1
  (label move-pieces-row-too-big)                        ;
  (,push "Row is too big\n")                             ;
  (,print)                                               ; print("Row is too big\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-1))                        ; goto label move-pieces-1
  (label move-pieces-col-too-small)                      ;
  (,push "Col is too small\n")                           ;
  (,print)                                               ; print("Col is too small\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-1))                        ; goto label move-pieces-1
  (label move-pieces-col-too-big)                        ;
  (,push "Col is too big\n")                             ;
  (,print)                                               ; print("Col is too big\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-1))                        ; goto label move-pieces-1
  (label move-pieces-invalid)                            ;
  (,push "The position is not your pieces\n")            ;
  (,print)                                               ; print("The position is not your piece\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-1))                        ; goto label move-pieces-1
  (label move-pieces-occupied)                           ;
  (,push "The position is occupied\n")                   ;
  (,print)                                               ; print("The position is occupied\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-1))                        ; goto label move-pieces-1
  (label move-too-far)                                   ;
  (,push "The move is too far\n")                        ;
  (,print)                                               ; print("The position is too far\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-1))                        ; goto label move-pieces-1
  (label move-pieces-row-too-small-cap)                  ;
  (,push "Row is too small\n")                           ;
  (,print)                                               ; print("Row is too small\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-retry-capture))            ; goto label move-pieces-retry-capture
  (label move-pieces-row-too-big-cap)                    ;
  (,push "Row is too big\n")                             ;
  (,print)                                               ; print("Row is too big\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-retry-capture))            ; goto label move-pieces-retry-capture
  (label move-pieces-col-too-small-cap)                  ;
  (,push "Col is too small\n")                           ;
  (,print)                                               ; print("Col is too small\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-retry-capture))            ; goto label move-pieces-retry-capture
  (label move-pieces-col-too-big-cap)                    ;
  (,push "Col is too big\n")                             ;
  (,print)                                               ; print("Col is too big\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-retry-capture))            ; goto label move-pieces-retry-capture
  (label move-pieces-invalid-cap)                        ;
  (,push "The position is not your opponent's pieces\n") ;
  (,print)                                               ; print("The position is not your opponent's pieces\n")
  (,push #t)                                             ;
  (,branch (label move-pieces-retry-capture))            ; goto label move-pieces-retry-capture

  ; ---------------------------------------------------------------------

  (label is-dist-invalid)                   ; function is-dist-invalid(src-row, src-col, dst-row, dst-col):
  (,set src-row)                            ; 
  (,set src-col)                            ; 
  (,set dst-row)                            ; 
  (,set dst-col)                            ; 
  (,get src-row)                            ; 
  (,get dst-row)                            ; 
  (,sub)                                    ;
  (,set row-diff)                           ; row-diff = src-row - dst-row
  (,get row-diff)                           ; 
  (,push 0)                                 ;
  (,geq)                                    ; 
  (,branch (label diff-skip-flip-row-diff)) ; if row-diff >= 0 { skip flip row-diff }
  (,push 0)                                 ;
  (,get row-diff)                           ; 
  (,sub)                                    ;
  (,set row-diff)                           ; row-diff = 0 - row-diff
  (label diff-skip-flip-row-diff)           ;
  (,get src-col)                            ; 
  (,get dst-col)                            ; 
  (,sub)                                    ;
  (,set col-diff)                           ; col-diff = src-col - dst-col
  (,get col-diff)                           ; 
  (,push 0)                                 ;
  (,geq)                                    ; 
  (,branch (label diff-skip-flip-col-diff)) ; if col-diff >= 0 { skip flip col-diff }
  (,push 0)                                 ;
  (,get col-diff)                           ; 
  (,sub)                                    ; 
  (,set col-diff)                           ; col-diff = 0 - col-diff
  (label diff-skip-flip-col-diff)           ;
  (,get row-diff)                           ;
  (,get col-diff)                           ;
  (,add)                                    ;
  (,push 1)                                 ;
  (,neq)                                    ;
  (,return)                                 ;

))


; ---------------------------------------------------------------------


;; Program = list of instructions
;; Each instruction is a list that starts with the op-code.
;; Number = address, list index into the program to obtain the next instruction to be interpreted
;; Stack, top of the stack is the frame, right after it will be the return address
;; Stack meant for the values produced and consumed by the op-codes

(struct machine-struct (
  program
  address
  stack
  values
) #:transparent)


;; Assembles the progra, replace labels with that actual address
;; Functional Programming, no cycles

(define main (lambda ()
  (let* ; env var
    (
      (assembled (assemble program))
      (machine (machine-struct assembled 0 '(() -1) '()))
    )
    (run machine)
  )
))

;; Associate label with their addresses
;; Replace label instructions into nop instructions
;; Replace labels in operands into their addresses

(define assemble (lambda (program) ; takes single argument: program
  (define is-label (lambda (entry) (eq? 'label (car entry)))) ; helper funct. checks if car entry is label
  (define replace-labels (lambda (input labels) ; recursively traverses the input and replaces each occurrence of a label with its corresponding memory address.
    (cond
      ((not (list? input)) input)
      ((null? input)       '())
      ((is-label input)    (cdr (assoc input labels)))
      (else                (map (lambda (x) (replace-labels x labels)) input))
    )
  ))
  (let* ; local env var
    (
      (program-length           (length program)) ;program length
      (line-numbers             (build-list program-length identity)) ; int 0 -> prg len
      (program-with-line-number (map cons program line-numbers))
      (labels                   (filter (lambda (entry) (eq? 'label (caar entry))) program-with-line-number))
      (noped                    (map (lambda (i) (if (is-label i) (list nop) i)) program))
    )
    (replace-labels noped labels)
  )
))

;; function tp update an association list with a new key-value pair
;; If the given key is already present in the list, its associated value is updated to the new value. 
;; If the key is not present in the list, a new (key . value) pair is added to the front of the list.

(define run (lambda (machine)
  (define set-helper (lambda (assoc key value)
    (cond
      ((null? assoc) (list (cons key value)))
      ((eq? (caar assoc) key) (cons (cons key value) (cdr assoc)))
      (else (cons (car assoc) (set-helper (cdr assoc) key value)))
    )
  ))
  (let* ; env var
    (
      (program        (machine-struct-program machine))
      (address        (machine-struct-address machine))
      (stack          (machine-struct-stack machine))
      (values         (machine-struct-values machine))
      (frame          (car stack))
      (stack-tail     (cdr stack))
      (return-address (car stack-tail))
      (instruction    (list-ref program address))
      (op-code        (car instruction))
    )
    ;(display "Running #")
    ;(display address)
    ;(newline)
    (cond
      ((= op-code print)
        (display (car values))
        (run (machine-struct program (+ address 1) stack (cdr values)))
      )
      ((= op-code readdata)
        (let
          (
            (data (read))
          )
          ; (display data)
          ; (display "\n")
          (run (machine-struct program (+ address 1) stack (cons data values)))
        )
      )
      ((= op-code push)
        (run (machine-struct program (+ address 1) stack (cons (cadr instruction) values)))
      )
      ((= op-code pop)
        (run (machine-struct program (+ address 1) stack (cdr values)))
      )
      ((= op-code get)
        (run (machine-struct program (+ address 1) stack (cons (cdr (assoc (cadr instruction) frame)) values)))
      )
      ((= op-code set)
        (run (machine-struct program (+ address 1) (cons (set-helper frame (cadr instruction) (car values)) stack-tail) (cdr values)))
      )
      ((= op-code call)
        (run (machine-struct program (cadr instruction) (cons '() (cons (+ address 1) stack)) values))
      )
      ((= op-code return)
        (if (= return-address -1)
          (display "")
          (run (machine-struct program return-address (cdr stack-tail) values))
        )
      )
      ((= op-code array-get)
        (let
          (
            (array (cadr values))
            (index (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (list-ref array index)(cddr values))))
        )
      )
      ((= op-code array-set)
        (let
          (
            (array (caddr values))
            (index (cadr values))
            (value (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (append (take array index) (cons value (drop array (+ index 1)))) (cdddr values))))
        )
      )
      ((= op-code add)
        (let
          (
            (a (cadr values))
            (b (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (+ a b)(cddr values))))
        )
      )
      ((= op-code sub)
        (let
          (
            (a (cadr values))
            (b (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (- a b)(cddr values))))
        )
      )
      ((= op-code mul)
        (let
          (
            (a (cadr values))
            (b (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (* a b)(cddr values))))
        )
      )
      ((= op-code lt)
        (let
          (
            (a (cadr values))
            (b (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (< a b)(cddr values))))
        )
      )
      ((= op-code leq)
        (let
          (
            (a (cadr values))
            (b (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (<= a b)(cddr values))))
        )
      )
      ((= op-code eq)
        (let
          (
            (a (cadr values))
            (b (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (eq? a b)(cddr values))))
        )
      )
      ((= op-code neq)
        (let
          (
            (a (cadr values))
            (b (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (not (= a b)) (cddr values))))
        )
      )
      ((= op-code geq)
        (let
          (
            (a (cadr values))
            (b (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (>= a b)(cddr values))))
        )
      )
      ((= op-code gt)
        (let
          (
            (a (cadr values))
            (b (car values))
          )
          (run (machine-struct program (+ address 1) stack (cons (> a b)(cddr values))))
        )
      )
      ((= op-code branch)
        (if
          (car values)
          (run (machine-struct program (cadr instruction) stack (cdr values)))
          (run (machine-struct program (+ address 1) stack (cdr values)))
        )
      )
      ((= op-code nop) ; no operation
        (run (machine-struct program (+ address 1) stack values))
      )
      ((= op-code debug)
        (display values)
        (run (machine-struct program (+ address 1) stack values))
      )
      (#t (error "Opcode not implemented"))
    )
  )
))

(main)