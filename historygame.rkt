;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname historygame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Literacy Test
(require 2htdp/image)
(require 2htdp/universe)

(define PARCHMENT (bitmap "parchment.png"))
(define SEAL (bitmap "seal.png"))

; Premise:
(define PREMISE "Premise:\n
\t1.) You are about to take a modernized version of the Louisiana literacy test.
\t2.) This test is to be given to anyone who cannot prove a fifth grade education.
\t3.) Be careful as one wrong answer denotes failure of this test.
\t3.) Failure of this test will result in your ineligability to vote.
\t4.) This test may be administered to anyone applying to vote, at the state's disgression.
\t5.) Do not include units for any questions.
\t6.) You have 10 minutes to answer 20 questions, time starts when you flip to the next page.")

(define CONTROLS "Controls:
- use left and right arrow keys to navigate test.
- use enter key to lock in your answer (answers not locked in will not be counted.)
- use the keyboard to type your answer.
- the mouse is not needed.")
(define INTERP "Brief Historical Interpretation:\n
\t- Before the passage of the Voting Rights Act of 1965, many southern states imposed
 poll taxes, residency and property restrictions, and literacy tests to deny
 sufferage to African Americans.\n
\t- To answer the question \"What does it mean to be an American?\", we learned in class
that it is important to understand how different people experienced history.\n
\t- This literacy test aims to teach the user empathy; the goal is for the user to
understand what being disenfranchised feels like.\n
\t - The goal of this literacy test is to get the user to understand how the meaning of
being an American changes during a time when blatant racial prejudice was intertwined
with United States Law.")

(define-struct entry [key value])
; A [MappingEntry X Y] is a (make-entry X Y)
; - where x is the key
; - where y is the valye

; A [Mapping X Y] is a [List-of [MappingEntry X Y]]

(define mapping-1 (list (make-entry 1 10)
                        (make-entry 2 10)
                        (make-entry 3 10)
                        (make-entry 4 10)
                        (make-entry 5 10)
                        (make-entry 6 10)))

; create-default-map : [List-of X] Y -> [Mapping X Y]
; Creates a mapping of x to y where y is the default value.
(define (create-default-map lox y)
  (map (λ (x) (make-entry x y)) lox))

(check-expect (create-default-map '(1 2 3 4 5 6) 10)
              mapping-1)

; map-ref : [Mapping X Y] Number -> [MappingEntry X Y]
; Gets the nth index of the mapping.
(define (map-ref mapping n)
  (list-ref mapping n))

(check-expect (map-ref mapping-1 0) (make-entry 1 10))

; contains-key? : X [Mapping X Y] [X X -> Boolean] -> Boolean
; Determines if a mapping already has x as a key.
(define (contains-key? x mapping eq?)
  (ormap (λ (entry) (eq? (entry-key entry) x)) mapping))

(check-expect (contains-key? 4 mapping-1 =) #true)
(check-expect (contains-key? 7 mapping-1 =) #false)

; put : X Y [Mapping X Y] [X X -> Boolean] -> [Mapping X Y]
; creates an entry of x to y or replaces an existing y.
(define (put x y mapping eq?)
  (if (contains-key? x mapping eq?)
      (map (λ (entry) (if (eq? (entry-key entry) x)
                          (make-entry x y)
                          entry))
           mapping)
      (cons (make-entry x y) mapping)))

; get : X [Mapping X] Y [X X -> Boolean] -> Y
; Returns the value from the key-value pair or an error if it doesn't exist.
(define (get x mapping base eq?)
  (foldr (λ (entry ans)
           (if (eq? (entry-key entry) x)
               (entry-value entry)
               ans))
         base
         mapping))

(check-expect (get 4 mapping-1 #false =) 10)
(check-expect (get 10 mapping-1 #false =) #false)
         
           

(check-expect (put 10 20 mapping-1 =)
              (cons (make-entry 10 20) mapping-1))
(check-expect (put 4 8 mapping-1 =)
              (list (make-entry 1 10)
                    (make-entry 2 10)
                    (make-entry 3 10)
                    (make-entry 4 8)
                    (make-entry 5 10)
                    (make-entry 6 10)))

; An Answer is one of
; - String
; - [List-of Answer]
                 
; answer-template : Answer -> ???
; Template for Answer

; A State is one of
; - "accepting-input"
; - "not-accepting-input"
; - "ending"

(define ACCEPTING-INPUT "accepting-input")
(define NOT-ACCEPTING-INPUT "not-accepting-input")
(define ENDING "ending")

(define-struct question [q answer correct])
; A Question is a (make-question String Answer [String Answer -> Boolean])
; - question : Represents the text of the question
; - answer : Represents all of the possible answers
; - correct : Is true if the user's answer is correct and false if incorrect.

(define Q1 (make-question "How many centimeters are in an inch? (2 decimal places)"
                          "2.54"
                          (λ (input answer)
                            (string=? input answer))))
(define Q2 (make-question "Who, of the following, was not a U.S. President? (Write the letter)\n
\tA. Alexander Hamilton\n\tB. George Washington\n\tC. John Adams\n\tD. Theodore Roosevelt"
                          "a"
                          (λ (input answer) (string=? (string-downcase input) answer))))
(define Q3 (make-question "How much dirt is in a hole that is 3 ft x 3 ft x 3 ft?"
                          "0"
                          (λ (input answer) (string=? input answer))))
(define Q4 (make-question "Spell forwards backward."
                          (list "forwards backward" "sdrawrof")
                          (λ (input answer)
                            (if (= (random 2) 0)
                                (string=? (first answer) (string-downcase input))
                                (string=? (second answer) (string-downcase input))))))
(define Q5 (make-question "Which letter is least like the others?\n
a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z"
                          "w"
                          (λ (input answer) (string=? input answer))))
(define Q6 (make-question "Assume that a is false and b is false, what is: (a or (not (a and b)))?"
                          (list "t" "true")
                          (λ (input answer) (member? (string-downcase input) answer))))
(define Q7 (make-question "What is the 10th value of the fibonacci sequence?"
                          "55"
                          (λ (input answer) (string=? input answer))))
(define Q8 (make-question "What year was the 16th amendment added to the US constitution?"
                          "1789"
                          (λ (input answer) (string=? input answer))))
(define Q9 (make-question "What President was in office during 1887? (first and last name)"
                          "grover cleveland"
                          (λ (input answer) (string=? (string-downcase input) answer))))
(define Q10 (make-question "Question ten. This is it, the last question, the tenth.
What was the first ordinal in the previous two sentences?"
                           "last"
                           (λ (input answer) (string=? (string-downcase input) answer))))                
(define Q11 (make-question "What do cows drink?"
                           "water"
                           (λ (input answer) (string=? (string-downcase input) answer))))
(define Q12 (make-question "Write the word \"noise\" backwards and make what would be the
second letter, should it had been written forward, capital."
                           "esiOn"
                           (λ (input answer) (string=? input answer))))
(define Q13 (make-question "True or false: The number of this question is prime."
                           (list "true" "t")
                           (λ (input answer) (member? (string-downcase input) answer))))
(define Q14 (make-question "What words are required by law to be on all coins and
paper currency of the U.S.?"
                           "in god we trust"
                           (λ (input answer) (string=? (string-downcase input) answer))))
(define Q15 (make-question "What is the shortest word in this line that doesn't start with \"i\"?"
                           "the"
                           (λ (input answer) (string=? input answer))))
(define Q16 (make-question "Who was the 30th President of the United States? (first and last name)"
                           "calvin coolidge"
                           (λ (input answer) (string=? (string-downcase input) answer))))
(define Q17 (make-question "How many seats are there currently in the House of Representatives?"
                           "435"
                           (λ (input answer) (string=? input answer))))
(define Q18 (make-question "What's nowhere but everywhere, except where something is?"
                           "nothing"
                           (λ (input answer) (string=? (string-downcase input) answer))))
(define Q19 (make-question "Write every third word in this line and capitalize every second
word in your answer, except for the 2nd second word."
                           "third THIS capitalize word answer THE word"
                           (λ (input answer) (string=? input answer))))
(define Q20 (make-question "What is the smallest number divisible by 1-10?"
                           "2520"
                           (λ (input answer) (string=? input answer))))


(define QUESTIONS (list Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10 Q11 Q12 Q13 Q14 Q15 Q16 Q17 Q18 Q19 Q20))

; question=? : Question Question -> Boolean
; Determines if two questions are equal.
(define (question=? q1 q2)
  (and (string=? (question-q q1) (question-q q2))
       (equal? (question-answer q1) (question-answer q2))))

(check-expect (question=? Q1 Q1) #true)
(check-expect (question=? Q1 Q2) #false)

; grade-test : [Mapping Question String] -> Boolean
; Determines if the user passed the test.
(define (grade-test mapping)
  (andmap (λ (entry)
            ((question-correct
              (entry-key entry)) (entry-value entry)
                                 (question-answer (entry-key entry))))
          mapping))
             


(define-struct world [current questions input ticks state stop])
; A World is a (make-world Number [Mapping Question String Boolean] String State)
; - current - the current question the user is looking at.
; - questions - the mapping of questions to answers
; - input - the current user input.
; - ticks - the amount of seconds that have gone by.
; - state - the current state of the world
; - stop - if the game is supposed to stop.

; set-world-current : World Number -> World
; Creates a new World with a new current value.
(define (set-world-current world current)
  (make-world current
              (world-questions world)
              (world-input world)
              (world-ticks world)
              (world-state world)
              (world-stop world)))

; set-world-questions : World [Mapping Question String] -> World
; Creates a new world with a new questions value
(define (set-world-questions world questions)
  (make-world (world-current world)
              questions
              (world-input world)
              (world-ticks world)
              (world-state world)
              (world-stop world)))

; set-world-input : World String -> World
; Creates a new world with a new input value
(define (set-world-input world input)
  (make-world (world-current world)
              (world-questions world)
              input
              (world-ticks world)
              (world-state world)
              (world-stop world)))


; set-world-ticks : World Number -> World
; Creates a new world with a new ticks value
(define (set-world-ticks world ticks)
  (make-world (world-current world)
              (world-questions world)
              (world-input world)
              ticks
              (world-state world)
              (world-stop world)))

; set-world-state : World State -> World
; Creates a world with a new state
(define (set-world-state world state)
  (make-world (world-current world)
              (world-questions world)
              (world-input world)
              (world-ticks world)
              state
              (world-stop world)))

; set-world-stop : World Boolean -> World
; Creates a new world with a new stop
(define (set-world-stop world stop)
  (make-world (world-current world)
              (world-questions world)
              (world-input world)
              (world-ticks world)
              (world-state world)
              stop))

  


(define initial-world
  (make-world 0 (create-default-map QUESTIONS "") "" 0 NOT-ACCEPTING-INPUT #false))

; current-question : World -> Question
; Gets the current question of the world.
(define (current-question world)
  (entry-key (map-ref (world-questions world) (world-current world))))


; main : Any -> Boolean
(define (main _)
  (big-bang initial-world
    [to-draw draw-scene]
    [on-key interact]
    [on-tick update-clock 1]
    [stop-when (λ (world) (world-stop world))]))

; update-clock : World -> World
; Updates the amount of seconds that have gone by.
(define (update-clock world)
  (local ((define still-time? (<= (world-ticks world) (* 60 10))))
    (cond [(string=? (world-state world) ENDING)
           (set-world-stop world #true)]
          [(string=? (world-state world) ACCEPTING-INPUT) 
           (if still-time?
               (set-world-ticks world (add1 (world-ticks world)))
               (set-world-stop world #true))]
          [else world])))


; all-questions-answered : World -> Boolean
; Determines if all questions have been answered
(define (all-questions-answered world)
  (andmap (λ (entry) (> (string-length (entry-value entry)) 0)) (world-questions world)))

; - "starting"
; - "accepting-input"
; - "question-answered"
; - "not-accepting-input"
; - "ending-failure"
; - "ending-success"

; interact : World KeyEvent -> World
; Lets a user interact with the game
; - If the 
(define (interact world keyevent)
  (local ((define state (world-state world)))
    (cond [(string=? state NOT-ACCEPTING-INPUT) (interact-not-accepting world keyevent)]
          [(string=? state ACCEPTING-INPUT) (interact-accepting world keyevent)]
          [else world])))

; interact-not-accepting : World KeyEvent-> World
; Lets a user interact during the starting state.
(define (interact-not-accepting world keyevent)
  (cond [(string=? keyevent "right") (set-world-state world ACCEPTING-INPUT)]
        [else world]))

(check-expect (world-state (interact-not-accepting initial-world "right"))
              ACCEPTING-INPUT)

; interact-accepting : World KeyEvent -> World
; Lets a user type input into the program.
(define (interact-accepting world keyevent)
  (cond
    [(string=? keyevent "right")
     (make-world (if (> (add1 (world-current world)) (sub1 (length (world-questions world))))
                     (world-current world)
                     (add1 (world-current world)))
                 (world-questions world)
                 ""
                 (world-ticks world)
                 (world-state world)
                 (world-stop world))]
    [(string=? keyevent "left")
     (make-world (if (< (sub1 (world-current world)) 0) (world-current world)
                     (sub1 (world-current world)))
                 (world-questions world)
                 ""
                 (world-ticks world)
                 (world-state world)
                 (world-stop world))]
    [(string=? keyevent "\r")
     (local ((define new-world (make-world (world-current world)
                                           (put (current-question world)
                                                (world-input world)
                                                (world-questions world)
                                                question=?)
                                           ""
                                           (world-ticks world)
                                           (world-state world)
                                           (world-stop world))))
       (if (all-questions-answered new-world)
           (set-world-state new-world ENDING)
           new-world))]
    [(or (string=? keyevent "shift") (string=? keyevent "rshift")) world]
    [(string=? keyevent "\b") (set-world-input world
                                               (substring
                                                (world-input world)
                                                0
                                                (if (<= (sub1 (string-length (world-input world))) 0)
                                                    0
                                                    (sub1 (string-length (world-input world))))))]
    [else (set-world-input world (string-append (world-input world) keyevent))]))

(check-expect (world-input (interact-accepting
                            (make-world 0
                                        (list (make-entry (make-question "test"
                                                                         (list 5)
                                                                         (λ (n) #false))
                                                          ""))
                                        "T"
                                        0
                                        ACCEPTING-INPUT
                                        #false)
                            "right"))
              "")


; ======================== Drawing ==========================

(define BACKGROUND-X 720)
(define BACKGROUND-Y 720)

(define ARROW-RED (above (triangle 35 "solid" "red") (rectangle 20 60 "solid" "red")))
(define RIGHT-ARROW (overlay (text "NEXT" 12 "black") (rotate 270 ARROW-RED)))
(define LEFT-ARROW (overlay (text "PREVIOUS" 12 "black") (rotate 90 ARROW-RED)))

(define TEXTBOX (frame (rectangle (* .8 BACKGROUND-X) (* .2 BACKGROUND-Y) "solid" "gray")))

; draw-scene -> World -> Image
(define (draw-scene world)
  (cond [(string=? (world-state world) NOT-ACCEPTING-INPUT)
         (draw-in-game-instructions world)]
        [(string=? (world-state world) ACCEPTING-INPUT)
         (place-image (draw-time world)
                      (/ BACKGROUND-X 2)
                      (/ BACKGROUND-Y 8)
                      (place-image
                       (text (question-q (current-question world)) 20 "black")
                       (/ BACKGROUND-X 2)
                       (/ BACKGROUND-Y 4)
                       (place-image LEFT-ARROW
                                    (/ BACKGROUND-X 8)
                                    (/ BACKGROUND-Y 8)
                                    (place-image
                                     RIGHT-ARROW
                                     (* (/ BACKGROUND-X 8) 7)
                                     (/ BACKGROUND-Y 8)
                                     (place-image (draw-input world)
                                                  (/ BACKGROUND-X 2)
                                                  (* (/ BACKGROUND-Y 8) 7)
                                                  PARCHMENT)))))]
        [(string=? (world-state world) ENDING)
         (print-certificate (grade-test (world-questions world)))]))

; draw-input : World -> Image
; Draws the text onto the canvas.
(define (draw-input world)
  (local ((define input (world-input world))
          (define last-answer (entry-value (map-ref (world-questions world) (world-current world))))) 
    (frame (overlay (text input 24 "black") (place-image
                                             (text (string-append
                                                    "Last Answer: "
                                                    (if (string=? last-answer "")
                                                        "?"
                                                        last-answer))
                                                   12
                                                   "black")
                                             (/ (image-width TEXTBOX) 2)
                                             (/ (image-height TEXTBOX) 8)
                                             TEXTBOX)))))

; draw-time : World -> Image
; Draws the amount of minutes and seconds left.
(define (draw-time world)
  (local ((define seconds (modulo (world-ticks world) 60))
          (define minutes (quotient (world-ticks world) 60))
          (define second-string (if (< seconds 10) (string-append "0" (number->string seconds))
                                    (number->string seconds))))
    (text (string-append (number->string minutes) ":" second-string) 40 (make-color 0 122 230 255))))


; draw-in-game-instructions : World -> Image
; Draws the instructions for the player during the game.
(define (draw-in-game-instructions world)
  (place-image (text "\"Unessay\" Project\n   Kyle Posluns" 24 (make-color 178 34 34 255))
               (/ BACKGROUND-X 2)
               (/ BACKGROUND-Y 8)            
               (place-image (text INTERP 16 "black")
                            (/ BACKGROUND-X 2)
                            (* (/ BACKGROUND-Y 16) 9)
                            (place-image (text CONTROLS 16 "black")
                                         (/ BACKGROUND-X 2)
                                         (* (/ BACKGROUND-Y 8) 7)
                                         (place-image (text PREMISE 16 "black")
                                                      (/ BACKGROUND-X 2)
                                                      (/ BACKGROUND-Y 4)
                                                      (place-image RIGHT-ARROW
                                                                   (* (/ BACKGROUND-X 8) 7)
                                                                   (/ BACKGROUND-Y 8)
                                                                   PARCHMENT))))))

; print-certificate : Boolean -> Image
; Gives the user feedback based on how they did on the test
(define (print-certificate results)
  (if results
      (place-image (frame (overlay (text "You are eligible to vote, congrats!" 30 "green")
                                   (rectangle 500 100 "solid" (make-color 255 255 255 200))))
                   (/ BACKGROUND-X 2)
                   (/ BACKGROUND-Y 4)
                   SEAL)
      (place-image (frame (overlay (text "YOU ARE INELIGIBLE TO VOTE!!!" 30 "red")
                                   (rectangle 500 100 "solid" (make-color 255 255 255 200))))
                   (/ BACKGROUND-X 2)
                   (/ BACKGROUND-Y 4)
                   SEAL)))

(main 2)