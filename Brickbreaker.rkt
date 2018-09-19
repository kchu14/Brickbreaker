;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw6 (1) (1)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|--- CONSTANTS ---|#
(define WIDTH 200)
(define HEIGHT 200)

(define BALL-COLOR 'blue)
(define BALL-RADIUS 6)
(define BALL-SPEED 4)
(define BALL-IMG (circle BALL-RADIUS "solid" BALL-COLOR))

(define BRICK-COLOR 'red)
(define BRICK-WIDTH 30)
(define BRICK-HEIGHT 10)
(define BRICK-PADDING 10)
(define ROWS 3)
(define COLUMNS 5)
(define BRICK-IMG (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR))

(define PADDLE-COLOR 'purple)
(define PADDLE-WIDTH 40)
(define PADDLE-HEIGHT BRICK-HEIGHT)
(define PADDLE-Y (- HEIGHT (/ PADDLE-HEIGHT 2)))
(define PADDLE-SPEED 10)



(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define PADDLE-IMG (rectangle PADDLE-WIDTH BRICK-HEIGHT "solid" PADDLE-COLOR))
(define WIN (place-image (text "You Win :)" 20 "black")
                         100 100
                         BACKGROUND))

(define LOSE (place-image (text "You Lost :(" 20 "black")
                          100 100
                          BACKGROUND))


;;; World definition:
;; A World is a (make-world LoB Paddle Ball Boolean Number Number)
;;Interpretation: World takes a list of bricks, a Paddle, a Ball,
;; a boolean on whether or not the world is moving
;; a number representing the amount of lives
;;and a number representing the score
(define-struct world (lob paddle ball moving? lives score))

;;World -> ???
(define (world-temp w)
  (... (lob-temp (world-lob w))... (paddle-temp (world-paddle w)) ... (ball-temp (world-ball w))...
       (world-moving? w) (world-lives w) (world-score w)))



;A Brick is a (make-posn x y)
(define B1 (make-posn 20 15))
(define B2 (make-posn 60 15))
(define B3 (make-posn 100 15))
(define B4 (make-posn 140 15))
(define B5 (make-posn 180 15))
(define B6 (make-posn 20 35))
(define B7 (make-posn 60 35))
(define B8 (make-posn 100 35))
(define B9 (make-posn 140 35))
(define B10 (make-posn 180 35))
(define B11 (make-posn 20 55))
(define B12 (make-posn 60 55))
(define B13 (make-posn 100 55))
(define B14 (make-posn 140 55))
(define B15 (make-posn 180 55))


; a LoB (list of Bricks) is one of:
; - '()
; - (cons Brick LoB)
(define lob0 '())
(define lob1 (list B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 B13 B14 B15))
(define lob2 (list B1 B2 B3))
(define lob3 (list B4 B5 B6 B7))


#;
;; LoB -> ???
(define (lob-temp lob)
  (cond [(empty? lob)...]
        [(cons? lob)...(brick-temp (first lob))
                    ...(lob-temp (rest lob))]))

;;A Paddle is a (make-paddle Number String)
(define-struct paddle (x dir))
; - x is the x coordinate of the paddle
; - dir is the direction of the paddle ("left" "right")

(define PADDLE (make-paddle (/ WIDTH 2) "right"))
(define PADDLE2 (make-paddle 105 "left"))
(define PADDLE3 (make-paddle 20 "right"))

;;Paddle -> ???
(define (paddle-temp p)
  (... (paddle-x p)... (paddle-dir p)...))

;;A Ball is a (make-ball Number Number Number Number)
(define-struct ball (x y vx vy))
; - where the first Number is the ball's x-coordinate
; - the second Number is the ball's y-coordinate
; - the third Number is the ball's x-velocity
; - the fourth Number is the ball's y-velocity

(define ball0 (make-ball 0 0 1 1))
(define INITIAL-BALL (make-ball (/ WIDTH 2)
                                (- HEIGHT PADDLE-HEIGHT (/ BALL-RADIUS 2))
                                BALL-SPEED 0))

;;Ball -> ???
(define (ball-temp b)
  (...(ball-x b) ... (ball-y b) ... (ball-vx b) ... (ball-vy b)...))


(define w0 (make-world lob0 PADDLE ball0 #false 3 0))
(define w1 (make-world lob1 PADDLE INITIAL-BALL #false 3 0))
(define w2 (make-world lob2 PADDLE2 INITIAL-BALL #false 10 14))
(define w3 (make-world lob3 PADDLE3 INITIAL-BALL #false 2 10))
(define w4 (make-world lob3 PADDLE3 INITIAL-BALL #true 0 10))
(define w5 (make-world lob1 PADDLE (make-ball 2 11 0 0) #true 3 0))
(define w6 (make-world lob2 PADDLE (make-ball 100 189 1 -2) #true 3 0))
(define w7 (make-world lob1 PADDLE (make-ball 79 26 1 -2) #true 3 0))
(define w8 (make-world lob1 PADDLE (make-ball 10 200 1 -2) #true 3 0))
(define w9 (make-world lob1 PADDLE (make-ball 6 170 1 -2) #true 3 0))



;;;;;;;;;;;;RENDERING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw-world: World -> Image
;; renders the bricks, ball, and paddle onto the window


(check-expect (draw-world w0)
              (place-image PADDLE-IMG (/ WIDTH 2) PADDLE-Y
                           (draw-score (world-score w0)
                                       (place-image BALL-IMG 0 0 BACKGROUND))))

(check-expect (draw-world w2)
              (place-image BRICK-IMG 20 15
                           (place-image BRICK-IMG 60 15
                                        (place-image BRICK-IMG 100 15
                                                     (place-image PADDLE-IMG 105 PADDLE-Y
                                                                  (draw-score (world-score w2)
                                                                              (place-image
                                                                               BALL-IMG
                                                                               100
                                                                               187 BACKGROUND)))))))

(check-expect (draw-world w3)
              (place-image BRICK-IMG 140 15
                           (place-image BRICK-IMG 180 15
                                        (place-image
                                         BRICK-IMG 20 35
                                         (place-image
                                          BRICK-IMG
                                          60 35
                                          (place-image PADDLE-IMG
                                                       20 PADDLE-Y
                                                       (draw-score (world-score w3)
                                                                   (place-image
                                                                    BALL-IMG
                                                                    100
                                                                    187 BACKGROUND))))))))

(check-expect (draw-world w4)
              (place-image BRICK-IMG 140 15
                           (place-image BRICK-IMG 180 15
                                        (place-image
                                         BRICK-IMG 20 35
                                         (place-image
                                          BRICK-IMG
                                          60 35
                                          (place-image PADDLE-IMG
                                                       20 PADDLE-Y
                                                       (draw-score (world-score w4)
                                                                   (place-image
                                                                    BALL-IMG
                                                                    100
                                                                    187 BACKGROUND))))))))



(define (draw-world w)
  (draw-paddle (world-paddle w) PADDLE-IMG
               (draw-score (world-score w)
                           (draw-ball (world-ball w) BALL-IMG
                                      (draw-lob (world-lob w))))))

;; draw-brick: Brick Image Image -> Image
;; draws the brick on a given image
(check-expect (draw-brick B1 BRICK-IMG BACKGROUND)
              (place-image BRICK-IMG 20 15 BACKGROUND))
(check-expect (draw-brick B2 BRICK-IMG BACKGROUND)
              (place-image BRICK-IMG 60 15 BACKGROUND))
(check-expect (draw-brick B3 BRICK-IMG BACKGROUND)
              (place-image BRICK-IMG 100 15 BACKGROUND))
(define (draw-brick b img scene)
  (place-image img (posn-x b) (posn-y b) scene))

;; draw-paddle: Paddle Image Image -> Image
;; draws the paddle on a given image
(check-expect (draw-paddle PADDLE PADDLE-IMG BACKGROUND)
              (place-image PADDLE-IMG 100 PADDLE-Y BACKGROUND))
(check-expect (draw-paddle PADDLE2 PADDLE-IMG BACKGROUND)
              (place-image PADDLE-IMG 105 PADDLE-Y BACKGROUND))
(check-expect (draw-paddle PADDLE3 PADDLE-IMG BACKGROUND)
              (place-image PADDLE-IMG 20 PADDLE-Y BACKGROUND))

(define (draw-paddle pd img scene)
  (place-image img (paddle-x pd) PADDLE-Y scene))

;; draw-score: Number Image -> Image
;; draws the score on the given image
(check-expect (draw-score 5 BACKGROUND)
              (place-image (text "5" 10 "black") 170 170 BACKGROUND))
(check-expect (draw-score 0 BACKGROUND) (place-image (text "0" 10 "black") 170 170 BACKGROUND))
(check-expect (draw-score 150 BACKGROUND) (place-image (text "150" 10 "black") 170 170 BACKGROUND))

(define (draw-score num scene)
  (place-image (text (number->string num) 10 "black") 170 170 scene))

;; draw-ball: Ball Image Image -> Image
;; draws the ball on a given image

(check-expect (draw-ball ball0 BALL-IMG BACKGROUND)
              (place-image BALL-IMG 0 0 BACKGROUND))

(check-expect (draw-ball INITIAL-BALL BALL-IMG BACKGROUND)
              (place-image BALL-IMG 100 187 BACKGROUND))

(define (draw-ball b img scene)
  (place-image img (ball-x b) (ball-y b) scene))

;; draw-lob: LoB -> Image
;; draws the bricks onto the background
(check-expect (draw-lob lob0) BACKGROUND)
(check-expect (draw-lob lob2)
              (place-image BRICK-IMG 20 15
                           (place-image BRICK-IMG 60 15
                                        (place-image BRICK-IMG 100 15 BACKGROUND))))
(check-expect (draw-lob lob3)
              (place-image BRICK-IMG 140 15
                           (place-image BRICK-IMG 180 15
                                        (place-image
                                         BRICK-IMG 20 35
                                         (place-image
                                          BRICK-IMG
                                          60 35 BACKGROUND)))))
              
(define (draw-lob alob)
  (cond [(empty? alob) BACKGROUND]
        [(cons? alob) (draw-brick (first alob) BRICK-IMG
                                  (draw-lob (rest alob)))]))


 
#|--- FUNCTIONS ---|#

;; move-world: World -> World
;; move the ball at each tick. If the ball hits a brick, it removes the brick
;; if it hits the paddle, it will bounce back
(check-expect (move-world w1) w1)
(check-expect (move-world w5) (make-world
                               (list
                                (make-posn 60 15)
                                (make-posn 100 15)
                                (make-posn 140 15)
                                (make-posn 180 15)
                                (make-posn 20 35)
                                (make-posn 60 35)
                                (make-posn 100 35)
                                (make-posn 140 35)
                                (make-posn 180 35)
                                (make-posn 20 55)
                                (make-posn 60 55)
                                (make-posn 100 55)
                                (make-posn 140 55)
                                (make-posn 180 55))
                               (make-paddle 100 "right")
                               (make-ball 2 11 0 0)
                               #true
                               3
                               10))
(check-expect (move-world w6) (make-world
                               (list
                                (make-posn 20 15)
                                (make-posn 60 15)
                                (make-posn 100 15))
                               (make-paddle 100 "right")
                               (make-ball 101 191 1 2)
                               #true
                               3
                               0))
(check-expect (move-world w7) 
              (make-world
               (list
                (make-posn 20 15)
                (make-posn 60 15)
                (make-posn 100 15)
                (make-posn 140 15)
                (make-posn 180 15)
                (make-posn 20 35)
                (make-posn 60 35)
                (make-posn 100 35)
                (make-posn 140 35)
                (make-posn 180 35)
                (make-posn 20 55)
                (make-posn 60 55)
                (make-posn 100 55)
                (make-posn 140 55)
                (make-posn 180 55))
               (make-paddle 100 "right")
               (make-ball 80 24 1 -2)
               #true
               3
               0))
(check-expect (move-world w8)
              (make-world
               (list
                (make-posn 20 15)
                (make-posn 60 15)
                (make-posn 100 15)
                (make-posn 140 15)
                (make-posn 180 15)
                (make-posn 20 35)
                (make-posn 60 35)
                (make-posn 100 35)
                (make-posn 140 35)
                (make-posn 180 35)
                (make-posn 20 55)
                (make-posn 60 55)
                (make-posn 100 55)
                (make-posn 140 55)
                (make-posn 180 55))
               (make-paddle 100 "right")
               (make-ball 100 187 4 0)
               #false
               2
               0))

(check-expect (move-world w9) (make-world
                               (list
                                (make-posn 20 15)
                                (make-posn 60 15)
                                (make-posn 100 15)
                                (make-posn 140 15)
                                (make-posn 180 15)
                                (make-posn 20 35)
                                (make-posn 60 35)
                                (make-posn 100 35)
                                (make-posn 140 35)
                                (make-posn 180 35)
                                (make-posn 20 55)
                                (make-posn 60 55)
                                (make-posn 100 55)
                                (make-posn 140 55)
                                (make-posn 180 55))
                               (make-paddle 100 "right")
                               (make-ball 5 168 -1 -2)
                               #true
                               3
                               0))


(define (move-world w)
  (cond [(and (touch-any? (world-lob w) (world-ball w))(world-moving? w))
         (make-world (new-brick-list (world-lob w) (world-ball w)) (world-paddle w) 
                     (change-ball-v (world-ball w)
                                    (brick-check (world-ball w) (world-lob w))) #true
                                                                                (world-lives w)
                                                                                (change-score
                                                                                 (world-score w)))]
        [(and (touch-paddle? (world-paddle w) (world-ball w)) (> 15 (length (world-lob w)))
              (world-moving? w))
         (make-world (world-lob w) (world-paddle w)
                     (move-ball (change-ball-v-paddle (world-paddle w)
                                                      (world-ball w))) (world-moving? w)
                                                                       (world-lives w)
                                                                       (world-score w))]
        [(and (touch-wall? (world-ball w))(world-moving? w))
         (make-world (world-lob w) (world-paddle w)
                     (move-ball (wall-v (world-ball w)))(world-moving? w)(world-lives w)
                     (world-score w))]
        [(touch-bottom-wall? (world-ball w)) (make-world (world-lob w) PADDLE INITIAL-BALL #false
                                                         (sub1 (world-lives w))(world-score w))]
        [(not (world-moving? w)) w]
        [else (make-world (world-lob w) (world-paddle w) (move-ball (world-ball w))
                          (world-moving? w)(world-lives w) (world-score w))])) 
                  
;; change-score: Number -> Number
;; updates the score of the game based off of the number of bricks hit
(check-expect (change-score 1) 11)
(check-expect (change-score 0) 10)
(check-expect (change-score 100) 110)
(check-expect (change-score -5) 5)
(define (change-score num)
  (+ 10 num))
      

;; brick-check: Ball LoB -> Brick
;; checks which brick the ball is hitting
(check-expect (brick-check INITIAL-BALL '()) '())
(check-expect (brick-check (make-ball 2 11 0 0) lob2) B1)
(check-expect (brick-check (make-ball 92 7 0 0) lob2) B3)
(define (brick-check ball lob)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (touch? (first lob) ball)
                         (first lob)
                         (brick-check ball (rest lob)))]))

;; move-ball: Ball -> Ball
;; creates a new ball based off of the given x and y velocity
(check-expect (move-ball INITIAL-BALL) (make-ball 104 187 4 0))
(check-expect (move-ball (make-ball 100 20 50 23)) (make-ball 150 43 50 23))
(check-expect (move-ball (make-ball 100 20 0 23)) (make-ball 100 43 0 23))
(define (move-ball ball)
  (make-ball (+ (ball-x ball) (ball-vx ball))
             (+ (ball-y ball) (ball-vy ball)) (ball-vx ball) (ball-vy ball))) 


;; change-ball-v: Ball Brick -> Ball
;; changes the velocity of the ball after checking which side it hits the brick from
;;if the ball does not hit a brick, do nothing
(check-expect (change-ball-v INITIAL-BALL B1) INITIAL-BALL)
(check-expect (change-ball-v (make-ball 60 7 1 0) B2) (make-ball 60 7 1 0))
(check-expect (change-ball-v (make-ball 20 22 0 1) B1) (make-ball 20 22 0 -1))
(check-expect (change-ball-v (make-ball 35 17 8 5) B1) (make-ball 35 17 -8 5))
(check-expect (change-ball-v (make-ball 5 17 -7 -1) B1) (make-ball 5 17 7 -1))
(check-expect (change-ball-v (make-ball 79 4  -1 1) B3) (make-ball 79 4 1 -1))
(check-expect (change-ball-v (make-ball 79 26 -1 3) B3) (make-ball 79 26 1 -3))
(check-expect (change-ball-v (make-ball 121 4 2 4) B3) (make-ball 121 4 -2 -4))
(check-expect (change-ball-v (make-ball 121 26 1 3) B3) (make-ball 121 26 -1 -3))





(define (change-ball-v ball brick)
  (cond [(and (touch-brick-right? ball brick) (touch-brick-bottom? ball brick))
         (velocity-top-bottom (velocity-side ball))]
        [(and (touch-brick-left? ball brick) (touch-brick-bottom? ball brick))
         (velocity-top-bottom (velocity-side ball))]
        [(and (touch-brick-top? ball brick) (touch-brick-right? ball brick))
         (velocity-top-bottom (velocity-side ball))]
        [(and (touch-brick-top? ball brick) (touch-brick-left? ball brick))
         (velocity-top-bottom (velocity-side ball))]
        [(or (touch-brick-left? ball brick) (touch-brick-right? ball brick))
         (velocity-side ball)]
        [(or (touch-brick-top? ball brick) (touch-brick-bottom? ball brick))
         (velocity-top-bottom ball)]
        [else ball]))
      

;; velocity-side: Ball -> Ball
;; changes the velocity of the ball if it hits object from the side
(check-expect (velocity-side (make-ball 100 20 50 23)) (make-ball 100 20 -50 23))
(check-expect (velocity-side INITIAL-BALL) (make-ball 100 187 -4 0))
(check-expect (velocity-side ball0) (make-ball 0 0 -1 1))
              
(define (velocity-side b)
  (make-ball (ball-x b) (ball-y b) (* -1 (ball-vx b)) (ball-vy b)))
      

;; velocity-top-bottom: Ball -> Ball
;; changes the velocity of the ball if it hits object from the top or bottom
(check-expect (velocity-top-bottom (make-ball 100 20 50 23)) (make-ball 100 20 50 -23))
(check-expect (velocity-top-bottom INITIAL-BALL) (make-ball 100 187 4 0))
(check-expect (velocity-top-bottom ball0) (make-ball 0 0 1 -1))

(define (velocity-top-bottom b)
  (make-ball (ball-x b) (ball-y b) (ball-vx b) (* -1 (ball-vy b))))


;; touch-any?: LoB Ball -> Boolean
;; checks if any of the bricks in the list are touching the ball
(check-expect (touch-any? lob1 INITIAL-BALL) #false)
(check-expect (touch-any? lob1 (make-ball 60 35 0 0)) #false)
(check-expect (touch-any? lob1 (make-ball 2 11 0 0)) #true)

(define (touch-any? lob ball)
  (cond [(empty? lob) #false]
        [(cons? lob) (or (touch? (first lob) ball)
                         (touch-any? (rest lob) ball))]))

;;new-brick-list: LoB Ball -> LoB
;;Creates a new list of all the bricks that have not been hit by the ball
(check-expect (new-brick-list lob1 INITIAL-BALL) lob1)
(check-expect (new-brick-list lob1 (make-ball 20 23 0 0))
              (list B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 B13 B14 B15))
(check-expect (new-brick-list lob1 (make-ball 180 47 0 0))
              (list B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 B13 B14))
(define (new-brick-list lob ball)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (touch? (first lob) ball)
                         (new-brick-list (rest lob) ball)
                         (cons (first lob) (new-brick-list (rest lob) ball)))]))

;; touch? : Brick Ball -> Boolean
;; are the ball and the brick touching/at the same location?
(check-expect (touch? B1 INITIAL-BALL) #false)
(check-expect (touch? B1 (make-ball 2 7 0 0)) #false)
(check-expect (touch? B1 (make-ball 2 11 0 0)) #true)
(check-expect (touch? B1 (make-ball 2 17 0 0)) #true)
(check-expect (touch? B1 (make-ball 17 7 0 0)) #true)
(check-expect (touch? B1 (make-ball 17 12 0 0)) #false)
(check-expect (touch? B1 (make-ball 17 17 0 0)) #false)
(check-expect (touch? B1 (make-ball 32 7 0 0)) #true)
(check-expect (touch? B1 (make-ball 32 12 0 0)) #true)
(check-expect (touch? B1 (make-ball 32 17 0 0)) #true)
(check-expect (touch? B1 (make-ball 2 15 0 0)) #true)
(check-expect (touch? B1 (make-ball 27 18 0 0)) #true)
(check-expect (touch? B1 (make-ball 39 15 0 0)) #true)



(define (touch? brick ball)
  (or (and (or (touch-brick-top? ball brick)
               (touch-brick-bottom? ball brick))
           (and (>= (ball-x ball) (- (posn-x brick) (/ BRICK-WIDTH 2)))
                (<= (ball-x ball) (+ (posn-x brick) (/ BRICK-WIDTH 2)))))   
      (and (or (touch-brick-right? ball brick)
               (touch-brick-left? ball brick))
           (and (>= (ball-y ball) (- (posn-y brick) (/ BRICK-HEIGHT 2)))
                (<= (ball-y ball) (+ (posn-y brick) (/ BRICK-HEIGHT 2)))))))



       
;; touch-brick-top?: Ball Brick -> Boolean
;; checks if the ball hits the brick from the top
(check-expect (touch-brick-top? INITIAL-BALL B1) #false)
(check-expect (touch-brick-top? (make-ball 17 17 0 0) B1) #false)
(check-expect (touch-brick-top? (make-ball 20 8 0 0) B1) #true)
(check-expect (touch-brick-top? (make-ball 60 7 0 0) B2) #true)
(check-expect (touch-brick-top? (make-ball 60 15 0 0) B2) #false)

(define (touch-brick-top? ball brick)
  (and (<= (+ (ball-y ball) BALL-RADIUS) (posn-y brick))
       (>= (+ (ball-y ball) BALL-RADIUS) (- (posn-y brick) (/ BRICK-HEIGHT 2)))))

;; touch-brick-bottom?: Ball Brick -> Boolean
;; checks if the ball hits the brick from the bottom
(check-expect (touch-brick-bottom? INITIAL-BALL B1) #false)
(check-expect (touch-brick-bottom? (make-ball 2 11 0 0) B1) #false)
(check-expect (touch-brick-bottom? (make-ball 2 17 0 0) B2) #false)
(check-expect (touch-brick-bottom? (make-ball 20 22 0 0) B1) #true)
(check-expect (touch-brick-bottom? (make-ball 60 21 0 0) B2) #true)
(define (touch-brick-bottom? ball brick)
  (and (<= (- (ball-y ball) BALL-RADIUS) (+ (posn-y brick) (/ BRICK-HEIGHT 2)))
       (>= (- (ball-y ball) BALL-RADIUS) (posn-y brick))))

;; touch-brick-right?: Ball Brick -> Boolean
;; checks if the ball hits the brick from the right
(check-expect (touch-brick-right? INITIAL-BALL B1) #false)
(check-expect (touch-brick-right? (make-ball 17 17 0 0) B1) #false)
(check-expect (touch-brick-right? (make-ball 35 17 0 0) B1) #true)
(define (touch-brick-right? ball brick)
  (and (>= (- (ball-x ball) BALL-RADIUS) (posn-x brick))
       (<= (- (ball-x ball) BALL-RADIUS) (+ (posn-x brick) (/ BRICK-WIDTH 2)))))
  
;; touch-brick-left?: Ball Brick -> Boolean
;; checks if the ball hits the brick from the left
(check-expect (touch-brick-left? INITIAL-BALL B1) #false)
(check-expect (touch-brick-left? (make-ball 17 17 0 0) B1) #false)
(check-expect (touch-brick-left? (make-ball 5 17 0 0) B1) #true)
(define (touch-brick-left? ball brick)
  (and (>= (+ (ball-x ball) BALL-RADIUS) (- (posn-x brick) (/ BRICK-WIDTH 2)))
       (<= (+ (ball-x ball) BALL-RADIUS) (posn-x brick))))
 
;; touch-paddle?: Paddle Ball -> Boolean
;; checks if the ball touches the paddle at all
(check-expect (touch-paddle? PADDLE INITIAL-BALL) #true)
(check-expect (touch-paddle? PADDLE (make-ball 100 100 -1 1)) #false)
(check-expect (touch-paddle? PADDLE (make-ball 110 187 1 1)) #true)
(check-expect (touch-paddle? PADDLE (make-ball 90 187 -2 1)) #true)
(define (touch-paddle? p b)
  (or (touch-mid-paddle? p b)
      (touch-left-paddle? p b)
      (touch-right-paddle? p b)))

;; change-ball-v-paddle: Paddle Ball -> Ball
;; creates a new ball based on which 3rd of the paddle the ball hits (mid, left, right)
(check-expect (change-ball-v-paddle PADDLE INITIAL-BALL) (velocity-top-bottom INITIAL-BALL))
(check-expect (change-ball-v-paddle PADDLE
                                    (make-ball 88 187 -1 2))
              (velocity-top-bottom
               (velocity-side (add-velocity (make-ball 88 187 -1 2)))))
(check-expect (change-ball-v-paddle PADDLE
                                    (make-ball 120 187 5 2))
              (velocity-top-bottom
               (velocity-side (sub-velocity (make-ball 120 187 5 2)))))
(check-expect (change-ball-v-paddle PADDLE
                                    (make-ball 88 187 3 2))
              (velocity-top-bottom (sub-velocity (make-ball 88 187 3 2))))
(check-expect (change-ball-v-paddle PADDLE
                                    (make-ball 120 187 -5 2))
              (velocity-top-bottom
               (add-velocity (make-ball 120 187 -5 2))))

(define (change-ball-v-paddle p b)
  (cond [(touch-mid-paddle? p b) (velocity-top-bottom b)]
        [(and (touch-left-paddle? p b) (touch-paddle-from-left? b))
         (velocity-top-bottom (velocity-side (add-velocity b)))]
        [(and (touch-right-paddle? p b) (touch-paddle-from-right? b))
         (velocity-top-bottom (velocity-side (sub-velocity b)))]
        [(and (touch-left-paddle? p b) (touch-paddle-from-right? b))
         (velocity-top-bottom (sub-velocity b))]
        [(and (touch-right-paddle? p b) (touch-paddle-from-left? b))
         (velocity-top-bottom (add-velocity b))]))

;; add-velocity: Ball -> Ball
;; adds 1 to the x-velocity of a ball
(check-expect (add-velocity INITIAL-BALL) (make-ball 100 187 5 0))
(check-expect (add-velocity (make-ball 100 200 300 400)) (make-ball 100 200 301 400))
(check-expect (add-velocity ball0) (make-ball 0 0 2 1))

(define (add-velocity b)
  (make-ball (ball-x b) (ball-y b) (+ 1 (ball-vx b)) (ball-vy b)))

;; sub-velocity: Ball -> Ball
;; subtracts 1 from the x-velocity of a ball
(check-expect (sub-velocity INITIAL-BALL) (make-ball 100 187 3 0))
(check-expect (sub-velocity (make-ball 100 200 300 400)) (make-ball 100 200 299 400))
(check-expect (sub-velocity ball0) (make-ball 0 0 0 1))

(define (sub-velocity b)
  (make-ball (ball-x b) (ball-y b) (- (ball-vx b) 1) (ball-vy b))) 

;; touch-mid-paddle?: Paddle Ball -> Boolean
;; checks if the ball hits the middle 3rd of the paddle
(check-expect (touch-mid-paddle? PADDLE INITIAL-BALL) #true)
(check-expect (touch-mid-paddle? PADDLE (make-ball 94 187 0 0)) #true)
(check-expect (touch-mid-paddle? PADDLE (make-ball 93.33 187 -1 2)) #false)
(check-expect (touch-mid-paddle? PADDLE (make-ball 106.66 187 5 -4)) #true)
              
(define (touch-mid-paddle? p b)
  (and (>= (+ (ball-y b) BALL-RADIUS) (- PADDLE-Y (/ PADDLE-HEIGHT 2)))
       (>= (ball-x b) (- (paddle-x p) 6.66))
       (<= (ball-x b) (+ (paddle-x p) 6.66))))



;; touch-left-paddle?: Paddle Ball -> Boolean
;; checks if the ball hits the left 3rd of the paddle

(check-expect (touch-left-paddle? PADDLE INITIAL-BALL) #false)
(check-expect (touch-left-paddle? PADDLE (make-ball 94 187 0 0)) #false)
(check-expect (touch-left-paddle? PADDLE (make-ball 88 187 -1 2)) #true)
(check-expect (touch-left-paddle? PADDLE (make-ball 93.33 187 5 -4)) #true)
              
(define (touch-left-paddle? p b)
  (and (>= (+ (ball-y b) BALL-RADIUS) (- PADDLE-Y (/ PADDLE-HEIGHT 2)))
       (>= (ball-x b) (- (paddle-x p) 20))
       (<= (ball-x b) (- (paddle-x p) 6.66))))



;; touch-right-paddle?: Paddle Ball -> Boolean
;; checks if the ball hits the right 3rd of the paddle
(check-expect (touch-right-paddle? PADDLE INITIAL-BALL) #false)
(check-expect (touch-right-paddle? PADDLE (make-ball 94 187 0 0)) #false)
(check-expect (touch-right-paddle? PADDLE (make-ball 120 187 -1 2)) #true)
(check-expect (touch-right-paddle? PADDLE (make-ball 106.66 187 5 -4)) #true)
             
(define (touch-right-paddle? p b)
  (and (>= (+ (ball-y b) BALL-RADIUS) (- PADDLE-Y (/ PADDLE-HEIGHT 2)))
       (>= (ball-x b) (+ (paddle-x p) 6.66))
       (<= (ball-x b) (+ (paddle-x p) 20))))

;; touch-paddle-from-left?: Ball -> Boolean
;; checks if the ball's x velocity is less than or equal to 0
(check-expect (touch-paddle-from-left? INITIAL-BALL) #false)
(check-expect (touch-paddle-from-left? ball0) #false)
(check-expect (touch-paddle-from-left? (make-ball 5 100 -1 10)) #true)
(check-expect (touch-paddle-from-left? (make-ball 5 100 0 10)) #true)

(define (touch-paddle-from-left? b)
  (>= 0 (ball-vx b)))

;; touch-paddle-from-right?: Ball -> Boolean
;; checks if the ball's x velocity is greater than or equal to 0
(check-expect (touch-paddle-from-right? INITIAL-BALL) #true)
(check-expect (touch-paddle-from-right? ball0) #true)
(check-expect (touch-paddle-from-right? (make-ball 5 100 -10 10)) #false)
(check-expect (touch-paddle-from-right? (make-ball 5 100 -50 32)) #false)
(check-expect (touch-paddle-from-right? (make-ball 5 100 0 10)) #true)

(define (touch-paddle-from-right? b)
  (<= 0 (ball-vx b)))


;; touch-wall?: Ball -> Boolean
;; checks if the ball touches any wall (left/right/or top)
(check-expect (touch-wall? INITIAL-BALL) #false)
(check-expect (touch-wall? (make-ball 0 100 5 -4)) #true)
(check-expect (touch-wall? (make-ball 200 20 -8 -4)) #true)
(check-expect (touch-wall? (make-ball 100 100 2 3)) #false)
(define (touch-wall? b)
  (or (touch-left-wall? b)
      (touch-right-wall? b)
      (touch-top-wall? b)))
      

;; touch-left-wall?: Ball -> Boolean
;;checks if the ball touches the left wall
(check-expect (touch-left-wall? (make-ball 0 100 5 -4)) #true)
(check-expect (touch-left-wall? (make-ball 100 100 2 3)) #false)
(check-expect (touch-left-wall? (make-ball 3 100 5 -4)) #true)
(check-expect (touch-left-wall? INITIAL-BALL) #false)
(define (touch-left-wall? b)
  (<= (- (ball-x b) BALL-RADIUS) 0))

;; touch-right-wall?: Ball -> Boolean
;;checks if the ball touches the right wall
(check-expect (touch-right-wall? (make-ball 200 100 5 -4)) #true)
(check-expect (touch-right-wall? (make-ball 100 100 2 3)) #false)
(check-expect (touch-right-wall? (make-ball 197 100 5 -4)) #true)
(check-expect (touch-right-wall? INITIAL-BALL) #false)
(define (touch-right-wall? b)
  (>= (+ (ball-x b) BALL-RADIUS) WIDTH))

;; touch-top-wall?: Ball -> Boolean
;;checks if the ball touches the top wall
(check-expect (touch-top-wall? (make-ball 200 100 5 -4)) #false)
(check-expect (touch-top-wall? (make-ball 100 0 2 3)) #true)
(check-expect (touch-top-wall? (make-ball 2 0 2 3)) #true)
(check-expect (touch-top-wall? (make-ball 197 100 5 -4)) #false)
(check-expect (touch-top-wall? INITIAL-BALL) #false)

(define (touch-top-wall? b)
  (<= (- (ball-y b) BALL-RADIUS) 0))

;; touch-bottom-wall?: Ball -> Boolean
;;checks if the ball touches the bottom wall
(check-expect (touch-bottom-wall? (make-ball 200 100 5 -4)) #false)
(check-expect (touch-bottom-wall? (make-ball 100 0 2 3)) #false)
(check-expect (touch-bottom-wall? INITIAL-BALL) #false)
(check-expect (touch-bottom-wall? (make-ball 100 200 2 3)) #true)
(check-expect (touch-bottom-wall? (make-ball 5 204 -32 7)) #true)

(define (touch-bottom-wall? b)
  (>= (+ (ball-y b) BALL-RADIUS) HEIGHT))


;; wall-v: Ball -> Ball
;; changes the ball velocity depending on what part of the wall it hits
;;if the ball doesn't hit any wall or hits the bottom wall, do nothing
(check-expect (wall-v (make-ball 100 0 2 3))
              (velocity-top-bottom (make-ball 100 0 2 3)))
(check-expect (wall-v (make-ball 197 100 5 -4))
              (velocity-side (make-ball 197 100 5 -4)))
(check-expect (wall-v (make-ball 3 100 5 -4))
              (velocity-side (make-ball 3 100 5 -4)))
(check-expect (wall-v INITIAL-BALL) INITIAL-BALL)

(define (wall-v b)
  (cond [(touch-top-wall? b) (velocity-top-bottom b)]
        [(touch-right-wall? b) (velocity-side b)]
        [(touch-left-wall? b) (velocity-side b)]
        [else b]))

;;;;;FROM WEBSITE
;; speed: Ball -> Number
;; compute the speed of the ball
(check-expect (speed INITIAL-BALL) 4)
(define (speed ball)
  (sqrt (+ (sqr (ball-vx ball))
           (sqr (ball-vy ball)))))

;;new-x-velocity : Ball Number -> Number
;;Produces the new x velocity of a ball that launched off a paddle with this x-coordinate
(define (new-x-velocity ball x)
  (inexact->exact
   (* .95
      (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS))
      (speed ball))))
(check-expect (new-x-velocity INITIAL-BALL 100) 0)
(check-expect (new-x-velocity (make-ball 60 190 3 4) 100)
              (inexact->exact (* 4.75 -40/26)))

;;new-y-velocity : Ball Number -> Number
;;Produces the new y velocity of a ball that launched off a paddle with this x-coordinate
(define (new-y-velocity ball x)
  (inexact->exact
   (* (- (sqrt (- 1 (sqr (* .95
                            (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS)))))))
      (speed ball))))
(check-expect (new-y-velocity INITIAL-BALL 100) -4)
(check-expect (new-y-velocity (make-ball 60 190 3 4) 100)
              (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))

;;launch-ball : Ball Number -> Ball
;;Launch ball off paddle with this x-coordinate
(define (launch-ball ball x)
  (make-ball (+ (ball-x ball) (new-x-velocity ball x))
             (+ (ball-y ball) (new-y-velocity ball x))
             (new-x-velocity ball x) (new-y-velocity ball x)))
(check-expect (launch-ball INITIAL-BALL 100)
              (make-ball 100 183 0 -4))
(check-expect (launch-ball (make-ball 60 190 3 4) 100)
              (make-ball (+ 60 (inexact->exact (* 4.75 -40/26)))
                         (+ 190 (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))
                         (inexact->exact (* 4.75 -40/26))
                         (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26))))))))

;;;;;;;;;;;;


;; change-dir: World KeyEvent -> World
;; Produces a new world based on the given keypress ("right", "left", " ")

(check-expect (change-dir w0 "left")
              (make-world
               '()
               (make-paddle 90 "left")
               (make-ball 20 0 1 1)
               #false
               3
               0))
(check-expect (change-dir w2 "right")
              (make-world
               (list
                (make-posn 20 15)
                (make-posn 60 15)
                (make-posn 100 15))
               (make-paddle 115 "right")
               (make-ball 110 187 4 0)
               #false
               10
               14))
(check-expect (change-dir w3 "right")
              (make-world
               (list
                (make-posn 140 15)
                (make-posn 180 15)
                (make-posn 20 35)
                (make-posn 60 35))
               (make-paddle 30 "right")
               (make-ball 110 187 4 0)
               #false
               2
               10))
(check-expect (change-dir w2 "left")
              (make-world
               (list
                (make-posn 20 15)
                (make-posn 60 15)
                (make-posn 100 15))
               (make-paddle 95 "left")
               (make-ball 90 187 4 0)
               #false
               10
               14))
(check-expect (change-dir w2 "a") w2)
(check-expect (change-dir w2 " ") (make-world (world-lob w2) (world-paddle w2)
                                              (launch-ball (world-ball w2)
                                                           (paddle-x (world-paddle w2)))
                                              #true (world-lives w2)
                                              (world-score w2)))
(check-expect (change-dir w4 "left") (make-world
                                      (list
                                       (make-posn 140 15)
                                       (make-posn 180 15)
                                       (make-posn 20 35)
                                       (make-posn 60 35))
                                      (make-paddle 20 "left")
                                      (make-ball 100 187 4 0)
                                      #true
                                      0
                                      10))
(check-expect (change-dir w4 "right") (make-world
                                       (list
                                        (make-posn 140 15)
                                        (make-posn 180 15)
                                        (make-posn 20 35)
                                        (make-posn 60 35))
                                       (make-paddle 30 "right")
                                       (make-ball 100 187 4 0)
                                       #true
                                       0
                                       10))
              


(define (change-dir w d)
  (cond [(and (or (key=? d "left") (key=? d "right")) (world-moving? w))
         (make-world (world-lob w) (make-paddle (update-x (world-paddle w) d) d) (world-ball w)
                     (world-moving? w) (world-lives w)(world-score w))]
        [(and (or (key=? d "left") (key=? d "right")) (not (world-moving? w)))
         (make-world (world-lob w) (make-paddle (update-x (world-paddle w) d) d)
                     (make-ball (update-x-ball (world-ball w) d) (ball-y (world-ball w))
                                (ball-vx (world-ball w)) (ball-vy (world-ball w)))
                     (world-moving? w) (world-lives w)(world-score w))]
        [(and (key=? d " ") (not (world-moving? w)))
         (make-world (world-lob w) (world-paddle w) (launch-ball (world-ball w)
                                                                 (paddle-x
                                                                  (world-paddle w)))
                     #true (world-lives w)(world-score w))]
        [else w]))

;; update-x: Paddle KeyEvent -> Number
;; changes the x coord of the paddle based on key pressed
(check-expect (update-x PADDLE "right") (+ (/ WIDTH 2) 10))
(check-expect (update-x PADDLE "left") (- (/ WIDTH 2) 10))
(check-expect (update-x PADDLE2 "right") 115)
(check-expect (update-x PADDLE2 "left") 95)

(define (update-x p d)
  (if (key=? d "right")
      (check-bounds (+ (paddle-x p) PADDLE-SPEED))
      (check-bounds(- (paddle-x p) PADDLE-SPEED))))

;; update-x-ball: Ball KeyEvent -> Number
;; changes the x coord of the ball based on key pressed
(check-expect (update-x-ball (make-ball 105 0 0 0) "right") 115)
(check-expect (update-x-ball (make-ball 105 0 0 0) "left") 95)

(define (update-x-ball b d)
  (if (key=? d "right")
      (check-bounds (+ (ball-x b) PADDLE-SPEED))
      (check-bounds(- (ball-x b) PADDLE-SPEED))))

;; check-bounds: Number -> Number
;; keeps the x coord in the bounds of the window
(check-expect (check-bounds 180) 180)
(check-expect (check-bounds 20) 20)
(check-expect (check-bounds 50) 50)
(define (check-bounds n)
  (cond [(>= n 180) 180]
        [(<= n 20) 20]
        [else n]))


;; end?: World -> Boolean
;; checks if there are any more bricks or if the ball is off the screen
(check-expect (end? w0) #true)
(check-expect (end? w1) #false)
(check-expect (end? w2) #false)
(check-expect (end? w3) #false)
(check-expect (end? w4) #true)

(define (end? w)
  (cond [(= 0 (length (world-lob w))) #true]
        [(= (world-lives w) 0) #true]
        [else #false]))

;; show-end: World -> Image
;; shows the end scene depending on whether you win or lose
(check-expect (show-end w0) WIN)
(check-expect (show-end w4) LOSE)

(define (show-end w)
  (cond [(= (length (world-lob w)) 0)
         WIN]
        [else LOSE]))

;; World -> World
;; launches the breakout game
(define (main w)
  (big-bang w
    [to-draw draw-world]
    [on-tick move-world]
    [on-key change-dir]
    [stop-when end? show-end]))

(main w1)
