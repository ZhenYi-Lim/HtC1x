;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE-MAX 100)
(define INVADE-RATE 10)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game g)
  (... (fn-for-loi (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
(define T3 (make-tank 0 -1))            ;at left edge
(define T4 (make-tank WIDTH 1))         ;at right edge

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader (- WIDTH 5) 100 12))   ;near the right edge moving right, close to bounce.
(define I5 (make-invader 5 100 -12))            ;near the left edge moving left, close to bounce.
(define I6 (make-invader (/ WIDTH 2) 0 0))

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvaders is one of:
;;  - empty
;;  - (cons Invader ListOfInvaders)
;; interp. a list of invaders
(define LOI1 empty)
(define LOI2 (cons I1 (cons I2 (cons I3 empty))))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                              ;not hit U1
(define M2 (make-missile (invader-x I1) (invader-y I1)))        ;direct hit on U1
(define M3 (make-missile (+ (invader-x I1) 5) (invader-y I1)))  ;right glancing hit on U1
(define M4 (make-missile 150 -2))                               ;off screen
(define M5 (make-missile (- (invader-x I1) 5) (invader-y I1)))  ;left glancing hit on U1
(define M6 (make-missile (/ WIDTH 2) (- HEIGHT (* 2 TANK-HEIGHT/2))))

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissiles is one of:
;;  - empty
;;  - (cons Missile ListOfMissiles)
;; interp. a list of missiles
(define LOM1 empty)
(define LOM2 (cons M1 (cons M2 (cons M3 empty))))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list I1 I3) (list M1 M2) T1))
(define G6 (make-game (list I6) (list M6) T0))

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                 ; Game
    (on-tick   advance-game)  ; Game -> Game
    (to-draw   render-game)   ; Game -> Image
    (on-key    handle-key)    ; Game KeyEvent -> Game
    (stop-when last-world?))) ; Game -> Boolean

;; Game -> Game
;; produce the next game state.
;(define (advance-g G0) G0) ;stub

;<use template from Balloon>

(define (advance-game g)
  (make-game (create-invaders (advance-invaders (destroy-invaders (game-missiles g) (game-invaders g))))
             (filter-missiles (advance-missiles (destroy-missiles (game-invaders g) (game-missiles g))))
             (advance-tank (game-tank g))))

;; ListOfMissiles ListOfInvaders -> ListOfInvaders
;; produce list of invaders sans the ones hit by the missiles
(check-expect (destroy-invaders (list M1) empty) empty)
(check-expect (destroy-invaders (list M1) (list I1 I2)) (list I1 I2)) ;no hits
(check-expect (destroy-invaders (list M1 M2) (list I1 I2)) (list I2)) ;M2 destroys I1
(check-expect (destroy-invaders (list M1 M2) (list I2 I1)) (list I2)) ;M2 destroys I1
;(define (destroy-invaders lom loi) loi) ;stub
(define (destroy-invaders lom loi)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (destroy-invaders (rest lom) (check-missile (first lom) loi))]))

;; Missile ListOfInvaders -> ListOfInvaders
;; produce list of invaders sans the ones hit by the missile
(check-expect (check-missile M1 empty) empty)               ;no invaders
(check-expect (check-missile M1 (list I1 I2)) (list I1 I2)) ;no hits detected
(check-expect (check-missile M2 (list I1 I2 I1)) (list I2)) ;hit both instances of I1
;(define (check-missile m loi) loi) ;stub
(define (check-missile m loi)
  (cond [(empty? loi) empty]
        [else
         (if (hit-invader? m (first loi))
             (check-missile m (rest loi))
             (cons (first loi) (check-missile m (rest loi))))]))

;; Missile Invader -> Boolean
;; check if missile hit the invader.
(check-expect (hit-invader? M1 I1) false) ;no hits detected
(check-expect (hit-invader? M2 I1) true)  ;direct hit on I1
(check-expect (hit-invader? M3 I1) true)  ;right glancing hit on I1
(check-expect (hit-invader? M5 I1) true)  ;left glancing hit on I1
;(define hit-invader? m i) false) ;stub
(define (hit-invader? m i)
  (and (<= (missile-x m) (+ (invader-x i) HIT-RANGE))
       (>= (missile-x m) (- (invader-x i) HIT-RANGE))
       (<= (missile-y m) (+ (invader-y i) HIT-RANGE))
       (>= (missile-y m) (- (invader-y i) HIT-RANGE))))

;; ListOfInvaders -> ListOfInvaders
;; produce new invaders at the top of the window.
;(define (create-invaders loi) loi) ;stub
(define (create-invaders loi)
  (if (< (random INVADE-RATE-MAX) INVADE-RATE)
      (cons (make-invader (random WIDTH) 0 (* (- (random 3) 1) INVADER-X-SPEED)) loi) ;create invader
      loi))

;; ListOfInvaders -> ListOfInvaders
;; advance invaders.
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list I1))
              (list (make-invader (+ (invader-dx I1) (invader-x I1))
                                  (+ INVADER-Y-SPEED (invader-y I1))
                                  (invader-dx I1))))
;(define (advance-invaders loi) (list I1 I2)) ;stub
(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader (first loi))
               (advance-invaders (rest loi)))]))

;; Invader -> Invader
;; advance invader.
(check-expect (advance-invader I1) (make-invader (+ (invader-dx I1) (invader-x I1))
                                                 (+ INVADER-Y-SPEED (invader-y I1))
                                                 (invader-dx I1)))
(check-expect (advance-invader I4) (make-invader (- WIDTH (- 12 5))
                                                 (+ INVADER-Y-SPEED 100)
                                                 -12))
(check-expect (advance-invader I5) (make-invader (- (+ 5 -12))
                                                 (+ INVADER-Y-SPEED 100)
                                                 12))
;(define (advance-invader i) I1) ;stub
(define (advance-invader i)
  (cond [(> (+ (invader-dx i) (invader-x i)) WIDTH) ;bounce, right
         (make-invader (- WIDTH (- (+ (invader-dx i) (invader-x i)) WIDTH))
                       (+ INVADER-Y-SPEED (invader-y i))
                       (- (invader-dx i)))]
        [(< (+ (invader-dx i) (invader-x i)) 0)     ;bounce, left
         (make-invader (- (+ (invader-dx i) (invader-x i)))
                       (+ INVADER-Y-SPEED (invader-y i))
                       (- (invader-dx i)))]
        [else                                       ;normal movement
         (make-invader (+ (invader-dx i) (invader-x i))
                       (+ INVADER-Y-SPEED (invader-y i))
                       (invader-dx i))]))

;; ListOfMissiles -> ListOfMissiles
;; remove missiles that have gone off-screen.
(check-expect (filter-missiles empty) empty)
(check-expect (filter-missiles (list M4 M1 M4 M2 M3 M4)) (list M1 M2 M3))
;(define (filter-missiles lom) (list M1)) ;stub
(define (filter-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (> (missile-y (first lom)) 0)
             (cons (first lom)
                   (filter-missiles (rest lom)))
             (filter-missiles (rest lom)))]))

;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; produce list of missiles sans those that destroyed invaders.
(check-expect (destroy-missiles (list I2) empty) empty)
(check-expect (destroy-missiles (list I2) (list M1 M2)) (list M1 M2)) ;no hits
(check-expect (destroy-missiles (list I1 I2) (list M1 M2)) (list M1)) ;M2 exploded on I1
(check-expect (destroy-missiles (list I1 I2) (list M2 M1)) (list M1)) ;M2 exploded on I1
;(define (destroy-missiles loi lom) lom) ;stub
(define (destroy-missiles loi lom)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (destroy-missiles (rest loi) (check-invader (first loi) lom))]))

;; Invader ListOfMissiles -> ListOfMissiles
;; produce list of missiles sans the ones that exploded on invaders
(check-expect (check-invader I2 empty) empty)
(check-expect (check-invader I2 (list M1 M2)) (list M1 M2)) ;no hits detected
(check-expect (check-invader I1 (list M2 M1 M2)) (list M1)) ;both instances of M2 exploded on the invader
;(define (check-invader i lom) lom) ;stub
(define (check-invader i lom)
  (cond [(empty? lom) empty]
        [else
         (if (hit-invader? (first lom) i)
             (check-invader i (rest lom))
             (cons (first lom) (check-invader i (rest lom))))]))

;; ListOfMissiles -> ListOfMissiles
;; advance missiles.
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list M1 M2 M3))
              (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))
                    (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED))
                    (make-missile (missile-x M3) (- (missile-y M3) MISSILE-SPEED))))
;(define (advance-missiles lom) (list M1)) ;stub
(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
               (advance-missiles (rest lom)))]))

;; Tank -> Tank
;; advance tank.
(check-expect (advance-tank T1) (make-tank (+ (tank-dir T1) (tank-x T1)) (tank-dir T1)))
(check-expect (advance-tank T2) (make-tank (+ (tank-dir T2) (tank-x T2)) (tank-dir T2)))
(check-expect (advance-tank T3) (make-tank 0 (tank-dir T3)))
(check-expect (advance-tank T4) (make-tank WIDTH (tank-dir T4)))
;(define (advance-tank t) t) ;stub
(define (advance-tank t)
  (cond [(< (+ (tank-dir t) (tank-x t)) 0) (make-tank 0 (tank-dir t))]
        [(> (+ (tank-dir t) (tank-x t)) WIDTH) (make-tank WIDTH (tank-dir t))]
        [else (make-tank (+ (tank-dir t) (tank-x t)) (tank-dir t))]))

;; Game -> Image
;; render the game state.
(check-expect (render-game G0) (place-image TANK
                                            (tank-x T0)
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
(check-expect (render-game G2) (place-image TANK
                                            (tank-x T1)
                                            (- HEIGHT TANK-HEIGHT/2)
                                            (place-image MISSILE
                                                         (missile-x M1)
                                                         (missile-y M1)
                                                         (place-image INVADER
                                                                      (invader-x I1)
                                                                      (invader-y I1)
                                                                      BACKGROUND))))
;(define (render-game g) BACKGROUND) ;stub
(define (render-game g)
  (render-tank (game-tank g)
               (render-missiles (game-missiles g)
                                (render-invaders (game-invaders g)
                                                 BACKGROUND))))

;; Tank Image -> Image
;; render image of tank at appropriate position.
(check-expect (render-tank T0 BACKGROUND) (place-image TANK
                                            (tank-x T0)
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
;(define (render-tank t) BACKGROUND) ;stub
(define (render-tank t img)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               img))

;; ListOfMissiles Image -> Image
;; render missiles at appropriate positions.
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list M1) BACKGROUND) (place-image MISSILE
                                                                  (missile-x M1)
                                                                  (missile-y M1)
                                                                  BACKGROUND))
;(define (render-missiles lom img) BACKGROUND) ;stub
(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) img))]))

;; ListOfInvaders Image -> Image
;; render invaders at appropriate positions.
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list I1) BACKGROUND) (place-image INVADER
                                                                  (invader-x I1)
                                                                  (invader-y I1)
                                                                  BACKGROUND))
;(define (render-invaders loi img) BACKGROUND) ;stub
(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) img))]))

;; Game KeyEvent -> Game
;; change moving direction of the tank when the left or right arrow keys are pressed.
;; creates new missile immediately in front of tank when the spacebar is pressed.
(check-expect (handle-key G0 "a") G0)
(check-expect (handle-key G0 "left") (make-game (game-invaders G0)
                                                (game-missiles G0)
                                                (make-tank (tank-x (game-tank G0)) -1)))
(check-expect (handle-key G0 "right") (make-game (game-invaders G0)
                                                 (game-missiles G0)
                                                 (make-tank (tank-x (game-tank G0)) 1)))
(check-expect (handle-key G0 " ") (make-game (game-invaders G0)
                                             (cons (make-missile (tank-x (game-tank G0))
                                                                 (- HEIGHT (* 2 TANK-HEIGHT/2)))
                                                   (game-missiles G0))
                                             (game-tank G0)))
;(define (handle-key g ke) G0) ;stub
(define (handle-key g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g)
                                      (game-missiles g)
                                      (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g)
                                       (game-missiles g)
                                       (make-tank (tank-x (game-tank g)) 1))]
        [(key=? ke " ") (make-game (game-invaders g)
                                   (cons (make-missile (tank-x (game-tank g))
                                                       (- HEIGHT (* 2 TANK-HEIGHT/2)))
                                         (game-missiles g))
                                   (game-tank g))]
        [else g]))

;; Game -> Boolean
;; produce true if game should end (an invader has reached the bottom of the world).
(check-expect (last-world? G0) false)
(check-expect (last-world? G2) false)
(check-expect (last-world? G3) true)
(check-expect (last-world? G4) true)
;(define (last-world? g) false) ;stub
(define (last-world? g)
  (not (not-landed? (game-invaders g))))

;; ListOfInvaders -> Boolean
;; produce false if an invader has reached the bottom of the world.
(check-expect (not-landed? (list I1)) true)
(check-expect (not-landed? (list I1 I2)) false)
(check-expect (not-landed? (list I1 I3)) false)
;(define (not-landed? loi) false) ;stub
(define (not-landed? loi)
  (cond [(empty? loi) true]
        [else
         (and (< (invader-y (first loi)) HEIGHT)
              (not-landed? (rest loi)))]))