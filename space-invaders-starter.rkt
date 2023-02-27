;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

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
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))




(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

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


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvader is one of:
;;   - empty
;;   - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI-1 empty)
(define LOI-2 (cons I1 empty))
(define LOI-3 (list I1 I2))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
                   (fn-for-loi (rest loi)))])) 



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;;ListOfMissile is one of:
;;    -empty
;;    -(cons Missile ListOfMissile
;;    interp. a list of missiles

(define LOM-1 empty)
(define LOM-2 (list M1))
(define LOM-3 (list M1 M2))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (first lom)
                   (fn-for-lom (rest lom)))]))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ===========FUNCTIONS=================

;; ==========WORLD FUNCTION===========

;; Game -> Game
;; start the world with (main empty)
;; 
(define (main g)
  (big-bang g                   ; Game
    (on-tick   update)     ; Game -> Game
    (to-draw   render)   ; Game -> Image
    (stop-when invaders-at-bottom?)      ; Game -> Boolean
    (on-key    handle-key)))    ; Game KeyEvent -> Game

;; ==========VIEW FUNCTIONS============

;; Game -> Image
;; update position of player, list of enemmies and missiles on screen
;; !!!
(check-expect (render G0)
              (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render G2)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image MISSILE (missile-x M1) (missile-y M1)
                                        (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))
               
;(define (render g) BACKGROUND) stub

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define (render g)
  (draw-tank (game-tank g)
             (draw-missiles (game-missiles g)
                            (draw-invaders (game-invaders g)))))

;; ListOfInvader -> Image
;; draw an arbitrary list of invaders on screen
;;
(check-expect (draw-invaders LOI-1) BACKGROUND)
(check-expect (draw-invaders LOI-3)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND)))

;(define (draw-invaders loi img) BACKGROUND)

(define (draw-invaders loi)
  (cond [(empty? loi) BACKGROUND]
        [else
         (draw-invader (first loi)
                       (draw-invaders (rest loi)))]))
  

;;Invader Image -> Image
;; draws an invader at its x and y position on an image
(check-expect (draw-invader I2 BACKGROUND)
              (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND))

;(define (draw-invader i img) BACKGROUND)

(define (draw-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


;;ListOfMissile Image-> Image
;; renders an arbitrary number of missiles onto an image

(check-expect (draw-missiles LOM-1 BACKGROUND) BACKGROUND)
(check-expect (draw-missiles LOM-2 BACKGROUND)
              (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-expect (draw-missiles LOM-3 BACKGROUND)
              (place-image MISSILE (missile-x M1) (missile-y M1)
                           (place-image MISSILE (missile-x M2) (missile-y M2) BACKGROUND)))

;(define (draw-missiles lom) BACKGROUND) stub

(define (draw-missiles lom img)
  (cond [(empty? lom) img]
        [else (draw-missile (first lom)
                            (draw-missiles (rest lom) img))]))

;; Missile Image -> Image
;; draws a missile at position x,y on image
(check-expect (draw-missile M1 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))

;(define (draw-missile m img) BACKGROUND) stub
;<template from Missile

(define (draw-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))

;; Tank Image -> Image
;; renders the tank at position x, TANK-Y on image
(check-expect (draw-tank T1 BACKGROUND) (place-image TANK 50 TANK-Y BACKGROUND))

; (define (draw-tank t img) BACKGROUND)

(define (draw-tank t img)
  (place-image TANK (tank-x t) TANK-Y img))

;; ==============GAME LOGIC FUNCTIONS================

;; Game -> Game
;; update game state for player, list of invaders and list of missiles
;; tank - move left or right
;; missile - change position, remove if off screen, remove it hit invader
;; invader - change position, remove if hit by missile, generate new invader
(check-expect (update G1)
              (make-game empty empty (make-tank (+ TANK-SPEED 50) 1)))
              
(define (update g) g)

#;
(define (update g)
  (make-game (advance-invaders (add-invader (filter-invaders (game-invaders g) (game-missiles g))))
       (advance-missiles (filter-missiles (game-missiles g) (game-invaders g))))
       (move-tank (game-tank g)))

;; ListOfInvader -> ListOfInvader
;; move all invaders in list by moving them toward bottom of screen
(check-expect (advance-invaders LOI-1) empty)
(check-expect (advance-invaders LOI-2) (list (make-invader (+ INVADER-X-SPEED (invader-x I1))
                                                           (+ INVADER-Y-SPEED (invader-y I1))
                                                           (invader-dx I1))))
                                                           
;(define (advance-invaders loi) loi)


(define (advance-invaders loi)
  (cond [(empty? loi) loi]
        [else (cons (advance-invader (first loi))
                   (advance-invaders (rest loi)))]))


;; Invader -> Invader
;; update invader x and y position by INVADER-X-SPEED and INVADER-Y-SPEED
;; if invader hits side then reverse direction
;; !!!
(check-expect (advance-invader (make-invader 150 100 12))
              (make-invader (+ INVADER-X-SPEED 150) (+ INVADER-Y-SPEED 100) 12))
(check-expect (advance-invader (make-invader 150 100 -12))
              (make-invader (- 150 INVADER-X-SPEED) (+ INVADER-Y-SPEED 100) -12))
(check-expect (advance-invader (make-invader WIDTH 100 12))
              (make-invader (- WIDTH INVADER-X-SPEED) (+ INVADER-Y-SPEED 100) -12))
(check-expect (advance-invader (make-invader 0 100 -12))
              (make-invader (+ 0 INVADER-X-SPEED) (+ INVADER-Y-SPEED 100) 12))
                               
                              
;(define (advance-invader i) i)

(define (advance-invader i)
  (cond [(>= (invader-x i) WIDTH)
         ((- WIDTH INVAD


;; ListOfInvader -> ListOfInvader
;; add invader randomly at x coordinate
;; !!!

(define (add-invader loi) loi)


;; ListOfInvader ListOfMissile -> ListOfInvader
;; filters out invaders that have not been hit by missiles
;; !!!

(define (filter-invaders loi lom) loi)


;; ListOfMissile -> ListOfMissile
;; updates all missiles in list by moving them up the screen
;; !!!

(define (update-missiles lom) lom)


;; ListOfMissile ListOfInvader -> ListOfMissile
;; filters list of missiles to include only those that haven't hit invaders
;; !!!

(define (filter-missiles lom loi) lom)

;; Tank -> Tank
;; updates tank position by TANK-SPEED, if tank dir == 1 -> to right, if tank dir = -1 to left

(define (move-tank t) t)


;;==========END CHECK FUNCTIONS===============

;; Game -> Boolean
;; checks to see if game is over because invader has reached bottom
;;!!!
(check-expect (game-over? G2) false)
(check-expect (game-over? G3) true)

;(define (game-over? g) false) ;stub

;template from Game

(define (game-over? g)
  (if (invaders-at-bottom? (game-invaders g))
      true
      false))


;; ListOfInvaders -> Boolean
;; checks to see if list of invaders has reached the bottom
(check-expect (invaders-at-bottom? LOI-1) false)
(check-expect (invaders-at-bottom? LOI-2) false)
(check-expect (invaders-at-bottom? LOI-3) true)

;(define (invaders-at-bottom? loi) false) stub
; <template from ListOfInvader>
  
(define (invaders-at-bottom? loi)
  (cond [(empty? loi) false]
        [else (if (invader-at-bottom? (first loi))
                  true
                  (invaders-at-bottom? (rest loi)))]))

;;Invader -> Boolean
;; checks to see if one invader's Y-position has reached HEIGHT (the bottom of the screen)
(check-expect (invader-at-bottom? I1) false)
(check-expect (invader-at-bottom? I3) true)

;(define (invader-at-bottom? i) false)
;<template from Invader>

(define (invader-at-bottom? i)
  (>= (invader-y i) HEIGHT))

;; ==================INPUT FUNCTIONS=======================

;; Game KeyEvent -> Game
;; moves player left when pushing left, moves player right when pushing right, fires missile with Space
(define (handle-key g ke) G0)