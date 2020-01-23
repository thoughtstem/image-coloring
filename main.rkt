#lang racket

;Note: This is aBlender's code from game-enine, (finally!) pulled out into its own package (Sep 29, 2019)
; I mostly just changed the names and added the sample image

(provide ; ==== IMAGE FUNCTIONS ====
         (rename-out [change-img-hue change-hue])     ; 0 to 360
         (rename-out [change-img-sat change-saturation])     ; 0 to 100
         (rename-out [change-img-bright change-brightness])  ; 0 to 100
         (rename-out [change-img-alpha change-alpha]) ; -255 to 255
 
         change-img-hue
         change-img-sat
         change-img-bright
         change-img-alpha

         (rename-out [set-img-hue set-hue])
         (rename-out [set-img-sat set-saturation])
         (rename-out [set-img-bright set-brightness])
         (rename-out [set-img-alpha set-alpha])

         set-img-hue        ; 0 to 360
         set-img-sat        ; 0 to 100
         set-img-bright     ; 0 to 100
         set-img-alpha      ; 0 to 255

         (rename-out [ tint-img tint-image])
         (rename-out [ set-img-color set-image-color])
         
         tint-img 
         mask
         mask-pixel
         scale-to-fit
         iconify-img
         has-color?
         set-img-color

         ; ==== COLOR FUNCTIONS ====
         

         (rename-out [change-hue change-color-hue])
         (rename-out [change-sat change-color-saturation])
         (rename-out [change-bright change-color-brightness])
         (rename-out [change-alpha change-color-alpha])

         (rename-out [set-hue set-color-hue])
         (rename-out [set-sat set-color-saturation])
         (rename-out [set-bright set-color-brightness])
         (rename-out [set-alpha set-color-alpha])

         name->color
         name->color-hsb
         name->hue
         name->sat

         color->color-hsb
         rgb->hue
         rgb->sat
         rgb->bright
         make-color-hue
         
         (struct-out color-hsb)
         make-color-hsb

         hsb->color

         hex->color
         color->hex-string
         (rename-out [color->hex-string color->hex]))

(require 2htdp/image)

(define (name->color string)
  (first (image->color-list (square 1 "solid" string))))

(define (name->color-hsb string)
  (color->color-hsb (name->color string)))

(define (name->hue string)
  (define c (name->color string))
  (rgb->hue (color-red c) (color-green c) (color-blue c)))

(define (name->sat string)
  (define c (name->color string))
  (color-hsb-sat (color->color-hsb c)))

(define (mask-pixel color1 color2)
    (if (eq? (color-alpha color1) 0)
        (make-color 0 0 0 0)
         color2))

(define (mask image1 image2)
  (define image1-list (image->color-list image1))
  (define image2-list (image->color-list image2))
  (color-list->bitmap (map mask-pixel image1-list image2-list) (image-width image1) (image-height image1)))


(define (tint-img color img)
  (define c (name->color color))
  (define tint-color (make-color (color-red c) (color-green c) (color-blue c) 128))
  ;(displayln tint-color)
  (define tinted-image (overlay (rectangle (image-width img) (image-height img) "solid" tint-color) img))
  (mask img tinted-image))

(provide (rename-out (make-color-hue color-from-hue)))

#|(define/contract (make-color-hue hue [a 255])
  (->* (number?) (number?) color?)
  (cond
    [(< hue 42.5)  (make-color 255
                               (* hue 6)
                               0
                               a)]
    [(< hue 85)    (make-color (- 255 (exact-round (* (- hue 42.5) 6)))
                               255
                               0
                               a)]
    [(< hue 127.5) (make-color 0
                               255
                               (exact-round (* (- hue 85) 6))
                               a)]
    [(< hue 170)   (make-color 0
                               (- 255 (exact-round (* (- hue 127.5) 6)))
                               255
                               a)]
    [(< hue 212.5) (make-color (exact-round (* (- hue 170) 6))
                               0
                               255
                               a)]
    [else          (make-color 255
                               0
                               (- 255 (exact-round (* (- hue 212.5) 6)))
                               a)]))|#

(define/contract (make-color-hue hue [a 255])
  (->* (number?) (number?) color?)
  (cond
    [(< hue 42.5)  (make-color 255
                               (* hue 6)
                               0
                               a)]
    [(< hue 85)    (make-color (- 255 (exact-round (* (- hue 42.5) 6)))
                               255
                               0
                               a)]
    [(< hue 127.5) (make-color 0
                               255
                               (exact-round (* (- hue 85) 6))
                               a)]
    [(< hue 170)   (make-color 0
                               (- 255 (exact-round (* (- hue 127.5) 6)))
                               255
                               a)]
    [(< hue 212.5) (make-color (exact-round (* (- hue 170) 6))
                               0
                               255
                               a)]
    [else          (make-color 255
                               0
                               (- 255 (exact-round (* (- hue 212.5) 6)))
                               a)]))

(define/contract (make-color-hue-equal-brightness hue [a 255])
  (->* (number?) (number?) color?)
  (cond
    [(< hue 85)  (make-color (- 255 (* hue 3))
                             (* hue 3)
                              0
                              a)]
    [(< hue 170) (make-color  0
                              (- 255 (* (- hue 85) 3))
                              (* (- hue 85) 3)
                              a)]
    [else        (make-color  (* (- hue 170) 3)
                              0
                              (- 255 (* (- hue 170) 3))
                              a)]))

(struct color-hsb
  (hue sat bright alpha))

(define (make-color-hsb hue [sat 100] [bright 100] [alpha 255])
  (color-hsb hue sat bright alpha))

(define (color->color-hsb c)
  (define r (/ (color-red c) 255))
  (define g (/ (color-green c) 255))
  (define b (/ (color-blue c) 255))
  (define a (color-alpha c))
  (define mx (max r g b))
  (define mn (min r g b))
  (define d (- mx mn))
  (define hue (cond
                [(= mx mn) (define h 0)
                           (exact-round (* h 60))]
                [(= mx r)  (define h (+ (/ (- g b) d) (if (< g b) 6 0)))
                           (exact-round (* h 60))]
                [(= mx g)  (define h (+ (/ (- b r) d) 2))
                           (exact-round (* h 60))]
                [(= mx b)  (define h (+ (/ (- r g) d) 4))
                           (exact-round (* h 60))]))
  (define sat (if (= mx 0)
                  0
                  (exact-round (* 100 (/ (- mx mn) mx)))))
  (define bright (exact-round (* (max r g b) 100)))
  (make-color-hsb hue sat bright a))

(define (rgb->hue red green blue)
  (define r (/ red 255))
  (define g (/ green 255))
  (define b (/ blue 255))
  (define mx (max r g b))
  (define mn (min r g b))
  (define d (- mx mn))
  (cond
    [(= mx mn) (define h 0)
                 (exact-round (* h 60))]
    [(= mx r)   (define h (+ (/ (- g b) d) (if (< g b) 6 0)))
                 (exact-round (* h 60))]
    [(= mx g)   (define h (+ (/ (- b r) d) 2))
                 (exact-round (* h 60))]
    [(= mx b)   (define h (+ (/ (- r g) d) 4))
                 (exact-round (* h 60))]))


(define (rgb->sat r g b)
  (define mx (max r g b))
  (define mn (min r g b))
  (if (= mx 0)
      0
      (exact-round (* 100 (/ (- mx mn) mx)))))

(define (rgb->bright r g b)
  (exact-round (* (/ (max r g b) 255) 100)))

(define (hsb->color c)
  (define h (/ (color-hsb-hue c) 360))
  (define s (/ (color-hsb-sat c) 100))
  (define b (/ (color-hsb-bright c) 100))
  (define a (color-hsb-alpha c))
  (define i (exact-floor (* h 6)))
  (define f (- (* h 6) i))
  (define p (* b (- 1 s)))
  (define q (* b (- 1 (* f s))))
  (define t (* b (- 1 (* (- 1 f) s))))
  (define B (exact-floor (* b 255)))
  (define P (exact-round (* p 255)))
  (define Q (exact-round (* q 255)))
  (define T (exact-round (* t 255)))
  (define case (remainder i 6))
  (cond
    [(= case 0) (make-color B T P a)]
    [(= case 1) (make-color Q B P a)]
    [(= case 2) (make-color P B T a)]
    [(= case 3) (make-color P Q B a)]
    [(= case 4) (make-color T P B a)]
    [(= case 5) (make-color B P Q a)]))

; ===== PIXEL CHANGERS =====
(define (change-hue amount c)
  ;(define hsb-c (color->color-hsb c))
  ;(define new-hue (modulo (+ (color-hsb-hue hsb-c) amount) 360))
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [hue (modulo (+ (color-hsb-hue hsb-c) amount) 360)])))))

(define (change-sat amount c)
  ;(define hsb-c (color->color-hsb c))
  ;(define new-sat (max 0 (min 100 (+ (color-hsb-sat hsb-c) amount))))
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [sat (max 0 (min 100 (+ (color-hsb-sat hsb-c) amount)))])))))

(define (change-bright amount c)
  ;(define hsb-c (color->color-hsb c))
  ;(define new-bright (max 0 (min 100  (+ (color-hsb-bright hsb-c) amount))))
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [bright (max 0 (min 100  (+ (color-hsb-bright hsb-c) amount)))])))))

(define (change-alpha amount c)
  (if (= (color-alpha c) 0)
      c
      (struct-copy color c
                   [alpha (max 0 (min 255  (+ (color-alpha c) amount)))])))

; ===== PIXEL SETTERS =====
(define (set-hue amount c)
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [hue (modulo amount 360)])))))

(define (set-sat amount c)
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [sat (max 0 (min 100 amount))])))))

(define (set-bright amount c)
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [bright (max 0 (min 100 amount))])))))

(define (set-alpha amount c)
  (if (= (color-alpha c) 0)
      c
      (struct-copy color c
                   [alpha (max 0 (min 255 amount))])))

; ===== IMAGE CHANGERS =====
(define (change-img-hue amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry change-hue amount) image-list) (image-width image) (image-height image)))

(define (change-img-sat amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry change-sat amount) image-list) (image-width image) (image-height image)))

(define (change-img-bright amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry change-bright amount) image-list) (image-width image) (image-height image)))

(define (change-img-alpha amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry change-alpha amount) image-list) (image-width image) (image-height image)))


; ===== IMAGE SETTERS =====
(define (set-img-hue amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry set-hue amount) image-list) (image-width image) (image-height image)))

(define (set-img-sat amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry set-sat amount) image-list) (image-width image) (image-height image)))

(define (set-img-bright amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry set-bright amount) image-list) (image-width image) (image-height image)))

(define (set-img-alpha amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry set-alpha amount) image-list) (image-width image) (image-height image)))


(define/contract (scale-to-fit i w)
  (-> image? number? image?)
  (define longer-side (if (> (image-width i) (image-height i))
                          (image-width i)
                          (image-height i)))
  (scale (/ w longer-side) i))

;turns all non-transparent one color (default, black)
(define (iconify-img img [t-color 'black])

  (define target-color (if (color? t-color)
                           t-color
                           (name->color t-color)))

  (define (maybe-color-pixel original-color)
  (mask-pixel original-color target-color))

  (define original-list (image->color-list img))
  (define final-list (map maybe-color-pixel original-list))
  (color-list->bitmap final-list (image-width img) (image-height img)))

(define (has-color? image [threshold 0.2])
  (define (transparent-pixel? c)
    (= (color-alpha c) 0))
  (define (color-pixel? hsb-color)
    (and (> (color-hsb-sat hsb-color)    50)
         (> (color-hsb-bright hsb-color) 50)))
  
  (define hsb-image-list (map color->color-hsb (filter-not transparent-pixel? (image->color-list image))))
  
  (define color-list (filter color-pixel? hsb-image-list))
  (>= (length color-list) (* (length hsb-image-list) threshold)))

(define (set-img-color color-name image)
  (if (has-color? image)
      (let ([hue (color-hsb-hue (name->color-hsb color-name))])
        (set-img-hue hue image))
      (tint-img color-name image)))


; ===== HEX COLOR CONVERTERS =====

; converts a hex number or string to a color struct
; hex can be either a number or a hex string with 6 or 8 digits
(define (hex->color hex)
  (define hex-string
    (cond [(number? hex) (number->string hex 16)]
          [(string? hex) (string-trim (string-trim hex "#x") "#")]
          [else (error "That was not a number or string!")]))
  (define hex-string-list
    (string->list hex-string))
  
  (define rgba-hex-string-list
    (list (string (first hex-string-list) (second hex-string-list))
          (string (third hex-string-list) (fourth hex-string-list))
          (string (fifth hex-string-list) (sixth hex-string-list))
          (if (>= (length hex-string-list) 8)
              (string (seventh hex-string-list) (eighth hex-string-list))
              "FF")))
  (apply make-color (map (curryr string->number 16) rgba-hex-string-list)))


; converts a color string, symbol, or color struct to a hex string
; set #:alpha? to #t to include alpha channel
(define (color->hex-string color #:alpha? [alpha? #f])
  (define c (cond [(or (string? color)
                       (symbol? color)) (name->color color)]
                  [(color? color) color]
                  [else "That wasn't a string, symbol, or color!"]))
  (define (pad-zero str)
    (~a str #:width 2 #:align 'right #:pad-string "0"))
  (define rgb-string
    (string-append (pad-zero (number->string (color-red c) 16))
                   (pad-zero (number->string (color-green c) 16))
                   (pad-zero (number->string (color-blue c) 16))))
  (if alpha?
      (string-append rgb-string
                     (pad-zero (number->string (color-alpha c) 16)))
      rgb-string))
