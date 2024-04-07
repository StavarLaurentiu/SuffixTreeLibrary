#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (longest-common-prefix-helper '() w1 w2)
)

(define (longest-common-prefix-helper prefix w1 w2)
  (if (or (null? w1) (null? w2) (not (eq? (car w1) (car w2))))
      (list (reverse prefix) w1 w2)
      (longest-common-prefix-helper (cons (car w1) prefix) (cdr w1) (cdr w2))
      )
)


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (longest-common-prefix-of-collection-helper (collection-first words) (collection-rest words))
)

(define (longest-common-prefix-of-collection-helper prefix words)
  (if (or (collection-empty? words) (null? prefix))
      prefix
      (longest-common-prefix-of-collection-helper (car (longest-common-prefix prefix (collection-first words))) (collection-rest words))
      )
)


(define (match-pattern-with-label st pattern)
  (let ((branch-matched (get-ch-branch st (car pattern))))
    (cond
      ((eq? branch-matched #f) (list #f '())) ; Nu exista niciun branch care sa inceapa cu aceeasi litera ca pattern-ul
      (else (match-evaluation branch-matched pattern)) ; Am gasit un branch care incepe cu aceeasi litera ca pattern-ul
                                                         ; si incepem cautarea in acest branch
      )
    )
  )

(define (match-evaluation branch pattern)
  (let* ((longest-prefix  (car (longest-common-prefix (get-branch-label branch) pattern))))
    (if (equal? longest-prefix pattern) ; Daca pattern-ul este inclus complet in label
        #t
        (if (< (length longest-prefix) (length (get-branch-label branch))) ; Daca pattern-ul nu se potriveste cu label-ul
            (list #f longest-prefix)                                        ; ex: pattern = "nao", label = "nana"
            (list (get-branch-label branch) (caddr (longest-common-prefix (get-branch-label branch) pattern)) (get-branch-subtree branch)) ; Avem un partial match
            )                                                                                                                                 ; ex: pattern = "nana", label = "na"
        )
    )
  )


(define (st-has-pattern? st pattern)
  (let* ((match-result (match-pattern-with-label st pattern)))
    (cond
      ((st-empty? st) #f) ; Contine doar o parte din pattern
      ((equal? match-result #t) #t) ; Contine tot pattern-ul
      ((and (list? match-result) (not (equal? (car match-result) #f))) ; Pattern-ul se potriveste cu eticheta dar nu este continut in ea
       (st-has-pattern? (caddr match-result) (cadr match-result)))
      (else #f) ; Pattern-ul nu a fost gasit
      )
    )
  )


(define (get-suffixes text)
  (if (null? text)
      '()
      (collection-cons text (get-suffixes (cdr text)))
  )
)


(define (get-ch-words words ch)
 (collection-filter (lambda (word) (if (null? word) #f (eq? ch (car word)))) words)
)


(define (ast-func suffixes)
  (cons (list (car (collection-first suffixes))) (collection-map cdr suffixes))
)

(define (cst-func suffixes)
  (let ((common-prefix (longest-common-prefix-of-collection suffixes)))
        (cons common-prefix
          (collection-map (lambda (suffix)
                 (caddr (longest-common-prefix common-prefix suffix)))
           suffixes
           ))
   )
)

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)

(define (suffixes->st labeling-func suffixes alphabet)
  ; PASUL 2 -> Gaseste lista de liste de sufixe in functie de prima litera si stege listele vide
  (let* ((grouped-suffixes-not-filtered (collection-map (lambda (ch) (get-ch-words suffixes ch)) alphabet)))
    (let* ((grouped-suffixes (collection-filter (lambda (list) (not (collection-empty? list))) grouped-suffixes-not-filtered)))
      ; Apeleaza funtia pentru pasul 3 pe lista calculata
      (suffixes->st-helper labeling-func grouped-suffixes alphabet)
    )
  )
)

; PASUL 3 -> Calculeaza fiecare ramura din arbore, ca pereche
; intre eticheta la pasul curent si subarborele ramas pe care se aplica recursiv functia
(define (suffixes->st-helper labeling-func grouped-suffixes alphabet)
  (cond
    ((collection-empty? grouped-suffixes) (empty-collection))
    (else (collection-map (lambda (new-grouped-suffixes) (let ((grouped-suffixes-labeled (labeling-func new-grouped-suffixes)))
                 (cons (car grouped-suffixes-labeled)
                      (suffixes->st labeling-func (get-branch-subtree grouped-suffixes-labeled) alphabet))))
                        grouped-suffixes))
  )
)


; nu uitați să convertiți alfabetul într-un flux

;; AM LUAT ACEASTA FUNCTIE DIN CHECKER PENTRU A TRANSFORMA O LISTA(ALFABETUL) INTR-UN STREAM
(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

(define text->st 
  (lambda (labeling-func) 
    (lambda (text) 
      ; Construieste sufixele si alfabetul
      (let* ((suffixes (get-suffixes (append text '(#\$))))
             (alphabet (sort (remove-duplicates (append text '(#\$))) char<?)))
        ; Apeleaza functia deja scrisa pentru sufixele si alfabetul create
        (suffixes->st labeling-func suffixes (list->stream alphabet))))))


(define (text->ast text)
  ((text->st ast-func) text)
)


(define (text->cst text)
  ((text->st cst-func) text)
)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let ((st ((text->st ast-func) text) ))
     (st-has-pattern? st pattern)                                    
  )
)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.

;(define (repeated-substring-of-given-length text len)
 ; '(Am implementarea comentata in cod pentru ca mi se ruleaza pe greu, am incercat sa o optimizez dar chiar nu inteleg de ce se ruleaza atat de greu)
;)

(define (repeated-substring-of-given-length text len)
  (let search-in-all-branches ((st (text->cst text)) (current-length 0) (possible-substring '())) ; Loop pentru a trece prin toate branch-urile
    (if (st-empty? st) ; Daca am parcurs toate branch-urile returnez false
        #f
        (let ((result (let* ((current-branch (first-branch st))) ; Altfel analizam branch-ul curent printr-o functie auxiliara
                (branch-analysis search-in-all-branches (get-branch-label current-branch)
                                 (get-branch-subtree current-branch) len current-length possible-substring))))
            (if (equal? result #f) ; Daca rezultatul analizei este #f cautam in restul branch-urilor
                (search-in-all-branches (other-branches st) current-length possible-substring)
                result ; Altfel returnez rezultatul obtinut
            )
         )
      )
   )
)

(define (branch-analysis search-in-all-branches current-label current-subtree len current-length possible-substring)
 (if (st-empty? current-subtree) ; Daca este un node terminal returnez #f
    #f
    (let ((current-label-length (length current-label)))
      (if (< (+ current-label-length current-length) len) ; Daca substring-ul NU ARE inca lungimea cautata continui cautarea
          (search-in-all-branches current-subtree (+ current-length current-label-length) (append possible-substring current-label))
          (let ((substring (append possible-substring current-label))) (take substring len)) ; Daca substring-ul ARE lungimea dorita il returnez
      )
    )
 )
)