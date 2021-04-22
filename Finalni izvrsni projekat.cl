;;Aleksandar Stamenkovic 16874
;;Milan Stankovic 16893


;;----------------------------------------------------------------------------------------
;;Format stanja igre
;;----------------------------------------------------------------------------------------

;(setf stanje '((x x x o)(x o o x)(x o o -)(o o x o)
;               (x x - -)(o o o -)(x o x -)(o - - -)
;               (o o - -)(o x - -)(x o x -)(- - - -)
;               (x o x o)(x o o o)(x o x -)(o x x -)
;               ))


;(setf stanje '((- - - - - -)(x o o o - -)(o x x o o x)(x x x o x -)(o - - - - -)(- - - - - -)
;               (- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)
;               (- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)
;               (- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)
;               (- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)(o - - - - -)(- - - - - -)
;               (- - - - - -)(- - - - - -)(- - - - - -)(- - - - - -)(o o - - - -)(o - - - - -)
;               ))

;;primer-dimenzija za test
;(setf n 4)
;(setf n 6)

;;----------------------------------------------------------------------------------------
;;ucitavanje dimenzije igre sa tastature (FJA ZA TESTIRANJE)
;;----------------------------------------------------------------------------------------


;(defun postavi_nivo ()(setf n (read)))

;;----------------------------------------------------------------------------------------
;;postavljanje dumenzije matrice n * n (FJA ZA TESTIRANJE)
;;----------------------------------------------------------------------------------------


;(setf nxn (* n n))


;;----------------------------------------------------------------------------------------
;;funkcija za generisanje jednog stuba
;;----------------------------------------------------------------------------------------


(defun generisi_stubic (list dim) 
  (
   if(> dim 0) (cons '- (generisi_stubic list (1- dim)))
  )
)

;;potrebno za testiranje
;;(setf stub (generisi_stubic '() n))

;;----------------------------------------------------------------------------------------
;;funkcija za generisanje celog stanja
;;----------------------------------------------------------------------------------------


(defun generisi_stanje (stub lista dim) 
  (
   if(> dim 0) 
      (cons stub (generisi_stanje stub lista (1- dim)))
  )
)

;;potrebno za testiranje
;;(setq stanje (generisi_stanje stub '() nxn))


;;----------------------------------------------------------------------------------------
;;Funkcija za generisanje stanja na osnovu unite vrednosti sa tastature:
;;----------------------------------------------------------------------------------------

;(defun generisi ()
 	 ;(let* ((n (read))) (generisi_stanje (generisi_stubic '() n) '() (* n n))))

(defun generisi (n)
 	 (generisi_stanje (generisi_stubic '() n) '() (* n n)))
;;----------------------------------------------------------------------------------------                         
;;funkcija provere kraja igre
;;----------------------------------------------------------------------------------------

;;pomocna funkcija
(defun provera_stuba (stub) 
  (
   if(null stub) T
    (
     if(equal (car stub) '-) NIL (provera_stuba (cdr stub)))
    )
  )

(defun provera_kraj (lista)
  (
   if(null lista) T
    (
     if (not (provera_stuba (car lista))) NIL (provera_kraj (cdr lista)))
    )
 )
   
;;----------------------------------------------------------------------------------------
;;formula za izracunavanje pozicije stubica {(- (* i dim) (- dim j))} (POMOCNE STVARI)
;;----------------------------------------------------------------------------------------

;;formula za izracunavanje pozicije dodavanja (ukoliko se dofdaje na osnovu
;;formata vrsta-kolona
;; [vrsta] * [dimenzija] - ( [dimenzija] - [kolona] )

;;(defun pozicija_calc(i j dim) (-(* i dim) (- dim j)))

;;----------------------------------------------------------------------------------------
;;funkcija za dodavanje znaka u vec prethodno pronadjeni stubic
;;----------------------------------------------------------------------------------------

(defun ubaci_na_stub(stub znak)
  (if(null stub) '()
    (if(equal '- (car stub)) (cons znak (cdr stub))
      (cons (car stub) (ubaci_na_stub (cdr stub) znak)))))

;;primer test poziva funkcije za dodavanje na stubic (setf stub (ubaci_na_stub stub znak))


;;----------------------------------------------------------------------------------------------------------
;;funkcija za proveru validnosti unetog poteza
;;zbog testa potrebna je globalna promenljiva n da bude postavljena na dimenziju table
;;----------------------------------------------------------------------------------------------------------


(defun proveri_potez(stanje index)
  (if(and (> index 0) (<= index (* n n)))(
   if(> index 1) (proveri_potez(cdr stanje) (1- index))
    (if(equal(member '- (car stanje)) nil) nil t))))



;;----------------------------------------------------------------------------------------------------------
;;funkcija za dodavanje novog poteza (znaka) u celokupno stanje gde ce se najpre pronaci stubic
;;a zatim ce se nad tako pronadjenim stubicem pozvati funkcija iznad za proveru validnosti poteza a
;;zatim za dodavanje na konkretan stubic stubic    
;;Dodavanje se vrsi u formatu broj_stubica
;;----------------------------------------------------------------------------------------------------------


(defun ubaci (stanje index znak)
  (if (proveri_potez stanje index)  
     (if(null stanje) '()
       (if(> index 1) (cons (car stanje) (ubaci (cdr stanje) (- index 1) znak))
         (cons (ubaci_na_stub (car stanje) znak) (cdr stanje)))) stanje ))

;;primer test poziva funkcije za ucitavanje novog poteza (setf stanje (ubaci stanje 6 'x)))


;;----------------------------------------------------------------------------------------------------------
;;funkcija za ucitavanje ko ce prvi da igra
;;----------------------------------------------------------------------------------------------------------



(defun ko_ce_prvi()
  (let* ((prvi 'x)
    (racunar (progn (format t"~%Unesite r ako racunar igra prvi (r ili c):")
                (read)))
    (auto(if(equal racunar 'r) (print "Prvi igra racunar") (print "Prvi igra covek"))))))

;;----------------------------------------------------------------------------------------------------------
;;funkcija za stampanje (modularna)
;;----------------------------------------------------------------------------------------------------------

;;koristi se za dopunu liste u pripremi stampanja
(defun filler(n znak)
  (loop for i from 1 to n
        collect znak))

;;modifikuje stanje kako bi ga pripremila za stapanje
(defun regenerisi(stanje n nn cnt)
  (if(null stanje) '()
    (cons(append (filler (1- cnt) '*) (append (car stanje) (filler (- n cnt) '*))) 
          (regenerisi (cdr stanje) n (1- nn) (1+ cnt)))))


;;funkcija za grupisanje podlisti kako bi lista mogla da se lepo modifikuje i bila spremna za stampanje
(defun grupisi ( l n )
    (if l (grupisi-sub (cons nil l) n n))
  )

(defun grupisi-sub ( l m n )
    (if (and (cdr l) (< 0 n))
        (grupisi-sub (cons (cons (cadr l) (car l)) (cddr l)) m (1- n))
        (cons (reverse (car l)) (grupisi (cdr l) m))
    )
  )

(defun pripremi-stampu(stanje n)
  (if(null stanje) '()
    (append (regenerisi (car stanje) n n 1) (pripremi-stampu (cdr stanje) n))))

;;funkcija koja nakon modifikacije (ubacivanja elemenata za maskiranje blanko znaka)
(defun flatten (l)
  (cond ((null l) '())
        ((atom l) (list l))
        ((atom (car l))
         (cons (car l) (flatten (cdr l))))
        (t(append (flatten (car l))
                  (flatten (cdr l))))))


;;funkcija koja vrsi stampanje kompletne table
(defun stampaj(stanje n)
  (let* ((max (* n n (+ n (1- n)))))
    (loop for j from 0 to  (1- (+ n (1- n)))
      do(if(not(equal j  (+ n (1- n))))
        (loop for i from 1 to max
          do(let* ((index (- (1- (* i (+ n (1- n )))) j ))
                          (znak (nth index stanje)))
              (if(and(<= index max) (not(equal znak nil)))
                  (if(equal znak '*)
                        (format t "   ")
                    (format t "  ~a" znak))
                (if(equal i max)
                    (format t "  ~%")
                  )))))) 
          (loop for x from 1 to (* n n)
                do(if(<= x 9) (format t "  ~a" x)(format t " ~a" x) ))))


;;Okidacka funkcija za stampanje koja pokrece ceo mehanizam naveden iznad
(defun full_stampa(stanje n)
  (stampaj (flatten (pripremi-stampu (grupisi stanje n) n)) n))

;;==========================================================================================================
;;====================FAZA 2================================================================================
;;==========================================================================================================



;;----------------------------------------------------------------------------------------------------------
;;funkcija za genersianje liste svih mogucih prelaza stanja
;;----------------------------------------------------------------------------------------------------------

;(defun generisi_moguca_stanja (stanje n znak &optional (i 1))
 ;         (
   ;        if (<= i (* n n))
;          (cons (ubaci stanje i znak) (generisi_moguca_stanja stanje n znak (1+ i))))) 



(defun generisi_moguca_stanja (stanje n znak &optional (i 1))
          (
           if (<= i (* n n));dodati n * n u finalnu
              (if(not(equalp (ubaci stanje i znak) stanje))
                  (cons (ubaci stanje i znak) (generisi_moguca_stanja stanje n znak (1+ i)))
                (generisi_moguca_stanja stanje n znak (1+ i)))))


;;----------------------------------------------------------------------------------------------------------
;;funkcija za odigravanje poteza (ne menja originalno stanje)
;;----------------------------------------------------------------------------------------------------------

(defun odigraj_potez (pozicija znak)
  (let* ((tmp_stanje stanje))
    (ubaci tmp_stanje pozicija znak)))



;;----------------------------------------------------------------------------------------------------------
;;funkcija za odredjivanje sledeceg igraca i odredjivanje znaka
;;----------------------------------------------------------------------------------------------------------


(defun sledeci(i prvi)
  (if (equalp prvi 'x)
      (let* ((znak (if(oddp i) 'x 'o)))
        znak)
    (let* ((znak (if(oddp i) 'o 'x)))
        znak)))
        
(defun stampaui(i prvi)
  (if (equalp prvi 'x)
      (let* ((znak (if(oddp i) 'x 'o)))
        (if(oddp i) (format t "~%igrac x na potezu: ") (format t "~%igrac o na potezu: ")) 
        )
    (let* ((znak (if(oddp i) 'o 'x)))
        (if(oddp i) (format t "~%igrac o na potezu: ") (format t "~%igrac x na potezu: ")) 
        )))

(defun sledeci2(i igracprvi)
  (if (equalp igracprvi 'r)
      (let* ((igrac (if(oddp i) 'r 'c)))
        igrac)
    (let* ((igrac (if(oddp i) 'c 'r)))
        igrac)))

;;----------------------------------------------------------------------------------------------------------
;;funkcija za proveru pobednika
;;----------------------------------------------------------------------------------------------------------

;funkcija za uzastopnog pojavljivanja elementa u listi
;vraca broj poena za odredjeni znak

;;POMOCNA
(defun prebroji_listu(lista znak)
  (if(<(length lista) '4) '0
    (let* ((cnt 0))
      (if (and (equalp znak (car lista)) 
               (equalp (car lista) (cadr lista)) 
               (equalp (cadr lista) (caddr lista))
               (equalp (caddr lista) (cadddr lista)))
          (setq cnt (1+ (prebroji_listu (cdr lista) znak)))
        (setq cnt (prebroji_listu (cdr lista) znak))))))


;;funkcija za proveru poena na svakom stubicu
(defun win_stubic (stanje znak)
  (if(null stanje) '0
    (+ (prebroji_listu (car stanje) znak) (win_stubic (cdr stanje) znak))))



;;POMOCNA
;;obradjivanje elementa jedne matrice
(defun obradi_matricu(matrica dim brstub znak)
  (let* ((poen 0) (st 0) (fn (1- dim)))            
    (loop for i from 0 to (1- dim)          
          do(let* ((listtmp '()))              
              (loop for j from 0 to (1- dim)                    
                    do(setq listtmp (cons (nth i (nth j matrica)) listtmp)))
              (setq poen (+ poen (prebroji_listu listtmp znak)))
              )) 
    (let* ((j (1- dim)) 
           (tmplist '())
           (k 0)           
           (tmplist2 '()))
      (loop for i from 0 to (1- dim)
            do(let* ()
                (setq tmplist (cons (nth j (nth i matrica)) tmplist))
                (setq j (1- j))
                (if(< j '0) 
                    (let* ()
                      (setq j (1- dim)) 
                      (setq poen (+ poen (prebroji_listu tmplist znak)))
                      (setq tmplist '())))
              
                (setq tmplist2 (cons (nth k (nth i matrica)) tmplist2))
                (setq k (1+ k))
                (if(> k (1- dim)) 
                    (let* ()
                      (setq k '0) 
                      (setq poen (+ poen (prebroji_listu tmplist2 znak)))
                      (setq tmplist2 '())))
                )))
    (setq poen (+ poen (proveri_paralelne_diag matrica dim znak)))
    poen)
  )



;;funkcija za odredjivanje poena po x osi
(defun proveri_xosu(stanje dim znak)
  (let* ((poen 0))
    (loop for i from 0 to (1- dim)
          do(let* ((matrica '()) )
              (loop for j from 0 to (1- dim)
                    do(let* ()
                        (setq matrica (cons (nth (+ j (* i dim)) stanje) matrica))
                        ))
              (setq poen (+ poen (obradi_matricu matrica dim dim znak)))
              ))
    poen)
  )
         
         
         
;;funkcija za odredjivanje poena po z osi
(defun proveri_zosu(stanje dim znak)
  (let* ((xt 0) (poen 0))
  (loop for k from 0 to (1- dim)
        do(let* ((matrica '()) (st 0))
            (loop for i from 0 to (1- dim)
                  do(let* () 
                      (setq matrica (cons (nth (+ xt (* st dim)) stanje) matrica))
                      (setq st (1+ st))
                      ))
            (setq poen (+ poen (obradi_matricu matrica dim dim znak)))            
            )
        (setq xt (1+ xt))
        ) 
    poen)
  )


;;POMOCNA
;;samo paralelno sa glavnom            
;;funkcija za proveru paralelnih dijagonala
(defun proveri_paralelne_diag(matrica dim znak)
  (let* ((poen 0))
    (let* ((tmplista '()) (j (- dim 2)))
      (loop for i from 0 to (- dim 2)
            do(let* ()
                (setq tmplista (cons (nth j (nth i matrica)) tmplista))
                (setq j (1- j))
                ))
      (setq poen (+ poen (prebroji_listu tmplista znak))))
    
    (let* ((tmplista '()) (j (- dim 3)))
      (loop for i from 0 to (- dim 3)
            do(let* ()
                (setq tmplista (cons (nth j (nth i matrica)) tmplista))
                (setq j (1- j))
                ))
      (setq poen (+ poen (prebroji_listu tmplista znak))))
    
    (let* ((tmplista '()) (j (- dim 1)))
      (loop for i from 1 to (- dim 1)
            do(let* ()
                (setq tmplista (cons (nth j (nth i matrica)) tmplista))
                (setq j (1- j))
                ))
      (setq poen (+ poen (prebroji_listu tmplista znak))))
    
    (let* ((tmplista '()) (j (- dim 1)))
      (loop for i from 2 to (- dim 1)
            do(let* ()
                (setq tmplista (cons (nth j (nth i matrica)) tmplista))
                (setq j (1- j))
                ))
      (setq poen (+ poen (prebroji_listu tmplista znak))))
    
    (let* ((tmplista '()) (j 0))
      (loop for i from 1 to (- dim 1)
            do(let* ()
                (setq tmplista (cons (nth j (nth i matrica)) tmplista))
                (setq j (1+ j))
                ))
      (setq poen (+ poen (prebroji_listu tmplista znak))))
    
    (let* ((tmplista '()) (j 0))
      (loop for i from 2 to (- dim 1)
            do(let* ()
                (setq tmplista (cons (nth j (nth i matrica)) tmplista))
                (setq j (1+ j))
                ))
      (setq poen (+ poen (prebroji_listu tmplista znak))))
    
    (let* ((tmplista '()) (j 1))
      (loop for i from 0 to (- dim 2)
            do(let* ()
                (setq tmplista (cons (nth j (nth i matrica)) tmplista))
                (setq j (1+ j))
                ))
      (setq poen (+ poen (prebroji_listu tmplista znak))))
    
    (let* ((tmplista '()) (j 2))
      (loop for i from 0 to (- dim 3)
            do(let* ()
                (setq tmplista (cons (nth j (nth i matrica)) tmplista))
                (setq j (1+ j))
                ))
      (setq poen (+ poen (prebroji_listu tmplista znak))))
    poen
    )
  )


;;funkcija za proveru po 3d osi
(defun proveri_3d_diag(stanje dim znak)
  (let* ((poen 0))
    (let* ((j 0) (matrica '()) (poen 0))
      (loop for i from 0 to (1- dim)
            do(let* ()
                (setq matrica (cons (nth j stanje) matrica))
                (setq j (+ j (1+ dim)))
                ))
      (setq poen (+ poen (obradi_matricu matrica dim dim znak))))
    (let* ((j (1- dim)) (matrica '()))
      (loop for i from 0 to (1- dim)
            do(let* ()
                (setq matrica (cons (nth j stanje) matrica))
                (setq j (+ j (1- dim)))
                ))
      (setq poen (+ poen (obradi_matricu matrica dim dim znak))))
    poen)
  )


;;funkcija za proveru 3d dijagonale paralelne
(defun proveri_3d_paralelne(stanje dim znak)
  (let* ((poen 0))
    (let* ((matrica '()) (j dim) (k dim))
      (loop for i from 1 to (1- dim)
            do(setq matrica (cons (nth k stanje) matrica))
            (setq k (+ k j 1))
            )      
      (setq poen (+ poen (obradi_matricu (expanzija matrica dim) dim dim znak))))
    (let* ((matrica '()) (j dim) (k (* 2 dim)))
      (loop for i from 2 to (1- dim)
            do(setq matrica (cons (nth k stanje) matrica))
            (setq k (+ k j 1))
            )      
      (setq poen (+ poen (obradi_matricu (expanzija matrica dim) dim dim znak))))
    (let* ((matrica '()) (j dim) (k 1))
      (loop for i from 1 to (1- dim)
            do(setq matrica (cons (nth k stanje) matrica))
            (setq k (+ k j 1))
            )      
      (setq poen (+ poen (obradi_matricu (expanzija matrica dim) dim dim znak))))
    (let* ((matrica '()) (j dim) (k 2))
      (loop for i from 2 to (1- dim)
            do(setq matrica (cons (nth k stanje) matrica))
            (setq k (+ k j 1))
            )      
      (setq poen (+ poen (obradi_matricu (expanzija matrica dim) dim dim znak))))
    (let* ((matrica '()) (j (1- dim)) (k (- dim 2)))
      (loop for i from 1 to (1- dim)
            do(setq matrica (cons (nth k stanje) matrica))
            (setq k (+ k j))
            )      
      (setq poen (+ poen (obradi_matricu (expanzija matrica dim) dim dim znak))))
    (let* ((matrica '()) (j (1- dim)) (k (- dim 3)))
      (loop for i from 2 to (1- dim)
            do(setq matrica (cons (nth k stanje) matrica))
            (setq k (+ k j))
            )      
      (setq poen (+ poen (obradi_matricu (expanzija matrica dim) dim dim znak))))
    
    (let* ((matrica '()) (j (1- dim)) (k (1- (* dim 2))))
      (loop for i from 1 to (1- dim)
            do(setq matrica (cons (nth k stanje) matrica))
            (setq k (+ k j))
            )      
      (setq poen (+ poen (obradi_matricu (expanzija matrica dim) dim dim znak))))
    (let* ((matrica '()) (j (1- dim)) (k (1- (* dim 3))))
      (loop for i from 2 to (1- dim)
            do(setq matrica (cons (nth k stanje) matrica))
            (setq k (+ k j))
            )      
      (setq poen (+ poen (obradi_matricu (expanzija matrica dim) dim dim znak))))
    poen)
  )


;;POMOCNA
;;funkcija za ekspanziju
(defun expanzija(matrica dim)
  (let* ((brdopuna (- dim (length matrica))) (matricatmp matrica))
    (if(not(equalp brdopuna '0))
          (loop for i from 1 to brdopuna
                do(setq matricatmp (cons (generisi_stubic '() dim) matricatmp)))
      )
    matricatmp))




(defun proveri_poene (stanje dim znak)
  (let* ((poen 0))
    (+ (win_stubic stanje znak)
       (proveri_xosu stanje dim znak)
       (proveri_zosu stanje dim znak)
       (proveri_3d_diag stanje dim znak)
       (proveri_3d_paralelne stanje dim znak))))

(defun pobednik(stanje dim)
  (let* ((xpoeni (proveri_poene stanje dim 'x))
         (opoeni (proveri_poene stanje dim 'o)))
    (if(> xpoeni opoeni)
        (format t "~%POBEDNI je igrac X sa brojem poena: ~a ~%O-poeni: ~a~%" xpoeni opoeni)
      (format t "~%POBEDNI je igrac O sa brojem poena: ~a ~%X-poeni: ~a~%" opoeni xpoeni))
    )
  )


;;==========================================================================================================
;;====================FAZA 3================================================================================
;;==========================================================================================================


(defun proceni-stanje(stanje)
   (list stanje (random 100)))


(defun nova-stanja(stanje potez)
  (generisi_moguca_stanja stanje n (if (equalp potez 'x) 'x 'o)))


;(defun heuristika(stanje comp)
;  (let* ((dim n)
;         (xpoeni (proveri_poene stanje dim 'x))
;         (opoeni (proveri_poene stanje dim 'o))
;         )
;    (if(equalp comp 'x) (list stanje (* 2 (- xpoeni opoeni)))
;      (list stanje (* 2 (- opoeni xpoeni))))
;    )
;  )

(defun heuristika(stanje comp)
  (let* ((dim n)
         (xpoeni (racunica-heuristike stanje dim 'x))
         (opoeni (racunica-heuristike stanje dim 'o))
         )
    (if(equalp comp 'x) (list stanje (- xpoeni opoeni))
      (list stanje (- opoeni xpoeni)))
    )
  )


(defun racunica-heuristike(stanje dim znak)
  (+ (win_stubic stanje znak)
         (* 2 (proveri_xosu stanje dim znak))
         (* 2 (proveri_zosu stanje dim znak))
         (* 3 (proveri_3d_diag stanje dim znak))
         (* 4 (proveri_3d_paralelne stanje dim znak))
         ))
 




(defun alphabeta (node depth Alfa Beta comp maximizing-player-p)
  (when (or (= depth 0) (provera_kraj node))
    (return-from alphabeta (heuristika node comp)));; vraca par (stanje heuristika)	
  (if maximizing-player-p
      (let ((best-value -9999) (best 'mt) (pom 'mt))
        (dolist (child (nova-stanja node maximizing-player-p))
          (setf pom (alphabeta child (1- depth) Alfa Beta comp nil))		  
          (when (> (cadr pom) best-value) (setf best child) (setf best-value (cadr pom)))
          (when (> best-value alfa) (setf alfa best-value))		  
          (when (<= Beta Alfa)
            ;; Beta cut-off
            (return)))
        (list best best-value))		
    (let ((best-value 9999) (best 'mt) (pom 'mt))
      (dolist (child (nova-stanja node maximizing-player-p))
        (setf pom (alphabeta child (1- depth) Alfa Beta comp t))		  
        (when (< (cadr pom) best-value) (setf best child) (setf best-value (cadr pom)))
        (when (< best-value beta) (setf beta best-value))		  
        (when (<= Beta Alfa)
          ;; Alfa cut-off
          (return)))
      (list best best-value))))




;;-----------------------------------------------------
;;FUNKCIJA ZA ODIGRAVANJE IGRE
;;-----------------------------------------------------


(defun pokreni_igru()
  (let* ((dim (progn(format t "~%Unesite dimenziju kocke: ")(read))) 
         (tmp_stanje (generisi dim)) 
         (prvi (progn(format t "~%Unesite ko igra prvi(x ili o): ")(read)))
         (igracprvi (progn(format t "~%Unesite ko igra prvi covek/racunar (c ili r): ")(read)))
                    (formax (* dim dim dim)))
         (setf n dim)
         (loop for i from 1 to formax
          do(let* ((znak (sledeci i prvi))
                   (igrac (sledeci2 i igracprvi)))
              (if (equalp igrac 'r) 
                  (let* ()
                    ;;(print znak)
                    (format t "~%RACUNAR ODIGRAVA POTEZ . . . ~%")
                    (setq tmp_stanje (car (alphabeta tmp_stanje 6 -999 999 znak znak)))
                    (full_stampa tmp_stanje dim))
                     (let* ((tmptmp (stampaui i prvi))
                             (pozicija (read))                   
                             (validnost (proveri_potez tmp_stanje pozicija)))
                        (if validnost 
                          (setq tmp_stanje (ubaci tmp_stanje pozicija znak))
                        (format t "~%Nevalidan potez"))              
                        (if validnost
                                 (full_stampa tmp_stanje dim)
                          (setq i (1- i)))))))
              (pobednik tmp_stanje dim))
)