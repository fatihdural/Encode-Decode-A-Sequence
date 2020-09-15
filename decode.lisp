; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Yakup Genc                       *
; *********************************************

; FATİH DURAL
; 151044041

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"

(defun read-as-list (filename)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters).
	(setq lines '(#\a))
	(setq temp '(#\a))
	(let ((in (open filename :if-does-not-exist nil)))
  		(when in
    	(loop for line = (read-line in nil)
        	while line do 
			(setf lines (split-string line)) ; her satiri split-string fonk gonderir.
    	)
    	(close in)))

	(if (null lines)
		0
		lines  
	)
)

(defun split-string (str)
	(setq mainList '(()))			; fonksiyon satirlari kelime kelime ayirir.
	(setq temp '())
	(loop 
        (if (or (= (c2i (char str 0)) (c2i #\space) ) (= (c2i (char str 0)) (c2i #\linefeed) ))
			(progn
				(push temp (cdr (last mainList)) ) 
				(setf str (subseq str 1))
				(setf temp '())
			)	
			(progn
				(setf temp (append temp (list (char str 0)) ) ) 
				(setq str (subseq str 1))
			)
		)
	   (when (= 0 (length str) ) (return  (cdr mainList) ))
	)
	(if (= 0 (length temp))
		(cdr mainList)			; kelimeleri '((a b) (c d)) gibi dondurur. (a b) ve (c d) bir satirin kelimeleri olmak uzere.
		(progn
			(push temp (cdr (last mainList)) )
			(cdr mainList)
		)
	)
)


; Encoder Functions

(defun encoder(doc cipher-alphabet)
	(setq cipher-doc '())
	(loop
		(setf cipher-doc (append cipher-doc (list (word-encoder (car doc) cipher-alphabet) ) )  )  ; metin dolasilir ve helper fonk
		(setq doc (cdr doc))  																		; ile sifrelenir.
	   (when (null doc) (return  cipher-doc) )
	)
	(format t "Encrypted document : ~a" cipher-doc)
	(terpri)
	(return-from encoder cipher-doc)
)

(defun word-encoder(word cipher-alphabet) ; kelimedeki her harfi gezip, karsılıgını char-encoderdan alip en son sifreli kelime dondurur.
	(setq words '())
	(loop
		(setf words (append words (list (char-encoder (c2i  (car word)) cipher-alphabet))))    
		(setf word (cdr word) ) 
		(when (null word) (return words) )
	)
)

(defun char-encoder(num cipher-alphabet) ; kelimedeki her bir harfin cipher alfabesindeki karsiligini dondurur.
	(setq chr "a")
	(loop
		(if (< num 1)
			(progn
				(setf chr (car cipher-alphabet))
				(return chr)
			)	 
		) 
		(setf cipher-alphabet (cdr cipher-alphabet) )
		(setf num (- num 1)) 
		(when (null cipher-alphabet) (return 1) )
	)
)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

(defun spell-checker-0 (word dict) ; review all words from dictionary.
	(loop
		(if (equal (car dict) word ) 
			(progn
				(return-from spell-checker-0 T)
			)			
			(return nil)
		)
		(setf dict (cdr dict))
		(when (null dic)  (return nil) )
	)
)

(defun spell-checker-1 (word dict) ; a better version with find function mapping.
	(if (equal (find word dict :test #'equal ) nil ) 
		(return-from spell-checker-1 nil)
		(progn
			(return-from spell-checker-1 T)
		)
	)
)
;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defun Gen-Decoder-A (paragraph dict-name)
	(setq letters (cons (which-letters-contain paragraph) '() ) ) ; metinde hangi harfler gectigini bulur. 
	(format t "Document contains these letters..~a" letters)	
	(terpri)
	(setq letters-temp letters)  
	(setq plain-doc '())
	(setq plain-alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(loop
		(setf plain-doc (append plain-doc (list (word-decoder (car letters-temp) plain-alphabet)  ) )  ) ; olası tum ihtimalleri word-decoder dondurur.
		(setq letters-temp (cdr letters-temp) )  

	   (when (null letters-temp) (return  plain-doc) )
	)
	(setf plain-doc (car plain-doc)) ; parantez duzenlemesi
	(all-combination paragraph plain-doc (car letters) dict-name)	; all-combination ile de tum ihtimaller denenir.	
	(print "Program is over.")
)

(defun all-combination (paragraph all-keys letters dict-name) 
	(format t "All possibilities are being tested right now..")
	(terpri)
	(setq liste-counter (list-length paragraph) )
	(setq dictlist (cdr (read-as-list dict-name)) )
	(sort dictlist #'string<= :key #'first)
	(setq counter-all 0)
	(setq big-counter -1)
	(setq big-key '())
	(setq big-plain '())
	(loop
		(setq counter 0)
		(setq key (car all-keys)) ; her bir alfabe kombinasyonu 
		(setq paragraph-temp paragraph)
		(setq plain-key '())
		(setq exit nil)
		(setf counter-all (+ counter 1))
		(loop
			(setq key-word (car (list (all-combination-word-decoder (car paragraph-temp) key letters) ) )  ) ; her kelime word fonk.a gonderilir.
			(if (equal T (spell-checker-1 key-word dictlist ) ) 
				(progn
					(setf counter (+ counter 1))
				)
			) 
			(setf plain-key(append plain-key (list key-word)  )  )  

			(setf paragraph-temp (cdr paragraph-temp))
			(when (or (null paragraph-temp) (equal counter 4 ))   (return 0))
		)
		(if (> counter big-counter) 
			(progn
				(setf big-counter counter)
				(setf big-key key)
				(setf big-plain plain-key)
			)
		)
		(setf all-keys (cdr all-keys))
		(when (or (equal counter liste-counter) (null all-keys) )  (return 0) )
	)
	(format t "Result : ~a" big-plain)
	(terpri)
)

(defun all-combination-word-decoder(word cipher-alphabet letters) ; kelimedeki her harfi gezip, karsılıgını char-decoderdan alip en son sifreli kelime dondurur.
	(setq words '())
	(loop
		(setf words (append words (list (all-combination-char-decoder (position (car word) letters :from-end t) cipher-alphabet))))  
		(setf word (cdr word) ) 
		(when (null word) (return words) )
	)
)

(defun all-combination-char-decoder(num cipher-alphabet);kelimedeki her bir harfin cipher alfabesindeki karsiligini dondurur.
	(setq chr "a")
	(loop
		(if (< num 1)
			(progn
				(setf chr (car cipher-alphabet))
				(return chr)
			)	
		) 
		(setf cipher-alphabet (cdr cipher-alphabet) )
		(setf num (- num 1)) 
		(when (null cipher-alphabet) (return 1) )
	)
)

(defun which-letters-contain(paragraph)
	(setq letters '())
	(loop
		(setq element (car paragraph) ) 
		(loop
			(if (eq (find (car element) letters :test #'equal ) nil ) ; olmayan harfleri ekleyerek sadece olan harfleri bulur.
				(progn
					(setf letters (append letters (list (car element) ) ) ) 
				)
			)
			(setf element (cdr element))
			(when (null element) (return 0))
		)
		(setf paragraph (cdr paragraph) )
		(when (null paragraph) (return letters))
	)
)


(defun word-decoder (word plain-alphabet) ; recursive ve loop iceren tum ihtimalleri bulan genis decoder yardımcı fonksiyonu
	(setq recur-liste '())
	(if (eq nil word)
		(return-from word-decoder '(  ) ) 
		(setf recur-liste  (word-decoder (cdr word) plain-alphabet ) )  
	)
	(setq counter 0)
	(setq retr-liste '( () ))
	(loop
		(setf word (list (i2c counter) ) ) 
		(setq recur-liste2 recur-liste) 
		(loop
			(if (not (equal word (car recur-liste2) ) )   
				(setf retr-liste (cons (append word (car recur-liste2) ) retr-liste ) ) 
			)
			(setf recur-liste2 (cdr recur-liste2) )
			(when (null recur-liste2) (return 0) )
		)	
		(setf counter (+ counter 1))
		(when (= counter 26) (return-from word-decoder (cdr (reverse retr-liste))   ))
	)
)
(defun which-letters-contain-B(paragraph) ; B fonksiyonu icin hangi harfler fazla adeti, yer degistirmesi gibi islemleri yapar.
	(setf counter-general (list-length (which-letters-contain paragraph) ) )
	(setq cipher-alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(setq letters '())
	(setq common-letters '(e t a o i n))
	(setq freq-list (make-list 26 :initial-element 1) )
	(loop
		(setq element (car paragraph) ) 
		(loop
			(if (eq (find (car element) letters :test #'equal ) nil )
				(progn
					(setf letters (append letters (list (car element) ) ) ) 
				)
				(progn
					(setf (nth (c2i (car element)) freq-list) (+ 1 (nth (c2i (car element)) freq-list) ))
				)
			)
			(setf element (cdr element))
			(when (null element) (return 0))
		)
		(setf paragraph (cdr paragraph) )
		(when (null paragraph) (return letters))
	)
	(setq most-freq-list (copy-list freq-list) )
	(sort most-freq-list #'<)
	(setq counter 0)
	(loop
		(setf most-freq-list (cdr most-freq-list))
		(setf counter (+ counter 1))
		(when (= counter 20) (return 1))
	)
	(setf most-freq-list (reverse most-freq-list))
	(setq most-usage-letters '())
	(loop
		(setf counter 0)
		(setq freq-list-temp freq-list)
		(setf flag 0)
		(loop
			(if (equal (car freq-list-temp) (car most-freq-list))
				(progn
					(setf repl-elem (nth counter cipher-alphabet))
					(setf repl-position (position (car common-letters) cipher-alphabet :from-end t) ) 
					(setf (nth counter cipher-alphabet) (car common-letters))
					(setf (nth repl-position cipher-alphabet) repl-elem)
					(setf (nth counter freq-list) 0)
					(setf most-usage-letters (append most-usage-letters (list (i2c counter)) ))
					(setf most-freq-list (cdr most-freq-list))
					(setf common-letters (cdr common-letters))
					(setf flag 1)
				)
			)
			(setf counter (+ counter 1))
			(setf freq-list-temp (cdr freq-list-temp)) 
			(when (or  (= flag 1) (null freq-list-temp) )  (return 1))
		)
		(setf counter-general (- counter-general 1))
		(when (= counter-general 0) (return 0))
	)
	(format t "Most Usage Letters are ~a and replace them with (e t a o i n). " most-usage-letters)
	(terpri)
	(return-from which-letters-contain-B cipher-alphabet)	
)
(defun Gen-Decoder-B-0 (paragraph dict-name)
	(setq cipher-alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(setq plain-words '(a))
  	(setq letters (cons (which-letters-contain-B paragraph) '() ) )
	(setq letters-temp letters)			; en cok kullanilan 6 harfin eslestirilmesi brute-force fonksiyonu.
	(loop
		(setq word (car paragraph))
		(setq plain-words-in '())
		(loop
			(setq cipher-alphabet-temp cipher-alphabet)
			(setq num (c2i (car word)) ) 
			(setq chr "a")
			(loop
				(if (< num 1)
					(progn
						(setf chr (car cipher-alphabet-temp))
						(return chr)
					)	
				) 
				(setf cipher-alphabet-temp (cdr cipher-alphabet-temp) )
				(setf num (- num 1)) 
				(when (null cipher-alphabet-temp) (return 1) )
			)
			(setf plain-words-in (append plain-words-in (list chr)   )  ) 
			(setf word(cdr word) )
			(when (null word) (return 0))
		)
		(push plain-words-in (cdr (last plain-words)))
		(setf paragraph (cdr paragraph) ) 
		(when (null paragraph) (return 0))
	)
	(setf plain-words (cdr plain-words))
	(format t "Result : ~a" plain-words)  
)

(defun Gen-Decoder-B-1 (paragraph dict-name)
	(setq cipher-alphabet '(b c d e f g h i j k l m n o p q r s t u v w x y z a))
	(setq plain-words '(a))
  	(setq letters (cons (which-letters-contain-B paragraph) '() ) )
	(setq letters-temp letters)
	(loop
		(setq word (car paragraph))
		(setq plain-words-in '())
		(loop
			(setq cipher-alphabet-temp cipher-alphabet)
			(setq num (c2i (car word)) ) 
			(setq chr "b")
			(loop
				(if (< num 1)
					(progn
						(setf chr (car cipher-alphabet-temp))
						(return chr)
					)	
				) 
				(setf cipher-alphabet-temp (cdr cipher-alphabet-temp) )
				(setf num (- num 1)) 
				(when (null cipher-alphabet-temp) (return 1) )
			)
			(setf plain-words-in (append plain-words-in (list chr)   )  ) 
			(setf word(cdr word) )
			(when (null word) (return 0))
		)
		(push plain-words-in (cdr (last plain-words)))
		(setf paragraph (cdr paragraph) ) 
		(when (null paragraph) (return 0))
	)
	(setf plain-words (cdr plain-words))
	(format t "Result : ~a" plain-words)  
)

(defun helper-for-cipher (doc) ; document sifreli ise bu fonksiyon ile duzeltme yapiyorum.
   		(setf doc2 '(a))
   		(loop
   			(push (car doc) (cdr (last doc2)) ) 
   			(setf doc (cdr doc)) 
   			(when (null doc) (return 0))
   		)
   		(setf doc2 (cdr doc2))
   		(return-from helper-for-cipher doc2)
)

(defun Code-Breaker (document dictionary decoder)
   	(let ((doc (read-as-list document)))
		(format t "Reading document is ~a" doc)
		(terpri)
   		(setf doc (helper-for-cipher doc)) ; ben document'in sifreli oldugunu varsaydim.
   		; eger document plain haldeyse, once sifreleyip daha sonra sifreyi cozmek istiyorsaniz alttaki fonksiyonun yorum satirini kaldirin lutfen
   		; Decoder-B fonksiyonunu kullanmak icinde dosya boyle olmalidir. Sadece Decoder-A fonksiyonun encode islemi icin alttaki yorum satiri kaldirilir.
   		;(setq doc (encoder doc '( d e f p q a b k l c r s t g y z h i j m n o u v w x)))
		(funcall decoder doc dictionary); Gen-Decoder fonksiyonun cagrilmasi.
        (terpri)
   	)
)

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(print "If you want to encrypt first, the alphabets are...")
	(print "Plain Alphabet : a b c d e f g h i j k l m n o p q r s t u v w x y z")
	(print "Cipher Alphabet: d e f p q a b k l c r s t g y z h i j m n o u v w x")
	(terpri)
	(terpri)
    (Code-Breaker "document1.txt" "dictionary1.txt" 'Gen-Decoder-A)
)

;; test code...
(test_on_test_data)   


