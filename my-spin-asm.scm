(module spin-asm-main
	(main start))

(define *debug-parser-reductions* #f)

(define *register-bank-memory-map*
  '(("sin0_rate"  0)
    ("sin0_range" 1)
    ("sin1_rate"  2)
    ("sin1_range" 3)
    ("rmp0_rate"  4)
    ("rmp0_range" 5)
    ("rmp1_rate"  6)
    ("rmp1_range" 7)
    ("pot0"       16)
    ("pot1"       17)
    ("pot2"       18)
    ("adcl"       20)
    ("adcr"       21)
    ("dacl"       22)
    ("dacr"       23)
    ("addr_ptr"   24)
    ("reg0"       32)
    ("reg1"       33)
    ("reg2"       34)
    ("reg3"       35)
    ("reg4"       36)
    ("reg5"       37)
    ("reg6"       38)
    ("reg7"       39)
    ("reg8"       40)
    ("reg9"       41)
    ("reg10"      42) 
    ("reg11"      43) 
    ("reg12"      44) 
    ("reg13"      45) 
    ("reg14"      46) 
    ("reg15"      47) 
    ("reg16"      48) 
    ("reg17"      49) 
    ("reg18"      50) 
    ("reg19"      51) 
    ("reg20"      52) 
    ("reg21"      53) 
    ("reg22"      54)
    ("reg23"      55) 
    ("reg24"      56)
    ("reg25"      57)
    ("reg26"      58) 
    ("reg27"      59) 
    ("reg28"      60) 
    ("reg29"      61) 
    ("reg30"      62) 
    ("reg31"      63))) 

(define (d-print . args)
  (when *debug-parser-reductions*
	(print args)))

;;; decimal-512s is encoded as 0 for 512, 1 for 1024, 2 for 2048 and 3 for 4096. No other values are allowed to be specified?
(define *operand-formats* '(binary decimal decimal-512s hex label s1.9 s1.14 s4.6 s.10 s.15 symbolic))

(define *fieldnames* '("C" "D" "M"
		       "CMASK" "N" "ADDR"
		       "CHO_TYPE"  ; value must be 0, 2, 3
		       "RDA"    ; operand value must be "rda" or 0
		       "SOF"    ; operand value must be "sof" or 2
		       "RDAL"   ; operand value must be "rdal" or 3
		       "OPCODE" ; this is a special field for the opcode. The value is given in attributes.
		       "MBZ"    ; this is a special field that must have all bits 0.
		       "FIXED"  ; this is a special field with a fixed value. The value is given in attributes of instruction
		       ))
  
(define *spin-instructions*
  '(("sof" (operands "C" "D")(fields ("C" (bit-width 16)(formats s1.14 hex symbolic))("D" (bit-width 11) (formats s.10 symbolic))("OPCODE" (bit-width 5)(value #xd))))
    ("and" (operands "M")(fields ("M" (bit-width 24)(formats binary hex symbolic))("MBZ" (bit-width 4))("OPCODE" (bit-width 5)(value #xe))))
    ("or"  (operands "M")(fields ("M" (bit-width 24)(formats binary hex symbolic))("MBZ" (bit-width 4))("OPCODE" (bit-width 5)(value #xf))))
    ("xor" (operands "M")(fields ("M" (bit-width 24)(formats binary hex symbolic))("MBZ" (bit-width 4))("OPCODE" (bit-width 5)(value #x10))))
    ("log" (operands "C" "D")(fields ("C" (bit-width 16)(formats s1.14 hex symbolic))("D" (bit-width 11) (formats s4.6 symbolic))("OPCODE" (bit-width 5)(value #xb))))
    ("exp" (operands "C" "D")(fields ("C" (bit-width 16)(formats s.1.14 hex symbolic))("D" (bit-width 11) (formats s.10 symbolic))("OPCODE" (bit-width 5)(value #xa))))
    ("skp" (operands "CMASK" "N")(fields ("CMASK" (bit-width 5)(formats binary hex symbolic))("N" (bit-width 6)(formats decimal label))("OPCODE" (bit-width 5)(value #x11))))
    ("rdax" (operands "ADDR" "C")(fields ("C" (bit-width 16)(formats s1.14 hex symbolic))("ADDR" (bit-width 6)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value #x4))))
    ("wrax" (operands "ADDR" "C")(fields ("C" (bit-width 16)(formats s1.14 hex symbolic))("ADDR" (bit-width 6)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value #x6))))
    ("maxx" (operands "ADDR" "C")(fields ("C" (bit-width 16)(formats s1.14 hex symbolic))("ADDR" (bit-width 6)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value #x9))))
    ("mulx" (operands "ADDR")(fields ("MBZ" (bit-width 21))("ADDR" (bit-width 6)(formats decimal hex symbolic))("OPCODE" (bit-width 5)))(opcode #xa))
    ("rdfx" (operands "ADDR" "C")(fields ("C" (bit-width 16)(formats s1.14 hex symbolic))("addr" (bit-width 6)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value #x5))))
    ("wrlx" (operands "ADDR" "C")(fields ("C" (bit-width 16)(formats s1.14 hex symbolic))("MBZ" (bit-width 5))("ADDR" (bit-width 6)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value #x8))))
    ("wrhx" (operands "ADDR" "C")(fields ("C" (bit-width 16)(formats s1.14 hex symbolic))("MBZ" (bit-width 5))("ADDR" (bit-width 6)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value #x7))))
    ("rda" (operands "ADDR" "C")(fields ("C" (bit-width 11)(formats s1.9 hex symbolic))("ADDR" (bit-width 15)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value 0))))
    ("rmpa" (operands "C")(fields ("C" (bit-width 11)(formats s1.9 hex symbolic))("FIXED" (bit-width 16)(value #x18))("OPCODE" (bit-width 5)(value 1))))
    ("wra" (operands "ADDR" "C")(fields ("C" (bit-width 11)(formats s1.9 hex symbolic))("ADDR" (bit-width 15)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value 2))))
    ("wrap" (operands "ADDR" "C")(fields ("C" (bit-width 11)(formats s1.9 hex symbolic))("ADDR" (bit-width 15)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value 3))))
    ("wlds" (operands "N" "F" "A")(fields ("MBZ" (bit-width 2))("N" (bit-width 1)(formats decimal hex))("A" (bit-width 15)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value #x12))))
    ("wldr" (operands "N" "F" "A")(fields ("FIXED" (bit-width 2)(value 1))("N" (bit-width 1)(formats decimal hex))("F" (bit-width 16)(formats decimal hex symbolic))("MBZ" (bit-width 6))("A" (bit-width 2)(formats decimal-512s symbolic))("OPCODE" (bit-width 5)(value #x12))))
    ("jam" (operands "N")(fields ("FIXED" (bit-width 25)(value 1))("N" (bit-width 1)(formats decimal hex))("OPCODE" (bit-width 5)(value #x13))))
    ("cho" (operands "RDA" "N" "C" "ADDR")(fields ("MBZ" (bit-width 2))("C" (bit-width 6)(formats binary symbolic))("ADDR" (bit-width 15)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value #x14))))
    ("cho" (operands "SOF" "N" "C" "D")(fields ("FIXED" (bit-width 2)(value 2))("C" (bit-width 6))("MBZ" (bit-width 1))("N" (bit-width 2)(formats decimal hex symbolic))("D" (bit-width 16)(formats s.15 symbolic))("OPCODE" (bit-width 5)(value #x14))))
    ("cho" (operands "RDAL" "N")(fields ("FIXED" (bit-width 9)(value #x184))("N" (bit-width 2)(formats decimal hex symbolic))("MBZ" (bit-width 16))("OPCODE" (bit-width 5)(value #x14))))
    ("cho" (operands "TYPE" "N")(fields ("FIXED" (bit-width 9)(value #x184))("N" (bit-width 2)(formats decimal hex symbolic))("MBZ" (bit-width 16))("OPCODE" (bit-width 5)(value #x14))))
    ("clr" (operands) (fields ("MBZ" (bit-width 27))("OPCODE" (bit-width 5)(value #x1e))))
    ("not" (operands) (fields ("FIXED" (bit-width 27)(value #x7fffff8))("OPCODE" (bit-width 5)(value #x10))))
    ("absa" (operands) (fields ("MBZ" (bit-width 27))("OPCODE" (bit-width 5)(value #x9))))
    ("ldax" (operands "ADDR")(fields ("ADDR" (bit-width 6)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value 5))))))

(define *predefined-symbols*
  '(("sin0_rate"     0)
    ("sin0_range"    1)
    ("sin1_rate"     2)
    ("sin1_range"    3)
    ("rmp0_rate"     4)
    ("rmp0_range"    5)
    ("rmp1_rate"     6)
    ("rmp1_range"    7)
    ("pot0"          #x10)
    ("pot1"          #x11)
    ("pot2"          #x12)
    ("adcl"          #x14)
    ("adcr"          #x15)
    ("dacl"          #x16)
    ("dacr"          #x17)
    ("addr_ptr"      #x18)
    ("reg0"          #x20)
    ("reg1"          #x21)
    ("reg2"          #x22)
    ("reg3"          #x23)
    ("reg4"          #x24)
    ("reg5"          #x25)
    ("reg6"          #x26)
    ("reg7"          #x27)
    ("reg8"          #x28)
    ("reg9"          #x29)
    ("reg10"         #x2a)
    ("reg11"         #x2b)
    ("reg12"         #x2c)
    ("reg13"         #x2d)
    ("reg14"         #x2e)
    ("reg15"         #x2f)
    ("reg16"         #x30)
    ("reg17"         #x31)
    ("reg18"         #x32)
    ("reg19"         #x33)
    ("reg20"         #x34)
    ("reg21"         #x35)
    ("reg22"         #x36)
    ("reg23"         #x37)
    ("reg24"         #x38)
    ("reg25"         #x39)
    ("reg26"         #x3a)
    ("reg27"         #x3b)
    ("reg28"         #x3c)
    ("reg29"         #x3d)
    ("reg30"         #x3e)
    ("reg31"         #x3f)
    ("sin0"          #x0)
    ("sin1"          #x1)
    ("rmp0"          #x2)
    ("rmp1"          3)
    ("rda"           0)  ; used with cho
    ("sof"           2)  ; used with cho
    ("rdal"          3)  ; used with cho
    ("sin"           0)  ; used with cho
    ("cos"           1)  ; used with cho
    ("reg"           2)  ; used with cho
    ("compc"         4)  ; used with cho
    ("rptr2"         #x10)  ; used with cho
    ("na"            #x20) ; used with cho
    ("run"           #x80000000) ; used with skp
    ("zrc"           #x40000000) ; used with skp
    ("zro"           #x20000000)
    ("gez"           #x10000000)
    ("neg"           #x80000000))) ; used with skp
    
(define *symbol-table* (create-hashtable))
(define *label-table* (create-hashtable))
(define *mnemonic-hash* (create-hashtable))

(define (ident-is-mnemonic? str)
  (hashtable-contains? *mnemonic-hash* str))
  
(define (process-ident str-in)
  (let ((str (string-downcase str-in)))
    (let ((rv (cond ((string=? str "equ") (cons 'EQU str))
		    ((string=? str "mem") (cons 'MEM str))
		    (else
		     (let ((reg-pair (assoc str *register-bank-memory-map*)))
		       (d-print "(reg-pair " reg-pair ")(str " str ")")
		       (if reg-pair
			   (cons 'REG str)
			   (if (ident-is-mnemonic? str)
			       (cons 'MNEMONIC str)
			       (cons 'IDENT str))))))))
      (d-print "((rv " rv ")(str " str "))")
      rv)))

(define (resolve-to-number arg)
  (if (number? arg)
      arg
      (if (hashtable-contains? *symbol-table* arg)
	  (resolve-to-number (hashtable-get *symbol-table* arg))
	  (begin
	    (dump-trace-stack (current-output-port) 10)
	    (error "IDENT" "identifier not found in symbol table" arg)))))

(define (process-mnemonic mnemonic . args)
  (print "(line " (- *line-number* 1) ")(address " (unsigned->string *current-address*) ")(mnemonic " mnemonic ")(args " args ")")
  (set! *current-address* (+ 4 *current-address*)))

(define (process-label label)
  (hashtable-put! *label-table* label *current-address*)
  (print "(label " label ")(address " (unsigned->string *current-address*) ")"))

(define *line-number* 1)
(define *current-address* 0)

(define *spin-l*
  (let ((armed #f))
    (regular-grammar
     ()
     ((: alpha (* alnum) ":")                       (cons 'LABEL (the-string)))
     ((: ";" (* all))                               (begin
						      (d-print *line-number* "COMMENT->ignore")
						      (ignore)))
     ((: digit (? ".") (* digit))   (begin
				      (cons 'SIGNED-FIXED-POINT (string->number (the-string)))))
     ((: "$" (+ xdigit))                            (begin
						      (let* ((hex-digits (list->string (cdr (string->list (the-string)))))
							     (the-number (string->number hex-digits 16)))
							(cons 'UNSIGNED-INTEGER the-number))))
     ((+ digit)                                     (begin
						      (cons 'UNSIGNED-INTEGER (string->number (the-string)))))
     ((: "%" (+ (or "0" "1")))                      (cons 'BIT-VECTOR (the-string)))
     ("|"                                           (cons 'OR (the-string)))
     ("-"                                           (cons 'MINUS (the-string)))
     ("+"                                           (cons 'PLUS (the-string)))
     ;ident
     ((: (out "+" "-" "!" ";" "," "|" #\tab #\space #\newline) (* (out "," "+" "|" "-" #\Tab #\Newline #\Space))) (process-ident (the-string)))
     (","                                           (cons 'COMMA (the-string)))
     ((+ (or #\tab #\space))                        (ignore))
     (#\Newline                                     (begin
						      (d-print *line-number* "Newline->ignore")
						      (set! *line-number* (+ 1 *line-number*))
						      (cons 'NEWLINE (the-string)))))))

(define *spin-g*
  (lalr-grammar
   (NEWLINE LABEL SIGNED-FIXED-POINT UNSIGNED-INTEGER BIT-VECTOR OR MINUS MNEMONIC PLUS EQU MEM REG IDENT COMMA)
   (file
    (())
    ((file line) (d-print *line-number* "(file line) -> file")))
   (line
    ((NEWLINE) (d-print *line-number* "(NEWLINE)->line"))
    ((LABEL NEWLINE) (begin
		       (d-print *line-number* "(LABEL NEWLINE)->line")
		       (process-label LABEL)))
    ((LABEL unlabeled-line NEWLINE) (begin
				      (d-print *line-number* "(LABEL unlabeled-line NEWLINE)->line")
				      (process-label LABEL)))
    ((unlabeled-line NEWLINE) (d-print *line-number* "(unlabeled-line NEWLINE)->line")))
   (value
    ((SIGNED-FIXED-POINT)      (begin
				 (d-print *line-number* "(SIGNED-FIXED-POINT)->value")
				 SIGNED-FIXED-POINT))
    ((PLUS SIGNED-FIXED-POINT) (begin
				 (d-print *line-number* "(PLUS SIGNED-FIXED-POINT)->value")
				 SIGNED-FIXED-POINT))
    ((MINUS SIGNED-FIXED-POINT) (begin
				  (d-print *line-number* "(MINUS SIGNED-FIXED-POINT)->value")
				  (- 0 SIGNED-FIXED-POINT)))
    ((UNSIGNED-INTEGER)         (begin
				  (d-print *line-number* "(UNSIGNED-INTEGER)->value")
				  UNSIGNED-INTEGER)))
   (ident-reg-or-value
    ((reg-or-value) (begin
		      (d-print *line-number* "(reg-or-value)->ident-reg-or-value")
		      (resolve-to-number reg-or-value)))
    ((MINUS IDENT) (begin
		     (d-print *line-number* "(MINUS IDENT)->ident-reg-or-value")
		     (- 0 (resolve-to-number IDENT))))
    ((ident-reg-or-value@a OR ident-reg-or-value@b) (begin
						      (d-print *line-number* "(ident-reg-or-value OR ident-reg-or-value)->ident-reg-or-value")
						      (bit-or (resolve-to-number a)
							      (resolve-to-number b))))
    ((ident-reg-or-value@a PLUS ident-reg-or-value@b) (begin
							(d-print *line-number* "(ident-reg-or-value PLUS ident-reg-or-value)->ident-reg-or-value")
							(+ (resolve-to-number a)
							   (resolve-to-number b))))
    ((ident-reg-or-value@a MINUS ident-reg-or-value@b) (begin
							 (d-print *line-number* "(ident-reg-or-value MINUS ident-reg-or-value)->ident-reg-or-value")
							 (- (resolve-to-number a)
							    (resolve-to-number b))))
    ((IDENT) (d-print *line-number* "(IDENT)->ident-reg-or-value") IDENT))
   (reg-or-value
    ((value) (d-print *line-number* "(value)->reg-or-value") value)
    ((REG) (d-print *line-number* "(IDENT)->reg-or-value") REG))
   (unlabeled-line
    ((EQU IDENT ident-reg-or-value) (begin
				      (d-print *line-number* "(EQU IDENT ident-reg-or-value)->unlabeled-line (IDENT " IDENT ")(ident-reg-or-value " ident-reg-or-value ")")
				      (hashtable-put! *symbol-table* IDENT ident-reg-or-value)))
    ((MEM IDENT value) (d-print *line-number* "(MEM IDENT value)->unlabeled-line"))
    ((MNEMONIC ) (begin
		   (d-print *line-number* "(MNEMONIC)->unlabeled-line")
		   (process-mnemonic MNEMONIC)))
    ((MNEMONIC ident-reg-or-value) (begin
				     (d-print *line-number* "(MNEMONIC ident-reg-or-value)->unlabeled-line")
				     (process-mnemonic MNEMONIC ident-reg-or-value)))
    ((MNEMONIC ident-reg-or-value@a COMMA ident-reg-or-value@b) (begin
							      (d-print *line-number* "(MNEMONIC ident-reg-or-value COMMA ident-reg-or-value)->unlabeled-line")
							      (process-mnemonic MNEMONIC a b)))
    ((MNEMONIC ident-reg-or-value@a COMMA ident-reg-or-value@b COMMA ident-reg-or-value@c)
     (begin
       (d-print *line-number* "(MNEMONIC ident-reg-or-value COMMA ident-reg-or-value)->unlabeled-line")
       (process-mnemonic MNEMONIC a b c)))
     
    ((MNEMONIC ident-reg-or-value@a COMMA ident-reg-or-value@b COMMA ident-reg-or-value@c COMMA ident-reg-or-value@d)
     (begin
       (d-print *line-number* "(MNEMONIC ident-reg-or-value COMMA ident-reg-or-value COMMA ident-reg-or-value COMMA ident-reg-or-value)->unlabeled-line")
       (process-mnemonic MNEMONIC a b c d))))))

(define (start argv)
  (print argv)
  (for-each (lambda (predefined-symbol-pair)
	      (hashtable-put! *symbol-table* (car predefined-symbol-pair) (cadr predefined-symbol-pair)))
	    *predefined-symbols*)
  (for-each (lambda (inst)
	      (hashtable-put! *mnemonic-hash* (car inst) #t))
	    *spin-instructions*)
  (read/lalrp *spin-g* *spin-l* (current-input-port)))
