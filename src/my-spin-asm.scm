(module spin-asm-main
	(main start))

(define *debug-parser-reductions* #f)
(define *debug-process-mnemonic* #f)
(define *debug-encoding* #f)

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

(define (n-print . args)
  (when *debug-process-mnemonic*
	(print args)))

(define (e-print . args)
  (when *debug-encoding*
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
   '(("sof" (operands ("C" "D"))(fields (("C" (bit-width 16)(formats s1.14 hex symbolic))("D" (bit-width 11) (formats s.10 symbolic))
											 ("OPCODE" (bit-width 5)(value #xd)))))
     ("and" (operands ("M"))(fields (("M" (bit-width 24)(formats binary hex symbolic))("MBZ" (bit-width 3))("OPCODE" (bit-width 5)(value #xe)))))
     ("or"  (operands ("M"))(fields (("M" (bit-width 24)(formats binary hex symbolic))("MBZ" (bit-width 3))("OPCODE" (bit-width 5)(value #xf)))))
     ("xor" (operands ("M"))(fields (("M" (bit-width 24)(formats binary hex symbolic))("MBZ" (bit-width 3))("OPCODE" (bit-width 5)(value #x10)))))
     ("log" (operands ("C" "D"))(fields (("C" (bit-width 16)(formats s1.14 hex symbolic))("D" (bit-width 11) (formats s4.6 symbolic))
											 ("OPCODE" (bit-width 5)(value #xb)))))
     ("exp" (operands ("C" "D"))(fields (("C" (bit-width 16)(formats s1.14 hex symbolic))
					 ("D" (bit-width 11) (formats s.10 symbolic))
					 ("OPCODE" (bit-width 5)(value #xc)))))
     ("skp" (operands ("CMASK" "N"))(fields (("CMASK" (bit-width 5)(formats binary hex symbolic))
					     ("N" (bit-width 6)(formats decimal label))
					     ("MBZ" (bit-width 16))
					     ("OPCODE" (bit-width 5)(value #x11)))))
     ("rdax" (operands ("ADDR" "C"))(fields (("C" (bit-width 16)(formats s1.14 hex symbolic))
					     ("MBZ" (bit-width 5))("ADDR" (bit-width 6)(formats decimal hex symbolic))
					     ("OPCODE" (bit-width 5)(value #x4)))))
     ("wrax" (operands ("ADDR" "C"))(fields (("C" (bit-width 16)(formats s1.14 hex symbolic))
					     ("MBZ" (bit-width 5))
					     ("ADDR" (bit-width 6)(formats decimal hex symbolic))
					     ("OPCODE" (bit-width 5)(value #x6)))))
     ("maxx" (operands ("ADDR" "C"))(fields (("C" (bit-width 16)(formats s1.14 hex symbolic))("ADDR" (bit-width 6)(formats decimal hex symbolic))
											     ("OPCODE" (bit-width 5)(value #x9)))))
     ("mulx" (operands ("ADDR"))(fields (("MBZ" (bit-width 21))("ADDR" (bit-width 6)(formats decimal hex symbolic))
							       ("OPCODE" (bit-width 5)(value #xa)))))
     ("rdfx" (operands ("ADDR" "C"))(fields (("C" (bit-width 16)(formats s1.14 hex symbolic))
					     ("MBZ" (bit-width 5))
					     ("ADDR" (bit-width 6)(formats decimal hex symbolic))
					     ("OPCODE" (bit-width 5)(value #x5)))))
     ("wrlx" (operands ("ADDR" "C"))(fields (("C" (bit-width 16)(formats s1.14 hex symbolic))
					     ("MBZ" (bit-width 5))("ADDR" (bit-width 6)(formats decimal hex symbolic))
					     ("OPCODE" (bit-width 5)(value #x8)))))
     ("wrhx" (operands ("ADDR" "C"))(fields (("C" (bit-width 16)(formats s1.14 hex symbolic))
					     ("MBZ" (bit-width 5))("ADDR" (bit-width 6)(formats decimal hex symbolic))
					     ("OPCODE" (bit-width 5)(value #x7)))))
     ("rda" (operands ("ADDR" "C"))(fields (("C" (bit-width 11)(formats s1.9 hex symbolic))
					    ("ADDR" (bit-width 16)(formats decimal hex symbolic))
					    ("OPCODE" (bit-width 5)(value 0)))))
     ("rmpa" (operands ("C"))(fields (("C" (bit-width 11)(formats s1.9 hex symbolic))("FIXED" (bit-width 16)(value #x18))
										     ("OPCODE" (bit-width 5)(value 1)))))
     ("wra" (operands ("ADDR" "C"))(fields (("C" (bit-width 11)(formats s1.9 hex symbolic))("ADDR" (bit-width 15)(formats decimal hex symbolic))
											   ("OPCODE" (bit-width 5)(value 2)))))
     ("wrap" (operands ("ADDR" "C"))(fields (("C" (bit-width 11)(formats s1.9 hex symbolic))("ADDR" (bit-width 15)(formats decimal hex symbolic))
											    ("OPCODE" (bit-width 5)(value 3)))))
     ("wlds" (operands ("N" "F" "A"))(fields (("MBZ" (bit-width 2))("N" (bit-width 1)(formats decimal hex))
								   ("A" (bit-width 15)(formats decimal hex symbolic))
								   ("OPCODE" (bit-width 5)(value #x12)))))
     ("wldr" (operands ("N" "F" "A"))(fields (("FIXED" (bit-width 2)(value 1))
					      ("N" (bit-width 1)(formats decimal hex))
					      ("F" (bit-width 16)(formats decimal hex symbolic))
					      ("MBZ" (bit-width 6))
					      ("A" (bit-width 2)(formats decimal-512s symbolic))("OPCODE" (bit-width 5)(value #x12)))))
    ("jam" (operands ("N"))(fields (("FIXED" (bit-width 25)(value 1))("N" (bit-width 1)(formats decimal hex))("OPCODE" (bit-width 5)(value #x13)))))
     ("cho" (operands ("RDA" "N" "C" "ADDR"))(fields (("MBZ" (bit-width 2))
						      ("C" (bit-width 6)(formats binary symbolic))
						      ("ADDR" (bit-width 15)(formats decimal hex symbolic))
						      ("OPCODE" (bit-width 5)(value #x14)))))
     ("cho" (operands ("SOF" "N" "C" "D"))(fields (("FIXED" (bit-width 2)(value 2))
						   ("C" (bit-width 6))("MBZ" (bit-width 1))
						   ("N" (bit-width 2)(formats decimal hex symbolic))
						   ("D" (bit-width 16)(formats s.15 symbolic))("OPCODE" (bit-width 5)(value #x14)))))
     ("cho" (operands ("RDAL" "N"))(fields (("FIXED" (bit-width 9)(value #x184))("N" (bit-width 2)(formats decimal hex symbolic))
										("MBZ" (bit-width 16))("OPCODE" (bit-width 5)(value #x14)))))
     ("cho" (operands ("TYPE" "N"))(fields (("FIXED" (bit-width 9)(value #x184))("N" (bit-width 2)(formats decimal hex symbolic))
										("MBZ" (bit-width 16))("OPCODE" (bit-width 5)(value #x14)))))
    ("clr" (operands ()) (fields (("MBZ" (bit-width 27))("OPCODE" (bit-width 5)(value #x1e)))))
    ("not" (operands ()) (fields (("FIXED" (bit-width 27)(value #x7fffff8))("OPCODE" (bit-width 5)(value #x10)))))
    ("absa" (operands ()) (fields (("MBZ" (bit-width 27))("OPCODE" (bit-width 5)(value #x9)))))
    ("ldax" (operands ("ADDR"))(fields (("ADDR" (bit-width 6)(formats decimal hex symbolic))("OPCODE" (bit-width 5)(value 5)))))))

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
(define *memory-map* (make-vector #x1000 #f)) ; vector for instructions for whole memory.
(define (ident-is-mnemonic? str)
  (hashtable-contains? *mnemonic-hash* str))

(define (hashtable->string hshtbl)
  (with-output-to-string (lambda ()
			   (display "(");
			   (hashtable-for-each hshtbl (lambda (key value)
							(print "(key " key " value " value ")")))
			   (print ")"))))

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

(define (resolve-to-number arg-in)
					;(print "(resolve-to-number " arg ")")
  (let ((rv (let* ((pair-in (pair? arg-in))
		   (arg (if (pair? arg-in)
			    (cadr arg-in)
			    arg-in)))
	      (if (number? arg)
		  (if pair-in
		      arg-in
		      arg)
		  (if (hashtable-contains? *symbol-table* arg)
					; this is all about the nonsense of letting use put "0" instead of "0.0" for a fixedpoint where a hex number is also allowed
		      (let ((resolved-number (resolve-to-number (hashtable-get *symbol-table* arg))))
			(if pair-in
			    (list (car arg-in) resolved-number)
			    resolved-number))
		      (begin
			(dump-trace-stack (current-output-port) 10)
			(error "resolve-to-number" "identifier not found in symbol table" arg)))))))
    rv))

(define (assoc-value key alist)
  (let ((rv-pair (assoc key alist)))
    (if rv-pair
	(cadr rv-pair)
	(begin
	  (dump-trace-stack (current-output-port) 10)
	  (error "assoc-value" "key was not found in alist (key  alist)" (list key alist))))))

(define (u->s number)
  (if (number? number)
      (unsigned->string number)
      number))

(define (u->s/pad number width)
  (let loop ((str (if (number? number)
		      (unsigned->string number)
		      number)))
    ;(print "(u->s/pad(str " str ")(len " (string-length str) ")(width " width "))")
    (if (< (string-length str) width)
	(loop (string-append "0" str))
	str)))

(define (->elong number)
  (if (number? number)
      (cond ((fixnum? number) (fixnum->elong number))
	    ((flonum? number) (flonum->elong number))
	    (else
	     (error "->elong" "Unsupported number type (number)" (list number))))
      (make-elong 0)))

(define *s1.14-lsb-value* 0.00006103516)
(define *s1.9-lsb-value* 0.001953125)
(define *s.10-lsb-value* 0.0009765625)

(define (s.15-range value)
  (if (and (number? value)
	   (<= value 0.999969482421875)
	   (>= value -1.0))
      #t
      #f))

(define (encode-s.15 value)
   (e-print "(encode-s.15 " value ")")
   ; (int)(num * 32768.0) & 0x7fff;
   (bit-andelong (->elong (* value 32768.0)) #x7fff))

(define (s1.14-range value)
  (if (and (number? value)
	   (<= value 1.99993896484)
	   (>= value -2.0))
      #t
      #f))

(define (encode-s1.14 value-in)
  (e-print "(encode-s1.14 " value-in ")")
  (let ((value (if (pair? value-in)
		   (cadr value-in)
		   value-in)))
    (e-print "(encode-s1.14(value " value ")")
    (if (s1.14-range value)
	;;(int)(num * (32768.0 / 2.0)) & 0xffff
	(bit-andelong (->elong (* value (/ 32768.0 2.0))) #xffff)
	(error "encode-s1.14" "value is not respresentable as s1.14 (value)" (list value)))))

(define (s1.9-range value)
  (if (and (number? value)
	   (<= value 1.998046875)
	   (>= value -2.0))
      #t
      #f))

(define (decode-s1.9 value)
   (/ (elong->flonum value) (/ 1024.0 2.0)))
    
(define (encode-s1.9 value-in)
   (e-print "(encode-s1.9 " value-in ")")
   (let ((value (if (pair? value-in)
		    (cadr value-in)
		    value-in)))
      (if (s1.9-range value)
	  ;; (num * (1024.0 / 2.0)) & 0x7ff;
	  (bit-andelong (->elong (* value-in (/ 1024.0 2.0))) #x7ff)
	  (error "encode-s1.9" "value is not respresentable as s1.9 (value)" (list value)))))

(define (s.10-range value)
  (if (and (number? value)
	   (<= value 0.9990234375)
	   (>= value -1.0))
      #t
      #f))
  
(define (encode-s.10 value-in)
   (e-print "(encode-s.10 " value-in ")")
   (let ((value (if (pair? value-in)
		    (cadr value-in)
		    value-in)))
      (if (s.10-range value)
	  ;(num * 1024.0) & 0x7ff;
	  (bit-andelong (->elong (* value 1024.0)) #x7ff)
	  (error "encode-s.10" "value is not respresentable as s.10 (value)" (list value)))))

(define (encode-value tagged-value formats shift mask)
   ;(print "(encode-value " value " " formats " " shift " " (u->s mask) ")")
   (let* ((tag (if (pair? tagged-value)
		   (car tagged-value)
		   #f))
	  (value (if tag
		     (cadr tagged-value)
		     tagged-value)))

      (let ((val (cond ((and (or (member 's1.14 formats))
			     (or (real? value)
				 (eq? tag 'decimal)))
			(encode-s1.14 value))
		       ((and (or (member 's1.9 formats))
			     (or (real? value)
				 (eq? tag 'decimal)))
			     (encode-s1.9 value))
			((and (or (member 's.10 formats))
			      (or (real? value)
				  (eq? tag 'decimal)))
			 (encode-s.10 value))
			((and (or (member 'decimal formats)
				  (member 'hex formats))
			      (or (integer? value)
				  (eq? tag 'decimal)))
			 (if (pair? value)
			     (->elong (cadr value))
			     (->elong value)))
			(else
			 (error "encode-value" "Unsupported type (line-number value formats)" (list *line-number* value formats))))))
	    (bit-lshelong (bit-andelong mask val) shift))))

(define (process-mnemonic mnemonic . args)
   (n-print "(process-mnemonic " mnemonic " " args ")")
   (let* ((instruction-motif (assoc mnemonic *spin-instructions*))
	  (operands (assoc-value 'operands (cdr instruction-motif)))
	  (operand-map (create-hashtable))
	  (fields (assoc-value 'fields (cdr instruction-motif)))
	  (encoding (make-elong 0))
	  (current-bit 31))
      (n-print "(process-mnemonic(line " (- *line-number* 1) ")(address " (unsigned->string *current-address*) ")(mnemonic " mnemonic ")(args " args
	       ")(operands " operands ")(fields " fields "))")
      (when (not (= (length operands) (length args)))
	 (error "process-mnemonic" "operand length mismatch args length (line mnemonic operands args)" (list *line-number* mnemonic operands args)))
      (let ((operand-index 0))
	 (for-each (lambda (operand)
		      (n-print "(operand " operand ")")
		      (hashtable-put! operand-map operand operand-index)
		      (set! operand-index (+ 1 operand-index)))
		   operands))
      (n-print "(operand-map " (hashtable->string operand-map) ")")
      (for-each (lambda (field-descriptor)
		   (n-print "(field-descriptor " field-descriptor ")")
		   (let* ((name (car field-descriptor))
			  (bit-width (assoc-value 'bit-width (cdr field-descriptor)))
			  (formats (let ((rrr (assoc 'formats (cdr field-descriptor))))
				      (if rrr
					  rrr
					  (if (or (string=? "MBZ" name)
						  (string=? "FIXED" name)
						  (string=? "OPCODE" name))
					      '(decimal)
					      #f))))
			  (operand-index (hashtable-get operand-map name))
			  (op-arg (if operand-index
				      (list-ref args operand-index)
				      #f))
			  (op-value (if (string=? "MBZ" name)
					(begin
					   (n-print "(process-mnemonic(MBZ force value to 0))")
					   0)
					(let ((rrr (assoc 'value (cdr field-descriptor))))
					   (if rrr
					       (cadr rrr)
					       #f))))
			  (mask (- (bit-lshelong 1 bit-width) 1))
			  (shift (if (> (- bit-width 1) current-bit)
				     (error "process-mnemonic" "bit-width > current-bit (line-number mnemonic name) " (list *line-number* mnemonic name))
				     (- current-bit (- bit-width 1))))
			  (enc (begin
				  (n-print "(process-mnemonic(name " name ")(op-arg " op-arg ")(op-value " op-value "))")
				  (encode-value (if op-arg op-arg op-value) formats shift mask))))

		      (n-print "((name " name ")(current-bit " current-bit ")(bit-width " bit-width ")(op-index " operand-index ")(op-arg " op-arg ")(op-value " op-value
			       ")(mask " (u->s mask) ")(shift " shift ")(enc " (u->s enc) "))")
		      (set! encoding (bit-orelong encoding enc))
		      (set! current-bit (- current-bit bit-width))))
		fields)
      (n-print "(process-mnemonic(current-bit " current-bit "))")
      (when (not (= current-bit -1))
	 (error "process-menmonic" "current-bit not -1, something is probably wrong with instruction motif (line-number mnemonic)"
		(list *line-number* mnemonic)))
      (vector-set! *memory-map* (/ *current-address* 4) encoding)
      (set! *current-address* (+ 4 *current-address*))))

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
     ((: digit "." (* digit))   (begin
				  (cons 'SIGNED-FIXED-POINT (string->number (the-string)))))
     ((: "$" (+ xdigit))                            (begin
						      (let* ((hex-digits (list->string (cdr (string->list (the-string)))))
							     (the-number (string->number hex-digits 16)))
							(cons 'HEX the-number))))
     ((+ digit)                                     (begin
						      (cons 'DECIMAL (string->number (the-string))))) ; decimal may be converted to signed fixed point
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
   (NEWLINE LABEL SIGNED-FIXED-POINT DECIMAL HEX BIT-VECTOR OR MINUS MNEMONIC PLUS EQU MEM REG IDENT COMMA)
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
    ((MINUS DECIMAL) (begin
				  (d-print *line-number* "(MINUS SIGNED-FIXED-POINT)->value")
				  (list 'decimal (- 0 DECIMAL))))
    ((DECIMAL)         (begin
			 (d-print *line-number* "(DECIMAL)->value")
			 (list 'decimal DECIMAL)))
    ((HEX)             (begin
			 (d-print *line-number* "(HEX)->value")
			 HEX)))
   (ident-reg-or-value
    ((reg-or-value) (begin
		      (d-print *line-number* "(reg-or-value)->ident-reg-or-value(reg-or-value " reg-or-value ")")
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
    ((IDENT) (d-print *line-number* "(IDENT)->ident-reg-or-value") (resolve-to-number IDENT)))
   (reg-or-value
    ((value) (d-print *line-number* "(value)->reg-or-value(value " value ")") value)
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

(define (intel-hex-checksum byte-count address record-type data)
  (when (> byte-count 8)
      (error "intel-hex-checksum" "byte-count > 8 not supported by this function (byte-count)" (list byte-count)))
  (let ((sum byte-count))
    (set! sum (bit-andelong #xff (+ sum (bit-andelong #xff address))))
    (set! sum (bit-andelong #xff (+ sum (bit-andelong #xff (bit-rshelong address 8)))))
    (let loop ((counter 0))
;      (print "(sum " sum ")")
      (when (< counter byte-count)
	    (begin
	      (set! sum (bit-andelong #xff (+ sum (bit-andelong #xff (bit-rshelong data (* counter 8))))))
	      (loop (+ counter 1)))))
    (- 0 sum)))

(define (intel-hex-record byte-count address record-type data)
; A record (line of text) consists of six fields (parts) that appear in order from left to right:

; Start code, one character, an ASCII colon ':'.

; Byte count, two hex digits, indicating the number of bytes (hex digit pairs) in the data field. The maximum byte
; count is 255 (0xFF). 16 (0x10) and 32 (0x20) are commonly used byte counts.

; Address, four hex digits, representing the 16-bit beginning memory address offset of the data. The
; physical address of the data is computed by adding this offset to a previously established base
; address, thus allowing memory addressing beyond the 64 kilobyte limit of 16-bit addresses. The base
; address, which defaults to zero, can be changed by various types of records. Base addresses and
; address offsets are always expressed as big endian values.

; Record type (see record types below), two hex digits, 00 to 05, defining the meaning of the data
; field.

; Data, a sequence of n bytes of data, represented by 2n hex digits. Some records omit this field (n
; equals zero). The meaning and interpretation of data bytes depends on the application.

; Checksum, two hex digits, a computed value that can be used to verify the record has no errors.

; record's checksum byte is the two's complement (negative) of the least significant byte (LSB) of
; the sum of all decoded byte values in the record preceding the checksum. It is computed by summing
; the decoded byte values and extracting the LSB of the sum (i.e., the data checksum), and then
; calculating the two's complement of the LSB (e.g., by inverting its bits and adding one).

; For example, in the case of the record :0300300002337A1E, the sum of the decoded byte values is 03 +
; 00 + 30 + 00 + 02 + 33 + 7A = E2. The two's complement of E2 is 1E, which is the checksum byte
; appearing at the end of the record.

; The validity of a record can be checked by computing its checksum and verifying that the computed
; checksum equals

; the checksum appearing in the record; an error is indicated if the checksums differ. Since the
; record's checksum byte is the negative of the data checksum, this process can be reduced to summing
; all decoded byte values - including the record's checksum - and verifying that the LSB of the sum
; is zero.

  (let ((data-record              0)
	(end-of-file              1)
	(extended-segment-address 2)
	(start-segment-address    3)
	(extended-linear-address  4)
	(start-linear-address     5))
    
    (with-output-to-string
      (lambda ()
	(display* ":" (u->s/pad byte-count 2) (u->s/pad address 4) (u->s/pad record-type 2) (u->s/pad data (* byte-count 2))
		  (u->s/pad (bit-andelong #xff (intel-hex-checksum byte-count address record-type data)) 2))))))

(define (start argv)
   ;(print argv)
  (for-each (lambda (predefined-symbol-pair)
	      (hashtable-put! *symbol-table* (car predefined-symbol-pair) (cadr predefined-symbol-pair)))
	    *predefined-symbols*)
  (for-each (lambda (inst)
	      (hashtable-put! *mnemonic-hash* (car inst) #t))
	    *spin-instructions*)
  (read/lalrp *spin-g* *spin-l* (current-input-port))
  ; pad to end of slot with SKP 0,0
  (let loop ((addr *current-address*))
     ;(print "(start - pad to end of slot - current-address " (u->s *current-address*) ")")
     (when (< *current-address* (* 4 128))
	(process-mnemonic "skp" 0 0)
	(loop *current-address*)))
  (let ((addr (make-elong 0))
	(data-record (make-elong 0)))
    (bind-exit
     (quit)
     (for-each (lambda (data)
		 (if data
		     (begin
		       (print (string-upcase (intel-hex-record 4 addr data-record data)))
		       (set! addr (+ 4 addr)))
		     (quit #f)))
	       (vector->list *memory-map*))))
  (print ":00000001ff"))

