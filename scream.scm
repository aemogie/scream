(use-modules (srfi srfi-1))

(define (join-symbols-with-spaces symbols)
  (if (not (null? symbols))
      (fold (lambda (next prev)
	      (string-append prev " " (symbol->string next)))
	    (symbol->string (car symbols))
	    (cdr symbols))
      ""))

(define (transpile-fn-signature signature)
  (define ret-type (symbol->string (car signature)))
  (define name (symbol->string (cadr signature)))
  (define params
    (map join-symbols-with-spaces (cddr signature)))
  (define params-joined
    (fold
     (lambda (param prev)
       (string-append prev "," param))
     (car params)
     (cdr params)))
  (string-append ret-type " " name "(" params-joined ")"))

(define (transpile-fndef fn)
  (define signature (transpile-fn-signature (car fn)))
  (define body (map transpile (cdr fn)))
  (define body-str
    (fold (lambda (next prev) (string-append prev ";" next ";"))
	  (car body)
	  (cdr body)))
  (string-append signature "{" body-str "}"))

(define (transpile-fncall name args)
  (define args-joined
    (if (not (null? args))
	(fold
	 (lambda (arg prev)
	   (string-append prev "," arg))
	 (transpile (car args))
	 (map transpile (cdr args)))
	""))
  (string-append (symbol->string name) "(" args-joined ")"))


(define (transpile-invoke expr)
  (define rator (car expr))
  (define rand (cdr expr))

  (cond
   ((eq? rator 'include)
    ;; TODO: i cant exactly recursively parse all files, but maybe
    ;; manually resolve the include for one level? i want to at some
    ;; point make this language to a full blown preprocessor, instead
    ;; of relying on C's preprocessor
    (string-append "#include " (car rand) "\n"))
   ((eq? rator 'fn)
    (transpile-fndef rand))
   ((eq? rator 'return)
    (if (not (null? rand))
	(string-append "return " (transpile (car rand)) ";")
	"return;"))
   ((member rator '(+ - * /))
    (let ((op (symbol->string rator))
	  (x (transpile (car rand)))
	  (y (transpile (cadr rand))))
      (string-append "(" x op y ")")))
   (#t (transpile-fncall rator rand))))

(define (transpile expr)
  (cond
   ((pair? expr) (transpile-invoke expr))
   ((integer? expr) (number->string expr))
   ((symbol? expr) (symbol->string expr))
   ((string? expr) (string-append "\"" expr "\""))
   (#t (error (format #f "TODO: Haven't implemented primitive `~:a'" expr)))))

;; TODO: make the filenames CLI arguments
(call-with-output-file "out.c"
  (lambda (out)
    (display
     (call-with-input-file "source.sc"
       (lambda (source)
	 (let loop ((src "")
		    (expr (read source)))
	   (if (not (eof-object? expr))
	       (loop (string-append src (transpile expr) ";") (read source))
	       src))))
     out)))
