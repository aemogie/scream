(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 format))

(define (transpile expr)
  (match expr
    ;; literals
    ((? string? str) (format #f "\"~a\"" str))
    ((? number? num) (format #f "~d" num))
    ((? symbol? sym) (format #f "~a" sym))
    ((? char? char) (format #f "'~a'" char))

    ;; preprocessor
    (('include file)
     (cond
      ((and (symbol? file)
	    (string-prefix? "<" (symbol->string file))
	    (string-suffix? ">" (symbol->string file)))
       (format #f "#include ~a" file))
      ((string? file)
       (format #f "#include \"~a\"" file))
      (#t (error "invalid include"))))

    ;; special
    (('fn (type name (? list? pars) ...) body ...)
     (format #f "~a ~a(~{~{~a~^ ~}~^, ~}) {~%~{~/~a;~%~}}"
	     type name pars (map transpile body)))

    ((or ('+ x y) ('- x y) ('* x y) ('/ x y))
     (format #f "(~a ~a ~a)" (transpile x) (car expr) (transpile y)))
    (('return expr) (format #f "return ~a" (transpile expr)))

    ;; function call (fallback expr)
    ((fn args ...)
     (format #f "~a(~{~a~^, ~})" fn (map transpile args)))))

(define (read-all port)
  (let go ((curr (read port))
	    (acc '()))
    (if (not (eof-object? curr))
	(go (read port) (append acc (list curr)))
	acc)))

(define (main args)
  (match args
    ((_ input "-o" output)
     (call-with-input-file input
       (lambda (input)
	 (call-with-output-file output
	   (lambda (output)
	     (define source (read-all input))
	     (define transpiled (string-join (map transpile source) "\n"))
	     (display transpiled output))))))
    ((guile input) (main (list guile input "-o" "out.c")))
    ((guile) (main (list guile "example.sc" "-o" "out.c")))))
(main (program-arguments))
