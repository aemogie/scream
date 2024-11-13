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

;; TODO: make the filenames CLI arguments
(call-with-output-file "out.c"
  (lambda (out)
    (display
     (call-with-input-file "source.sc"
       (lambda (source)
	 (let loop ((src "")
		    (expr (read source)))
	   (if (not (eof-object? expr))
	       (loop (string-append src (transpile expr) "\n") (read source))
	       src))))
     out)))
