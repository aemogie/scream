;; -*- mode: scheme; -*- close enough

(include "<stdio.h>")

(fn (int add (int x) (int y))
  (return (+ x y)))

(fn (char* greet ())
  (return "Hello, World!"))

(fn (int main (int argc) (char** argv))
  (printf "%s %d" (greet) (add 3 4))
  (return 0))
