(define :words:
  (list 
   "Spurs"
   "Out"
   "Interview"
   "Yao"
   "Rockets"
   "Celtics"
   "Gorlick"
   "Injury"
   "League"
   "Sign"
   "Zheng"
   "Contract"
   "Championship"
   "Kevin Garnett"
   "NBA"
   "Salary"
   "Kobe"
   "Basketball"
   "Justin"
   "Source"
   "Duncan"
   "Nike"))

(define (random-cloud words n)
  (map (lambda (word) (cons word (random-integer n))) words))

(define (cloud-to-json cloud)
  (let* ((counts
	  (map
	   (lambda (pair) (format "\t{name:\"~a\", count:~d}" (car pair) (cdr pair)))
	   cloud))
	 (intermediate (string-join counts ",\n"))
	 (final (string-append "{items:[\n" intermediate "\n]}")))
    final))

(define (cloud/json)
  (cloud-to-json (random-cloud :words: 30)))


