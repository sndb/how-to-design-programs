;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex232) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
`(1 "a" 2 #false 3 "c")
(list `1 `"a" `2 `#false `3 `"c")
(list 1 "a" 2 #false 3 "c")

`(("alan" ,(* 2 500))
  ("barb" 2000)
  (,(string-append "carl" " , the great") 1500)
  ("dawn" 2300))
(list `("alan" ,(* 2 500))
      `("barb" 2000)
      `(,(string-append "carl" " , the great") 1500)
      `("darn" 2300))
(list (list `"alan" `,(* 2 500))
      (list `"barb" `2000)
      (list `,(string-append "carl" " , the great") `1500)
      (list `"darn" `2300))
(list (list "alan" (* 2 500))
      (list "barb" 2000)
      (list (string-append "carl" " , the great") 1500)
      (list "darn" 2300))
(list (list "alan" 1000)
      (list "barb" 2000)
      (list "carl , the great" 1500)
      (list "dawn" 2300))

(define title "ratings")
`(html
  (head
   (title ,title))
  (body
   (h1 ,title)
   (p "A second web page")))
(list `html
      `(head
        (title ,title))
      `(body
        (h1 ,title)
        (p "A second web page")))
(list `html
      (list `head
            `(title ,title))
      (list `body
            `(h1 ,title)
            `(p "A second web page")))
(list `html
      (list `head
            (list `title `,title))
      (list `body
            (list `h1 `,title)
            (list `p `"A second web page")))
(list `html
      (list `head
            (list `title title))
      (list `body
            (list `h1 title)
            (list `p "A second web page")))
(list 'html
      (list 'head
            (list 'title title))
      (list 'body
            (list 'h1 title)
            (list 'p "A second web page")))