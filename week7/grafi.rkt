;BFS DFS
;BFS 
(define (BFS v G)
  (define (helper res queue visited)
    (if (null? queue) res
        (let* [(curr (car queue))
               (next-visited (cons curr visited))
               (to-push (filter (lambda (x) (and (not (member x queue))
                                                 (not (member x visited))))
                                (neighbours curr G)))
               (next-queue (append (cdr queue) to-push))
               (next-res (append res (list curr)))]
          (helper next-res next-queue next-visited))))
  (helper '() (list v) (list v)))
                                  
;DFS
;Smenq se queue sus stack, imeto na f-ciqta i se razmenqt dve
(define (DFS v G)
  (define (helper res stack visited)
    (if (null? stack) res
        (let* [(curr (car stack))
               (next-visited (cons curr visited))
               (to-push (filter (lambda (x) (and (not (member x stack))
                                                 (not (member x visited))))
                                (neighbours curr G)))
               (next-stack (append to-push (cdr stack)));Tuk se razmenqt 2
               (next-res (append res (list curr)))]
          (helper next-res next-stack next-visited))))
  (helper '() (list v) (list v)))
           
    