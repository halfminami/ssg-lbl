(define-module lib.gen-all
  (use file.util)

  (use lib.util :prefix u:)
  (use lib.frontmatter :only (read-frontmatter!))
  (use lib.markdown :only (markdown->html!))
  (use lib.html :only (html->html))
  (use lib.defaults :prefix d:)
  (use lib.commandopt)
  (export-all))

(define-module lib.gen
  (import lib.gen-all)
  (export generate input-path->output-path))

(select-module lib.gen-all)

;; frontmatter has `special` which contains 'special 'children
(define (make-special path children env)
  (hash-table-from-pairs 'eq?
                         `(special . ((path . ,path)
                                      (output-path . ,(input-path->output-path path env))))
                         `(children . ,children)))

(define (read-drop-frontmatter! in env)
  (let1 m (make-module #f)
    (eval `(define env ,env) m)
    (let* ([res (read-frontmatter! in m)]
           [fm  (u:as-> (car res) data
                        (or (and data (car data)) '())
                        (map (^p
                              (cons (x->string (car p)) (cdr p)))
                             data)
                        (hash-table-from-pairs 'eq? `(user . ,data)))]
           [in  (cadr res)])
      (u:hash-table/alist-union! fm env)
      `(,in ,fm))))

(define (frontmatter-then-content parse!)
  (^ (in out env)
    (let* ([res (read-drop-frontmatter! in env)]
           [in  (car res)]
           [fm  (cadr res)])
      (parse! in out fm)
      fm)))

(define (read-md! in out env)
  (let* ([env (hash-table-copy env)]
         [toc (markdown->html! in out (hash-table-copy env))])
    (hash-table-update! env
                        'special
                        (cut alist-update-in <> '(toc) (^l (append l toc)) eq? '())
                        '())
    env))

(define (read-html! in out env)
  (html->html in out env)
  env)

(define (which-read path)
  (let1 ext (path-extension path)
    (cond [(string=? "md" ext) read-md!]
          [(string=? "html" ext) read-html!]
          [else (raise (error (string-append "extension " ext " is not supporsed to be parsed")))])))

(define (parsable ext) (u:equal-one-of? ext "html" "md"))

(define (input-path->output-path path env)
  (let* ([config  (hash-table-get env 'config '())]
         [in-dir  (alist-ref config 'in eq? d:config-in)]
         [out-dir (alist-ref config 'out eq? d:config-out)])
    (let-values ([(dir name ext) (decompose-path path)])
      (let1 out-dir ($ string-append out-dir $ substring dir (string-length in-dir) -1)
        (if (parsable ext)
          ;; dir/name.md => dir/name/index.html
          (let ([out-dir (if (string=? "index" name) out-dir (string-append out-dir "/" name))]
                [name    "index"]
                [ext     "html"])
            #"~|out-dir|/~|name|.~|ext|")
          #"~|out-dir|/~|name|.~|ext|")))))

(define (ensure-path path)
  (define-values (dir a b) (decompose-path path))
  (make-directory* dir))

;; convert all files under `path` directory, returns list of config
(define (generate path env)
  (define-values (dir files) (directory-list2 path :children? #t :add-path? #t))
  (define children (map (^p `(,p . ,(generate p env))) dir))
  (define template (and-let* ([config (hash-table-get env 'config #f)]
                              [re     (alist-ref config 'template)])
                     (find (pa$ rxmatch re) files)))
  (when template (set! files (remove (pa$ string=? template) files))) ; ??
  (verbose-print (string-join `(,#"parsing directory ~|path|"
                                ,(if template (string-join `("with template" ,template)) "without template"))))
  (u:->> files
         (map (^p
               (let1 should-parse? (parsable (path-extension p))
                 (cond
                  [(and should-parse? template)
                   (call-with-input-file template
                     (^i (convert/template (pa$ (frontmatter-then-content (which-read template)) i)
                                           p
                                           children
                                           env)))]
                  [should-parse?
                   (call-with-input-string "{{content}}"
                     (^i (convert/template (pa$ (frontmatter-then-content html->html) i)
                                           p
                                           children
                                           env)))]
                  [else
                   (let1 output-path (input-path->output-path p env)
                     (ensure-path output-path)
                     (copy-file p output-path :if-exists :supersede))]))))
         (filter identity)))

;; I wanted to manipulate output-path via frontmatter, but it requires evaluating frontmatter first, mutating input port
;; html or md only
(define (convert/template template$ path children env)
  (let ([env     (hash-table-copy env)]
        [special (make-special path children env)])
    (u:hash-table/alist-union! special env)
    (call-with-input-file path
      (^ (in)
        (let* ([res         (read-drop-frontmatter! in special)]
               [in          (car res)]
               [fm          (cadr res)]
               [output-path (input-path->output-path path fm)]
               [content!    (^ (out env)
                              (verbose-print #"parsing file ~|path| with env 'user ~(hash-table-get env 'user #f)")
                              ((which-read path) in out env))])
          (ensure-path output-path)
          
          (u:hash-table/alist-union! fm special)
          (hash-table-set! fm 'content! content!)

          (hash-table-update! fm 'special (cut alist-adjoin <> 'input-path path) '())
          (hash-table-update! fm 'special (cut alist-adjoin <> 'output-path output-path) '())
          
          (call-with-output-file output-path
            (^ (out)
              (verbose-print #"writing to ~|output-path|")
              (template$ out fm))))))))
