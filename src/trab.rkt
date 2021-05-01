#lang racket
(require 2htdp/batch-io)

;;funcao que analisa a metrica do numero de imports do arquivo
(define (calcula-imports arq)
  (soma-todos (map acha-imports arq))
  )

(define (acha-imports string)
  (cond
    [(string-prefix? string "(require") 1]
    [else 0]
    )
  )

;;funcao que analisa a metrica do numero de comentarios no arquivo
(define (calcula-comentarios arq)
  (soma-todos (map acha-comentario arq))
  )

(define (soma-todos lista)
 (foldr + 0 lista))

(define (acha-comentario string)
  (cond
    [(string-prefix? string ";") 1]
    [(string-prefix? string "#;") 1]
    [(string-prefix? string "#!") 1]
    [(string-prefix? string "#|") 1]
    [else 0]
  ))

;;funcao que analisa a metrica do numero de funcoes do arquivo
(define (calcula-funcoes arq)
  (soma-todos (map acha-funcoes arq))
  )

(define (acha-funcoes string)
  (cond
    [(string-prefix? string "(define") 1]
    [else 0]
  ))

;;funcao que analisa a metrica do numero de linhas de uma funcao do arquivo
(define (conta-linhas arq indices_funcoes)
  (for/list([i indices_funcoes])
    (cond [(equal? (length indices_funcoes) 1) (- (length arq) i)]
          [else (lambda() (percorre-funcao(split-at arq i)))])
          )
  )

(define (linhas-funcao arq)
  (conta-linhas arq (indexes-of(map acha-funcoes arq) 1))
  )

(define (percorre-funcao lista)
  (map acha-fim lista)
  )

(define (acha-fim string)
  (cond
    [(string-prefix? string "(define") 1]
    [(equal? (acha-comentario string) 1) 1]
    [(string-prefix? string "") 1]
    [else 0]
  ))


;; funcao que retorna a lista com os valores calculados pelas metricas
(define (avalia-codigo arq nomeArq)
  (printf "\nCodigo analisado: ~a\n" nomeArq)
  (printf "\nMetricas calculadas: ~a"
             (list (string-append "\nNumero de linhas: " (~v(length arq))) ;;metrica 1 do total de linhas do arquivo
                   (string-append "\nNumero de comentarios: " (~v(calcula-comentarios arq)))
                   (string-append "\nNumero de funcoes: " (~v(calcula-funcoes arq)))
                   (string-append "\nNumero de imports: " (~v(calcula-imports arq)))
                   (string-append "\n"))
             )
  (gera-pontos (length arq) (calcula-comentarios arq) (calcula-funcoes arq) (calcula-imports arq))
  )
;; funcao que calcula os pesos e gera a pontuacao
(define (gera-pontos m1 m2 m3 m4)
  (printf "\nPontuacao: ~a" (+ (* m1 0.4) (* m2 0.2) (* m3 0.3) (* m4 0.1))
  ))

;; funcao que le os arquivos
(define (le-arquivos)
  (let ([arquivos (map path->string (directory-list "files/" #:build? #t))])
  (map (lambda (arq) (avalia-codigo (read-lines arq) (string-replace arq "/files" ""))) arquivos)
 ))

(define (get-nome string)
  (string-replace string "/files" ""))
