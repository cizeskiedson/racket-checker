#lang racket
(require 2htdp/batch-io)

;;funcao que analisa a metrica do numero de comentarios no arquivo
(define (calcula-comentarios arq)
  (map acha_comentario arq)
  )

(define (acha_comentario string)
  (cond
    [(string-prefix? string ";") 1]
    [(string-prefix? string "#;") 1]
    [(string-prefix? string "#!") 1]
    [(string-prefix? string "#|") 1]
    [else 0]
  ))
;; funcao que retorna a lista com os valores calculados pelas metricas
(define (avalia-codigo arq nomeArq)
  (printf "\nCodigo analisado: ~a\n" nomeArq)
  (printf "\nMetricas calculadas: ~a"
             (list (string-append "\nNumero de linhas: " (~v(length arq))) ;;metrica 1 do total de linhas do arquivo
                   (string-append "\nNumero de comentarios: " "teste")
                   (string-append "\nNumero de funcoes: " "teste2"))
             )
  )

;; funcao que le os arquivos
(define (le-arquivos arq)
  (avalia-codigo(read-lines arq) arq))            