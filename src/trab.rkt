#lang racket
(require 2htdp/batch-io)
(require rackunit)
(require rackunit/text-ui)

;; slice (lista inicio n) -> Lista: uma lista. Inicio: indice inical. N: nro de posicoes desejadas para o recorte
;; Funcao que recorta uma lista em uma sublista
(define (slice lista inicio n)
  (take (drop lista inicio) n))


;; indices-funcao(arq) -> Arq: Lista de Strings
;; Funcao que retorna uma lista os indices de todas as funcoes de um arquivo
(define (indices-funcao arq)
  (indexes-of  (map acha-funcoes arq) 1))

;; conta-linhas-funcao (arq) -> Arq: uma lista de strings
;; Funcao pega os indices de todas as funcoes do arquivo
;; E retorna a quantidade de funcoes com mais de cinco linhas
(define (conta-linhas-funcao arq lista-indices acc)
  (define (recursao arq lista)
    (define x (mais-que-5 arq (first lista)))
    (cond [(equal? x 1) (conta-linhas-funcao arq (rest lista) (add1 acc))]
          [else (conta-linhas-funcao arq (rest lista) acc)])
    )
    (cond
      [(empty? lista-indices) acc]
      [else (recursao arq lista-indices)])
  )

;; mais-que-5 (arq indice) -> Arq: uma lista de strings. Indice: um valor inteiro
;; Funcao pega a partir de um indice as 5 proximas strings
;; Calcula a quantidade de parenteses e retorna 1 para caso tenha mais de cinco linhas
;; Ou 0 caso nao tenha
(define (mais-que-5 arq indice)
  (define string-bonita (string-join (cond
                                       [(> (+ 5 indice) (length arq)) (slice arq indice (- (length arq) indice))]
                                       [else (slice arq indice 5)])))
  (define res (my-sum (map conta-parenteses(string->list string-bonita)) 0 0))
  (cond
    [(positive? res) 1]
    [(negative? res) 0]
    [else 0])
  )
;; my-sum (lista acc flag) -> Lista: uma lista de inteiros. Acc: acumulador. Flag: uma flag para dizer se eh a primeira iteracao ou nao
;; Recursivamente soma todos os valores de uma lista (parecido com o foldr) mas para se chegar a 0 parcialmente, retornando -1
(define (my-sum lista acc flag)
  (cond
    [(empty? lista) acc]
    [(and (equal? acc 0) (positive? flag)) -1]
    [else (my-sum (rest lista) (+ acc (first lista)) (add1 flag))]
  ))


;; conta-parenteses (char) -> Char: um caractere
;; Funcao verifica se um caractere eh um parenteses aberto ou fechado
(define (conta-parenteses char)
  (cond
    [(equal? char #\() 1]
    [(equal? char #\)) -1]
    [else 0]))

;; soma-todos(lista) -> Lista: uma lista (vetor)
;; Funcao que soma todos os itens de uma lista e retorna um valor
(define (soma-todos lista)
 (foldr + 0 lista))

;;calcula-imports(arq) -> Arq: uma lista de strings
;;Funcao que calcula a metrica do numero de imports do arquivo
(define (calcula-imports arq)
  (soma-todos (map acha-imports arq))
  )

;;acha-imports(string) -> String: sequencia de caracteres
;;Funcao que analisa se uma string corresponde a um import ou nao
(define (acha-imports string)
  (cond
    [(string-prefix? string "(require") 1]
    [else 0]
    )
  )

;;calcula-comentarios(arq) -> Arq: uma lista de strings
;;Funcao que calcula a metrica do numero de comentarios no arquivo
(define (calcula-comentarios arq)
  (soma-todos (map acha-comentario arq))
  )


;; acha-comentario(string) -> String: sequencia de caracteres que corresponde a uma linha do arq
;; Funcao que retorna se uma string corresponde a um comentario ou nao
(define (acha-comentario string)
  (cond
    [(string-contains? (string-trim string) ";") 1]
    [(string-contains? (string-trim string) ";;") 1]
    [(string-contains? (string-trim string) "#;") 1]
    [(string-contains? (string-trim string) "#!") 1]
    [(string-contains? (string-trim string) "#|") 1]
    [else 0]
  ))

;;calcula-funcoes (arq) -> Arq: lista de strings
;;Funcao que calcula a metrica do numero de funcoes do arquivo
(define (calcula-funcoes arq)
  (soma-todos (map acha-funcoes arq))
  )

;; acha-funcoes(string) -> String: sequencia de caracteres que corresponde a uma linha do arq
;; Funcao que retorna se uma string corresponde a uma funcao ou nao
(define (acha-funcoes string)
  (cond
    [(string-prefix? (string-trim string) "(define (") 1]
    [else 0]
  ))

;;funcao que analisa a metrica do numero de linhas de uma funcao do arquivo

;; calcula-linhas(arq) -> Arq: lista de strings
;; Metrica do nro de linhas do arquivo
(define (calcula-linhas arq)
  (length arq))

;; avalia-codigo (arq nomeArq) onde arq eh uma lista de strings e nomeArq eh uma string
;; Funcao que recebe uma lista de strings correspondente a um arquivo
;; e retorna uma lista com as metricas calculadas para um arquivo
(define (avalia-codigo arq nomeArq)
  (printf "\nCodigo analisado: ~a\n" nomeArq)
  (printf "\nMetricas calculadas: ~a"
             (list (string-append "\nNumero de linhas: " (~v(calcula-linhas arq))) ;;metrica 1 do total de linhas do arquivo
                   (string-append "\nNumero de comentarios: " (~v(calcula-comentarios arq)))
                   (string-append "\nNumero de funcoes: " (~v(calcula-funcoes arq)))
                   (string-append "\nNumero de imports: " (~v(calcula-imports arq)))
                   (string-append "\nNumero de funcoes com mais de 5 linhas: " (~v(conta-linhas-funcao arq (indices-funcao arq) 0)))
                   (string-append "\n"))
             )
  (gera-pontos (calcula-linhas arq) (calcula-comentarios arq) (calcula-funcoes arq) (calcula-imports arq) (conta-linhas-funcao arq (indices-funcao arq) 0))
  )
;; gera-pontos(x y z w) onde (X -> Inteiro) (Y -> Inteiro) (Z -> Inteiro) (W -> Inteiro)
;; gera-pontos (x y z w) -> Pontuacao (Float)
;; Funcao que calcula os pesos de cada metrica e gera a pontuacao
(define (gera-pontos m1 m2 m3 m4 m5)
  (printf "\nPontuacao: ~a\n" (+ (* m1 0.3) (* m2 0.1) (* m3 0.2) (* m4 0.1) (* m5 0.3)))
  0)


;; le-arquivos ... -> Void
;; Funcao responsavel por ler os arquivos do diretorio "files/"
;; Eh o fluxo principal do programa
(define (le-arquivos)
  (define (rkt? string) (string-suffix? string ".rkt"))
  (define folder (map path->string (directory-list "files/" #:build? #t)))
  (define rkt-folder(filter rkt? folder))
  (let ([arquivos rkt-folder])
  (map (lambda (arq) (avalia-codigo (read-lines arq) (string-replace arq "files/" ""))) arquivos))
  )

;; comentarios-test ... -> Void
;; Teste unitario das funcoes relacionadas ao nro de comentarios do arq
(define comentarios-test
  (test-suite "Testes para a metrica de comentarios"
              (check-equal? (acha-comentario ";;comentario teste 1") 1)
              (check-equal? (acha-comentario "(define (teste string))") 0)
              (check-equal? (calcula-comentarios (read-lines "files/queue3.rkt")) 7)))


;;funcoes-test ... -> Void
;; Teste unitario das funcoes relacionadas ao nro de funcoes do arq
(define funcoes-test
  (test-suite "Testes para a metrica de nro de funcoes"
              (check-equal? (acha-funcoes ";;comentario teste 1") 0)
              (check-equal? (acha-funcoes "(define (teste string))") 1)
              (check-equal? (acha-funcoes "(define-struct") 0)
              (check-equal? (calcula-funcoes (read-lines "files/queue3.rkt")) 9)))

;;imports-test ... -> Void
;; Teste unitario das funcoes relacionadas ao nro de imports do arq
(define imports-test
  (test-suite "Testes para a metrica de nro de imports"
              (check-equal? (acha-imports ";;comentario teste 1") 0)
              (check-equal? (acha-imports "(define (teste string))") 0)
              (check-equal? (acha-imports "(define-struct") 0)
              (check-equal? (acha-imports "(require") 1)
              (check-equal? (calcula-imports (read-lines "files/queue3.rkt")) 0)
              (check-equal? (calcula-imports (read-lines "files/queue2.rkt")) 1)))

;; tamanho-test ... -> Void
;; Teste unitario das funcoes relacionadas ao nro de linhas do arquivo
(define tamanho-test
  (test-suite "Testes para a metrica do nro de linhas do arquivo"
              (check-equal? (calcula-linhas (read-lines "files/queue1.rkt")) 54)
              (check-equal? (calcula-linhas (read-lines "files/queue2.rkt")) 100)
              (check-equal? (calcula-linhas (read-lines "files/queue3.rkt")) 68)))

;; roda-testes ... -> Void
;; Responsavel por executar um conjunto de testes unitarios
(define (roda-testes)
  (printf "\n################## Executando testes ##################\n")
  (printf "######### Teste 1: Nro de linhas do arquivo #########\n")
  (run-tests tamanho-test)
  (printf "\n######### Teste 2: Nro de comentarios do arquivo #########\n")
  (run-tests comentarios-test)
  (printf "\n######### Teste 3: Nro de funcoes do arquivo #########\n")
  (run-tests funcoes-test)
  (printf "\n######### Teste 4: Nro de imports do arquivo #########\n")
  (run-tests imports-test))
  