#lang racket
(require 2htdp/batch-io)
(require rackunit)
(require rackunit/text-ui)

;;################### FUNCOES AUXILIARES AO ALGORITMO ###################

;; slice: Lista, Inteiro(inicio, n) -> Sublista
;; Funcao que recorta uma lista em uma sublista com n valores a partir de inicio
(define (slice lista inicio n)
  (take (drop lista inicio) n))


;; indices-funcao: Lista de Strings -> Lista de Inteiros
;; Funcao que retorna uma lista dos indices de todas as funcoes de um arquivo
(define (indices-funcao arq)
  (indexes-of  (map acha-funcoes arq) 1))

;; my-sum: Lista de Inteiros (lista), Inteiro(Acc), Inteiro(Flag) -> Inteiro
;; Flag serve pra dizer se eh a primeira iteracao ou nao da recursao
;; Recursivamente soma todos os valores de uma lista (parecido com o foldr) mas para se chegar a 0 parcialmente, retornando -1
(define (my-sum lista acc flag)
  (cond
    [(empty? lista) acc]
    [(and (equal? acc 0) (positive? flag)) -1]
    [else (my-sum (rest lista) (+ acc (first lista)) (add1 flag))]
  ))


;; conta-parenteses: char -> Inteiro
;; Funcao verifica se um caractere eh um parenteses aberto ou fechado
(define (conta-parenteses char)
  (cond
    [(equal? char #\() 1]
    [(equal? char #\)) -1]
    [else 0]))

;; soma-todos: Lista -> Inteiro
;; Funcao que soma todos os itens de uma lista e retorna um valor
(define (soma-todos lista)
 (foldr + 0 lista))


;;acha-menor: lista de numeros -> numero
;;encontra o menor valor dentre uma lista de numeros, e retorna-o
(define (acha-menor lista)
  (let loop ((lista lista)
             (menor (first lista)))
    (cond
      [(empty? lista) menor]
      [(< menor (first lista)) (loop (rest lista) menor)]
      [else (loop (rest lista) (first lista))]))) 

;;###############################################################################################

;;################### METRICAS ###################

;; ------------------ METRICA 5 ------------------

;; conta-linhas-funcao: Lista de Strings(arq), Lista de Inteiros (lista-indices), Inteiro(acc)
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

;; mais-que-5: lista de strings(arq) , inteiro(indice) -> inteiro
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

;; ------------------------------------

;; ;; ------------------ METRICA 4 ------------------

;;calcula-imports: Lista de Strings(arq) -> Inteiro
;;Funcao que calcula a metrica do numero de imports do arquivo
(define (calcula-imports arq)
  (soma-todos (map acha-imports arq))
  )

;;acha-imports: String -> Inteiro(1 se for import, 0 senao)
;;Funcao que analisa se uma string corresponde a um import ou nao
(define (acha-imports string)
  (cond
    [(string-prefix? string "(require") 1]
    [else 0]
    )
  )

;; ------------------------------------

;; ------------------ METRICA 3 ------------------

;;calcula-funcoes: lista de strings (arq) -> numero inteiro
;;Funcao que calcula a metrica do numero de funcoes do arquivo
(define (calcula-funcoes arq)
  (soma-todos (map acha-funcoes arq))
  )

;; acha-funcoes: String -> retorna 1 se for funcao, 0 senao
;; Funcao que retorna se uma string corresponde a uma funcao ou nao
(define (acha-funcoes string)
  (cond
    [(string-prefix? (string-trim string) "(define (") 1]
    [else 0]
  ))

;; ------------------------------------

;; ------------------ METRICA 2 ------------------

;;calcula-comentarios: lista de strings(arq) -> Inteiro 
;;Funcao que calcula a metrica do numero de comentarios no arquivo
(define (calcula-comentarios arq)
  (soma-todos (map acha-comentario arq))
  )


;; acha-comentario: String -> Inteiro (1 se achar comentario, 0 senao)
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

;; ------------------------------------

;; ------------------ METRICA 1 ------------------

;; calcula-linhas(arq): lista de strings (arq) -> numero inteiro
;; Metrica que retorna o nro de linhas do arquivo
(define (calcula-linhas arq)
  (length arq))

;;###############################################################################################

;;################### FLUXO PRINCIPAL ###################

;; le-arquivos ... -> Void
;; Funcao responsavel por ler os arquivos do diretorio "files/"
;; Eh o fluxo principal do programa
(define (le-arquivos)
  (define (rkt? string) (string-suffix? string ".rkt"))
  (define folder (map path->string (directory-list "files/" #:build? #t)))
  (define rkt-folder(filter rkt? folder))
  (let ([arquivos rkt-folder])
  (define lista-pontos (map (lambda (arq) (avalia-codigo (read-lines arq) (string-replace arq "files/" ""))) arquivos))
  (avalia-pontuacao arquivos lista-pontos))
  )

;; avalia-codigo: lista de strings (arq) e string (nomeArq) -> numero float
;; Funcao que recebe uma lista de strings correspondente a um arquivo
;; e retorna a pontuacao final do arquivo
(define (avalia-codigo arq nomeArq)
  (printf "\nCodigo analisado: ~a\n" nomeArq)
  (define m1 (calcula-linhas arq))
  (define m2 (calcula-comentarios arq))
  (define m3 (calcula-funcoes arq))
  (define m4 (calcula-imports arq))
  (define m5 (conta-linhas-funcao arq (indices-funcao arq) 0))
  (printf "\nMetricas calculadas: ~a"
             (list (string-append "\nNumero de linhas: " (~v m1)) ;;metrica 1 do total de linhas do arquivo
                   (string-append "\nNumero de comentarios: " (~v m2))
                   (string-append "\nNumero de funcoes: " (~v m3))
                   (string-append "\nNumero de imports: " (~v m4))
                   (string-append "\nNumero de funcoes com mais de 5 linhas: " (~v m5))
                   (string-append "\n"))
             )
  (gera-pontos m1 m2 m3 m4 m5)
  )
;; gera-pontos: numeros inteiros -> numero float
;; Funcao que calcula os pesos de cada metrica e gera a pontuacao do codigo analisado
(define (gera-pontos m1 m2 m3 m4 m5)
  (define p1 (* m1 0.3))
  (define p2 (* m2 0.1))
  (define p3 (* m3 0.2))
  (define p4 (* m4 0.1))
  (define p5 (* m5 0.3))
  (define pontuacao (+ p1 p2 p3 p4 p5))
  (printf "\nPontuacao: ~a\n" pontuacao)
  pontuacao)

;;avalia-pontuacao: lista de numeros -> void
;;funcao que encontra a melhor pontuacao dentre os codigos analisados
(define (avalia-pontuacao arquivos pontos)
  (printf "\n=============== Resultado ===============")
  (define menor (acha-menor pontos))
  (define indice-menor (index-of pontos menor))
  (define arq-menor (list-ref arquivos indice-menor))
  (printf "\nA melhor pontuacao dentre os codigos foi: ~a\n" menor)
  (printf "Obtida pelo: ~a\n" (string-replace arq-menor "files/" "")))

;;###############################################################################################

;;################### TESTES UNITARIOS ###################

;; comentarios-test ... -> Void
;; Teste unitario das funcoes relacionadas ao nro de comentarios do arq
(define comentarios-test
  (test-suite "Testes para a metrica de comentarios"
              (check-equal? (acha-comentario ";;comentario teste 1") 1)
              (check-equal? (acha-comentario "(define (teste string))") 0)
              (check-equal? (calcula-comentarios arq3) 7)))


;;funcoes-test ... -> Void
;; Teste unitario das funcoes relacionadas ao nro de funcoes do arq
(define funcoes-test
  (test-suite "Testes para a metrica de nro de funcoes"
              (check-equal? (acha-funcoes ";;comentario teste 1") 0)
              (check-equal? (acha-funcoes "(define (teste string))") 1)
              (check-equal? (acha-funcoes "(define-struct") 0)
              (check-equal? (calcula-funcoes arq3) 9)))

;;imports-test ... -> Void
;; Teste unitario das funcoes relacionadas ao nro de imports do arq
(define imports-test
  (test-suite "Testes para a metrica de nro de imports"
              (check-equal? (acha-imports ";;comentario teste 1") 0)
              (check-equal? (acha-imports "(define (teste string))") 0)
              (check-equal? (acha-imports "(define-struct") 0)
              (check-equal? (acha-imports "(require") 1)
              (check-equal? (calcula-imports arq1) 0)
              (check-equal? (calcula-imports arq2) 1)))

;; tamanho-test ... -> Void
;; Teste unitario das funcoes relacionadas ao nro de linhas do arquivo
(define tamanho-test
  (test-suite "Testes para a metrica do nro de linhas do arquivo"
              (check-equal? (calcula-linhas arq1) 54)
              (check-equal? (calcula-linhas arq2) 100)
              (check-equal? (calcula-linhas arq3) 68)))

;; nro5linhas-test ... -> Void
;; Teste unitario das funcoes relacionadas ao nro de funcoes com mais de 5 linhas de um arquivo
(define nro5linhas-test
  (test-suite "Testes para a metrica do nro de funcoes com mais de 5 linhas do arquivo"
              (check-equal? (conta-linhas-funcao arq1 (indices-funcao arq1) 0) 1)
              (check-equal? (conta-linhas-funcao arq2 (indices-funcao arq2) 0) 0)
              (check-equal? (conta-linhas-funcao arq3 (indices-funcao arq3) 0) 5)))

;;############################################################################

;;############## TESTE AUTOMATIZADO #################

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
  (run-tests imports-test)
  (printf "\n######### Teste 5: Nro de funcoes com mais de 5 linhas do arquivo #########\n")
  (run-tests nro5linhas-test))
;;##################################################

;;Funcoes apenas para auxiliar nos testes unitarios
(define arq1 (read-lines "files/queue1.rkt"))
(define arq2 (read-lines "files/queue2.rkt"))
(define arq3 (read-lines "files/queue3.rkt"))