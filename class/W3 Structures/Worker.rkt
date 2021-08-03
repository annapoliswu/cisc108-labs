;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Worker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct hourly-worker (wage hours))
;; make-hourly-worker: number number --> Hourly-Worker
(define-struct exempt-worker (salary))
;; make-exempt-worker: number --> Exempt-Worker

;; How to define a Worker?
;;A WRONG
(define-struct worker (wage hours salary))
;; make-worker: number number number --> Worker

;;B WRONG
(define-struct worker (exempt? wage hours salary))
;; make-worker: boolean number number number --> Worker

;;C CORRECT
;; A Worker is either
;; -- an Hourly-Worker, or
;; -- an Exempt-Worker

;;D WRONG
;; A WorkerType is either "hourly" or "exempt"
(define-struct worker (type wage hours salary))
;; make-worker: WorkerType number number number --> Worker

;; Given that
;; A Worker is either
;; -- an Hourly-Worker, or
;; -- an Exempt-Worker

#| the the TEMPLATE for a Worker should be:
;; A WRONG
(define (worker-fun aw)
  ...(worker-wage aw) ;number
  ...(worker-hours aw) ;number
  ...(worker-salary aw) );number

;; B WRONG
(define (worker-fun aw)
  (cond [HourlyWorker (worker-fun aw)]
        [ExemptWorker (worker-fun aw)]))

;; C CORRECT
(define (worker-fun aw)
  (cond [(hourly-worker? aw) (hourly-worker-fun aw)]
        [(exempt-worker? aw) (exempt-worker-fun aw)]))
|#

  





