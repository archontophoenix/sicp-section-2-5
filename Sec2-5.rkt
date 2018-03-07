#lang racket

(require racket/hash)

; This is a response to the dynamic dispatch mechanism proposed in Sections 2.4
; and 2.5 of SICP. That mechanism makes use of a global mutable map to perform
; dynamic dispatching, but neither global variables nor mutability are required
; to achieve the desired effect. In fact, without them, we can come up with a
; mechanism for dynamic dispatch that is less verbose than what the book
; proposes.
;
; The goal here is to be able to invoke dynamic functions with syntax like:
;
;    (D 'f args)
;
; where D is a dynamic dispatcher object (which is immutable), 'f is the name of
; a function dispatched dynamically, and args are the arguments to 'f that
; determine which implementation is chosen for 'f (presumably by examining the
; types of the arguments, but this file doesn't address that aspect of
; dispatch. It might be convenient to use the pattern matching facilities in
; racket/match in dynamically dispatched function implementations).
;
; To keep track of what we're doing, this file uses a hokey ad-hoc totally
; unrigorous type notation, in which dynamic dispatchers can be thought of as
; having type:
;
; type Message = <anything>
; type Result = <anything>
; type DynamicDispatcher = Symbol -> Message -> Result
;
; where Message is the type of arguments to dynamically dispatched functions
; (that is, anything at all) and Result is the type of values that dynamically
; dispatched functions can return (again, anything at all).
;
; How do we build a useful DynamicDisptcher? Let's try to do it from the bottom
; up.



; Suppose that message-passing object o is, as in the book, just a function that
; takes a message (which can be an arbitrary structure; you can think of it as
; including a bunch of arguments):

(define (o msg) '...)

; type Object = Message -> Result

; An object is a suitable convention for the implementation of a single dynamic
; function, where the message is the arguments for the function, and the result
; the result of applying the function to the arguments. For example, an object
; that implements the dynamic function '+ for both numbers and lists might look
; like:

(define (plus-impl msg)
  (if (and (pair? msg) (= 2 (length msg)))
      (let ((arg0 (car msg))
            (arg1 (cadr msg)))
        (cond ((and (number? arg0) (number? arg1)) (+ arg0 arg1))
              ((and (pair? arg0) (pair? arg1)) (append arg0 arg1))
              (else (error "Unsupported argument types for +:" msg))))
      (error "Wrong number of arguments (required 2) for +:" msg)))

; Now, suppose we wanted to equip such a thing with a fallback object to
; handle messages not understood by the original object. Such a "protoobject"
; takes *two* (curried) parameters, the fallback object (which is a full-fledged
; object, not a protoobject) and the message (equivalently, you can think of
; a protoobject as taking an object and returning an object). For example:

(define (o1 fallback)
  (lambda (msg) '(if (understood msg) (do-something) (fallback msg))))

; type Protoobject = Object -> Message -> Result  ; (i.e., Object -> Object)

; We can turn two protoobjects into a single protoobject by chaining them:

(define (chain-protoobjs p0 p1)
  (lambda (fallback) (p0 (p1 fallback))))

; chain-protoobjs: Protoobject -> Protoobject -> Protoobject

; We might want a way to create an ultimate fallback object that gives an error
; for any message it tries to process (with some auxiliary info to display about
; the origin of the message):

(define (fail-obj info)
  (lambda (msg) (error "Message not understood: " info msg)))

; fail-obj: <anything> -> Object

; Implementers of dynamically dispatched functions will need to create maps from
; function names to implementations. We can use Racket's builtin hash tables for
; the maps. The implementations in question will be protoobjects, not objects;
; we'll want to be able to put together two maps in such a way that a given
; function's implementation combines its impelementations in both maps.
;
; Such a map might have a shape like this (call it a FunMap):

(hash
 'f
 '(... (protoobj) implementation for f ...)
 'g
 '(... (protoobj) implementation for g ...))

; type FunMap = HashTable Symbol Protoobject

; One thing we might want to do with FunMaps is chain them together. Chaining
; two FunMaps m0 and m1 means that the resulting FunMap has implementations for
; all functions present in either m0 or m1, and if any functions are present in
; both, we chain together the (protoobject) implementations from m0 and m1. (The
; m0 implementation comes first, which means it takes precedence over m1, which
; might or might not be what you want; it's your responsibility to chain things
; together in the proper sequence.)

(define (chain-fun-maps m0 m1)
  (hash-union m0 m1 #:combine/key (lambda (k p0 p1) (chain-protoobjs p0 p1))))

; chain-fun-maps: FunMap -> FunMap -> FunMap

; We can chain a whole bunch of FunMaps together if we supply a list of them:

(define (chain-fun-map-list maps)
  (foldl chain-fun-maps (hash) maps))

; chain-fun-map-list: List FunMap -> FunMap

; We might also want to turn a FunMap into a map whose values are proper
; objects instead of protoobjects. A good way to do this might be supply each
; protoobject with a failure object whose error message includes the name of
; the dynamic function whose dispatch failed:

(define (map-fallback-to-failure m)
  (make-immutable-hash (hash-map m (lambda (f p) (f . (p (fail-obj f)))))) )

; map-fallback-to-failure: FunMap -> HashTable Symbol Object

; FunMap still isn't quite the right type of thing for implementers of
; dynamically dispatched functions to be writing directly. We need to be able to
; dispatch functions dynamically inside the function implementations --
; including dispatching to the dynamic dispatcher *of which the implementation
; is a part* (recurse-o-rama!). Let's equip each module of dynamically
; dispatched functions (call it a FunMod) with a parameter for the
; DyamicDispatcher of which the FunMod is a part:

(lambda (D)
  (hash
   'f
   '(... (protoobj) implementation for f (that contains calls to D) ...)
   'g
   '(... (protoobj) implementation for g (that contains calls to D) ...)))
   
; type FunMod = DynamicDispatcher -> FunMap

; Now we need to supply the function, dynamic-dispatcher, that builds a
; DynamicDispatcher from a list of FunMods. First, we need to supply each FunMod
; with a copy of the dynamic dispatcher being built (crazy recursion, amirite?)
; to turn it into a FunMap; then we chain the FunMaps together into a single
; master FunMap that includes *all* function implementations for each function
; in the FunMods; then we turn the FunMap into a map from symbols to *objects*
; (by falling back to the failure object); and then we have something the
; dynamic dispatcher under construction can actually use to handle its inputs:

(define (dynamic-dispatcher fun-mods)
  (define (D f args)
    (let* ((fun-maps (map (lambda (m) (m D)) fun-mods)) ; recurse on D!
           (master-fun-map (chain-fun-map-list fun-maps))
           (master-obj-map (map-fallback-to-failure master-fun-map)))
      ((hash-ref master-obj-map f (error "No such dynamic function:" f)) args)))
  D)

; dynamic-dispatcher: List FunMod -> DynamicDispatcher



