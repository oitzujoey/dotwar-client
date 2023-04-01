# dotwar-client

## Installation

Currently only works on SBCL due to the combination of required libraries.

## Usage

```lisp
(asdf:load-system "dotwar-client")
;; View the system in 3D
(dotwar-client:view)
;; Pursue a ship.
(let ((interval-in-seconds (* 60 60)))
  (dotwar-client:track-target "Evil Ship" "My ship" interval-in-seconds))
```
